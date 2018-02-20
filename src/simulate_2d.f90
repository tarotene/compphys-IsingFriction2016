PROGRAM main
  !$  USE omp_lib
  USE mod_global
  USE mod_rand
  USE mod_proc_int
  USE mod_proc_file
  USE IFPORT, ONLY: access

  IMPLICIT NONE

  ! observables
  INTEGER(kind = 4), ALLOCATABLE :: spin(:, :)
  INTEGER(kind = 4) :: energy

  ! simulation variables
  INTEGER(kind = 4) :: seed_master
  INTEGER(kind = 4) :: seed_x, seed_z, seed_prob
  INTEGER(kind = 4), ALLOCATABLE :: rn_x(:, :), rn_z(:, :)
  REAL(kind = 8), ALLOCATABLE :: rn_prob(:, :)
  TYPE (VSL_STREAM_STATE), ALLOCATABLE :: str_x(:), str_z(:), str_prob(:)

  ! loop variables
  INTEGER(kind = 4) :: z, n_steps, i_step, i_vel, i_sweep, i_sample
  CHARACTER(len = 4, kind = 1) :: si_sweep, si_sample

  CHARACTER(:), ALLOCATABLE :: filename_spin, filename_energy

  ! slot variables
  INTEGER(kind = 4), ALLOCATABLE :: slot_spin(:), slot_energy(:)

  ! omp variables
  INTEGER(kind = 4) :: i_th, err_x, err_z, err_prob

  ! stat variables
  INTEGER(kind = 4) :: exist_list_parameters, exist_stat_samples
  INTEGER(kind = 4), ALLOCATABLE :: stat_sample_e(:)
  INTEGER(kind = 4), ALLOCATABLE :: stat_sample_a(:)

  CALL inputParameters_2d(len_x, len_z, beta, vel, &
       n_sweeps_therm, n_sweeps_stead, id_IC, id_BC, n_samples)

  IF ( n_samples < n_samples0 ) THEN
     n_samples = n_samples0
  END IF

  ! setting array of flip energies and their probabilities
  CALL makeDeltaEArray_2d(deltaE_2d(-1:1, -1:1, -1:1, -1:1, -1:1))
  CALL makeProbArray_2d(beta, deltaE_2d(-1:1, -1:1, -1:1, -1:1, -1:1), &
       prob_2d(-1:1, -1:1, -1:1, -1:1, -1:1))

  ! adjustment program to machine
  n_ths = 1
  !$  n_ths = omp_get_max_threads()
  !$  CALL omp_set_num_threads(n_ths)
  !allocation slots
  ALLOCATE(slot_energy(0:n_ths - 1))
  ALLOCATE(slot_spin(0:n_ths - 1))
  ! setting slots
  DO i_th = 0, n_ths - 1
     slot_energy(i_th) = 20 + i_th
     slot_spin(i_th) = 20 + i_th + n_ths
  END DO

  ! allocation random numbers and their streams
  ALLOCATE(rn_x(0:n_ths - 1, 1:len_x * len_z))
  ALLOCATE(rn_z(0:n_ths - 1, 1:len_x * len_z))
  ALLOCATE(rn_prob(0:n_ths - 1, 1:len_x * len_z))
  ALLOCATE(str_x(1:n_samples), str_z(1:n_samples), str_prob(1:n_samples))

  ! allocation observables
  ALLOCATE(spin(1:len_x, 1:len_z))

  ! initialize and save spin
  IF ( n_samples0 == 0 ) THEN
     CALL initializeSpin_2d(id_IC, &
          len_x, len_z, spin(1:len_x, 1:len_z))
     CALL exportSpin_2d(slot_spin(0), "spin_initial.dat", &
          len_x, len_z, spin(1:len_x, 1:len_z))
  END IF

  ! initialize and save vectors of random number
  seed_master = 100
  DO i_sample = n_samples0 + 1, n_samples, 1
     seed_x = seed_master + i_sample
     seed_z = seed_master + i_sample + n_samples
     seed_prob = seed_master + i_sample + 2 * n_samples
     CALL initializeRN(seed_x, str_x(i_sample), err_x)
     CALL initializeRN(seed_z, str_z(i_sample), err_z)
     CALL initializeRN(seed_prob, str_prob(i_sample), err_prob)
  END DO

  ! generage new samples
  !$omp parallel do schedule(static, 1) default(none) &
  !$omp shared(n_samples0, n_samples) &
  !$omp shared(n_sweeps_therm, n_sweeps_stead) &
  !$omp shared(id_BC, len_x, len_z, vel) &
  !$omp shared(str_x, str_z, str_prob) &
  !$omp shared(rn_x, rn_z, rn_prob) &
  !$omp shared(slot_spin, slot_energy) &
  !$omp private(err_x, err_z, err_prob) &
  !$omp private(filename_spin, filename_energy) &
  !$omp private(i_th, si_sample, si_sweep, i_step, n_steps) &
  !$omp private(spin, energy)
  DO i_sample = n_samples0 + 1, n_samples, 1
     i_th = 0
     !$ i_th = omp_get_thread_num()

     WRITE(si_sample, '(i0.4)') i_sample

     !  open energy stream
     filename_energy="energy_s"//si_sample//"_step.dat"
     OPEN(slot_energy(i_th), file=filename_energy, status="new")
     WRITE(slot_energy(i_th), '(a)') "# i_sweep, i_vel, i_step, energy"

     !  import spin
     filename_spin="spin_initial.dat"
     CALL importSpin_2d(slot_spin(i_th), filename_spin, &
          len_x, len_z, spin(1:len_x, 1:len_z))
     !  calculation of total energy
     CALL calcEnergy_2d(id_BC, len_x, len_z, &
          spin(1:len_x, 1:len_z), energy)

     !  thermalization
     n_steps = len_x * len_z
     DO i_sweep = 1, n_sweeps_therm, 1
        WRITE(si_sweep, '(i0.4)') i_sweep

        CALL generateRN_int(str_x(i_sample), 1, len_x, n_steps, &
             rn_x(i_th, 1:n_steps), err_x)
        CALL generateRN_int(str_z(i_sample), 1, len_z, n_steps, &
             rn_z(i_th, 1:n_steps), err_z)
        CALL generateRN_real(str_prob(i_sample), 0.0d0, 1.0d0, n_steps, &
             rn_prob(i_th, 1:n_steps), err_prob)

        CALL sweep_singleflip_2d(slot_energy(i_th), i_sweep, 0, id_BC, len_x, len_z, n_steps, &
             rn_x(i_th, 1:n_steps), &
             rn_z(i_th, 1:n_steps), &
             rn_prob(i_th, 1:n_steps), &
             spin(1:len_x, 1:len_z), energy)

        filename_spin="spin_t"//si_sweep//"s"//si_sample//".dat"
        CALL exportSpin_2d(slot_spin(i_th), &
             filename_spin, len_x, len_z, spin(1:len_x, 1:len_z))
     END DO

     !  steadization
     n_steps = len_x * len_z / vel
     DO i_sweep = n_sweeps_therm + 1, n_sweeps_therm + n_sweeps_stead, 1
       WRITE(si_sweep, '(i0.4)') i_sweep

        DO i_vel = 1, vel, 1
           CALL generateRN_int(str_x(i_sample), 1, len_x, n_steps, &
                rn_x(i_th, 1:n_steps), err_x)
           CALL generateRN_int(str_z(i_sample), 1, len_z, n_steps, &
                rn_z(i_th, 1:n_steps), err_z)
           CALL generateRN_real(str_prob(i_sample), 0.0d0, 1.0d0, n_steps, &
                rn_prob(i_th, 1:n_steps), err_prob)

           CALL shiftUpperHalf_2d(slot_energy(i_th), i_sweep, i_vel, id_BC, &
                len_x, len_z, spin(1:len_x, 1:len_z), energy)

           CALL sweep_singleflip_2d(slot_energy(i_th), i_sweep, i_vel, id_BC, &
                len_x, len_z, n_steps, &
                rn_x(i_th, 1:n_steps), rn_z(i_th, 1:n_steps), rn_prob(i_th, 1:n_steps), &
                spin(1:len_x, 1:len_z), energy)
        END DO

        filename_spin="spin_t"//si_sweep//"s"//si_sample//".dat"
        CALL exportSpin_2d(slot_spin(i_th), &
             filename_spin, len_x, len_z, spin(1:len_x, 1:len_z))
     END DO

     CLOSE(slot_energy(i_th))
  END DO
  !$omp end parallel do

  ! destruction vectors of random numbers
  !$omp parallel do schedule(dynamic, 1) default(none) &
  !$omp shared(n_samples) &
  !$omp shared(str_x, str_z, str_prob) &
  !$omp private(err_x, err_z, err_prob)
  DO i_sample = 1, n_samples, 1
     CALL destractRN(str_x(i_sample), err_x)
     CALL destractRN(str_z(i_sample), err_z)
     CALL destractRN(str_prob(i_sample), err_prob)
  END DO
  !$omp end parallel do
END PROGRAM main
