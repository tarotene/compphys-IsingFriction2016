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
  REAL(kind = 8), ALLOCATABLE :: m_z(:)
  REAL(kind = 8) :: pump, diss, energy

  ! simulation variables
  INTEGER(kind = 4) :: seed_master
  INTEGER(kind = 4) :: seed_x, seed_z, seed_prob
  INTEGER(kind = 4), ALLOCATABLE :: rn_x(:, :), rn_z(:, :)
  REAL(kind = 8), ALLOCATABLE :: rn_prob(:, :)
  TYPE (VSL_STREAM_STATE), ALLOCATABLE :: str_x(:), str_z(:), str_prob(:)

  ! loop variables
  INTEGER(kind = 4) :: z, n_steps, i_step, i_vel, i_sweep, i_sample
  CHARACTER(len = 4, kind = 1) :: si_sample

  CHARACTER(:), ALLOCATABLE :: &
       filename_stream, filename_m_z, filename_spin
  CHARACTER(len = 40, kind = 1) :: filename_str

  ! slot variables
  INTEGER(kind = 4), ALLOCATABLE :: slot_stream(:), slot_spin(:), slot_m_z(:)

  ! omp variables
  INTEGER(kind = 4) :: i_th, err_x, err_z, err_prob

  ! stat variables
  INTEGER(kind = 4) :: exist_list_parameters, exist_stat_samples
  INTEGER(kind = 4), ALLOCATABLE :: stat_sample_e(:)
  INTEGER(kind = 4), ALLOCATABLE :: stat_sample_a(:)

  CALL inputParameters_2d(len_x, len_z, J, beta, vel, &
       n_sweeps_therm, n_sweeps_stead, id_IC, id_BC, n_samples, &
       onoff_stream, onoff_m_z)

  exist_list_parameters = access( "./list_parameters.dat", " ")

  IF ( exist_list_parameters == 0 ) THEN
     CALL getListParameters_2d(10, "list_parameters.dat", &
          len_x, len_z, J, beta, vel, &
          n_sweeps_therm0, n_sweeps_stead0, id_IC, id_BC, n_samples0, &
          onoff_stream, onoff_m_z)
  ELSE
     n_sweeps_therm0 = 0
     n_sweeps_stead0 = 0
     n_samples0 = 0
  END IF

  IF ( n_samples < n_samples0 ) THEN
     n_samples = n_samples0
  END IF
  IF ( n_sweeps_therm < n_sweeps_therm0 ) THEN
     n_sweeps_therm = n_sweeps_therm0
  END IF
  IF ( n_sweeps_stead < n_sweeps_stead0 ) THEN
     n_sweeps_stead = n_sweeps_stead0
  END IF

  !TODO: 対応する基底状態の順にICとBCを揃える
  ! id_IC: 1. all-up, 2. DW, 3. random
  ! id_BC: 1. anti-parallel, 2. parallel, 3. free
  ! n_sweeps = n_sweeps_therm + n_sweeps_stead

  ! setting array of flip energies and their probabilities
  CALL makeDeltaEArray_2d(J, deltaE_2d(-1:1, -1:1, -1:1, -1:1, -1:1))
  CALL makeProbArray_2d(beta, deltaE_2d(-1:1, -1:1, -1:1, -1:1, -1:1), &
       prob_2d(-1:1, -1:1, -1:1, -1:1, -1:1))

  ! allocation stat array
  ALLOCATE(stat_sample_e(1:n_samples))
  ALLOCATE(stat_sample_a(1:n_samples))
  stat_sample_e(1:n_samples) = 0
  stat_sample_a(1:n_samples) = 0
  exist_stat_samples = access( "./stat_samples.dat", " ")
  IF ( exist_stat_samples == 0 ) THEN
     CALL getStatSamples(15, "stat_samples.dat", &
          n_samples0, stat_sample_e(1:n_samples0), &
          stat_sample_a(1:n_samples0))
  END IF

  ! copy averaged stream file to new file
  DO i_sample = 1, n_samples0 * onoff_stream, 1
     IF ( stat_sample_e(i_sample) == 0 ) THEN
        WRITE(si_sample, '(i0.4)') i_sample
        filename_stream="stream_s"//si_sample//".dat"
        CALL copyStream2Stream(20, &
             "stream.dat", filename_stream, &
             n_sweeps_therm, n_sweeps_stead)
     END IF
  END DO

  ! copy averaged m_z file to new file
  DO i_sample = 1, n_samples0 * onoff_m_z, 1
     IF ( stat_sample_e(i_sample) == 0 ) THEN
        WRITE(si_sample, '(i0.4)') i_sample
        filename_m_z="m_z_s"//si_sample//".dat"
        CALL copyM_z2M_z_2d(30, &
             "m_z.dat", filename_m_z, &
             len_x, len_z, n_sweeps_therm, n_sweeps_stead)
     END IF
  END DO

  ! adjustment program to machine
  n_ths = 1
  !$  n_ths = omp_get_max_threads()
  !$  CALL omp_set_num_threads(n_ths)
  !allocation slots
  ALLOCATE(slot_stream(0:n_ths - 1))
  ALLOCATE(slot_m_z(0:n_ths - 1))
  ALLOCATE(slot_spin(0:n_ths - 1))
  !setting slots
  DO i_th = 0, n_ths - 1
     slot_stream(i_th) = 20 + i_th
     slot_spin(i_th) = 20 + i_th + n_ths
     slot_m_z(i_th) = 20 + i_th + 2 * n_ths
  END DO

  ! allocation random numbers and their streams
  ALLOCATE(rn_x(0:n_ths - 1, 1:len_x * len_z))
  ALLOCATE(rn_z(0:n_ths - 1, 1:len_x * len_z))
  ALLOCATE(rn_prob(0:n_ths - 1, 1:len_x * len_z))
  ALLOCATE(str_x(1:n_samples), str_z(1:n_samples), str_prob(1:n_samples))

  ! import random number streams
  DO i_sample = 1, n_samples0, 1
     WRITE(si_sample, '(i0.4)') i_sample
     filename_str="str_x_initial_s"//si_sample//".bin"
     CALL loadRNstat(str_x(i_sample), filename_str, err_x)
     filename_str="str_z_initial_s"//si_sample//".bin"
     CALL loadRNstat(str_z(i_sample), filename_str, err_z)
     filename_str="str_prob_initial_s"//si_sample//".bin"
     CALL loadRNstat(str_prob(i_sample), filename_str, err_prob)
  END DO

  ! allocation observables
  ALLOCATE(spin(1:len_x, 1:len_z), m_z(1:len_z))

  ! overwrite existing samples
  !$omp parallel do schedule(static, 1) default(none) &
  !$omp shared(n_samples0) &
  !$omp shared(n_sweeps_therm0, n_sweeps_stead0) &
  !$omp shared(n_sweeps_therm, n_sweeps_stead) &
  !$omp shared(id_BC, onoff_stream, onoff_m_z, len_x, len_z, vel) &
  !$omp shared(stat_sample_e, stat_sample_a) &
  !$omp shared(str_x, str_z, str_prob) &
  !$omp shared(rn_x, rn_z, rn_prob) &
  !$omp shared(slot_stream, slot_m_z, slot_spin) &
  !$omp private(err_x, err_z, err_prob) &
  !$omp private(filename_stream, filename_m_z, filename_spin) &
  !$omp private(filename_str) &
  !$omp private(i_th, si_sample, n_steps) &
  !$omp private(spin, pump, diss, energy)
  DO i_sample = 1, n_samples0, 1
     i_th = 0
     !$ i_th = omp_get_thread_num()

     WRITE(si_sample, '(i0.4)') i_sample

     !  import energy stream
     filename_stream="stream_s"//si_sample//".dat"
     OPEN(slot_stream(i_th), file=filename_stream, status="old")
     READ(slot_stream(i_th), '()')
     !  read through energy stream
     DO i_sweep = 1, n_sweeps_therm0 * onoff_stream, 1
        READ(slot_stream(i_th), '()')
     END DO

     !  import magnetization profile
     filename_m_z="m_z_s"//si_sample//".dat"
     OPEN(slot_m_z(i_th), file=filename_m_z, status="old")
     READ(slot_m_z(i_th), '()')
     !  read through m_z
     DO i_sweep = 1, n_sweeps_therm0 * onoff_m_z, 1
        DO z = 1, len_z, 1
           READ(slot_m_z(i_th), '()')
        END DO
        READ(slot_m_z(i_th), '()')
     END DO

     !  import spin
     filename_spin="spin_thermalized_s"//si_sample//".dat"
     CALL importSpin_2d(slot_spin(i_th), filename_spin, &
          len_x, len_z, spin(1:len_x, 1:len_z))
     !  calculation total energy
     CALL calcEnergy_2d(id_BC, len_x, len_z, &
          spin(1:len_x, 1:len_z), energy)

     !  import vectors of random numbers
     filename_str="str_x_thermalized_s"//si_sample//".bin"
     CALL loadRNstat(str_x(i_sample), filename_str, err_x)
     filename_str="str_z_thermalized_s"//si_sample//".bin"
     CALL loadRNstat(str_z(i_sample), filename_str, err_z)
     filename_str="str_prob_thermalized_s"//si_sample//".bin"
     CALL loadRNstat(str_prob(i_sample), filename_str, err_prob)

     !  calculation
     n_steps = len_x * len_z
     DO i_sweep = n_sweeps_therm0 + 1, n_sweeps_therm, 1
        CALL generateRN_int(str_x(i_sample), 1, len_x, n_steps, &
             rn_x(i_th, 1:n_steps), err_x)
        CALL generateRN_int(str_z(i_sample), 1, len_z, n_steps, &
             rn_z(i_th, 1:n_steps), err_z)
        CALL generateRN_real(str_prob(i_sample), 0.0d0, 1.0d0, n_steps, &
             rn_prob(i_th, 1:n_steps), err_prob)

        CALL sweep_singleflip_2d(id_BC, len_x, len_z, n_steps, &
             rn_x(i_th, 1:n_steps), &
             rn_z(i_th, 1:n_steps), &
             rn_prob(i_th, 1:n_steps), &
             spin(1:len_x, 1:len_z), diss)

        energy = energy + diss

        CALL exportStream_onfile(onoff_stream, slot_stream(i_th), &
             i_sweep, 0.0d0, diss, energy)
        CALL exportM_z_onfile_2d(onoff_m_z, slot_m_z(i_th), &
             i_sweep, len_x, len_z, spin)
     END DO

     !  marker point of thermalization
     IF ( n_sweeps_therm > n_sweeps_therm0 ) THEN
        WRITE(slot_stream(i_th), '(a)') "# -- Thermalized --"
     ELSE
        READ(slot_stream(i_th), '()')
     END IF

     !  save spin and random number stream
     filename_spin="spin_thermalized_s"//si_sample//".dat"
     CALL exportSpin_2d(slot_spin(i_th), &
          filename_spin, len_x, len_z, spin(1:len_x, 1:len_z))
     filename_str="str_x_thermalized_s"//si_sample//".bin"
     CALL saveRNstat(str_x(i_sample), filename_str, err_x)
     filename_str="str_z_thermalized_s"//si_sample//".bin"
     CALL saveRNstat(str_z(i_sample), filename_str, err_z)
     filename_str="str_prob_thermalized_s"//si_sample//".bin"
     CALL saveRNstat(str_prob(i_sample), filename_str, err_prob)

     !  read through energy stream
     DO i_sweep = 1, SIGN(n_sweeps_stead0, n_sweeps_therm0 - n_sweeps_therm) * onoff_stream, 1
        READ(slot_stream(i_th), '()')
     END DO

     !  read through m_z
     DO i_sweep = 1, SIGN(n_sweeps_stead0, n_sweeps_therm0 - n_sweeps_therm) * onoff_m_z, 1
        DO z = 1, len_z, 1
           READ(slot_m_z(i_th), '()')
        END DO
        READ(slot_m_z(i_th), '()')
     END DO

     !  import spin
     DO i_sweep = 1, SIGN(1, n_sweeps_therm0 - n_sweeps_therm), 1
       filename_spin="spin_steadized_s"//si_sample//".dat"
       CALL importSpin_2d(slot_spin(i_th), filename_spin, &
            len_x, len_z, spin(1:len_x, 1:len_z))
       !  calculation total energy
       CALL calcEnergy_2d(id_BC, len_x, len_z, &
            spin(1:len_x, 1:len_z), energy)
     END DO

     !  import vectors of random numbers
     DO i_sweep = 1, SIGN(1, n_sweeps_therm0 - n_sweeps_therm), 1
       filename_str="str_x_steadized_s"//si_sample//".bin"
       CALL loadRNstat(str_x(i_sample), filename_str, err_x)
       filename_str="str_z_steadized_s"//si_sample//".bin"
       CALL loadRNstat(str_z(i_sample), filename_str, err_z)
       filename_str="str_prob_steadized_s"//si_sample//".bin"
       CALL loadRNstat(str_prob(i_sample), filename_str, err_prob)
     END DO

     !  calculation
     n_steps = len_x * len_z / vel
     DO i_sweep = n_sweeps_therm + n_sweeps_stead0 + &
          MAX(-n_sweeps_stead0, (n_sweeps_therm0 - n_sweeps_therm) * n_sweeps_stead0) + 1, &
          n_sweeps_therm + n_sweeps_stead, 1
        DO i_vel = 1, vel, 1
           CALL generateRN_int(str_x(i_sample), 1, len_x, n_steps, &
                rn_x(i_th, 1:n_steps), err_x)
           CALL generateRN_int(str_z(i_sample), 1, len_z, n_steps, &
                rn_z(i_th, 1:n_steps), err_z)
           CALL generateRN_real(str_prob(i_sample), 0.0d0, 1.0d0, n_steps, &
                rn_prob(i_th, 1:n_steps), err_prob)

           CALL shiftUpperHalf_2d(id_BC, len_x, len_z, &
                spin(1:len_x, 1:len_z), pump)
           energy = energy + pump

           CALL sweep_singleflip_2d(id_BC, len_x, len_z, n_steps, &
                rn_x(i_th, 1:n_steps), &
                rn_z(i_th, 1:n_steps), &
                rn_prob(i_th, 1:n_steps), &
                spin(1:len_x, 1:len_z), diss)
           energy = energy + diss
        END DO

        CALL exportStream_onfile(onoff_stream, slot_stream(i_th), &
             i_sweep, pump, diss, energy)
        CALL exportM_z_onfile_2d(onoff_m_z, slot_m_z(i_th), &
             i_sweep, len_x, len_z, spin(1:len_x, 1:len_z))
     END DO

     !  marker point of thermalization
     IF ( n_sweeps_therm > n_sweeps_therm0 .OR. &
          n_sweeps_stead > n_sweeps_stead0 ) THEN
        WRITE(slot_stream(i_th), '(a)') "# -- Steadized --"
     ELSE
        READ(slot_stream(i_th), '()')
     END IF

     CLOSE(slot_stream(i_th))
     CLOSE(slot_m_z(i_th))

     !  save spin and random number stream
     filename_spin="spin_steadized_s"//si_sample//".dat"
     CALL exportSpin_2d(slot_spin(i_th), &
          filename_spin, len_x, len_z, spin(1:len_x, 1:len_z))
     filename_str="str_x_steadized_s"//si_sample//".bin"
     CALL saveRNstat(str_x(i_sample), filename_str, err_x)
     filename_str="str_z_steadized_s"//si_sample//".bin"
     CALL saveRNstat(str_z(i_sample), filename_str, err_z)
     filename_str="str_prob_steadized_s"//si_sample//".bin"
     CALL saveRNstat(str_prob(i_sample), filename_str, err_prob)

     stat_sample_e(i_sample) = 1
     stat_sample_a(i_sample) = 0
  END DO
  !$omp end parallel do

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

     WRITE(si_sample, '(i0.4)') i_sample

     filename_str="str_x_initial_s"//si_sample//".bin"
     CALL saveRNstat(str_x(i_sample), filename_str, err_x)
     filename_str="str_z_initial_s"//si_sample//".bin"
     CALL saveRNstat(str_z(i_sample), filename_str, err_z)
     filename_str="str_prob_initial_s"//si_sample//".bin"
     CALL saveRNstat(str_prob(i_sample), filename_str, err_prob)
  END DO

  ! generage new samples
  !$omp parallel do schedule(static, 1) default(none) &
  !$omp shared(n_samples0, n_samples) &
  !$omp shared(n_sweeps_therm, n_sweeps_stead) &
  !$omp shared(id_BC, onoff_stream, onoff_m_z, len_x, len_z, vel) &
  !$omp shared(stat_sample_e, stat_sample_a) &
  !$omp shared(str_x, str_z, str_prob) &
  !$omp shared(rn_x, rn_z, rn_prob) &
  !$omp shared(slot_stream, slot_m_z, slot_spin) &
  !$omp private(err_x, err_z, err_prob) &
  !$omp private(filename_stream, filename_m_z, filename_spin) &
  !$omp private(filename_str) &
  !$omp private(i_th, si_sample, i_step, n_steps) &
  !$omp private(spin, pump, diss, energy)
  DO i_sample = n_samples0 + 1, n_samples, 1
     i_th = 0
     !$ i_th = omp_get_thread_num()

     WRITE(si_sample, '(i0.4)') i_sample

     !  open energy stream
     filename_stream="stream_s"//si_sample//".dat"
     OPEN(slot_stream(i_th), file=filename_stream, status="new")
     WRITE(slot_stream(i_th), '(a)') &
          "# i_sweep, pump, diss, energy, fluc_pump, fluc_diss, fluc_energy"
     !  open magnetization profile
     filename_m_z="m_z_s"//si_sample//".dat"
     OPEN(slot_m_z(i_th), file=filename_m_z, status="new")
     WRITE(slot_m_z(i_th), '(a)') "# i_sweep, z, m_z, fluc_m_z"

     !  import spin
     filename_spin="spin_initial.dat"
     CALL importSpin_2d(slot_spin(i_th), filename_spin, &
          len_x, len_z, spin(1:len_x, 1:len_z))
     !  calculation total energy
     CALL calcEnergy_2d(id_BC, len_x, len_z, &
          spin(1:len_x, 1:len_z), energy)

     !  import random number stream
     filename_str="str_x_initial_s"//si_sample//".bin"
     CALL loadRNstat(str_x(i_sample), filename_str, err_x)
     filename_str="str_z_initial_s"//si_sample//".bin"
     CALL loadRNstat(str_z(i_sample), filename_str, err_z)
     filename_str="str_prob_initial_s"//si_sample//".bin"
     CALL loadRNstat(str_prob(i_sample), filename_str, err_prob)

     !  calculation
     n_steps = len_x * len_z
     DO i_sweep = 1, n_sweeps_therm, 1
        CALL generateRN_int(str_x(i_sample), 1, len_x, n_steps, &
             rn_x(i_th, 1:n_steps), err_x)
        CALL generateRN_int(str_z(i_sample), 1, len_z, n_steps, &
             rn_z(i_th, 1:n_steps), err_z)
        CALL generateRN_real(str_prob(i_sample), 0.0d0, 1.0d0, n_steps, &
             rn_prob(i_th, 1:n_steps), err_prob)

        CALL sweep_singleflip_2d(id_BC, len_x, len_z, n_steps, &
             rn_x(i_th, 1:n_steps), &
             rn_z(i_th, 1:n_steps), &
             rn_prob(i_th, 1:n_steps), &
             spin(1:len_x, 1:len_z), diss)

        energy = energy + diss

        CALL exportStream_onfile(onoff_stream, slot_stream(i_th), &
             i_sweep, 0.0d0, diss, energy)
        CALL exportM_z_onfile_2d(onoff_m_z, slot_m_z(i_th), &
             i_sweep, len_x, len_z, spin(1:len_x, 1:len_z))
     END DO

     !  marker point of thermalization
     WRITE(slot_stream(i_th), '(a)') "# -- Thermalized --"

     !  save spin and random number stream
     filename_spin="spin_thermalized_s"//si_sample//".dat"
     CALL exportSpin_2d(slot_spin(i_th), filename_spin, &
          len_x, len_z, spin(1:len_x, 1:len_z))

     filename_str="str_x_thermalized_s"//si_sample//".bin"
     CALL saveRNstat(str_x(i_sample), filename_str, err_x)
     filename_str="str_z_thermalized_s"//si_sample//".bin"
     CALL saveRNstat(str_z(i_sample), filename_str, err_z)
     filename_str="str_prob_thermalized_s"//si_sample//".bin"
     CALL saveRNstat(str_prob(i_sample), filename_str, err_prob)

     !  calculation
     n_steps = len_x * len_z / vel
     DO i_sweep = n_sweeps_therm + 1, &
          n_sweeps_therm + n_sweeps_stead, 1

        DO i_vel = 1, vel, 1
           CALL generateRN_int(str_x(i_sample), 1, len_x, n_steps, &
                rn_x(i_th, 1:n_steps), err_x)
           CALL generateRN_int(str_z(i_sample), 1, len_z, n_steps, &
                rn_z(i_th, 1:n_steps), err_z)
           CALL generateRN_real(str_prob(i_sample), 0.0d0, 1.0d0, n_steps, &
                rn_prob(i_th, 1:n_steps), err_prob)

           CALL shiftUpperHalf_2d(id_BC, len_x, len_z, &
                spin(1:len_x, 1:len_z), pump)
           energy = energy + pump

           CALL sweep_singleflip_2d(id_BC, len_x, len_z, n_steps, &
                rn_x(i_th, 1:n_steps), &
                rn_z(i_th, 1:n_steps), &
                rn_prob(i_th, 1:n_steps), &
                spin(1:len_x, 1:len_z), diss)

           energy = energy + diss
        END DO

        CALL exportStream_onfile(onoff_stream, slot_stream(i_th), &
             i_sweep, pump, diss, energy)
        CALL exportM_z_onfile_2d(onoff_m_z, slot_m_z(i_th), &
             i_sweep, len_x, len_z, spin(1:len_x, 1:len_z))
     END DO

     !  marker point of thermalization
     WRITE(slot_stream(i_th), '(a)') "# -- Steadized --"

     CLOSE(slot_stream(i_th))
     CLOSE(slot_m_z(i_th))

     !  save spin and random number stream
     filename_spin="spin_steadized_s"//si_sample//".dat"
     CALL exportSpin_2d(slot_spin(i_th), &
          filename_spin, len_x, len_z, spin(1:len_x, 1:len_z))

     filename_str="str_x_steadized_s"//si_sample//".bin"
     CALL saveRNstat(str_x(i_sample), filename_str, err_x)
     filename_str="str_z_steadized_s"//si_sample//".bin"
     CALL saveRNstat(str_z(i_sample), filename_str, err_z)
     filename_str="str_prob_steadized_s"//si_sample//".bin"
     CALL saveRNstat(str_prob(i_sample), filename_str, err_prob)

     stat_sample_e(i_sample) = 1
     stat_sample_a(i_sample) = 0
  END DO
  !$omp end parallel do

  ! destruction vectors of random numbers
  !$omp parallel do schedule(dynamic, 1) default(none) &
  !$omp shared(n_samples) &
  !$omp shared(str_x, str_z, str_prob) &
  !$omp private(err_x, err_z, err_prob)
  DO i_sample = 1, n_samples, 1
     CALL destractRN(str_x(i_sample), err_x)
     CALL destractRN(str_z(i_sample), err_x)
     CALL destractRN(str_prob(i_sample), err_prob)
  END DO
  !$omp end parallel do

  CALL refreshListParameters_2d(50, "list_parameters.dat", &
       len_x, len_z, J, beta, vel, &
       n_sweeps_therm, n_sweeps_stead, id_IC, id_BC, n_samples, &
       onoff_stream, onoff_m_z)

  CALL refreshStatSamples(60, "stat_samples.dat", n_samples, &
       stat_sample_e(1:n_samples), stat_sample_a(1:n_samples))
END PROGRAM main
