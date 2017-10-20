PROGRAM main
  !$  USE omp_lib
  USE mod_global
  USE mod_rand
  USE mod_proc_int
  USE mod_proc_file
  USE IFPORT, ONLY: access

  IMPLICIT NONE

  ! !observables
  ! INTEGER(kind = 4), ALLOCATABLE :: spin(:, :)
  ! REAL(kind = 8), ALLOCATABLE :: m_z(:)
  ! REAL(kind = 8) :: pump, diss, energy

  ! !simulation variables
  ! INTEGER(kind = 4) :: seed_master
  ! INTEGER(kind = 4) :: seed_x, seed_z, seed_prob
  ! INTEGER(kind = 4), ALLOCATABLE :: rn_x(:, :), rn_z(:, :)
  ! REAL(kind = 8), ALLOCATABLE :: rn_prob(:, :)

  ! !loop variables
  ! INTEGER(kind = 4) :: i_line, ios, i_vel, i_sweep, i_sample, x, z
  ! INTEGER(kind = 4) :: n_samples0
  ! INTEGER(kind = 4), ALLOCATABLE :: n_sweeps_therm0(:), n_sweeps_stead0(:)
  ! CHARACTER(len = 4, kind = 1) :: si_sample
  ! CHARACTER(len = 30, kind = 1) :: &
  !      filename_stream, filename_m_z, filename_snap
  ! CHARACTER(len = 40, kind = 1) :: filename_str

  ! !slot variables
  ! INTEGER(kind = 4), ALLOCATABLE :: slot_stream(:), slot_snap(:), slot_m_z(:)

  ! !omp variables
  ! INTEGER(kind = 4) :: i_th, n_ths, err_x, err_z, err_prob
  !
  ! !stat variables
  ! INTEGER(kind = 4) :: stat_snap
  ! INTEGER(kind = 4), ALLOCATABLE :: stat_stream_e(:), stat_m_z_e(:)
  ! INTEGER(kind = 4), ALLOCATABLE :: stat_stream_a(:), stat_m_z_a(:)
  ! CHARACTER(:), ALLOCATABLE :: command_cpStream, command_cpM_z
  ! INTEGER(kind = 4) :: averaged

  CALL inputParameters_2d(len_x, len_z, J, beta, vel, &
       n_sweeps_therm, n_sweeps_stead, id_IC, id_BC, n_samples, &
       onoff_stream, onoff_m_z)
  !TODO: 対応する基底状態の順にICとBCを揃える
  ! id_IC: 1. all-up, 2. DW, 3. random
  ! id_BC: 1. anti-parallel, 2. parallel, 3. free
  ! n_sweeps = n_sweeps_therm + n_sweeps_stead

  ! ALLOCATE(n_sweeps_therm0(1:n_samples), n_sweeps_stead0(1:n_samples))
  ! n_sweeps_therm0(1:n_samples) = 0; n_sweeps_stead0(1:n_samples) = 0
  ! CALL getNumSamples(40, n_samples0)
  ALLOCATE(e_stream0(1:n_samples0), e_m_z0(1:n_samples0))
  ALLOCATE(a_stream0(1:n_samples0), e_stream0(1:n_samples0))
  CALL getStatsSamples(40, n_samples0, &
       stat_sample_e(1:n_samples0), stat_sample_a(1:n_samples0))
  DO i_sample = 1, n_samples0, 1
     IF ( stat_sample_e(i_sample) == 0 ) THEN
        WRITE(si_sample, '(i0.4)') i_sample
        command_cpStream = "cp stream.dat stream_s"//si_sample//".dat"
        command_cpM_z = "cp m_z.dat m_z_s"//si_sample//".dat"
        CALL system(command_cpStream)
        CALL system(command_cpM_z)
        CALL getNumSweeps(11, i_sample, &
             n_sweeps_therm0(i_sample), n_sweeps_stead0(i_sample))
     ELSE
        CALL getNumSweeps(11, i_sample, &
             n_sweeps_therm0(i_sample), n_sweeps_stead0(i_sample))
     END IF
  END DO
  !TODO: 本当はこれも一発で良い筈
  !（異なるn_sweeps_therm0を持つサンプルは共存できない仕様だから）

  DO i_sample = 1, n_samples, 1
     IF ( n_sweeps_therm <= n_sweeps_therm0(i_sample) ) THEN
        n_sweeps_therm = n_sweeps_therm0(i_sample)
        IF ( n_sweeps_stead <= n_sweeps_stead0(i_sample) ) THEN
           n_sweeps_stead = n_sweeps_stead0(i_sample)
        END IF
     END IF
     IF ( n_sweeps_therm > n_sweeps_therm0(i_sample) ) THEN
        n_sweeps_stead0(i_sample) = 0
     END IF
  END DO
  IF ( n_samples < n_samples0 ) THEN
     n_samples = n_samples0
  END IF

  n_ths = 1
  !$  n_ths = omp_get_max_threads()
  !$	CALL omp_set_num_threads(n_ths)
  ALLOCATE(rn_x(0:n_ths - 1, 1:len_x * len_z))
  ALLOCATE(rn_z(0:n_ths - 1, 1:len_x * len_z))
  ALLOCATE(rn_prob(0:n_ths - 1, 1:len_x * len_z))
  ALLOCATE(slot_stream(0:n_ths - 1))
  ALLOCATE(slot_m_z(0:n_ths - 1))
  ALLOCATE(slot_snap(0:n_ths - 1))

  ALLOCATE(str_x(1:n_samples), str_z(1:n_samples), str_prob(1:n_samples))

  CALL system_CLOCK(seed_master)
  !$omp parallel do default(none) &
  !$omp shared(seed_master, n_samples) &
  !$omp shared(str_x, str_z, str_prob) &
  !$omp private(seed_x, seed_z, seed_prob) &
  !$omp private(err_x, err_z, err_prob)
  DO i_sample = 1, n_samples, 1
     seed_x = seed_master + i_sample
     seed_z = seed_master + i_sample + n_samples
     seed_prob = seed_master + i_sample + 2 * n_samples
     CALL initializeRN(seed_x, str_x(i_sample), err_x)
     CALL initializeRN(seed_z, str_z(i_sample), err_z)
     CALL initializeRN(seed_prob, str_prob(i_sample), err_prob)
  END DO
  !$omp end parallel do

  DO i_th = 0, n_ths - 1
     slot_stream(i_th) = 20 + i_th
     slot_snap(i_th) = 20 + i_th + n_ths
     slot_m_z(i_th) = 20 + i_th + 2 * n_ths
  END DO

  ALLOCATE(spin(1:len_x, 1:len_z), temp_spin(1:len_x, 1:len_z / 2))
  ALLOCATE(m_z(1:len_z))

  CALL makeDeltaEArray
  CALL makeProbArray

  !If not snapshot exists, load one. Initialize Spin.
  stat_snap = access("snap_initial.dat", " ")
  IF ( stat_snap == 0 ) THEN
     CALL importSnapshot(30, "snap_initial.dat", &
          spin(1:len_x, 1:len_z))
  ELSE
     CALL initializeSpin(id_init, &
          spin(1:len_x, 1:len_z))
     CALL exportSnapshot(30, "snap_initial.dat", &
          spin(1:len_x, 1:len_z))
  END IF

  !$omp parallel do schedule(dynamic, 1) default(none) &
  !$omp firstprivate(spin) &
  !$omp private(i_th, err_x, err_z, err_prob) &
  !$omp private(i_sweep, i_vel, si_sample, x, z, n_steps) &
  !$omp private(temp_spin, pump, diss, energy, m_z) &
  !$omp private(filename_stream, filename_m_z, filename_snap, filename_str) &
  !$omp shared(n_samples, len_x, len_z, vel) &
  !$omp shared(str_x, str_z, str_prob, rn_x, rn_z, rn_prob) &
  !$omp shared(n_sweeps_therm, n_sweeps_therm0) &
  !$omp shared(n_sweeps_stead, n_sweeps_stead0) &
  !$omp shared(slot_stream, slot_m_z, slot_snap)
  DO i_sample = 1, n_samples, 1
     i_th = 0
     !$ i_th = omp_get_thread_num()

     WRITE(si_sample, '(i0.4)') i_sample
     filename_stream=TRIM("stream_s"//si_sample//".dat")
     filename_m_z=TRIM("m_z_s"//si_sample//".dat")

     IF ( n_sweeps_therm0(i_sample) > 0 ) THEN
        OPEN(slot_stream(i_th), file=filename_stream, status="old")
        READ(slot_stream(i_th), '()')
        OPEN(slot_m_z(i_th), file=filename_m_z, status="old")
        READ(slot_m_z(i_th), '()')
        filename_snap=TRIM("snap_thermalized_s"//si_sample//".dat")

        filename_str=TRIM("str_x_thermalized_s"//si_sample//".bin")
        CALL loadRNstat(str_x(i_sample), filename_str, err_x)
        filename_str=TRIM("str_z_thermalized_s"//si_sample//".bin")
        CALL loadRNstat(str_z(i_sample), filename_str, err_z)
        filename_str=TRIM("str_prob_thermalized_s"//si_sample//".bin")
        CALL loadRNstat(str_prob(i_sample), filename_str, err_prob)
     ELSE
        OPEN(slot_stream(i_th), file=filename_stream, status="new")
        WRITE(slot_stream(i_th), '(a)') "# i_sweep, pump, diss, energy"
        OPEN(slot_m_z(i_th), file=filename_m_z, status="new")
        WRITE(slot_m_z(i_th), '(a)') "# i_sweep, z, m_z"
        filename_snap=TRIM("snap_initial.dat")
     END IF
     CALL importSnapshot(slot_snap(i_th), filename_snap, &
          spin(1:len_x, 1:len_z))

     CALL calcEnergy(spin, energy)

     !Thermalize, read through.
     CALL readthroughM_z_onfile(1, n_sweeps_therm0(i_sample), &
     wh_mz, len_z, slot_m_z(i_th))
     CALL readthroughStream_onfile(1, n_sweeps_therm0(i_sample), &
     slot, i_sweep, pump, diss, energy)

     n_steps = len_x * len_z

     pump = 0.0d0

     !Thermalize.
     DO i_sweep = 1 + n_sweeps_therm0(i_sample), n_sweeps_therm, 1
        CALL generateRN_int(str_x(i_sample), 1, len_x, n_steps, &
             rn_x(i_th, 1:n_steps), err_x)
        CALL generateRN_int(str_z(i_sample), 1, len_z, n_steps, &
             rn_z(i_th, 1:n_steps), err_z)
        CALL generateRN_real(str_prob(i_sample), 0.0d0, 1.0d0, n_steps, &
             rn_prob(i_th, 1:n_steps), err_prob)

        CALL sweep_singleflip(spin, n_steps, &
             rn_x(i_th, 1:n_steps), rn_z(i_th, 1:n_steps), &
             rn_prob(i_th, 1:n_steps), diss)
        energy = energy + diss

        CALL exportM_z_onfile(wh_mz, len_x, len_z, slot_m_z(i_th), &
        i_sweep, spin)
        CALL exportStream_onfile(slot_stream(i_th), i_sweep, pump, diss, energy)
     END DO

     IF ( n_sweeps_therm > n_sweeps_therm0(i_sample) ) THEN
        WRITE(slot_stream(i_th), '(a)') "# -- Thermalized --"
     ELSE
        READ(slot_stream(i_th), '()')
     END IF

     !Save snapshot end-of-thermalize.
     filename_snap=TRIM("snap_thermalized_s"//si_sample//".dat")
     CALL exportSnapshot(slot_snap(i_th), &
          filename_snap, spin(1:len_x, 1:len_z))
     filename_str=TRIM("str_x_thermalized_s"//si_sample//".bin")
     CALL saveRNstat(str_x(i_sample), filename_str, err_x)
     filename_str=TRIM("str_z_thermalized_s"//si_sample//".bin")
     CALL saveRNstat(str_z(i_sample), filename_str, err_z)
     filename_str=TRIM("str_prob_thermalized_s"//si_sample//".bin")
     CALL saveRNstat(str_prob(i_sample), filename_str, err_prob)

     IF ( n_sweeps_therm == n_sweeps_therm0(i_sample) ) THEN
        filename_snap=TRIM("snap_steadized_s"//si_sample//".dat")
        CALL importSnapshot(slot_snap(i_th), &
             filename_snap, spin(1:len_x, 1:len_z))

        filename_str=TRIM("str_x_steadized_s"//si_sample//".bin")
        CALL loadRNstat(str_x(i_sample), filename_str, err_x)
        filename_str=TRIM("str_z_steadized_s"//si_sample//".bin")
        CALL loadRNstat(str_z(i_sample), filename_str, err_z)
        filename_str=TRIM("str_prob_steadized_s"//si_sample//".bin")
        CALL loadRNstat(str_prob(i_sample), filename_str, err_prob)
     END IF

     !Steadize, read through.
     CALL readthroughM_z_onfile(n_sweeps_therm + 1, &
     n_sweeps_therm + n_sweeps_stead0(i_sample), &
       wh_mz, len_z, slot_m_z(i_th))
     CALL readthroughStream_onfile(n_sweeps_therm + 1, &
     n_sweeps_therm + n_sweeps_stead0(i_sample), &
       slot_stream(i_th), i_sweep, pump, diss, energy)

     n_steps = len_x * len_z / vel

     !Steadize.
     DO i_sweep = n_sweeps_therm + n_sweeps_stead0(i_sample) + 1, &
          n_sweeps_therm + n_sweeps_stead, 1
        DO i_vel = 1, vel, 1
           CALL generateRN_int(str_x(i_sample), 1, len_x, n_steps, &
                rn_x(i_th, 1:n_steps), err_x)
           CALL generateRN_int(str_z(i_sample), 1, len_z, n_steps, &
                rn_z(i_th, 1:n_steps), err_z)
           CALL generateRN_real(str_prob(i_sample), 0.0d0, 1.0d0, n_steps, &
                rn_prob(i_th, 1:n_steps), err_prob)

           CALL shiftUpperHalf(spin, pump, temp_spin)
           energy = energy + pump
           CALL sweep_singleflip(spin, n_steps, &
                rn_x(i_th, 1:n_steps), &
                rn_z(i_th, 1:n_steps), &
                rn_prob(i_th, 1:n_steps), diss)
           energy = energy + diss
        END DO

        CALL exportM_z_onfile(wh_mz, len_x, len_z, slot_m_z(i_th), &
        i_sweep, spin)
        CALL exportStream_onfile(slot_stream(i_th), i_sweep, pump, diss, energy)
     END DO

     WRITE(slot_stream(i_th), '(a)') "# -- Steadized --"

     CLOSE(slot_stream(i_th))
     CLOSE(slot_m_z(i_th))

     !Save snapshot end-of-steadize.
     filename_snap=TRIM("snap_steadized_s"//si_sample//".dat")
     CALL exportSnapshot(slot_snap(i_th), filename_snap, spin(1:len_x, 1:len_z))

     filename_str=TRIM("str_x_steadized_s"//si_sample//".bin")
     CALL saveRNstat(str_x(i_sample), filename_str, err_x)
     filename_str=TRIM("str_z_steadized_s"//si_sample//".bin")
     CALL saveRNstat(str_z(i_sample), filename_str, err_z)
     filename_str=TRIM("str_prob_steadized_s"//si_sample//".bin")
     CALL saveRNstat(str_prob(i_sample), filename_str, err_prob)
  END DO
  !$omp end parallel do

  !$omp parallel do default(none) &
  !$omp shared(n_samples) &
  !$omp shared(str_x, str_z, str_prob) &
  !$omp private(err_x, err_z, err_prob)
  DO i_sample = 1, n_samples
     CALL destractRN(str_x(i_sample), err_x)
     CALL destractRN(str_z(i_sample), err_x)
     CALL destractRN(str_prob(i_sample), err_prob)
  END DO
  !$omp end parallel do

  if ( n_sweeps_therm > n_sweeps_therm0(i_sample) &
       .OR. n_sweeps_stead > n_sweeps_stead0(i_sample) ) then
    averaged = 0
  ELSE
    averaged = 1
  end if

  wh_stream = 1
  exists_m_z

  CALL refreshList_samples(slot, filename, &
    n_samples, len_x, len_z, J, beta, vel, &
    n_sweeps_therm, n_sweeps_stead, id_IC, id_BC, &
    e_stream, e_m_z, a_stream(1:n_samples), a_m_z(1:n_samples))
END PROGRAM main
