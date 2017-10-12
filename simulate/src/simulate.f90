PROGRAM main
  !$  USE omp_lib
  USE global_variables
  USE mod_rng
  USE main_procedures
  USE IFPORT, ONLY: access

  IMPLICIT NONE

  !observables
  INTEGER(kind = 4), ALLOCATABLE :: spin(:, :), temp_spin(:, :)
  REAL(kind = 8), ALLOCATABLE :: m_z(:)
  REAL(kind = 8) :: pump, diss, energy

  !simulation variables
  INTEGER(kind = 4) :: seed_master
  INTEGER(kind = 4) :: seed_x, seed_z, seed_prob
  INTEGER(kind = 4), ALLOCATABLE :: rn_x(:, :), rn_z(:, :)
  REAL(kind = 8), ALLOCATABLE :: rn_prob(:, :)

  !loop variables
  INTEGER(kind = 4) :: i_line, ios, i_vel, i_sweep, i_sample, x, z
  INTEGER(kind = 4) :: n_samples0
  INTEGER(kind = 4), ALLOCATABLE :: n_sweeps_therm0(:), n_sweeps_stead0(:)
  CHARACTER(len = 4, kind = 1) :: si_sample
  CHARACTER(len = 30, kind = 1) :: &
       filename_stream, filename_m_z, filename_snap
  CHARACTER(len = 40, kind = 1) :: filename_str

  !slot variables
  INTEGER(kind = 4), ALLOCATABLE :: slot_stream(:), slot_snap(:), slot_m_z(:)

  !omp variables
  INTEGER(kind = 4) :: n_ths, err_x, err_z, err_prob

  !stat variables
  INTEGER(kind = 4) :: stat_snap
  INTEGER(kind = 4), ALLOCATABLE :: stat_sample_e(:), stat_sample_a(:)
  CHARACTER(:), ALLOCATABLE :: command_cpStream, command_cpM_z
  LOGICAL(kind = 8) :: modifying

  CALL inputParameters
  ALLOCATE(n_sweeps_therm0(1:n_samples), n_sweeps_stead0(1:n_samples))
  n_sweeps_therm0(1:n_samples) = 0
  n_sweeps_stead0(1:n_samples) = 0
  CALL getNumSamples(40, n_samples0)
  ALLOCATE(stat_sample_e(1:n_samples0), stat_sample_a(1:n_samples0))
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

  !$omp parallel default(none) &
  !$omp shared(n_ths)
  n_ths = 1
  !$  n_ths = omp_get_max_threads()
  !$omp end parallel
  ALLOCATE(rn_x(0:n_ths - 1, 1:len_x * len_z))
  ALLOCATE(rn_z(0:n_ths - 1, 1:len_x * len_z))
  ALLOCATE(rn_prob(0:n_ths - 1, 1:len_x * len_z))

  ALLOCATE(str_x(1:n_samples), str_z(1:n_samples), str_prob(1:n_samples))

  CALL system_CLOCK(seed_master)
  !$omp parallel do default(none) &
  !$omp shared(seed_master) &
  !$omp shared(str_x, str_z, str_prob) &
  !$omp private(seed_x, seed_z, seed_prob) &
  !$omp private(err_x, err_z, err_prob)
  DO i_sample = 1, n_samples, 1
     seed_x = seed_master + i_sample
     seed_z = seed_master + i_sample + n_samples
     seed_prob = seed_master + i_sample + n_samples
     CALL initializeRN(seed_x, str_x(i_sample), err_x)
     CALL initializeRN(seed_z, str_z(i_sample), err_z)
     CALL initializeRN(seed_prob, str_prob(i_sample), err_prob)
  END DO
  !$omp end parallel do

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

  !$omp parallel do default(none) &
  !$omp firstprivate(spin) &
  !$omp private(err_x, err_z, err_prob) &
  !$omp private(i_sweep, i_vel, si_sample, x, z, n_steps) &
  !$omp private(temp_spin, pump, diss, energy, m_z) &
  !$omp private(filename_stream, filename_m_z, filename_snap, filename_str) &
  !$omp shared(n_samples, len_x, len_z, vel) &
  !$omp shared(str_x, str_z, str_prob, rn_x, rn_z, rn_prob) &
  !$omp shared(n_sweeps_therm, n_sweeps_therm0) &
  !$omp shared(n_sweeps_stead, n_sweeps_stead0)
  DO i_sample = 1, n_samples, 1
     WRITE(si_sample, '(i0.4)') i_sample
     filename_stream=TRIM("stream_s"//si_sample//".dat")
     filename_m_z=TRIM("m_z_s"//si_sample//".dat")

     IF ( n_sweeps_therm0(i_sample) > 0 ) THEN
        OPEN(i_sample, file=filename_stream, status="old")
        READ(i_sample, '()')
        OPEN(i_sample + n_ths, file=filename_m_z, status="old")
        READ(i_sample + n_ths, '()')
        filename_snap=TRIM("snap_thermalized_s"//si_sample//".dat")

        filename_str=TRIM("str_x_thermalized_s"//si_sample//".bin")
        CALL loadRNstat(str_x(i_sample), filename_str, err_x)
        filename_str=TRIM("str_z_thermalized_s"//si_sample//".bin")
        CALL loadRNstat(str_z(i_sample), filename_str, err_z)
        filename_str=TRIM("str_prob_thermalized_s"//si_sample//".bin")
        CALL loadRNstat(str_prob(i_sample), filename_str, err_prob)
     ELSE
        OPEN(i_sample, file=filename_stream, status="new")
        WRITE(i_sample, '(a)') "# i_sweep, pump, diss, energy"
        OPEN(i_sample + n_ths, file=filename_m_z, status="new")
        WRITE(i_sample + n_ths, '(a)') "# i_sweep, z, m_z"
        filename_snap=TRIM("snap_initial.dat")
     END IF
     CALL importSnapshot(i_sample + n_ths * 2, filename_snap, spin(1:len_x, 1:len_z))

     CALL calcEnergy(spin, energy)

     !Thermalize, read through.
     DO i_sweep = 1, n_sweeps_therm0(i_sample), 1
        DO z = 1, len_z, 1
           READ(i_sample + n_ths, '()')
        END DO
        READ(i_sample + n_ths, '()')
        READ(i_sample, '()')
     END DO

     n_steps = len_x * len_z

     !Thermalize.
     DO i_sweep = 1 + n_sweeps_therm0(i_sample), n_sweeps_therm, 1
        CALL generateRN_int(str_x(i_sample), 1, len_x, n_steps, &
             rn_x(i_sample, 1:n_steps), err_x)
        CALL generateRN_int(str_z(i_sample), 1, len_z, n_steps, &
             rn_z(i_sample, 1:n_steps), err_z)
        CALL generateRN_real(str_prob(i_sample), 0.0d0, 1.0d0, n_steps, &
             rn_prob(i_sample, 1:n_steps), err_prob)

        CALL sweep_singleflip(spin, n_steps, &
             rn_x(i_sample, 1:n_steps), rn_z(i_sample, 1:n_steps), &
             rn_prob(i_sample, 1:n_steps), diss)
        energy = energy + diss

        DO z = 1, len_z, 1
           m_z(z) = DBLE(SUM(spin(1:len_x, z), dim=1)) / DBLE(len_x)
           WRITE(i_sample + n_ths, '( i5, a, i5, a, f0.4)') &
                i_sweep, ", ", z, ", ", m_z(z)
        END DO
        WRITE(i_sample + n_ths, '()')

        WRITE(i_sample, '(i5,  a, f0.4, a, f0.4, a, f0.4, a, &
             f0.4, a, f0.4, a, f0.4)') &
             i_sweep, ", ", 0.0d0, ", ", diss, ", ", energy, ", ", &
             0.0d0, ", ", 0.0d0, ", ", 0.0d0
     END DO

     IF ( n_sweeps_therm > n_sweeps_therm0(i_sample) ) THEN
        WRITE(i_sample, '(a)') "# -- Thermalized --"
     ELSE
        READ(i_sample, '()')
     END IF

     !Save snapshot end-of-thermalize.
     filename_snap=TRIM("snap_thermalized_s"//si_sample//".dat")
     CALL exportSnapshot(i_sample + n_ths * 2, &
          filename_snap, spin(1:len_x, 1:len_z))
     filename_str=TRIM("str_x_thermalized_s"//si_sample//".bin")
     CALL saveRNstat(str_x(i_sample), filename_str, err_x)
     filename_str=TRIM("str_z_thermalized_s"//si_sample//".bin")
     CALL saveRNstat(str_z(i_sample), filename_str, err_z)
     filename_str=TRIM("str_prob_thermalized_s"//si_sample//".bin")
     CALL saveRNstat(str_prob(i_sample), filename_str, err_prob)

     IF ( n_sweeps_therm == n_sweeps_therm0(i_sample) ) THEN
        filename_snap=TRIM("snap_steadized_s"//si_sample//".dat")
        CALL importSnapshot(i_sample + n_ths * 2, &
             filename_snap, spin(1:len_x, 1:len_z))

        filename_str=TRIM("str_x_steadized_s"//si_sample//".bin")
        CALL loadRNstat(str_x(i_sample), filename_str, err_x)
        filename_str=TRIM("str_z_steadized_s"//si_sample//".bin")
        CALL loadRNstat(str_z(i_sample), filename_str, err_z)
        filename_str=TRIM("str_prob_steadized_s"//si_sample//".bin")
        CALL loadRNstat(str_prob(i_sample), filename_str, err_prob)
     END IF

     !Steadize, read through.
     DO i_sweep = n_sweeps_therm + 1, &
          n_sweeps_therm + n_sweeps_stead0(i_sample), 1
        DO z = 1, len_z, 1
           READ(i_sample + n_ths, '()')
        END DO
        READ(i_sample + n_ths, '()')

        READ(i_sample, '()')
     END DO

     n_steps = len_x * len_z / vel

     !Steadize.
     DO i_sweep = n_sweeps_therm + n_sweeps_stead0(i_sample) + 1, &
          n_sweeps_therm + n_sweeps_stead, 1
        DO i_vel = 1, vel, 1
           CALL generateRN_int(str_x(i_sample), 1, len_x, n_steps, &
                rn_x(i_sample, 1:n_steps), err_x)
           CALL generateRN_int(str_z(i_sample), 1, len_z, n_steps, &
                rn_z(i_sample, 1:n_steps), err_z)
           CALL generateRN_real(str_prob(i_sample), 0.0d0, 1.0d0, n_steps, &
                rn_prob(i_sample, 1:n_steps), err_prob)

           CALL shiftUpperHalf(spin, pump, temp_spin)
           energy = energy + pump
           CALL sweep_singleflip(spin, n_steps, &
                rn_x(i_sample, 1:n_steps), &
                rn_z(i_sample, 1:n_steps), &
                rn_prob(i_sample, 1:n_steps), diss)
           energy = energy + diss
        END DO

        DO z = 1, len_z, 1
           m_z(z) = DBLE(SUM(spin(1:len_x, z), dim=1)) / DBLE(len_x)
           WRITE(i_sample + n_ths, '( i5, a, i5, a, f0.4)') &
                i_sweep, ", ", z, ", ", m_z(z)
        END DO
        WRITE(i_sample + n_ths, '()')

        WRITE(i_sample, '(  i5, a, f0.4, a, f0.4, a, f0.4, a, &
             f0.4, a, f0.4, a, f0.4)') &
             i_sweep, ", ", pump, ", ", diss, ", ", energy, ", ", &
             0.0d0, ", ", 0.0d0, ", ", 0.0d0
     END DO

     WRITE(i_sample, '(a)') "# -- Steadized --"

     CLOSE(i_sample)
     CLOSE(i_sample + n_ths)

     !Save snapshot end-of-steadize.
     filename_snap=TRIM("snap_steadized_s"//si_sample//".dat")
     CALL exportSnapshot(i_sample + n_ths * 2, filename_snap, spin(1:len_x, 1:len_z))

     filename_str=TRIM("str_x_steadized_s"//si_sample//".bin")
     CALL saveRNstat(str_x(i_sample), filename_str, err_x)
     filename_str=TRIM("str_z_steadized_s"//si_sample//".bin")
     CALL saveRNstat(str_z(i_sample), filename_str, err_z)
     filename_str=TRIM("str_prob_steadized_s"//si_sample//".bin")
     CALL saveRNstat(str_prob(i_sample), filename_str, err_prob)
  END DO
  !$omp end parallel do

  !$omp parallel do default(none) &
  !$omp shared(n_ths) &
  !$omp shared(str_x, str_z, str_prob) &
  !$omp private(err_x, err_z, err_prob)
  DO i_sample = 0, n_ths - 1
     CALL destractRN(str_x(i_sample), err_x)
     CALL destractRN(str_z(i_sample), err_x)
     CALL destractRN(str_prob(i_sample), err_prob)
  END DO
  !$omp end parallel do

  OPEN(10, file="list_samples.dat", status="old")
  DO i_sample = 1, n_samples, 1
     modifying = LOGICAL(n_sweeps_therm > n_sweeps_therm0(i_sample) &
          .OR. n_sweeps_stead > n_sweeps_stead0(i_sample))
     WRITE(10, '(i5, a, i2, a, i2)') i_sample, ", ", 1, ", ", modifying*1 + 1
  END DO
  CLOSE(10)
END PROGRAM main
