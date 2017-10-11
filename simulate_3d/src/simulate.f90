PROGRAM main
  !$  USE omp_lib
  USE global_variables
  USE mod_rng
  USE main_procedures
  USE IFPORT, ONLY: access

  IMPLICIT NONE

  !observables
  INTEGER(kind = 4), ALLOCATABLE :: spin(:, :, :), temp_spin(:, :, :)
  REAL(kind = 8), ALLOCATABLE :: m_z(:)
  REAL(kind = 8) :: pump, diss, energy

  !simulation variables
  INTEGER(kind = 4) :: seed_master
  INTEGER(kind = 4) :: seed_x, seed_y, seed_z, seed_prob
  INTEGER(kind = 4) :: lb_rn, ub_rn, len_rns
  INTEGER(kind = 4), ALLOCATABLE :: rn_x(:, :), rn_y(:, :), rn_z(:, :)
  REAL(kind = 8), ALLOCATABLE :: rn_prob(:, :)
  ! REAL(kind = 8), ALLOCATABLE :: sw_x(:), sw_z(:), sw_p(:)
  ! TYPE(type_rand), ALLOCATABLE :: mrc_x(:), mrc_z(:), mrc_p(:)

  !loop variables
  INTEGER(kind = 4) :: i_line, ios, i_vel, i_sweep, i_sample, x, y, z
  INTEGER(kind = 4) :: n_samples0
  INTEGER(kind = 4), ALLOCATABLE :: n_sweeps_therm0(:), n_sweeps_stead0(:)
  CHARACTER(len = 4, kind = 1) :: si_sample
  CHARACTER(len = 30, kind = 1) :: &
       filename_stream, filename_m_z, filename_snap
  CHARACTER(len = 40, kind = 1) :: filename_str

  !slot variables
  INTEGER(kind = 4), ALLOCATABLE :: slot_stream(:), slot_snap(:), slot_m_z(:)

  !omp variables
  INTEGER(kind = 4) :: i_th, n_ths, err_x, err_y, err_z, err_prob

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

  len_rns = len_x * len_y * len_z
  ALLOCATE(rn_x(0:n_ths - 1, 1:len_rns))
  ALLOCATE(rn_z(0:n_ths - 1, 1:len_rns))
  ALLOCATE(rn_prob(0:n_ths - 1, 1:len_rns))
  ALLOCATE(str_x(0:n_ths - 1), str_y(0:n_ths - 1), str_z(0:n_ths - 1), &
       str_prob(0:n_ths - 1))
  ALLOCATE(slot_stream(0:n_ths - 1), slot_snap(0:n_ths - 1), &
       slot_m_z(0:n_ths - 1))

  CALL system_CLOCK(seed_master)
  !$omp parallel do default(none) &
  !$omp shared(n_ths, seed_master) &
  !$omp shared(str_x, str_y, str_z, str_prob) &
  !$omp private(seed_x, seed_y, seed_z, seed_prob) &
  !$omp private(err_x, err_y, err_z, err_prob)
  DO i_th = 0, n_ths - 1
     seed_x = seed_master + i_th
     seed_y = seed_x + n_ths
     seed_z = seed_y + n_ths
     seed_prob = seed_z + n_ths
     CALL initializeRN(seed_x, str_x(i_th), err_x)
     CALL initializeRN(seed_y, str_y(i_th), err_y)
     CALL initializeRN(seed_z, str_z(i_th), err_z)
     CALL initializeRN(seed_prob, str_prob(i_th), err_prob)
  END DO
  !$omp end parallel do

  DO i_th = 0, n_ths - 1
     slot_stream(i_th) = 20 + i_th
     slot_snap(i_th) = slot_stream(i_th) + n_ths
     slot_m_z(i_th) = slot_snap(i_th) + n_ths
  END DO

  ALLOCATE(spin(1:len_x, 1:len_y, 1:len_z), &
       temp_spin(1:len_x, 1:len_y, 1:len_z / 2))
  ALLOCATE(m_z(1:len_z))

  CALL makeDeltaEArray
  CALL makeProbArray

  ! スナップショットがあれば読み込み，なければスピンを初期化
  stat_snap = access("snap_initial.dat", " ")
  IF ( stat_snap == 0 ) THEN
     CALL importSnapshot(30, "snap_initial.dat", &
          spin(1:len_x, 1:len_y, 1:len_z))
  ELSE
     CALL initializeSpin(id_init, &
          spin(1:len_x, 1:len_y, 1:len_z))
     CALL exportSnapshot(30, "snap_initial.dat", &
          spin(1:len_x, 1:len_y, 1:len_z))
  END IF

  !$omp parallel do default(none) &
  !$omp firstprivate(spin, str_x, str_y, str_z, str_prob) &
  !$omp private(err_x, err_y, err_z, err_prob, modifying) &
  !$omp private(i_sweep, i_vel, i_th, si_sample, x, y, z, n_steps) &
  !$omp private(temp_spin, pump, diss, energy, m_z) &
  !$omp private(filename_stream, filename_m_z, filename_snap, filename_str) &
  !$omp shared(n_samples, len_x, len_y, len_z, vel, len_rns) &
  !$omp shared(rn_x, rn_y, rn_z, rn_prob) &
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
        CALL loadRNstat(str_x(i_th), filename_str, err_x)
        filename_str=TRIM("str_z_thermalized_s"//si_sample//".bin")
        CALL loadRNstat(str_z(i_th), filename_str, err_z)
        filename_str=TRIM("str_prob_thermalized_s"//si_sample//".bin")
        CALL loadRNstat(str_prob(i_th), filename_str, err_prob)
     ELSE
        OPEN(slot_stream(i_th), file=filename_stream, status="new")
        WRITE(slot_stream(i_th), '(a)') "# i_sweep, pump, diss, energy"
        OPEN(slot_m_z(i_th), file=filename_m_z, status="new")
        WRITE(slot_m_z(i_th), '(a)') "# i_sweep, z, m_z"
        filename_snap=TRIM("snap_initial.dat")
     END IF
     CALL importSnapshot(slot_snap(i_th), filename_snap, &
          spin(1:len_x, 1:len_y, 1:len_z))

     CALL calcEnergy(spin, energy)

     !  平衡化・空読み
     DO i_sweep = 1, n_sweeps_therm0(i_sample), 1
        DO z = 1, len_z, 1
           READ(slot_m_z(i_th), '()')
        END DO
        READ(slot_m_z(i_th), '()')
        READ(slot_stream(i_th), '()')
     END DO

     n_steps = len_x * len_y * len_z

     !  平衡化
     DO i_sweep = 1 + n_sweeps_therm0(i_sample), n_sweeps_therm, 1
        CALL generateRN_int(str_x(i_th), 1, len_x, n_steps, &
             rn_x(i_th, :), err_x)
        CALL generateRN_int(str_y(i_th), 1, len_y, n_steps, &
             rn_y(i_th, :), err_y)
        CALL generateRN_int(str_z(i_th), 1, len_z, n_steps, &
             rn_z(i_th, :), err_z)
        CALL generateRN_real(str_prob(i_th), 0.0d0, 1.0d0, n_steps, &
             rn_prob(i_th, :), err_prob)
        ! lb_rn = (i_sweep - 1) * n_steps + 1
        ! ub_rn = i_sweep * n_steps

        CALL sweep_singleflip(spin, n_steps, &
             rn_x(i_th, :), rn_y(i_th, :), rn_z(i_th, :), &
             rn_prob(i_th, :), diss)
        energy = energy + diss

        DO z = 1, len_z, 1
           m_z(z) = DBLE(SUM(spin(1:len_x, 1:len_y, z))) / DBLE(len_x * len_y)
           WRITE(slot_m_z(i_th), '( i5, a, i5, a, f0.4)') &
                i_sweep, ", ", z, ", ", m_z(z)
        END DO
        WRITE(slot_m_z(i_th), '()')

        WRITE(slot_stream(i_th), '(i5,  a, f0.4, a, f0.4, a, f0.4, a, &
             f0.4, a, f0.4, a, f0.4)') &
             i_sweep, ", ", 0.0d0, ", ", diss, ", ", energy, ", ", &
             0.0d0, ", ", 0.0d0, ", ", 0.0d0
     END DO

     IF ( n_sweeps_therm > n_sweeps_therm0(i_sample) ) THEN
        WRITE(slot_stream(i_th), '(a)') "# -- Thermalized --"
     ELSE
        READ(slot_stream(i_th), '()')
     END IF

     !平衡化後のスナップの保存
     filename_snap=TRIM("snap_thermalized_s"//si_sample//".dat")
     CALL exportSnapshot(slot_snap(i_th), &
          filename_snap, spin(1:len_x, 1:len_y, 1:len_z))
     filename_str=TRIM("str_x_thermalized_s"//si_sample//".bin")
     CALL saveRNstat(str_x(i_th), filename_str, err_x)
     filename_str=TRIM("str_y_thermalized_s"//si_sample//".bin")
     CALL saveRNstat(str_y(i_th), filename_str, err_y)
     filename_str=TRIM("str_z_thermalized_s"//si_sample//".bin")
     CALL saveRNstat(str_z(i_th), filename_str, err_z)
     filename_str=TRIM("str_prob_thermalized_s"//si_sample//".bin")
     CALL saveRNstat(str_prob(i_th), filename_str, err_prob)

     IF ( n_sweeps_therm == n_sweeps_therm0(i_sample) ) THEN
        filename_snap=TRIM("snap_steadized_s"//si_sample//".dat")
        CALL importSnapshot(slot_snap(i_th), &
             filename_snap, spin(1:len_x, 1:len_y, 1:len_z))

        filename_str=TRIM("str_x_steadized_s"//si_sample//".bin")
        CALL loadRNstat(str_x(i_th), filename_str, err_x)
        filename_str=TRIM("str_y_steadized_s"//si_sample//".bin")
        CALL loadRNstat(str_y(i_th), filename_str, err_y)
        filename_str=TRIM("str_z_steadized_s"//si_sample//".bin")
        CALL loadRNstat(str_z(i_th), filename_str, err_z)
        filename_str=TRIM("str_prob_steadized_s"//si_sample//".bin")
        CALL loadRNstat(str_prob(i_th), filename_str, err_prob)
     END IF

     !  定常化・空読み
     DO i_sweep = n_sweeps_therm + 1, &
          n_sweeps_therm + n_sweeps_stead0(i_sample), 1
        DO z = 1, len_z, 1
           READ(slot_m_z(i_th), '()')
        END DO
        READ(slot_m_z(i_th), '()')

        READ(slot_stream(i_th), '()')
     END DO

     n_steps = len_x * len_y * len_z / vel

     !  定常化
     DO i_sweep = n_sweeps_therm + n_sweeps_stead0(i_sample) + 1, &
          n_sweeps_therm + n_sweeps_stead, 1
        DO i_vel = 1, vel, 1
           CALL generateRN_int(str_x(i_th), 1, len_x, n_steps, &
                rn_x(i_th, :), err_x)
           CALL generateRN_int(str_y(i_th), 1, len_y, n_steps, &
                rn_y(i_th, :), err_y)
           CALL generateRN_int(str_z(i_th), 1, len_z, n_steps, &
                rn_z(i_th, :), err_z)
           CALL generateRN_real(str_prob(i_th), 0.0d0, 1.0d0, n_steps, &
                rn_prob(i_th, :), err_prob)
           !  lb_rn = (i_vel - 1 + vel * (i_sweep - 1)) * n_steps  + 1
           !  ub_rn = (i_vel + vel * (i_sweep - 1)) * n_steps
           !  CALL chargeRandomNumbers(  n_spins / vel, i_sweep, &
           !       mrc_x, mrc_z, mrc_p, sw_x, sw_z, sw_p)
           CALL shiftUpperHalf(spin, pump, temp_spin)
           energy = energy + pump
           CALL sweep_singleflip(spin, n_steps, &
                rn_x(i_th, 1:n_steps), &
                rn_y(i_th, 1:n_steps), &
                rn_z(i_th, 1:n_steps), &
                rn_prob(i_th, 1:n_steps), diss)
           energy = energy + diss
        END DO

        DO z = 1, len_z, 1
           m_z(z) = DBLE(SUM(spin(1:len_x, 1:len_y, z))) / DBLE(len_x * len_y)
           WRITE(slot_m_z(i_th), '( i5, a, i5, a, f0.4)') &
                i_sweep, ", ", z, ", ", m_z(z)
        END DO
        WRITE(slot_m_z(i_th), '()')

        WRITE(slot_stream(i_th), '(  i5, a, f0.4, a, f0.4, a, f0.4, a, &
             f0.4, a, f0.4, a, f0.4)') &
             i_sweep, ", ", pump, ", ", diss, ", ", energy, ", ", &
             0.0d0, ", ", 0.0d0, ", ", 0.0d0
     END DO

     WRITE(slot_stream(i_th), '(a)') "# -- Steadized --"

     CLOSE(slot_stream(i_th))
     CLOSE(slot_m_z(i_th))

     !定常化後のスナップの保存
     filename_snap=TRIM("snap_steadized_s"//si_sample//".dat")
     CALL exportSnapshot(slot_snap(i_th), filename_snap, &
          spin(1:len_x, 1:len_y, 1:len_z))

     filename_str=TRIM("str_x_steadized_s"//si_sample//".bin")
     CALL saveRNstat(str_x(i_th), filename_str, err_x)
     filename_str=TRIM("str_y_steadized_s"//si_sample//".bin")
     CALL saveRNstat(str_y(i_th), filename_str, err_y)
     filename_str=TRIM("str_z_steadized_s"//si_sample//".bin")
     CALL saveRNstat(str_z(i_th), filename_str, err_z)
     filename_str=TRIM("str_prob_steadized_s"//si_sample//".bin")
     CALL saveRNstat(str_prob(i_th), filename_str, err_prob)

     !  modifying = LOGICAL(n_sweeps_therm > n_sweeps_therm0(i_sample) &
     !  .OR. n_sweeps_stead > n_sweeps_stead0(i_sample))
     !  !$omp atomic
     !  WRITE(10, '(i5, a, i2, a, i2)') i_sample, ", ", 1, ", ", modifying*1 + 1
     !  !$omp end atomic
  END DO
  !$omp end parallel do

  !$omp parallel do default(none) &
  !$omp shared(n_ths) &
  !$omp shared(str_x, str_z, str_prob) &
  !$omp private(err_x, err_z, err_prob)
  DO i_th = 0, n_ths - 1
     CALL destractRN(str_x(i_th), err_x)
     CALL destractRN(str_y(i_th), err_y)
     CALL destractRN(str_z(i_th), err_x)
     CALL destractRN(str_prob(i_th), err_prob)
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
