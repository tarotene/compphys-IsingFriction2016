! MODULE global_variables
!   IMPLICIT NONE
!
!   !input parameters
!   INTEGER(kind = 4), SAVE :: len_x, len_z
!
!   !parameters
!   INTEGER(kind = 4), SAVE :: n_samples, n_samples_old, n_samples_new
!
!   !reduced parameters
!   INTEGER(kind = 4), SAVE :: n_sweeps_therm, n_sweeps_stead, n_sweeps
! END MODULE global_variables

! MODULE main_procedures
!   USE global_variables
!   ! USE sub_procedures
!   USE IFPORT
!   IMPLICIT NONE
! CONTAINS
! 	SUBROUTINE readStream(i_sample, slot, pump, diss, energy, &
!        fluc_pump, fluc_diss, fluc_energy)
!     INTEGER(kind = 4), INTENT(in) :: i_sample, slot
!     REAL(kind = 8), INTENT(out) :: pump(1:), diss(1:), energy(1:)
!     REAL(kind = 8), INTENT(out), OPTIONAL :: fluc_pump(1:)
!     REAL(kind = 8), INTENT(out), OPTIONAL :: fluc_diss(1:)
!     REAL(kind = 8), INTENT(out), OPTIONAL :: fluc_energy(1:)
!
!     CHARACTER(len = 4) :: si_sample
!     CHARACTER(len = 30) :: filename
!     INTEGER(kind = 4) :: i_sweep, i_dum
!
!     IF ( i_sample == 0 ) THEN
!        filename=TRIM("stream.dat")
!     ELSE
!        WRITE(si_sample, '(i0.4)') i_sample
!        filename=TRIM("stream_s"//si_sample//".dat")
!     END IF
!
!     OPEN(slot, file=filename, status="old")
!     READ(slot, '()')
!     DO i_sweep = 1, n_sweeps_therm, 1
!        READ(slot, *)	i_dum, pump(i_sweep), diss(i_sweep), energy(i_sweep), &
!             fluc_pump(i_sweep), fluc_diss(i_sweep), fluc_energy(i_sweep)
!     END DO
!     READ(slot, '()')
!     DO i_sweep = n_sweeps_therm + 1, n_sweeps_therm + n_sweeps_stead, 1
!        READ(slot, *)	i_dum, pump(i_sweep), diss(i_sweep), energy(i_sweep), &
!             fluc_pump(i_sweep), fluc_diss(i_sweep), fluc_energy(i_sweep)
!     END DO
!     CLOSE(slot)
!   END SUBROUTINE readStream
!
!   ! SUBROUTINE readSpin(i_sample, slot, spin)
!   !   INTEGER(kind = 4), INTENT(in) :: i_sample, slot
!   !   INTEGER(kind = 4), INTENT(out) :: spin(1:, 1:, 1:)
!   !
!   !   CHARACTER(len = 4) :: si_sample
!   !   CHARACTER(len = 30) :: filename
!   !   INTEGER(kind = 4) :: i_sweep, i_dum, x, x_dum, z, z_dum
!   !
!   !   WRITE(si_sample, '(i0.4)') i_sample
!   !   filename=TRIM("spin_s"//si_sample//".dat")
!   !
!   !   OPEN(slot, file=filename, status="old")
!   !   DO i_sweep = 1, n_sweeps, 1
!   !      DO z = 1, len_z + 1, 1
!   !         READ(slot, '()')
!   !         DO x = 1, len_x + 1, 1
!   !            READ(slot, *) i_dum, x_dum, z_dum, spin(i_sweep, x, z)
!   !         END DO
!   !      END DO
!   !   END DO
!   !   CLOSE(slot)
!   ! END SUBROUTINE readSpin
!
!   SUBROUTINE readM_z(i_sample, slot, m_z, fluc_m_z)
!     INTEGER(kind = 4), INTENT(in) :: i_sample, slot
!     REAL(kind = 8), INTENT(out) :: m_z(1:, 1:)
!     REAL(kind = 8), INTENT(out), OPTIONAL :: fluc_m_z(1:, 1:)
!
!     CHARACTER(len = 4) :: si_sample
!     CHARACTER(len = 30) :: filename
!     INTEGER(kind = 4) :: i_sweep, i_dum, z, z_dum
!
!     IF ( i_sample == 0 ) THEN
!        filename=TRIM("m_z.dat")
!        OPEN(slot, file=filename, status="old")
!        DO i_sweep = 1, n_sweeps, 1
!           READ(slot, '()')
!           DO z = 1, len_z, 1
!              READ(slot, *) i_dum, z_dum, m_z(z, i_sweep), fluc_m_z(z, i_sweep)
!           END DO
!        END DO
!        CLOSE(slot)
!     ELSE
!        WRITE(si_sample, '(i0.4)') i_sample
!        filename=TRIM("m_z_s"//si_sample//".dat")
!        OPEN(slot, file=filename, status="old")
!        DO i_sweep = 1, n_sweeps, 1
!           READ(slot, '()')
!           DO z = 1, len_z, 1
!              READ(slot, *) i_dum, z_dum, m_z(z, i_sweep)
!           END DO
!        END DO
!        CLOSE(slot)
!     END IF
!   END SUBROUTINE readM_z
!
!   SUBROUTINE getNumSamples(slot, n_samples)
!     INTEGER(kind = 4), INTENT(in) :: slot
!     INTEGER(kind = 4), INTENT(out) :: n_samples
!
!     INTEGER(kind = 4) :: i_line, ios, n_dum
!
!     n_samples = 0
!     OPEN(slot, file="list_samples.dat", status="old")
!     DO i_line = 1, 10000, 1
!        READ (slot, *, iostat = ios) n_dum
!        IF (ios < 0) EXIT
!        n_samples = n_samples + 1
!     END DO
!     CLOSE(slot)
!   END SUBROUTINE getNumSamples
!
!   SUBROUTINE getStatsSamples(slot, n_samples, stat_sample_e, stat_sample_a)
!     INTEGER(kind = 4), INTENT(in) :: slot, n_samples
!     INTEGER(kind = 4), INTENT(inout) :: stat_sample_e(1:)
!     INTEGER(kind = 4), INTENT(inout) :: stat_sample_a(1:)
!
!     INTEGER(kind = 4) :: i_sample, n_dum, i_dum, e_dum, a_dum
!
!     OPEN(slot, file="list_samples.dat", status="old")
!     DO i_sample = 1, n_samples, 1
!        READ (slot, *) i_dum, e_dum, a_dum
!        stat_sample_e(i_dum) = e_dum
!        stat_sample_a(i_dum) = a_dum
!     END DO
!     CLOSE(slot)
!   END SUBROUTINE getStatsSamples
!
!   SUBROUTINE getNumSweeps(slot, i_sample, n_sweeps_therm, n_sweeps_stead)
!     INTEGER(kind = 4), INTENT(in) :: slot, i_sample
!     INTEGER(kind = 4), INTENT(out) :: n_sweeps_therm, n_sweeps_stead
!
!     CHARACTER(len = 4) :: si_sample
!     CHARACTER(len = 30) :: filename
!     INTEGER(kind = 4) :: i_line, ios, n0
!
!     n_sweeps_therm = 0
!     n_sweeps_stead = 0
!
!     WRITE(si_sample, '(i0.4)') i_sample
!     filename=TRIM("stream_s"//si_sample//".dat")
!     OPEN(slot, file=filename, status="old")
!     READ (slot, '()')
!
!     DO i_line = 1, 10000, 1
!        READ (slot, *, iostat = ios) n0
!        IF (ios == 0) THEN
!           n_sweeps_therm = n_sweeps_therm + 1
!        ELSE
!           EXIT
!        END IF
!     END DO
!
!     DO i_line = 1, 10000, 1
!        READ (slot, *, iostat = ios) n0
!        IF (ios == 0) THEN
!           n_sweeps_stead = n_sweeps_stead + 1
!        ELSE
!           EXIT
!        END IF
!     END DO
!
!     CLOSE(slot)
!   END SUBROUTINE getNumSweeps
!
!   SUBROUTINE inputParameters
!     ! WRITE(0, '(a)') "n_sweeps_therm, n_sweeps_stead = ?"
!     READ(*, *) len_x, len_z
!   END SUBROUTINE inputParameters
!
!   SUBROUTINE addNewStreamSample2Sum(	i_sample, slot, &
!        sum_pump, sum_diss, sum_energy)
!     INTEGER(kind = 4), INTENT(in) :: i_sample, slot
!     REAL(kind = 8), INTENT(inout) :: sum_pump(1:)
!     REAL(kind = 8), INTENT(inout) :: sum_diss(1:)
!     REAL(kind = 8), INTENT(inout) :: sum_energy(1:)
!
!     REAL(kind = 8) :: pump(1:n_sweeps)
!     REAL(kind = 8) :: diss(1:n_sweeps)
!     REAL(kind = 8) :: energy(1:n_sweeps)
!     REAL(kind = 8) :: fluc_pump(1:n_sweeps)
!     REAL(kind = 8) :: fluc_diss(1:n_sweeps)
!     REAL(kind = 8) :: fluc_energy(1:n_sweeps)
!
!     CALL readStream(i_sample, slot, &
!          pump(1:n_sweeps), diss(1:n_sweeps), energy(1:n_sweeps), &
!          fluc_pump(1:n_sweeps), fluc_diss(1:n_sweeps), fluc_energy(1:n_sweeps))
!
!     sum_pump(1:n_sweeps) = sum_pump(1:n_sweeps) + pump(1:n_sweeps)
!     sum_diss(1:n_sweeps) = sum_diss(1:n_sweeps) + diss(1:n_sweeps)
!     sum_energy(1:n_sweeps) = sum_energy(1:n_sweeps) + energy(1:n_sweeps)
!   END SUBROUTINE addNewStreamSample2Sum
!
!   SUBROUTINE addNewStreamSample2Sumsd(	i_sample, slot, &
!        ave_pump, ave_diss, ave_energy, &
!        sumsd_pump, sumsd_diss, sumsd_energy)
!     INTEGER(kind = 4), INTENT(in) :: i_sample, slot
!     REAL(kind = 8), INTENT(in) :: ave_pump(1:)
!     REAL(kind = 8), INTENT(in) :: ave_diss(1:)
!     REAL(kind = 8), INTENT(in) :: ave_energy(1:)
!     REAL(kind = 8), INTENT(inout) :: sumsd_pump(1:)
!     REAL(kind = 8), INTENT(inout) :: sumsd_diss(1:)
!     REAL(kind = 8), INTENT(inout) :: sumsd_energy(1:)
!
!     REAL(kind = 8) :: pump(1:n_sweeps)
!     REAL(kind = 8) :: diss(1:n_sweeps)
!     REAL(kind = 8) :: energy(1:n_sweeps)
!     REAL(kind = 8) :: fluc_pump(1:n_sweeps)
!     REAL(kind = 8) :: fluc_diss(1:n_sweeps)
!     REAL(kind = 8) :: fluc_energy(1:n_sweeps)
!
!     CALL readStream(i_sample, slot, &
!          pump(1:n_sweeps), diss(1:n_sweeps), energy(1:n_sweeps), &
!          fluc_pump(1:n_sweeps), fluc_diss(1:n_sweeps), fluc_energy(1:n_sweeps))
!
!     sumsd_pump(1:n_sweeps) = sumsd_pump(1:n_sweeps) + &
!          (pump(1:n_sweeps) - ave_pump(1:n_sweeps)) ** 2 + &
!          fluc_pump(1:n_sweeps)
!     sumsd_diss(1:n_sweeps) = sumsd_diss(1:n_sweeps) + &
!          (diss(1:n_sweeps) - ave_diss(1:n_sweeps)) ** 2 + &
!          fluc_diss(1:n_sweeps)
!     sumsd_energy(1:n_sweeps) = sumsd_energy(1:n_sweeps) + &
!          (energy(1:n_sweeps) - ave_energy(1:n_sweeps)) ** 2 + &
!          fluc_energy(1:n_sweeps)
!   END SUBROUTINE addNewStreamSample2Sumsd
!
!   ! SUBROUTINE addOldStream(	slot, &
!   !      sum_pump, sum_diss, sum_energy, sumsd_pump, sumsd_diss, sumsd_energy)
!   !   INTEGER(kind = 4), INTENT(in) :: slot
! 	!
!   !   REAL(kind = 8), INTENT(inout) :: sum_pump(1:)
!   !   REAL(kind = 8), INTENT(inout) :: sum_diss(1:)
!   !   REAL(kind = 8), INTENT(inout) :: sum_energy(1:)
!   !   REAL(kind = 8), INTENT(inout) :: sumsd_pump(1:)
!   !   REAL(kind = 8), INTENT(inout) :: sumsd_diss(1:)
!   !   REAL(kind = 8), INTENT(inout) :: sumsd_energy(1:)
! 	!
!   !   REAL(kind = 8) :: ave_pump(1:n_sweeps)
!   !   REAL(kind = 8) :: ave_diss(1:n_sweeps)
!   !   REAL(kind = 8) :: ave_energy(1:n_sweeps)
!   !   REAL(kind = 8) :: fluc_pump(1:n_sweeps)
!   !   REAL(kind = 8) :: fluc_diss(1:n_sweeps)
!   !   REAL(kind = 8) :: fluc_energy(1:n_sweeps)
! 	!
!   !   CALL readStream(0, slot, &
!   !        ave_pump(1:n_sweeps), ave_diss(1:n_sweeps), ave_energy(1:n_sweeps), &
!   !        fluc_pump(1:n_sweeps), fluc_diss(1:n_sweeps), fluc_energy(1:n_sweeps))
! 	!
!   !   sum_pump(1:n_sweeps) = sum_pump(1:n_sweeps) + ave_pump(1:n_sweeps)
!   !   sum_diss(1:n_sweeps) = sum_diss(1:n_sweeps) + ave_diss(1:n_sweeps)
!   !   sum_energy(1:n_sweeps) = sum_energy(1:n_sweeps) + ave_energy(1:n_sweeps)
!   !   sumsd_pump(1:n_sweeps) = sumsd_pump(1:n_sweeps) + &
!   !        fluc_pump(1:n_sweeps)
!   !   sumsd_diss(1:n_sweeps) = sumsd_diss(1:n_sweeps) + &
!   !        fluc_diss(1:n_sweeps)
!   !   sumsd_energy(1:n_sweeps) = sumsd_energy(1:n_sweeps) + &
!   !        fluc_energy(1:n_sweeps)
!   ! END SUBROUTINE addOldStream
!
!   ! SUBROUTINE addSpinSample(i_sample, slot, ave_spin)
!   !   INTEGER(kind = 4), INTENT(in) :: i_sample, slot
!   !   REAL(kind = 8), INTENT(inout) :: ave_spin(1:, 1:, 1:)
!   !
!   !   INTEGER(kind = 4) :: spin(1:len_x + 1, 1:len_z + 1, 1:n_sweeps)
!   !
!   !   CALL readSpin(i_sample, slot, spin)
!   !   ave_spin(1:len_x + 1, 1:len_z + 1, 1:n_sweeps) = &
!   !   ave_spin(1:len_x + 1, 1:len_z + 1, 1:n_sweeps) + &
!   ! 	 DBLE(spin(1:len_x + 1, 1:len_z + 1, 1:n_sweeps))
!   ! END SUBROUTINE addSpinSample
!
!   SUBROUTINE addNewM_zSample2Sum(	i_sample, slot, sum_m_z)
!     INTEGER(kind = 4), INTENT(in) :: i_sample, slot
!     REAL(kind = 8), INTENT(inout) :: sum_m_z(1:, 1:)
!
!     REAL(kind = 8) :: m_z(1:len_z, 1:n_sweeps)
!
!     CALL readM_z(i_sample, slot, m_z(1:len_z, 1:n_sweeps))
!     sum_m_z(1:len_z, 1:n_sweeps) = sum_m_z(1:len_z, 1:n_sweeps) + m_z(1:len_z, 1:n_sweeps)
!   END SUBROUTINE addNewM_zSample2Sum
!
!   SUBROUTINE addNewM_zSample2Sumsd(	i_sample, slot, ave_m_z, sumsd_m_z)
!     INTEGER(kind = 4), INTENT(in) :: i_sample, slot
!     REAL(kind = 8), INTENT(in) :: ave_m_z(1:, 1:)
!     REAL(kind = 8), INTENT(inout) :: sumsd_m_z(1:, 1:)
!
!     REAL(kind = 8) :: m_z(1:len_z, 1:n_sweeps)
!
!     CALL readM_z(i_sample, slot, m_z(1:len_z, 1:n_sweeps))
!
!     sumsd_m_z(1:len_z, 1:n_sweeps) = sumsd_m_z(1:len_z, 1:n_sweeps) + &
!          (m_z(1:len_z, 1:n_sweeps) - ave_m_z(1:len_z, 1:n_sweeps)) ** 2
!   END SUBROUTINE addNewM_zSample2Sumsd
!
!   SUBROUTINE addOldM_z(	i_sample, slot, sum_m_z, sumsd_m_z)
!     INTEGER(kind = 4), INTENT(in) :: i_sample, slot
!     REAL(kind = 8), INTENT(inout) :: sum_m_z(1:, 1:)
!     REAL(kind = 8), INTENT(inout) :: sumsd_m_z(1:, 1:)
!
!     REAL(kind = 8) :: ave_m_z(1:len_z, 1:n_sweeps)
!     REAL(kind = 8) :: fluc_m_z(1:len_z, 1:n_sweeps)
!
!     CALL readM_z(i_sample, slot, ave_m_z(1:len_z, 1:n_sweeps), fluc_m_z(1:len_z, 1:n_sweeps))
!
!     sum_m_z(1:len_z, 1:n_sweeps) = sum_m_z(1:len_z, 1:n_sweeps) + ave_m_z(1:len_z, 1:n_sweeps)
!     sumsd_m_z(1:len_z, 1:n_sweeps) = sumsd_m_z(1:len_z, 1:n_sweeps) + &
!          fluc_m_z(1:len_z, 1:n_sweeps)
!   END SUBROUTINE addOldM_z
!
!   SUBROUTINE synthesizeSets(n_olds, n_news, &
!     a_old, f_old, a_new, f_new, a_tot, f_tot)
!     INTEGER(kind = 4), INTENT(in) :: n_olds, n_news
!     REAL(kind = 8), INTENT(in) :: a_old, f_old, a_new, f_new
!     REAL(kind = 8), INTENT(out) :: a_tot, f_tot
!
!     INTEGER(kind = 4) :: n_tot
!     REAL(kind = 8) :: r_olds, r_news
!
!     n_tot = n_olds + n_news
!     r_olds = n_olds / n_tot
!     r_news = n_news / n_tot
!
!     a_tot = a_old * r_olds + a_new * r_news
!
!     f_tot = f_old * r_olds + f_new * r_news
!     f_tot = f_tot + &
!     r_olds * a_old ** 2 + r_news * a_new ** 2 - a_tot ** 2
!   END SUBROUTINE synthesizeSets
!
!   SUBROUTINE synthesizeSets_dim2(n_olds, n_news, &
!     a_old, f_old, a_new, f_new, a_tot, f_tot)
!     INTEGER(kind = 4), INTENT(in) :: n_olds, n_news
!     REAL(kind = 8), INTENT(in) :: a_old(1:, 1:), f_old(1:, 1:)
!     REAL(kind = 8), INTENT(in) :: a_new(1:, 1:), f_new(1:, 1:)
!     REAL(kind = 8), INTENT(out) :: a_tot(1:, 1:), f_tot(1:, 1:)
!
!     INTEGER(kind = 4) :: n_tot
!     REAL(kind = 8) :: r_olds, r_news
!
!     n_tot = n_olds + n_news
!     r_olds = n_olds / n_tot
!     r_news = n_news / n_tot
!
!     a_tot(:, :) = a_old(:, :) * r_olds + a_new(:, :) * r_news
!
!     f_tot(:, :) = f_old(:, :) * r_olds + f_new(:, :) * r_news
!     f_tot(:, :) = f_tot(:, :) + &
!     r_olds * a_old(:, :) ** 2 + r_news * a_new(:, :) ** 2
!     f_tot(:, :) = f_tot(:, :) - a_tot(:, :) ** 2
!   END SUBROUTINE synthesizeSets_dim2
! END MODULE main_procedures

PROGRAM main
  !$  USE omp_lib
  USE global_variables
  USE main_procedures

  IMPLICIT NONE
  INTEGER(kind = 4) :: i_sweep, i_sample
  REAL(kind = 8), ALLOCATABLE :: ave_pump(:), ave_diss(:), ave_energy(:)
  REAL(kind = 8), ALLOCATABLE :: ave_pump_old(:), ave_pump_new(:)
  REAL(kind = 8), ALLOCATABLE :: ave_diss_old(:), ave_diss_new(:)
  REAL(kind = 8), ALLOCATABLE :: ave_energy_old(:), ave_energy_new(:)
  REAL(kind = 8), ALLOCATABLE :: fluc_pump(:), fluc_diss(:), fluc_energy(:)
  REAL(kind = 8), ALLOCATABLE :: fluc_pump_old(:), fluc_pump_new(:)
  REAL(kind = 8), ALLOCATABLE :: fluc_diss_old(:), fluc_diss_new(:)
  REAL(kind = 8), ALLOCATABLE :: fluc_energy_old(:), fluc_energy_new(:)
  REAL(kind = 8), ALLOCATABLE :: ave_m_z(:, :), fluc_m_z(:, :)
  REAL(kind = 8), ALLOCATABLE :: ave_m_z_old(:, :), ave_m_z_new(:, :)
  REAL(kind = 8), ALLOCATABLE :: fluc_m_z_old(:, :), fluc_m_z_new(:, :)

  INTEGER(kind = 4), ALLOCATABLE :: stat_sample_e(:), stat_sample_a(:)
  INTEGER(kind = 4) :: slot
  INTEGER(kind = 4) :: i_dum, z_dum, x, z, n_ths
  INTEGER(kind = 4) :: slot_in_stats1, slot_in_stats2, slot_in_stats3
  INTEGER(kind = 4) :: slot_in_stream1, slot_in_stream2, slot_out_stream
  INTEGER(kind = 4) :: slot_in_m_z1, slot_in_m_z2, slot_out_m_z
  INTEGER(kind = 4) :: slot_out_stats

  CALL inputParameters

  slot_in_stats1 = 11
  slot_in_stats2 = 16
  slot_in_stats3 = 21

  slot_in_stream1 = 26
  slot_in_stream2 = 31
  slot_out_stream = 30

  slot_in_m_z1 = 36
  slot_in_m_z2 = 41
  slot_out_m_z = 40

  slot_out_stats = 50

  ! 総合サンプル数の取得
  CALL getNumSamples(slot_in_stats1, n_samples)
  ALLOCATE(stat_sample_e(1:n_samples), stat_sample_a(1:n_samples))
  ! 旧サンプル数・新サンプル数の取得
  CALL getStatsSamples(slot_in_stats2, n_samples, &
       stat_sample_e(1:n_samples), stat_sample_a(1:n_samples))
	! 平均化の必要性を判断
	IF ( SUM(stat_sample_a(1:n_samples), dim=1) == n_samples ) THEN
		STOP
	END IF
	n_samples_new = SUM(stat_sample_e(1:n_samples), dim = 1)
  n_samples_old = n_samples - n_samples_new
  ! 平衡化計算時間・定常化計算時間の取得
  DO i_sample = 1, n_samples, 1
     IF ( stat_sample_e(i_sample) == 1 ) THEN
        CALL getNumSweeps(slot_in_stats3, i_sample, n_sweeps_therm, n_sweeps_stead)
        EXIT
     END IF
  END DO
  ! 総計算時間の算出
  n_sweeps = n_sweeps_therm + n_sweeps_stead

  ! スレッド数の取得
  !$omp parallel default(none) &
  !$omp shared(n_ths)
  n_ths = 1
  !$  n_ths = omp_get_max_threads()
  !$omp end parallel

  ! streamに関する配列のセット
  ALLOCATE(ave_pump(1:n_sweeps), ave_diss(1:n_sweeps), ave_energy(1:n_sweeps))
  ALLOCATE(ave_pump_old(1:n_sweeps), ave_pump_new(1:n_sweeps))
  ALLOCATE(ave_diss_old(1:n_sweeps), ave_diss_new(1:n_sweeps))
  ALLOCATE(ave_energy_old(1:n_sweeps), ave_energy_new(1:n_sweeps))
  ALLOCATE(fluc_pump(1:n_sweeps), fluc_diss(1:n_sweeps), &
  fluc_energy(1:n_sweeps))
  ALLOCATE(fluc_pump_old(1:n_sweeps), fluc_pump_new(1:n_sweeps))
  ALLOCATE(fluc_diss_old(1:n_sweeps), fluc_diss_new(1:n_sweeps))
  ALLOCATE(fluc_energy_old(1:n_sweeps), fluc_energy_new(1:n_sweeps))

  ! WRITE(0, *) 1

  ! streamの旧平均値と旧ゆらぎの読み込み
  ave_pump_old(1:n_sweeps) = 0.0d0
  ave_diss_old(1:n_sweeps) = 0.0d0
  ave_energy_old(1:n_sweeps) = 0.0d0
  fluc_pump_old(1:n_sweeps) = 0.0d0
  fluc_diss_old(1:n_sweeps) = 0.0d0
  fluc_energy_old(1:n_sweeps) = 0.0d0
  IF ( n_samples_old > 0 ) THEN
    slot = slot_in_stream1
    CALL readStream(0, slot, &
    ave_pump_old(1:n_sweeps), &
    ave_diss_old(1:n_sweeps), &
    ave_energy_old(1:n_sweeps), &
    fluc_pump_old(1:n_sweeps), &
    fluc_diss_old(1:n_sweeps), &
    fluc_energy_old(1:n_sweeps))
  END IF

  ! WRITE(0, *) 2

	! streamの新平均値と新ゆらぎの算出
  ! - 新平均値
  ave_pump_new(1:n_sweeps) = 0.0d0
  ave_diss_new(1:n_sweeps) = 0.0d0
  ave_energy_new(1:n_sweeps) = 0.0d0
  DO i_sample = n_samples_old + 1, n_samples, 1
    slot = slot_in_stream1
    CALL addNewStreamSample2Sum(i_sample, slot, &
    ave_pump_new(1:n_sweeps), &
    ave_diss_new(1:n_sweeps), &
    ave_energy_new(1:n_sweeps))
  END DO
  ave_pump_new(1:n_sweeps) = ave_pump_new(1:n_sweeps) / DBLE(n_samples_new)
  ave_diss_new(1:n_sweeps) = ave_diss_new(1:n_sweeps) / DBLE(n_samples_new)
  ave_energy_new(1:n_sweeps) = ave_energy_new(1:n_sweeps) / DBLE(n_samples_new)
  ! - 新ゆらぎ
  fluc_pump_new(1:n_sweeps) = 0.0d0
  fluc_diss_new(1:n_sweeps) = 0.0d0
  fluc_energy_new(1:n_sweeps) = 0.0d0
  DO i_sample = n_samples_old + 1, n_samples, 1
     slot = slot_in_stream2
    CALL addNewStreamSample2Sumsd( i_sample, slot, &
    ave_pump_new(1:n_sweeps), &
    ave_diss_new(1:n_sweeps), &
    ave_energy_new(1:n_sweeps), &
    fluc_pump_new(1:n_sweeps), &
    fluc_diss_new(1:n_sweeps), &
    fluc_energy_new(1:n_sweeps))
  END DO
  fluc_pump_new(1:n_sweeps) = fluc_pump_new(1:n_sweeps) / &
  DBLE(n_samples_new)
  fluc_diss_new(1:n_sweeps) = fluc_diss_new(1:n_sweeps) / &
  DBLE(n_samples_new)
  fluc_energy_new(1:n_sweeps) = fluc_energy_new(1:n_sweeps) / &
  DBLE(n_samples_new)

  ! WRITE(0, *) 3

  ! streamの総平均値と総ゆらぎの算出と出力
  ! - 算出
  DO i_sweep = 1, n_sweeps, 1
    CALL synthesizeSets(n_samples_old, n_samples_new, &
    ave_pump_old(i_sweep), fluc_pump_old(i_sweep), &
    ave_pump_new(i_sweep), fluc_pump_new(i_sweep), &
    ave_pump(i_sweep), fluc_pump(i_sweep))
    CALL synthesizeSets(n_samples_old, n_samples_new, &
    ave_diss_old(i_sweep), fluc_diss_old(i_sweep), &
    ave_diss_new(i_sweep), fluc_diss_new(i_sweep), &
    ave_diss(i_sweep), fluc_diss(i_sweep))
    CALL synthesizeSets(n_samples_old, n_samples_new, &
    ave_energy_old(i_sweep), fluc_energy_old(i_sweep), &
    ave_energy_new(i_sweep), fluc_energy_new(i_sweep), &
    ave_energy(i_sweep), fluc_energy(i_sweep))
  END DO
  ! - 出力
  OPEN(slot_out_stream, file="stream.dat", status="replace")
  WRITE(slot_out_stream, '(a)') "# i_sweep, ave_pump, ave_diss, ave_energy, fluc_pump, fluc_diss, fluc_energy"
  DO i_sweep = 1, n_sweeps_therm, 1
     WRITE(slot_out_stream, '(	i5, a, f0.4, a, f0.4, a, f0.4, a, &
          f0.4, a, f0.4, a, f0.4)') &
          i_sweep, ", ", ave_pump(i_sweep), ", ", ave_diss(i_sweep), ", ", ave_energy(i_sweep), ", ", &
          fluc_pump(i_sweep), ", ", fluc_diss(i_sweep), ", ", fluc_energy(i_sweep)
  END DO
  WRITE(slot_out_stream, '(a)') "# -- Thermalized --"
  DO i_sweep = n_sweeps_therm + 1, n_sweeps, 1
     WRITE(slot_out_stream, '(	i5, a, f0.4, a, f0.4, a, f0.4, a, &
          f0.4, a, f0.4, a, f0.4)') &
          i_sweep, ", ", ave_pump(i_sweep), ", ", ave_diss(i_sweep), ", ", ave_energy(i_sweep), ", ", &
          fluc_pump(i_sweep), ", ", fluc_diss(i_sweep), ", ", fluc_energy(i_sweep)
  END DO
  WRITE(slot_out_stream, '(a)') "# -- Steadized --"
  CLOSE(slot_out_stream)

  ! WRITE(0, *) 4

  ! streamに関する配列の解除
  DEALLOCATE(ave_pump, ave_diss, ave_energy)
  DEALLOCATE(ave_pump_old, ave_pump_new, ave_diss_old, ave_diss_new)
  DEALLOCATE(ave_energy_old, ave_energy_new)
  DEALLOCATE(fluc_pump, fluc_diss, fluc_energy)
  DEALLOCATE(fluc_pump_old, fluc_pump_new, fluc_diss_old, fluc_diss_new)
  DEALLOCATE(fluc_energy_old, fluc_energy_new)

  ! WRITE(0, *) 5

  ! m_zに関する配列のセット
  ALLOCATE(ave_m_z(1:len_z, 1:n_sweeps), fluc_m_z(1:len_z, 1:n_sweeps))
  ALLOCATE(ave_m_z_old(1:len_z, 1:n_sweeps), ave_m_z_new(1:len_z, 1:n_sweeps))
  ALLOCATE(fluc_m_z_old(1:len_z, 1:n_sweeps), fluc_m_z_new(1:len_z, 1:n_sweeps))

  ! WRITE(0, *) 6

  ! m_zの旧平均値と旧ゆらぎの読み込み
  ave_m_z_old(1:len_z, 1:n_sweeps) = 0.0d0
  fluc_m_z_old(1:len_z, 1:n_sweeps) = 0.0d0
  IF ( n_samples_old > 0 ) THEN
    slot = slot_in_stream1
    CALL readM_z(0, slot, &
    ave_m_z_old(1:len_z, 1:n_sweeps), fluc_m_z_old(1:len_z, 1:n_sweeps))
  END IF

  ! WRITE(0, *) 7

  ! m_zの新平均値と新ゆらぎの算出
  ! - 新平均値
  ave_m_z_new(1:len_z, 1:n_sweeps) = 0.0d0
  DO i_sample = n_samples_old + 1, n_samples, 1
    slot = slot_in_m_z1
    CALL addNewM_zSample2Sum(i_sample, slot, ave_m_z_new(1:len_z, 1:n_sweeps))
  END DO
  ave_m_z_new(1:len_z, 1:n_sweeps) = ave_m_z_new(1:len_z, 1:n_sweeps) / &
  DBLE(n_samples_new)
  ! - 新ゆらぎ
  fluc_m_z_new(1:len_z, 1:n_sweeps) = 0.0d0
  DO i_sample = n_samples_old + 1, n_samples, 1
    slot = slot_in_m_z2
    CALL addNewM_zSample2Sumsd(i_sample, slot, &
    ave_m_z_new(1:len_z, 1:n_sweeps), fluc_m_z_new(1:len_z, 1:n_sweeps))
  END DO
  fluc_m_z_new(1:len_z, 1:n_sweeps) = fluc_m_z_new(1:len_z, 1:n_sweeps) / &
  DBLE(n_samples_new)

  ! WRITE(0, *) 8

  ! m_zの総平均値と総ゆらぎの算出と出力
  ! - 算出
  DO i_sweep = 1, n_sweeps, 1
    DO z = 1, len_z, 1
      CALL synthesizeSets(n_samples_old, n_samples_new, &
      ave_m_z_old(z, i_sweep), fluc_m_z_old(z, i_sweep), &
      ave_m_z_new(z, i_sweep), fluc_m_z_new(z, i_sweep), &
      ave_m_z(z, i_sweep), fluc_m_z(z, i_sweep))
    END DO
  END DO
  ! - 出力
  OPEN(slot_out_m_z, file="m_z.dat", status="replace")
  WRITE(slot_out_m_z, '(a)') "# i_sweep, z, ave_m_z, fluc_m_z"
  DO i_sweep = 1, n_sweeps, 1
    DO z = 1, len_z, 1
       WRITE(slot_out_m_z, '( i5, a, i5, a, f0.4, a, f0.4)') &
            i_sweep, ", ", z, ", ", &
            ave_m_z(z, i_sweep), ", ", fluc_m_z(z, i_sweep)
    END DO
    WRITE(slot_out_m_z, '()')
  END DO
  CLOSE(slot_out_m_z)

  ! WRITE(0, *) 9

  ! m_zに関する配列の解除
  DEALLOCATE(ave_m_z, fluc_m_z)
  DEALLOCATE(ave_m_z_old, ave_m_z_new, fluc_m_z_old, fluc_m_z_new)

  ! WRITE(0, *) 10

  ! list_samples.datの更新
  OPEN(slot_out_stats, file="list_samples.dat", status="old")
  DO i_sample = 1, n_samples, 1
     SELECT CASE (stat_sample_a(i_sample))
     CASE (0)
        WRITE(slot_out_stats, '(i5, a, i2, a, i2)') i_sample, ", ", 1, ", ", 1
     CASE (1)
        READ(slot_out_stats, '()')
     END SELECT
  END DO
  CLOSE(slot_out_stats)

  ! WRITE(0, *) 11
END PROGRAM main
