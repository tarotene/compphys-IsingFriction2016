MODULE main_procedures
  USE global_variables
  ! USE sub_procedures
  USE IFPORT
  IMPLICIT NONE
CONTAINS
	SUBROUTINE readStream(i_sample, slot, pump, diss, energy, &
       fluc_pump, fluc_diss, fluc_energy)
    INTEGER(kind = 4), INTENT(in) :: i_sample, slot
    REAL(kind = 8), INTENT(out) :: pump(1:), diss(1:), energy(1:)
    REAL(kind = 8), INTENT(out), OPTIONAL :: fluc_pump(1:)
    REAL(kind = 8), INTENT(out), OPTIONAL :: fluc_diss(1:)
    REAL(kind = 8), INTENT(out), OPTIONAL :: fluc_energy(1:)

    CHARACTER(len = 4) :: si_sample
    CHARACTER(len = 30) :: filename
    INTEGER(kind = 4) :: i_sweep, i_dum

    IF ( i_sample == 0 ) THEN
       filename=TRIM("stream.dat")
    ELSE
       WRITE(si_sample, '(i0.4)') i_sample
       filename=TRIM("stream_s"//si_sample//".dat")
    END IF

    OPEN(slot, file=filename, status="old")
    READ(slot, '()')
    DO i_sweep = 1, n_sweeps_therm, 1
       READ(slot, *)	i_dum, pump(i_sweep), diss(i_sweep), energy(i_sweep), &
            fluc_pump(i_sweep), fluc_diss(i_sweep), fluc_energy(i_sweep)
    END DO
    READ(slot, '()')
    DO i_sweep = n_sweeps_therm + 1, n_sweeps_therm + n_sweeps_stead, 1
       READ(slot, *)	i_dum, pump(i_sweep), diss(i_sweep), energy(i_sweep), &
            fluc_pump(i_sweep), fluc_diss(i_sweep), fluc_energy(i_sweep)
    END DO
    CLOSE(slot)
  END SUBROUTINE readStream

  ! SUBROUTINE readSpin(i_sample, slot, spin)
  !   INTEGER(kind = 4), INTENT(in) :: i_sample, slot
  !   INTEGER(kind = 4), INTENT(out) :: spin(1:, 1:, 1:)
  !
  !   CHARACTER(len = 4) :: si_sample
  !   CHARACTER(len = 30) :: filename
  !   INTEGER(kind = 4) :: i_sweep, i_dum, x, x_dum, z, z_dum
  !
  !   WRITE(si_sample, '(i0.4)') i_sample
  !   filename=TRIM("spin_s"//si_sample//".dat")
  !
  !   OPEN(slot, file=filename, status="old")
  !   DO i_sweep = 1, n_sweeps, 1
  !      DO z = 1, len_z + 1, 1
  !         READ(slot, '()')
  !         DO x = 1, len_x + 1, 1
  !            READ(slot, *) i_dum, x_dum, z_dum, spin(i_sweep, x, z)
  !         END DO
  !      END DO
  !   END DO
  !   CLOSE(slot)
  ! END SUBROUTINE readSpin

  SUBROUTINE readM_z(i_sample, slot, m_z, fluc_m_z)
    INTEGER(kind = 4), INTENT(in) :: i_sample, slot
    REAL(kind = 8), INTENT(out) :: m_z(1:, 1:)
    REAL(kind = 8), INTENT(out), OPTIONAL :: fluc_m_z(1:, 1:)

    CHARACTER(len = 4) :: si_sample
    CHARACTER(len = 30) :: filename
    INTEGER(kind = 4) :: i_sweep, i_dum, z, z_dum

    IF ( i_sample == 0 ) THEN
       filename=TRIM("m_z.dat")
       OPEN(slot, file=filename, status="old")
       DO i_sweep = 1, n_sweeps, 1
          READ(slot, '()')
          DO z = 1, len_z, 1
             READ(slot, *) i_dum, z_dum, m_z(z, i_sweep), fluc_m_z(z, i_sweep)
          END DO
       END DO
       CLOSE(slot)
    ELSE
       WRITE(si_sample, '(i0.4)') i_sample
       filename=TRIM("m_z_s"//si_sample//".dat")
       OPEN(slot, file=filename, status="old")
       DO i_sweep = 1, n_sweeps, 1
          READ(slot, '()')
          DO z = 1, len_z, 1
             READ(slot, *) i_dum, z_dum, m_z(z, i_sweep)
          END DO
       END DO
       CLOSE(slot)
    END IF
  END SUBROUTINE readM_z

  SUBROUTINE getNumSamples(slot, n_samples)
    INTEGER(kind = 4), INTENT(in) :: slot
    INTEGER(kind = 4), INTENT(out) :: n_samples

    INTEGER(kind = 4) :: i_line, ios, n_dum

    n_samples = 0
    OPEN(slot, file="list_samples.dat", status="old")
    DO i_line = 1, 10000, 1
       READ (slot, *, iostat = ios) n_dum
       IF (ios < 0) EXIT
       n_samples = n_samples + 1
    END DO
    CLOSE(slot)
  END SUBROUTINE getNumSamples

  SUBROUTINE getStatsSamples(slot, n_samples, stat_sample_e, stat_sample_a)
    INTEGER(kind = 4), INTENT(in) :: slot, n_samples
    INTEGER(kind = 4), INTENT(inout) :: stat_sample_e(1:)
    INTEGER(kind = 4), INTENT(inout) :: stat_sample_a(1:)

    INTEGER(kind = 4) :: i_sample, n_dum, i_dum, e_dum, a_dum

    OPEN(slot, file="list_samples.dat", status="old")
    DO i_sample = 1, n_samples, 1
       READ (slot, *) i_dum, e_dum, a_dum
       stat_sample_e(i_dum) = e_dum
       stat_sample_a(i_dum) = a_dum
    END DO
    CLOSE(slot)
  END SUBROUTINE getStatsSamples

  SUBROUTINE getNumSweeps(slot, i_sample, n_sweeps_therm, n_sweeps_stead)
    INTEGER(kind = 4), INTENT(in) :: slot, i_sample
    INTEGER(kind = 4), INTENT(out) :: n_sweeps_therm, n_sweeps_stead

    CHARACTER(len = 4) :: si_sample
    CHARACTER(len = 30) :: filename
    INTEGER(kind = 4) :: i_line, ios, n0

    n_sweeps_therm = 0
    n_sweeps_stead = 0

    WRITE(si_sample, '(i0.4)') i_sample
    filename=TRIM("stream_s"//si_sample//".dat")
    OPEN(slot, file=filename, status="old")
    READ (slot, '()')

    DO i_line = 1, 10000, 1
       READ (slot, *, iostat = ios) n0
       IF (ios == 0) THEN
          n_sweeps_therm = n_sweeps_therm + 1
       ELSE
          EXIT
       END IF
    END DO

    DO i_line = 1, 10000, 1
       READ (slot, *, iostat = ios) n0
       IF (ios == 0) THEN
          n_sweeps_stead = n_sweeps_stead + 1
       ELSE
          EXIT
       END IF
    END DO

    CLOSE(slot)
  END SUBROUTINE getNumSweeps

  SUBROUTINE inputParameters
    ! WRITE(0, '(a)') "n_sweeps_therm, n_sweeps_stead = ?"
    READ(*, *) len_x, len_z
  END SUBROUTINE inputParameters

  SUBROUTINE addNewStreamSample2Sum(	i_sample, slot, &
       sum_pump, sum_diss, sum_energy)
    INTEGER(kind = 4), INTENT(in) :: i_sample, slot
    REAL(kind = 8), INTENT(inout) :: sum_pump(1:)
    REAL(kind = 8), INTENT(inout) :: sum_diss(1:)
    REAL(kind = 8), INTENT(inout) :: sum_energy(1:)

    REAL(kind = 8) :: pump(1:n_sweeps)
    REAL(kind = 8) :: diss(1:n_sweeps)
    REAL(kind = 8) :: energy(1:n_sweeps)
    REAL(kind = 8) :: fluc_pump(1:n_sweeps)
    REAL(kind = 8) :: fluc_diss(1:n_sweeps)
    REAL(kind = 8) :: fluc_energy(1:n_sweeps)

    CALL readStream(i_sample, slot, &
         pump(1:n_sweeps), diss(1:n_sweeps), energy(1:n_sweeps), &
         fluc_pump(1:n_sweeps), fluc_diss(1:n_sweeps), fluc_energy(1:n_sweeps))

    sum_pump(1:n_sweeps) = sum_pump(1:n_sweeps) + pump(1:n_sweeps)
    sum_diss(1:n_sweeps) = sum_diss(1:n_sweeps) + diss(1:n_sweeps)
    sum_energy(1:n_sweeps) = sum_energy(1:n_sweeps) + energy(1:n_sweeps)
  END SUBROUTINE addNewStreamSample2Sum

  SUBROUTINE addNewStreamSample2Sumsd(	i_sample, slot, &
       ave_pump, ave_diss, ave_energy, &
       sumsd_pump, sumsd_diss, sumsd_energy)
    INTEGER(kind = 4), INTENT(in) :: i_sample, slot
    REAL(kind = 8), INTENT(in) :: ave_pump(1:)
    REAL(kind = 8), INTENT(in) :: ave_diss(1:)
    REAL(kind = 8), INTENT(in) :: ave_energy(1:)
    REAL(kind = 8), INTENT(inout) :: sumsd_pump(1:)
    REAL(kind = 8), INTENT(inout) :: sumsd_diss(1:)
    REAL(kind = 8), INTENT(inout) :: sumsd_energy(1:)

    REAL(kind = 8) :: pump(1:n_sweeps)
    REAL(kind = 8) :: diss(1:n_sweeps)
    REAL(kind = 8) :: energy(1:n_sweeps)
    REAL(kind = 8) :: fluc_pump(1:n_sweeps)
    REAL(kind = 8) :: fluc_diss(1:n_sweeps)
    REAL(kind = 8) :: fluc_energy(1:n_sweeps)

    CALL readStream(i_sample, slot, &
         pump(1:n_sweeps), diss(1:n_sweeps), energy(1:n_sweeps), &
         fluc_pump(1:n_sweeps), fluc_diss(1:n_sweeps), fluc_energy(1:n_sweeps))

    sumsd_pump(1:n_sweeps) = sumsd_pump(1:n_sweeps) + &
         (pump(1:n_sweeps) - ave_pump(1:n_sweeps)) ** 2 + &
         fluc_pump(1:n_sweeps)
    sumsd_diss(1:n_sweeps) = sumsd_diss(1:n_sweeps) + &
         (diss(1:n_sweeps) - ave_diss(1:n_sweeps)) ** 2 + &
         fluc_diss(1:n_sweeps)
    sumsd_energy(1:n_sweeps) = sumsd_energy(1:n_sweeps) + &
         (energy(1:n_sweeps) - ave_energy(1:n_sweeps)) ** 2 + &
         fluc_energy(1:n_sweeps)
  END SUBROUTINE addNewStreamSample2Sumsd

  ! SUBROUTINE addOldStream(	slot, &
  !      sum_pump, sum_diss, sum_energy, sumsd_pump, sumsd_diss, sumsd_energy)
  !   INTEGER(kind = 4), INTENT(in) :: slot
	!
  !   REAL(kind = 8), INTENT(inout) :: sum_pump(1:)
  !   REAL(kind = 8), INTENT(inout) :: sum_diss(1:)
  !   REAL(kind = 8), INTENT(inout) :: sum_energy(1:)
  !   REAL(kind = 8), INTENT(inout) :: sumsd_pump(1:)
  !   REAL(kind = 8), INTENT(inout) :: sumsd_diss(1:)
  !   REAL(kind = 8), INTENT(inout) :: sumsd_energy(1:)
	!
  !   REAL(kind = 8) :: ave_pump(1:n_sweeps)
  !   REAL(kind = 8) :: ave_diss(1:n_sweeps)
  !   REAL(kind = 8) :: ave_energy(1:n_sweeps)
  !   REAL(kind = 8) :: fluc_pump(1:n_sweeps)
  !   REAL(kind = 8) :: fluc_diss(1:n_sweeps)
  !   REAL(kind = 8) :: fluc_energy(1:n_sweeps)
	!
  !   CALL readStream(0, slot, &
  !        ave_pump(1:n_sweeps), ave_diss(1:n_sweeps), ave_energy(1:n_sweeps), &
  !        fluc_pump(1:n_sweeps), fluc_diss(1:n_sweeps), fluc_energy(1:n_sweeps))
	!
  !   sum_pump(1:n_sweeps) = sum_pump(1:n_sweeps) + ave_pump(1:n_sweeps)
  !   sum_diss(1:n_sweeps) = sum_diss(1:n_sweeps) + ave_diss(1:n_sweeps)
  !   sum_energy(1:n_sweeps) = sum_energy(1:n_sweeps) + ave_energy(1:n_sweeps)
  !   sumsd_pump(1:n_sweeps) = sumsd_pump(1:n_sweeps) + &
  !        fluc_pump(1:n_sweeps)
  !   sumsd_diss(1:n_sweeps) = sumsd_diss(1:n_sweeps) + &
  !        fluc_diss(1:n_sweeps)
  !   sumsd_energy(1:n_sweeps) = sumsd_energy(1:n_sweeps) + &
  !        fluc_energy(1:n_sweeps)
  ! END SUBROUTINE addOldStream

  ! SUBROUTINE addSpinSample(i_sample, slot, ave_spin)
  !   INTEGER(kind = 4), INTENT(in) :: i_sample, slot
  !   REAL(kind = 8), INTENT(inout) :: ave_spin(1:, 1:, 1:)
  !
  !   INTEGER(kind = 4) :: spin(1:len_x + 1, 1:len_z + 1, 1:n_sweeps)
  !
  !   CALL readSpin(i_sample, slot, spin)
  !   ave_spin(1:len_x + 1, 1:len_z + 1, 1:n_sweeps) = &
  !   ave_spin(1:len_x + 1, 1:len_z + 1, 1:n_sweeps) + &
  ! 	 DBLE(spin(1:len_x + 1, 1:len_z + 1, 1:n_sweeps))
  ! END SUBROUTINE addSpinSample

  SUBROUTINE addNewM_zSample2Sum(	i_sample, slot, sum_m_z)
    INTEGER(kind = 4), INTENT(in) :: i_sample, slot
    REAL(kind = 8), INTENT(inout) :: sum_m_z(1:, 1:)

    REAL(kind = 8) :: m_z(1:len_z, 1:n_sweeps)

    CALL readM_z(i_sample, slot, m_z(1:len_z, 1:n_sweeps))
    sum_m_z(1:len_z, 1:n_sweeps) = sum_m_z(1:len_z, 1:n_sweeps) + m_z(1:len_z, 1:n_sweeps)
  END SUBROUTINE addNewM_zSample2Sum

  SUBROUTINE addNewM_zSample2Sumsd(	i_sample, slot, ave_m_z, sumsd_m_z)
    INTEGER(kind = 4), INTENT(in) :: i_sample, slot
    REAL(kind = 8), INTENT(in) :: ave_m_z(1:, 1:)
    REAL(kind = 8), INTENT(inout) :: sumsd_m_z(1:, 1:)

    REAL(kind = 8) :: m_z(1:len_z, 1:n_sweeps)

    CALL readM_z(i_sample, slot, m_z(1:len_z, 1:n_sweeps))

    sumsd_m_z(1:len_z, 1:n_sweeps) = sumsd_m_z(1:len_z, 1:n_sweeps) + &
         (m_z(1:len_z, 1:n_sweeps) - ave_m_z(1:len_z, 1:n_sweeps)) ** 2
  END SUBROUTINE addNewM_zSample2Sumsd

  SUBROUTINE addOldM_z(	i_sample, slot, sum_m_z, sumsd_m_z)
    INTEGER(kind = 4), INTENT(in) :: i_sample, slot
    REAL(kind = 8), INTENT(inout) :: sum_m_z(1:, 1:)
    REAL(kind = 8), INTENT(inout) :: sumsd_m_z(1:, 1:)

    REAL(kind = 8) :: ave_m_z(1:len_z, 1:n_sweeps)
    REAL(kind = 8) :: fluc_m_z(1:len_z, 1:n_sweeps)

    CALL readM_z(i_sample, slot, ave_m_z(1:len_z, 1:n_sweeps), fluc_m_z(1:len_z, 1:n_sweeps))

    sum_m_z(1:len_z, 1:n_sweeps) = sum_m_z(1:len_z, 1:n_sweeps) + ave_m_z(1:len_z, 1:n_sweeps)
    sumsd_m_z(1:len_z, 1:n_sweeps) = sumsd_m_z(1:len_z, 1:n_sweeps) + &
         fluc_m_z(1:len_z, 1:n_sweeps)
  END SUBROUTINE addOldM_z

  SUBROUTINE synthesizeSets(n_olds, n_news, &
    a_old, f_old, a_new, f_new, a_tot, f_tot)
    INTEGER(kind = 4), INTENT(in) :: n_olds, n_news
    REAL(kind = 8), INTENT(in) :: a_old, f_old, a_new, f_new
    REAL(kind = 8), INTENT(out) :: a_tot, f_tot

    INTEGER(kind = 4) :: n_tot
    REAL(kind = 8) :: r_olds, r_news

    n_tot = n_olds + n_news
    r_olds = n_olds / n_tot
    r_news = n_news / n_tot

    a_tot = a_old * r_olds + a_new * r_news

    f_tot = f_old * r_olds + f_new * r_news
    f_tot = f_tot + &
    r_olds * a_old ** 2 + r_news * a_new ** 2 - a_tot ** 2
  END SUBROUTINE synthesizeSets

  SUBROUTINE synthesizeSets_dim2(n_olds, n_news, &
    a_old, f_old, a_new, f_new, a_tot, f_tot)
    INTEGER(kind = 4), INTENT(in) :: n_olds, n_news
    REAL(kind = 8), INTENT(in) :: a_old(1:, 1:), f_old(1:, 1:)
    REAL(kind = 8), INTENT(in) :: a_new(1:, 1:), f_new(1:, 1:)
    REAL(kind = 8), INTENT(out) :: a_tot(1:, 1:), f_tot(1:, 1:)

    INTEGER(kind = 4) :: n_tot
    REAL(kind = 8) :: r_olds, r_news

    n_tot = n_olds + n_news
    r_olds = n_olds / n_tot
    r_news = n_news / n_tot

    a_tot(:, :) = a_old(:, :) * r_olds + a_new(:, :) * r_news

    f_tot(:, :) = f_old(:, :) * r_olds + f_new(:, :) * r_news
    f_tot(:, :) = f_tot(:, :) + &
    r_olds * a_old(:, :) ** 2 + r_news * a_new(:, :) ** 2
    f_tot(:, :) = f_tot(:, :) - a_tot(:, :) ** 2
  END SUBROUTINE synthesizeSets_dim2
END MODULE main_procedures
