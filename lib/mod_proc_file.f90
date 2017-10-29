MODULE mod_proc_file
  USE mod_global
  USE IFPORT, ONLY: access
  IMPLICIT NONE
CONTAINS
  ! SUBROUTINE getNumSamples(slot, filename, n_samples)
  !   INTEGER(kind = 4), INTENT(in) :: slot
  !   CHARACTER(LEN = *), INTENT(in) :: filename
  !   INTEGER(kind = 4), INTENT(out) :: n_samples
  !
  !   INTEGER(kind = 4) :: i_line, ios, n_dum
  !
  !   n_samples = 0
  !   OPEN(slot, file=filename, status="old")
  !   DO i_line = 1, 10000, 1
  !      READ (slot, *, iostat = ios) n_dum
  !      IF (ios < 0) EXIT
  !      n_samples = n_samples + 1
  !   END DO
  !   CLOSE(slot)
  ! END SUBROUTINE getNumSamples

  SUBROUTINE inputParameters_2d(len_x, len_z, J, beta, vel, &
       n_sweeps_therm, n_sweeps_stead, id_IC, id_BC, n_samples, &
       onoff_stream, onoff_m_z)
    INTEGER(kind = 4), INTENT(out) :: len_x, len_z
    REAL(kind = 8), INTENT(out) :: J, beta
    INTEGER(kind = 4), INTENT(out) :: vel
    INTEGER(kind = 4), INTENT(out) :: n_sweeps_therm, n_sweeps_stead
    INTEGER(kind = 4), INTENT(out) :: id_IC, id_BC
    INTEGER(kind = 4), INTENT(out) :: n_samples
    INTEGER(kind = 4), INTENT(out) :: onoff_stream, onoff_m_z

    READ(*, *) len_x, len_z, J, beta, vel, n_sweeps_therm, n_sweeps_stead, id_IC, id_BC, n_samples, onoff_stream, onoff_m_z
  END SUBROUTINE inputParameters_2d

  SUBROUTINE inputParameters_3d(len_x, len_y, len_z, J, beta, vel, &
       n_sweeps_therm, n_sweeps_stead, id_IC, id_BC, n_samples, &
       onoff_stream, onoff_m_z)
    INTEGER(kind = 4), INTENT(out) :: len_x, len_y, len_z
    REAL(kind = 8), INTENT(out) :: J, beta
    INTEGER(kind = 4), INTENT(out) :: vel
    INTEGER(kind = 4), INTENT(out) :: n_sweeps_therm, n_sweeps_stead
    INTEGER(kind = 4), INTENT(out) :: id_IC, id_BC
    INTEGER(kind = 4), INTENT(out) :: n_samples
    INTEGER(kind = 4), INTENT(out) :: onoff_stream, onoff_m_z

    READ(*, *) len_x, len_y, len_z, J, beta, vel, &
    n_sweeps_therm, n_sweeps_stead, id_IC, id_BC, n_samples, &
    onoff_stream, onoff_m_z
  END SUBROUTINE inputParameters_3d

  SUBROUTINE getListParameters_2d(slot, filename, &
       len_x, len_z, J, beta, vel, &
       n_sweeps_therm0, n_sweeps_stead0, id_IC, id_BC, n_samples0, &
       onoff_stream, onoff_m_z)

    INTEGER(kind = 4), INTENT(in) :: slot
    CHARACTER(LEN = *), INTENT(in) :: filename
    INTEGER(kind = 4), INTENT(out) :: len_x, len_z
    REAL(kind = 8), INTENT(out) :: J, beta
    INTEGER(kind = 4), INTENT(out) :: vel
    INTEGER(kind = 4), INTENT(out) :: n_sweeps_therm0, n_sweeps_stead0
    INTEGER(kind = 4), INTENT(out) :: id_IC, id_BC
    INTEGER(kind = 4), INTENT(out) :: n_samples0
    INTEGER(kind = 4), INTENT(out) :: onoff_stream, onoff_m_z

    OPEN(slot, file=filename, status="old")
    READ(slot, '()')
    READ(slot, *) len_x, len_z, J, beta, vel, &
         n_sweeps_therm0, n_sweeps_stead0, id_IC, id_BC, n_samples0, &
         onoff_stream, onoff_m_z
    CLOSE(slot)
  END SUBROUTINE getListParameters_2d

  SUBROUTINE getListParameters_3d(slot, filename, &
       len_x, len_y, len_z, J, beta, vel, &
       n_sweeps_therm0, n_sweeps_stead0, id_IC, id_BC, n_samples0, &
       onoff_stream, onoff_m_z)

    INTEGER(kind = 4), INTENT(in) :: slot
    CHARACTER(LEN = *), INTENT(in) :: filename
    INTEGER(kind = 4), INTENT(out) :: len_x, len_y, len_z
    REAL(kind = 8), INTENT(out) :: J, beta
    INTEGER(kind = 4), INTENT(out) :: vel
    INTEGER(kind = 4), INTENT(out) :: n_sweeps_therm0, n_sweeps_stead0
    INTEGER(kind = 4), INTENT(out) :: id_IC, id_BC
    INTEGER(kind = 4), INTENT(out) :: n_samples0
    INTEGER(kind = 4), INTENT(out) :: onoff_stream, onoff_m_z

    OPEN(slot, file=filename, status="old")
    READ(slot, '()')
    READ(slot, *) len_x, len_y, len_z, J, beta, vel, &
         n_sweeps_therm0, n_sweeps_stead0, id_IC, id_BC, n_samples0, &
         onoff_stream, onoff_m_z
    CLOSE(slot)
  END SUBROUTINE getListParameters_3d

  SUBROUTINE refreshListParameters_2d(slot, filename, &
       len_x, len_z, J, beta, vel, &
       n_sweeps_therm, n_sweeps_stead, id_IC, id_BC, n_samples, &
       onoff_stream, onoff_m_z)
    INTEGER(kind = 4), INTENT(in) :: slot
    CHARACTER(LEN = *), INTENT(in) :: filename
    INTEGER(kind = 4), INTENT(in) :: len_x, len_z
    REAL(kind = 8), INTENT(in) :: J, beta
    INTEGER(kind = 4), INTENT(in) :: vel
    INTEGER(kind = 4), INTENT(in) :: n_sweeps_therm, n_sweeps_stead
    INTEGER(kind = 4), INTENT(in) :: id_IC, id_BC
    INTEGER(kind = 4), INTENT(in) :: n_samples
    INTEGER(kind = 4), INTENT(in) :: onoff_stream, onoff_m_z

    OPEN(slot, file=filename, status="replace")
    WRITE(slot, '(a)') "# len_x, len_z, J, beta, vel, &
         n_sweeps_therm, n_sweeps_stead, id_IC, id_BC, n_samples, &
				 onoff_stream, onoff_m_z"
    WRITE(slot, '(i4, a, i4, a, f0.4, a, f0.4, a, i4, a, &
		i5, a, i5, a, i2, a, i2, a, i5, a, i2, a, i2)') &
		len_x, ", ", len_z, ", ", J, ", ", beta, ", ", vel, ", ", &
		n_sweeps_therm, ", ", n_sweeps_stead, ", ", &
		id_IC, ", ", id_BC, ", ", n_samples, ", ", &
		onoff_stream, ", ", onoff_m_z
    CLOSE(slot)
  END SUBROUTINE refreshListParameters_2d

  SUBROUTINE refreshListParameters_3d(slot, filename, &
       len_x, len_y, len_z, J, beta, vel, &
       n_sweeps_therm, n_sweeps_stead, id_IC, id_BC, n_samples, &
       onoff_stream, onoff_m_z)
    INTEGER(kind = 4), INTENT(in) :: slot
    CHARACTER(LEN = *), INTENT(in) :: filename
    INTEGER(kind = 4), INTENT(in) :: len_x, len_y, len_z
    REAL(kind = 8), INTENT(in) :: J, beta
    INTEGER(kind = 4), INTENT(in) :: vel
    INTEGER(kind = 4), INTENT(in) :: n_sweeps_therm, n_sweeps_stead
    INTEGER(kind = 4), INTENT(in) :: id_IC, id_BC
    INTEGER(kind = 4), INTENT(in) :: n_samples
    INTEGER(kind = 4), INTENT(in) :: onoff_stream, onoff_m_z

    OPEN(slot, file=filename, status="replace")
    WRITE(slot, '(a)') "# i_sample, len_x, len_y len_z, J, beta, vel, &
         n_sweeps_therm, n_sweeps_stead, id_IC, id_BC, n_samples, &
         onoff_stream, onoff_m_z"
    WRITE(slot, '(i4, a, i4, a, f0.4, a, f0.4, a, f0.4, a, i4, a, &
		i5, a, i5, a, i2, a, i2, a, i5)') &
		len_x, ", ", len_y, ", ", len_z, ", ", J, ", ", beta, ", ", vel, ", ", &
		n_sweeps_therm, ", ", n_sweeps_stead, ", ", &
		id_IC, ", ", id_BC, ", ", n_samples, ", ", &
		onoff_stream, ", ", onoff_m_z
    CLOSE(slot)
  END SUBROUTINE refreshListParameters_3d

  SUBROUTINE getStatSamples(slot, filename, &
       n_samples, stat_sample_e, stat_sample_a)
    INTEGER(kind = 4), INTENT(in) :: slot
    CHARACTER(LEN = *), INTENT(in) :: filename
    INTEGER(kind = 4), INTENT(in) :: n_samples
    INTEGER(kind = 4), INTENT(out) :: stat_sample_e(1:)
    INTEGER(kind = 4), INTENT(out) :: stat_sample_a(1:)

    INTEGER(kind = 4) :: i_sample, i_dum

    OPEN(slot, file=filename, status="old")
    READ(slot, '()')
    DO i_sample = 1, n_samples, 1
       READ (slot, *) &
            i_dum, stat_sample_e(i_sample), stat_sample_a(i_sample)
    END DO
    CLOSE(slot)
  END SUBROUTINE getStatSamples

  SUBROUTINE refreshStatSamples(slot, filename, &
       n_samples, stat_sample_e, stat_sample_a)
    INTEGER(kind = 4), INTENT(in) :: slot
    CHARACTER(LEN = *), INTENT(in) :: filename
    INTEGER(kind = 4), INTENT(in) :: n_samples
    INTEGER(kind = 4), INTENT(in) :: stat_sample_e(1:)
    INTEGER(kind = 4), INTENT(in) :: stat_sample_a(1:)

    INTEGER(kind = 4) :: i_sample

    OPEN(slot, file=filename, status="replace")
    WRITE (slot, '(a)') "# i_sample, stat_sample_e, stat_sample_a"
    DO i_sample = 1, n_samples, 1
       WRITE (slot, '(i5, a, i2, a, i2)') &
			 i_sample, ", ", stat_sample_e(i_sample), ", ", stat_sample_a(i_sample)
    END DO
    CLOSE(slot)
  END SUBROUTINE refreshStatSamples

  ! SUBROUTINE getNumSweeps(slot, filename, n_sweeps_therm, n_sweeps_stead)
  !   INTEGER(kind = 4), INTENT(in) :: slot
  !   CHARACTER(LEN = *), INTENT(in) :: filename
  !   INTEGER(kind = 4), INTENT(out) :: n_sweeps_therm, n_sweeps_stead
  !
  !   INTEGER(kind = 4) :: i_line, ios, n0
  !
  !   OPEN(slot, file=filename, status="old")
  !   READ (slot, '()')
  !
  !   n_sweeps_therm = 0
  !   DO i_line = 1, 10000, 1
  !      READ (slot, *, iostat = ios) n0
  !      IF (ios == 0) THEN
  !         n_sweeps_therm = n_sweeps_therm + 1
  !      ELSE
  !         EXIT
  !      END IF
  !   END DO
  !
  !   n_sweeps_stead = 0
  !   DO i_line = 1, 10000, 1
  !      READ (slot, *, iostat = ios) n0
  !      IF (ios == 0) THEN
  !         n_sweeps_stead = n_sweeps_stead + 1
  !      ELSE
  !         EXIT
  !      END IF
  !   END DO
  !
  !   CLOSE(slot)
  ! END SUBROUTINE getNumSweeps

  SUBROUTINE readStream(slot, filename, n_sweeps_therm, n_sweeps_stead, &
       pump, diss, energy, fluc_pump, fluc_diss, fluc_energy)
    INTEGER(kind = 4), INTENT(in) :: slot
    CHARACTER(LEN = *), INTENT(in) :: filename
    INTEGER(kind = 4), INTENT(in) :: n_sweeps_therm, n_sweeps_stead
    REAL(kind = 8), INTENT(out) :: pump(1:), diss(1:), energy(1:)
    REAL(kind = 8), INTENT(out) :: fluc_pump(1:)
    REAL(kind = 8), INTENT(out) :: fluc_diss(1:)
    REAL(kind = 8), INTENT(out) :: fluc_energy(1:)

    INTEGER(kind = 4) :: i_sweep, i_dum

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

  SUBROUTINE writeStream(slot, filename, n_sweeps_therm, n_sweeps_stead, &
    pump, diss, energy, fluc_pump, fluc_diss, fluc_energy)
    INTEGER(kind = 4), INTENT(in) :: slot
    CHARACTER(LEN = *), INTENT(in) :: filename
    INTEGER(kind = 4), INTENT(in) :: n_sweeps_therm, n_sweeps_stead
    REAL(kind = 8), INTENT(in) :: pump(1:), diss(1:), energy(1:)
    REAL(kind = 8), INTENT(in) :: fluc_pump(1:)
    REAL(kind = 8), INTENT(in) :: fluc_diss(1:)
    REAL(kind = 8), INTENT(in) :: fluc_energy(1:)

    INTEGER(kind = 4) :: i_sweep, i_dum

    OPEN(slot, file=filename, status="replace")
    WRITE(slot, '(a)') &
    "# i_sweep, pump, diss, energy, fluc_pump, fluc_diss, fluc_energy"
    DO i_sweep = 1, n_sweeps_therm, 1
       CALL exportStream_onfile(1, slot, i_sweep, &
       pump(i_sweep), diss(i_sweep), energy(i_sweep), &
       fluc_pump(i_sweep), fluc_diss(i_sweep), fluc_energy(i_sweep))
    END DO
    WRITE(slot, '(a)') "# -- Thermalized --"
    DO i_sweep = n_sweeps_therm + 1, n_sweeps_therm + n_sweeps_stead, 1
      CALL exportStream_onfile(1, slot, i_sweep, &
      pump(i_sweep), diss(i_sweep), energy(i_sweep), &
      fluc_pump(i_sweep), fluc_diss(i_sweep), fluc_energy(i_sweep))
    END DO
    WRITE(slot, '(a)') "# -- Steadized --"
    CLOSE(slot)
  END SUBROUTINE writeStream

  SUBROUTINE copyStream2Stream(slot, filename1, filename2, &
    n_sweeps_therm, n_sweeps_stead)
    INTEGER(kind = 4), INTENT(in) :: slot
    CHARACTER(LEN = *), INTENT(in) :: filename1, filename2
    INTEGER(kind = 4), INTENT(in) :: n_sweeps_therm, n_sweeps_stead

    REAL(kind = 8) :: pump(1:n_sweeps_therm + n_sweeps_stead)
    REAL(kind = 8) :: diss(1:n_sweeps_therm + n_sweeps_stead)
    REAL(kind = 8) :: energy(1:n_sweeps_therm + n_sweeps_stead)
    REAL(kind = 8) :: fluc_pump(1:n_sweeps_therm + n_sweeps_stead)
    REAL(kind = 8) :: fluc_diss(1:n_sweeps_therm + n_sweeps_stead)
    REAL(kind = 8) :: fluc_energy(1:n_sweeps_therm + n_sweeps_stead)

    INTEGER(kind = 4) :: n_sweeps

    n_sweeps = n_sweeps_therm + n_sweeps_stead

    CALL readStream(slot, filename1, n_sweeps_therm, n_sweeps_stead, &
    pump(1:n_sweeps), diss(1:n_sweeps), energy(1:n_sweeps), &
    fluc_pump(1:n_sweeps), fluc_diss(1:n_sweeps), fluc_energy(1:n_sweeps))

    CALL writeStream(slot, filename2, n_sweeps_therm, n_sweeps_stead, &
    pump(1:n_sweeps), diss(1:n_sweeps), energy(1:n_sweeps), &
    fluc_pump(1:n_sweeps), fluc_diss(1:n_sweeps), fluc_energy(1:n_sweeps))
  END SUBROUTINE copyStream2Stream

  SUBROUTINE addNewStreamSample2Sum(slot, filename, &
       n_sweeps_therm, n_sweeps_stead, sum_pump, sum_diss, sum_energy)
    INTEGER(kind = 4), INTENT(in) :: slot
    CHARACTER(LEN = *), INTENT(in) :: filename
    INTEGER(kind = 4), INTENT(in) :: n_sweeps_therm, n_sweeps_stead
    REAL(kind = 8), INTENT(inout) :: sum_pump(1:)
    REAL(kind = 8), INTENT(inout) :: sum_diss(1:)
    REAL(kind = 8), INTENT(inout) :: sum_energy(1:)

    INTEGER(kind = 4) :: n_sweeps
    REAL(kind = 8) :: pump(1:n_sweeps_therm + n_sweeps_stead)
    REAL(kind = 8) :: diss(1:n_sweeps_therm + n_sweeps_stead)
    REAL(kind = 8) :: energy(1:n_sweeps_therm + n_sweeps_stead)
    REAL(kind = 8) :: fluc_pump(1:n_sweeps_therm + n_sweeps_stead)
    REAL(kind = 8) :: fluc_diss(1:n_sweeps_therm + n_sweeps_stead)
    REAL(kind = 8) :: fluc_energy(1:n_sweeps_therm + n_sweeps_stead)

    CALL readStream(slot, filename, n_sweeps_therm, n_sweeps_stead, &
         pump(1:), diss(1:), energy(1:), &
         fluc_pump(1:), fluc_diss(1:), fluc_energy(1:))

    n_sweeps = n_sweeps_therm + n_sweeps_stead
    sum_pump(1:n_sweeps) = sum_pump(1:n_sweeps) + pump(1:n_sweeps)
    sum_diss(1:n_sweeps) = sum_diss(1:n_sweeps) + diss(1:n_sweeps)
    sum_energy(1:n_sweeps) = sum_energy(1:n_sweeps) + energy(1:n_sweeps)
  END SUBROUTINE addNewStreamSample2Sum

  SUBROUTINE addNewStreamSample2Sumsd(slot, filename, &
       n_sweeps_therm, n_sweeps_stead, &
       ave_pump, ave_diss, ave_energy, sumsd_pump, sumsd_diss, sumsd_energy)
    INTEGER(kind = 4), INTENT(in) :: slot
    CHARACTER(LEN = *), INTENT(in) :: filename
    INTEGER(kind = 4), INTENT(in) :: n_sweeps_therm, n_sweeps_stead
    REAL(kind = 8), INTENT(in) :: ave_pump(1:)
    REAL(kind = 8), INTENT(in) :: ave_diss(1:)
    REAL(kind = 8), INTENT(in) :: ave_energy(1:)
    REAL(kind = 8), INTENT(inout) :: sumsd_pump(1:)
    REAL(kind = 8), INTENT(inout) :: sumsd_diss(1:)
    REAL(kind = 8), INTENT(inout) :: sumsd_energy(1:)

    REAL(kind = 8) :: pump(1:n_sweeps_therm + n_sweeps_stead)
    REAL(kind = 8) :: diss(1:n_sweeps_therm + n_sweeps_stead)
    REAL(kind = 8) :: energy(1:n_sweeps_therm + n_sweeps_stead)
    REAL(kind = 8) :: fluc_pump(1:n_sweeps_therm + n_sweeps_stead)
    REAL(kind = 8) :: fluc_diss(1:n_sweeps_therm + n_sweeps_stead)
    REAL(kind = 8) :: fluc_energy(1:n_sweeps_therm + n_sweeps_stead)

    CALL readStream(slot, filename, n_sweeps_therm, n_sweeps_stead, &
         pump(1:), diss(1:), energy(1:), &
         fluc_pump(1:), fluc_diss(1:), fluc_energy(1:))

    n_sweeps = n_sweeps_therm + n_sweeps_stead
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

  SUBROUTINE readM_z(slot, filename, len_z, n_sweeps, m_z, fluc_m_z)
    INTEGER(kind = 4), INTENT(in) :: slot
    CHARACTER(LEN = *), INTENT(in) :: filename
    INTEGER(kind = 4), INTENT(in) :: n_sweeps, len_z
    REAL(kind = 8), INTENT(out) :: m_z(1:, 1:)
    REAL(kind = 8), INTENT(out), OPTIONAL :: fluc_m_z(1:, 1:)

    INTEGER(kind = 4) :: i_sweep, i_dum, z, z_dum

    OPEN(slot, file=filename, status="old")
    DO i_sweep = 1, n_sweeps, 1
       READ(slot, '()')
       DO z = 1, len_z, 1
          READ(slot, *) i_dum, z_dum, m_z(z, i_sweep), fluc_m_z(z, i_sweep)
       END DO
    END DO
    CLOSE(slot)
  END SUBROUTINE readM_z

  SUBROUTINE writeM_z_2d(slot, filename, len_x, len_z, n_sweeps, m_z, fluc_m_z)
    INTEGER(kind = 4), INTENT(in) :: slot
    CHARACTER(LEN = *), INTENT(in) :: filename
    INTEGER(kind = 4), INTENT(in) :: len_x, len_z, n_sweeps
    REAL(kind = 8), INTENT(in) :: m_z(1:, 1:)
    REAL(kind = 8), INTENT(in), OPTIONAL :: fluc_m_z(1:, 1:)

    INTEGER(kind = 4) :: i_sweep, i_dum, z, z_dum

    OPEN(slot, file=filename, status="replace")
    WRITE(slot, '(a)') "# i_sweep, z, ave_m_z, fluc_m_z"
    DO i_sweep = 1, n_sweeps, 1
      DO z = 1, len_z, 1
         WRITE(slot, '(i5, a, i4, a, f0.4, a, f0.4)') &
         i_sweep, z, m_z(z, i_sweep), fluc_m_z(z, i_sweep)
      END DO
      WRITE(slot, '()')
    END DO
    CLOSE(slot)
  END SUBROUTINE writeM_z_2d

  SUBROUTINE copyM_z2M_z_2d(slot, filename1, filename2, &
    len_x, len_z, n_sweeps_therm, n_sweeps_stead)
    INTEGER(kind = 4), INTENT(in) :: slot
    CHARACTER(LEN = *), INTENT(in) :: filename1, filename2
    INTEGER(kind = 4), INTENT(in) :: len_x, len_z
    INTEGER(kind = 4), INTENT(in) :: n_sweeps_therm, n_sweeps_stead

    REAL(kind = 8) :: m_z(1:len_z, 1:n_sweeps_therm + n_sweeps_stead)
    REAL(kind = 8) :: fluc_m_z(1:len_z, 1:n_sweeps_therm + n_sweeps_stead)

    INTEGER(kind = 4) :: n_sweeps

    n_sweeps = n_sweeps_therm + n_sweeps_stead

    CALL readM_z(slot, filename1, len_z, n_sweeps, &
    m_z(1:len_z, 1:n_sweeps), fluc_m_z(1:len_z, 1:n_sweeps))

    CALL writeM_z_2d(slot, filename2, len_x, len_z, n_sweeps, &
    m_z(1:len_z, 1:n_sweeps), fluc_m_z(1:len_z, 1:n_sweeps))
  END SUBROUTINE copyM_z2M_z_2d

  SUBROUTINE addNewM_zSample2Sum(slot, filename, &
       len_z, n_sweeps, sum_m_z)
    INTEGER(kind = 4), INTENT(in) :: slot
    CHARACTER(LEN = *), INTENT(in) :: filename
    INTEGER(kind = 4), INTENT(in) :: len_z, n_sweeps
    REAL(kind = 8), INTENT(inout) :: sum_m_z(1:, 1:)

    REAL(kind = 8) :: m_z(1:len_z, 1:n_sweeps)
    REAL(kind = 8) :: fluc_m_z(1:len_z, 1:n_sweeps)

    CALL readM_z(slot, filename, len_z, n_sweeps, &
         m_z(1:, 1:), fluc_m_z(1:, 1:))

    sum_m_z(1:len_z, 1:n_sweeps) = sum_m_z(1:len_z, 1:n_sweeps) &
         + m_z(1:len_z, 1:n_sweeps)
  END SUBROUTINE addNewM_zSample2Sum

  SUBROUTINE addNewM_zSample2Sumsd(slot, filename, &
       len_z, n_sweeps, ave_m_z, sumsd_m_z)
    INTEGER(kind = 4), INTENT(in) :: slot
    CHARACTER(LEN = *), INTENT(in) :: filename
    INTEGER(kind = 4), INTENT(in) :: len_z, n_sweeps
    REAL(kind = 8), INTENT(in) :: ave_m_z(1:, 1:)
    REAL(kind = 8), INTENT(inout) :: sumsd_m_z(1:, 1:)

    REAL(kind = 8) :: m_z(1:len_z, 1:n_sweeps)

    CALL readM_z(slot, filename, len_z, n_sweeps, &
    m_z(1:len_z, 1:n_sweeps), sumsd_m_z(1:len_z, 1:n_sweeps))

    sumsd_m_z(1:len_z, 1:n_sweeps) = sumsd_m_z(1:len_z, 1:n_sweeps) + &
         (m_z(1:len_z, 1:n_sweeps) - ave_m_z(1:len_z, 1:n_sweeps)) ** 2
  END SUBROUTINE addNewM_zSample2Sumsd

  SUBROUTINE addOldM_z(slot, filename, &
       len_z, n_sweeps, sum_m_z, sumsd_m_z)
    INTEGER(kind = 4), INTENT(in) :: slot
    CHARACTER(LEN = *), INTENT(in) :: filename
    INTEGER(kind = 4), INTENT(in) :: len_z, n_sweeps
    REAL(kind = 8), INTENT(inout) :: sum_m_z(1:, 1:)
    REAL(kind = 8), INTENT(inout) :: sumsd_m_z(1:, 1:)

    REAL(kind = 8) :: ave_m_z(1:len_z, 1:n_sweeps)
    REAL(kind = 8) :: fluc_m_z(1:len_z, 1:n_sweeps)

    CALL readM_z(slot, filename, len_z, n_sweeps, &
         ave_m_z(1:, 1:), fluc_m_z(1:, 1:))

    sum_m_z(1:len_z, 1:n_sweeps) = sum_m_z(1:len_z, 1:n_sweeps) &
         + ave_m_z(1:len_z, 1:n_sweeps)
    sumsd_m_z(1:len_z, 1:n_sweeps) = sumsd_m_z(1:len_z, 1:n_sweeps) &
         + fluc_m_z(1:len_z, 1:n_sweeps)
  END SUBROUTINE addOldM_z

  SUBROUTINE importSpin_2d(slot, filename, len_x, len_z, spin)
    INTEGER(kind = 4), INTENT(in) :: slot
    CHARACTER(len = *, kind = 1), INTENT(in) :: filename
    INTEGER(kind = 4), INTENT(in) :: len_x, len_z
    INTEGER(kind = 4), INTENT(out) :: spin(1:, 1:)

    INTEGER(kind = 4) :: x, z, dum_x, dum_z

    OPEN(slot, file=filename, status="old")
    DO z = 1, len_z, 1
       READ(slot, '()')
       DO x = 1, len_x, 1
          READ(slot, *) dum_x, dum_z, spin(x, z)
       END DO
    END DO
    CLOSE(slot)
  END SUBROUTINE importSpin_2d

  SUBROUTINE importSpin_3d(slot, filename, len_x, len_y, len_z, spin)
    INTEGER(kind = 4), INTENT(in) :: slot
    CHARACTER(len = *, kind = 1), INTENT(in) :: filename
    INTEGER(kind = 4), INTENT(in) :: len_x, len_y, len_z
    INTEGER(kind = 4), INTENT(out) :: spin(1:, 1:, 1:)

    INTEGER(kind = 4) :: x, y, z, dum_x, dum_y, dum_z

    OPEN(slot, file=filename, status="old")
    DO z = 1, len_z, 1
       DO y = 1, len_y, 1
          READ(slot, '()')
          DO x = 1, len_x, 1
             READ(slot, *) dum_x, dum_y, dum_z, spin(x, y, z)
          END DO
       END DO
    END DO
    CLOSE(slot)
  END SUBROUTINE importSpin_3d

  SUBROUTINE exportSpin_2d(slot, filename, len_x, len_z, spin)
    INTEGER(kind = 4), INTENT(in) :: slot
    CHARACTER(len = *, kind = 1), INTENT(in) :: filename
    INTEGER(kind = 4), INTENT(in) :: len_x, len_z
    INTEGER(kind = 4), INTENT(in) :: spin(1:, 1:)

    INTEGER(kind = 4) :: x, z

    OPEN(slot, file=filename, status="replace")
    WRITE(slot, '(a)') "# x, z, spin"
    DO z = 1, len_z, 1
       DO x = 1, len_x, 1
          WRITE(slot, '(i0.4, a, i0.4, a, i0.4)') &
               x, ", ", z, ", ", spin(x, z)
       END DO
       WRITE(slot, '()')
    END DO
    CLOSE(slot)
  END SUBROUTINE exportSpin_2d

  SUBROUTINE exportSpin_3d(slot, filename, len_x, len_y, len_z, spin)
    INTEGER(kind = 4), INTENT(in) :: slot
    CHARACTER(len = *, kind = 1), INTENT(in) :: filename
    INTEGER(kind = 4), INTENT(in) :: len_x, len_y, len_z
    INTEGER(kind = 4), INTENT(in) :: spin(1:, 1:, 1:)

    INTEGER(kind = 4) :: x, y, z

    OPEN(slot, file=filename, status="replace")
    WRITE(slot, '(a)') "# x, z, spin"
    DO z = 1, len_z, 1
       DO y = 1, len_y, 1
          DO x = 1, len_x, 1
             WRITE(slot, '(i4, a, i4, a, i4, a, i4)') &
                  x, ", ", y, ", ", z, ", ", spin(x, y, z)
          END DO
          WRITE(slot, '()')
       END DO
    END DO
    CLOSE(slot)
  END SUBROUTINE exportSpin_3d

  SUBROUTINE exportM_z_onfile_2d(onoff_m_z, slot, &
       i_sweep, len_x, len_z, spin)
    INTEGER(kind = 4), INTENT(in) :: onoff_m_z, slot
    INTEGER(kind = 4), INTENT(in) :: i_sweep, len_x, len_z, &
    spin(1:, 1:)

    INTEGER(kind = 4) :: z
    REAL(kind = 8) :: m_z(1:len_z)

    SELECT CASE (onoff_m_z)
    CASE (0)
       RETURN
    CASE (1)
       DO z = 1, len_z, 1
          m_z(z) = DBLE(SUM(spin(1:len_x, z))) / DBLE(len_x)
          WRITE(slot, '( i5, a, i5, a, f0.4, a, f0.4)') &
               i_sweep, ", ", z, ", ", m_z(z), ", ", 0.0d0
       END DO
       WRITE(slot, '()')
    END SELECT
  END SUBROUTINE exportM_z_onfile_2d

  SUBROUTINE exportM_z_onfile_3d(onoff_m_z, slot, &
       i_sweep, len_x, len_y, len_z, spin)
    INTEGER(kind = 4), INTENT(in) :: onoff_m_z, slot
    INTEGER(kind = 4), INTENT(in) :: i_sweep, len_x, len_y, len_z, &
    spin(1:, 1:, 1:)

    INTEGER(kind = 4) :: z
    REAL(kind = 8) :: m_z(1:len_z)

    SELECT CASE (onoff_m_z)
    CASE (0)
       RETURN
    CASE (1)
       DO z = 1, len_z, 1
          m_z(z) = DBLE(SUM(spin(1:len_x, 1:len_y, z))) / DBLE(len_x * len_y)
          WRITE(slot, '( i5, a, i5, a, f0.4, a, f0.4)') &
               i_sweep, ", ", z, ", ", m_z(z), ", ", 0.0d0
       END DO
       WRITE(slot, '()')
    END SELECT
  END SUBROUTINE exportM_z_onfile_3d

  SUBROUTINE readthroughM_z_onfile(onoff_m_z, slot, &
       start_sweep, end_sweep, len_z)
    INTEGER(kind = 4), INTENT(in) :: onoff_m_z, slot
    INTEGER(kind = 4), INTENT(in) :: start_sweep, end_sweep, len_z

    INTEGER(kind = 4) :: i_sweep, z

    SELECT CASE (onoff_m_z)
    CASE (0)
       RETURN
    CASE (1)
       DO i_sweep = start_sweep, end_sweep, 1
          DO z = 1, len_z, 1
             READ(slot, '()')
          END DO
          READ(slot, '()')
       END DO
    END SELECT
  END SUBROUTINE readthroughM_z_onfile

  SUBROUTINE exportStream_onfile(onoff_stream, slot, &
       i_sweep, &
       pump, diss, energy, &
       fluc_pump, fluc_diss, fluc_energy)
    INTEGER(kind = 4), INTENT(in) :: onoff_stream, slot
    INTEGER(kind = 4), INTENT(in) :: i_sweep
    REAL(kind = 8), INTENT(in) :: pump, diss, energy
    REAL(kind = 8), INTENT(in) :: fluc_pump, fluc_diss, fluc_energy

    SELECT CASE (onoff_stream)
    CASE (0)
       RETURN
    CASE (1)
       WRITE(slot, '(  i5, a, f0.4, a, f0.4, a, f0.4, a, &
            f0.4, a, f0.4, a, f0.4)') &
            i_sweep, ", ", pump, ", ", diss, ", ", energy, ", ", &
            fluc_pump, ", ", fluc_diss, ", ", fluc_energy
    END SELECT
  END SUBROUTINE exportStream_onfile

  SUBROUTINE readthroughStream_onfile(onoff_stream, slot, &
       start_sweep, end_sweep, pump, diss, energy)
    INTEGER(kind = 4), INTENT(in) :: onoff_stream, slot
    INTEGER(kind = 4), INTENT(in) :: start_sweep, end_sweep
    REAL(kind = 8), INTENT(in) :: pump, diss, energy

    INTEGER(kind = 4) :: i_sweep

    SELECT CASE (onoff_stream)
    CASE (0)
       RETURN
    CASE (1)
       READ(slot, '()')
    END SELECT
  END SUBROUTINE readthroughStream_onfile
END MODULE mod_proc_file
