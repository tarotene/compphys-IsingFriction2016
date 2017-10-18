MODULE main_procedures
  USE global_variables
  IMPLICIT NONE
CONTAINS
  SUBROUTINE getNumSamples(slot, n_samples)
    INTEGER(kind = 4), INTENT(in) :: slot
    INTEGER(kind = 4), INTENT(out) :: n_samples

    INTEGER(kind = 4) :: i_line, ios, n_dum

    n_samples = 0
    OPEN(slot, file="list_samples.dat", status="old")
    DO i_line = 1, 100000, 1
       READ (slot, *, iostat = ios) n_dum
       IF (ios < 0) EXIT
       n_samples = n_samples + 1
    END DO
    CLOSE(slot)
  END SUBROUTINE getNumSamples

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

    DO i_line = 1, 100000, 1
       READ (slot, *, iostat = ios) n0
       IF (ios == 0) THEN
          n_sweeps_therm = n_sweeps_therm + 1
       ELSE
          EXIT
       END IF
    END DO

    DO i_line = 1, 100000, 1
       READ (slot, *, iostat = ios) n0
       IF (ios == 0) THEN
          n_sweeps_stead = n_sweeps_stead + 1
       ELSE
          EXIT
       END IF
    END DO

    CLOSE(slot)
  END SUBROUTINE getNumSweeps

  SUBROUTINE calcAutocorrelation(stream, ac)
    REAL(kind = 8), INTENT(in) :: stream(1:)
    REAL(kind = 8), INTENT(out) :: ac(0:)

    REAL(kind = 8) :: mean
    INTEGER(kind = 4) :: t, t_prime, len_str

    len_str = SIZE(stream(:))

    mean = SUM(stream(1:len_str)) / DBLE(len_str)

    ac(0:len_str / 2) = 0.0d0
    DO t = 0, len_str / 2, 1
       DO t_prime = 1, len_str - t, 1
          ac(t) = ac(t) + stream(t_prime) * stream(t_prime + t)
       END DO
       ac(t) = ac(t) / DBLE(len_str - t)
    END DO
    ac(0:len_str / 2) = ac(0:len_str / 2) - mean ** 2
  END SUBROUTINE calcAutocorrelation

  SUBROUTINE calcCorrelationTime(ac, tau)
    REAL(kind = 8), INTENT(in) :: ac(0:)
    REAL(kind = 8), INTENT(out) :: tau

    INTEGER(kind = 4) :: len_ac

    len_ac = SIZE(ac(:))

    IF ( ac(0) <= EPSILON(0.0d0) ) THEN
       tau = 1.0d0
    ELSE
       tau = SUM(ac(0:len_ac - 1)) / ac(0)
    END IF
  END SUBROUTINE calcCorrelationTime
END MODULE main_procedures
