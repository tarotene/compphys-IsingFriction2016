MODULE main_procedures
  IMPLICIT NONE

CONTAINS
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
END MODULE main_procedures
