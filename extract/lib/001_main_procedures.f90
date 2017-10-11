MODULE main_procedures
  USE global_variables
  IMPLICIT NONE
CONTAINS
  SUBROUTINE calcMeanErr(n_samples, stream, fluc, mean, err)
    INTEGER(kind = 4), INTENT(in) :: n_samples
    REAL(kind = 8), INTENT(in) :: stream(1:), fluc(1:)
    REAL(kind = 8), INTENT(out) :: mean
    REAL(kind = 8), INTENT(out) :: err

    INTEGER(kind = 4) :: t, t_prime

    mean = SUM(stream(1:)) / DBLE(SIZE(stream(1:)))
    err = SQRT(SUM(fluc(1:)) / n_samples) / DBLE(SIZE(fluc(1:)))
  END SUBROUTINE calcMeanErr
END MODULE main_procedures
