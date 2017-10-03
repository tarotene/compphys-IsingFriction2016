MODULE mod_rng
  USE global_variables
  USE MKL_VSL_TYPE
  USE MKL_VSL

  IMPLICIT NONE
CONTAINS
  SUBROUTINE initializeRN(seed, str, err)
    INTEGER(kind = 4), INTENT(in) :: seed
    TYPE (VSL_STREAM_STATE), INTENT(out) :: str
    INTEGER(kind = 4), INTENT(out) :: err

    INTEGER(kind = 4) :: brng

    brng = VSL_BRNG_MT19937
    err = vslnewstream(str, brng, seed)
  END SUBROUTINE initializeRN

  SUBROUTINE generateRN_int(str, lb, ub, len_rns, rn, err)
    TYPE (VSL_STREAM_STATE), INTENT(in) :: str
    INTEGER(kind = 4), INTENT(in) :: lb, ub, len_rns
    INTEGER(kind = 4), INTENT(out) :: rn(1:)
    INTEGER(kind = 4), INTENT(out) :: err

    INTEGER(kind = 4) :: method

    method = VSL_RNG_METHOD_UNIFORM_STD
    err = virnguniform(method, str, len_rns, rn, lb, ub + 1)
  END SUBROUTINE generateRN_int

  SUBROUTINE generateRN_real(str, lb, ub, len_rns, rn, err)
    TYPE (VSL_STREAM_STATE), INTENT(in) :: str
    REAL(kind = 8), INTENT(in) :: lb, ub
    INTEGER(kind = 4), INTENT(in) :: len_rns
    REAL(kind = 8), INTENT(out) :: rn(1:)
    INTEGER(kind = 4), INTENT(out) :: err

    INTEGER(kind = 4) :: method

    method = VSL_RNG_METHOD_UNIFORM_STD
    err = vdrnguniform(method, str, len_rns, rn, lb, ub)
  END SUBROUTINE generateRN_real

  SUBROUTINE destractRN(str, err)
    TYPE (VSL_STREAM_STATE), INTENT(in) :: str
    INTEGER(kind = 4), INTENT(out) :: err

    err = vsldeletestream(str)
  END SUBROUTINE destractRN

  SUBROUTINE loadRNstat(str, filename, err)
    CHARACTER(len = *, kind = 1), INTENT(in) :: filename
    TYPE (VSL_STREAM_STATE), INTENT(out) :: str
    INTEGER(kind = 4), INTENT(out) :: err

    err = vslloadstreamf(str, TRIM(filename))
  END SUBROUTINE loadRNstat

  SUBROUTINE saveRNstat(str, filename, err)
    CHARACTER(len = *, kind = 1), INTENT(in) :: filename
    TYPE (VSL_STREAM_STATE), INTENT(in) :: str
    INTEGER(kind = 4), INTENT(out) :: err

    err = vslsavestreamf(str, TRIM(filename))
  END SUBROUTINE saveRNstat
END MODULE mod_rng
