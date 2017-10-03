INCLUDE 'mkl_vsl.f90'

MODULE global_variables
  USE MKL_VSL_TYPE
  USE MKL_VSL

  IMPLICIT NONE

  !input variables
  INTEGER(kind = 4), SAVE :: len_x, len_z, vel, &
       n_sweeps_therm, n_sweeps_stead, id_init, id_bound, &
       n_samples
  REAL(kind = 8), SAVE :: J, beta

  !reduced variables
  INTEGER(kind = 4), SAVE :: n_steps, n_sweeps
  REAL(kind = 8), SAVE :: prob(-1:1, -1:1, -1:1, -1:1, -1:1)
  REAL(kind = 8), SAVE :: deltaE(-1:1, -1:1, -1:1, -1:1, -1:1)

  !rng variables
  TYPE (VSL_STREAM_STATE), ALLOCATABLE :: str_x(:), str_z(:), str_prob(:)
END MODULE global_variables
