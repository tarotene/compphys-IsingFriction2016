INCLUDE 'mkl_vsl.f90'

MODULE mod_global
  USE MKL_VSL_TYPE
  USE MKL_VSL

  IMPLICIT NONE

  !parameters
  INTEGER(kind = 4), SAVE :: len_x, len_z, vel, id_init, id_bound
  INTEGER(kind = 4), SAVE :: n_samples, n_samples_old, n_samples_new
  INTEGER(kind = 4), SAVE :: n_sweeps_therm, n_sweeps_stead
  REAL(kind = 8), SAVE :: J, beta

  !reduced parameters
  INTEGER(kind = 4), SAVE :: n_sweeps
  REAL(kind = 8), SAVE :: prob(-1:1, -1:1, -1:1, -1:1, -1:1)
  REAL(kind = 8), SAVE :: deltaE(-1:1, -1:1, -1:1, -1:1, -1:1)
END MODULE mod_global
