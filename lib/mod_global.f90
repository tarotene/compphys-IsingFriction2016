INCLUDE 'mkl_vsl.f90'

MODULE mod_global
  USE MKL_VSL_TYPE
  USE MKL_VSL

  IMPLICIT NONE

  ! condition parameters
  INTEGER(kind = 4), SAVE :: id_IC, id_BC, onoff_stream, onoff_m_z

  ! omp parameters
  INTEGER(kind = 4), SAVE :: n_ths

  ! general parameters
  INTEGER(kind = 4), SAVE :: len_x, len_z, vel
  INTEGER(kind = 4), SAVE :: n_samples, n_samples0
  INTEGER(kind = 4), SAVE :: n_samples_old, n_samples_new
  INTEGER(kind = 4), SAVE :: n_sweeps_therm, n_sweeps_stead
  INTEGER(kind = 4), SAVE :: n_sweeps_therm0, n_sweeps_stead0
  REAL(kind = 8), SAVE :: J, beta

  !reduced parameters
  INTEGER(kind = 4), SAVE :: n_sweeps
  REAL(kind = 8), SAVE :: prob_2d(-1:1, -1:1, -1:1, -1:1, -1:1)
  REAL(kind = 8), SAVE :: deltaE_2d(-1:1, -1:1, -1:1, -1:1, -1:1)
  REAL(kind = 8), SAVE :: prob_3d(-1:1, -1:1, -1:1, -1:1, -1:1, -1:1, -1:1)
  REAL(kind = 8), SAVE :: deltaE_3d(-1:1, -1:1, -1:1, -1:1, -1:1, -1:1, -1:1)
END MODULE mod_global
