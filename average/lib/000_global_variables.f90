MODULE global_variables
  IMPLICIT NONE

  !input parameters
  INTEGER(kind = 4), SAVE :: len_x, len_z

  !parameters
  INTEGER(kind = 4), SAVE :: n_samples, n_samples_old, n_samples_new

  !reduced parameters
  INTEGER(kind = 4), SAVE :: n_sweeps_therm, n_sweeps_stead, n_sweeps
END MODULE global_variables
