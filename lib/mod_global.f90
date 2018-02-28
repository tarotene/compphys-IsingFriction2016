  MODULE mod_global
    IMPLICIT NONE

    ! internal paramters
    INTEGER(kind = 4), PARAMETER :: i8b = selected_int_KIND(18)  ! eight-byte integer

    ! condition parameters
    INTEGER(kind = 4), SAVE :: id_IC, id_BC

    ! omp parameters
    INTEGER(kind = 4), SAVE :: n_ths

    ! general parameters
    INTEGER(kind = 4), SAVE :: len_x, len_y, len_z, vel
    INTEGER(kind = 4), SAVE :: len_s, len_s0
    INTEGER(kind = 4), SAVE :: len_t1, len_t2
    REAL(kind = 8), SAVE :: beta

    !reduced parameters
    INTEGER(kind = 4), SAVE :: n_sweeps
    REAL(kind = 8), SAVE :: pr_2d(-1:1, -1:1, -1:1, -1:1, -1:1)
    REAL(kind = 8), SAVE :: pr_3d(-1:1, -1:1, -1:1, -1:1, -1:1, -1:1, -1:1)
  END MODULE mod_global
