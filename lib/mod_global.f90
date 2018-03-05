  MODULE mod_global
    IMPLICIT NONE

    ! internal paramters
    INTEGER(kind = 4), PARAMETER :: i8b = selected_int_KIND(18)  ! eight-byte integer

    ! condition parameters
    INTEGER(kind = 4), SAVE :: id_IC, id_BC

    ! general parameters
    INTEGER(kind = 4), SAVE :: l_x, l_y, l_z, vel, n_s, n_s0, l_t
    REAL(kind = 8), SAVE :: beta

    !reduced parameters
    REAL(kind = 8), SAVE :: pr_2d(-1:1, -1:1, -1:1, -1:1, -1:1)
    REAL(kind = 8), SAVE :: pr_3d(-1:1, -1:1, -1:1, -1:1, -1:1, -1:1, -1:1)
  END MODULE mod_global
