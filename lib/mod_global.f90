MODULE mod_global
  IMPLICIT NONE

  ! internal paramters
  INTEGER(4), PARAMETER :: i8b = selected_int_KIND(18)  ! eight-byte integer

  ! condition parameters
  INTEGER(4), SAVE :: id_IC, id_BC

  ! general parameters
  INTEGER(4), SAVE :: l_x, l_y, l_z, vel, n_s, n_s0, l_t, n_st, l_th, n_b
  REAL(8), SAVE :: beta

  !reduced parameters
  REAL(8), SAVE :: p_2d(-1:1, -1:1, -1:1, -1:1, -1:1)
  REAL(8), SAVE :: p_3d(-1:1, -1:1, -1:1, -1:1, -1:1, -1:1, -1:1)
END MODULE mod_global
