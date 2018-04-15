MODULE mod_global
  IMPLICIT NONE

  INTEGER(1), ALLOCATABLE :: IS2_ini1(:, :), IS3_ini1(:, :)
  INTEGER(4), ALLOCATABLE :: IS2(:, :), IS2_ini(:, :), IS3(:, :), IS3_ini(:, :)
  INTEGER(4), ALLOCATABLE :: eb(:), mb(:), r_x(:), r_z(:)
  INTEGER(4) :: err
  INTEGER(4) :: i_v, t, s
  
  INTEGER(4) :: sl_sp, sl_en, sl_eb, sl_m, sl_mb

  INTEGER(4) :: ee, me
  INTEGER(4) :: sl_ee, sl_me, sl_p
  INTEGER(4), ALLOCATABLE :: pmp(:)

  REAL(8), ALLOCATABLE :: r_p(:)
  TYPE(VSL_STREAM_STATE) :: str_x, str_z, str_p
  CHARACTER(8) :: st
  CHARACTER(4) :: ss

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
