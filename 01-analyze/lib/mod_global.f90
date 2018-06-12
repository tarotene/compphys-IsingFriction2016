MODULE mod_global

  IMPLICIT NONE

  ! parameters
  INTEGER(4) :: l_x, l_z, n_betas
  INTEGER(4) :: l_t0, l_t1, n_st, max_l
  INTEGER(4) :: id_IC, id_BC, n_w
  REAL(8), ALLOCATABLE :: MSC_beta(:,:)
  CHARACTER(len=11), ALLOCATABLE :: sbeta(:,:)
  ! MSC slot vars
  INTEGER(4), ALLOCATABLE :: MSC_sl_iw(:), MSC_sl_eb(:,:), MSC_sl_mb(:,:), MSC_sl_ee(:,:), MSC_sl_me(:,:)
END MODULE mod_global
