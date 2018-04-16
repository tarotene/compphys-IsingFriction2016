  INCLUDE 'mkl_vsl.f90'

  MODULE mod_global
    USE MKL_VSL_TYPE
    USE MKL_VSL

    IMPLICIT NONE

    ! physical parameters
    INTEGER(4), SAVE :: l_x, l_y, l_z, vel, n_s, n_s0, l_t, n_st, l_th, n_b
    REAL(8), SAVE :: beta
		INTEGER(4), SAVE :: id_IC, id_BC

    ! reduced physical parameters
    REAL(8), SAVE :: p_2d(-1:1, -1:1, -1:1, -1:1, -1:1)
    REAL(8), SAVE :: p_3d(-1:1, -1:1, -1:1, -1:1, -1:1, -1:1, -1:1)

		! spin variables
		INTEGER(1), ALLOCATABLE :: IS2_ini1(:, :), IS3_ini1(:, :)
		INTEGER(4), ALLOCATABLE :: IS2_ini(:, :), IS3_ini(:, :)
		INTEGER(4), ALLOCATABLE, SAVE :: IS2(:, :), IS3(:, :)

		! physical quantities
		INTEGER(4), ALLOCATABLE :: eb(:), mb(:), pmp(:)
		INTEGER(4) :: ee, me

		! random streams
	  TYPE(VSL_STREAM_STATE), SAVE :: str_x, str_z, str_p

	  ! random arrays
		REAL(8), ALLOCATABLE :: r_p(:)
	  INTEGER(4), ALLOCATABLE :: r_x(:), r_z(:)

		! counters
	  INTEGER(4) :: i_v, t, s
	  CHARACTER(8) :: st
	  CHARACTER(4) :: ss

		! I/O slots
		INTEGER(4), SAVE :: sl_sp, sl_en, sl_eb, sl_m, sl_mb
		INTEGER(4), SAVE :: sl_ee, sl_me, sl_p
  END MODULE mod_global
