INCLUDE 'mkl_vsl.f90'

PROGRAM main
  !$  USE omp_lib
  USE mod_global
  USE MKL_VSL_TYPE
  USE MKL_VSL
  USE mod_proc
  USE IFPORT, ONLY: access

  IMPLICIT NONE

  INTEGER(1), ALLOCATABLE :: sp_ini1(:, :)
  INTEGER(4), ALLOCATABLE :: sp(:, :), sp_ini(:, :), eb(:), mb(:), r_x(:), r_z(:)
  INTEGER(4) :: err
  INTEGER(4) :: i_st, i_v, t, s, x, z
  
  INTEGER(4) :: sl_sp, sl_en, sl_eb, sl_m, sl_mb

  INTEGER(4) :: ee, me
  INTEGER(4) :: sl_ee, sl_me, sl_p
  INTEGER(4), ALLOCATABLE :: pmp(:)

  REAL(8), ALLOCATABLE :: r_p(:)
  TYPE(VSL_STREAM_STATE) :: str_x, str_z, str_p
  CHARACTER(8) :: st
  CHARACTER(4) :: ss
  INTEGER(4) :: center, east, west, south, north

  CALL inputParams_2d(l_x, l_z, beta, vel, l_t, id_IC, id_BC, n_s)
  n_st = l_x * l_z / vel
  CALL countSamples(n_s0)
  CALL metropolis_2d(beta, p_2d)

  ALLOCATE(r_x(1:n_st), r_z(1:n_st), r_p(1:n_st))
  ALLOCATE(sp_ini1(0:l_x + 1, 0:l_z + 1))
  ALLOCATE(eb(0:n_st), mb(0:n_st), sp(0:l_x + 1, 0:l_z + 1), sp_ini(0:l_x + 1, 0:l_z + 1))
  ! eb(0:n_st) = 0
  ! mb(0:n_st) = 0
  CALL initSp_2d(sp_ini(0:l_x + 1, 0:l_z + 1))

  ALLOCATE(pmp(1:vel))

  !$omp parallel do schedule(static, 1) default(none) &
  !$omp shared(n_s0, n_s, l_t, l_x, l_z, vel, n_st) &
  !$omp private(s, sl_sp, sl_en, sl_eb, sl_m, sl_mb) &
  !$omp private(i_st, t, i_v, err, str_x, str_z, str_p, r_x, r_z, r_p, ss, st, eb, mb, sp, sp_ini1) &
  !$omp firstprivate(sp_ini) &
  !$omp private(sl_ee, sl_me, sl_p, ee, pmp)
  DO s = 1, n_s0, 1
     sl_sp = 20 + s + 0 * n_s

    !  sl_en = 20 + s + 1 * n_s
    !  sl_m = 20 + s + 2 * n_s

     sl_eb = 20 + s + 3 * n_s
     sl_mb = 20 + s + 4 * n_s

     sl_ee = 20 + s + 5 * n_s
     sl_me = 20 + s + 6 * n_s
     sl_p = 20 + s + 7 * n_s

     WRITE(ss, '(i0.4)') s
     ! OPEN(sl_en, file="en_step/en_s"//ss//"_step.bin", access="stream", status="old", position="append")
     ! OPEN(sl_m, file="m_step/m_s"//ss//"_step.bin", access="stream", status="old", position="append")
     OPEN(sl_eb, file="eb_sweep/en_bulk_s"//ss//"_sweep.bin", access="stream", status="old", position="append")
     OPEN(sl_mb, file="mb_sweep/m_bulk_s"//ss//"_sweep.bin", access="stream", status="old", position="append")

     OPEN(sl_ee, file="ee_sweep/en_edge_s"//ss//"_sweep.bin", access="stream", status="old", position="append")
     OPEN(sl_me, file="me_sweep/m_edge_s"//ss//"_sweep.bin", access="stream", status="old", position="append")
     OPEN(sl_p, file="p_sweep/pump_s"//ss//"_sweep.bin", access="stream", status="old", position="append")
     
     err = vslloadstreamf(str_p, "str_p_s"//ss//".bin")
     err = vslloadstreamf(str_x, "str_x_s"//ss//".bin")
     err = vslloadstreamf(str_z, "str_z_s"//ss//".bin")

     OPEN(sl_sp, file="sp_fin_s"//ss//".bin", access="stream", status="old")
     READ(sl_sp) sp_ini1(1:l_x, 1:l_z)
     CLOSE(sl_sp)
     sp_ini(1:l_x, 1:l_z) = INT4(sp_ini1(1:l_x, 1:l_z))
  
     CALL calcEn_2d(sp_ini(0:l_x + 1, 0:l_z + 1), eb(0))
     mb(0) = SUM(sp_ini(1:l_x, 1:l_z))

     sp(0:l_x + 1, 0:l_z + 1) = sp_ini(0:l_x + 1, 0:l_z + 1)

     DO t = 1, l_t, 1
        DO i_v = 1, vel, 1
           CALL shift_2d(sp(0:l_x + 1, 0:l_z + 1), pmp(i_v), eb(0))
           err = vdrnguniform(VSL_RNG_METHOD_UNIFORM_STD, str_p, n_st, r_p(1:n_st), 0.0d0, 1.0d0)
           err = virnguniform(VSL_RNG_METHOD_UNIFORM_STD, str_x, n_st, r_x(1:n_st), 1, l_x + 1)
           err = virnguniform(VSL_RNG_METHOD_UNIFORM_STD, str_z, n_st, r_z(1:n_st), 1, l_z + 1)
           CALL mSSFs_2d(r_x(1:n_st), r_z(1:n_st), r_p(1:n_st), sp(0:l_x + 1, 0:l_z + 1), eb(0:n_st), mb(0:n_st))
           ! WRITE(sl_en) eb(0:n_st)
           eb(0) = eb(n_st)
           ! WRITE(sl_m) mb(0:n_st)
           mb(0) = mb(n_st)
        END DO

        ! WRITE(st, '(i0.8)') t
        ! OPEN(sl_sp, file="sp_sweep/sp_s"//ss//"t"//st//"_sweep.bin", access="stream", status="replace")
        ! WRITE(sl_sp) INT1(sp(1:l_x, 1:l_z))
        ! CLOSE(sl_sp)
        WRITE(sl_eb) eb(0)
        WRITE(sl_mb) mb(0)

        CALL calcEE_2d(sp(0:l_x + 1, 0:l_z + 1), ee)
        WRITE(sl_ee) ee
        WRITE(sl_me) SUM(sp(1:l_x, l_z / 2:l_z / 2 + 1))
        WRITE(sl_p) SUM(pmp(1:vel))
     END DO

     err = vslsavestreamf(str_p, "str_p_s"//ss//".bin")
     err = vslsavestreamf(str_x, "str_x_s"//ss//".bin")
     err = vslsavestreamf(str_z, "str_z_s"//ss//".bin")

     err = vsldeletestream(str_p)
     err = vsldeletestream(str_x)
     err = vsldeletestream(str_z)
     ! CLOSE(sl_en)
     ! CLOSE(sl_m)
     
     CLOSE(sl_eb)
     CLOSE(sl_mb)

     CLOSE(sl_ee)
     CLOSE(sl_me)
     CLOSE(sl_p)

     OPEN(sl_sp, file="sp_fin_s"//ss//".bin", access="stream", status="replace")
     WRITE(sl_sp) INT1(sp(1:l_x, 1:l_z))
     CLOSE(sl_sp)
  END DO
  !$omp end parallel do
END PROGRAM main
