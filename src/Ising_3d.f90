INCLUDE 'mkl_vsl.f90'

PROGRAM main
  !$  USE omp_lib
  USE mod_global
  USE MKL_VSL_TYPE
  USE MKL_VSL
  USE mod_proc
  USE IFPORT, ONLY: access

  IMPLICIT NONE

  CALL inputParams_3d(l_x, l_y, l_z, beta, vel, l_t, id_IC, id_BC, n_s)
  n_st = l_x * l_y * l_z / vel
  CALL countSamples(n_s0)
  CALL metropolis_3d(beta, p_3d)

  ALLOCATE(r_x(1:n_st), r_y(1:n_st), r_z(1:n_st), r_p(1:n_st))
  ALLOCATE(eb(0:n_st), mb(0:n_st), IS3(0:l_x + 1, 0:l_y + 1, 0:l_z + 1), IS3_ini0:l_x + 1, 0:l_y + 1, 0:l_z + 1))
  eb(0:n_st) = 0
  mb(0:n_st) = 0
  CALL initSp_3d(IS3_ini0:l_x + 1, 0:l_y + 1, 0:l_z + 1))

  ALLOCATE(pmp(1:vel))

  !$omp parallel do schedule(static, 1) default(none) &
  !$omp shared(n_s0, n_s, l_t, l_x, l_y,  l_z, vel, n_st, sp_ini) &
  !$omp private(s, sl_sp, sl_en, sl_eb, sl_m, sl_mb) &
  !$omp private(i_st, t, i_v, err, str_x, str_y, str_z, str_p, r_x, r_y, r_z, r_p, ss, st, eb, mb, sp) &
  !$omp private(sl_ee, sl_me, sl_p, ee, pmp)
  DO s = n_s0 + 1, n_s, 1
     sl_sp = 20 + s + 0 * n_s

    !  sl_en = 20 + s + 1 * n_s
    !  sl_m = 20 + s + 2 * n_s

     sl_eb = 20 + s + 3 * n_s
     sl_mb = 20 + s + 4 * n_s

     sl_ee = 20 + s + 5 * n_s
     sl_me = 20 + s + 6 * n_s
     sl_p = 20 + s + 7 * n_s

     WRITE(ss, '(i0.4)') s
     ! OPEN(sl_en, file="en_step/en_s"//ss//"_step.bin", access="stream", status="replace")
     ! OPEN(sl_m, file="m_step/m_s"//ss//"_step.bin", access="stream", status="replace")
     OPEN(sl_eb, file="eb_sweep/en_bulk_s"//ss//"_sweep.bin", access="stream", status="replace")
     OPEN(sl_mb, file="mb_sweep/m_bulk_s"//ss//"_sweep.bin", access="stream", status="replace")

     OPEN(sl_ee, file="ee_sweep/en_edge_s"//ss//"_sweep.bin", access="stream", status="replace")
     OPEN(sl_me, file="me_sweep/m_edge_s"//ss//"_sweep.bin", access="stream", status="replace")
     OPEN(sl_p, file="p_sweep/pump_s"//ss//"_sweep.bin", access="stream", status="replace")
     
     err = vslnewstream(str_p, VSL_BRNG_SFMT19937, 100 + 4 * (s - 1) + 0)
     err = vslnewstream(str_x, VSL_BRNG_SFMT19937, 100 + 4 * (s - 1) + 1)
     err = vslnewstream(str_y, VSL_BRNG_SFMT19937, 100 + 4 * (s - 1) + 2)
     err = vslnewstream(str_z, VSL_BRNG_SFMT19937, 100 + 4 * (s - 1) + 3)

     CALL calcEn_3d(IS3_ini0:l_x + 1, 0:l_y + 1, 0:l_z + 1), eb(0))
     mb(0) = SUM(IS3_ini1:l_x, 1:l_y, 1:l_z))
     IS3(0:l_x + 1, 0:l_y + 1, 0:l_z + 1) = IS3_ini0:l_x + 1, 0:l_y + 1, 0:l_z + 1)

     DO t = 1, l_t, 1
        DO i_v = 1, vel, 1
           CALL shift_3d(IS3(0:l_x + 1, 0:l_y + 1, 0:l_z + 1), pmp(i_v), eb(0))
           err = vdrnguniform(VSL_RNG_METHOD_UNIFORM_STD, str_p, n_st, r_p(1:n_st), 0.0d0, 1.0d0)
           err = virnguniform(VSL_RNG_METHOD_UNIFORM_STD, str_x, n_st, r_x(1:n_st), 1, l_x + 1)
           err = virnguniform(VSL_RNG_METHOD_UNIFORM_STD, str_y, n_st, r_y(1:n_st), 1, l_y + 1)
           err = virnguniform(VSL_RNG_METHOD_UNIFORM_STD, str_z, n_st, r_z(1:n_st), 1, l_z + 1)
           CALL mSSFs_3d(r_x(1:n_st), r_y(1:n_st), r_z(1:n_st), r_p(1:n_st), IS3(0:l_x + 1, 0:l_y + 1, 0:l_z + 1), eb(0:n_st), mb(0:n_st))
           ! WRITE(sl_en) eb(0:n_st)
           eb(0) = eb(n_st)
           ! WRITE(sl_m) mb(0:n_st)
           mb(0) = mb(n_st)
        END DO


        WRITE(st, '(i0.8)') t
        ! OPEN(sl_sp, file="sp_sweep/sp_s"//ss//"t"//st//"_sweep.bin", access="stream", status="replace")
        ! WRITE(sl_sp) INT1(IS3(1:l_x, 1:l_z))
        ! CLOSE(sl_sp)
        WRITE(sl_eb) eb(0)
        WRITE(sl_mb) mb(0)

        CALL calcEE_3d(IS3(0:l_x + 1, 0:l_y + 1, 0:l_z + 1), ee)
        WRITE(sl_ee) ee
        WRITE(sl_me) SUM(IS3(1:l_x, 1:l_y, l_z / 2:l_z / 2 + 1))
        WRITE(sl_p) SUM(pmp(1:vel))
     END DO

     err = vslsavestreamf(str_p, "str_p_s"//ss//"_sweep.bin")
     err = vslsavestreamf(str_x, "str_x_s"//ss//"_sweep.bin")
     err = vslsavestreamf(str_y, "str_y_s"//ss//"_sweep.bin")
     err = vslsavestreamf(str_z, "str_z_s"//ss//"_sweep.bin")

     err = vsldeletestream(str_p)
     err = vsldeletestream(str_x)
     err = vsldeletestream(str_y)
     err = vsldeletestream(str_z)
     ! CLOSE(sl_en)
     ! CLOSE(sl_m)
     
     CLOSE(sl_eb)
     CLOSE(sl_mb)

     CLOSE(sl_ee)
     CLOSE(sl_me)
     CLOSE(sl_p)
  END DO
  !$omp end parallel do
END PROGRAM main
