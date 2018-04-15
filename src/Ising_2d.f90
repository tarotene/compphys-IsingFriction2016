PROGRAM main
  !$  USE omp_lib
  USE mod_global
  USE mod_proc
  USE IFPORT, ONLY: access

  IMPLICIT NONE

  CALL inputParams_2d(l_x, l_z, beta, vel, l_t, id_IC, id_BC, n_s)
  n_st = l_x * l_z / vel
  CALL countSamples(n_s0)
  CALL metropolis_2d(beta, p_2d)

  ALLOCATE(r_x(1:n_st), r_z(1:n_st), r_p(1:n_st))
  ALLOCATE(eb(0:n_st), mb(0:n_st), IS2(0:l_x + 1, 0:l_z + 1), IS2_ini(0:l_x + 1, 0:l_z + 1))
  eb(0:n_st) = 0
  mb(0:n_st) = 0
  CALL initSp_2d(IS2_ini(0:l_x + 1, 0:l_z + 1))

  ALLOCATE(pmp(1:vel))

  !$omp parallel do schedule(static, 1) default(none) &
  !$omp shared(n_s0, n_s, l_t, l_x, l_z, vel, n_st) &
  !$omp private(s, sl_sp, sl_en, sl_eb, sl_m, sl_mb) &
  !$omp private(i_st, t, i_v, err, str_x, str_z, str_p, r_x, r_z, r_p, ss, st, sp) &
  !$omp firstprivate(eb, mb, sp_ini) &
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
    
     CALL constNewStream_SFMT19937(100 + 4 * (s - 1) + 0, str_p)
     CALL constNewStream_SFMT19937(100 + 4 * (s - 1) + 1, str_x)
     CALL constNewStream_SFMT19937(100 + 4 * (s - 1) + 3, str_z)

     CALL calcEn_2d(IS2_ini(0:l_x + 1, 0:l_z + 1), eb(0))
     mb(0) = SUM(IS2_ini(1:l_x, 1:l_z))
     IS2(0:l_x + 1, 0:l_z + 1) = IS2_ini(0:l_x + 1, 0:l_z + 1)

     DO t = 1, l_t, 1
        DO i_v = 1, vel, 1
           CALL shift_2d(IS2(0:l_x + 1, 0:l_z + 1), pmp(i_v), eb(0))
           CALL updateDRand_Uniform(str_p, n_st, 0.0d0, 1.0d0, r_p(1:n_st))
           CALL updateIRand_Uniform(str_x, n_st, 1, l_x + 1, r_x(1:n_st))
           CALL updateIRand_Uniform(str_z, n_st, 1, l_z + 1, r_z(1:n_st))
           ! WRITE(sl_en) eb(0:n_st)
           eb(0) = eb(n_st)
           ! WRITE(sl_m) mb(0:n_st)
           mb(0) = mb(n_st)
        END DO

        ! WRITE(st, '(i0.8)') t
        ! OPEN(sl_sp, file="sp_sweep/sp_s"//ss//"t"//st//"_sweep.bin", access="stream", status="replace")
        ! WRITE(sl_sp) INT1(IS2(1:l_x, 1:l_z))
        ! CLOSE(sl_sp)
        WRITE(sl_eb) eb(0)
        WRITE(sl_mb) mb(0)

        CALL calcEE_2d(IS2(0:l_x + 1, 0:l_z + 1), ee)
        WRITE(sl_ee) ee
        WRITE(sl_me) SUM(IS2(1:l_x, l_z / 2:l_z / 2 + 1))
        WRITE(sl_p) SUM(pmp(1:vel))
     END DO
     
     CALL saveRand(str_p, "str_p_s"//ss//".bin")
     CALL saveRand(str_x, "str_x_s"//ss//".bin")
     CALL saveRand(str_z, "str_z_s"//ss//".bin")
    
     CALL destRnd(str_p)
     CALL destRnd(str_x)
     CALL destRnd(str_z)
     ! CLOSE(sl_en)
     ! CLOSE(sl_m)
     
     CLOSE(sl_eb)
     CLOSE(sl_mb)

     CLOSE(sl_ee)
     CLOSE(sl_me)
     CLOSE(sl_p)

     OPEN(sl_sp, file="sp_fin_s"//ss//".bin", access="stream", status="replace")
     WRITE(sl_sp) INT1(IS2(1:l_x, 1:l_z))
     CLOSE(sl_sp)
  END DO
  !$omp end parallel do
END PROGRAM main
