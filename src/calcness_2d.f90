PROGRAM main
  USE mod_global
  USE MKL_VSL_TYPE
  USE MKL_VSL
  USE mod_proc
  IMPLICIT NONE

  INTEGER(4) :: sl_eb, sl_ee, sl_mb, sl_me, sl_p

  INTEGER(4), ALLOCATABLE :: eb(:, :), ee(:, :), mb(:, :), me(:, :), p(:, :)
  REAL(8), ALLOCATABLE :: avg_eb(:), avg_ee(:), avg_amb(:), avg_ame(:), avg_p(:)
  REAL(8), ALLOCATABLE :: err_eb(:), err_ee(:), err_amb(:), err_ame(:), err_p(:)

  REAL(8), ALLOCATABLE :: int_eb(:), int_ee(:), int_amb(:), int_ame(:), int_p(:)
  REAL(8), ALLOCATABLE :: err_int_eb(:), err_int_ee(:), err_int_amb(:), err_int_ame(:), err_int_p(:)

  INTEGER(4), ALLOCATABLE :: eb_sq(:, :), ee_sq(:, :), amb_sq(:, :), ame_sq(:, :), amb_fp(:, :), ame_fp(:, :)
  REAL(8), ALLOCATABLE :: avg_eb_sq(:), avg_ee_sq(:), avg_amb_sq(:), avg_ame_sq(:), avg_amb_fp(:), avg_ame_fp(:)
  REAL(8), ALLOCATABLE :: err_eb_sq(:), err_ee_sq(:), err_amb_sq(:), err_ame_sq(:), err_amb_fp(:), err_ame_fp(:)

  REAL(8), ALLOCATABLE :: cb(:), ce(:), chib(:), chie(:), ub(:), ue(:)
  REAL(8), ALLOCATABLE :: err_cb(:), err_ce(:), err_chib(:), err_chie(:), err_ub(:), err_ue(:)

  REAL(8), ALLOCATABLE :: mat_obs(:, :, :), avg(:), err(:)
  
  INTEGER(4) :: t, s, loc, t_
  CHARACTER(4) :: ss

  CALL paramsCalc_2d(l_x, l_z, beta, vel, l_t, n_s, l_th, l_b)

  ALLOCATE(eb(1:l_t, 1:n_s), ee(1:l_t, 1:n_s), mb(1:l_t, 1:n_s), me(1:l_t, 1:n_s), p(1:l_t, 1:n_s))
  ALLOCATE(avg_eb(1:n_s), avg_ee(1:n_s), avg_amb(1:n_s), avg_ame(1:n_s), avg_p(1:n_s))
  ALLOCATE(err_eb(1:n_s), err_ee(1:n_s), err_amb(1:n_s), err_ame(1:n_s), err_p(1:n_s))

  ALLOCATE(int_eb(1:n_s), int_ee(1:n_s), int_amb(1:n_s), int_ame(1:n_s), int_p(1:n_s))
  ALLOCATE(err_int_eb(1:n_s), err_int_ee(1:n_s), err_int_amb(1:n_s), err_int_ame(1:n_s), err_int_p(1:n_s))

  ALLOCATE(eb_sq(1:l_t, 1:n_s), ee_sq(1:l_t, 1:n_s), amb_sq(1:l_t, 1:n_s), ame_sq(1:l_t, 1:n_s), amb_fp(1:l_t, 1:n_s), ame_fp(1:l_t, 1:n_s))
  ALLOCATE(avg_eb_sq(1:n_s), avg_ee_sq(1:n_s), avg_amb_sq(1:n_s), avg_ame_sq(1:n_s), avg_amb_fp(1:n_s), avg_ame_fp(1:n_s))
  ALLOCATE(err_eb_sq(1:n_s), err_ee_sq(1:n_s), err_amb_sq(1:n_s), err_ame_sq(1:n_s), err_amb_fp(1:n_s), err_ame_fp(1:n_s))

  ALLOCATE(cb(1:n_s), ce(1:n_s), chib(1:n_s), chie(1:n_s), ub(1:n_s), ue(1:n_s))
  ALLOCATE(err_cb(1:n_s), err_ce(1:n_s), err_chib(1:n_s), err_chie(1:n_s), err_ub(1:n_s), err_ue(1:n_s))

  DO s = 1, n_s, 1
     sl_eb = 20 + s + 2 * n_s
     sl_mb = 20 + s + 3 * n_s
     sl_ee = 20 + s + 4 * n_s
     sl_me = 20 + s + 5 * n_s
     sl_p  = 20 + s + 6 * n_s

     WRITE(ss, '(i0.4)') s
     OPEN(sl_eb, file="eb_sweep/en_bulk_s"//ss//"_sweep.bin", access="stream", status="old", buffered="YES")
     OPEN(sl_ee, file="ee_sweep/en_edge_s"//ss//"_sweep.bin", access="stream", status="old", buffered="YES")
     OPEN(sl_mb, file="mb_sweep/m_bulk_s"//ss//"_sweep.bin",  access="stream", status="old", buffered="YES")
     OPEN(sl_me, file="me_sweep/m_edge_s"//ss//"_sweep.bin",  access="stream", status="old", buffered="YES")
     OPEN(sl_p,  file="p_sweep/pump_s"//ss//"_sweep.bin",        access="stream", status="old", buffered="YES")

     loc = 1
     DO t = 1, l_t, 1
        READ(sl_eb, pos=loc) eb(t, s)
        READ(sl_mb, pos=loc) mb(t, s)
        READ(sl_ee, pos=loc) ee(t, s)
        READ(sl_me, pos=loc) me(t, s)
        loc = loc + 4
     END DO

     CLOSE(sl_eb); CLOSE(sl_mb); CLOSE(sl_ee); CLOSE(sl_me); CLOSE(sl_p)
  END DO

  eb_sq(1:l_t, 1:n_s)  = eb(1:l_t, 1:n_s) ** 2
  ee_sq(1:l_t, 1:n_s)  = ee(1:l_t, 1:n_s) ** 2
  amb_sq(1:l_t, 1:n_s) = ABS(mb(1:l_t, 1:n_s)) ** 2
  ame_sq(1:l_t, 1:n_s) = ABS(me(1:l_t, 1:n_s)) ** 2
  amb_fp(1:l_t, 1:n_s) = amb_sq(1:l_t, 1:n_s) ** 2
  ame_fp(1:l_t, 1:n_s) = ame_sq(1:l_t, 1:n_s) ** 2

  DEALLOCATE(eb, mb, ee, me)

  ALLOCATE(mat_obs(1:l_t, 1:n_s, 1:11), avg(1:11), err(1:11))
  mat_obs(1:l_t, 1:n_s, 1)  = DBLE(eb(1:l_t, 1:n_s))
  mat_obs(1:l_t, 1:n_s, 2)  = DBLE(ee(1:l_t, 1:n_s))
  mat_obs(1:l_t, 1:n_s, 3)  = DBLE(eb_sq(1:l_t, 1:n_s))
  mat_obs(1:l_t, 1:n_s, 4)  = DBLE(ee_sq(1:l_t, 1:n_s))
  mat_obs(1:l_t, 1:n_s, 5)  = DBLE(ABS(mb(1:l_t, 1:n_s)))
  mat_obs(1:l_t, 1:n_s, 6)  = DBLE(ABS(me(1:l_t, 1:n_s)))
  mat_obs(1:l_t, 1:n_s, 7)  = DBLE(amb_sq(1:l_t, 1:n_s))
  mat_obs(1:l_t, 1:n_s, 8)  = DBLE(ame_sq(1:l_t, 1:n_s))
  mat_obs(1:l_t, 1:n_s, 9)  = DBLE(amb_fp(1:l_t, 1:n_s))
  mat_obs(1:l_t, 1:n_s, 10) = DBLE(ame_fp(1:l_t, 1:n_s))
  mat_obs(1:l_t, 1:n_s, 11) = DBLE(p(1:l_t, 1:n_s))
  DO CONCURRENT (s = 1:n_s:1)
     CALL calcStatsStream(l_th, l_b, 11, mat_obs(1:l_t, s, 1:11), avg(1:11), err(1:11))
     avg_eb(s)      = avg(1)  ; err_eb(s)     = err(1)
     avg_ee(s)      = avg(2)  ; err_ee(s)     = err(2)
     avg_eb_sq(s)   = avg(3)  ; err_eb_sq(s)  = err(3)
     avg_ee_sq(s)   = avg(4)  ; err_ee_sq(s)  = err(4)
     avg_amb(s)     = avg(5)  ; err_amb(s)    = err(5)
     avg_ame(s)     = avg(6)  ; err_ame(s)    = err(6)
     avg_amb_sq(s)  = avg(7)  ; err_amb_sq(s) = err(7)
     avg_ame_sq(s)  = avg(8)  ; err_ame_sq(s) = err(8)
     avg_amb_fp(s)  = avg(9)  ; err_amb_fp(s) = err(9)
     avg_ame_fp(s)  = avg(10) ; err_ame_fp(s) = err(10)
     avg_p(s)       = avg(11) ; err_p(s)      = err(11)
  END DO
  DEALLOCATE(mat_obs, avg, err)

  DO CONCURRENT (s = 1:n_s:1)
     int_eb(s)  = avg_eb(s) / DBLE(l_x * l_z)
     int_ee(s)  = avg_ee(s) / DBLE(l_x)
     int_amb(s) = avg_amb(s) / DBLE(l_x * l_z)
     int_ame(s) = avg_ame(s) / DBLE(2 * l_x)
     int_p(s)   = avg_p(s) / DBLE(l_x)

     err_int_eb(s)  = err_eb(s) / DBLE(l_x * l_z)
     err_int_ee(s)  = err_ee(s) / DBLE(l_x)
     err_int_amb(s) = err_amb(s) / DBLE(l_x * l_z)
     err_int_ame(s) = err_ame(s) / DBLE(2 * l_x)
     err_int_p(s)   = err_p(s) / DBLE(l_x)
  END DO

  OPEN(10, file="physquan1.dat", status="replace")
  DO s = 1, n_s, 1
     WRITE(10, '(f0.4, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8)') &
     beta, ", ", int_eb(s), ", ", err_int_eb(s), ", ", int_ee(s), ", ", err_int_ee(s), ", ", &
     int_amb(s), ", ", err_int_amb(s), ", ", int_ame(s), ", ", err_int_ame(s), ", ", &
     int_p(s), ", ", err_int_p(s)
  END DO
  CLOSE(10)

  DO CONCURRENT (s = 1:n_s:1)
     cb(s)    = (beta ** 2) * (avg_eb_sq(s) - avg_eb(s) ** 2) / DBLE(l_x * l_z)
     ce(s)    = (beta ** 2) * (avg_eb_sq(s) - avg_eb(s) ** 2) / DBLE(l_x)
     chib(s)  = - beta * (avg_amb_sq(s) - avg_amb(s) ** 2) / DBLE(l_x * l_z)
     chie(s)  = - beta * (avg_ame_sq(s) - avg_ame(s) ** 2) / DBLE(2 * l_x)
     ub(s)    = 1 - avg_amb_fp(s) / (3.0d0 * avg_amb_sq(s) ** 2)
     ub(s)    = 1 - avg_ame_fp(s) / (3.0d0 * avg_ame_sq(s) ** 2)
     
    !  TODO: 誤差の公式の確認
     err_cb(s)    = (beta ** 2) * (4 * err_eb(s) + err_eb(s) ** 2) / DBLE(l_x * l_z)
     err_ce(s)    = (beta ** 2) * (4 * err_ee(s) + err_eb(s) ** 2) / DBLE(l_x)
     err_chib(s)  = - beta * (4 * err_amb(s) + err_amb(s) ** 2) / DBLE(l_x * l_z)
     err_chie(s)  = - beta * (4 * err_ame(s) + err_ame(s) ** 2) / DBLE(2 * l_x)
     err_ub(s)    = 4 * (err_amb(s) / avg_amb(s)) ** 2
     err_ub(s)    = 4 * (err_ame(s) / avg_ame(s)) ** 2
  END DO

  OPEN(20, file="physquan2.dat", status="replace")
  DO s = 1, n_s, 1
     WRITE(20, '(f0.4, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8)') &
     beta, ", ", cb(s), ", ", err_cb(s), ", ", ce(s), ", ", err_ce(s), ", ", &
     chib(s), ", ", err_chib(s), ", ", chie(s), ", ", err_chie(s), ", ", &
     ub(s), ", ", err_ub(s), ", ", ue(s), ", ", err_ue(s)
  END DO
  CLOSE(20)

END PROGRAM main
