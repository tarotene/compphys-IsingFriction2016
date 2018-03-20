PROGRAM main
  USE mod_global
  USE MKL_VSL_TYPE
  USE MKL_VSL
  USE mod_proc
  IMPLICIT NONE

  INTEGER(4) :: sl_eb, sl_ee, sl_mb, sl_me, sl_p

  INTEGER(4), ALLOCATABLE :: ex_eb(:, :), ex_ee(:, :), ex_mb(:, :), ex_me(:, :), ex_p(:, :)

  REAL(8), ALLOCATABLE :: eb(:, :), ee(:, :), mb(:, :), me(:, :), p(:, :)
  REAL(8), ALLOCATABLE :: avg_eb(:), avg_ee(:), avg_mb(:), avg_me(:), avg_p(:)
  REAL(8), ALLOCATABLE :: err_eb(:), err_ee(:), err_mb(:), err_me(:), err_p(:)

  REAL(8), ALLOCATABLE :: eb_sq(:, :), ee_sq(:, :), mb_sq(:, :), me_sq(:, :), mb_fp(:, :), me_fp(:, :)
  REAL(8), ALLOCATABLE :: avg_eb_sq(:), avg_ee_sq(:), avg_mb_sq(:), avg_me_sq(:), avg_mb_fp(:), avg_me_fp(:)
  REAL(8), ALLOCATABLE :: err_eb_sq(:), err_ee_sq(:), err_mb_sq(:), err_me_sq(:), err_mb_fp(:), err_me_fp(:)

  REAL(8), ALLOCATABLE :: cb(:), ce(:), chib(:), chie(:), ub(:), ue(:)
  REAL(8), ALLOCATABLE :: err_cb(:), err_ce(:), err_chib(:), err_chie(:), err_ub(:), err_ue(:)

  REAL(8), ALLOCATABLE :: mat_obs(:, :, :), avg(:), err(:)
  
  INTEGER(4) :: t, s, loc, t_
  CHARACTER(4) :: ss

  CALL paramsCalc_2d(l_x, l_z, beta, vel, l_t, n_s, l_th, l_b)

  ALLOCATE(ex_eb(1:l_t, 1:n_s), ex_ee(1:l_t, 1:n_s), ex_mb(1:l_t, 1:n_s), ex_me(1:l_t, 1:n_s), ex_p(1:l_t, 1:n_s))

  ALLOCATE(eb(1:l_t, 1:n_s), ee(1:l_t, 1:n_s), mb(1:l_t, 1:n_s), me(1:l_t, 1:n_s), p(1:l_t, 1:n_s))
  ALLOCATE(avg_eb(1:n_s), avg_ee(1:n_s), avg_mb(1:n_s), avg_me(1:n_s), avg_p(1:n_s))
  ALLOCATE(err_eb(1:n_s), err_ee(1:n_s), err_mb(1:n_s), err_me(1:n_s), err_p(1:n_s))

  ALLOCATE(eb_sq(1:l_t, 1:n_s), ee_sq(1:l_t, 1:n_s), mb_sq(1:l_t, 1:n_s), me_sq(1:l_t, 1:n_s), mb_fp(1:l_t, 1:n_s), me_fp(1:l_t, 1:n_s))
  ALLOCATE(avg_eb_sq(1:n_s), avg_ee_sq(1:n_s), avg_mb_sq(1:n_s), avg_me_sq(1:n_s), avg_mb_fp(1:n_s), avg_me_fp(1:n_s))
  ALLOCATE(err_eb_sq(1:n_s), err_ee_sq(1:n_s), err_mb_sq(1:n_s), err_me_sq(1:n_s), err_mb_fp(1:n_s), err_me_fp(1:n_s))

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
     OPEN(sl_p,  file="p_sweep/pump_s"//ss//"_sweep.bin",     access="stream", status="old", buffered="YES")

     loc = 1
     DO t = 1, l_t, 1
        READ(sl_eb, pos = loc) ex_eb(t, s)
        READ(sl_mb, pos = loc) ex_mb(t, s)
        READ(sl_ee, pos = loc) ex_ee(t, s)
        READ(sl_me, pos = loc) ex_me(t, s)
        READ(sl_p,  pos = loc) ex_p(t, s)
        loc = loc + 4
     END DO

     CLOSE(sl_eb); CLOSE(sl_mb); CLOSE(sl_ee); CLOSE(sl_me); CLOSE(sl_p)
  END DO

  eb(1:l_t, 1:n_s) = DBLE(ex_eb(1:l_t, 1:n_s)) / DBLE(l_x * l_z) 
  ee(1:l_t, 1:n_s) = DBLE(ex_ee(1:l_t, 1:n_s)) / DBLE(l_x)
  mb(1:l_t, 1:n_s) = DBLE(ABS(ex_mb(1:l_t, 1:n_s))) / DBLE(l_x * l_z)
  me(1:l_t, 1:n_s) = DBLE(ABS(ex_me(1:l_t, 1:n_s))) / DBLE(2 * l_x)
  p(1:l_t, 1:n_s)  = DBLE(ex_p(1:l_t, 1:n_s)) / DBLE(l_x)

  DEALLOCATE(ex_eb, ex_mb, ex_ee, ex_me, ex_p)

  eb_sq(1:l_t, 1:n_s) = eb(1:l_t, 1:n_s) ** 2
  ee_sq(1:l_t, 1:n_s) = ee(1:l_t, 1:n_s) ** 2
  mb_sq(1:l_t, 1:n_s) = mb(1:l_t, 1:n_s) ** 2
  me_sq(1:l_t, 1:n_s) = me(1:l_t, 1:n_s) ** 2
  mb_fp(1:l_t, 1:n_s) = mb_sq(1:l_t, 1:n_s) ** 2
  me_fp(1:l_t, 1:n_s) = me_sq(1:l_t, 1:n_s) ** 2

  ALLOCATE(mat_obs(1:l_t, 1:n_s, 1:11), avg(1:11), err(1:11))
  mat_obs(1:l_t, 1:n_s, 1)  = eb(1:l_t, 1:n_s)
  mat_obs(1:l_t, 1:n_s, 2)  = ee(1:l_t, 1:n_s)
  mat_obs(1:l_t, 1:n_s, 3)  = eb_sq(1:l_t, 1:n_s)
  mat_obs(1:l_t, 1:n_s, 4)  = ee_sq(1:l_t, 1:n_s)
  mat_obs(1:l_t, 1:n_s, 5)  = mb(1:l_t, 1:n_s)
  mat_obs(1:l_t, 1:n_s, 6)  = me(1:l_t, 1:n_s)
  mat_obs(1:l_t, 1:n_s, 7)  = mb_sq(1:l_t, 1:n_s)
  mat_obs(1:l_t, 1:n_s, 8)  = me_sq(1:l_t, 1:n_s)
  mat_obs(1:l_t, 1:n_s, 9)  = mb_fp(1:l_t, 1:n_s)
  mat_obs(1:l_t, 1:n_s, 10) = me_fp(1:l_t, 1:n_s)
  mat_obs(1:l_t, 1:n_s, 11) = p(1:l_t, 1:n_s)
  DO CONCURRENT (s = 1:n_s:1)
     CALL calcStatsStream(l_th, l_b, 11, mat_obs(1:l_t, s, 1:11), avg(1:11), err(1:11))
     avg_eb(s)    = avg(1)  ; err_eb(s)    = err(1)
     avg_ee(s)    = avg(2)  ; err_ee(s)    = err(2)
     avg_eb_sq(s) = avg(3)  ; err_eb_sq(s) = err(3)
     avg_ee_sq(s) = avg(4)  ; err_ee_sq(s) = err(4)
     avg_mb(s)    = avg(5)  ; err_mb(s)    = err(5)
     avg_me(s)    = avg(6)  ; err_me(s)    = err(6)
     avg_mb_sq(s) = avg(7)  ; err_mb_sq(s) = err(7)
     avg_me_sq(s) = avg(8)  ; err_me_sq(s) = err(8)
     avg_mb_fp(s) = avg(9)  ; err_mb_fp(s) = err(9)
     avg_me_fp(s) = avg(10) ; err_me_fp(s) = err(10)
     avg_p(s)     = avg(11) ; err_p(s)     = err(11)
  END DO
  DEALLOCATE(mat_obs, avg, err)

  OPEN(10, file="physquan1.dat", status="replace")
  DO s = 1, n_s, 1
     WRITE(10, '(f0.4, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8)') &
     beta, ", ", avg_eb(s), ", ", err_eb(s), ", ", avg_ee(s), ", ", err_ee(s), ", ", &
     avg_mb(s), ", ", err_mb(s), ", ", avg_me(s), ", ", err_me(s), ", ", &
     avg_p(s), ", ", err_p(s)
  END DO
  CLOSE(10)

  DO CONCURRENT (s = 1:n_s:1)
     cb(s)    = (beta ** 2) * (avg_eb_sq(s) - avg_eb(s) ** 2) * DBLE(l_x * l_z)
     ce(s)    = (beta ** 2) * (avg_eb_sq(s) - avg_eb(s) ** 2) * DBLE(l_x)
     chib(s)  = - beta * (avg_mb_sq(s) - avg_mb(s) ** 2) * DBLE(l_x * l_z)
     chie(s)  = - beta * (avg_me_sq(s) - avg_me(s) ** 2) * DBLE(2 * l_x)
     ub(s)    = 1 - avg_mb_fp(s) / (3.0d0 * avg_mb_sq(s) ** 2)
     ub(s)    = 1 - avg_me_fp(s) / (3.0d0 * avg_me_sq(s) ** 2)
     
    !  TODO: 誤差の公式の確認
     err_cb(s)    = (beta ** 2) * (4 * err_eb(s) + err_eb(s) ** 2) * DBLE(l_x * l_z)
     err_ce(s)    = (beta ** 2) * (4 * err_ee(s) + err_eb(s) ** 2) * DBLE(l_x)
     err_chib(s)  = - beta * (4 * err_mb(s) + err_mb(s) ** 2) * DBLE(l_x * l_z)
     err_chie(s)  = - beta * (4 * err_me(s) + err_me(s) ** 2) * DBLE(2 * l_x)
     err_ub(s)    = 4 * (err_mb(s) / avg_mb(s)) ** 2
     err_ub(s)    = 4 * (err_me(s) / avg_me(s)) ** 2
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
