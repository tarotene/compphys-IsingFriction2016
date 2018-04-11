PROGRAM main
  USE mod_global
  USE MKL_VSL_TYPE
  USE MKL_VSL
  USE mod_proc
  IMPLICIT NONE

  INTEGER(4) :: sl_eb, sl_ee, sl_mb, sl_me, sl_p

  INTEGER(4), ALLOCATABLE :: ex_eb(:, :), ex_ee(:, :), ex_mb(:, :), ex_me(:, :), ex_p(:, :)

  REAL(8), ALLOCATABLE :: eb(:, :), ee(:, :), mb(:, :), me(:, :), p(:, :)
  REAL(8), ALLOCATABLE :: b_eb(:, :), b_ee(:, :), b_mb(:, :), b_me(:, :), b_p(:, :)
  REAL(8), ALLOCATABLE :: sq_b_eb(:, :), sq_b_ee(:, :), sq_b_mb(:, :), sq_b_me(:, :)
  REAL(8), ALLOCATABLE :: fp_b_mb(:, :), fp_b_me(:, :)

  REAL(8), ALLOCATABLE :: avg_eb(:), avg_ee(:), avg_mb(:), avg_me(:), avg_p(:)
  REAL(8), ALLOCATABLE :: var_eb(:), var_ee(:), var_mb(:), var_me(:), var_p(:)
  REAL(8), ALLOCATABLE :: err_eb(:), err_ee(:), err_mb(:), err_me(:), err_p(:)

  REAL(8), ALLOCATABLE :: cb(:), ce(:), chib(:), chie(:), ub(:), ue(:)
  REAL(8), ALLOCATABLE :: err_cb(:), err_ce(:), err_chib(:), err_chie(:), err_ub(:), err_ue(:)
  
  INTEGER(4) :: t, s, loc, t_
  CHARACTER(4) :: ss

  CALL paramsCalc_2d(l_x, l_z, beta, vel, l_t, n_s, l_th, n_b)

  ALLOCATE(ex_eb(1:l_t, 1:n_s), ex_ee(1:l_t, 1:n_s), ex_mb(1:l_t, 1:n_s), ex_me(1:l_t, 1:n_s), ex_p(1:l_t, 1:n_s))

  ALLOCATE(eb(1:l_t, 1:n_s), ee(1:l_t, 1:n_s), mb(1:l_t, 1:n_s), me(1:l_t, 1:n_s), p(1:l_t, 1:n_s))
  ALLOCATE(b_eb(1:n_b, 1:n_s), b_ee(1:n_b, 1:n_s), b_mb(1:n_b, 1:n_s), b_me(1:n_b, 1:n_s), b_p(1:n_b, 1:n_s))
  ALLOCATE(sq_b_eb(1:n_b, 1:n_s), sq_b_ee(1:n_b, 1:n_s), sq_b_mb(1:n_b, 1:n_s), sq_b_me(1:n_b, 1:n_s))
  ALLOCATE(fp_b_mb(1:n_b, 1:n_s), fp_b_me(1:n_b, 1:n_s))

  ALLOCATE(avg_eb(1:n_s), avg_ee(1:n_s), avg_mb(1:n_s), avg_me(1:n_s), avg_p(1:n_s))
  ALLOCATE(var_eb(1:n_s), var_ee(1:n_s), var_mb(1:n_s), var_me(1:n_s), var_p(1:n_s))
  ALLOCATE(err_eb(1:n_s), err_ee(1:n_s), err_mb(1:n_s), err_me(1:n_s), err_p(1:n_s))

  ALLOCATE(cb(1:n_s), ce(1:n_s), chib(1:n_s), chie(1:n_s), ub(1:n_s), ue(1:n_s))
  ALLOCATE(err_cb(1:n_s), err_ce(1:n_s), err_chib(1:n_s), err_chie(1:n_s), err_ub(1:n_s), err_ue(1:n_s))

  !$omp parallel do schedule(static, 1) default(none) &
  !$omp shared(n_s, l_t, ex_eb, ex_mb, ex_ee, ex_me, ex_p) &
  !$omp private(sl_eb, sl_mb, sl_ee, sl_me, sl_p, ss, loc, t)
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
  !$omp end parallel do

  eb(1:l_t, 1:n_s) = DBLE(ex_eb(1:l_t, 1:n_s)) / DBLE(l_x * l_z) 
  ee(1:l_t, 1:n_s) = DBLE(ex_ee(1:l_t, 1:n_s)) / DBLE(l_x)
  mb(1:l_t, 1:n_s) = DBLE(ABS(ex_mb(1:l_t, 1:n_s))) / DBLE(l_x * l_z)
  me(1:l_t, 1:n_s) = DBLE(ABS(ex_me(1:l_t, 1:n_s))) / DBLE(2 * l_x)
  p(1:l_t, 1:n_s) = DBLE(ex_p(1:l_t, 1:n_s)) / DBLE(l_x)
  DEALLOCATE(ex_eb, ex_mb, ex_ee, ex_me, ex_p)

  !$omp parallel do schedule(static, 1) default(none) &
  !$omp shared(n_s, l_th, l_t, n_b) &
  !$omp shared(eb, ee, mb, me, p) &
  !$omp shared(b_eb, b_ee, b_mb, b_me, b_p)
  DO s = 1, n_s, 1
    CALL convertStreamSamples(l_th, n_b, eb(1:l_t, s), b_eb(1:n_b, s))
    CALL convertStreamSamples(l_th, n_b, ee(1:l_t, s), b_ee(1:n_b, s))
    CALL convertStreamSamples(l_th, n_b, mb(1:l_t, s), b_mb(1:n_b, s))
    CALL convertStreamSamples(l_th, n_b, me(1:l_t, s), b_me(1:n_b, s))
    CALL convertStreamSamples(l_th, n_b, p(1:l_t, s), b_p(1:n_b, s))
  END DO
  !$omp end parallel do
  DEALLOCATE(eb, mb, ee, me, p)

  !$omp parallel do schedule(static, 1) default(none) &
  !$omp shared(n_s, n_b) &
  !$omp shared(b_eb, b_ee, b_mb, b_me, b_p) &
  !$omp shared(avg_eb, avg_ee, avg_mb, avg_me, avg_p) &
  !$omp shared(var_eb, var_ee, var_mb, var_me, var_p) &
  !$omp shared(err_eb, err_ee, err_mb, err_me, err_p)
  DO s = 1, n_s, 1
    CALL calcAvgVar(n_b, b_eb(1:n_b, s), avg_eb(s), var_eb(s))
    CALL calcAvgVar(n_b, b_ee(1:n_b, s), avg_ee(s), var_ee(s))
    CALL calcAvgVar(n_b, b_mb(1:n_b, s), avg_mb(s), var_mb(s))
    CALL calcAvgVar(n_b, b_me(1:n_b, s), avg_me(s), var_me(s))
    CALL calcAvgVar(n_b, b_p(1:n_b, s), avg_p(s), var_p(s))

    err_eb(s) = SQRT(var_eb(s)) / SQRT(DBLE(n_b))
    err_ee(s) = SQRT(var_ee(s)) / SQRT(DBLE(n_b))
    err_mb(s) = SQRT(var_mb(s)) / SQRT(DBLE(n_b))
    err_me(s) = SQRT(var_me(s)) / SQRT(DBLE(n_b))
    err_p(s) = SQRT(var_p(s)) / SQRT(DBLE(n_b))
  END DO
  !$omp end parallel do
  
  OPEN(10, file="physquan1.dat", status="replace")
  DO s = 1, n_s, 1
     WRITE(10, '(i4, a, f0.4, a, i4, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8)') &
     l_z, ", ", beta, ", ", vel, ", ", avg_eb(s), ", ", err_eb(s), ", ", avg_ee(s), ", ", err_ee(s), ", ", &
     avg_mb(s), ", ", err_mb(s), ", ", avg_me(s), ", ", err_me(s), ", ", &
     avg_p(s), ", ", err_p(s)
  END DO
  CLOSE(10)

  sq_b_eb(1:n_b, 1:n_s) = b_eb(1:n_b, 1:n_s) ** 2
  sq_b_ee(1:n_b, 1:n_s) = b_ee(1:n_b, 1:n_s) ** 2
  sq_b_mb(1:n_b, 1:n_s) = b_mb(1:n_b, 1:n_s) ** 2
  sq_b_me(1:n_b, 1:n_s) = b_me(1:n_b, 1:n_s) ** 2
  fp_b_mb(1:n_b, 1:n_s) = sq_b_mb(1:n_b, 1:n_s) ** 2
  fp_b_me(1:n_b, 1:n_s) = sq_b_me(1:n_b, 1:n_s) ** 2

  !$omp parallel do schedule(static, 1) default(none) &
  !$omp shared(n_s, n_b, beta, l_x, l_z) &
  !$omp shared(cb, ce, chib, chie, ub, ue) &
  !$omp shared(err_cb, err_ce, err_chib, err_chie, err_ub, err_ue) &
  !$omp shared(sq_b_eb, sq_b_ee, sq_b_mb, sq_b_me, fp_b_mb, fp_b_me) &
  !$omp shared(err_eb, err_ee, err_mb, err_me, err_p) &
  !$omp shared(avg_eb, avg_ee, avg_mb, avg_me)
  DO s = 1, n_s, 1
    cb(s) = (beta ** 2) * (SUM(sq_b_eb(1:n_b, s)) / DBLE(n_b) - avg_eb(s) ** 2) * DBLE(l_x * l_z)
    ce(s) = (beta ** 2) * (SUM(sq_b_ee(1:n_b, s)) / DBLE(n_b) - avg_ee(s) ** 2) * DBLE(l_x)
    chib(s) = beta * (SUM(sq_b_mb(1:n_b, s)) / DBLE(n_b) - avg_mb(s) ** 2) * DBLE(l_x * l_z)
    chie(s) = beta * (SUM(sq_b_me(1:n_b, s)) / DBLE(n_b) - avg_me(s) ** 2) * DBLE(2 * l_x)
    ub(s) = 1 - (SUM(fp_b_mb(1:n_b, s)) / DBLE(n_b)) / (3.0d0 * (SUM(sq_b_mb(1:n_b, s)) / DBLE(n_b)) ** 2)
    ue(s) = 1 - (SUM(fp_b_me(1:n_b, s)) / DBLE(n_b)) / (3.0d0 * (SUM(sq_b_me(1:n_b, s)) / DBLE(n_b)) ** 2)
    
    !  TODO: 誤差の公式の確認
    err_cb(s)    = (beta ** 2) * (4 * err_eb(s) + err_eb(s) ** 2) * DBLE(l_x * l_z)
    err_ce(s)    = (beta ** 2) * (4 * err_ee(s) + err_eb(s) ** 2) * DBLE(l_x)
    err_chib(s)  = - beta * (4 * err_mb(s) + err_mb(s) ** 2) * DBLE(l_x * l_z)
    err_chie(s)  = - beta * (4 * err_me(s) + err_me(s) ** 2) * DBLE(2 * l_x)
    err_ub(s)    = 4 * (err_mb(s) / avg_mb(s)) ** 2
    err_ub(s)    = 4 * (err_me(s) / avg_me(s)) ** 2
  END DO
  !$omp end parallel do

  OPEN(20, file="physquan2.dat", status="replace")
  DO s = 1, n_s, 1
     WRITE(20, '(i4, a, f0.4, a, i4, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8)') &
     l_z, ", ", beta, ", ", vel, ", ", cb(s), ", ", err_cb(s), ", ", ce(s), ", ", err_ce(s), ", ", &
     chib(s), ", ", err_chib(s), ", ", chie(s), ", ", err_chie(s), ", ", &
     ub(s), ", ", err_ub(s), ", ", ue(s), ", ", err_ue(s)
  END DO
  CLOSE(20)

END PROGRAM main
