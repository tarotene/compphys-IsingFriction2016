PROGRAM main
  USE mod_global
  USE MKL_VSL_TYPE
  USE MKL_VSL
  USE mod_proc
  IMPLICIT NONE

  INTEGER(4) :: sl_eb, sl_mb

  INTEGER(4), ALLOCATABLE :: ex_eb(:, :), ex_mb(:, :)

  REAL(8), ALLOCATABLE :: eb(:, :), mb(:, :)
  REAL(8), ALLOCATABLE :: b_eb(:, :), b_mb(:, :)
  REAL(8), ALLOCATABLE :: sq_b_eb(:, :), sq_b_mb(:, :), fp_b_mb(:, :)

  REAL(8), ALLOCATABLE :: avg_eb(:), avg_mb(:), var_eb(:), var_mb(:), err_eb(:), err_mb(:)

  REAL(8), ALLOCATABLE :: cb(:), chib(:), ub(:), err_cb(:), err_chib(:), err_ub(:)
  
  INTEGER(4) :: t, s, loc, t_
  CHARACTER(4) :: ss

  CALL paramsCalc_3d_eq(l_x, l_y, l_z, beta, l_t, n_s, l_th, n_b)

  ALLOCATE(ex_eb(1:l_t, 1:n_s), ex_mb(1:l_t, 1:n_s))

  ALLOCATE(eb(1:l_t, 1:n_s), mb(1:l_t, 1:n_s))
  ALLOCATE(b_eb(1:n_b, 1:n_s), b_mb(1:n_b, 1:n_s))
  ALLOCATE(sq_b_eb(1:n_b, 1:n_s), sq_b_mb(1:n_b, 1:n_s), fp_b_mb(1:n_b, 1:n_s))

  ALLOCATE(avg_eb(1:n_s), avg_mb(1:n_s), var_eb(1:n_s), var_mb(1:n_s), err_eb(1:n_s), err_mb(1:n_s))

  ALLOCATE(cb(1:n_s), chib(1:n_s), ub(1:n_s))
  ALLOCATE(err_cb(1:n_s), err_chib(1:n_s), err_ub(1:n_s))

  !$omp parallel do schedule(static, 1) default(none) &
  !$omp shared(n_s, l_t, ex_eb, ex_mb) &
  !$omp private(sl_eb, sl_mb, ss, loc, t)
  DO s = 1, n_s, 1
     sl_eb = 20 + s + 2 * n_s
     sl_mb = 20 + s + 3 * n_s

     WRITE(ss, '(i0.4)') s
     OPEN(sl_eb, file="eb_sweep/en_bulk_s"//ss//"_sweep.bin", access="stream", status="old", buffered="YES")
     OPEN(sl_mb, file="mb_sweep/m_bulk_s"//ss//"_sweep.bin",  access="stream", status="old", buffered="YES")

     loc = 1
     DO t = 1, l_t, 1
        READ(sl_eb, pos = loc) ex_eb(t, s)
        READ(sl_mb, pos = loc) ex_mb(t, s)
        loc = loc + 4
     END DO

     CLOSE(sl_eb); CLOSE(sl_mb)
  END DO
  !$omp end parallel do

  eb(1:l_t, 1:n_s) = DBLE(ex_eb(1:l_t, 1:n_s)) / DBLE(l_x * l_y * l_z)
  mb(1:l_t, 1:n_s) = DBLE(ABS(ex_mb(1:l_t, 1:n_s))) / DBLE(l_x * l_y * l_z)
  DEALLOCATE(ex_eb, ex_mb)

  !$omp parallel do schedule(static, 1) default(none) &
  !$omp shared(n_s, l_th, l_t, n_b) &
  !$omp shared(eb, mb, b_eb, b_mb)
  DO s = 1, n_s, 1
    CALL convertStreamSamples(l_th, n_b, eb(1:l_t, s), b_eb(1:n_b, s))
    CALL convertStreamSamples(l_th, n_b, mb(1:l_t, s), b_mb(1:n_b, s))
  END DO
  !$omp end parallel do
  DEALLOCATE(eb, mb)

  !$omp parallel do schedule(static, 1) default(none) &
  !$omp shared(n_s, n_b, b_eb, b_mb) &
  !$omp shared(avg_eb, avg_mb, var_eb, var_mb, err_eb, err_mb)
  DO s = 1, n_s, 1
    CALL calcAvgVar(n_b, b_eb(1:n_b, s), avg_eb(s), var_eb(s))
    CALL calcAvgVar(n_b, b_mb(1:n_b, s), avg_mb(s), var_mb(s))

    err_eb(s) = SQRT(var_eb(s)) / SQRT(DBLE(n_b))
    err_mb(s) = SQRT(var_mb(s)) / SQRT(DBLE(n_b))
  END DO
  !$omp end parallel do
  
  OPEN(10, file="physquan1.dat", status="replace")
  DO s = 1, n_s, 1
     WRITE(10, '(i4, a, f0.4, a, i4, a, f0.8, a, f0.8, a, f0.8, a, f0.8)') &
     l_z, ", ", beta, ", ", vel, ", ", avg_eb(s), ", ", err_eb(s), ", ", avg_mb(s), ", ", err_mb(s)
  END DO
  CLOSE(10)

  sq_b_eb(1:n_b, 1:n_s) = b_eb(1:n_b, 1:n_s) ** 2
  sq_b_mb(1:n_b, 1:n_s) = b_mb(1:n_b, 1:n_s) ** 2
  fp_b_mb(1:n_b, 1:n_s) = sq_b_mb(1:n_b, 1:n_s) ** 2

  !$omp parallel do schedule(static, 1) default(none) &
  !$omp shared(n_s, n_b, beta, l_x, l_y, l_z) &
  !$omp shared(cb, chib, ub, err_cb, err_chib, err_ub) &
  !$omp shared(sq_b_eb, sq_b_mb, fp_b_mb) &
  !$omp shared(err_eb, err_mb, avg_eb, avg_mb)
  DO s = 1, n_s, 1
    cb(s) = (beta ** 2) * (SUM(sq_b_eb(1:n_b, s)) / DBLE(n_b) - avg_eb(s) ** 2) * DBLE(l_x * l_y * l_z)
    chib(s) = beta * (SUM(sq_b_mb(1:n_b, s)) / DBLE(n_b) - avg_mb(s) ** 2) * DBLE(l_x * l_y * l_z)
    ub(s) = 1 - (SUM(fp_b_mb(1:n_b, s)) / DBLE(n_b)) / (3.0d0 * (SUM(sq_b_mb(1:n_b, s)) / DBLE(n_b)) ** 2)
    
    !  TODO: 誤差の公式の確認
    err_cb(s)    = (beta ** 2) * (4 * err_eb(s) + err_eb(s) ** 2) * DBLE(l_x * l_y * l_z)
    err_chib(s)  = - beta * (4 * err_mb(s) + err_mb(s) ** 2) * DBLE(l_x * l_y * l_z)
    err_ub(s)    = 4 * (err_mb(s) / avg_mb(s)) ** 2
  END DO
  !$omp end parallel do

  OPEN(20, file="physquan2.dat", status="replace")
  DO s = 1, n_s, 1
     WRITE(20, '(i4, a, f0.4, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8)') &
     l_z, ", ", beta, ", ", cb(s), ", ", err_cb(s), ", ", chib(s), ", ", err_chib(s), ", ", ub(s), ", ", err_ub(s)
  END DO
  CLOSE(20)

END PROGRAM main
