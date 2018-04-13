PROGRAM main
  USE mod_global
  USE MKL_VSL_TYPE
  USE MKL_VSL
  USE mod_proc
  IMPLICIT NONE

  INTEGER(4) :: sl_eb, sl_mb
  INTEGER(4) :: sl_ee, sl_me, sl_p

  INTEGER(4), ALLOCATABLE :: ex_eb(:, :), ex_mb(:, :)
  ! INTEGER(4), ALLOCATABLE :: ex_ee(:, :), ex_me(:, :), ex_p(:, :)

  REAL(8), ALLOCATABLE :: eb(:, :), mb(:, :), eb_sq(:, :), mb_sq(:, :), mb_fp(:, :), b_eb(:, :), b_mb(:, :), b_eb_sq(:, :), b_mb_sq(:, :), b_mb_fp(:, :)
  ! REAL(8), ALLOCATABLE :: ee(:, :), me(:, :), p(:, :), ee_sq(:, :), me_sq(:, :), me_fp(:, :), b_ee(:, :), b_me(:, :), b_p(:, :), b_ee_sq(:, :), b_me_sq(:, :), b_me_fp(:, :)

  REAL(8), ALLOCATABLE :: avg_eb(:), avg_mb(:), var_eb(:), var_mb(:), err_eb(:), err_mb(:)
  ! REAL(8), ALLOCATABLE :: avg_ee(:), avg_me(:), avg_p(:), var_ee(:), var_me(:), var_p(:), err_ee(:), err_me(:), err_p(:)

  REAL(8), ALLOCATABLE :: cb(:), chib(:), ub(:), err_cb(:), err_chib(:), err_ub(:)
  ! REAL(8), ALLOCATABLE :: ce(:), chie(:), ue(:), err_ce(:), err_chie(:), err_ue(:)
  
  INTEGER(4) :: t, s, loc, t_
  CHARACTER(4) :: ss

  CALL paramsCalc_2d(l_x, l_z, beta, vel, l_t, n_s, l_th, n_b)

  ALLOCATE(ex_eb(1:l_t, 1:n_s), ex_mb(1:l_t, 1:n_s))
  ! ALLOCATE(ex_ee(1:l_t, 1:n_s), ex_me(1:l_t, 1:n_s), ex_p(1:l_t, 1:n_s))

  ALLOCATE(eb(1:l_t, 1:n_s), mb(1:l_t, 1:n_s), eb_sq(1:l_t, 1:n_s), mb_sq(1:l_t, 1:n_s), mb_fp(1:l_t, 1:n_s))
  ! ALLOCATE(ee(1:l_t, 1:n_s), me(1:l_t, 1:n_s), p(1:l_t, 1:n_s), ee_sq(1:l_t, 1:n_s), me_sq(1:l_t, 1:n_s), me_fp(1:l_t, 1:n_s))

  ALLOCATE(b_eb(1:n_b, 1:n_s), b_mb(1:n_b, 1:n_s), b_eb_sq(1:n_b, 1:n_s), b_mb_sq(1:n_b, 1:n_s), b_mb_fp(1:n_b, 1:n_s))
  ! ALLOCATE(b_ee(1:n_b, 1:n_s), b_me(1:n_b, 1:n_s), b_p(1:n_b, 1:n_s), b_ee_sq(1:n_b, 1:n_s), b_me_sq(1:n_b, 1:n_s), b_me_fp(1:n_b, 1:n_s))

  ALLOCATE(avg_eb(1:n_s), avg_mb(1:n_s), var_eb(1:n_s), var_mb(1:n_s), err_eb(1:n_s), err_mb(1:n_s))
  ! ALLOCATE(avg_ee(1:n_s), var_ee(1:n_s), err_ee(1:n_s), avg_me(1:n_s), var_me(1:n_s), err_me(1:n_s), avg_p(1:n_s), var_p(1:n_s), err_p(1:n_s))

  ALLOCATE(cb(1:n_s), chib(1:n_s), ub(1:n_s), err_cb(1:n_s), err_chib(1:n_s), err_ub(1:n_s))
  ! ALLOCATE(ce(1:n_s), chie(1:n_s), ue(1:n_s), err_ce(1:n_s), err_chie(1:n_s), err_ue(1:n_s))

  !$omp parallel do schedule(static, 1) default(none) &
  !$omp shared(n_s, l_t, ex_eb, ex_mb) &
  !$omp private(sl_eb, sl_mb, ss, loc, t)
  DO s = 1, n_s, 1
     sl_eb = 20 + s + 2 * n_s
     sl_mb = 20 + s + 3 * n_s

    !  sl_ee = 20 + s + 4 * n_s
    !  sl_me = 20 + s + 5 * n_s
    !  sl_p  = 20 + s + 6 * n_s

     WRITE(ss, '(i0.4)') s
     OPEN(sl_eb, file="eb_sweep/en_bulk_s"//ss//"_sweep.bin", access="stream", status="old", buffered="YES")
     OPEN(sl_mb, file="mb_sweep/m_bulk_s"//ss//"_sweep.bin",  access="stream", status="old", buffered="YES")

    !  OPEN(sl_ee, file="ee_sweep/en_edge_s"//ss//"_sweep.bin", access="stream", status="old", buffered="YES")
    !  OPEN(sl_me, file="me_sweep/m_edge_s"//ss//"_sweep.bin",  access="stream", status="old", buffered="YES")
    !  OPEN(sl_p,  file="p_sweep/pump_s"//ss//"_sweep.bin",     access="stream", status="old", buffered="YES")

     loc = 1
     DO t = 1, l_t, 1
        READ(sl_eb, pos = loc) ex_eb(t, s)
        READ(sl_mb, pos = loc) ex_mb(t, s)

        ! READ(sl_ee, pos = loc) ex_ee(t, s)
        ! READ(sl_me, pos = loc) ex_me(t, s)
        ! READ(sl_p,  pos = loc) ex_p(t, s)
        loc = loc + 4
     END DO

     CLOSE(sl_eb)
     CLOSE(sl_mb)

    !  CLOSE(sl_ee)
    !  CLOSE(sl_me)
    !  CLOSE(sl_p)
  END DO
  !$omp end parallel do

  eb(1:l_t, 1:n_s) = DBLE(ex_eb(1:l_t, 1:n_s)) / DBLE(l_x * l_z)
  mb(1:l_t, 1:n_s) = DBLE(ABS(ex_mb(1:l_t, 1:n_s))) / DBLE(l_x * l_z)

  ! ee(1:l_t, 1:n_s) = DBLE(ex_ee(1:l_t, 1:n_s)) / DBLE(l_x)
  ! me(1:l_t, 1:n_s) = DBLE(ABS(ex_me(1:l_t, 1:n_s))) / DBLE(2 * l_x)
  ! p(1:l_t, 1:n_s) = DBLE(ex_p(1:l_t, 1:n_s)) / DBLE(l_x)

  DEALLOCATE(ex_eb, ex_mb)
  ! DEALLOCATE(ex_ee, ex_me, ex_p)

  eb_sq(1:l_t, 1:n_s) = eb(1:l_t, 1:n_s) ** 2
  mb_sq(1:l_t, 1:n_s) = mb(1:l_t, 1:n_s) ** 2
  mb_fp(1:l_t, 1:n_s) = mb_sq(1:l_t, 1:n_s) ** 2

  ! ee_sq(1:l_t, 1:n_s) = ee(1:l_t, 1:n_s) ** 2
  ! me_sq(1:l_t, 1:n_s) = me(1:l_t, 1:n_s) ** 2
  ! me_fp(1:l_t, 1:n_s) = me_sq(1:l_t, 1:n_s) ** 2

  !$omp parallel do schedule(static, 1) default(none) &
  !$omp shared(n_s, l_th, l_t, n_b, eb, mb, b_eb, b_mb)
  DO s = 1, n_s, 1
    CALL convertStreamSamples(l_th, n_b, eb(1:l_t, s), b_eb(1:n_b, s))
    CALL convertStreamSamples(l_th, n_b, mb(1:l_t, s), b_mb(1:n_b, s))
    
    ! CALL convertStreamSamples(l_th, n_b, ee(1:l_t, s), b_ee(1:n_b, s))
    ! CALL convertStreamSamples(l_th, n_b, me(1:l_t, s), b_me(1:n_b, s))
    ! CALL convertStreamSamples(l_th, n_b, p(1:l_t, s), b_p(1:n_b, s))
  END DO
  !$omp end parallel do

  DEALLOCATE(eb, mb, ee, me, p)

  !$omp parallel do schedule(static, 1) default(none) &
  !$omp shared(n_s, l_th, l_t, n_b, eb_sq, mb_sq, mb_fp, b_eb_sq, b_mb_sq, b_mb_fp)
  DO s = 1, n_s, 1
    CALL convertStreamSamples(l_th, n_b, eb_sq(1:l_t, s), b_eb_sq(1:n_b, s))
    CALL convertStreamSamples(l_th, n_b, mb_sq(1:l_t, s), b_mb_sq(1:n_b, s))
    CALL convertStreamSamples(l_th, n_b, mb_fp(1:l_t, s), b_mb_fp(1:n_b, s))

    ! CALL convertStreamSamples(l_th, n_b, ee_sq(1:l_t, s), b_ee_sq(1:n_b, s))
    ! CALL convertStreamSamples(l_th, n_b, me_sq(1:l_t, s), b_me_sq(1:n_b, s))
    ! CALL convertStreamSamples(l_th, n_b, me_fp(1:l_t, s), b_me_fp(1:n_b, s))
  END DO
  !$omp end parallel do

  !$omp parallel do schedule(static, 1) default(none) &
  !$omp shared(n_s, n_b, b_eb, b_mb, b_p, avg_eb, avg_mb, var_eb, var_mb, err_eb, err_mb, err_p)
  DO s = 1, n_s, 1
    CALL calcAvgVar(n_b, b_eb(1:n_b, s), avg_eb(s), var_eb(s))
    CALL calcAvgVar(n_b, b_mb(1:n_b, s), avg_mb(s), var_mb(s))

    ! CALL calcAvgVar(n_b, b_ee(1:n_b, s), avg_ee(s), var_ee(s))
    ! CALL calcAvgVar(n_b, b_me(1:n_b, s), avg_me(s), var_me(s))
    ! CALL calcAvgVar(n_b, b_p(1:n_b, s), avg_p(s), var_p(s))

    err_eb(s) = SQRT(var_eb(s)) / SQRT(DBLE(n_b))
    err_mb(s) = SQRT(var_mb(s)) / SQRT(DBLE(n_b))

    ! err_ee(s) = SQRT(var_ee(s)) / SQRT(DBLE(n_b))
    ! err_me(s) = SQRT(var_me(s)) / SQRT(DBLE(n_b))
    ! err_p(s) = SQRT(var_p(s)) / SQRT(DBLE(n_b))
  END DO
  !$omp end parallel do
  
  OPEN(10, file="physquan1.dat", status="replace")
  ! WRITE(10, '(a)') "#  l_z,     beta,  vel,       eb,    err_eb,       mb,   err_mb"
  WRITE(10, '(a, i4, a, f8.4, a, i4, a, f8.4, a, f8.4, a, f8.4, a, f8.4)') &
  "  ", l_z, ", ", beta, ", ", vel, ", ", &
  SUM(avg_eb(1:n_s)) / DBLE(n_s), ", ", SUM(err_eb(1:n_s)) / DBLE(n_s), ", ", &
  SUM(avg_mb(1:n_s)) / DBLE(n_s), ", ", SUM(err_mb(1:n_s)) / DBLE(n_s)
  CLOSE(10)

  !$omp parallel do schedule(static, 1) default(none) &
  !$omp shared(n_s, n_b, beta, l_x, l_z, avg_eb, err_eb, avg_mb, err_mb, b_eb_sq, b_mb_sq, b_mb_fp, cb, err_cb, chib, err_chib, ub, err_ub)
  DO s = 1, n_s, 1
    cb(s) = (beta ** 2) * (SUM(b_eb_sq(1:n_b, s)) / DBLE(n_b) - avg_eb(s) ** 2) * DBLE(l_x * l_z)
    chib(s) = beta * (SUM(b_mb_sq(1:n_b, s)) / DBLE(n_b) - avg_mb(s) ** 2) * DBLE(l_x * l_z)
    ub(s) = 1 - (SUM(b_mb_fp(1:n_b, s)) / DBLE(n_b)) / (3.0d0 * (SUM(b_mb_fp(1:n_b, s)) / DBLE(n_b)) ** 2)

    !  TODO: 誤差の公式の確認
    err_cb(s)    = (beta ** 2) * (4 * err_eb(s) + err_eb(s) ** 2) * DBLE(l_x * l_z)
    err_chib(s)  = - beta * (4 * err_mb(s) + err_mb(s) ** 2) * DBLE(l_x * l_z)
    err_ub(s)    = 4 * (err_mb(s) / avg_mb(s)) ** 2

    ! ce(s) = (beta ** 2) * (SUM(b_ee_sq(1:n_b, s)) / DBLE(n_b) - avg_ee(s) ** 2) * DBLE(l_x)
    ! chie(s) = beta * (SUM(b_me_sq(1:n_b, s)) / DBLE(n_b) - avg_me(s) ** 2) * DBLE(2 * l_x
    ! ue(s) = 1 - (SUM(b_me_fp(1:n_b, s)) / DBLE(n_b)) / (3.0d0 * (SUM(b_me_fp(1:n_b, s)) / DBLE(n_b)) ** 2)

    ! err_ce(s)    = (beta ** 2) * (4 * err_ee(s) + err_ee(s) ** 2) * DBLE(l_x)
    ! err_chie(s)  = - beta * (4 * err_me(s) + err_me(s) ** 2) * DBLE(2 * l_x)
    ! err_ue(s)    = 4 * (err_me(s) / avg_me(s)) ** 2
  END DO
  !$omp end parallel do

  OPEN(20, file="physquan2.dat", status="replace")
  ! WRITE(20, '(a)') "#  l_z,     beta,  vel,       cb,    err_cb,     chib, err_chib,       ub,   err_ub"
  WRITE(20, '(a, i4, a, f8.4, a, i4, a, f8.4, a, f8.4, a, f8.4, a, f8.4, a, f8.4, a, f8.4)') &
  "  ", l_z, ", ", beta, ", ", vel, ", ", &
  SUM(cb(1:n_s)) / DBLE(n_s), ", ", SUM(err_cb(1:n_s)) / DBLE(n_s), ", ", &
  SUM(chib(1:n_s)) / DBLE(n_s), ", ", SUM(err_chib(1:n_s)) / DBLE(n_s), ", ", &
  SUM(ub(1:n_s)) / DBLE(n_s), ", ", SUM(err_ub(1:n_s)) / DBLE(n_s), ", ", &

  ! SUM(ce(1:n_s)) / DBLE(n_s), ", ", SUM(err_ce(1:n_s)) / DBLE(n_s), ", ", &
  ! SUM(chie(1:n_s)) / DBLE(n_s), ", ", SUM(err_chie(1:n_s)) / DBLE(n_s), ", ", &
  ! SUM(ue(1:n_s)) / DBLE(n_s), ", ", SUM(err_ue(1:n_s)) / DBLE(n_s)
  CLOSE(20)

END PROGRAM main
