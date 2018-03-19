PROGRAM main
  USE mod_global
  USE MKL_VSL_TYPE
  USE MKL_VSL
  USE mod_proc
  IMPLICIT NONE

  INTEGER(4) :: sl_eb, sl_ee, sl_mb, sl_me
  INTEGER(4), ALLOCATABLE :: lb_t(:), rb_t(:)

  INTEGER(4), ALLOCATABLE :: eb(:, :), ee(:, :), mb(:, :), me(:, :)
  REAL(4), ALLOCATABLE :: ex_eb(:, :), ex_ee(:, :), ex_mb(:, :), ex_me(:, :)
  REAL(4), ALLOCATABLE :: amb(:, :), ame(:, :)
  REAL(4), ALLOCATABLE :: eb_sq(:, :), mb_sq(:, :), ee_sq(:, :), me_sq(:, :)
  REAL(4), ALLOCATABLE :: mb_fp(:, :), me_fp(:, :)
  REAL(4), ALLOCATABLE :: ac_eb(:, :), ac_amb(:, :), ac_ee(:, :), ac_ame(:, :)
  REAL(4), ALLOCATABLE :: m_eb(:), m_ee(:), m_amb(:), m_ame(:)
  REAL(4), ALLOCATABLE :: bm_eb(:, :), bm_ee(:, :), bm_amb(:, :), bm_ame(:, :)
  REAL(4), ALLOCATABLE :: m_eb_sq(:), m_ee_sq(:), m_mb_sq(:), m_me_sq(:)
  REAL(4), ALLOCATABLE :: bm_eb_sq(:, :), bm_ee_sq(:, :), bm_mb_sq(:, :), bm_me_sq(:, :)
  REAL(4), ALLOCATABLE :: m_mb_fp(:), m_me_fp(:)
  REAL(4), ALLOCATABLE :: bm_mb_fp(:, :), bm_me_fp(:, :)
  REAL(4), ALLOCATABLE :: b_cb(:, :), b_ce(:, :), b_chib(:, :), b_chie(:, :), b_ub(:, :), b_ue(:, :)
  REAL(4), ALLOCATABLE :: b_cb_sq(:, :), b_ce_sq(:, :), b_chib_sq(:, :), b_chie_sq(:, :), b_ub_sq(:, :), b_ue_sq(:, :)
  REAL(4), ALLOCATABLE :: m_cb(:), m_ce(:), m_chib(:), m_chie(:), m_ub(:), m_ue(:)
  REAL(4), ALLOCATABLE :: b_err_cb(:, :), b_err_ce(:, :), b_err_chib(:, :), b_err_chie(:, :), b_err_ub(:, :), b_err_ue(:, :)
  REAL(4), ALLOCATABLE :: err_cb(:), err_ce(:), err_chib(:), err_chie(:), err_ub(:), err_ue(:)
  INTEGER(4) :: b, n_b, s, t, loc, t_
  CHARACTER(4) :: ss

  CALL paramsCalc_2d(l_x, l_z, beta, vel, l_t, n_s, l_th, l_b)

  ALLOCATE(eb(1:n_s, 1:l_t), ee(1:n_s, 1:l_t), mb(1:n_s, 1:l_t), me(1:n_s, 1:l_t))
  ALLOCATE(ex_eb(1:n_s, 1:l_t), ex_ee(1:n_s, 1:l_t), ex_mb(1:n_s, 1:l_t), ex_me(1:n_s, 1:l_t))
  ALLOCATE(amb(1:n_s, 1:l_t), ame(1:n_s, 1:l_t))
  ALLOCATE(eb_sq(1:n_s, 1:l_t), ee_sq(1:n_s, 1:l_t), mb_sq(1:n_s, 1:l_t), me_sq(1:n_s, 1:l_t))
  ALLOCATE(mb_fp(1:n_s, 1:l_t), me_fp(1:n_s, 1:l_t))
  ALLOCATE(ac_eb(1:n_s, 1:l_t / 2), ac_ee(1:n_s, 1:l_t / 2), ac_amb(1:n_s, 1:l_t / 2), ac_ame(1:n_s, 1:l_t / 2))

  DO s = 1, n_s, 1
     sl_eb = 20 + s + 2 * n_s; sl_mb = 20 + s + 3 * n_s
     sl_ee = 20 + s + 4 * n_s; sl_me = 20 + s + 5 * n_s

     WRITE(ss, '(i0.4)') s
     OPEN(sl_eb, file="eb_sweep/en_bulk_s"//ss//"_sweep.bin", access="stream", status="old", buffered="YES")
     OPEN(sl_ee, file="ee_sweep/en_edge_s"//ss//"_sweep.bin", access="stream", status="old", buffered="YES")
     OPEN(sl_mb, file="mb_sweep/m_bulk_s"//ss//"_sweep.bin", access="stream", status="old", buffered="YES")
     OPEN(sl_me, file="me_sweep/m_edge_s"//ss//"_sweep.bin", access="stream", status="old", buffered="YES")

     loc = 1
     DO t = 1, l_t, 1
        READ(sl_eb, pos=loc) eb(s, t); READ(sl_mb, pos=loc) mb(s, t)
        READ(sl_ee, pos=loc) ee(s, t); READ(sl_me, pos=loc) me(s, t)
        loc = loc + 4
     END DO

     CLOSE(sl_eb); CLOSE(sl_mb); CLOSE(sl_ee); CLOSE(sl_me)
  END DO

  ex_eb(1:n_s, 1:l_t) = REAL(eb(1:n_s, 1:l_t)) / REAL(l_x * l_z)
  ex_ee(1:n_s, 1:l_t) = REAL(ee(1:n_s, 1:l_t)) / REAL(l_x)
  ex_mb(1:n_s, 1:l_t) = REAL(mb(1:n_s, 1:l_t)) / REAL(l_x * l_z)
  ex_me(1:n_s, 1:l_t) = REAL(me(1:n_s, 1:l_t)) / REAL(2 * l_x)

  eb_sq(1:n_s, 1:l_t) = ex_eb(1:n_s, 1:l_t) ** 2
  ee_sq(1:n_s, 1:l_t) = ex_ee(1:n_s, 1:l_t) ** 2
  amb(1:n_s, 1:l_t) = ABS(ex_mb(1:n_s, 1:l_t))
  ame(1:n_s, 1:l_t) = ABS(ex_me(1:n_s, 1:l_t))
  mb_sq(1:n_s, 1:l_t) = ex_mb(1:n_s, 1:l_t) ** 2
  me_sq(1:n_s, 1:l_t) = ex_me(1:n_s, 1:l_t) ** 2
  mb_fp(1:n_s, 1:l_t) = ex_mb(1:n_s, 1:l_t) ** 4
  me_fp(1:n_s, 1:l_t) = ex_me(1:n_s, 1:l_t) ** 4

  n_b = (l_t - l_th) / l_b
  ALLOCATE(lb_t(1:n_b), rb_t(1:n_b))
  DO CONCURRENT (b = 1:n_b:1)
     lb_t(b) = l_th + 1 + (b - 1) * l_b
     rb_t(b) = l_th + b * l_b
  END DO
  ALLOCATE(bm_eb(1:n_b, 1:n_s), bm_ee(1:n_b, 1:n_s), bm_amb(1:n_b, 1:n_s), bm_ame(1:n_b, 1:n_s))
  ALLOCATE(bm_eb_sq(1:n_b, 1:n_s), bm_ee_sq(1:n_b, 1:n_s), bm_mb_sq(1:n_b, 1:n_s), bm_me_sq(1:n_b, 1:n_s))
  ALLOCATE(bm_mb_fp(1:n_b, 1:n_s), bm_me_fp(1:n_b, 1:n_s))
  ALLOCATE(b_cb(1:n_b, 1:n_s), b_ce(1:n_b, 1:n_s), b_chib(1:n_b, 1:n_s), b_chie(1:n_b, 1:n_s), b_ub(1:n_b, 1:n_s), b_ue(1:n_b, 1:n_s))
  ALLOCATE(b_cb_sq(1:n_b, 1:n_s), b_ce_sq(1:n_b, 1:n_s), b_chib_sq(1:n_b, 1:n_s), b_chie_sq(1:n_b, 1:n_s), b_ub_sq(1:n_b, 1:n_s), b_ue_sq(1:n_b, 1:n_s))
  ALLOCATE(m_cb(1:n_s), m_ce(1:n_s), m_chib(1:n_s), m_chie(1:n_s), m_ub(1:n_s), m_ue(1:n_s))
  ALLOCATE(b_err_cb(1:n_b, 1:n_s), b_err_ce(1:n_b, 1:n_s), b_err_chib(1:n_b, 1:n_s), b_err_chie(1:n_b, 1:n_s), b_err_ub(1:n_b, 1:n_s), b_err_ue(1:n_b, 1:n_s))
  ALLOCATE(err_cb(1:n_s), err_ce(1:n_s), err_chib(1:n_s), err_chie(1:n_s), err_ub(1:n_s), err_ue(1:n_s))

  DO CONCURRENT (s = 1:n_s:1)
     DO CONCURRENT (b = 1:n_b:1)
        bm_eb(b, s) = REAL(SUM(ex_eb(s, lb_t(b):rb_t(b)))) / REAL(l_b)
        bm_ee(b, s) = REAL(SUM(ex_ee(s, lb_t(b):rb_t(b)))) / REAL(l_b)
        bm_amb(b, s) = REAL(SUM(amb(s, lb_t(b):rb_t(b)))) / REAL(l_b)
        bm_ame(b, s) = REAL(SUM(ame(s, lb_t(b):rb_t(b)))) / REAL(l_b)
        bm_eb_sq(b, s) = REAL(SUM(eb_sq(s, lb_t(b):rb_t(b)))) / REAL(l_b)
        bm_ee_sq(b, s) = REAL(SUM(ee_sq(s, lb_t(b):rb_t(b)))) / REAL(l_b)
        bm_mb_sq(b, s) = REAL(SUM(mb_sq(s, lb_t(b):rb_t(b)))) / REAL(l_b)
        bm_me_sq(b, s) = REAL(SUM(me_sq(s, lb_t(b):rb_t(b)))) / REAL(l_b)
        bm_mb_fp(b, s) = REAL(SUM(mb_fp(s, lb_t(b):rb_t(b)))) / REAL(l_b)
        bm_me_fp(b, s) = REAL(SUM(me_fp(s, lb_t(b):rb_t(b)))) / REAL(l_b)
     END DO
  END DO
  b_cb(1:n_b, 1:n_s) = (bm_eb_sq(1:n_b, 1:n_s) - bm_eb(1:n_b, 1:n_s) ** 2) * (beta ** 2) * REAL(l_x * l_z)
  b_ce(1:n_b, 1:n_s) = (bm_ee_sq(1:n_b, 1:n_s) - bm_ee(1:n_b, 1:n_s) ** 2) * (beta ** 2) * REAL(l_x)
  b_chib(1:n_b, 1:n_s) = (bm_mb_sq(1:n_b, 1:n_s) - bm_amb(1:n_b, 1:n_s) ** 2) * beta * REAL(l_x * l_z)
  b_chie(1:n_b, 1:n_s) = (bm_me_sq(1:n_b, 1:n_s) - bm_ame(1:n_b, 1:n_s) ** 2) * beta * REAL(l_x * 2)
  b_ub(1:n_b, 1:n_s) = 1 - bm_mb_fp(1:n_b, 1:n_s) / (3.0 * (bm_mb_sq(1:n_b, 1:n_s) ** 2))
  b_ue(1:n_b, 1:n_s) = 1 - bm_me_fp(1:n_b, 1:n_s) / (3.0 * (bm_me_sq(1:n_b, 1:n_s) ** 2))

  b_cb_sq(1:n_b, 1:n_s) = b_cb(1:n_b, 1:n_s) ** 2
  b_ce_sq(1:n_b, 1:n_s) = b_ce(1:n_b, 1:n_s) ** 2
  b_chib_sq(1:n_b, 1:n_s) = b_chib(1:n_b, 1:n_s) ** 2
  b_chie_sq(1:n_b, 1:n_s) = b_chie(1:n_b, 1:n_s) ** 2
  b_ub_sq(1:n_b, 1:n_s) = b_ub(1:n_b, 1:n_s) ** 2
  b_ue_sq(1:n_b, 1:n_s) = b_ue(1:n_b, 1:n_s) ** 2

  !TODO: ブロック平均とブロック分散の計算
  DO CONCURRENT (s = 1:n_s:1)
    m_cb(s) = SUM(b_cb(1:n_b, s)) / REAL(n_b)
    m_ce(s) = SUM(b_ce(1:n_b, s)) / REAL(n_b)
    m_chib(s) = SUM(b_chib(1:n_b, s)) / REAL(n_b)
    m_chie(s) = SUM(b_chie(1:n_b, s)) / REAL(n_b)
    m_ub(s) = SUM(b_ub(1:n_b, s)) / REAL(n_b)
    m_ue(s) = SUM(b_ue(1:n_b, s)) / REAL(n_b)

    err_cb(s) = SQRT(SUM(b_cb_sq(1:n_b, s)) / REAL(n_b) - m_cb(s) ** 2) / SQRT(REAL(n_b * (n_b - 1)))
    err_ce(s) = SQRT(SUM(b_ce_sq(1:n_b, s)) / REAL(n_b) - m_ce(s) ** 2) / SQRT(REAL(n_b * (n_b - 1)))
    err_chib(s) = SQRT(SUM(b_chib_sq(1:n_b, s)) / REAL(n_b) - m_chib(s) ** 2) / SQRT(REAL(n_b * (n_b - 1)))
    err_chie(s) = SQRT(SUM(b_chie_sq(1:n_b, s)) / REAL(n_b) - m_chie(s) ** 2) / SQRT(REAL(n_b * (n_b - 1)))
    err_ub(s) = SQRT(SUM(b_ub_sq(1:n_b, s)) / REAL(n_b) - m_ub(s) ** 2) / SQRT(REAL(n_b * (n_b - 1)))
    err_ue(s) = SQRT(SUM(b_ue_sq(1:n_b, s)) / REAL(n_b) - m_ue(s) ** 2) / SQRT(REAL(n_b * (n_b - 1)))
  END DO

  DO s = 1, n_s, 1
     WRITE(*, '(f0.4, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8, a, f0.8)') &
     beta, ", ", m_cb(s), ", ", err_cb(s), ", ", m_ce(s), ", ", err_ce(s), ", ", &
     m_chib(s), ", ", err_chib(s), ", ", m_chie(s), ", ", err_chie(s), ", ", &
     m_ub(s), ", ", err_ub(s), ", ", m_ue(s), ", ", err_ue(s)
  END DO

END PROGRAM main
