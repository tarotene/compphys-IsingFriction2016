PROGRAM main
  USE mod_global
  USE MKL_VSL_TYPE
  USE MKL_VSL
  USE mod_proc
  IMPLICIT NONE

  INTEGER(4) :: sl_eb, sl_mb
  INTEGER(4), ALLOCATABLE :: eb(:, :), mb(:, :), eb_sq(:, :), mb_sq(:, :)
  REAL(4), ALLOCATABLE :: ac_eb(:, :), ac_mb(:, :), m_eb(:), m_mb(:), m_eb_sq(:), m_mb_sq(:)

  ! INTEGER(4) :: sl_ee, sl_me, sl_p
  ! INTEGER(4), ALLOCATABLE :: ee(:, :), me(:, :), p(:, :), ee_sq(:, :), me_sq(:, :), p_sq(:, :)
  ! REAL(4), ALLOCATABLE :: ac_ee(:, :), ac_me(:, :), ac_p(:, :), m_ee(:), m_me(:), m_p(:), m_ee_sq(:), m_me_sq(:), m_p_sq(:)

  INTEGER(4) :: s, t, loc, t_
  CHARACTER(4) :: ss

  CALL paramsCorr_3d_eq(l_x, l_y, l_z, beta, l_t, n_s)

  ALLOCATE(eb(1:n_s, 1:l_t), mb(1:n_s, 1:l_t), eb_sq(1:n_s, 1:l_t), mb_sq(1:n_s, 1:l_t), ac_eb(1:n_s, 1:l_t / 2), ac_mb(1:n_s, 1:l_t / 2))
  
  ! ALLOCATE(ee(1:n_s, 1:l_t), me(1:n_s, 1:l_t), p(1:n_s, 1:l_t), ee_sq(1:n_s, 1:l_t), me_sq(1:n_s, 1:l_t), p_sq(1:n_s, 1:l_t), ac_ee(1:n_s, 1:l_t / 2), , , ac_me(1:n_s, 1:l_t / 2), ac_p(1:n_s, 1:l_t))

  !$omp parallel do schedule(static, 1) default(none) &
  !$omp shared(n_s, l_t, eb, mb) &
  !$omp private(sl_eb, sl_mb, ss, loc, t)
  DO s = 1, n_s, 1
     sl_eb = 20 + s + 2 * n_s
     sl_mb = 20 + s + 3 * n_s

    !  sl_ee = 20 + s + 4 * n_s
    !  sl_me = 20 + s + 5 * n_s
    !  sl_p = 20 + s + 6 * n_s

     WRITE(ss, '(i0.4)') s
     OPEN(sl_eb, file="eb_sweep/en_bulk_s"//ss//"_sweep.bin", access="stream", status="old", buffered="YES")
     OPEN(sl_mb, file="mb_sweep/m_bulk_s"//ss//"_sweep.bin", access="stream", status="old", buffered="YES")

    !  OPEN(sl_ee, file="ee_sweep/en_edge_s"//ss//"_sweep.bin", access="stream", status="old", buffered="YES")
    !  OPEN(sl_me, file="me_sweep/m_edge_s"//ss//"_sweep.bin", access="stream", status="old", buffered="YES")
    !  OPEN(sl_p, file="p_sweep/pump_s"//ss//"_sweep.bin", access="stream", status="old", buffered="YES")
     
     loc = 1
     DO t = 1, l_t, 1
        READ(sl_eb, pos=loc) eb(s, t)
        READ(sl_mb, pos=loc) mb(s, t)

        ! READ(sl_ee, pos=loc) ee(s, t)
        ! READ(sl_me, pos=loc) me(s, t)
        ! READ(sl_p, pos=loc) p(s, t)
        loc = loc + 4
     END DO

     CLOSE(sl_eb)
     CLOSE(sl_mb)
     
    !  CLOSE(sl_ee)
    !  CLOSE(sl_me)
    !  CLOSE(sl_p)
  END DO
  !$omp end parallel do

  eb_sq(1:n_s, 1:l_t) = eb(1:n_s, 1:l_t) ** 2
  mb_sq(1:n_s, 1:l_t) = mb(1:n_s, 1:l_t) ** 2
  
  ! ee_sq(1:n_s, 1:l_t) = ee(1:n_s, 1:l_t) ** 2
  ! me_sq(1:n_s, 1:l_t) = me(1:n_s, 1:l_t) ** 2
  ! p_sq(1:n_s, 1:l_t) = p(1:n_s, 1:l_t) ** 2

  ALLOCATE(m_eb(1:n_s), m_mb(1:n_s), m_eb_sq(1:n_s), m_mb_sq(1:n_s))
  
  ! ALLOCATE(m_ee(1:n_s), m_me(1:n_s), m_p(1:n_s), m_ee_sq(1:n_s), m_me_sq(1:n_s), m_p_sq(1:n_s))

  ac_eb(1:n_s, 1:l_t / 2) = 0.0
  ac_mb(1:n_s, 1:l_t / 2) = 0.0

  ! ac_ee(1:n_s, 1:l_t / 2) = 0.0
  ! ac_me(1:n_s, 1:l_t / 2) = 0.0
  ! ac_p(1:n_s, 1:l_t / 2) = 0.0

  !$omp parallel do schedule(static, 1) default(none) &
  !$omp shared(n_s, eb, m_eb, mb, m_mb, ac_eb, ac_mb, l_t) &
  !$omp private(t_, t)
  DO s = 1, n_s, 1
     m_eb(s) = SUM(eb(s, 1:l_t)) / REAL(l_t)
     m_mb(s) = SUM(mb(s, 1:l_t)) / REAL(l_t)
     
    !  m_ee(s) = SUM(ee(s, 1:l_t)) / REAL(l_t)
    !  m_me(s) = SUM(me(s, 1:l_t)) / REAL(l_t)
    !  m_p(s) = SUM(p(s, 1:l_t)) / REAL(l_t)

     DO CONCURRENT (t = 1:l_t / 2:1)
        DO t_ = 1, l_t - t, 1
           ac_eb(s, t) = ac_eb(s, t) + (REAL(eb(s, t_)) - m_eb(s)) * (REAL(eb(s, t_ + t)) - m_eb(s))
           ac_mb(s, t) = ac_mb(s, t) + (REAL(mb(s, t_)) - m_mb(s)) * (REAL(mb(s, t_ + t)) - m_mb(s))

          !  ac_ee(s, t) = ac_ee(s, t) + (REAL(ee(s, t_)) - m_ee(s)) * (REAL(ee(s, t_ + t)) - m_ee(s))
          !  ac_me(s, t) = ac_me(s, t) + (REAL(me(s, t_)) - m_me(s)) * (REAL(me(s, t_ + t)) - m_me(s))
          !  ac_p(s, t) = ac_p(s, t) + (REAL(p(s, t_)) - m_p(s)) * (REAL(p(s, t_ + t)) - m_p(s))
        END DO

        ac_eb(s, t) = ac_eb(s, t) / REAL(l_t - t)
        ac_mb(s, t) = ac_mb(s, t) / REAL(l_t - t)

        ! ac_ee(s, t) = ac_ee(s, t) / REAL(l_t - t)
        ! ac_me(s, t) = ac_me(s, t) / REAL(l_t - t)
        ! ac_p(s, t) = ac_p(s, t) / REAL(l_t - t)
     END DO

     IF (ac_eb(s, 1) >= epsilon(0.0)) ac_eb(s, 1:l_t / 2) = ac_eb(s, 1:l_t / 2) / ac_eb(s, 1)
     IF (ac_mb(s, 1) >= epsilon(0.0)) ac_mb(s, 1:l_t / 2) = ac_mb(s, 1:l_t / 2) / ac_mb(s, 1)

    !  IF (ac_ee(s, 1) >= epsilon(0.0)) ac_ee(s, 1:l_t / 2) = ac_ee(s, 1:l_t / 2) / ac_ee(s, 1)
    !  IF (ac_me(s, 1) >= epsilon(0.0)) ac_me(s, 1:l_t / 2) = ac_me(s, 1:l_t / 2) / ac_me(s, 1)
    !  IF (ac_p(s, 1) >= epsilon(0.0)) ac_p(s, 1:l_t / 2) = ac_p(s, 1:l_t / 2) / ac_p(s, 1)
  END DO
  !$omp end parallel do

  !TODO: 空間的な相関関数の計算を行う

  !$omp parallel do schedule(static, 1) default(none) &
  !$omp shared(n_s, ac_eb, ac_mb, l_t) &
  !$omp private(sl_eb, sl_mb, ss)
  DO s = 1, n_s, 1
     sl_eb = 20 + s + 2 * n_s
     sl_mb = 20 + s + 3 * n_s

    !  sl_ee = 20 + s + 4 * n_s
    !  sl_me = 20 + s + 5 * n_s
    !  sl_p = 20 + s + 6 * n_s

     WRITE(ss, '(i0.4)') s
     OPEN(sl_eb, file="ac_en_bulk_s"//ss//".bin", status="replace", access="stream", buffered="YES")
     OPEN(sl_mb, file="ac_m_bulk_s"//ss//".bin", status="replace", access="stream", buffered="YES")
     
    !  OPEN(sl_ee, file="ac_en_edge_s"//ss//".bin", status="replace", access="stream", buffered="YES")
    !  OPEN(sl_me, file="ac_m_edge_s"//ss//".bin", status="replace", access="stream", buffered="YES")
    !  OPEN(sl_p, file="ac_p_s"//ss//".bin", status="replace", access="stream", buffered="YES")

     WRITE(sl_eb) ac_eb(s, 1:l_t / 2)
     WRITE(sl_mb) ac_mb(s, 1:l_t / 2)

    !  WRITE(sl_ee) ac_ee(s, 1:l_t / 2)
    !  WRITE(sl_me) ac_me(s, 1:l_t / 2)
    !  WRITE(sl_p) ac_p(s, 1:l_t / 2)

     CLOSE(sl_eb)
     CLOSE(sl_mb)

    !  CLOSE(sl_ee)
    !  CLOSE(sl_me)
    !  CLOSE(sl_p)
  END DO
  !$omp end parallel do
END PROGRAM main
