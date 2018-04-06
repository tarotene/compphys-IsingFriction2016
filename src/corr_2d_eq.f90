PROGRAM main
  USE mod_global
  USE MKL_VSL_TYPE
  USE MKL_VSL
  USE mod_proc
  IMPLICIT NONE

  INTEGER(4) :: sl_eb, sl_ee, sl_mb, sl_me, sl_p
  INTEGER(4), ALLOCATABLE :: eb(:, :), mb(:, :), p(:, :)
  INTEGER(4), ALLOCATABLE :: amb(:, :), 
  INTEGER(4), ALLOCATABLE :: eb_sq(:, :), mb_sq(:, :)
  REAL(4), ALLOCATABLE :: ac_eb(:, :), ac_amb(:, :)
  REAL(4), ALLOCATABLE :: m_eb(:), m_amb(:)
  REAL(4), ALLOCATABLE :: m_eb_sq(:), m_mb_sq(:)
  INTEGER(4) :: s, t, loc, t_
  CHARACTER(4) :: ss

  CALL paramsCorrelate_2d_eq(l_x, l_z, beta, l_t, n_s)

  ALLOCATE(eb(1:n_s, 1:l_t), mb(1:n_s, 1:l_t), amb(1:n_s, 1:l_t), eb_sq(1:n_s, 1:l_t), mb_sq(1:n_s, 1:l_t))
  ALLOCATE(ac_eb(1:n_s, 1:l_t / 2), ac_amb(1:n_s, 1:l_t / 2))

  !$omp parallel do schedule(static, 1) default(none) &
  !$omp shared(n_s, l_t, eb, mb) &
  !$omp private(sl_eb, sl_mb, ss, loc, t)
  DO s = 1, n_s, 1
     sl_eb = 20 + s + 2 * n_s; sl_mb = 20 + s + 3 * n_s

     WRITE(ss, '(i0.4)') s
     OPEN(sl_eb, file="eb_sweep/en_bulk_s"//ss//"_sweep.bin", access="stream", status="old", buffered="YES")
     OPEN(sl_mb, file="mb_sweep/m_bulk_s"//ss//"_sweep.bin", access="stream", status="old", buffered="YES")
     
     loc = 1
     DO t = 1, l_t, 1
        READ(sl_eb, pos=loc) eb(s, t); READ(sl_mb, pos=loc) mb(s, t)
        loc = loc + 4
     END DO

     CLOSE(sl_eb); CLOSE(sl_mb)
  END DO
  !$omp end parallel do

  eb_sq(1:n_s, 1:l_t) = eb(1:n_s, 1:l_t) ** 2
  amb(1:n_s, 1:l_t) = ABS(mb(1:n_s, 1:l_t))
  mb_sq(1:n_s, 1:l_t) = amb(1:n_s, 1:l_t) ** 2

  ALLOCATE(m_eb(1:n_s), m_amb(1:n_s), m_eb_sq(1:n_s), m_mb_sq(1:n_s))

  ac_eb(1:n_s, 1:l_t / 2) = 0.0
  ac_amb(1:n_s, 1:l_t / 2) = 0.0

  !$omp parallel do schedule(static, 1) default(none) &
  !$omp shared(n_s, m_eb, m_amb, eb, amb, ac_eb, ac_amb, l_t) &
  !$omp private(t_, t)
  DO s = 1, n_s, 1
     m_eb(s) = SUM(eb(s, 1:l_t)) / REAL(l_t)
     m_amb(s) = SUM(amb(s, 1:l_t)) / REAL(l_t)

     DO CONCURRENT (t = 1:l_t / 2:1)
        DO t_ = 1, l_t - t, 1
           ac_eb(s, t) = ac_eb(s, t) + (REAL(eb(s, t_)) - m_eb(s)) * (REAL(eb(s, t_ + t)) - m_eb(s))
           ac_amb(s, t) = ac_amb(s, t) + (REAL(amb(s, t_)) - m_amb(s)) * (REAL(amb(s, t_ + t)) - m_amb(s))
        END DO

        ac_eb(s, t) = ac_eb(s, t) / REAL(l_t - t)
        ac_amb(s, t) = ac_amb(s, t) / REAL(l_t - t)
     END DO

     IF (ac_eb(s, 1) >= epsilon(0.0)) ac_eb(s, 1:l_t / 2) = ac_eb(s, 1:l_t / 2) / ac_eb(s, 1)
     IF (ac_amb(s, 1) >= epsilon(0.0)) ac_amb(s, 1:l_t / 2) = ac_amb(s, 1:l_t / 2) / ac_amb(s, 1)
  END DO
  !$omp end parallel do

  !TODO: 空間的な相関関数の計算を行う

  !$omp parallel do schedule(static, 1) default(none) &
  !$omp shared(n_s, ac_eb, ac_amb, l_t) &
  !$omp private(sl_eb, sl_mb, ss)
  DO s = 1, n_s, 1
     sl_eb = 20 + s + 2 * n_s; sl_mb = 20 + s + 3 * n_s

     WRITE(ss, '(i0.4)') s
     OPEN(sl_eb, file="ac_en_bulk_s"//ss//".bin", status="replace", access="stream", buffered="YES")
     OPEN(sl_mb, file="ac_m_bulk_s"//ss//".bin", status="replace", access="stream", buffered="YES")

     WRITE(sl_eb) ac_eb(s, 1:l_t / 2); WRITE(sl_mb) ac_amb(s, 1:l_t / 2)

     CLOSE(sl_eb); CLOSE(sl_mb)
  END DO
  !$omp end parallel do
END PROGRAM main
