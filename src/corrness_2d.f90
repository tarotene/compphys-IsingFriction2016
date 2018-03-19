PROGRAM main
  USE mod_global
  USE MKL_VSL_TYPE
  USE MKL_VSL
  USE mod_proc
  IMPLICIT NONE

  INTEGER(4) :: sl_eb, sl_ee, sl_mb, sl_me
  INTEGER(4), ALLOCATABLE :: eb(:, :), ee(:, :), mb(:, :), me(:, :)
  INTEGER(4), ALLOCATABLE :: amb(:, :), ame(:, :)
  INTEGER(4), ALLOCATABLE :: eb_sq(:, :), mb_sq(:, :), ee_sq(:, :), me_sq(:, :)
  REAL(4), ALLOCATABLE :: ac_eb(:, :), ac_amb(:, :), ac_ee(:, :), ac_ame(:, :)
  REAL(4), ALLOCATABLE :: m_eb(:), m_ee(:), m_amb(:), m_ame(:)
  REAL(4), ALLOCATABLE :: m_eb_sq(:), m_ee_sq(:), m_mb_sq(:), m_me_sq(:)
  INTEGER(4) :: s, t, loc, t_
  CHARACTER(4) :: ss

  CALL paramsCorrelate_2d(l_x, l_z, beta, vel, l_t, n_s)

  ALLOCATE(eb(1:n_s, 1:l_t), ee(1:n_s, 1:l_t), mb(1:n_s, 1:l_t), me(1:n_s, 1:l_t))
  ALLOCATE(amb(1:n_s, 1:l_t), ame(1:n_s, 1:l_t))
  ALLOCATE(eb_sq(1:n_s, 1:l_t), ee_sq(1:n_s, 1:l_t), mb_sq(1:n_s, 1:l_t), me_sq(1:n_s, 1:l_t))
  ALLOCATE(ac_eb(1:n_s, 1:l_t / 2), ac_ee(1:n_s, 1:l_t / 2), ac_amb(1:n_s, 1:l_t / 2), ac_ame(1:n_s, 1:l_t / 2))

  loc = 1
  DO s = 1, n_s, 1
     sl_eb = 20 + s + 2 * n_s; sl_mb = 20 + s + 3 * n_s
     sl_ee = 20 + s + 4 * n_s; sl_me = 20 + s + 5 * n_s

     WRITE(ss, '(i0.4)') s
     OPEN(sl_eb, file="eb_sweep/en_bulk_s"//ss//"_sweep.bin", access="stream", status="old", buffered="YES")
     OPEN(sl_ee, file="ee_sweep/en_edge_s"//ss//"_sweep.bin", access="stream", status="old", buffered="YES")
     OPEN(sl_mb, file="mb_sweep/m_bulk_s"//ss//"_sweep.bin", access="stream", status="old", buffered="YES")
     OPEN(sl_me, file="me_sweep/m_edge_s"//ss//"_sweep.bin", access="stream", status="old", buffered="YES")

     DO t = 1, l_t, 1
        READ(sl_eb, pos=loc) eb(s, t); READ(sl_mb, pos=loc) mb(s, t)
        READ(sl_ee, pos=loc) ee(s, t); READ(sl_me, pos=loc) me(s, t)
        loc = loc + 4
     END DO

     CLOSE(sl_eb); CLOSE(sl_mb); CLOSE(sl_ee); CLOSE(sl_me)
  END DO

  eb_sq(1:n_s, 1:l_t) = eb(1:n_s, 1:l_t) ** 2
  ee_sq(1:n_s, 1:l_t) = ee(1:n_s, 1:l_t) ** 2
  amb(1:n_s, 1:l_t) = ABS(mb(1:n_s, 1:l_t))
  ame(1:n_s, 1:l_t) = ABS(me(1:n_s, 1:l_t))
  mb_sq(1:n_s, 1:l_t) = amb(1:n_s, 1:l_t) ** 2
  me_sq(1:n_s, 1:l_t) = ame(1:n_s, 1:l_t) ** 2

  ALLOCATE(m_eb(1:n_s), m_ee(1:n_s), m_amb(1:n_s), m_ame(1:n_s))
  ALLOCATE(m_eb_sq(1:n_s), m_ee_sq(1:n_s), m_mb_sq(1:n_s), m_me_sq(1:n_s))

  ac_eb(1:n_s, 1:l_t / 2) = 0.0; ac_ee(1:n_s, 1:l_t / 2) = 0.0
  ac_amb(1:n_s, 1:l_t / 2) = 0.0; ac_ame(1:n_s, 1:l_t / 2) = 0.0

  DO CONCURRENT (s = 1:n_s:1)
     m_eb(s) = SUM(eb(s, 1:l_t)) / REAL(l_t)
     m_ee(s) = SUM(ee(s, 1:l_t)) / REAL(l_t)
     m_amb(s) = SUM(amb(s, 1:l_t)) / REAL(l_t)
     m_ame(s) = SUM(ame(s, 1:l_t)) / REAL(l_t)

     DO CONCURRENT (t = 1:l_t / 2:1)
        DO t_ = 1, l_t - t, 1
           ac_eb(s, t) = ac_eb(s, t) + (REAL(eb(s, t_)) - m_eb(s)) * (REAL(eb(s, t_ + t)) - m_eb(s))
           ac_ee(s, t) = ac_ee(s, t) + (REAL(ee(s, t_)) - m_ee(s)) * (REAL(ee(s, t_ + t)) - m_ee(s))
           ac_amb(s, t) = ac_amb(s, t) + (REAL(amb(s, t_)) - m_amb(s)) * (REAL(amb(s, t_ + t)) - m_amb(s))
           ac_ame(s, t) = ac_ame(s, t) + (REAL(ame(s, t_)) - m_ame(s)) * (REAL(ame(s, t_ + t)) - m_ame(s))
        END DO
        ac_eb(s, t) = ac_eb(s, t) / REAL(l_t - t); ac_ee(s, t) = ac_ee(s, t) / REAL(l_t - t)
        ac_amb(s, t) = ac_amb(s, t) / REAL(l_t - t); ac_ame(s, t) = ac_ame(s, t) / REAL(l_t - t)
     END DO
     IF (ac_eb(s, 1) >= epsilon(0.0)) ac_eb(s, 1:l_t / 2) = ac_eb(s, 1:l_t / 2) / ac_eb(s, 1)
     IF (ac_ee(s, 1) >= epsilon(0.0)) ac_ee(s, 1:l_t / 2) = ac_ee(s, 1:l_t / 2) / ac_ee(s, 1)
     IF (ac_amb(s, 1) >= epsilon(0.0)) ac_amb(s, 1:l_t / 2) = ac_amb(s, 1:l_t / 2) / ac_amb(s, 1)
     IF (ac_ame(s, 1) >= epsilon(0.0)) ac_ame(s, 1:l_t / 2) = ac_ame(s, 1:l_t / 2) / ac_ame(s, 1)
  END DO

  !TODO: 空間的な相関関数の計算を行う

  DO s = 1, n_s, 1
     sl_eb = 20 + s + 2 * n_s; sl_mb = 20 + s + 3 * n_s
     sl_ee = 20 + s + 4 * n_s; sl_me = 20 + s + 5 * n_s

     WRITE(ss, '(i0.4)') s
     OPEN(sl_eb, file="ac_en_bulk_s"//ss//".bin", status="replace", access="stream", buffered="YES")
     OPEN(sl_ee, file="ac_en_edge_s"//ss//".bin", status="replace", access="stream", buffered="YES")
     OPEN(sl_mb, file="ac_m_bulk_s"//ss//".bin", status="replace", access="stream", buffered="YES")
     OPEN(sl_me, file="ac_m_edge_s"//ss//".bin", status="replace", access="stream", buffered="YES")

     WRITE(sl_eb) ac_eb(s, 1:l_t); WRITE(sl_mb) ac_amb(s, 1:l_t)
     WRITE(sl_ee) ac_ee(s, 1:l_t); WRITE(sl_me) ac_ame(s, 1:l_t)

     CLOSE(sl_eb); CLOSE(sl_mb)
     CLOSE(sl_ee); CLOSE(sl_me)
  END DO
END PROGRAM main
