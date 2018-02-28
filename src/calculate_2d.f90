PROGRAM main

  IMPLICIT NONE

  s = 1
  WRITE(ss, '(i0.4)') s

  ! adjustment program to machine
  n_ths = 1
  !$  n_ths = omp_get_max_threads()
  !$  CALL omp_set_num_threads(n_ths)
  !allocation slots
  ALLOCATE(sl_spin(0:n_ths - 1))
  ! setting slots
  DO i_th = 0, n_ths - 1
     sl_spin(i_th) = 20 + i_th + n_ths
  END DO

  DO CONCURRENT (t = 1:len_t)
     i_th = 0
     !$ i_th = omp_get_thread_num()

     WRITE(st, '(i0.4)') t
     fl_spin="spin_t"//st//"s"//ss//".bin"
     CALL importSpin_2d(sl_spin(i_th), fl_spin, spin(1:len_x, 1:len_z))

     DO CONCURRENT (z = 1:len_z)
        m_pr(z, t) = DBLE(SUM(spin(1:len_x, z)))
     END DO
     m_pr(z, t) = m_pr(z, t) / DBLE(len_x)

     en_pr(1:len_z, t) = 0.0d0
     DO CONCURRENT (z = 1:len_z)
        DO x = 1, len_x - 1, 1
           en_pr(z, t) = en_pr(z, t) + DBLE(spin(x, z) * spin(x + 1, z))
        END DO
        en_pr(z, t) = en_pr(z, t) + DBLE(spin(1, z) * spin(len_x, z))
     END DO
     en_pr(1:len_z, t) = en_pr(1:len_z, t) / DBLE(len_x)

     c_zb1(1:len_x / 2, t) = 0.0d0
     DO CONCURRENT (x = 1:len_x / 2)
        DO x_ = 1, len_x - x
           c_zb1(x, t) = c_zb1(x, t) + DBLE(spin(x_, len_z / 2) * spin(x_ + x, len_z / 2))
        END DO
        c_zb1(x, t) = c_zb1(x, t) / DBLE(len_x - x)
     END DO
     c_zb1(1:len_x / 2) = c_zb1(1:len_x / 2) - m_pr(len_z / 2) ** 2

     c_zb2(1:len_x / 2, t) = 0.0d0
     DO CONCURRENT (x = 1:len_x / 2)
        DO x_ = 1, len_x - x
           c_zb2(x, t) = c_zb2(x, t) + DBLE(spin(x_, len_z / 2 + 1) * spin(x_ + x, len_z / 2 + 1))
        END DO
        c_zb2(x, t) = c_zb2(x, t) / DBLE(len_x - x)
     END DO
     c_zb2(1:len_x / 2, t) = c_zb2(1:len_x / 2, t) - m_pr(len_z / 2 + 1) ** 2
  END DO

  DEALLOCATE(spin)
END PROGRAM main
