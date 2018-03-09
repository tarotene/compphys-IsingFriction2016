PROGRAM main

  IMPLICIT NONE

  INTEGER(kind = 4), ALLOCATABLE :: sl_spin(:, :), sl_en(:)

  CALL importParams_2d(len_x, len_z, beta, vel, len_t, id_IC, id_BC, len_s)

  ALLOCATE(sl_en(1:len_s))
  FORALL (s=1:len_s:1)
     sl_en(s) = 20 + s - 1
  END FORALL
  ALLOCATE(sl_spin(1:len_s, 1:len_t))
  FORALL (s=1:len_s:1, t=1:len_t:1)
     sl_spin(s, t) = 20 + len_s + (t - 1) * len_s + s - 1
  END FORALL

  DO s = 1, len_s, 1
     WRITE(ss, '(i0.4)') s
     OPEN(sl_en(s), file="en_s"//ss//"_step.bin", access="stream", status="old")
     DO t = 1, len_t, 1
        WRITE(st, '(i0.4)') t
        OPEN(sl_spin(s, t), file="spin_t"//st//"s"//ss//".bin", access="stream", status="old", buffered="YES")
     END DO
  END DO

  !$omp parallel do schedule(static, 1) default(none) &
  !$omp shared(len_s, len_t, len_x, len_z) &
  !$omp shared(sl_spin, sl_en) &
  !$omp private(ss, st, spin1, M_bulk, M_edge, EN_bulk, EN_edge, loc_en)
  DO s = 1, len_s, 1
     loc_en = 1
     WRITE(ss, '(i0.4)') s
     DO CONCURRENT (t = 1:len_t)
        WRITE(st, '(i0.4)') t
        READ(sl_spin(s, t)) spin1(1:len_x, 1:len_z)
        M_bulk(t) = SUM(spin1(1:len_x, 1:len_z))
        M_edge(t) = SUM(spin1(1:len_x, len_z / 2:len_z / 2 + 1))
        CALL calcEn_2d(INT4(spin1(1:len_x, 1:len_z)), EN_bulk)
        CALL calcBoundEn_2d(INT4(spin1(1:len_x, 1:len_z)), EN_edge)

        read(n_file_in, pos=loc_en) EN
        loc = loc +len_int
     END DO
  END DO

  DO s = 1, len_s, 1
     CLOSE(sl_en(s))
     DO t = 1, len_t, 1
        CLOSE(sl_spin(s, t))
     END DO
  END DO
END PROGRAM main
