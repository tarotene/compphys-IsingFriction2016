PROGRAM main
  !$  USE omp_lib
  USE mod_global
  USE mod_proc
  USE IFPORT, ONLY: access

  IMPLICIT NONE

  INTEGER(kind = 4), ALLOCATABLE :: sp4(:, :), sp_ini4(:, :)
  INTEGER(kind = 4) :: en, eb, ee, mb, me

  INTEGER(kind = 4) :: seed_master, seed_x, seed_z, seed_p, n_st, err
  INTEGER(kind = 4) :: i_st, i_v, t, s
  INTEGER(kind = 4), ALLOCATABLE :: r_x(:), r_z(:)
  INTEGER(kind = 4), ALLOCATABLE :: sl_sp(:, :), sl_en(:)
  INTEGER(kind = 4), ALLOCATABLE :: sl_eb(:), sl_ee(:), sl_mb(:), sl_me(:)
  REAL(kind = 8), ALLOCATABLE :: r_p(:)
  TYPE(VSL_STREAM_STATE), ALLOCATABLE :: str_x(:), str_z(:), str_p(:)
  CHARACTER(len = 4, kind = 1) :: st, ss
  CHARACTER(:), ALLOCATABLE :: fn_sp, fn_en

  CALL importParams_2d(l_x, l_z, beta, vel, l_t, id_IC, id_BC, n_s)

  CALL countSamples(n_s, n_s0)

  CALL makePr_2d(beta, pr_2d(-1:1, -1:1, -1:1, -1:1, -1:1))

  ALLOCATE(sl_en(n_s0 + 1:n_s))
  DO CONCURRENT (s = 1:n_s - n_s0:1)
     sl_en(s) = 20 + s + 0 * (n_s - n_s0)
  END DO

  ALLOCATE(sl_eb(n_s0 + 1:n_s), sl_ee(n_s0 + 1:n_s))
  ALLOCATE(sl_mb(n_s0 + 1:n_s), sl_me(n_s0 + 1:n_s))
  DO CONCURRENT (s = 1:n_s - n_s0:1)
     sl_eb(s) = 20 + s + 1 * (n_s - n_s0); sl_ee(s) = 20 + s + 2 * (n_s - n_s0)
     sl_mb(s) = 20 + s + 3 * (n_s - n_s0); sl_me(s) = 20 + s + 4 * (n_s - n_s0)
  END DO

  ALLOCATE(sl_sp(1:n_s - n_s0, 1:l_t))
  FORALL (s = 1:n_s - n_s0:1, t = 1:l_t:1)
     sl_sp(s, t) = 20 + 5 * (n_s - n_s0) + (t - 1) * (n_s - n_s0) + s - 1
  END FORALL

  DO s = n_s0 + 1, n_s, 1
     WRITE(ss, '(i0.4)') s
     OPEN(sl_en(s), file="en_s"//ss//"_step.bin", access="stream", status="new")
     OPEN(sl_eb(s), file="en_bulk_s"//ss//"_sweep.bin", access="stream", status="new")
     OPEN(sl_ee(s), file="en_edge_s"//ss//"_sweep.bin", access="stream", status="new")
     OPEN(sl_mb(s), file="m_bulk_s"//ss//"_sweep.bin", access="stream", status="new")
     OPEN(sl_me(s), file="m_edge_s"//ss//"_sweep.bin", access="stream", status="new")
     DO t = 1, l_t, 1
        WRITE(st, '(i0.4)') t
        OPEN(sl_sp(s, t), file="sp_t"//st//"s"//ss//".bin", access="stream", status="new", buffered="YES")
     END DO
  END DO

  ALLOCATE(str_x(n_s0 + 1:n_s), str_z(n_s0 + 1:n_s), str_p(n_s0 + 1:n_s))
  ALLOCATE(r_x(1:l_x * l_z), r_z(1:l_x * l_z), r_p(1:l_x * l_z))
  ALLOCATE(sp4(1:l_x, 1:l_z), sp_ini4(1:l_x, 1:l_z))

  seed_master = 100
  DO s = n_s0 + 1, n_s, 1
     err = vslnewstream(str_x(s), VSL_BRNG_SFMT19937, seed_master + 4 * (s - 1))
     err = vslnewstream(str_z(s), VSL_BRNG_SFMT19937, seed_master + 4 * (s - 1) + 2)
     err = vslnewstream(str_p(s), VSL_BRNG_SFMT19937, seed_master + 4 * (s - 1) + 3)
  END DO

  CALL initSp_2d(50, sp_ini4(1:l_x, 1:l_z))
  n_st = l_x * l_z / vel

  !$omp parallel do schedule(static, 1) default(none) &
  !$omp shared(n_s0, n_s, l_t, id_BC, l_x, l_z, vel) &
  !$omp shared(str_x, str_z, str_p, sl_sp, sl_en, sl_eb, sl_ee, sl_mb, sl_me, sp_ini4) &
  !$omp private(err, r_x, r_z, r_p, fn_sp, fn_en, ss, st, n_st, sp4, en, ee)
  DO s = n_s0 + 1, n_s, 1
     sp4(1:l_x, 1:l_z) = sp_ini4(1:l_x, 1:l_z); CALL calcEn_2d(sp4(1:l_x, 1:l_z), en)

     DO t = 1, l_t, 1
        DO i_v = 1, vel, 1
           CALL shift_2d(sl_en(s), sl_eb(s), t, i_v, sp4(1:l_x, 1:l_z), en)
           err = virnguniform(VSL_RNG_METHOD_UNIFORM_STD, str_x(s), n_st, r_x(1:n_st), 1, l_x + 1)
           err = virnguniform(VSL_RNG_METHOD_UNIFORM_STD, str_z(s), n_st, r_z(1:n_st), 1, l_z + 1)
           err = vdrnguniform(VSL_RNG_METHOD_UNIFORM_STD, str_p(s), n_st, r_p(1:n_st), 0.0d0, 1.0d0)
           CALL mSSFs_2d(sl_en(s), t, i_v, n_st, r_x(1:n_st), r_z(1:n_st), r_p(1:n_st), sp4(1:l_x, 1:l_z), en)
        END DO
        WRITE(sl_sp(s, t)) INT1(sp4(1:l_x, 1:l_z))

        WRITE(sl_eb(s)) en; CALL calcEE_2d(sp4(1:, 1:), ee); WRITE(sl_ee(s)) ee
        WRITE(sl_mb(s)) SUM(sp4(1:l_x, 1:l_z)); WRITE(sl_me(s)) SUM(sp4(1:l_x, l_z / 2:l_z / 2 + 1))
     END DO
  END DO
  !$omp end parallel do

  DO s = n_s0 + 1, n_s, 1
     CLOSE(sl_en(s))
     DO t = 1, l_t, 1
        CLOSE(sl_sp(s, t))
     END DO
  END DO

  !$omp parallel do schedule(dynamic, 1) default(none) &
  !$omp shared(n_s0, n_s, str_x, str_z, str_p) &
  !$omp private(err)
  DO s = n_s0 + 1, n_s, 1
     err = vsldeletestream(str_x(s)); err = vsldeletestream(str_z(s)); err = vsldeletestream(str_p(s))
  END DO
  !$omp end parallel do
END PROGRAM main
