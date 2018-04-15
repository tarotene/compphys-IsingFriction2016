  INCLUDE 'mkl_vsl.f90'

  MODULE mod_proc
     USE mod_global
     USE MKL_VSL_TYPE
     USE MKL_VSL
     USE IFPORT, ONLY:access
     IMPLICIT NONE
  CONTAINS
     SUBROUTINE constNewStream_SFMT19937(seed, stream)
        INTEGER(4), INTENT(in) :: seed
        TYPE(VSL_STREAM_STATE), INTENT(out) :: stream
        
        err = vslnewstream(stream, VSL_BRNG_SFMT19937, seed)
     END SUBROUTINE constNewStream_SFMT19937

     SUBROUTINE updateDRand_Uniform(stream, len_rand, lbound, rbound, rand)
        TYPE(VSL_STREAM_STATE), INTENT(in) :: stream
        INTEGER(4), INTENT(in) :: len_ran
        REAL(8), INTENT(in) :: lbound, rbound
        REAL(8), INTENT(out) :: rand(1:)
        
        err = vdrnguniform(VSL_RNG_METHOD_UNIFORM_STD, stream, len_rand, rand(1:), lbound, rbound)
     END SUBROUTINE updateDRand_Uniform

     SUBROUTINE updateDRand_Uniform(stream, len_rand, lbound, rbound, rand)
        TYPE(VSL_STREAM_STATE), INTENT(in) :: stream
        INTEGER(4), INTENT(in) :: len_ran
        REAL(8), INTENT(in) :: lbound, rbound
        REAL(8), INTENT(out) :: rand(1:)
        
        err = vdrnguniform(VSL_RNG_METHOD_UNIFORM_STD, stream, len_rand, rand(1:), lbound, rbound)
     END SUBROUTINE updateDRand_Uniform

     !TODO: 関数化
     SUBROUTINE inputParams_2d(l_x, l_z, beta, vel, l_t, id_IC, id_BC, n_s)
        INTEGER(4), INTENT(out) :: l_x, l_z
        REAL(8), INTENT(out) :: beta
        INTEGER(4), INTENT(out) :: vel, l_t, id_IC, id_BC, n_s

        READ (*, *) l_x, l_z, beta, vel, l_t, id_IC, id_BC, n_s
     END SUBROUTINE inputParams_2d

     SUBROUTINE inputParams_2d_eq(l_x, l_z, beta, l_t, id_IC, id_BC, n_s)
        INTEGER(4), INTENT(out) :: l_x, l_z
        REAL(8), INTENT(out) :: beta
        INTEGER(4), INTENT(out) :: l_t, id_IC, id_BC, n_s

        READ (*, *) l_x, l_z, beta, l_t, id_IC, id_BC, n_s
     END SUBROUTINE inputParams_2d_eq

     SUBROUTINE inputParams_3d(l_x, l_y, l_z, beta, vel, l_t, id_IC, id_BC, n_s)
        INTEGER(4), INTENT(out) :: l_x, l_y, l_z
        REAL(8), INTENT(out) :: beta
        INTEGER(4), INTENT(out) :: vel, l_t, id_IC, id_BC, n_s

        READ (*, *) l_x, l_y, l_z, beta, vel, l_t, id_IC, id_BC, n_s
     END SUBROUTINE inputParams_3d

     SUBROUTINE inputParams_3d_eq(l_x, l_y, l_z, beta, l_t, id_IC, id_BC, n_s)
        INTEGER(4), INTENT(out) :: l_x, l_y, l_z
        REAL(8), INTENT(out) :: beta
        INTEGER(4), INTENT(out) :: l_t, id_IC, id_BC, n_s

        READ (*, *) l_x, l_y, l_z, beta, l_t, id_IC, id_BC, n_s
     END SUBROUTINE inputParams_3d_eq

     SUBROUTINE paramsCorr_2d(l_x, l_z, beta, vel, l_t, n_s)
        INTEGER(4), INTENT(out) :: l_x, l_z
        REAL(8), INTENT(out) :: beta
        INTEGER(4), INTENT(out) :: vel, l_t, n_s

        READ (*, *) l_x, l_z, beta, vel, l_t, n_s
     END SUBROUTINE paramsCorr_2d

     SUBROUTINE paramsCorr_2d_eq(l_x, l_z, beta, l_t, n_s)
        INTEGER(4), INTENT(out) :: l_x, l_z
        REAL(8), INTENT(out) :: beta
        INTEGER(4), INTENT(out) :: l_t, n_s

        READ (*, *) l_x, l_z, beta, l_t, n_s
     END SUBROUTINE paramsCorr_2d_eq

     SUBROUTINE paramsCorr_3d(l_x, l_y, l_z, beta, vel, l_t, n_s)
        INTEGER(4), INTENT(out) :: l_x, l_y, l_z
        REAL(8), INTENT(out) :: beta
        INTEGER(4), INTENT(out) :: vel, l_t, n_s

        READ (*, *) l_x, l_y, l_z, beta, vel, l_t, n_s
     END SUBROUTINE paramsCorr_3d

     SUBROUTINE paramsCorr_3d_eq(l_x, l_y, l_z, beta, l_t, n_s)
        INTEGER(4), INTENT(out) :: l_x, l_y, l_z
        REAL(8), INTENT(out) :: beta
        INTEGER(4), INTENT(out) :: l_t, n_s

        READ (*, *) l_x, l_y, l_z, beta, l_t, n_s
     END SUBROUTINE paramsCorr_3d_eq

     SUBROUTINE paramsCalc_2d(l_x, l_z, beta, vel, l_t, n_s, l_th, l_b)
        INTEGER(4), INTENT(out) :: l_x, l_z
        REAL(8), INTENT(out) :: beta
        INTEGER(4), INTENT(out) :: vel, l_t, n_s, l_th, l_b

        READ (*, *) l_x, l_z, beta, vel, l_t, n_s, l_th, l_b
     END SUBROUTINE paramsCalc_2d

     SUBROUTINE paramsCalc_2d_eq(l_x, l_z, beta, l_t, n_s, l_th, l_b)
        INTEGER(4), INTENT(out) :: l_x, l_z
        REAL(8), INTENT(out) :: beta
        INTEGER(4), INTENT(out) :: l_t, n_s, l_th, l_b

        READ (*, *) l_x, l_z, beta, l_t, n_s, l_th, l_b
     END SUBROUTINE paramsCalc_2d_eq

     SUBROUTINE paramsCalc_3d(l_x, l_y, l_z, beta, vel, l_t, n_s, l_th, l_b)
        INTEGER(4), INTENT(out) :: l_x, l_y, l_z
        REAL(8), INTENT(out) :: beta
        INTEGER(4), INTENT(out) :: vel, l_t, n_s, l_th, l_b

        READ (*, *) l_x, l_y, l_z, beta, vel, l_t, n_s, l_th, l_b
     END SUBROUTINE paramsCalc_3d

     SUBROUTINE paramsCalc_3d_eq(l_x, l_y, l_z, beta, l_t, n_s, l_th, l_b)
        INTEGER(4), INTENT(out) :: l_x, l_y, l_z
        REAL(8), INTENT(out) :: beta
        INTEGER(4), INTENT(out) :: l_t, n_s, l_th, l_b

        READ (*, *) l_x, l_y, l_z, beta, l_t, n_s, l_th, l_b
     END SUBROUTINE paramsCalc_3d_eq

     ! TODO: 名前に因んで関数化
     SUBROUTINE metropolis_2d(beta, p_2d)
        REAL(8), INTENT(in) :: beta
        REAL(8), INTENT(out) :: p_2d(-1:1, -1:1, -1:1, -1:1, -1:1)

        INTEGER(4) :: dE(-1:1, -1:1, -1:1, -1:1, -1:1), c, e, w, s, n

        POSITIVE:FORALL (c=-1:1, e=-1:1, w=-1:1, s=-1:1, n=-1:1)
        p_2d(c, e, w, s, n) = EXP(-2.0d0*beta*DBLE(c*(e + w + s + n)))
        END FORALL POSITIVE
        WHERE (p_2d >= 1.0d0) p_2d = 1.0d0
     END SUBROUTINE metropolis_2d

     ! TODO: 名前に因んで関数化
     SUBROUTINE metropolis_3d(beta, p_3d)
        REAL(8), INTENT(in) :: beta
        REAL(8), INTENT(out) :: p_3d(-1:1, -1:1, -1:1, -1:1, -1:1, -1:1, -1:1)

        INTEGER(4) :: dE(-1:1, -1:1, -1:1, -1:1, -1:1, -1:1, -1:1), c, e, w, s, n, u, d

        POSITIVE:FORALL (c=-1:1, e=-1:1, w=-1:1, s=-1:1, n=-1:1, u=-1:1, d=-1:1)
        p_3d(c, e, w, s, n, u, d) = EXP(-2.0d0*beta*DBLE(c*(e + w + s + n + u + d)))
        END FORALL POSITIVE
        WHERE (p_3d >= 1.0d0) p_3d = 1.0d0
     END SUBROUTINE metropolis_3d

     ! TODO: 名前に因んで関数化
     SUBROUTINE orient_2d(sp, x, z, east, west, south, north)
        INTEGER(4), INTENT(in) :: sp(0:, 0:), x, z
        INTEGER(4), INTENT(out) :: east, west, south, north

        east = sp(MOD(x, l_x) + 1, z)
        IF (x == 1) THEN
           west = sp(l_x, z)
        ELSE
           west = sp(x - 1, z)
        END IF

        south = sp(x, z - 1)
        north = sp(x, z + 1)
     END SUBROUTINE orient_2d

     ! TODO: 名前に因んで関数化
     SUBROUTINE orient_3d(sp, x, y, z, east, west, south, north, up, down)
        INTEGER(4), INTENT(in) :: sp(0:, 0:, 0:), x, y, z
        INTEGER(4), INTENT(out) :: east, west, south, north, up, down

        east = sp(MOD(x, l_x) + 1, y, z)
        IF (x == 1) THEN
           west = sp(l_x, y, z)
        ELSE
           west = sp(x - 1, y, z)
        END IF

        south = sp(x, MOD(y, l_y) + 1, z)
        IF (y == 1) THEN
           north = sp(x, l_y, z)
        ELSE
           north = sp(x, y - 1, z)
        END IF

        up = sp(x, y, z - 1)
        down = sp(x, y, z + 1)
     END SUBROUTINE orient_3d

     SUBROUTINE calcEn_2d(sp, en)
        INTEGER(4), INTENT(in) :: sp(0:, 0:)
        INTEGER(4), INTENT(out) :: en

        INTEGER(4) :: x, z, east, west, south, north

        en = 0
        DO z = 1, l_z, 1
           DO x = 1, l_x, 1
              CALL orient_2d(sp, x, z, east, west, south, north)
              en = en - sp(x, z) * (east + west + south + north)
           END DO
        END DO
        DO x = 1, l_x, 1
          en = en - sp(x, 0) * sp(x, 1) - sp(x, l_z) * sp(x, l_z + 1)
        END DO
        en = en / 2
     END SUBROUTINE calcEn_2d

     SUBROUTINE calcEn_3d(sp, en)
        INTEGER(4), INTENT(in) :: sp(0:, 0:, 0:)
        INTEGER(4), INTENT(out) :: en

        INTEGER(4) :: x, y, z, east, west, south, north, up, down

        en = 0
        DO z = 1, l_z, 1
           DO y = 1, l_y, 1
              DO x = 1, l_x, 1
                 CALL orient_3d(sp, x, y, z, east, west, south, north, up, down)
                 en = en - sp(x, y, z) * (east + west + south + north + up + down)
              END DO
           END DO
        END DO
        DO y = 1, l_y, 1
          DO x = 1, l_x, 1
            en = en - sp(x, y, 0) * sp(x, y, 1) - sp(x, y, l_z) * sp(x, y, l_z + 1)
          END DO
        END DO
        en = en / 2
     END SUBROUTINE calcEn_3d

     SUBROUTINE calcEE_2d(sp, en)
        INTEGER(4), INTENT(in) :: sp(0:, 0:)
        INTEGER(4), INTENT(out) :: en

        INTEGER(4) :: x

        en = 0
        DO x = 1, l_x, 1
           en = en - sp(x, l_z / 2) * sp(x, l_z / 2 + 1)
        END DO
     END SUBROUTINE calcEE_2d

     SUBROUTINE calcEE_3d(sp, en)
        INTEGER(4), INTENT(in) :: sp(0:, 0:, 0:)
        INTEGER(4), INTENT(out) :: en

        INTEGER(4) :: x, y

        en = 0
        DO y = 1, l_y, 1
           DO x = 1, l_x, 1
              en = en - sp(x, y, l_z / 2) * sp(x, y, l_z / 2 + 1)
           END DO
        END DO
     END SUBROUTINE calcEE_3d

     SUBROUTINE initSp_2d(sp)
        INTEGER(4), INTENT(inout) :: sp(0:, 0:)

        INTEGER(4) :: err, z
        TYPE(VSL_STREAM_STATE) :: str_sp

        sp(0, 0:l_z + 1) = 0
        sp(l_x + 1, 0:l_z + 1) = 0

        SELECT CASE (id_BC)
        CASE (1) !BC: anti-parallel
           sp(1:l_x, 0) = 1
           sp(1:l_x, l_z + 1) = -1
        CASE (2) !BC: parallel
           sp(1:l_x, 0) = 1
           sp(1:l_x, l_z + 1) = 1
        CASE (3) !BC: free
           sp(1:l_x, 0) = 0
           sp(1:l_x, l_z + 1) = 0
        END SELECT

        SELECT CASE (id_IC)
        CASE (1)
           sp(1:l_x, 1:l_z/2) = 1
           sp(1:l_x, l_z/2 + 1:l_z) = -1
        CASE (2)
           sp(1:l_x, 1:l_z) = 1
        CASE (3)
           err = vslnewstream(str_sp, VSL_BRNG_MT19937, 50)
           DO z = 1, l_z, 1
              err = virnguniform(VSL_RNG_METHOD_UNIFORM_STD, str_sp, l_x, sp(1:l_x, z), 0, 2)
           END DO
           err = vsldeletestream(str_sp)
           sp(1:l_x, 1:l_z) = 2 * sp(1:l_x, 1:l_z) - 1
        END SELECT
     END SUBROUTINE initSp_2d

     SUBROUTINE initSp_3d(sp)
        INTEGER(4), INTENT(inout) :: sp(0:, 0:, 0:)

        INTEGER(4) :: err, x, y, z
        TYPE(VSL_STREAM_STATE) :: str_sp

        sp(0, 0, 0:l_z + 1) = 0
        sp(0, l_y + 1, 0:l_z + 1) = 0
        sp(l_x + 1, 0, 0:l_z + 1) = 0
        sp(l_x + 1, l_y + 1, 0:l_z + 1) = 0

        SELECT CASE (id_BC)
        CASE (1) !BC: anti-parallel
           sp(1:l_x, 1:l_y, 0) = 1
           sp(1:l_x, 1:l_y, l_z + 1) = -1
        CASE (2) !BC: parallel
           sp(1:l_x, 1:l_y, 0) = 1
           sp(1:l_x, 1:l_y, l_z + 1) = 1
        CASE (3) !BC: free
           sp(1:l_x, 1:l_y, 0) = 0
           sp(1:l_x, 1:l_y, l_z + 1) = 0
        END SELECT

        SELECT CASE (id_IC)
        CASE (1)
           sp(1:l_x, 1:l_y, 1:l_z/2) = 1
           sp(1:l_x, 1:l_y, l_z/2 + 1:l_z) = -1
        CASE (2)
           sp(1:l_x, 1:l_y, 1:l_z) = 1
        CASE (3)
           err = vslnewstream(str_sp, VSL_BRNG_MT19937, 50)
           DO z = 1, l_z, 1
              DO y = 1, l_y, 1
                 err = virnguniform(VSL_RNG_METHOD_UNIFORM_STD, str_sp, l_x, sp(1:l_x, y, z), 0, 2)
              END DO
           END DO
           err = vsldeletestream(str_sp)
           sp(1:l_x, 1:l_y, 1:l_z) = 2 * sp(1:l_x, 1:l_y, 1:l_z) - 1
        END SELECT
     END SUBROUTINE initSp_3d

     SUBROUTINE SSF_2d(x, z, p, sp, eb_prv, eb_nxt, mb_prv, mb_nxt)
        INTEGER(4), INTENT(in) :: x, z
        REAL(8), INTENT(in) :: p
        INTEGER(4), INTENT(inout) :: sp(0:, 0:)
        INTEGER(4), INTENT(in) :: eb_prv, mb_prv
        INTEGER(4), INTENT(out) :: eb_nxt, mb_nxt

        INTEGER(4) :: east, west, south, north

        CALL orient_2d(sp(0:l_x + 1, 0:l_z + 1), x, z, east, west, south, north)

        IF (p <= p_2d(sp(x, z), east, west, south, north)) THEN
           eb_nxt = eb_prv + 2 * sp(x, z) * (east + west + south + north)
           mb_nxt = mb_prv - 2 * sp(x, z)
           sp(x, z) = - sp(x, z)
        ELSE
           eb_nxt = eb_prv
           mb_nxt = mb_prv
        END IF
     END SUBROUTINE SSF_2d

     SUBROUTINE SSF_3d(x, y, z, p, sp, eb_prv, eb_nxt, mb_prv, mb_nxt)
        INTEGER(4), INTENT(in) :: x, y, z
        REAL(8), INTENT(in) :: p
        INTEGER(4), INTENT(inout) :: sp(0:, 0:, 0:)
        INTEGER(4), INTENT(in) :: eb_prv, mb_prv
        INTEGER(4), INTENT(out) :: eb_nxt, mb_nxt

        INTEGER(4) :: east, west, south, north, up, down

        CALL orient_3d(sp(0:, 0:, 0:), x, y, z, east, west, south, north, up, down)

        IF (p <= p_3d(sp(x, y, z), east, west, south, north, up, down)) THEN
           eb_nxt = eb_prv + 2 * sp(x, y, z) * (east + west + south + north + up + down)
           mb_nxt = mb_prv - 2 * sp(x, y, z)
           sp(x, y, z) = - sp(x, y, z)
        ELSE
           eb_nxt = eb_prv
           mb_nxt = mb_prv
        END IF
     END SUBROUTINE SSF_3d

     SUBROUTINE mSSFs_2d(r_x, r_z, r_p, sp, eb, mb)
        INTEGER(4), INTENT(in) :: r_x(1:), r_z(1:)
        REAL(8), INTENT(in) :: r_p(1:)
        INTEGER(4), INTENT(inout) :: sp(0:, 0:), eb(0:), mb(0:)

        INTEGER(4) :: i_st, rel

        DO i_st = 1, n_st, 1
           CALL SSF_2d(r_x(i_st), r_z(i_st), r_p(i_st), sp(0:, 0:), eb(i_st - 1), eb(i_st), mb(i_st - 1), mb(i_st))
        END DO
     END SUBROUTINE mSSFs_2d

     SUBROUTINE mSSFs_3d(r_x, r_y, r_z, r_p, sp, eb, mb)
        INTEGER(4), INTENT(in) :: r_x(1:), r_y(1:), r_z(1:)
        REAL(8), INTENT(in) :: r_p(1:)
        INTEGER(4), INTENT(inout) :: sp(0:, 0:, 0:), eb(0:), mb(0:)

        INTEGER(4) :: i_st, rel

        DO i_st = 1, n_st, 1
           CALL SSF_3d(r_x(i_st), r_y(i_st), r_z(i_st), r_p(i_st), sp(0:, 0:, 0:), eb(i_st - 1), eb(i_st), mb(i_st - 1), mb(i_st))
        END DO
     END SUBROUTINE mSSFs_3d

     SUBROUTINE shift_2d(sp, pmp, eb)
        INTEGER(4), INTENT(inout) :: sp(0:, 0:), eb
        INTEGER(4), INTENT(out) :: pmp

        INTEGER(4) :: eb_prv, eb_nxt

        CALL calcEE_2d(sp(0:, 0:), eb_prv)
        sp(1:l_x, 1:l_z/2) = CSHIFT(sp(1:l_x, 1:l_z/2), 1, 1)
        CALL calcEE_2d(sp(0:, 0:), eb_nxt)
        pmp = eb_nxt - eb_prv
        eb = eb + pmp
     END SUBROUTINE shift_2d

     SUBROUTINE shift_3d(sp, pmp, eb)
        INTEGER(4), INTENT(inout) :: sp(0:, 0:, 0:), eb
        INTEGER(4), INTENT(out) :: pmp

        INTEGER(4) :: eb_prv, eb_nxt

        CALL calcEE_3d(sp(0:, 0:, 0:), eb_prv)
        sp(1:l_x, 1:l_y, 1:l_z/2) = CSHIFT(sp(1:l_x, 1:l_y, 1:l_z/2), 1, 1)
        CALL calcEE_3d(sp(0:, 0:, 0:), eb_nxt)
        pmp = eb_nxt - eb_prv
        eb = eb + pmp
     END SUBROUTINE shift_3d

     SUBROUTINE calcAC(a, ac)
        INTEGER(4), INTENT(in) :: a(1:)
        REAL(8), INTENT(out) :: ac(1:)

        INTEGER(4) :: t, t_, l_t

        ac(1:l_t/2) = 0.0d0
        DO t = 1, l_t/2, 1
           DO t_ = 1, l_t - t, 1
              ac(t) = ac(t) + a(t_)*a(t_ + t)
           END DO
           ac(t) = ac(t)/DBLE(l_t - t) - DBLE(SUM(a(1:l_t)))/DBLE(l_t)
        END DO
     END SUBROUTINE calcAC

     SUBROUTINE countSamples(n_s0)
        INTEGER(4), INTENT(out) :: n_s0
        CHARACTER(4) :: ss

        INTEGER(4) :: s, st_sp

        DO s = 1, 5000000, 1
           WRITE (ss, '(i0.4)') s
           st_sp = access("eb_sweep/en_bulk_s"//ss//"_sweep.bin", " ")
           IF (st_sp > 0) THEN
              n_s0 = s - 1; EXIT
           END IF
        END DO
     END SUBROUTINE countSamples

     PURE subroutine calcAvgVar(n_s, sample, avg, var_sq)
        integer(4), intent(in) :: n_s
        real(8), intent(in) :: sample(1:n_s)
        real(8), intent(out) :: avg, var_sq

        real(8) :: sq(1:n_s)

        avg = SUM(sample(1:n_s))/DBLE(n_s)
        sq(1:n_s) = sample(1:n_s)**2
        var_sq = (SUM(sq(1:n_s))/DBLE(n_s) - avg**2)/DBLE(n_s - 1)
     end subroutine calcAvgVar

     PURE subroutine convertStreamSamples(l_th, n_s, stream, sample)
        integer(4), intent(in) ::  l_th, n_s
        real(8), intent(in) :: stream(1:)
        real(8), intent(out) :: sample(1:)

        integer(4) :: s, l_s

        l_s = (SIZE(stream) - l_th)/n_s
        DO CONCURRENT(s=1:n_s:1)
           sample(s) = SUM(stream(1 + l_s*(s - 1):l_s*s))/DBLE(l_s)
        END DO
     end subroutine convertStreamSamples
  END MODULE mod_proc
