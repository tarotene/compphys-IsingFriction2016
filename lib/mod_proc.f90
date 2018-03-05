  INCLUDE 'mkl_vsl.f90'

  MODULE mod_proc
    USE mod_global
    USE MKL_VSL_TYPE
    USE MKL_VSL
    USE IFPORT, ONLY: access
    IMPLICIT NONE
  CONTAINS
    SUBROUTINE importParams_2d(l_x, l_z, beta, vel, l_t, id_IC, id_BC, n_s)
      INTEGER(kind = 4), INTENT(out) :: l_x, l_z
      REAL(kind = 8), INTENT(out) :: beta
      INTEGER(kind = 4), INTENT(out) :: vel, l_t, id_IC, id_BC, n_s

      READ(*, *) l_x, l_z, beta, vel, l_t, id_IC, id_BC, n_s
    END SUBROUTINE importParams_2d

    SUBROUTINE importParams_3d(l_x, l_y, l_z, beta, vel, l_t, id_IC, id_BC, n_s)
      INTEGER(kind = 4), INTENT(out) :: l_x, l_y, l_z
      REAL(kind = 8), INTENT(out) :: beta
      INTEGER(kind = 4), INTENT(out) :: vel, l_t, id_IC, id_BC, n_s

      READ(*, *) l_x, l_y, l_z, beta, vel, l_t, id_IC, id_BC, n_s
    END SUBROUTINE importParams_3d

    SUBROUTINE makePr_2d(beta, pr)
      REAL(kind = 8), INTENT(in) :: beta
      REAL(kind = 8), INTENT(out) :: pr(-1:1, -1:1, -1:1, -1:1, -1:1)

      INTEGER(kind = 4) :: dE(-1:1, -1:1, -1:1, -1:1,-1:1), c, e, w, s, n

      FORALL (c=-1:1:1, e=-1:1:1, w=-1:1:1, s=-1:1:1, n=-1:1:1)
         dE(c, e, w, s, n) = 2 * (c * (e + w + s + n))
      END FORALL
      POSITIVE: FORALL (c=-1:1:1,e=-1:1:1,w=-1:1:1,s=-1:1:1,n=-1:1:1,dE(c,e,w,s,n)>0)
         pr(c,e,w,s,n) = EXP(- beta * DBLE(dE(c,e,w,s,n)))
      END FORALL POSITIVE
    END SUBROUTINE makePr_2d

    SUBROUTINE makePr_3d(beta, pr)
      REAL(kind = 8), INTENT(in) :: beta
      REAL(kind = 8), INTENT(out) :: pr(-1:1, -1:1, -1:1, -1:1, -1:1, -1:1, -1:1)

      INTEGER(kind = 4) :: dE(-1:1, -1:1, -1:1, -1:1, -1:1, -1:1, -1:1), c, e, w, s, n, u, d

      FORALL (c=-1:1:1, e=-1:1:1, w=-1:1:1, s=-1:1:1, n=-1:1:1, u=-1:1:1, d=-1:1:1)
         dE(c, e, w, s, n, u, d) = 2 * (c * (e + w + s + n + u + d))
      END FORALL
      POSITIVE: FORALL (c=-1:1,e=-1:1,w=-1:1,s=-1:1,n=-1:1,u=-1:1,d=-1:1,dE(c,e,w,s,n,u,d)>0)
         pr(c,e,w,s,n,u,d) = EXP(- beta * DBLE(dE(c,e,w,s,n,u,d)))
      END FORALL POSITIVE
    END SUBROUTINE makePr_3d

    ! TODO: 名前に因んで関数化
    SUBROUTINE orient_2d(sp4, x, z, east, west, south, north)
      INTEGER(kind = 4), INTENT(in) :: sp4(1:, 1:), x, z
      INTEGER(kind = 4), INTENT(out) :: east, west, south, north

      IF ( x == l_x ) THEN
         east = sp4(1, z)
      ELSE
         east = sp4(x + 1, z)
      END IF
      IF ( x == 1 ) THEN
         west = sp4(l_x, z)
      ELSE
         west = sp4(x - 1, z)
      END IF

      SELECT CASE (id_BC)
      CASE (1) !BC: anti-parallel
         IF ( z == 1 ) THEN
            south = 1
         ELSE
            south = sp4(x, z - 1)
         END IF
         IF ( z == l_z ) THEN
            north = -1
         ELSE
            north = sp4(x, z + 1)
         END IF
      CASE (2) !BC: parallel
         IF ( z == 1 ) THEN
            south = 1
         ELSE
            south = sp4(x, z - 1)
         END IF
         IF ( z == l_z ) THEN
            north = 1
         ELSE
            north = sp4(x, z + 1)
         END IF
      CASE (3) !BC: free
         IF ( z == 1 ) THEN
            south = 0
         ELSE
            south = sp4(x, z - 1)
         END IF
         IF ( z == l_z ) THEN
            north = 0
         ELSE
            north = sp4(x, z + 1)
         END IF
      END SELECT
    END SUBROUTINE orient_2d

    ! TODO: 名前に因んで関数化
    SUBROUTINE orient_3d(sp4, x, y, z, east, west, south, north, up, down)
      INTEGER(kind = 4), INTENT(in) :: sp4(1:, 1:, 1:), x, y, z
      INTEGER(kind = 4), INTENT(out) :: east, west, south, north, up, down

      IF ( x == l_x ) THEN
         east = sp4(1, y, z)
      ELSE
         east = sp4(x + 1, y, z)
      END IF
      IF ( x == 1 ) THEN
         west = sp4(l_x, y, z)
      ELSE
         west = sp4(x - 1, y, z)
      END IF

      IF ( y == l_y ) THEN
         north = sp4(x, 1, z)
      ELSE
         north = sp4(x, y + 1, z)
      END IF
      IF ( y == 1 ) THEN
         south = sp4(x, l_y, z)
      ELSE
         south = sp4(x, y - 1, z)
      END IF

      SELECT CASE (id_BC)
      CASE (1) !z-BC: anti-parallel
         IF ( z == 1 ) THEN
            down = 1
         ELSE
            down = sp4(x, y, z - 1)
         END IF
         IF ( z == l_z ) THEN
            up = -1
         ELSE
            up = sp4(x, y, z + 1)
         END IF
      CASE (2) !z-BC: parallel
         IF ( z == 1 ) THEN
            down = 1
         ELSE
            down = sp4(x, y, z - 1)
         END IF
         IF ( z == l_z ) THEN
            up = 1
         ELSE
            up = sp4(x, y, z + 1)
         END IF
      CASE (3) !z-BC: free
         IF ( z == 1 ) THEN
            down = 0
         ELSE
            down = sp4(x, y, z - 1)
         END IF
         IF ( z == l_z ) THEN
            up = 0
         ELSE
            up = sp4(x, y, z + 1)
         END IF
      END SELECT
    END SUBROUTINE orient_3d

    SUBROUTINE calcEn_2d(sp4, en)
      INTEGER(kind = 4), INTENT(in) :: sp4(1:, 1:)
      INTEGER(kind = 4), INTENT(out) :: en

      INTEGER(kind = 4) :: x, z, east, west, south, north

      en = 0
      DO z = 1, l_z
         DO x = 1, l_x
            CALL orient_2d(sp4, x, z, east, west, south, north)
            en = en - sp4(x,z) * (east + west + south + north)
         END DO
      END DO
      en = en / 2
    END SUBROUTINE calcEn_2d

    SUBROUTINE calcEn_3d(sp4, en)
      INTEGER(kind = 4), INTENT(in) :: sp4(1:, 1:, 1:)
      INTEGER(kind = 4), INTENT(out) :: en

      INTEGER(kind = 4) :: x, y, z, east, west, south, north, up, down

      en = 0
      DO z = 1, l_z
         DO y = 1, l_y
            DO x = 1, l_x
               CALL orient_3d(sp4, x, y, z, east, west, south, north, up, down)
               en = en - sp4(x,y,z) * (east + west + south + north + up + down)
            END DO
         END DO
      END DO
      en = en / 2
    END SUBROUTINE calcEn_3d

    SUBROUTINE calcEE_2d(sp4, en)
      INTEGER(kind = 4), INTENT(in) :: sp4(1:, 1:)
      INTEGER(kind = 4), INTENT(out) :: en

      INTEGER(kind = 4) :: x

      en = 0
      DO CONCURRENT (x = 1:l_x)
         en = en - sp4(x, l_z / 2) * sp4(x, l_z / 2 + 1)
      END DO
    END SUBROUTINE calcEE_2d

    SUBROUTINE calcEE_3d(sp4, en)
      INTEGER(kind = 4), INTENT(in) :: sp4(1:, 1:, 1:)
      INTEGER(kind = 4), INTENT(out) :: en

      INTEGER(kind = 4) :: x, y, east, west, south, north, up, down

      en = 0
      DO CONCURRENT (y = 1:l_y)
         DO CONCURRENT (x = 1:l_x)
            en = en - sp4(x, y, l_z / 2) * sp4(x, y, l_z / 2 + 1)
         END DO
      END DO
    END SUBROUTINE calcEE_3d

    SUBROUTINE initSp_2d(seed, sp4)
      INTEGER(kind = 4), INTENT(in) :: seed
      INTEGER(kind = 4), INTENT(out) :: sp4(1:, 1:)

      INTEGER(kind = 4) :: err, z
      TYPE(VSL_STREAM_STATE) :: str_sp

      SELECT CASE (id_IC)
      CASE (1)
         sp4(1:l_x, 1:l_z / 2) = 1; sp4(1:l_x, l_z / 2 + 1:l_z) = -1
      CASE (2)
         sp4(1:l_x, 1:l_z) = 1
      CASE (3)
         err = vslnewstream(str_sp, VSL_BRNG_MT19937, seed)
         DO z = 1, l_z, 1
            err = virnguniform(VSL_RNG_METHOD_UNIFORM_STD, str_sp, l_x, sp4(1:l_x, z), 0, 2)
         END DO
         err = vsldeletestream(str_sp)
         sp4(1:l_x, 1:l_z) = 2 * sp4(1:l_x, 1:l_z) - 1
      END SELECT
    END SUBROUTINE initSp_2d

    SUBROUTINE initSp_3d(seed, sp4)
      INTEGER(kind = 4), INTENT(in) :: seed
      INTEGER(kind = 4), INTENT(out) :: sp4(1:, 1:, 1:)

      INTEGER(kind = 4) :: err, x, y, z
      TYPE(VSL_STREAM_STATE) :: str_sp

      SELECT CASE (id_IC)
      CASE (1)
         sp4(1:l_x, 1:l_y, 1:l_z / 2) = 1; sp4(1:l_x, 1:l_y, l_z / 2 + 1:l_z) = -1
      CASE (2)
         sp4(1:l_x, 1:l_y, 1:l_z) = 1
      CASE (3)
         err = vslnewstream(str_sp, VSL_BRNG_MT19937, seed)
         DO z = 1, l_z, 1
            DO y = 1, l_y, 1
               err = virnguniform(VSL_RNG_METHOD_UNIFORM_STD, str_sp, l_x, sp4(1:l_x, y, z), 0, 2)
            END DO
         END DO
         err = vsldeletestream(str_sp)
         sp4(1:l_x, 1:l_y, 1:l_z) = 2 * sp4(1:l_x, 1:l_y, 1:l_z) - 1
      END SELECT
    END SUBROUTINE initSp_3d

    SUBROUTINE SSF_2d(x, z, p, sp4, rel)
      INTEGER(kind = 4), INTENT(in) :: x, z
      REAL(kind = 8), INTENT(in) :: p
      INTEGER(kind = 4), INTENT(inout) :: sp4(1:, 1:)
      INTEGER(kind = 4), INTENT(out) :: rel

      INTEGER(kind = 4) :: east, west, south, north

      CALL orient_2d(sp4(1:l_x, 1:l_z), x, z, east, west, south, north)

      IF (p <= pr_2d(sp4(x, z), east, west, south, north)) THEN
         rel =  2 * sp4(x, z) * (east + west + south + north); sp4(x, z) = - sp4(x, z)
      ELSE
         rel = 0
      END IF
    END SUBROUTINE SSF_2d

    SUBROUTINE SSF_3d(x, y, z, p, sp4, rel)
      INTEGER(kind = 4), INTENT(in) :: x, y, z
      REAL(kind = 8), INTENT(in) :: p
      INTEGER(kind = 4), INTENT(inout) :: sp4(1:, 1:, 1:)
      INTEGER(kind = 4), INTENT(out) :: rel

      INTEGER(kind = 4) :: east, west, south, north, up, down

      CALL orient_3d(sp4(1:l_x, 1:l_y, 1:l_z), x, y, z, east, west, south, north, up, down)

      IF (p <= pr_3d(sp4(x, y, z), east, west, south, north, up, down)) THEN
         rel =  2 * sp4(x, y, z) * (east + west + south + north + up + down); sp4(x, y, z) = - sp4(x, y, z)
      ELSE
         rel = 0
      END IF
    END SUBROUTINE SSF_3d

    SUBROUTINE mSSFs_2d(slot, t, i_v, n_st, r_x, r_z, r_p, sp4, en)
      INTEGER(kind = 4), INTENT(in) :: slot, t, i_v, n_st, r_x(1:), r_z(1:)
      REAL(kind = 8), INTENT(in) :: r_p(1:)
      INTEGER(kind = 4), INTENT(inout) :: sp4(1:, 1:), en

      INTEGER(kind = 4) :: i_st, rel

      DO i_st = 1, n_st, 1
         CALL SSF_2d(r_x(i_st), r_z(i_st), r_p(i_st), sp4(1:, 1:), rel); en = en + rel
         WRITE(slot) en
      END DO
    END SUBROUTINE mSSFs_2d

    SUBROUTINE mSSFs_3d(slot, t, i_v, n_st, r_x, r_y, r_z, r_p, sp4, en)
      INTEGER(kind = 4), INTENT(in) :: slot, t, i_v, n_st, r_x(1:), r_y(1:), r_z(1:)
      REAL(kind = 8), INTENT(in) :: r_p(1:)
      INTEGER(kind = 4), INTENT(inout) :: sp4(1:, 1:, 1:), en

      INTEGER(kind = 4) :: i_st, rel

      DO i_st = 1, n_st, 1
         CALL SSF_3d(r_x(i_st), r_y(i_st), r_z(i_st), r_p(i_st), sp4(1:, 1:, 1:), rel); en = en + rel
         WRITE(slot) en
      END DO
    END SUBROUTINE mSSFs_3d

    SUBROUTINE shift_2d(sl_en, sl_eb, t, i_v, sp4, en)
      INTEGER(kind = 4), INTENT(in) :: sl_en, sl_eb, t, i_v
      INTEGER(kind = 4), INTENT(inout) :: sp4(1:, 1:), en

      INTEGER(kind = 4) :: ee, ee_prv

      CALL calcEE_2d(sp4(1:, 1:), ee_prv)
      sp4(1:l_x, 1:l_z / 2) = CSHIFT(sp4(1:l_x, 1:l_z / 2), 1, 1)
      CALL calcEE_2d(sp4(1:, 1:), ee)

      en = en + ee - ee_prv; WRITE(sl_en) en
    END SUBROUTINE shift_2d

    SUBROUTINE shift_3d(sl_en, sl_eb, t, i_v, sp4, en)
      INTEGER(kind = 4), INTENT(in) :: sl_en, sl_eb, t, i_v
      INTEGER(kind = 4), INTENT(inout) :: sp4(1:, 1:, 1:), en

      INTEGER(kind = 4) :: ee, ee_prv

      CALL calcEE_3d(sp4(1:, 1:, 1:), ee_prv)
      sp4(1:l_x, 1:l_y, 1:l_z / 2) = CSHIFT(sp4(1:l_x, 1:l_y, 1:l_z / 2), 1, 1)
      CALL calcEE_3d(sp4(1:, 1:, 1:), ee)

      en = en + ee - ee_prv; WRITE(sl_en) en
    END SUBROUTINE shift_3d

    SUBROUTINE calcAC(a, ac)
      REAL(kind = 8), INTENT(in) :: a(1:)
      REAL(kind = 8), INTENT(out) :: ac(1:)
      INTEGER(kind = 4) :: t, t_, l_t

      ac(1:l_t / 2) = 0.0d0
      DO CONCURRENT (t = 1:l_t/2)
         DO t_ = 1, l_t - t
            ac(t) = ac(t) + DBLE(a(t_) * a(t_ + t))
         END DO
         ac(t) = ac(t) / DBLE(l_t - t); ac(t) = ac(t) - DBLE(SUM(a(1:l_t))) / DBLE(l_t)
      END DO
    END SUBROUTINE calcAC

    SUBROUTINE countSamples(n_s, n_s0)
      INTEGER(kind = 4), INTENT(in) :: n_s
      INTEGER(kind = 4), INTENT(out) :: n_s0
      CHARACTER(len = 4, kind = 1) :: ss

      INTEGER(kind = 4) :: s, st_sp

      DO s = 1, n_s, 1
         WRITE(ss, '(i0.4)') s
         st_sp = access ("en_s"//ss//"_step.bin", " ")
         IF (st_sp > 0) THEN
            n_s0 = s - 1; EXIT
         END IF
      END DO
    END SUBROUTINE countSamples
  END MODULE mod_proc
