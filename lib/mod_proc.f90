INCLUDE 'mkl_vsl.f90'

MODULE mod_proc
  USE mod_global
  USE MKL_VSL_TYPE
  USE MKL_VSL
  USE IFPORT, ONLY: access
  IMPLICIT NONE
CONTAINS
  SUBROUTINE importParams_2d(len_x, len_z, beta, vel, len_t1, len_t2, id_IC, id_BC, len_s)
    INTEGER(kind = 4), INTENT(out) :: len_x, len_z
    REAL(kind = 8), INTENT(out) :: beta
    INTEGER(kind = 4), INTENT(out) :: vel, len_t1, len_t2, id_IC, id_BC, len_s

    READ(*, *) len_x, len_z, beta, vel, len_t1, len_t2, id_IC, id_BC, len_s
  END SUBROUTINE importParams_2d

  SUBROUTINE importParams_3d(len_x, len_y, len_z, beta, vel, len_t1, len_t2, id_IC, id_BC, len_s)
    INTEGER(kind = 4), INTENT(out) :: len_x, len_y, len_z
    REAL(kind = 8), INTENT(out) :: beta
    INTEGER(kind = 4), INTENT(out) :: vel, len_t1, len_t2, id_IC, id_BC, len_s

    READ(*, *) len_x, len_y, len_z, beta, vel, len_t1, len_t2, id_IC, id_BC, len_s
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
  SUBROUTINE orient_2d(spin4, x, z, east, west, south, north)
    INTEGER(kind = 4), INTENT(in) :: spin4(1:, 1:), x, z
    INTEGER(kind = 4), INTENT(out) :: east, west, south, north

    IF ( x == len_x ) THEN
       east = spin4(1, z)
    ELSE
       east = spin4(x + 1, z)
    END IF
    IF ( x == 1 ) THEN
       west = spin4(len_x, z)
    ELSE
       west = spin4(x - 1, z)
    END IF

    SELECT CASE (id_BC)
    CASE (1) !BC: anti-parallel
       IF ( z == 1 ) THEN
          south = 1
       ELSE
          south = spin4(x, z - 1)
       END IF
       IF ( z == len_z ) THEN
          north = -1
       ELSE
          north = spin4(x, z + 1)
       END IF
    CASE (2) !BC: parallel
       IF ( z == 1 ) THEN
          south = 1
       ELSE
          south = spin4(x, z - 1)
       END IF
       IF ( z == len_z ) THEN
          north = 1
       ELSE
          north = spin4(x, z + 1)
       END IF
    CASE (3) !BC: free
       IF ( z == 1 ) THEN
          south = 0
       ELSE
          south = spin4(x, z - 1)
       END IF
       IF ( z == len_z ) THEN
          north = 0
       ELSE
          north = spin4(x, z + 1)
       END IF
    END SELECT
  END SUBROUTINE orient_2d

  ! TODO: 名前に因んで関数化
  SUBROUTINE orient_3d(spin4, x, y, z, east, west, south, north, up, down)
    INTEGER(kind = 4), INTENT(in) :: spin4(1:, 1:, 1:), x, y, z
    INTEGER(kind = 4), INTENT(out) :: east, west, south, north, up, down

    IF ( x == len_x ) THEN
       east = spin4(1, y, z)
    ELSE
       east = spin4(x + 1, y, z)
    END IF
    IF ( x == 1 ) THEN
       west = spin4(len_x, y, z)
    ELSE
       west = spin4(x - 1, y, z)
    END IF

    IF ( y == len_y ) THEN
       north = spin4(x, 1, z)
    ELSE
       north = spin4(x, y + 1, z)
    END IF
    IF ( y == 1 ) THEN
       south = spin4(x, len_y, z)
    ELSE
       south = spin4(x, y - 1, z)
    END IF

    SELECT CASE (id_BC)
    CASE (1) !z-BC: anti-parallel
       IF ( z == 1 ) THEN
          down = 1
       ELSE
          down = spin4(x, y, z - 1)
       END IF
       IF ( z == len_z ) THEN
          up = -1
       ELSE
          up = spin4(x, y, z + 1)
       END IF
    CASE (2) !z-BC: parallel
       IF ( z == 1 ) THEN
          down = 1
       ELSE
          down = spin4(x, y, z - 1)
       END IF
       IF ( z == len_z ) THEN
          up = 1
       ELSE
          up = spin4(x, y, z + 1)
       END IF
    CASE (3) !z-BC: free
       IF ( z == 1 ) THEN
          down = 0
       ELSE
          down = spin4(x, y, z - 1)
       END IF
       IF ( z == len_z ) THEN
          up = 0
       ELSE
          up = spin4(x, y, z + 1)
       END IF
    END SELECT
  END SUBROUTINE orient_3d

  SUBROUTINE calcEn_2d(spin4, en)
    INTEGER(kind = 4), INTENT(in) :: spin4(1:, 1:)
    INTEGER(kind = 4), INTENT(out) :: en

    INTEGER(kind = 4) :: x, z, east, west, south, north

    en = 0
    DO z = 1, len_z
       DO x = 1, len_x
          CALL orient_2d(spin4, x, z, east, west, south, north)
          en = en - spin4(x,z) * (east + west + south + north)
       END DO
    END DO
    en = en / 2
  END SUBROUTINE calcEn_2d

  SUBROUTINE calcEn_3d(spin4, en)
    INTEGER(kind = 4), INTENT(in) :: spin4(1:, 1:, 1:)
    INTEGER(kind = 4), INTENT(out) :: en

    INTEGER(kind = 4) :: x, y, z, east, west, south, north, up, down

    en = 0
    DO z = 1, len_z
       DO y = 1, len_y
          DO x = 1, len_x
             CALL orient_3d(spin4, x, y, z, east, west, south, north, up, down)
             en = en - spin4(x,y,z) * (east + west + south + north + up + down)
          END DO
       END DO
    END DO
    en = en / 2
  END SUBROUTINE calcEn_3d

  SUBROUTINE calcBoundEn_2d(spin4, en)
    INTEGER(kind = 4), INTENT(in) :: spin4(1:, 1:)
    INTEGER(kind = 4), INTENT(out) :: en

    INTEGER(kind = 4) :: x

    en = 0
    DO CONCURRENT (x = 1:len_x)
       en = en - spin4(x, len_z / 2) * spin4(x, len_z / 2 + 1)
    END DO
  END SUBROUTINE calcBoundEn_2d

  SUBROUTINE calcBoundEn_3d(spin4, en)
    INTEGER(kind = 4), INTENT(in) :: spin4(1:, 1:, 1:)
    INTEGER(kind = 4), INTENT(out) :: en

    INTEGER(kind = 4) :: x, y, east, west, south, north, up, down

    en = 0
    DO CONCURRENT (y = 1:len_y)
       DO CONCURRENT (x = 1:len_x)
          en = en - spin4(x, y, len_z / 2) * spin4(x, y, len_z / 2 + 1)
       END DO
    END DO
  END SUBROUTINE calcBoundEn_3d

  SUBROUTINE initSpin_2d(seed, spin4)
    INTEGER(kind = 4), INTENT(in) :: seed
    INTEGER(kind = 4), INTENT(out) :: spin4(1:, 1:)

    INTEGER(kind = 4) :: err, z
    TYPE(VSL_STREAM_STATE) :: str_spin

    SELECT CASE (id_IC)
    CASE (1)
       spin4(1:len_x, 1:len_z / 2) = 1
       spin4(1:len_x, len_z / 2 + 1:len_z) = -1
    CASE (2)
       spin4(1:len_x, 1:len_z) = 1
    CASE (3)
       err = vslnewstream(str_spin, VSL_BRNG_MT19937, seed)
       do z = 1, len_z, 1
         err = virnguniform(VSL_RNG_METHOD_UNIFORM_STD, str_spin, len_x, spin4(1:len_x, z), 0, 2)
       end do
       err = vsldeletestream(str_spin)
       spin4(1:len_x, 1:len_z) = 2 * spin4(1:len_x, 1:len_z) - 1
    END SELECT
  END SUBROUTINE initSpin_2d

  SUBROUTINE initSpin_3d(seed, spin4)
    INTEGER(kind = 4), INTENT(in) :: seed
    INTEGER(kind = 4), INTENT(out) :: spin4(1:, 1:, 1:)

    INTEGER(kind = 4) :: err, x, y, z
    TYPE(VSL_STREAM_STATE) :: str_spin

    SELECT CASE (id_IC)
    CASE (1)
       spin4(1:len_x, 1:len_y, 1:len_z / 2) = 1
       spin4(1:len_x, 1:len_y, len_z / 2 + 1:len_z) = -1
    CASE (2)
       spin4(1:len_x, 1:len_y, 1:len_z) = 1
    CASE (3)
       err = vslnewstream(str_spin, VSL_BRNG_MT19937, seed)
       do z = 1, len_z, 1
         do y = 1, len_y, 1
           err = virnguniform(VSL_RNG_METHOD_UNIFORM_STD, str_spin, len_x, spin4(1:len_x, y, z), 0, 2)
         end do
       end do
       err = vsldeletestream(str_spin)
       spin4(1:len_x, 1:len_y, 1:len_z) = 2 * spin4(1:len_x, 1:len_y, 1:len_z) - 1
    END SELECT
  END SUBROUTINE initSpin_3d

  SUBROUTINE SSF_2d(x, z, p, spin4, relax)
    INTEGER(kind = 4), INTENT(in) :: x, z
    REAL(kind = 8), INTENT(in) :: p
    INTEGER(kind = 4), INTENT(inout) :: spin4(1:, 1:)
    INTEGER(kind = 4), INTENT(out) :: relax

    INTEGER(kind = 4) :: east, west, south, north

    CALL orient_2d(spin4(1:len_x, 1:len_z), x, z, east, west, south, north)

    IF (p <= pr_2d(spin4(x, z), east, west, south, north)) THEN
       relax =  2 * spin4(x, z) * (east + west + south + north)
       spin4(x, z) = - spin4(x, z)
    ELSE
       relax = 0
    END IF
  END SUBROUTINE SSF_2d

  SUBROUTINE SSF_3d(x, y, z, p, spin4, relax)
    INTEGER(kind = 4), INTENT(in) :: x, y, z
    REAL(kind = 8), INTENT(in) :: p
    INTEGER(kind = 4), INTENT(inout) :: spin4(1:, 1:, 1:)
    INTEGER(kind = 4), INTENT(out) :: relax

    INTEGER(kind = 4) :: east, west, south, north, up, down

    CALL orient_3d(spin4(1:len_x, 1:len_y, 1:len_z), x, y, z, east, west, south, north, up, down)

    IF (p <= pr_3d(spin4(x, y, z), east, west, south, north, up, down)) THEN
       relax =  2 * spin4(x, y, z) * (east + west + south + north + up + down)
       spin4(x, y, z) = - spin4(x, y, z)
    ELSE
       relax = 0
    END IF
  END SUBROUTINE SSF_3d

  SUBROUTINE multiSSFs_2d(slot, t, i_vel, n_steps, rn_x, rn_z, rn_p, spin4, en)
    INTEGER(kind = 4), INTENT(in) :: slot, t, i_vel, n_steps, rn_x(1:), rn_z(1:)
    REAL(kind = 8), INTENT(in) :: rn_p(1:)
    INTEGER(kind = 4), INTENT(inout) :: spin4(1:, 1:), en

    INTEGER(kind = 4) :: i_step
    INTEGER(kind = 4) :: relax

    DO i_step = 1, n_steps, 1
       CALL SSF_2d(rn_x(i_step), rn_z(i_step), rn_p(i_step), spin4(1:, 1:), relax)
       en = en + relax
       WRITE(slot) en
    END DO
  END SUBROUTINE multiSSFs_2d

  SUBROUTINE multiSSFs_3d(slot, t, i_vel, n_steps, rn_x, rn_y, rn_z, rn_p, spin4, en)
    INTEGER(kind = 4), INTENT(in) :: slot, t, i_vel, n_steps, rn_x(1:), rn_y(1:), rn_z(1:)
    REAL(kind = 8), INTENT(in) :: rn_p(1:)
    INTEGER(kind = 4), INTENT(inout) :: spin4(1:, 1:, 1:), en

    INTEGER(kind = 4) :: i_step
    INTEGER(kind = 4) :: relax

    DO i_step = 1, n_steps, 1
       CALL SSF_3d(rn_x(i_step), rn_y(i_step), rn_z(i_step), rn_p(i_step), spin4(1:, 1:, 1:), relax)
       en = en + relax
       WRITE(slot) en
    END DO
  END SUBROUTINE multiSSFs_3d

  SUBROUTINE shift_2d(slot, t, i_vel, spin4, en)
    INTEGER(kind = 4), INTENT(in) :: slot, t, i_vel
    INTEGER(kind = 4), INTENT(inout) :: spin4(1:, 1:), en

    INTEGER(kind = 4) :: prev, next

    CALL calcBoundEn_2d(spin4(1:, 1:), prev)
    spin4(1:len_x, 1:len_z / 2) = CSHIFT(spin4(1:len_x, 1:len_z / 2), 1, 1)
    CALL calcBoundEn_2d(spin4(1:, 1:), next)
    en = en - prev + next
    WRITE(slot) en
  END SUBROUTINE shift_2d

  SUBROUTINE shift_3d(slot, t, i_vel, spin4, en)
    INTEGER(kind = 4), INTENT(in) :: slot, t, i_vel
    INTEGER(kind = 4), INTENT(inout) :: spin4(1:, 1:, 1:), en

    INTEGER(kind = 4) :: prev, next

    CALL calcBoundEn_3d(spin4(1:, 1:, 1:), prev)
    spin4(1:len_x, 1:len_y, 1:len_z / 2) = CSHIFT(spin4(1:len_x, 1:len_y, 1:len_z / 2), 1, 1)
    CALL calcBoundEn_3d(spin4(1:, 1:, 1:), next)
    en = en - prev + next
    WRITE(slot) en
  END SUBROUTINE shift_3d

  SUBROUTINE calcAC(a, ac)
    REAL(kind = 8), INTENT(in) :: a(1:)
    REAL(kind = 8), INTENT(out) :: ac(1:)
    INTEGER(kind = 4) :: t, t_, len_t

    ac(1:len_t / 2) = 0.0d0
    DO CONCURRENT (t = 1:len_t/2)
       DO t_ = 1, len_t - t
          ac(t) = ac(t) + DBLE(a(t_) * a(t_ + t))
       END DO
       ac(t) = ac(t) / DBLE(len_t - t)
       ac(t) = ac(t) - DBLE(SUM(a(1:len_t))) / DBLE(len_t)
    END DO
  END SUBROUTINE calcAC
END MODULE mod_proc
