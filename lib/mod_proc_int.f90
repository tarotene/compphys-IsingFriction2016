MODULE mod_proc_int
  USE mod_global
  USE mod_rand
  USE IFPORT, ONLY: access
  IMPLICIT NONE
CONTAINS
  SUBROUTINE makeProbArray_2d(beta, deltaE, prob)
    REAL(kind = 8), INTENT(in) :: beta
    REAL(kind = 8), INTENT(in) :: deltaE(-1:, -1:, -1:, -1:, -1:)
    REAL(kind = 8), INTENT(out) :: prob(-1:, -1:, -1:, -1:, -1:)

    WHERE ( deltaE > 0.0d0 )
       prob(-1:1, -1:1, -1:1, -1:1, -1:1) = &
            EXP(- beta * deltaE(-1:1, -1:1, -1:1, -1:1, -1:1))
    ELSEWHERE
       prob(-1:1, -1:1, -1:1, -1:1, -1:1) = 1.0d0
    END WHERE
  END SUBROUTINE makeProbArray_2d

  SUBROUTINE makeProbArray_3d(beta, deltaE, prob)
    REAL(kind = 8), INTENT(in) :: beta
    REAL(kind = 8), INTENT(in) :: deltaE(-1:, -1:, -1:, -1:, -1:, -1:, -1:)
    REAL(kind = 8), INTENT(out) :: prob(-1:, -1:, -1:, -1:, -1:, -1:, -1:)

    WHERE ( deltaE > 0.0d0 )
       prob(-1:1, -1:1, -1:1, -1:1, -1:1, -1:1, -1:1) = &
            EXP(- beta * deltaE(-1:1, -1:1, -1:1, -1:1, -1:1, -1:1, -1:1))
    ELSEWHERE
       prob(-1:1, -1:1, -1:1, -1:1, -1:1, -1:1, -1:1) = 1.0d0
    END WHERE
  END SUBROUTINE makeProbArray_3d

  !TODO: function手続きを用いて書き直すべき
  SUBROUTINE makeDeltaEArray_2d(J, deltaE)
    REAL(kind = 8), INTENT(in) :: J
    REAL(kind = 8), INTENT(out) :: deltaE(-1:, -1:, -1:, -1:, -1:)

    INTEGER(kind = 4) :: center, east, west, south, north

    DO center = -1, 1, 1
       DO east = -1, 1, 1
          DO west = -1, 1, 1
             DO south = -1, 1, 1
                DO north = -1, 1, 1
                   deltaE(center, east, west, south, north) &
                        = 2.0d0 * J * DBLE(center * (east + west + south + north))
                END DO
             END DO
          END DO
       END DO
    END DO
  END SUBROUTINE makeDeltaEArray_2d

  !TODO: function手続きを用いて書き直すべき
  SUBROUTINE makeDeltaEArray_3d(J, deltaE)
    REAL(kind = 8), INTENT(in) :: J
    REAL(kind = 8), INTENT(out) :: deltaE(-1:, -1:, -1:, -1:, -1:, -1:, -1:)

    INTEGER(kind = 4) :: center, east, west, south, north, up, down

    DO center = -1, 1, 1
       DO east = -1, 1, 1
          DO west = -1, 1, 1
             DO south = -1, 1, 1
                DO north = -1, 1, 1
                   DO up = -1, 1, 1
                      DO down = -1, 1, 1
                         deltaE(center, east, west, south, north, up, down) &
                              = 2 * J &
                              * DBLE(center * (east + west + south + north + up + down))
                      END DO
                   END DO
                END DO
             END DO
          END DO
       END DO
    END DO
  END SUBROUTINE makeDeltaEArray_3d

  SUBROUTINE set_direction_2d(id_BC, len_x, len_z, spin, x, z, &
       east, west, south, north)
    INTEGER(kind = 4), INTENT(in) :: id_BC, len_x, len_z
    INTEGER(kind = 4), INTENT(in) :: spin(1:, 1:)
    INTEGER(kind = 4), INTENT(in) :: x, z
    INTEGER(kind = 4), INTENT(out) :: east, west, south, north

    IF ( x == len_x ) THEN
       east = spin(1, z)
    ELSE
       east = spin(x + 1, z)
    END IF
    IF ( x == 1 ) THEN
       west = spin(len_x, z)
    ELSE
       west = spin(x - 1, z)
    END IF

    SELECT CASE (id_BC)
    CASE (1) !BC: anti-parallel
       IF ( z == 1 ) THEN
          south = 1
       ELSE
          south = spin(x, z - 1)
       END IF
       IF ( z == len_z ) THEN
          north = -1
       ELSE
          north = spin(x, z + 1)
       END IF
    CASE (2) !BC: parallel
       IF ( z == 1 ) THEN
          south = 1
       ELSE
          south = spin(x, z - 1)
       END IF
       IF ( z == len_z ) THEN
          north = 1
       ELSE
          north = spin(x, z + 1)
       END IF
    CASE (3) !BC: free
       IF ( z == 1 ) THEN
          south = 0
       ELSE
          south = spin(x, z - 1)
       END IF
       IF ( z == len_z ) THEN
          north = 0
       ELSE
          north = spin(x, z + 1)
       END IF
    END SELECT
  END SUBROUTINE set_direction_2d

  SUBROUTINE set_direction_3d(id_BC, len_x, len_y, len_z, spin, x, y, z, &
       east, west, south, north, up, down)
    INTEGER(kind = 4), INTENT(in) :: id_BC, len_x, len_y, len_z
    INTEGER(kind = 4), INTENT(in) :: spin(1:, 1:, 1:)
    INTEGER(kind = 4), INTENT(in) :: x, y, z
    INTEGER(kind = 4), INTENT(out) :: east, west, south, north, up, down

    IF ( x == len_x ) THEN
       east = spin(1, y, z)
    ELSE
       east = spin(x + 1, y, z)
    END IF
    IF ( x == 1 ) THEN
       west = spin(len_x, y, z)
    ELSE
       west = spin(x - 1, y, z)
    END IF

    IF ( y == len_y ) THEN
       north = spin(x, 1, z)
    ELSE
       north = spin(x, y + 1, z)
    END IF
    IF ( y == 1 ) THEN
       south = spin(x, len_y, z)
    ELSE
       south = spin(x, y - 1, z)
    END IF

    SELECT CASE (id_BC)
    CASE (1) !z-BC: anti-parallel
       IF ( z == 1 ) THEN
          down = 1
       ELSE
          down = spin(x, y, z - 1)
       END IF
       IF ( z == len_z ) THEN
          up = -1
       ELSE
          up = spin(x, y, z + 1)
       END IF
    CASE (2) !z-BC: parallel
       IF ( z == 1 ) THEN
          down = 1
       ELSE
          down = spin(x, y, z - 1)
       END IF
       IF ( z == len_z ) THEN
          up = 1
       ELSE
          up = spin(x, y, z + 1)
       END IF
    CASE (3) !z-BC: free
       IF ( z == 1 ) THEN
          down = 0
       ELSE
          down = spin(x, y, z - 1)
       END IF
       IF ( z == len_z ) THEN
          up = 0
       ELSE
          up = spin(x, y, z + 1)
       END IF
    END SELECT
  END SUBROUTINE set_direction_3d

  SUBROUTINE calcEnergy_2d(id_BC, len_x, len_z, spin, energy)
    INTEGER(kind = 4), INTENT(in) :: id_BC, len_x, len_z
    INTEGER(kind = 4), INTENT(in) :: spin(1:, 1:)
    REAL(kind = 8), INTENT(out) :: energy

    INTEGER(kind = 4) :: x, z, east, west, south, north

    energy = 0.0d0
    DO x = 1, len_x
       DO z = 1, len_z
          CALL set_direction_2d(id_BC, len_x, len_z, spin, x, z, &
               east, west, south, north)
          energy = energy - J * DBLE(spin(x,z) * (east + west + south + north))
       END DO
    END DO
    energy = 0.5d0 * energy
  END SUBROUTINE calcEnergy_2d

  SUBROUTINE calcEnergy_3d(id_BC, len_x, len_y, len_z, spin, energy)
    INTEGER(kind = 4), INTENT(in) :: id_BC, len_x, len_y, len_z
    INTEGER(kind = 4), INTENT(in) :: spin(1:, 1:, 1:)
    REAL(kind = 8), INTENT(out) :: energy

    INTEGER(kind = 4) :: x, y, z, east, west, south, north, up, down

    energy = 0.0d0
    DO x = 1, len_x
       DO y = 1, len_y
          DO z = 1, len_z
             CALL set_direction_3d(id_BC, len_x, len_y, len_z, spin, x, y, z, &
                  east, west, south, north, up, down)
             energy = energy &
                  - J * DBLE(spin(x,y,z) * (east + west + south + north + up + down))
          END DO
       END DO
    END DO
    energy = 0.5d0 * energy
  END SUBROUTINE calcEnergy_3d

  SUBROUTINE calcSlipplaneEnergy_2d(id_BC, len_x, len_z, spin, energy)
    INTEGER(kind = 4), INTENT(in) :: id_BC, len_x, len_z
    INTEGER(kind = 4), INTENT(in) :: spin(1:, 1:)
    REAL(kind = 8), INTENT(out) :: energy

    INTEGER(kind = 4) :: x, east, west, south, north

    energy = 0.0d0
    DO x = 1, len_x, 1
       CALL set_direction_2d(id_BC, len_x, len_z, &
            spin(1:len_x, 1:len_z), &
            x, len_z / 2, east, west, south, north)
       energy = energy &
            - J * DBLE(spin(x, len_z / 2) * (east + west + south + north))
    END DO
  END SUBROUTINE calcSlipplaneEnergy_2d

  SUBROUTINE calcSlipplaneEnergy_3d(id_BC, len_x, len_y, len_z, spin, energy)
    INTEGER(kind = 4), INTENT(in) :: id_BC, len_x, len_y, len_z
    INTEGER(kind = 4), INTENT(in) :: spin(1:, 1:, 1:)
    REAL(kind = 8), INTENT(out) :: energy

    INTEGER(kind = 4) :: x, y, east, west, south, north, up, down

    energy = 0.0d0
    DO x = 1, len_x, 1
       DO y = 1, len_y, 1
          CALL set_direction_3d(id_BC, len_x, len_y, len_z, &
               spin(1:len_x, 1:len_y, 1:len_z), &
               x, y, len_z / 2, east, west, south, north, up, down)
          energy = energy &
               - J * DBLE(spin(x, y, len_z / 2) * (east + west + south + north + up + down))
       END DO
    END DO
  END SUBROUTINE calcSlipplaneEnergy_3d

  SUBROUTINE initializeSpin_2d(id_IC, len_x, len_z, spin)
    INTEGER(kind = 4), INTENT(in) :: id_IC, len_x, len_z
    INTEGER(kind = 4), INTENT(out) :: spin(1:, 1:)

    INTEGER(kind = 4) :: seed_spin, err_spin
    INTEGER(kind = 4) :: x, z
    TYPE(VSL_STREAM_STATE) :: str_spin

    SELECT CASE (id_IC)
    CASE (1)
       spin(1:len_x, 1:len_z / 2) = 1
       spin(1:len_x, len_z / 2 + 1:len_z) = -1
    CASE (2)
       spin(1:len_x, 1:len_z) = 1
    CASE (3)
       CALL system_CLOCK(seed_spin)
       CALL initializeRN(seed_spin, str_spin, err_spin)
       DO z = 1, len_z, 1
          CALL generateRN_int(str_spin, 0, 1, len_x, &
               spin(1:len_x, z), err_spin)
       END DO
       spin(1:len_x, 1:len_z) = 2 * spin(1:len_x, 1:len_z) - 1
    END SELECT
  END SUBROUTINE initializeSpin_2d

  SUBROUTINE initializeSpin_3d(id_IC, len_x, len_y, len_z, spin)
    INTEGER(kind = 4), INTENT(in) :: id_IC, len_x, len_y, len_z
    INTEGER(kind = 4), INTENT(out) :: spin(1:, 1:, 1:)

    INTEGER(kind = 4) :: seed_spin, err_spin
    INTEGER(kind = 4) :: x, y, z
    TYPE(VSL_STREAM_STATE) :: str_spin

    SELECT CASE (id_IC)
    CASE (1)
      spin(1:len_x, 1:len_y, 1:len_z / 2) = 1
      spin(1:len_x, 1:len_y, len_z / 2 + 1:len_z) = -1
    CASE (2)
      spin(1:len_x, 1:len_y, 1:len_z) = 1
    CASE (3)
       CALL system_CLOCK(seed_spin)
       CALL initializeRN(seed_spin, str_spin, err_spin)
       DO z = 1, len_z, 1
          DO y = 1, len_y, 1
             CALL generateRN_int(str_spin, 0, 1, len_x, &
                  spin(1:len_x, y, z), err_spin)
          END DO
       END DO
       spin(1:len_x, 1:len_y, 1:len_z) = 2 * spin(1:len_x, 1:len_y, 1:len_z) - 1
    END SELECT
  END SUBROUTINE initializeSpin_3d

  SUBROUTINE step_singleflip_2d(id_BC, len_x, len_z, &
       x, z, p, spin, relax)
    INTEGER(kind = 4), INTENT(in) :: id_BC, len_x, len_z, x, z
    REAL(kind = 8), INTENT(in) :: p
    INTEGER(kind = 4), INTENT(out) :: spin(1:, 1:)
    REAL(kind = 8), INTENT(out) :: relax

    REAL(kind = 8) :: en_loc
    INTEGER(kind = 4) :: east, west, south, north

    ! WRITE(*, '(a, i2, a, i2, a, i2, a)') "spin(", x, ", ", z, ") = ", spin(x, z), "."

    CALL set_direction_2d(id_BC, len_x, len_z, &
         spin(1:len_x, 1:len_z), x, z, &
         east, west, south, north)

    ! WRITE(*, *) 22.380d0
    ! WRITE(*, '(a, i2, a, i2, a, i2, a, i2, a)') &
    ! "ewsn=(", east, ", ", west, ", ", south, ", ", north, ")."

    IF (p <= prob_2d(spin(x, z), east, west, south, north)) THEN
       en_loc =  - spin(x, z) * (east + west + south + north)
       relax = - 2.0d0 * en_loc
       spin(x, z) = - spin(x, z)
       !  WRITE(*, *) 22.381d0
    ELSE
       relax = 0.0d0
       !  WRITE(*, *) 22.382d0
    END IF

    ! WRITE(*, *) 22.385d0
  END SUBROUTINE step_singleflip_2d

  SUBROUTINE step_singleflip_3d(id_BC, len_x, len_y, len_z, &
       x, y, z, p, spin, relax)
    INTEGER(kind = 4), INTENT(in) :: id_BC, len_x, len_y, len_z, x, y, z
    REAL(kind = 8), INTENT(in) :: p
    INTEGER(kind = 4), INTENT(out) :: spin(1:, 1:, 1:)
    REAL(kind = 8), INTENT(out) :: relax

    REAL(kind = 8) :: en_loc
    INTEGER(kind = 4) :: east, west, south, north, up, down

    CALL set_direction_3d(id_BC, len_x, len_y, len_z, &
         spin(1:len_x, 1:len_y, 1:len_z), x, y, z, &
         east, west, south, north, up, down)

    IF (p <= prob_3d(spin(x, y, z), east, west, south, north, up, down)) THEN
       en_loc =  - spin(x, y, z) * (east + west + south + north + up + down)
       relax = - 2 * en_loc
       spin(x, y, z) = - spin(x, y, z)
    ELSE
       relax = 0.0d0
    END IF
  END SUBROUTINE step_singleflip_3d

  SUBROUTINE sweep_singleflip_2d(slot, i_sweep, i_vel, id_BC, len_x, len_z, n_steps, &
       rn_x, rn_z, rn_p, spin, energy)
    INTEGER(kind = 4), INTENT(in) :: slot, i_sweep, id_BC, len_x, len_z, n_steps
    INTEGER(kind = 4), INTENT(in) :: rn_x(1:), rn_z(1:)
    REAL(kind = 8), INTENT(in) :: rn_p(1:)
    INTEGER(kind = 4), INTENT(inout) :: spin(1:, 1:)
    REAL(kind = 8), INTENT(inout) :: energy

    INTEGER(kind = 4) :: i_step
    REAL(kind = 8) :: relax

    DO i_step = 1, n_steps, 1
       CALL step_singleflip_2d(id_BC, len_x, len_z, &
            rn_x(i_step), rn_z(i_step), rn_p(i_step), &
            spin(1:, 1:), relax)
       energy = energy + relax
       WRITE(slot, '(i5, a, i5, a, i5, a, f0.4)') i_sweep, ", ", i_vel, ", ", i_step, ", ", energy
    END DO
  END SUBROUTINE sweep_singleflip_2d

  SUBROUTINE sweep_singleflip_3d(id_BC, len_x, len_y, len_z, n_steps, &
       rn_x, rn_y, rn_z, rn_p, spin, diss)
    INTEGER(kind = 4), INTENT(in) :: id_BC, len_x, len_y, len_z, n_steps
    INTEGER(kind = 4), INTENT(in) :: rn_x(1:), rn_y(1:), rn_z(1:)
    REAL(kind = 8), INTENT(in) :: rn_p(1:)
    INTEGER(kind = 4), INTENT(inout) :: spin(1:, 1:, 1:)
    REAL(kind = 8), INTENT(out) :: diss

    INTEGER(kind = 4) :: i_step
    REAL(kind = 8) :: relax

    diss = 0.0d0
    DO i_step = 1, n_steps, 1
       CALL step_singleflip_3d(id_BC, len_x, len_y, len_z, &
            rn_x(i_step), rn_y(i_step), rn_z(i_step), rn_p(i_step), &
            spin(1:, 1:, 1:), relax)
       diss = diss + relax
    END DO
  END SUBROUTINE sweep_singleflip_3d

  SUBROUTINE shiftUpperHalf_2d(slot, i_sweep, i_vel, id_BC, len_x, len_z, spin, energy)
    INTEGER(kind = 4), INTENT(in) :: slot, i_sweep, i_vel, id_BC, len_x, len_z
    INTEGER(kind = 4), INTENT(inout) :: spin(1:, 1:)
    REAL(kind = 8), INTENT(inout) :: energy

    REAL(kind = 8) :: prev, next

    CALL calcSlipplaneEnergy_2d(id_BC, len_x, len_z, &
         spin(1:, 1:), prev)
    spin(1:len_x, 1:len_z / 2) &
         = CSHIFT(spin(1:len_x, 1:len_z / 2), shift=1, dim=1)
    CALL calcSlipplaneEnergy_2d(id_BC, len_x, len_z, &
         spin(1:, 1:), next)
    energy = energy - prev + next
    WRITE(slot, '(i5, a, i5, a, i5, a, f0.4)') i_sweep, ", ", i_vel, ", ", 0, ", ", energy
  END SUBROUTINE shiftUpperHalf_2d

  SUBROUTINE shiftUpperHalf_3d(id_BC, len_x, len_y, len_z, spin, pump)
    INTEGER(kind = 4), INTENT(in) :: id_BC, len_x, len_y, len_z
    INTEGER(kind = 4), INTENT(inout) :: spin(1:, 1:, 1:)
    REAL(kind = 8), INTENT(out) :: pump

    REAL(kind = 8) :: prev, next

    CALL calcSlipplaneEnergy_3d(id_BC, len_x, len_y, len_z, &
         spin(1:, 1:, 1:), prev)
    spin(1:len_x, 1:len_y, 1:len_z / 2) &
         = CSHIFT(spin(1:len_x, 1:len_y, 1:len_z / 2), shift=1, dim=1)
    CALL calcSlipplaneEnergy_3d(id_BC, len_x, len_y, len_z, &
         spin(1:, 1:, 1:), next)
    pump = next - prev
  END SUBROUTINE shiftUpperHalf_3d

  SUBROUTINE synthesizeSets(n_olds, n_news, &
       a_old, f_old, a_new, f_new, a_tot, f_tot)
    INTEGER(kind = 4), INTENT(in) :: n_olds, n_news
    REAL(kind = 8), INTENT(in) :: a_old, f_old, a_new, f_new
    REAL(kind = 8), INTENT(out) :: a_tot, f_tot

    REAL(kind = 8) :: r_olds, r_news

    r_olds = DBLE(n_olds) / DBLE(n_olds + n_news)
    r_news = DBLE(n_news) / DBLE(n_olds + n_news)

    a_tot = a_old * r_olds + a_new * r_news

    f_tot = f_old * r_olds + f_new * r_news
    f_tot = f_tot + &
         r_olds * a_old ** 2 + r_news * a_new ** 2 - a_tot ** 2
  END SUBROUTINE synthesizeSets

  SUBROUTINE synthesizeSets_dim2(n_olds, n_news, &
       a_old, f_old, a_new, f_new, a_tot, f_tot)
    INTEGER(kind = 4), INTENT(in) :: n_olds, n_news
    REAL(kind = 8), INTENT(in) :: a_old(1:, 1:), f_old(1:, 1:)
    REAL(kind = 8), INTENT(in) :: a_new(1:, 1:), f_new(1:, 1:)
    REAL(kind = 8), INTENT(out) :: a_tot(1:, 1:), f_tot(1:, 1:)

    REAL(kind = 8) :: r_olds, r_news

    r_olds = DBLE(n_olds) / DBLE(n_olds + n_news)
    r_news = DBLE(n_news) / DBLE(n_olds + n_news)

    a_tot(:, :) = a_old(:, :) * r_olds + a_new(:, :) * r_news

    f_tot(:, :) = f_old(:, :) * r_olds + f_new(:, :) * r_news
    f_tot(:, :) = f_tot(:, :) + &
         r_olds * a_old(:, :) ** 2 + r_news * a_new(:, :) ** 2
    f_tot(:, :) = f_tot(:, :) - a_tot(:, :) ** 2
  END SUBROUTINE synthesizeSets_dim2

  SUBROUTINE calcAutocorrelation(stream, ac)
    REAL(kind = 8), INTENT(in) :: stream(1:)
    REAL(kind = 8), INTENT(out) :: ac(0:)

    REAL(kind = 8) :: mean
    INTEGER(kind = 4) :: t, t_prime, len_str

    len_str = SIZE(stream(:))
    mean = SUM(stream(1:len_str)) / DBLE(len_str)

    ac(0:len_str / 2) = 0.0d0
    DO t = 0, len_str / 2, 1
       DO t_prime = 1, len_str - t, 1
          ac(t) = ac(t) + stream(t_prime) * stream(t_prime + t)
       END DO
       ac(t) = ac(t) / DBLE(len_str - t)
    END DO
    ac(0:len_str / 2) = ac(0:len_str / 2) - mean ** 2
  END SUBROUTINE calcAutocorrelation

  SUBROUTINE calcCorrelationTime(ac, tau)
    REAL(kind = 8), INTENT(in) :: ac(0:)
    REAL(kind = 8), INTENT(out) :: tau

    INTEGER(kind = 4) :: len_ac

    len_ac = SIZE(ac(:))

    IF ( ac(0) <= EPSILON(0.0d0) ) THEN
       tau = 1.0d0
    ELSE
       tau = SUM(ac(0:len_ac - 1)) / ac(0)
    END IF
  END SUBROUTINE calcCorrelationTime

  SUBROUTINE calcMeanErr(n_samples, stream, fluc, mean, err)
    INTEGER(kind = 4), INTENT(in) :: n_samples
    REAL(kind = 8), INTENT(in) :: stream(1:), fluc(1:)
    REAL(kind = 8), INTENT(out) :: mean
    REAL(kind = 8), INTENT(out) :: err

    INTEGER(kind = 4) :: t, t_prime

    mean = SUM(stream(1:)) / DBLE(SIZE(stream(1:)))
    err = SQRT(SUM(fluc(1:)) / n_samples) / DBLE(SIZE(fluc(1:)))
  END SUBROUTINE calcMeanErr
END MODULE mod_proc_int
