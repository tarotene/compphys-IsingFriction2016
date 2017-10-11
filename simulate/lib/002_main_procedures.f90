MODULE main_procedures
  USE global_variables
  USE mod_rng
  IMPLICIT NONE
CONTAINS
  SUBROUTINE getNumSamples(slot, n_samples)
    INTEGER(kind = 4), INTENT(in) :: slot
    INTEGER(kind = 4), INTENT(out) :: n_samples

    INTEGER(kind = 4) :: i_line, ios, n_dum

    n_samples = 0
    OPEN(slot, file="list_samples.dat", status="old")
    DO i_line = 1, 10000, 1
       READ (slot, *, iostat = ios) n_dum
       IF (ios < 0) EXIT
       n_samples = n_samples + 1
    END DO
    CLOSE(slot)
  END SUBROUTINE getNumSamples

  SUBROUTINE getNumSweeps(slot, i_sample, n_sweeps_therm, n_sweeps_stead)
    INTEGER(kind = 4), INTENT(in) :: slot, i_sample
    INTEGER(kind = 4), INTENT(out) :: n_sweeps_therm, n_sweeps_stead

    CHARACTER(len = 4) :: si_sample
    CHARACTER(len = 30) :: filename
    INTEGER(kind = 4) :: i_line, ios, n0

    n_sweeps_therm = 0
    n_sweeps_stead = 0

    WRITE(si_sample, '(i0.4)') i_sample
    filename=TRIM("stream_s"//si_sample//".dat")
    OPEN(slot, file=filename, status="old")
    READ (slot, '()')

    DO i_line = 1, 10000, 1
       READ (slot, *, iostat = ios) n0
       IF (ios == 0) THEN
          n_sweeps_therm = n_sweeps_therm + 1
       ELSE
          EXIT
       END IF
    END DO

    DO i_line = 1, 10000, 1
       READ (slot, *, iostat = ios) n0
       IF (ios == 0) THEN
          n_sweeps_stead = n_sweeps_stead + 1
       ELSE
          EXIT
       END IF
    END DO

    CLOSE(slot)
  END SUBROUTINE getNumSweeps

  SUBROUTINE getStatsSamples(slot, n_samples, stat_sample_e, stat_sample_a)
    INTEGER(kind = 4), INTENT(in) :: slot, n_samples
    INTEGER(kind = 4), INTENT(inout) :: stat_sample_e(1:)
    INTEGER(kind = 4), INTENT(inout) :: stat_sample_a(1:)

    INTEGER(kind = 4) :: i_sample, n_dum, i_dum, e_dum, a_dum

    OPEN(slot, file="list_samples.dat", status="old")
    DO i_sample = 1, n_samples, 1
       READ (slot, *) i_dum, e_dum, a_dum
       stat_sample_e(i_dum) = e_dum
       stat_sample_a(i_dum) = a_dum
    END DO
    CLOSE(slot)
  END SUBROUTINE getStatsSamples

  SUBROUTINE inputParameters
    ! WRITE(0, '(a)') "len_x, len_z, J, beta, vel, n_sweeps_therm, n_sweeps_stead, id_init, id_bound, n_clones = ?"
    ! WRITE(0, '(a)') "(id_init: 1. all-up, 2. DW, 3. random)"
    ! WRITE(0, '(a)') "(id_bound: 1. anti-parallel, 2. parallel, 3. free)"
    READ(*, *) len_x, len_z, J, beta, vel, n_sweeps_therm, n_sweeps_stead, id_init, id_bound, n_samples

    n_steps = len_x * len_z
    n_sweeps = n_sweeps_therm + n_sweeps_stead
  END SUBROUTINE inputParameters

  SUBROUTINE makeProbArray
    prob(-1:1, -1:1, -1:1, -1:1, -1:1) = 1.0d0
    WHERE ( deltaE > 0.0d0 )
       prob(-1:1, -1:1, -1:1, -1:1, -1:1) = &
            EXP(- beta * deltaE(-1:1, -1:1, -1:1, -1:1, -1:1))
    END WHERE
  END SUBROUTINE makeProbArray

  SUBROUTINE makeDeltaEArray
    INTEGER(kind = 4) :: center, east, west, south, north

    DO center = -1, 1, 1
       DO east = -1, 1, 1
          DO west = -1, 1, 1
             DO south = -1, 1, 1
                DO north = -1, 1, 1
                   deltaE(center, east, west, south, north)=&
                   2 * J * &
                   DBLE(center * (east + west + south + north))
                END DO
             END DO
          END DO
       END DO
    END DO
  END SUBROUTINE makeDeltaEArray

  SUBROUTINE calcEnergy(spin, energy)
    INTEGER(kind = 4), INTENT(in) :: spin(1:, 1:)
    REAL(kind = 8), INTENT(out) :: energy
    INTEGER(kind = 4) :: x, z, east, west, south, north

    energy = 0.0d0
    DO x = 1, len_x
       DO z = 1, len_z
          CALL set_direction(id_bound, spin, x, z, east, west, south, north)
          energy = energy &
               - J * DBLE(spin(x,z) * (east + west + south + north))
       END DO
    END DO
    energy = 0.5d0 * energy
  END SUBROUTINE calcEnergy

  SUBROUTINE calcSlipplaneEnergy(spin, energy)
    INTEGER(kind = 4), INTENT(in) :: spin(1:, 1:)
    REAL(kind = 8), INTENT(out) :: energy

    INTEGER(kind = 4) :: x, east, west, south, north

    energy = 0.0d0
    DO x = 1, len_x
       CALL set_direction(id_bound, spin, x, len_z / 2, &
            east, west, south, north)
       energy = energy - J * DBLE(spin(x, len_z / 2) * (east + west + south + north))
    END DO
  END SUBROUTINE calcSlipplaneEnergy

  SUBROUTINE set_direction(id_bound, spin, x, z, east, west, south, north)
    INTEGER(kind = 4), INTENT(in) :: id_bound, spin(1:, 1:), x, z
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

    SELECT CASE (id_bound)
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
  END SUBROUTINE set_direction

  SUBROUTINE initializeSpin(id_init, spin)
    INTEGER(kind = 4), INTENT(in) :: id_init
    INTEGER(kind = 4), INTENT(out) :: spin(1:, 1:)

    INTEGER(kind = 4) :: seed_spin, err_spin
    INTEGER(kind = 4) :: x, z
    TYPE(VSL_STREAM_STATE) :: str_spin

    SELECT CASE (id_init)
    CASE (1)
       spin(1:len_x, 1:len_z) = 1
    CASE (2)
       spin(1:len_x, 1:len_z / 2) = 1
       spin(1:len_x, len_z / 2 + 1:len_z) = -1
    CASE (3)
       CALL system_CLOCK(seed_spin)
       CALL initializeRN(seed_spin, str_spin, err_spin)
       DO z = 1, len_z, 1
          CALL generateRN_int(str_spin, 0, 1, len_x, &
               spin(1:len_x, z), err_spin)
       END DO
       spin(1:len_x, 1:len_z) = 2 * spin(1:len_x, 1:len_z) - 1
    END SELECT
  END SUBROUTINE initializeSpin

  SUBROUTINE step_singleflip(spin, x, z, p, relax)
    INTEGER(kind = 4), INTENT(inout) :: spin(1:, 1:)
    INTEGER(kind = 4), INTENT(in) :: x, z
    REAL(kind = 8), INTENT(in) :: p
    REAL(kind = 8), INTENT(out) :: relax

    REAL(kind = 8) :: en_loc
    INTEGER(kind = 4) :: east, west, south, north

    CALL set_direction(id_bound, spin, x, z, east, west, south, north)

    IF (p <= prob(spin(x, z), east, west, south, north)) THEN
       en_loc =  - spin(x, z) * (east + west + south + north)
       relax = - 2 * en_loc
       spin(x, z) = - spin(x, z)
    ELSE
       relax = 0.0d0
    END IF
  END SUBROUTINE step_singleflip

  SUBROUTINE sweep_singleflip(spin, n_steps, rn_x, rn_z, rn_p, diss)
    INTEGER(kind = 4), INTENT(inout) :: spin(1:, 1:)
    INTEGER(kind = 4), INTENT(in) :: n_steps
    INTEGER(kind = 4), INTENT(in) :: rn_x(1:), rn_z(1:)
    REAL(kind = 8), INTENT(in) :: rn_p(1:)
    REAL(kind = 8), INTENT(out) :: diss

    INTEGER(kind = 4) :: i_step
    REAL(kind = 8) :: relax

    diss = 0.0d0
    DO i_step = 1, n_steps, 1
       CALL step_singleflip(spin, &
            rn_x(i_step), rn_z(i_step), rn_p(i_step), relax)
       diss = diss + relax
    END DO
  END SUBROUTINE sweep_singleflip

  SUBROUTINE shiftUpperHalf(spin, pump, temp_spin)
    INTEGER(kind = 4), INTENT(inout) :: spin(1:, 1:)
    REAL(kind = 8), INTENT(out) :: pump
    INTEGER(kind = 4), INTENT(inout) :: temp_spin(1:, 1:)
    REAL(kind = 8) :: prev, next

    CALL calcSlipplaneEnergy(spin, prev)
    temp_spin(1:len_x, 1:len_z / 2) = spin(1:len_x, 1:len_z / 2)
    temp_spin(1:len_x, 1:len_z / 2) = &
         CSHIFT(temp_spin(1:len_x, 1:len_z / 2), shift=1, dim=1)
    spin(1:len_x, 1:len_z / 2) = temp_spin(1:len_x, 1:len_z / 2)
    CALL calcSlipplaneEnergy(spin, next)

    pump = next - prev
  END SUBROUTINE shiftUpperHalf

  SUBROUTINE importSnapshot(slot, filename, spin)
    INTEGER(kind = 4), INTENT(in) :: slot
    CHARACTER(len = *, kind = 1), INTENT(in) :: filename
    INTEGER(kind = 4), INTENT(out) :: spin(1:, 1:)

    INTEGER(kind = 4) :: x, z, dum_x, dum_z

    OPEN(slot, file=filename, status="old")
    DO z = 1, len_z, 1
       READ(slot, '()')
       DO x = 1, len_x, 1
          READ(slot, *) dum_x, dum_z, spin(x, z)
       END DO
    END DO
    CLOSE(slot)
  END SUBROUTINE importSnapshot

  SUBROUTINE exportSnapshot(slot, filename, spin)
    INTEGER(kind = 4), INTENT(in) :: slot
    CHARACTER(len = *, kind = 1), INTENT(in) :: filename
    INTEGER(kind = 4), INTENT(in) :: spin(1:, 1:)

    INTEGER(kind = 4) :: x, z

    OPEN(slot, file=filename, status="replace")
    WRITE(slot, '(a)') "# x, z, spin"
    DO z = 1, len_z, 1
       DO x = 1, len_x, 1
          WRITE(slot, '(i0.4, a, i0.4, a, i0.4)') &
               x, ", ", z, ", ", spin(x, z)
       END DO
       WRITE(slot, '()')
    END DO
    CLOSE(slot)
  END SUBROUTINE exportSnapshot
END MODULE main_procedures
