! MODULE global_variables
!   IMPLICIT NONE
!
!   INTEGER(kind = 4) :: n_sweeps_therm, n_sweeps_stead, slot, begin_sweep
!   INTEGER(kind = 4) :: len_z
!   INTEGER(kind = 4) :: duration
!   DOUBLE PRECISION :: beta
! END MODULE global_variables

MODULE main_procedures
  USE global_variables
  IMPLICIT NONE
CONTAINS
  SUBROUTINE calcMeanVarErr(stream, ac, ct, mean, std_err)
    DOUBLE PRECISION, INTENT(in) :: stream(:)
    DOUBLE PRECISION, INTENT(out) :: ac(0:)
    DOUBLE PRECISION, INTENT(out) :: ct, mean
    DOUBLE PRECISION, INTENT(out) :: std_err

    INTEGER(kind = 4) :: t, t_prime

    mean = SUM(stream(:)) / DBLE(SIZE(stream(:)))

    ac(0:SIZE(stream(:)) / 2) = 0.0d0
    DO t = 0, SIZE(stream(:)) / 2, 1
       DO t_prime = 1, SIZE(stream(:)) / 2 - t, 1
          ac(t) = ac(t) + (stream(t_prime) - mean) * (stream(t_prime + t) - mean)
       END DO
       ac(t) = ac(t) / DBLE(SIZE(stream(:)) - t)
    END DO

    IF (ac(0) < 0.1d0) THEN
       std_err = 0.0d0
       ac(0) = 1.0d0
       ac(1:SIZE(stream(:)) / 2) = 0.0d0
    ELSE
       std_err = ac(0) / SQRT(DBLE(SIZE(stream(:))))
       ac(0:SIZE(stream(:)) / 2) = ac(0:SIZE(stream(:)) / 2) / ac(0)
    END IF

    ct = 0.0d0
    DO t = 0, SIZE(stream(:)) / 2, 1
       IF ( ac(t) <= 0.0d0 ) EXIT
       ct = ct + ac(t)
    END DO

    std_err = ct * std_err
  END SUBROUTINE calcMeanVarErr
END MODULE main_procedures

PROGRAM main
  USE global_variables
  USE main_procedures
  IMPLICIT NONE

  INTEGER(kind = 4) :: i_sweep, i_dum, z, z_dum
  DOUBLE PRECISION, ALLOCATABLE :: pump(:), diss(:), energy(:)
  DOUBLE PRECISION, ALLOCATABLE :: m_z(:, :), devDW(:), devDW_sq(:)
  ! DOUBLE PRECISION, ALLOCATABLE :: widthDW(:)
  DOUBLE PRECISION, ALLOCATABLE :: ac_pump(:), ac_diss(:), ac_energy(:)
  ! DOUBLE PRECISION, ALLOCATABLE :: ac_widthDW(:)
  DOUBLE PRECISION :: ct_pump, ct_diss, ct_energy
  ! DOUBLE PRECISION :: ct_widthDW
  DOUBLE PRECISION :: mean_pump, mean_diss, mean_energy, flucDW
  ! DOUBLE PRECISION :: mean_widthDW
  DOUBLE PRECISION :: std_err_pump, std_err_diss, std_err_energy
  ! DOUBLE PRECISION :: std_err_widthDW
  CHARACTER(len = 50) :: file_stream, file_m_z

  ! WRITE(0, '(a)') "n_sweeps_therm, n_sweeps_stead, beta, begin_sweep, filename = ?"
  READ(*, *) len_z, n_sweeps_therm, n_sweeps_stead, beta, begin_sweep, &
       file_stream, file_m_z

  duration = n_sweeps_stead - begin_sweep + 1

  ALLOCATE(pump(1:n_sweeps_stead))
  ALLOCATE(diss(1:n_sweeps_stead))
  ALLOCATE(energy(1:n_sweeps_stead))
  ALLOCATE(m_z(1:n_sweeps_stead, 1:len_z))
  ALLOCATE(devDW(1:n_sweeps_stead), devDW_sq(1:n_sweeps_stead))
  ! ALLOCATE(widthDW(1:n_sweeps_stead))
  ALLOCATE(ac_pump(0:(n_sweeps_stead - begin_sweep + 1) / 2))
  ALLOCATE(ac_diss(0:(n_sweeps_stead - begin_sweep + 1) / 2))
  ALLOCATE(ac_energy(0:(n_sweeps_stead - begin_sweep + 1) / 2))
  ! ALLOCATE(ac_widthDW(0:(n_sweeps_stead - begin_sweep + 1) / 2))
  slot = 10

  OPEN(slot, file=TRIM(file_stream), status="old")
  DO i_sweep = 1, n_sweeps_therm, 1
     READ(slot, '()')
  END DO
  DO i_sweep = 1, n_sweeps_stead, 1
     READ(slot, *)  i_dum, pump(i_sweep), diss(i_sweep), energy(i_sweep)
  END DO
  CLOSE(slot)

  OPEN(slot, file=TRIM(file_m_z), status="old")
  DO i_sweep = 1, n_sweeps_therm, 1
     DO z = 1, len_z, 1
        READ(slot, '()')
     END DO
     READ(slot, '()')
  END DO
  DO i_sweep = 1, n_sweeps_stead, 1
     DO z = 1, len_z, 1
        READ(slot, *)  i_dum, z_dum, m_z(i_sweep, z)
     END DO
     READ(slot, '()')
  END DO
  CLOSE(slot)

  ! DO i_sweep = 1, 100, 1
  !    DO z = 1, len_z, 1
  !       WRITE(*, '(a, i0.5, a, i0.3, a, f0.4)') &
  !       "m_z(", i_sweep, ", ", z, ") = ", m_z(i_sweep, z)
  !    END DO
  ! END DO

  ! DO i_sweep = 1, n_sweeps_stead, 1
  !   WRITE(*, '(a, i0.5, a, f0.4)') "widthDW(", i_sweep, ") = ", widthDW(i_sweep)
  ! END DO

  ! DO i_sweep = 1, 100, 1
  !   WRITE(*, '(a, i0.5, a, f0.4)') &
  !       "widthDW(", i_sweep, ") = ", widthDW(i_sweep)
  ! END DO

  CALL calcMeanVarErr(pump(begin_sweep:n_sweeps_stead), &
       ac_pump(:), ct_pump, &
       mean_pump, std_err_pump)
  CALL calcMeanVarErr(diss(begin_sweep:n_sweeps_stead), &
       ac_diss(:), ct_diss, &
       mean_diss, std_err_diss)
  CALL calcMeanVarErr(energy(begin_sweep:n_sweeps_stead), &
       ac_energy(:), ct_energy, &
       mean_energy, std_err_energy)

  devDW(1:n_sweeps_stead) = 0.5d0 * &
  (m_z(1:n_sweeps_stead, len_z / 2) + m_z(1:n_sweeps_stead, len_z / 2 + 1)) / &
  (m_z(1:n_sweeps_stead, len_z / 2) - m_z(1:n_sweeps_stead, len_z / 2 + 1))

  devDW_sq(1:n_sweeps_stead) = devDW(1:n_sweeps_stead) ** 2
  flucDW = SUM(devDW_sq(1:n_sweeps_stead)) / DBLE(n_sweeps_stead)

  WRITE(*, '( f0.4, a, f0.4, a, f0.4, a, f0.4, a, &
  f0.4, a, f0.4, a, f0.4, a, f0.4, a, f0.4, a, f0.4, a, f0.4)') &
  beta, ", ", mean_pump, ", ", ct_pump, ", ", std_err_pump, ", ", &
  mean_diss, ", ", ct_diss, ", ", std_err_diss, ", ", mean_energy, ", ", ct_diss, ", ", std_err_energy, ", ", flucDW

  ! DEALLOCATE(pump, diss, energy, widthDW, m_z, ac)
END PROGRAM main
