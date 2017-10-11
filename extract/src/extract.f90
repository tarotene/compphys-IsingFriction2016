PROGRAM main
  USE global_variables
  USE main_procedures
  IMPLICIT NONE

  INTEGER(kind = 4) :: i_sweep, i_dum, z, z_dum, n_samples
  DOUBLE PRECISION, ALLOCATABLE :: pump(:), diss(:), energy(:)
  DOUBLE PRECISION, ALLOCATABLE :: fluc_pump(:), fluc_diss(:), fluc_energy(:)
  REAL(kind = 8) :: mean_pump, mean_diss, mean_energy
  REAL(kind = 8) :: err_pump, err_diss, err_energy
  CHARACTER(len = 50) :: file_stream, file_m_z

  READ(*, *) len_z, n_sweeps_therm, n_sweeps_stead, beta, begin_sweep, &
  n_samples

  duration = n_sweeps_stead - begin_sweep + 1

  ALLOCATE(pump(1:n_sweeps_stead), fluc_pump(1:n_sweeps_stead))
  ALLOCATE(diss(1:n_sweeps_stead), fluc_diss(1:n_sweeps_stead))
  ALLOCATE(energy(1:n_sweeps_stead), fluc_energy(1:n_sweeps_stead))
  slot = 10

  OPEN(slot, file="stream.dat", status="old")
  READ(slot, '()')
  DO i_sweep = 1, n_sweeps_therm, 1
     READ(slot, '()')
  END DO
  READ(slot, '()')
  DO i_sweep = 1, n_sweeps_stead, 1
     READ(slot, *)  i_dum, pump(i_sweep), diss(i_sweep), energy(i_sweep), &
     fluc_pump(i_sweep), fluc_diss(i_sweep), fluc_energy(i_sweep)
  END DO
  CLOSE(slot)

  CALL calcMeanErr(n_samples, pump(begin_sweep:n_sweeps_stead), &
  fluc_pump(begin_sweep:n_sweeps_stead), mean_pump, err_pump)
  CALL calcMeanErr(n_samples, diss(begin_sweep:n_sweeps_stead), &
  fluc_diss(begin_sweep:n_sweeps_stead), mean_diss, err_diss)
  CALL calcMeanErr(n_samples, energy(begin_sweep:n_sweeps_stead), &
  fluc_energy(begin_sweep:n_sweeps_stead), mean_energy, err_energy)

  WRITE(*, '( f0.4, a, f0.4, a, f0.4, a, f0.4, a, &
       f0.4, a, f0.4, a, f0.4)') &
       beta, ", ", mean_pump, ", ", mean_diss, ", ", mean_energy, ", ", &
       err_pump, ", ", err_diss, ", ", err_energy
END PROGRAM main
