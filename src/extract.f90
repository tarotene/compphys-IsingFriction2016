PROGRAM main
  USE global_variables
  USE main_procedures
  IMPLICIT NONE

  INTEGER(kind = 4) :: i_sweep, i_tsample, i_dum, z, z_dum, i_sample
  INTEGER(kind = 4) :: n_samples, n_tsamples, n_tstrides
  DOUBLE PRECISION, ALLOCATABLE :: pump(:, :), diss(:, :), energy(:, :)
  DOUBLE PRECISION, ALLOCATABLE :: fluc_pump(:, :), fluc_diss(:, :), &
       fluc_energy(:, :)
  REAL(kind = 8) :: ave_pump, ave_diss, ave_energy
  REAL(kind = 8) :: err_pump, err_diss, err_energy
  CHARACTER(:), ALLOCATABLE :: filename
  CHARACTER(len = 4) :: si_sample

  READ(*, *) len_z, n_sweeps_therm, n_sweeps_stead, beta, n_samples

  n_tsamples = 100
  n_tstrides = 10

  ! duration = n_sweeps_stead - begin_sweep + 1

  ALLOCATE(pump(1:n_samples, 1:n_sweeps_stead))
  ALLOCATE(fluc_pump(1:n_samples, 1:n_sweeps_stead))
  ALLOCATE(diss(1:n_samples, 1:n_sweeps_stead))
  ALLOCATE(fluc_diss(1:n_samples, 1:n_sweeps_stead))
  ALLOCATE(energy(1:n_samples, 1:n_sweeps_stead))
  ALLOCATE(fluc_energy(1:n_samples, 1:n_sweeps_stead))

  !$omp parallel do default(none) &
  !$omp private(si_sample, filename, i_sweep, i_dum) &
  !$omp shared(n_samples, n_sweeps_therm, n_sweeps_stead) &
  !$omp shared(pump, diss, energy, fluc_pump, fluc_diss, fluc_energy)
  DO i_sample = 1, n_samples, 1
     WRITE(si_sample, '(i0.4)') i_sample
     filename=TRIM("stream_s"//si_sample//".dat")
     OPEN(i_sample, file=filename, status="old")
     READ(i_sample, '()')
     DO i_sweep = 1, n_sweeps_therm, 1
        READ(i_sample, '()')
     END DO
     READ(i_sample, '()')
     DO i_sweep = 1, n_sweeps_stead, 1
        READ(i_sample, *)  i_dum, &
             pump(i_sample, i_sweep), &
             diss(i_sample, i_sweep), energy(i_sample, i_sweep), &
             fluc_pump(i_sample, i_sweep), &
             fluc_diss(i_sample, i_sweep), fluc_energy(i_sample, i_sweep)
     END DO
     CLOSE(i_sample)
  END DO
  !$omp end parallel do

  !observation
  ave_pump = 0.0d0
  DO i_tsample = 0, n_tsamples - 1, 1
     ave_pump = ave_pump + &
          SUM(pump(1:n_samples, n_sweeps_stead - n_tstrides * i_tsample)) / &
          DBLE(n_samples)
  END DO
  ave_pump = ave_pump / DBLE(n_tsamples)

  err_pump = 0.0d0
  DO i_sample = 1, n_samples, 1
     DO i_tsample = 0, n_tsamples - 1, 1
        err_pump = err_pump + &
             (pump(i_sample, n_sweeps_stead - n_tstrides * i_tsample) - ave_pump) ** 2
     END DO
     err_pump = err_pump / DBLE(n_tsamples)
  END DO
  err_pump = err_pump / DBLE(n_samples)
  err_pump = SQRT(err_pump / DBLE(n_tsamples * n_samples))

  ! CALL calcMeanErr(n_samples, pump(begin_sweep:n_sweeps_stead), &
  ! fluc_pump(begin_sweep:n_sweeps_stead), mean_pump, err_pump)
  ! CALL calcMeanErr(n_samples, diss(begin_sweep:n_sweeps_stead), &
  ! fluc_diss(begin_sweep:n_sweeps_stead), mean_diss, err_diss)
  ! CALL calcMeanErr(n_samples, energy(begin_sweep:n_sweeps_stead), &
  ! fluc_energy(begin_sweep:n_sweeps_stead), mean_energy, err_energy)

  WRITE(*, '( f0.4, a, f0.4, a, f0.4, a, f0.4, a, &
       f0.4, a, f0.4, a, f0.4)') &
       beta, ", ", ave_pump, ", ", err_pump
END PROGRAM main
