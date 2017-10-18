PROGRAM main
  USE global_variables
  USE main_procedures
  IMPLICIT NONE

  REAL(kind = 8), ALLOCATABLE :: pump_st(:, :)
  REAL(kind = 8), ALLOCATABLE :: diss_th(:, :), diss_st(:, :)
  REAL(kind = 8), ALLOCATABLE :: energy_th(:, :), energy_st(:, :)

  REAL(kind = 8), ALLOCATABLE :: ac_pump_st(:, :)
  REAL(kind = 8), ALLOCATABLE :: ac_diss_th(:, :), ac_diss_st(:, :)
  REAL(kind = 8), ALLOCATABLE :: ac_energy_th(:, :), ac_energy_st(:, :)

  REAL(kind = 8), ALLOCATABLE :: tau_diss_th(:)
  REAL(kind = 8), ALLOCATABLE :: tau_energy_th(:)
  REAL(kind = 8), ALLOCATABLE :: tau_pump_st(:), tau_diss_st(:)
  REAL(kind = 8), ALLOCATABLE :: tau_energy_st(:)

  REAL(kind = 8) :: str_dum1, str_dum2, str_dum3
  INTEGER(kind = 4) :: i_dum, i_sample, i_slot, i_sweep
  CHARACTER(len = 4, kind = 1) :: si_sample
  CHARACTER(:), ALLOCATABLE :: filename

  CALL getNumSamples(10, n_samples)
  CALL getNumSweeps(10, 1, n_sweeps_therm, n_sweeps_stead)

  ! WRITE(0, *) 1

  ALLOCATE(pump_st(0:n_samples, 1:n_sweeps_stead))
  ALLOCATE(diss_th(0:n_samples, 1:n_sweeps_therm))
  ALLOCATE(diss_st(0:n_samples, 1:n_sweeps_stead))
  ALLOCATE(energy_th(0:n_samples, 1:n_sweeps_therm))
  ALLOCATE(energy_st(0:n_samples, 1:n_sweeps_stead))

  ! WRITE(0, *) 1.1d0

  ALLOCATE(ac_pump_st(0:n_samples, 1:n_sweeps_stead))
  ALLOCATE(ac_diss_th(0:n_samples, 1:n_sweeps_stead))
  ALLOCATE(ac_diss_st(0:n_samples, 1:n_sweeps_stead))
  ALLOCATE(ac_energy_th(0:n_samples, 1:n_sweeps_therm))
  ALLOCATE(ac_energy_st(0:n_samples, 1:n_sweeps_stead))

  ! WRITE(0, *) 1.2d0

  ALLOCATE(tau_pump_st(0:n_samples))
  ALLOCATE(tau_diss_th(0:n_samples))
  ALLOCATE(tau_diss_st(0:n_samples))
  ALLOCATE(tau_energy_th(0:n_samples))
  ALLOCATE(tau_energy_st(0:n_samples))

  ! WRITE(0, *) 2

  !$omp parallel do default(none) &
  !$omp private(si_sample, i_slot, i_sweep, i_dum, filename) &
  !$omp private(str_dum1, str_dum2, str_dum3) &
  !$omp shared(n_samples, n_sweeps_therm, n_sweeps_stead) &
  !$omp shared(diss_th, energy_th, pump_st, diss_st, energy_st)
  DO i_sample = 1, n_samples, 1
     i_slot = 10 + i_sample
     WRITE(si_sample, '(i0.4)') i_sample
     filename=TRIM("stream_s"//si_sample//".dat")
     OPEN(i_slot, file=filename, status="old")
     READ(i_slot, '()')
     DO i_sweep = 1, n_sweeps_therm, 1
        READ(i_slot, *)  i_dum, &
             diss_th(i_sample, i_sweep), energy_th(i_sample, i_sweep), &
             str_dum1, str_dum2, str_dum3
     END DO
     READ(i_slot, '()')
     DO i_sweep = 1, n_sweeps_stead, 1
        READ(i_slot, *)  i_dum, &
             pump_st(i_sample, i_sweep), &
             diss_st(i_sample, i_sweep), energy_st(i_sample, i_sweep), &
             str_dum1, str_dum2, str_dum3
     END DO
     CLOSE(i_slot)
     DEALLOCATE(filename)
  END DO
  !$omp end parallel do

  ! WRITE(0, *) 3

  i_slot = 10
  OPEN(i_slot, file="stream.dat", status="old")
  READ(i_slot, '()')
  DO i_sweep = 1, n_sweeps_therm, 1
     READ(i_slot, *)  i_dum, &
          pump_th(0, i_sweep), &
          diss_th(0, i_sweep), energy_th(0, i_sweep), &
          str_dum1, str_dum2, str_dum3
  END DO
  READ(i_slot, '()')
  DO i_sweep = 1, n_sweeps_stead, 1
     READ(i_slot, *)  i_dum, &
          pump_st(0, i_sweep), &
          diss_st(0, i_sweep), energy_st(0, i_sweep), &
          str_dum1, str_dum2, str_dum3
  END DO
  CLOSE(i_slot)

  ! WRITE(0, *) 4

  DO i_sample = 0, n_samples, 1
     CALL calcAutocorrelation(pump_th(i_sample, 1:n_sweeps_therm), &
          ac_pump_th(i_sample, 0:n_sweeps_therm / 2))
     CALL calcAutocorrelation(pump_st(i_sample, 1:n_sweeps_stead), &
          ac_pump_st(i_sample, 0:n_sweeps_stead / 2))
     CALL calcAutocorrelation(diss_th(i_sample, 1:n_sweeps_therm), &
          ac_diss_th(i_sample, 0:n_sweeps_therm / 2))
     CALL calcAutocorrelation(diss_st(i_sample, 1:n_sweeps_stead), &
          ac_diss_st(i_sample, 0:n_sweeps_stead / 2))
     CALL calcAutocorrelation(energy_th(i_sample, 1:n_sweeps_therm), &
          ac_energy_th(i_sample, 0:n_sweeps_therm / 2))
     CALL calcAutocorrelation(energy_st(i_sample, 1:n_sweeps_stead), &
          ac_energy_st(i_sample, 0:n_sweeps_stead / 2))
  END DO

  ! WRITE(0, *) 5

  !$omp parallel do default(none) &
  !$omp private(si_sample, i_slot, i_sweep, i_dum, filename) &
  !$omp private(str_dum1, str_dum2, str_dum3) &
  !$omp shared(n_samples, n_sweeps_therm, n_sweeps_stead) &
  !$omp shared(ac_diss_th, ac_energy_th)
  DO i_sample = 1, n_samples, 1
     i_slot = 10 + i_sample
     WRITE(si_sample, '(i0.4)') i_sample
     filename=TRIM("autocorrelation_th_s"//si_sample//".dat")
     OPEN(i_slot, file=filename, status="replace")
     WRITE(i_slot, '(a)') "# i_sweep, ac_pump, ac_diss, ac_energy"
     DO i_sweep = 0, n_sweeps_therm / 2, 1
        WRITE(i_slot, '(i5, a, f0.4, a, f0.4, a, f0.4)')  i_sweep, ", ", &
             ac_diss_th(i_sample, i_sweep), ", ", &
             ac_energy_th(i_sample, i_sweep)
     END DO
     CLOSE(i_slot)
     DEALLOCATE(filename)
  END DO
  !$omp end parallel do

  ! WRITE(0, *) 6

  i_slot = 10
  OPEN(i_slot, file="autocorrelation_th.dat", status="replace")
  WRITE(i_slot, '(a)') "# i_sweep, ac_pump, ac_diss, ac_energy"
  DO i_sweep = 0, n_sweeps_therm / 2, 1
     WRITE(i_slot, '(i5, a, f0.4, a, f0.4, a, f0.4)')  i_sweep, ", ", &
          ac_diss_th(0, i_sweep), ", ", &
          ac_energy_th(0, i_sweep)
  END DO
  CLOSE(i_slot)

  ! WRITE(0, *) 7

  !$omp parallel do default(none) &
  !$omp private(si_sample, i_slot, i_sweep, i_dum, filename) &
  !$omp private(str_dum1, str_dum2, str_dum3) &
  !$omp shared(n_samples, n_sweeps_therm, n_sweeps_stead) &
  !$omp shared(ac_pump_st, ac_diss_st, ac_energy_st)
  DO i_sample = 1, n_samples, 1
     i_slot = 10 + i_sample
     WRITE(si_sample, '(i0.4)') i_sample
     filename=TRIM("autocorrelation_st_s"//si_sample//".dat")
     OPEN(i_slot, file=filename, status="replace")
     WRITE(i_slot, '(a)') "# i_sweep, ac_pump, ac_diss, ac_energy"
     DO i_sweep = 0, n_sweeps_stead / 2, 1
        WRITE(i_slot, '(i5, a, f0.4, a, f0.4, a, f0.4)')  i_sweep, ", ", &
             ac_pump_st(i_sample, i_sweep), ", ", &
             ac_diss_st(i_sample, i_sweep), ", ", &
             ac_energy_st(i_sample, i_sweep)
     END DO
     CLOSE(i_slot)
     DEALLOCATE(filename)
  END DO
  !$omp end parallel do

  ! WRITE(0, *) 8

  i_slot = 10
  OPEN(i_slot, file="autocorrelation_st.dat", status="replace")
  WRITE(i_slot, '(a)') "# i_sweep, ac_pump, ac_diss, ac_energy"
  DO i_sweep = 0, n_sweeps_stead / 2, 1
     WRITE(i_slot, '(i5, a, f0.4, a, f0.4, a, f0.4)')  i_sweep, ", ", &
          ac_pump_st(0, i_sweep), ", ", &
          ac_diss_st(0, i_sweep), ", ", &
          ac_energy_st(0, i_sweep)
  END DO
  CLOSE(i_slot)

  ! WRITE(0, *) 9

  DO i_sample = 0, n_samples, 1
     CALL calcCorrelationTime(ac_pump_st(i_sample, 1:n_sweeps_therm), &
          tau_pump_st(i_sample))
     CALL calcCorrelationTime(ac_diss_th(i_sample, 1:n_sweeps_therm), &
          tau_diss_th(i_sample))
     CALL calcCorrelationTime(ac_diss_st(i_sample, 1:n_sweeps_therm), &
          tau_diss_st(i_sample))
     CALL calcCorrelationTime(ac_energy_th(i_sample, 1:n_sweeps_therm), &
          tau_energy_th(i_sample))
     CALL calcCorrelationTime(ac_energy_st(i_sample, 1:n_sweeps_therm), &
          tau_energy_st(i_sample))
  END DO

  ! WRITE(0, *) 10

  beta = 0.0d0

  WRITE(*, '(f0.4, a, f0.4, a, f0.4, a, &
       f0.4, a, f0.4, a, f0.4)') beta, ", ", &
       tau_pump_st(0), ", ", &
       tau_diss_th(0), ", ", tau_diss_st(0), ", ", &
       tau_energy_th(0), ", ", tau_energy_st(0)

  WRITE(0, *) 11

END PROGRAM main
