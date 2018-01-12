PROGRAM main
  !$  USE omp_lib
  USE mod_global
  USE mod_proc_int
  USE mod_proc_file

  IMPLICIT NONE
  ! INTEGER(kind = 4) :: i_sweep, i_sample
  
  ! observables
  REAL(kind = 8), ALLOCATABLE :: ave_pump(:), ave_diss(:), ave_energy(:)
  REAL(kind = 8), ALLOCATABLE :: ave_pump_old(:), ave_pump_new(:)
  REAL(kind = 8), ALLOCATABLE :: ave_diss_old(:), ave_diss_new(:)
  REAL(kind = 8), ALLOCATABLE :: ave_energy_old(:), ave_energy_new(:)
  REAL(kind = 8), ALLOCATABLE :: fluc_pump(:), fluc_diss(:), fluc_energy(:)
  REAL(kind = 8), ALLOCATABLE :: fluc_pump_old(:), fluc_pump_new(:)
  REAL(kind = 8), ALLOCATABLE :: fluc_diss_old(:), fluc_diss_new(:)
  REAL(kind = 8), ALLOCATABLE :: fluc_energy_old(:), fluc_energy_new(:)
  REAL(kind = 8), ALLOCATABLE :: ave_m_z(:, :), fluc_m_z(:, :)
  REAL(kind = 8), ALLOCATABLE :: ave_m_z_old(:, :), ave_m_z_new(:, :)
  REAL(kind = 8), ALLOCATABLE :: fluc_m_z_old(:, :), fluc_m_z_new(:, :)

  ! loop variables
  INTEGER(kind = 4) :: z, n_steps, i_sweep, i_sample
  CHARACTER(len = 4, kind = 1) :: si_sample

  CHARACTER(:), ALLOCATABLE :: &
       filename_stream, filename_m_z

  ! slot variables
  INTEGER(kind = 4) :: slot_in_stream1, slot_in_stream2, slot_out_stream
  INTEGER(kind = 4) :: slot_in_m_z1, slot_in_m_z2, slot_out_m_z

  ! stat variables
  ! INTEGER(kind = 4) :: exist_list_parameters, exist_stat_samples
  INTEGER(kind = 4), ALLOCATABLE :: stat_sample_e(:)
  INTEGER(kind = 4), ALLOCATABLE :: stat_sample_a(:)

  CALL getListParameters_3d(10, "list_parameters.dat", &
       len_x, len_y,  len_z, J, beta, vel, &
       n_sweeps_therm, n_sweeps_stead, id_IC, id_BC, n_samples, &
       onoff_stream, onoff_m_z)
  ALLOCATE(stat_sample_e(1:n_samples), stat_sample_a(1:n_samples))
  CALL getStatSamples(20, "stat_samples.dat", &
       n_samples, stat_sample_e(1:n_samples), stat_sample_a(1:n_samples))

  n_samples_new = SUM(stat_sample_e(1:n_samples))
  n_samples_old = n_samples - n_samples_new

  !adjustment program to machine
  n_ths = 1
  !$  n_ths = omp_get_max_threads()
  !$	CALL omp_set_num_threads(n_ths)

  !allocation stream
  n_sweeps = n_sweeps_therm + n_sweeps_stead
  ALLOCATE(ave_pump(1:n_sweeps))
  ALLOCATE(ave_diss(1:n_sweeps))
  ALLOCATE(ave_energy(1:n_sweeps))
  ALLOCATE(ave_pump_old(1:n_sweeps), ave_pump_new(1:n_sweeps))
  ALLOCATE(ave_diss_old(1:n_sweeps), ave_diss_new(1:n_sweeps))
  ALLOCATE(ave_energy_old(1:n_sweeps), ave_energy_new(1:n_sweeps))
  ALLOCATE(fluc_pump(1:n_sweeps))
  ALLOCATE(fluc_diss(1:n_sweeps))
  ALLOCATE(fluc_energy(1:n_sweeps))
  ALLOCATE(fluc_pump_old(1:n_sweeps), fluc_pump_new(1:n_sweeps))
  ALLOCATE(fluc_diss_old(1:n_sweeps), fluc_diss_new(1:n_sweeps))
  ALLOCATE(fluc_energy_old(1:n_sweeps), fluc_energy_new(1:n_sweeps))

  IF ( n_samples_old * onoff_stream > 0 ) THEN
     CALL readStream(slot_in_stream1, "stream.dat", &
          n_sweeps_therm, n_sweeps_stead, &
          ave_pump_old(1:n_sweeps), &
          ave_diss_old(1:n_sweeps), &
          ave_energy_old(1:n_sweeps), &
          fluc_pump_old(1:n_sweeps), &
          fluc_diss_old(1:n_sweeps), &
          fluc_energy_old(1:n_sweeps))
  END IF

  ave_pump_new(1:n_sweeps) = 0.0d0
  ave_diss_new(1:n_sweeps) = 0.0d0
  ave_energy_new(1:n_sweeps) = 0.0d0
  !$omp parallel do schedule(static, 1) default(none) &
  !$omp shared(n_samples_old, n_samples_new, n_samples, onoff_stream) &
  !$omp shared(n_sweeps_therm, n_sweeps_stead, n_sweeps) &
  !$omp private(slot_in_stream2, si_sample) &
  !$omp private(filename_stream) &
  !$omp reduction(+:ave_pump_new, ave_diss_new, ave_energy_new)
  DO i_sample = n_samples_old + 1, n_samples * onoff_stream, 1
     slot_in_stream2 = 10
     !$ slot_in_stream2 = 10 + omp_get_thread_num()

     WRITE(si_sample, '(i0.4)') i_sample
     filename_stream=TRIM("stream_s"//si_sample//".dat")

     CALL addNewStreamSample2Sum(slot_in_stream2, filename_stream, &
          n_sweeps_therm, n_sweeps_stead, &
          ave_pump_new(1:n_sweeps), &
          ave_diss_new(1:n_sweeps), &
          ave_energy_new(1:n_sweeps))
  END DO
  !$omp end parallel do
  ave_pump_new(1:n_sweeps) = ave_pump_new(1:n_sweeps) / DBLE(n_samples_new)
  ave_diss_new(1:n_sweeps) = ave_diss_new(1:n_sweeps) / DBLE(n_samples_new)
  ave_energy_new(1:n_sweeps) = ave_energy_new(1:n_sweeps) / DBLE(n_samples_new)

  fluc_pump_new(1:n_sweeps) = 0.0d0
  fluc_diss_new(1:n_sweeps) = 0.0d0
  fluc_energy_new(1:n_sweeps) = 0.0d0
  !$omp parallel do schedule(static, 1) default(none) &
  !$omp shared(n_samples_old, n_samples_new, n_samples, onoff_stream) &
  !$omp shared(n_sweeps_therm, n_sweeps_stead, n_sweeps) &
  !$omp shared(ave_pump_new, ave_diss_new, ave_energy_new) &
  !$omp private(slot_in_stream2, si_sample) &
  !$omp private(filename_stream) &
  !$omp reduction(+:fluc_pump_new, fluc_diss_new, fluc_energy_new)
  DO i_sample = n_samples_old + 1, n_samples * onoff_stream, 1
     slot_in_stream2 = 20
     !$ slot_in_stream2 = 20 + omp_get_thread_num()

     WRITE(si_sample, '(i0.4)') i_sample
     filename_stream=TRIM("stream_s"//si_sample//".dat")

     CALL addNewStreamSample2Sumsd(slot_in_stream2, filename_stream, &
          n_sweeps_therm, n_sweeps_stead, &
          ave_pump_new(1:n_sweeps), &
          ave_diss_new(1:n_sweeps), &
          ave_energy_new(1:n_sweeps), &
          fluc_pump_new(1:n_sweeps), &
          fluc_diss_new(1:n_sweeps), &
          fluc_energy_new(1:n_sweeps))
  END DO
  !$omp end parallel do
  fluc_pump_new(1:n_sweeps) = fluc_pump_new(1:n_sweeps) / &
       DBLE(n_samples_new)
  fluc_diss_new(1:n_sweeps) = fluc_diss_new(1:n_sweeps) / &
       DBLE(n_samples_new)
  fluc_energy_new(1:n_sweeps) = fluc_energy_new(1:n_sweeps) / &
       DBLE(n_samples_new)

  !$omp parallel do schedule(static, 1) default(none) &
  !$omp shared(n_sweeps, onoff_stream) &
  !$omp shared(n_samples_old, n_samples_new) &
  !$omp shared(ave_pump, fluc_pump) &
  !$omp shared(ave_pump_old, fluc_pump_old) &
  !$omp shared(ave_pump_new, fluc_pump_new) &
  !$omp shared(ave_diss, fluc_diss) &
  !$omp shared(ave_diss_old, fluc_diss_old) &
  !$omp shared(ave_diss_new, fluc_diss_new) &
  !$omp shared(ave_energy, fluc_energy) &
  !$omp shared(ave_energy_old, fluc_energy_old) &
  !$omp shared(ave_energy_new, fluc_energy_new)
  DO i_sweep = 1, n_sweeps * onoff_stream, 1
     CALL synthesizeSets(n_samples_old, n_samples_new, &
          ave_pump_old(i_sweep), fluc_pump_old(i_sweep), &
          ave_pump_new(i_sweep), fluc_pump_new(i_sweep), &
          ave_pump(i_sweep), fluc_pump(i_sweep))
     CALL synthesizeSets(n_samples_old, n_samples_new, &
          ave_diss_old(i_sweep), fluc_diss_old(i_sweep), &
          ave_diss_new(i_sweep), fluc_diss_new(i_sweep), &
          ave_diss(i_sweep), fluc_diss(i_sweep))
     CALL synthesizeSets(n_samples_old, n_samples_new, &
          ave_energy_old(i_sweep), fluc_energy_old(i_sweep), &
          ave_energy_new(i_sweep), fluc_energy_new(i_sweep), &
          ave_energy(i_sweep), fluc_energy(i_sweep))
  END DO
  !$omp end parallel do

  IF ( onoff_stream == 1 ) THEN
     CALL writeStream(slot_out_stream, "stream.dat", &
          n_sweeps_therm, n_sweeps_stead, &
          ave_pump(1:n_sweeps), ave_diss(1:n_sweeps), ave_energy(1:n_sweeps), &
          fluc_pump(1:n_sweeps), fluc_diss(1:n_sweeps), fluc_energy(1:n_sweeps))
  END IF

  DEALLOCATE(ave_pump, ave_diss, ave_energy)
  DEALLOCATE(ave_pump_old, ave_pump_new, ave_diss_old, ave_diss_new)
  DEALLOCATE(ave_energy_old, ave_energy_new)
  DEALLOCATE(fluc_pump, fluc_diss, fluc_energy)
  DEALLOCATE(fluc_pump_old, fluc_pump_new, fluc_diss_old, fluc_diss_new)
  DEALLOCATE(fluc_energy_old, fluc_energy_new)

  !allocation m_z
  ALLOCATE(ave_m_z(1:len_z, 1:n_sweeps), fluc_m_z(1:len_z, 1:n_sweeps))
  ALLOCATE(ave_m_z_old(1:len_z, 1:n_sweeps), ave_m_z_new(1:len_z, 1:n_sweeps))
  ALLOCATE(fluc_m_z_old(1:len_z, 1:n_sweeps), fluc_m_z_new(1:len_z, 1:n_sweeps))

  IF ( n_samples_old * onoff_m_z > 0 ) THEN
     CALL readM_z(slot_in_m_z1, "m_z.dat", len_z, n_sweeps, &
     ave_m_z_old(1:len_z, 1:n_sweeps), fluc_m_z_old(1:len_z, 1:n_sweeps))
  END IF

  ave_m_z_new(1:len_z, 1:n_sweeps) = 0.0d0
  !$omp parallel do schedule(static, 1) default(none) &
  !$omp shared(n_samples_old, n_samples_new, n_samples, onoff_m_z) &
  !$omp shared(n_sweeps_therm, n_sweeps_stead, n_sweeps, len_z) &
  !$omp private(slot_in_m_z2, si_sample) &
  !$omp private(filename_m_z) &
  !$omp reduction(+:ave_m_z_new)
  DO i_sample = n_samples_old + 1, n_samples * onoff_m_z, 1
     slot_in_m_z2 = 30
     !$ slot_in_m_z2 = 30 + omp_get_thread_num()

     WRITE(si_sample, '(i0.4)') i_sample
     filename_m_z=TRIM("m_z_s"//si_sample//".dat")

     CALL addNewM_zSample2Sum(slot_in_m_z2, filename_m_z, &
     len_z, n_sweeps, ave_m_z_new(1:len_z, 1:n_sweeps))
  END DO
  !$omp end parallel do
  ave_m_z_new(1:len_z, 1:n_sweeps) = ave_m_z_new(1:len_z, 1:n_sweeps) / &
  DBLE(n_samples_new)

  fluc_m_z_new(1:len_z, 1:n_sweeps) = 0.0d0
  !$omp parallel do default(none) &
  !$omp shared(n_samples_old, n_samples_new, n_samples, onoff_m_z) &
  !$omp shared(n_sweeps_therm, n_sweeps_stead, n_sweeps, len_z) &
  !$omp shared(ave_m_z_new) &
  !$omp private(slot_in_m_z2, si_sample) &
  !$omp private(filename_m_z) &
  !$omp reduction(+:fluc_m_z_new)
  DO i_sample = n_samples_old + 1, n_samples * onoff_m_z, 1
     slot_in_m_z2 = 40
     !$ slot_in_m_z2 = 40 + omp_get_thread_num()

     WRITE(si_sample, '(i0.4)') i_sample
     filename_m_z=TRIM("m_z_s"//si_sample//".dat")

     CALL addNewM_zSample2Sumsd(slot_in_m_z2, filename_m_z, &
          len_z, n_sweeps, &
          ave_m_z_new(1:len_z, 1:n_sweeps), fluc_m_z_new(1:len_z, 1:n_sweeps))
  END DO
  !$omp end parallel do
  fluc_m_z_new(1:len_z, 1:n_sweeps) = fluc_m_z_new(1:len_z, 1:n_sweeps) / &
  DBLE(n_samples_new)

  !$omp parallel do schedule(static, 1) default(none) &
  !$omp shared(n_sweeps, onoff_m_z) &
  !$omp shared(n_samples_old, n_samples_new, len_z) &
  !$omp shared(ave_m_z, fluc_m_z) &
  !$omp shared(ave_m_z_old, fluc_m_z_old) &
  !$omp shared(ave_m_z_new, fluc_m_z_new)
  DO i_sweep = 1, n_sweeps * onoff_m_z, 1
     DO z = 1, len_z, 1
        CALL synthesizeSets(n_samples_old, n_samples_new, &
             ave_m_z_old(z, i_sweep), fluc_m_z_old(z, i_sweep), &
             ave_m_z_new(z, i_sweep), fluc_m_z_new(z, i_sweep), &
             ave_m_z(z, i_sweep), fluc_m_z(z, i_sweep))
     END DO
  END DO
  !$omp end parallel do

  IF ( onoff_m_z == 1 ) THEN
     CALL writeM_z_3d(slot_out_m_z, "m_z.dat", len_x, len_y, len_z, &
     n_sweeps, ave_m_z(1:len_z, 1:n_sweeps), fluc_m_z(1:len_z, 1:n_sweeps))
  END IF

  DEALLOCATE(ave_m_z, fluc_m_z)
  DEALLOCATE(ave_m_z_old, ave_m_z_new, fluc_m_z_old, fluc_m_z_new)

  stat_sample_a(1:n_samples) = 1
  CALL refreshStatSamples(20, "stat_samples.dat", &
       n_samples, stat_sample_e(1:n_samples), stat_sample_a(1:n_samples))
END PROGRAM main
