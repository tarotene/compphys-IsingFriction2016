PROGRAM main
  USE mod_global
  USE mod_proc_file
  IMPLICIT NONE

  CHARACTER(:), ALLOCATABLE :: command1, command2
  INTEGER(4), ALLOCATABLE :: stat_sample_e(:), stat_sample_a(:)

  INTEGER(4) :: i_sample
  CHARACTER(4) :: si_sample

  CALL getListParameters_2d(10, "list_parameters.dat", &
       len_x, len_z, J, beta, vel, &
       n_sweeps_therm, n_sweeps_stead, id_IC, id_BC, n_samples, &
       onoff_stream, onoff_m_z)
  ALLOCATE(stat_sample_e(1:n_samples), stat_sample_a(1:n_samples))
  CALL getStatSamples(20, "stat_samples.dat", &
       n_samples, stat_sample_e(1:n_samples), stat_sample_a(1:n_samples))
  
  DO i_sample = 1, n_samples, 1
     IF ( stat_sample_e(i_sample) == 1 .AND. stat_sample_a(i_sample) == 1 ) THEN
        WRITE(si_sample, '(i0.4)') i_sample
        IF ( onoff_stream == 1 ) CALL system("rm stream_s"//si_sample//".dat")
        IF ( onoff_m_z == 1 ) CALL system("rm m_z_s"//si_sample//".dat")
        stat_sample_e(i_sample) = 0
     END IF
  END DO

  CALL refreshStatSamples(20, "stat_samples.dat", &
       n_samples, stat_sample_e(1:n_samples), stat_sample_a(1:n_samples))
END PROGRAM main
