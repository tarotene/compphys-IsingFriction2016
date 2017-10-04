! MODULE global_variables
!   IMPLICIT NONE
!
!   !parameters
!   INTEGER(kind = 4), SAVE :: n_samples, n_samples_old, n_samples_new
! END MODULE global_variables

! MODULE main_procedures
!   IMPLICIT NONE
!
! CONTAINS
!   SUBROUTINE getNumSamples(slot, n_samples)
!     INTEGER(kind = 4), INTENT(in) :: slot
!     INTEGER(kind = 4), INTENT(out) :: n_samples
!
!     INTEGER(kind = 4) :: i_line, ios, n_dum
!
!     n_samples = 0
!     OPEN(slot, file="list_samples.dat", status="old")
!     DO i_line = 1, 10000, 1
!        READ (slot, *, iostat = ios) n_dum
!        IF (ios < 0) EXIT
!        n_samples = n_samples + 1
!     END DO
!     CLOSE(slot)
!   END SUBROUTINE getNumSamples
!
!   SUBROUTINE getStatsSamples(slot, n_samples, stat_sample_e, stat_sample_a)
!     INTEGER(kind = 4), INTENT(in) :: slot, n_samples
!     INTEGER(kind = 4), INTENT(inout) :: stat_sample_e(1:)
!     INTEGER(kind = 4), INTENT(inout) :: stat_sample_a(1:)
!
!     INTEGER(kind = 4) :: i_sample, n_dum, i_dum, e_dum, a_dum
!
!     OPEN(slot, file="list_samples.dat", status="old")
!     DO i_sample = 1, n_samples, 1
!        READ (slot, *) i_dum, e_dum, a_dum
!        stat_sample_e(i_dum) = e_dum
!        stat_sample_a(i_dum) = a_dum
!     END DO
!     CLOSE(slot)
!   END SUBROUTINE getStatsSamples
! END MODULE main_procedures

PROGRAM main
  USE global_variables
  USE main_procedures
  IMPLICIT NONE

  CHARACTER(:), ALLOCATABLE :: command1, command2
  INTEGER(4), ALLOCATABLE :: stat_sample_e(:), stat_sample_a(:)
  INTEGER(4) :: slot_in_stats1, slot_out_stats

  INTEGER(4) :: i_sample
  CHARACTER(4) :: si_sample

  slot_in_stats1 = 21
  slot_out_stats = 20
  ! n_samples = 10
  CALL getNumSamples(slot_in_stats1, n_samples)
  ALLOCATE(stat_sample_e(1:n_samples), stat_sample_a(1:n_samples))
  CALL getStatsSamples(slot_in_stats1, n_samples, stat_sample_e, stat_sample_a)

  OPEN(slot_out_stats, file="list_samples.dat", status="old")
  DO i_sample = 1, n_samples, 1
     IF ( stat_sample_e(i_sample) == 1 .AND. stat_sample_a(i_sample) == 1 ) THEN
        WRITE(si_sample, '(i0.4)') i_sample
        command1 = "rm stream_s"//si_sample//".dat"
        command2 = "rm m_z_s"//si_sample//".dat"
        ! WRITE(*, '("command1 = ", a)') command1
        ! WRITE(*, '("command2 = ", a)') command2
        CALL system(command1)
        CALL system(command2)
        WRITE(slot_out_stats, '(i0.4, a, i0.4, a, i0.4)') i_sample, ", ", 0, ", ", 1
     ELSE
        WRITE(slot_out_stats, '(i0.4, a, i0.4, a, i0.4)') i_sample, ", ", stat_sample_e(i_sample), ", ", stat_sample_a(i_sample)
     END IF
  END DO
END PROGRAM main
