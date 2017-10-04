! INCLUDE 'mkl_vsl.f90'

! MODULE global_variables
!   USE MKL_VSL_TYPE
!   USE MKL_VSL
!
!   IMPLICIT NONE
!
!   !input variables
!   INTEGER(kind = 4), SAVE :: len_x, len_z, vel, &
!        n_sweeps_therm, n_sweeps_stead, id_init, id_bound, &
!        n_samples
!   REAL(kind = 8), SAVE :: J, beta
!
!   !reduced variables
!   INTEGER(kind = 4), SAVE :: n_steps, n_sweeps
!   REAL(kind = 8), SAVE :: prob(-1:1, -1:1, -1:1, -1:1, -1:1)
!   REAL(kind = 8), SAVE :: deltaE(-1:1, -1:1, -1:1, -1:1, -1:1)
!
!   !rng variables
!   TYPE (VSL_STREAM_STATE), ALLOCATABLE :: str_x(:), str_z(:), str_prob(:)
! END MODULE global_variables

! MODULE mod_rng
!   USE global_variables
!   USE MKL_VSL_TYPE
!   USE MKL_VSL
!
!   IMPLICIT NONE
! CONTAINS
!   SUBROUTINE initializeRN(seed, str, err)
!     INTEGER(kind = 4), INTENT(in) :: seed
!     TYPE (VSL_STREAM_STATE), INTENT(out) :: str
!     INTEGER(kind = 4), INTENT(out) :: err
!
!     INTEGER(kind = 4) :: brng
!
!     brng = VSL_BRNG_MT19937
!     err = vslnewstream(str, brng, seed)
!   END SUBROUTINE initializeRN
!
!   SUBROUTINE generateRN_int(str, lb, ub, len_rns, rn, err)
!     TYPE (VSL_STREAM_STATE), INTENT(in) :: str
!     INTEGER(kind = 4), INTENT(in) :: lb, ub, len_rns
!     INTEGER(kind = 4), INTENT(out) :: rn(1:)
!     INTEGER(kind = 4), INTENT(out) :: err
!
!     INTEGER(kind = 4) :: method
!
!     method = VSL_RNG_METHOD_UNIFORM_STD
!     err = virnguniform(method, str, len_rns, rn, lb, ub + 1)
!   END SUBROUTINE generateRN_int
!
!   SUBROUTINE generateRN_real(str, lb, ub, len_rns, rn, err)
!     TYPE (VSL_STREAM_STATE), INTENT(in) :: str
!     REAL(kind = 8), INTENT(in) :: lb, ub
!     INTEGER(kind = 4), INTENT(in) :: len_rns
!     REAL(kind = 8), INTENT(out) :: rn(1:)
!     INTEGER(kind = 4), INTENT(out) :: err
!
!     INTEGER(kind = 4) :: method
!
!     method = VSL_RNG_METHOD_UNIFORM_STD
!     err = vdrnguniform(method, str, len_rns, rn, lb, ub)
!   END SUBROUTINE generateRN_real
!
!   SUBROUTINE destractRN(str, err)
!     TYPE (VSL_STREAM_STATE), INTENT(in) :: str
!     INTEGER(kind = 4), INTENT(out) :: err
!
!     err = vsldeletestream(str)
!   END SUBROUTINE destractRN
!
!   SUBROUTINE loadRNstat(str, filename, err)
!     CHARACTER(len = *, kind = 1), INTENT(in) :: filename
!     TYPE (VSL_STREAM_STATE), INTENT(out) :: str
!     INTEGER(kind = 4), INTENT(out) :: err
!
!     err = vslloadstreamf(str, TRIM(filename))
!   END SUBROUTINE loadRNstat
!
!   SUBROUTINE saveRNstat(str, filename, err)
!     CHARACTER(len = *, kind = 1), INTENT(in) :: filename
!     TYPE (VSL_STREAM_STATE), INTENT(in) :: str
!     INTEGER(kind = 4), INTENT(out) :: err
!
!     err = vslsavestreamf(str, TRIM(filename))
!   END SUBROUTINE saveRNstat
! END MODULE mod_rng

! MODULE main_procedures
!   USE global_variables
!   USE mod_rng
!   IMPLICIT NONE
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
!   SUBROUTINE getNumSweeps(slot, i_sample, n_sweeps_therm, n_sweeps_stead)
!     INTEGER(kind = 4), INTENT(in) :: slot, i_sample
!     INTEGER(kind = 4), INTENT(out) :: n_sweeps_therm, n_sweeps_stead
!
!     CHARACTER(len = 4) :: si_sample
!     CHARACTER(len = 30) :: filename
!     INTEGER(kind = 4) :: i_line, ios, n0
!
!     n_sweeps_therm = 0
!     n_sweeps_stead = 0
!
!     WRITE(si_sample, '(i0.4)') i_sample
!     filename=TRIM("stream_s"//si_sample//".dat")
!     OPEN(slot, file=filename, status="old")
!     READ (slot, '()')
!
!     DO i_line = 1, 10000, 1
!        READ (slot, *, iostat = ios) n0
!        IF (ios == 0) THEN
!           n_sweeps_therm = n_sweeps_therm + 1
!        ELSE
!           EXIT
!        END IF
!     END DO
!
!     DO i_line = 1, 10000, 1
!        READ (slot, *, iostat = ios) n0
!        IF (ios == 0) THEN
!           n_sweeps_stead = n_sweeps_stead + 1
!        ELSE
!           EXIT
!        END IF
!     END DO
!
!     CLOSE(slot)
!   END SUBROUTINE getNumSweeps
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
!
!   SUBROUTINE inputParameters
!     ! WRITE(0, '(a)') "len_x, len_z, J, beta, vel, n_sweeps_therm, n_sweeps_stead, id_init, id_bound, n_clones = ?"
!     ! WRITE(0, '(a)') "(id_init: 1. all-up, 2. DW, 3. random)"
!     ! WRITE(0, '(a)') "(id_bound: 1. anti-parallel, 2. parallel, 3. free)"
!     READ(*, *) len_x, len_z, J, beta, vel, n_sweeps_therm, n_sweeps_stead, id_init, id_bound, n_samples
!
!     n_steps = len_x * len_z
!     n_sweeps = n_sweeps_therm + n_sweeps_stead
!   END SUBROUTINE inputParameters
!
!   SUBROUTINE makeProbArray
!     prob(-1:1, -1:1, -1:1, -1:1, -1:1) = 1.0d0
!     WHERE ( deltaE > 0.0d0 )
!        prob(-1:1, -1:1, -1:1, -1:1, -1:1) = &
!             EXP(- beta * deltaE(-1:1, -1:1, -1:1, -1:1, -1:1))
!     END WHERE
!   END SUBROUTINE makeProbArray
!
!   SUBROUTINE makeDeltaEArray
!     INTEGER(kind = 4) :: center, east, west, south, north
!
!     DO center = -1, 1, 1
!        DO east = -1, 1, 1
!           DO west = -1, 1, 1
!              DO south = -1, 1, 1
!                 DO north = -1, 1, 1
!                    deltaE(center, east, west, south, north)=&
!                         2 * J * DBLE(center * (east + west + south + north))
!                 END DO
!              END DO
!           END DO
!        END DO
!     END DO
!   END SUBROUTINE makeDeltaEArray
!
!   SUBROUTINE calcEnergy(spin, energy)
!     INTEGER(kind = 4), INTENT(in) :: spin(1:, 1:)
!     REAL(kind = 8), INTENT(out) :: energy
!     INTEGER(kind = 4) :: x, z, east, west, south, north
!
!     energy = 0.0d0
!     DO x = 1, len_x
!        DO z = 1, len_z
!           CALL set_direction(id_bound, spin, x, z, east, west, south, north)
!           energy = energy &
!                - J * DBLE(spin(x,z) * (east + west + south + north))
!        END DO
!     END DO
!     energy = 0.5d0 * energy
!   END SUBROUTINE calcEnergy
!
!   SUBROUTINE calcSlipplaneEnergy(spin, energy)
!     INTEGER(kind = 4), INTENT(in) :: spin(1:, 1:)
!     REAL(kind = 8), INTENT(out) :: energy
!
!     INTEGER(kind = 4) :: x, east, west, south, north
!
!     energy = 0.0d0
!     DO x = 1, len_x
!        CALL set_direction(id_bound, spin, x, len_z / 2, &
!             east, west, south, north)
!        energy = energy - J * DBLE(spin(x, len_z / 2) * (east + west + south + north))
!     END DO
!   END SUBROUTINE calcSlipplaneEnergy
!
!   SUBROUTINE set_direction(id_bound, spin, x, z, east, west, south, north)
!     INTEGER(kind = 4), INTENT(in) :: id_bound, spin(1:, 1:), x, z
!     INTEGER(kind = 4), INTENT(out) :: east, west, south, north
!
!     IF ( x == len_x ) THEN
!        east = spin(1, z)
!     ELSE
!        east = spin(x + 1, z)
!     END IF
!     IF ( x == 1 ) THEN
!        west = spin(len_x, z)
!     ELSE
!        west = spin(x - 1, z)
!     END IF
!
!     SELECT CASE (id_bound)
!     CASE (1) !BC: anti-parallel
!        IF ( z == 1 ) THEN
!           south = 1
!        ELSE
!           south = spin(x, z - 1)
!        END IF
!        IF ( z == len_z ) THEN
!           north = -1
!        ELSE
!           north = spin(x, z + 1)
!        END IF
!     CASE (2) !BC: parallel
!        IF ( z == 1 ) THEN
!           south = 1
!        ELSE
!           south = spin(x, z - 1)
!        END IF
!        IF ( z == len_z ) THEN
!           north = 1
!        ELSE
!           north = spin(x, z + 1)
!        END IF
!     CASE (3) !BC: free
!        IF ( z == 1 ) THEN
!           south = 0
!        ELSE
!           south = spin(x, z - 1)
!        END IF
!        IF ( z == len_z ) THEN
!           north = 0
!        ELSE
!           north = spin(x, z + 1)
!        END IF
!     END SELECT
!   END SUBROUTINE set_direction
!
!   SUBROUTINE initializeSpin(id_init, spin)
!     INTEGER(kind = 4), INTENT(in) :: id_init
!     INTEGER(kind = 4), INTENT(out) :: spin(1:, 1:)
!
!     INTEGER(kind = 4) :: seed_spin, err_spin
!     INTEGER(kind = 4) :: x, z
!     TYPE(VSL_STREAM_STATE) :: str_spin
!
!     SELECT CASE (id_init)
!     CASE (1)
!        spin(1:len_x, 1:len_z) = 1
!     CASE (2)
!        spin(1:len_x, 1:len_z / 2) = 1
!        spin(1:len_x, len_z / 2 + 1:len_z) = -1
!     CASE (3)
!        CALL system_CLOCK(seed_spin)
!        CALL initializeRN(seed_spin, str_spin, err_spin)
!        DO z = 1, len_z, 1
!           CALL generateRN_int(str_spin, 0, 1, len_x, &
!                spin(1:len_x, z), err_spin)
!        END DO
!        spin(1:len_x, 1:len_z) = 2 * spin(1:len_x, 1:len_z) - 1
!     END SELECT
!   END SUBROUTINE initializeSpin
!
!   SUBROUTINE step_singleflip(spin, x, z, p, relax)
!     INTEGER(kind = 4), INTENT(inout) :: spin(1:, 1:)
!     INTEGER(kind = 4), INTENT(in) :: x, z
!     REAL(kind = 8), INTENT(in) :: p
!     REAL(kind = 8), INTENT(out) :: relax
!
!     REAL(kind = 8) :: en_loc
!     INTEGER(kind = 4) :: east, west, south, north
!
!     CALL set_direction(id_bound, spin, x, z, east, west, south, north)
!
!     IF (p <= prob(spin(x, z), east, west, south, north)) THEN
!        en_loc =  - spin(x, z) * (east + west + south + north)
!        relax = - 2 * en_loc
!        spin(x, z) = - spin(x, z)
!     ELSE
!        relax = 0.0d0
!     END IF
!   END SUBROUTINE step_singleflip
!
!   SUBROUTINE sweep_singleflip(spin, n_steps, rn_x, rn_z, rn_p, diss)
!     INTEGER(kind = 4), INTENT(inout) :: spin(1:, 1:)
!     INTEGER(kind = 4), INTENT(in) :: n_steps
!     INTEGER(kind = 4), INTENT(in) :: rn_x(1:), rn_z(1:)
!     REAL(kind = 8), INTENT(in) :: rn_p(1:)
!     REAL(kind = 8), INTENT(out) :: diss
!
!     INTEGER(kind = 4) :: i_step
!     REAL(kind = 8) :: relax
!
!     diss = 0.0d0
!     DO i_step = 1, n_steps, 1
!        CALL step_singleflip(spin, &
!             rn_x(i_step), rn_z(i_step), rn_p(i_step), relax)
!        diss = diss + relax
!     END DO
!   END SUBROUTINE sweep_singleflip
!
!   SUBROUTINE shiftUpperHalf(spin, pump, temp_spin)
!     INTEGER(kind = 4), INTENT(inout) :: spin(1:, 1:)
!     REAL(kind = 8), INTENT(out) :: pump
!     INTEGER(kind = 4), INTENT(inout) :: temp_spin(1:, 1:)
!     REAL(kind = 8) :: prev, next
!
!     CALL calcSlipplaneEnergy(spin, prev)
!     temp_spin(1:len_x, 1:len_z / 2) = spin(1:len_x, 1:len_z / 2)
!     temp_spin(1:len_x, 1:len_z / 2) = &
!          CSHIFT(temp_spin(1:len_x, 1:len_z / 2), shift=1, dim=1)
!     spin(1:len_x, 1:len_z / 2) = temp_spin(1:len_x, 1:len_z / 2)
!     CALL calcSlipplaneEnergy(spin, next)
!
!     pump = next - prev
!   END SUBROUTINE shiftUpperHalf
!
!   SUBROUTINE importSnapshot(slot, filename, spin)
!     INTEGER(kind = 4), INTENT(in) :: slot
!     CHARACTER(len = *, kind = 1), INTENT(in) :: filename
!     INTEGER(kind = 4), INTENT(out) :: spin(1:, 1:)
!
!     INTEGER(kind = 4) :: x, z, dum_x, dum_z
!
!     OPEN(slot, file=filename, status="old")
!     DO z = 1, len_z, 1
!        READ(slot, '()')
!        DO x = 1, len_x, 1
!           READ(slot, *) dum_x, dum_z, spin(x, z)
!        END DO
!     END DO
!     CLOSE(slot)
!   END SUBROUTINE importSnapshot
!
!   SUBROUTINE exportSnapshot(slot, filename, spin)
!     INTEGER(kind = 4), INTENT(in) :: slot
!     CHARACTER(len = *, kind = 1), INTENT(in) :: filename
!     INTEGER(kind = 4), INTENT(in) :: spin(1:, 1:)
!
!     INTEGER(kind = 4) :: x, z
!
!     OPEN(slot, file=filename, status="replace")
!     WRITE(slot, '(a)') "# x, z, spin"
!     DO z = 1, len_z, 1
!        DO x = 1, len_x, 1
!           WRITE(slot, '(i0.4, a, i0.4, a, i0.4)') &
!                x, ", ", z, ", ", spin(x, z)
!        END DO
!        WRITE(slot, '()')
!     END DO
!     CLOSE(slot)
!   END SUBROUTINE exportSnapshot
! END MODULE main_procedures

PROGRAM main
  !$  USE omp_lib
  USE global_variables
  USE mod_rng
  USE main_procedures
  USE IFPORT, ONLY: access

  IMPLICIT NONE

  !observables
  INTEGER(kind = 4), ALLOCATABLE :: spin(:, :), temp_spin(:, :)
  REAL(kind = 8), ALLOCATABLE :: m_z(:)
  REAL(kind = 8) :: pump, diss, energy

  !simulation variables
  INTEGER(kind = 4) :: seed_master
  INTEGER(kind = 4) :: seed_x, seed_z, seed_prob
  INTEGER(kind = 4) :: lb_rn, ub_rn, len_rns
  INTEGER(kind = 4), ALLOCATABLE :: rn_x(:, :), rn_z(:, :)
  REAL(kind = 8), ALLOCATABLE :: rn_prob(:, :)
  ! REAL(kind = 8), ALLOCATABLE :: sw_x(:), sw_z(:), sw_p(:)
  ! TYPE(type_rand), ALLOCATABLE :: mrc_x(:), mrc_z(:), mrc_p(:)

  !loop variables
  INTEGER(kind = 4) :: i_line, ios, i_vel, i_sweep, i_sample, x, z
  INTEGER(kind = 4) :: n_samples0
  INTEGER(kind = 4), ALLOCATABLE :: n_sweeps_therm0(:), n_sweeps_stead0(:)
  CHARACTER(len = 4, kind = 1) :: si_sample
  CHARACTER(len = 30, kind = 1) :: &
       filename_stream, filename_m_z, filename_snap
  CHARACTER(len = 40, kind = 1) :: filename_str

  !slot variables
  INTEGER(kind = 4), ALLOCATABLE :: slot_stream(:), slot_snap(:), slot_m_z(:)

  !omp variables
  INTEGER(kind = 4) :: i_th, n_ths, err_x, err_z, err_prob

  !stat variables
  INTEGER(kind = 4) :: stat_snap
  INTEGER(kind = 4), ALLOCATABLE :: stat_sample_e(:), stat_sample_a(:)
  CHARACTER(:), ALLOCATABLE :: command_cpStream, command_cpM_z
  LOGICAL(kind = 8) :: modifying

  CALL inputParameters
  ALLOCATE(n_sweeps_therm0(1:n_samples), n_sweeps_stead0(1:n_samples))
  n_sweeps_therm0(1:n_samples) = 0
  n_sweeps_stead0(1:n_samples) = 0
  CALL getNumSamples(40, n_samples0)
  ALLOCATE(stat_sample_e(1:n_samples0), stat_sample_a(1:n_samples0))
  CALL getStatsSamples(40, n_samples0, &
       stat_sample_e(1:n_samples0), stat_sample_a(1:n_samples0))
  DO i_sample = 1, n_samples0, 1
     IF ( stat_sample_e(i_sample) == 0 ) THEN
        WRITE(si_sample, '(i0.4)') i_sample
        command_cpStream = "cp stream.dat stream_s"//si_sample//".dat"
        command_cpM_z = "cp m_z.dat m_z_s"//si_sample//".dat"
        CALL system(command_cpStream)
        CALL system(command_cpM_z)
        CALL getNumSweeps(11, i_sample, &
             n_sweeps_therm0(i_sample), n_sweeps_stead0(i_sample))
     ELSE
        CALL getNumSweeps(11, i_sample, &
             n_sweeps_therm0(i_sample), n_sweeps_stead0(i_sample))
     END IF
  END DO
  !TODO: 本当はこれも一発で良い筈
  !（異なるn_sweeps_therm0を持つサンプルは共存できない仕様だから）

  DO i_sample = 1, n_samples, 1
     IF ( n_sweeps_therm <= n_sweeps_therm0(i_sample) ) THEN
        n_sweeps_therm = n_sweeps_therm0(i_sample)
        IF ( n_sweeps_stead <= n_sweeps_stead0(i_sample) ) THEN
           n_sweeps_stead = n_sweeps_stead0(i_sample)
        END IF
     END IF
     IF ( n_sweeps_therm > n_sweeps_therm0(i_sample) ) THEN
        n_sweeps_stead0(i_sample) = 0
     END IF
  END DO
  IF ( n_samples < n_samples0 ) THEN
     n_samples = n_samples0
  END IF

  !$omp parallel default(none) &
  !$omp shared(n_ths)
  n_ths = 1
  !$  n_ths = omp_get_max_threads()
  !$omp end parallel

  len_rns = len_x * len_z * n_sweeps
  ALLOCATE(rn_x(0:n_ths - 1, 1:len_rns))
  ALLOCATE(rn_z(0:n_ths - 1, 1:len_rns))
  ALLOCATE(rn_prob(0:n_ths - 1, 1:len_rns))
  ALLOCATE(str_x(0:n_ths - 1), str_z(0:n_ths - 1), str_prob(0:n_ths - 1))
  ALLOCATE(slot_stream(0:n_ths - 1), slot_snap(0:n_ths - 1), &
       slot_m_z(0:n_ths - 1))

  CALL system_CLOCK(seed_master)
  !$omp parallel do default(none) &
  !$omp shared(n_ths, seed_master) &
  !$omp shared(str_x, str_z, str_prob) &
  !$omp private(seed_x, seed_z, seed_prob, err_x, err_z, err_prob)
  DO i_th = 0, n_ths - 1
     seed_x = seed_master + i_th
     seed_z = seed_x + n_ths
     seed_prob = seed_z + n_ths
     CALL initializeRN(seed_x, str_x(i_th), err_x)
     CALL initializeRN(seed_z, str_z(i_th), err_z)
     CALL initializeRN(seed_prob, str_prob(i_th), err_prob)
  END DO
  !$omp end parallel do

  DO i_th = 0, n_ths - 1
     slot_stream(i_th) = 20 + i_th
     slot_snap(i_th) = slot_stream(i_th) + n_ths
     slot_m_z(i_th) = slot_snap(i_th) + n_ths
  END DO

  ALLOCATE(spin(1:len_x, 1:len_z), temp_spin(1:len_x, 1:len_z / 2))
  ALLOCATE(m_z(1:len_z))

  CALL makeDeltaEArray
  CALL makeProbArray

  ! スナップショットがあれば読み込み，なければスピンを初期化
  stat_snap = access("snap_initial.dat", " ")
  IF ( stat_snap == 0 ) THEN
     CALL importSnapshot(30, "snap_initial.dat", spin(1:len_x, 1:len_z))
  ELSE
     CALL initializeSpin(id_init, spin(1:len_x, 1:len_z))
     CALL exportSnapshot(30, "snap_initial.dat", spin(1:len_x, 1:len_z))
  END IF

  !$omp parallel do default(none) &
  !$omp firstprivate(spin, str_x, str_z, str_prob) &
  !$omp private(ub_rn, lb_rn, err_x, err_z, err_prob, modifying) &
  !$omp private(i_sweep, i_vel, i_th, si_sample, x, z, n_steps) &
  !$omp private(temp_spin, pump, diss, energy, m_z) &
  !$omp private(filename_stream, filename_m_z, filename_snap, filename_str) &
  !$omp shared(n_samples, len_x, len_z, vel, len_rns, rn_x, rn_z, rn_prob) &
  !$omp shared(n_sweeps_therm, n_sweeps_therm0) &
  !$omp shared(n_sweeps_stead, n_sweeps_stead0) &
  !$omp shared(slot_stream, slot_m_z, slot_snap)
  DO i_sample = 1, n_samples, 1
     i_th = 0
     !$ i_th = omp_get_thread_num()

     CALL generateRN_int(str_x(i_th), 1, len_x, len_rns, rn_x(i_th, :), err_x)
     CALL generateRN_int(str_z(i_th), 1, len_z, len_rns, rn_z(i_th, :), err_z)
     CALL generateRN_real(str_prob(i_th), 0.0d0, 1.0d0, len_rns, &
          rn_prob(i_th, :), err_prob)

     WRITE(si_sample, '(i0.4)') i_sample

     filename_stream=TRIM("stream_s"//si_sample//".dat")
     filename_m_z=TRIM("m_z_s"//si_sample//".dat")

     IF ( n_sweeps_therm0(i_sample) > 0 ) THEN
        OPEN(slot_stream(i_th), file=filename_stream, status="old")
        READ(slot_stream(i_th), '()')
        OPEN(slot_m_z(i_th), file=filename_m_z, status="old")
        READ(slot_m_z(i_th), '()')
        filename_snap=TRIM("snap_thermalized_s"//si_sample//".dat")

        filename_str=TRIM("str_x_thermalized_s"//si_sample//".bin")
        CALL loadRNstat(str_x(i_th), filename_str, err_x)
        filename_str=TRIM("str_z_thermalized_s"//si_sample//".bin")
        CALL loadRNstat(str_z(i_th), filename_str, err_z)
        filename_str=TRIM("str_prob_thermalized_s"//si_sample//".bin")
        CALL loadRNstat(str_prob(i_th), filename_str, err_prob)
     ELSE
        OPEN(slot_stream(i_th), file=filename_stream, status="new")
        WRITE(slot_stream(i_th), '(a)') "# i_sweep, pump, diss, energy"
        OPEN(slot_m_z(i_th), file=filename_m_z, status="new")
        WRITE(slot_m_z(i_th), '(a)') "# i_sweep, z, m_z"
        filename_snap=TRIM("snap_initial.dat")
     END IF
     CALL importSnapshot(slot_snap(i_th), filename_snap, spin(1:len_x, 1:len_z))

     CALL calcEnergy(spin, energy)

     !  平衡化・空読み
     DO i_sweep = 1, n_sweeps_therm0(i_sample), 1
        DO z = 1, len_z, 1
           READ(slot_m_z(i_th), '()')
        END DO
        READ(slot_m_z(i_th), '()')
        READ(slot_stream(i_th), '()')
     END DO

     n_steps = len_x * len_z

     !  平衡化
     DO i_sweep = 1 + n_sweeps_therm0(i_sample), n_sweeps_therm, 1
        lb_rn = (i_sweep - 1) * n_steps + 1
        ub_rn = i_sweep * n_steps

        CALL sweep_singleflip(spin, n_steps, &
             rn_x(i_th, lb_rn:ub_rn), &
             rn_z(i_th, lb_rn:ub_rn), &
             rn_prob(i_th, lb_rn:ub_rn), diss)
        energy = energy + diss

        DO z = 1, len_z, 1
           m_z(z) = DBLE(SUM(spin(1:len_x, z), dim=1)) / DBLE(len_x)
           WRITE(slot_m_z(i_th), '( i5, a, i5, a, f0.4)') &
                i_sweep, ", ", z, ", ", m_z(z)
        END DO
        WRITE(slot_m_z(i_th), '()')

        WRITE(slot_stream(i_th), '(i5,  a, f0.4, a, f0.4, a, f0.4, a, &
             f0.4, a, f0.4, a, f0.4)') &
             i_sweep, ", ", 0.0d0, ", ", diss, ", ", energy, ", ", &
             0.0d0, ", ", 0.0d0, ", ", 0.0d0
     END DO

     IF ( n_sweeps_therm > n_sweeps_therm0(i_sample) ) THEN
        WRITE(slot_stream(i_th), '(a)') "# -- Thermalized --"
     ELSE
        READ(slot_stream(i_th), '()')
     END IF

     !平衡化後のスナップの保存
     filename_snap=TRIM("snap_thermalized_s"//si_sample//".dat")
     CALL exportSnapshot(slot_snap(i_th), &
          filename_snap, spin(1:len_x, 1:len_z))
     filename_str=TRIM("str_x_thermalized_s"//si_sample//".bin")
     CALL saveRNstat(str_x(i_th), filename_str, err_x)
     filename_str=TRIM("str_z_thermalized_s"//si_sample//".bin")
     CALL saveRNstat(str_z(i_th), filename_str, err_z)
     filename_str=TRIM("str_prob_thermalized_s"//si_sample//".bin")
     CALL saveRNstat(str_prob(i_th), filename_str, err_prob)

     IF ( n_sweeps_therm == n_sweeps_therm0(i_sample) ) THEN
        filename_snap=TRIM("snap_steadized_s"//si_sample//".dat")
        CALL importSnapshot(slot_snap(i_th), &
             filename_snap, spin(1:len_x, 1:len_z))

        filename_str=TRIM("str_x_steadized_s"//si_sample//".bin")
        CALL loadRNstat(str_x(i_th), filename_str, err_x)
        filename_str=TRIM("str_z_steadized_s"//si_sample//".bin")
        CALL loadRNstat(str_z(i_th), filename_str, err_z)
        filename_str=TRIM("str_prob_steadized_s"//si_sample//".bin")
        CALL loadRNstat(str_prob(i_th), filename_str, err_prob)
     END IF

     !  定常化・空読み
     DO i_sweep = n_sweeps_therm + 1, &
          n_sweeps_therm + n_sweeps_stead0(i_sample), 1
        DO z = 1, len_z, 1
           READ(slot_m_z(i_th), '()')
        END DO
        READ(slot_m_z(i_th), '()')

        READ(slot_stream(i_th), '()')
     END DO

     n_steps = len_x * len_z / vel

     !  定常化
     DO i_sweep = n_sweeps_therm + n_sweeps_stead0(i_sample) + 1, &
          n_sweeps_therm + n_sweeps_stead, 1
        DO i_vel = 1, vel, 1
           lb_rn = (i_vel - 1 + vel * (i_sweep - 1)) * n_steps  + 1
           ub_rn = (i_vel + vel * (i_sweep - 1)) * n_steps
           !  CALL chargeRandomNumbers(  n_spins / vel, i_sweep, &
           !       mrc_x, mrc_z, mrc_p, sw_x, sw_z, sw_p)
           CALL shiftUpperHalf(spin, pump, temp_spin)
           energy = energy + pump
           CALL sweep_singleflip(spin, n_steps, &
                rn_x(i_th, lb_rn:ub_rn), &
                rn_z(i_th, lb_rn:ub_rn), &
                rn_prob(i_th, lb_rn:ub_rn), diss)
           energy = energy + diss
        END DO

        DO z = 1, len_z, 1
           m_z(z) = DBLE(SUM(spin(1:len_x, z), dim=1)) / DBLE(len_x)
           WRITE(slot_m_z(i_th), '( i5, a, i5, a, f0.4)') &
                i_sweep, ", ", z, ", ", m_z(z)
        END DO
        WRITE(slot_m_z(i_th), '()')

        WRITE(slot_stream(i_th), '(  i5, a, f0.4, a, f0.4, a, f0.4, a, &
             f0.4, a, f0.4, a, f0.4)') &
             i_sweep, ", ", pump, ", ", diss, ", ", energy, ", ", &
             0.0d0, ", ", 0.0d0, ", ", 0.0d0
     END DO

     WRITE(slot_stream(i_th), '(a)') "# -- Steadized --"

     CLOSE(slot_stream(i_th))
     CLOSE(slot_m_z(i_th))

     !定常化後のスナップの保存
     filename_snap=TRIM("snap_steadized_s"//si_sample//".dat")
     CALL exportSnapshot(slot_snap(i_th), filename_snap, spin(1:len_x, 1:len_z))

     filename_str=TRIM("str_x_steadized_s"//si_sample//".bin")
     CALL saveRNstat(str_x(i_th), filename_str, err_x)
     filename_str=TRIM("str_z_steadized_s"//si_sample//".bin")
     CALL saveRNstat(str_z(i_th), filename_str, err_z)
     filename_str=TRIM("str_prob_steadized_s"//si_sample//".bin")
     CALL saveRNstat(str_prob(i_th), filename_str, err_prob)

    !  modifying = LOGICAL(n_sweeps_therm > n_sweeps_therm0(i_sample) &
    !  .OR. n_sweeps_stead > n_sweeps_stead0(i_sample))
    !  !$omp atomic
    !  WRITE(10, '(i5, a, i2, a, i2)') i_sample, ", ", 1, ", ", modifying*1 + 1
    !  !$omp end atomic
  END DO
  !$omp end parallel do

  !$omp parallel do default(none) &
  !$omp shared(n_ths) &
  !$omp shared(str_x, str_z, str_prob) &
  !$omp private(err_x, err_z, err_prob)
  DO i_th = 0, n_ths - 1
     CALL destractRN(str_x(i_th), err_x)
     CALL destractRN(str_z(i_th), err_x)
     CALL destractRN(str_prob(i_th), err_x)
  END DO
  !$omp end parallel do

  OPEN(10, file="list_samples.dat", status="old")
  DO i_sample = 1, n_samples, 1
    modifying = LOGICAL(n_sweeps_therm > n_sweeps_therm0(i_sample) &
    .OR. n_sweeps_stead > n_sweeps_stead0(i_sample))
    WRITE(10, '(i5, a, i2, a, i2)') i_sample, ", ", 1, ", ", modifying*1 + 1
  END DO
  CLOSE(10)
END PROGRAM main
