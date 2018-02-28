PROGRAM main
  !$  USE omp_lib
  USE mod_global
  USE mod_proc
  USE IFPORT, ONLY: access

  IMPLICIT NONE

  INTEGER(kind = 4), ALLOCATABLE :: spin4(:, :)
  INTEGER(kind = 4) :: en

  INTEGER(kind = 4) :: seed_master, seed_x, seed_z, seed_pr, n_steps, err
  INTEGER(kind = 4) :: i_step, i_vel, t, s
  INTEGER(kind = 4), ALLOCATABLE :: rn_x(:), rn_z(:), sl_spin(:, :), sl_en(:)
  REAL(kind = 8), ALLOCATABLE :: rn_pr(:)
  TYPE (VSL_STREAM_STATE), ALLOCATABLE :: str_x(:), str_z(:), str_pr(:)
  CHARACTER(len = 4, kind = 1) :: st, ss
  CHARACTER(:), ALLOCATABLE :: fl_spin, fl_en

  CALL importParams_2d(len_x, len_z, beta, vel, len_t1, len_t2, id_IC, id_BC, len_s)

  len_s0 = 0

  IF ( len_s < len_s0 ) THEN
     len_s = len_s0
  END IF

  CALL makePr_2d(beta, pr_2d(-1:1, -1:1, -1:1, -1:1, -1:1))

  ALLOCATE(sl_en(len_s0 + 1:len_s))
  ALLOCATE(sl_spin(len_s0 + 1:len_s, 1:len_t1 + len_t2))
  FORALL (s=len_s0 + 1:len_s:1)
     sl_en(s) = 20 + s - 1
  END FORALL
  FORALL (s=len_s0 + 1:len_s:1, t=1:len_t1 + len_t2:1)
     sl_spin(s, t) = 20 + (len_s - len_s0) + (t - 1) * (len_s - len_s0) + s - 1
  END FORALL

  WRITE(*, *) 1

  DO s = len_s0 + 1, len_s, 1
     WRITE(ss, '(i0.4)') s
     OPEN(sl_en(s), file="en_s"//ss//"_step.bin", access="stream", status="new")
     DO t = 1, len_t1 + len_t2, 1
        WRITE(st, '(i0.4)') t
        OPEN(sl_spin(s, t), file="spin_t"//st//"s"//ss//".bin", access="stream", status="new", buffered="YES")
     END DO
  END DO

  WRITE(*, *) 2

  ALLOCATE(str_x(len_s0 + 1:len_s), str_z(len_s0 + 1:len_s), str_pr(len_s0 + 1:len_s))
  ALLOCATE(rn_x(1:len_x * len_z), rn_z(1:len_x * len_z), rn_pr(1:len_x * len_z))
  ALLOCATE(spin4(1:len_x, 1:len_z))

  WRITE(*, *) 3

  seed_master = 100
  DO s = len_s0 + 1, len_s, 1
     err = vslnewstream(str_x(s), VSL_BRNG_SFMT19937, seed_master + 4 * (s - 1))
     err = vslnewstream(str_z(s), VSL_BRNG_SFMT19937, seed_master + 4 * (s - 1) + 2)
     err = vslnewstream(str_pr(s), VSL_BRNG_SFMT19937, seed_master + 4 * (s - 1) + 3)
  END DO

  WRITE(*, *) 4

  !$omp parallel do schedule(static, 1) default(none) &
  !$omp shared(len_s0, len_s, len_t1, len_t2, id_BC, len_x, len_z, vel) &
  !$omp shared(str_x, str_z, str_pr, sl_spin, sl_en) &
  !$omp private(err, rn_x, rn_z, rn_pr, fl_spin, fl_en, ss, st, n_steps, spin4, en)
  DO s = len_s0 + 1, len_s, 1

     CALL initSpin_2d(s, spin4(1:len_x, 1:len_z))
     CALL calcEn_2d(spin4(1:len_x, 1:len_z), en)

     WRITE(*, *) spin4(1:len_x, 1:len_z)

     WRITE(*, *) 5

     !  thermalization
     n_steps = len_x * len_z
     DO t = 1, len_t1, 1
        err = virnguniform(VSL_RNG_METHOD_UNIFORM_STD, str_x(s), n_steps, rn_x(1:n_steps), 1, len_x + 1)
        WRITE(*, *) 5.1
        err = virnguniform(VSL_RNG_METHOD_UNIFORM_STD, str_z(s), n_steps, rn_z(1:n_steps), 1, len_z + 1)
        WRITE(*, *) 5.2
        err = vdrnguniform(VSL_RNG_METHOD_UNIFORM_STD, str_pr(s), n_steps, rn_pr(1:n_steps), 0.0d0, 1.0d0)
        WRITE(*, *) 5.3
        CALL multiSSFs_2d(sl_en(s), t, 0, n_steps, &
             rn_x(1:n_steps), rn_z(1:n_steps), rn_pr(1:n_steps), spin4(1:len_x, 1:len_z), en)
        WRITE(*, *) 5.4
        WRITE(sl_spin(s, t)) INT1(spin4(1:len_x, 1:len_z))
        WRITE(*, *) 5.5
     END DO

     WRITE(*, *) 6

     !  steadization
     n_steps = len_x * len_z / vel
     DO t = len_t1 + 1, len_t1 + len_t2, 1
        DO i_vel = 1, vel, 1
           err = virnguniform(VSL_RNG_METHOD_UNIFORM_STD, str_x(s), n_steps, rn_x(1:n_steps), 1, len_x + 1)
           err = virnguniform(VSL_RNG_METHOD_UNIFORM_STD, str_z(s), n_steps, rn_z(1:n_steps), 1, len_z + 1)
           err = vdrnguniform(VSL_RNG_METHOD_UNIFORM_STD, str_pr(s), n_steps, rn_pr(1:n_steps), 0.0d0, 1.0d0)
           CALL shift_2d(sl_en(s), t, i_vel, spin4(1:len_x, 1:len_z), en)
           CALL multiSSFs_2d(sl_en(s), t, i_vel, n_steps, &
                rn_x(1:n_steps), rn_z(1:n_steps), rn_pr(1:n_steps), spin4(1:len_x, 1:len_z), en)
        END DO
        WRITE(sl_spin(s, t)) INT1(spin4(1:len_x, 1:len_z))
     END DO
  END DO
  !$omp end parallel do

  WRITE(*, *) 7

  DO s = len_s0 + 1, len_s, 1
     CLOSE(sl_en(s))
     DO t = 1, len_t1 + len_t2, 1
        CLOSE(sl_spin(s, t))
     END DO
  END DO

  WRITE(*, *) 8

  !$omp parallel do schedule(dynamic, 1) default(none) &
  !$omp shared(len_s0, len_s, str_x, str_z, str_pr) &
  !$omp private(err)
  DO s = len_s0 + 1, len_s, 1
     err = vsldeletestream(str_x(s))
     err = vsldeletestream(str_z(s))
     err = vsldeletestream(str_pr(s))
  END DO
  !$omp end parallel do

  WRITE(*, *) 9
END PROGRAM main
