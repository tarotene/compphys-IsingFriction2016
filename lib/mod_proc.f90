MODULE mod_proc
  USE mod_global
  USE IFPORT, ONLY:access

  IMPLICIT NONE
CONTAINS
  SUBROUTINE countLines(slot, filename, n_lines)
    INTEGER(4), INTENT(in) :: slot
    CHARACTER(*), INTENT(in) :: filename
    INTEGER(4), INTENT(out) :: n_lines

    INTEGER(4) :: i_beta, istat
    
    OPEN(slot, file=filename, status="old")
    DO i_beta = 1, HUGE(1), 1
      READ(slot, '()', iostat=istat)
      if ( istat < 0 ) exit
      n_lines = n_lines + 1
    END DO
    CLOSE(slot)
  END SUBROUTINE countLines

  SUBROUTINE constRand_SFMT19937(seed, stream)
    INTEGER(4), INTENT(in) :: seed
    TYPE(VSL_STREAM_STATE), INTENT(out) :: stream

    INTEGER(4) :: err

    err = vslnewstream(stream, VSL_BRNG_SFMT19937, seed)
  END SUBROUTINE constRand_SFMT19937

  SUBROUTINE destRand(stream)
    TYPE(VSL_STREAM_STATE), INTENT(in) :: stream

    INTEGER(4) :: err

    err = vsldeletestream(stream)
  END SUBROUTINE destRand

  SUBROUTINE saveRand(stream, filename)
    TYPE(VSL_STREAM_STATE), INTENT(in) :: stream
    CHARACTER(13), INTENT(in) :: filename

    INTEGER(4) :: err

    err = vslsavestreamf(stream, TRIM(filename))
  END SUBROUTINE saveRand

  SUBROUTINE updateDRand_Uniform(stream, len_rand, lbound, rbound, rand)
    TYPE(VSL_STREAM_STATE), INTENT(in) :: stream
    INTEGER(4), INTENT(in) :: len_rand
    REAL(8), INTENT(in) :: lbound, rbound
    REAL(8), INTENT(out) :: rand(1:)

    INTEGER(4) :: err

    err = vdrnguniform(VSL_RNG_METHOD_UNIFORM_STD, stream, len_rand, rand(1:), lbound, rbound)
  END SUBROUTINE updateDRand_Uniform

  SUBROUTINE updateIRand_Uniform(stream, len_rand, lbound, rbound, rand)
    TYPE(VSL_STREAM_STATE), INTENT(in) :: stream
    INTEGER(4), INTENT(in) :: len_rand
    INTEGER(4), INTENT(in) :: lbound, rbound
    INTEGER(4), INTENT(out) :: rand(1:)

    INTEGER(4) :: err

    err = virnguniform(VSL_RNG_METHOD_UNIFORM_STD, stream, len_rand, rand(1:), lbound, rbound)
  END SUBROUTINE updateIRand_Uniform

  !TODO: 関数化
  SUBROUTINE readParams_2d(l_x,l_z,beta,vel,l_t,id_IC,id_BC,n_s)
    INTEGER(4), INTENT(out) :: l_x, l_z
    REAL(8), INTENT(out) :: beta
    INTEGER(4), INTENT(out) :: vel, l_t, id_IC, id_BC, n_s

    READ (*, *) l_x, l_z, beta, vel, l_t, id_IC, id_BC, n_s
  END SUBROUTINE readParams_2d

  SUBROUTINE readParams_3d(l_x,l_y,l_z,beta,vel,l_t,id_IC,id_BC,n_s)
    INTEGER(4), INTENT(out) :: l_x, l_y, l_z
    REAL(8), INTENT(out) :: beta
    INTEGER(4), INTENT(out) :: vel, l_t, id_IC, id_BC, n_s

    READ (*, *) l_x, l_y, l_z, beta, vel, l_t, id_IC, id_BC, n_s
  END SUBROUTINE readParams_3d

  SUBROUTINE readParams_2d_eq(l_x,l_z,beta,l_t0,l_t1,id_IC,id_BC,n_s)
    INTEGER(4), INTENT(out) :: l_x, l_z
    REAL(8), INTENT(out) :: beta
    INTEGER(4), INTENT(out) :: l_t0, l_t1, id_IC, id_BC, n_s
    READ (*, *) l_x, l_z, beta, l_t0, l_t1, id_IC, id_BC, n_s
  END SUBROUTINE readParams_2d_eq

  SUBROUTINE readParams_3d_eq(l_x,l_y,l_z,beta,l_t,id_IC,id_BC,n_s)
    INTEGER(4), INTENT(out) :: l_x, l_y, l_z
    REAL(8), INTENT(out) :: beta
    INTEGER(4), INTENT(out) :: l_t, id_IC, id_BC, n_s

    READ (*, *) l_x, l_y, l_z, beta, l_t, id_IC, id_BC, n_s
  END SUBROUTINE readParams_3d_eq

  SUBROUTINE paramsCorr_2d(l_x, l_z, beta, vel, l_t, n_s)
    INTEGER(4), INTENT(out) :: l_x, l_z
    REAL(8), INTENT(out) :: beta
    INTEGER(4), INTENT(out) :: vel, l_t, n_s

    READ (*, *) l_x, l_z, beta, vel, l_t, n_s
  END SUBROUTINE paramsCorr_2d

  SUBROUTINE paramsCorr_2d_eq(l_x, l_z, beta, l_t, n_s)
    INTEGER(4), INTENT(out) :: l_x, l_z
    REAL(8), INTENT(out) :: beta
    INTEGER(4), INTENT(out) :: l_t, n_s

    READ (*, *) l_x, l_z, beta, l_t, n_s
  END SUBROUTINE paramsCorr_2d_eq

  SUBROUTINE paramsCorr_3d(l_x, l_y, l_z, beta, vel, l_t, n_s)
    INTEGER(4), INTENT(out) :: l_x, l_y, l_z
    REAL(8), INTENT(out) :: beta
    INTEGER(4), INTENT(out) :: vel, l_t, n_s

    READ (*, *) l_x, l_y, l_z, beta, vel, l_t, n_s
  END SUBROUTINE paramsCorr_3d

  SUBROUTINE paramsCorr_3d_eq(l_x, l_y, l_z, beta, l_t, n_s)
    INTEGER(4), INTENT(out) :: l_x, l_y, l_z
    REAL(8), INTENT(out) :: beta
    INTEGER(4), INTENT(out) :: l_t, n_s

    READ (*, *) l_x, l_y, l_z, beta, l_t, n_s
  END SUBROUTINE paramsCorr_3d_eq

  SUBROUTINE paramsCalc_2d(l_x, l_z, beta, vel, l_t, n_s, l_th, l_b)
    INTEGER(4), INTENT(out) :: l_x, l_z
    REAL(8), INTENT(out) :: beta
    INTEGER(4), INTENT(out) :: vel, l_t, n_s, l_th, l_b

    READ (*, *) l_x, l_z, beta, vel, l_t, n_s, l_th, l_b
  END SUBROUTINE paramsCalc_2d

  SUBROUTINE paramsCalc_2d_eq(l_x, l_z, beta, l_t, n_s, l_th, l_b)
    INTEGER(4), INTENT(out) :: l_x, l_z
    REAL(8), INTENT(out) :: beta
    INTEGER(4), INTENT(out) :: l_t, n_s, l_th, l_b

    READ (*, *) l_x, l_z, beta, l_t, n_s, l_th, l_b
  END SUBROUTINE paramsCalc_2d_eq

  SUBROUTINE paramsCalc_3d(l_x, l_y, l_z, beta, vel, l_t, n_s, l_th, l_b)
    INTEGER(4), INTENT(out) :: l_x, l_y, l_z
    REAL(8), INTENT(out) :: beta
    INTEGER(4), INTENT(out) :: vel, l_t, n_s, l_th, l_b

    READ (*, *) l_x, l_y, l_z, beta, vel, l_t, n_s, l_th, l_b
  END SUBROUTINE paramsCalc_3d

  SUBROUTINE paramsCalc_3d_eq(l_x, l_y, l_z, beta, l_t, n_s, l_th, l_b)
    INTEGER(4), INTENT(out) :: l_x, l_y, l_z
    REAL(8), INTENT(out) :: beta
    INTEGER(4), INTENT(out) :: l_t, n_s, l_th, l_b

    READ (*, *) l_x, l_y, l_z, beta, l_t, n_s, l_th, l_b
  END SUBROUTINE paramsCalc_3d_eq

  ! TODO: 名前に因んで関数化
  SUBROUTINE metropolis_2d(beta, p_2d)
    REAL(8), INTENT(in) :: beta
    REAL(8), INTENT(out) :: p_2d(-1:1, -1:1, -1:1, -1:1, -1:1)

    INTEGER(4) :: dE(-1:1, -1:1, -1:1, -1:1, -1:1), c, e, w, s, n

    POSITIVE:FORALL (c=-1:1, e=-1:1, w=-1:1, s=-1:1, n=-1:1)
       p_2d(c, e, w, s, n) = EXP(-2.0d0*beta*DBLE(c*(e + w + s + n)))
    END FORALL POSITIVE
    WHERE (p_2d >= 1.0d0) p_2d = 1.0d0
  END SUBROUTINE metropolis_2d

  ! TODO: 名前に因んで関数化
  SUBROUTINE metropolis_3d(beta, p_3d)
    REAL(8), INTENT(in) :: beta
    REAL(8), INTENT(out) :: p_3d(-1:1, -1:1, -1:1, -1:1, -1:1, -1:1, -1:1)

    INTEGER(4) :: dE(-1:1, -1:1, -1:1, -1:1, -1:1, -1:1, -1:1), c, e, w, s, n, u, d

    POSITIVE:FORALL (c=-1:1, e=-1:1, w=-1:1, s=-1:1, n=-1:1, u=-1:1, d=-1:1)
       p_3d(c, e, w, s, n, u, d) = EXP(-2.0d0*beta*DBLE(c*(e + w + s + n + u + d)))
    END FORALL POSITIVE
    WHERE (p_3d >= 1.0d0) p_3d = 1.0d0
  END SUBROUTINE metropolis_3d

  SUBROUTINE setNeighbour_2d(IS2,x,z,e,w,s,n)
    INTEGER(4), TARGET, INTENT(in) :: IS2(0:, 0:)
    INTEGER(4), INTENT(in) :: x, z
    INTEGER(4), POINTER, INTENT(out) :: e, w, s, n

    e => IS2(MOD(x,l_x)+1,z)
    IF (x==1) THEN
       w => IS2(l_x,z)
    ELSE
       w => IS2(x-1,z)
    END IF
    s => IS2(x,z-1)
    n => IS2(x,z+1)
  END SUBROUTINE setNeighbour_2d

  ! TODO: 名前に因んで関数化
  SUBROUTINE setNeighbour_3d(IS3, x, y, z, e, w, s, n, u, d)
    INTEGER(4), TARGET, INTENT(in) :: IS3(0:, 0:, 0:)
    INTEGER(4), INTENT(in) :: x, y, z
    INTEGER(4), POINTER, INTENT(out) :: e, w, s, n, u, d

    e => IS3(MOD(x, l_x) + 1, y, z)
    IF (x == 1) THEN
       w => IS3(l_x, y, z)
    ELSE
       w => IS3(x - 1, y, z)
    END IF

    s => IS3(x, MOD(y, l_y) + 1, z)
    IF (y == 1) THEN
       n => IS3(x, l_y, z)
    ELSE
       n => IS3(x, y - 1, z)
    END IF

    u => IS3(x, y, z - 1)
    d => IS3(x, y, z + 1)
  END SUBROUTINE setNeighbour_3d

  SUBROUTINE calcEB_2d(IS2, eb)
    INTEGER(4), INTENT(in) :: IS2(0:, 0:)
    INTEGER(4), INTENT(out) :: eb

    INTEGER(4) :: x,z
    INTEGER(4), POINTER :: e,w,s,n

    eb = 0
    DO z = 1, l_z, 1
       DO x = 1, l_x, 1
          CALL setNeighbour_2d(IS2(0:l_x+1,0:l_y+1),x,z,e,w,s,n)
          eb = eb - IS2(x,z) * (e+w+s+n)
       END DO
    END DO
    DO x = 1, l_x, 1
       eb = eb - IS2(x,0)*IS2(x,1) - IS2(x,l_z)*IS2(x,l_z+1)
    END DO
    eb = eb / 2
  END SUBROUTINE calcEB_2d

  SUBROUTINE calcEn_3d(IS3, en)
    INTEGER(4), INTENT(in) :: IS3(0:, 0:, 0:)
    INTEGER(4), INTENT(out) :: en

    INTEGER(4) :: x, y, z
    INTEGER(4), POINTER :: e, w, s, n, u, d

    en = 0
    DO z = 1, l_z, 1
       DO y = 1, l_y, 1
          DO x = 1, l_x, 1
             CALL setNeighbour_3d(IS3(0:l_x+1,0:l_y+1,0:l_z+1),x,y,z,e,w,s,n,u,d)
             en = en - IS3(x,y,z)*(e+w+s+n+u+d)
          END DO
       END DO
    END DO
    DO y = 1, l_y, 1
       DO x = 1, l_x, 1
          en = en - IS3(x,y,0)*IS3(x,y,1)-IS3(x,y,l_z)*IS3(x,y,l_z+1)
       END DO
    END DO
    en = en / 2
  END SUBROUTINE calcEn_3d

  SUBROUTINE calcEE_2d(IS2, en)
    INTEGER(4), INTENT(in) :: IS2(0:, 0:)
    INTEGER(4), INTENT(out) :: en

    INTEGER(4) :: x

    en = 0
    DO x = 1, l_x, 1
       en = en - IS2(x, l_z / 2) * IS2(x, l_z / 2 + 1)
    END DO
  END SUBROUTINE calcEE_2d

  SUBROUTINE calcEE_3d(IS3, en)
    INTEGER(4), INTENT(in) :: IS3(0:, 0:, 0:)
    INTEGER(4), INTENT(out) :: en

    INTEGER(4) :: x, y

    en = 0
    DO y = 1, l_y, 1
       DO x = 1, l_x, 1
          en = en - IS3(x, y, l_z / 2) * IS3(x, y, l_z / 2 + 1)
       END DO
    END DO
  END SUBROUTINE calcEE_3d

  SUBROUTINE initSp_2d(IS2)
    INTEGER(4), INTENT(inout) :: IS2(0:, 0:)

    INTEGER(4) :: err, z
    TYPE(VSL_STREAM_STATE) :: str_sp

    IS2(0, 0:l_z + 1) = 0
    IS2(l_x + 1, 0:l_z + 1) = 0

    SELECT CASE (id_BC)
    CASE (1) !BC: anti-parallel
       IS2(1:l_x, 0) = 1
       IS2(1:l_x, l_z + 1) = -1
    CASE (2) !BC: parallel
       IS2(1:l_x, 0) = 1
       IS2(1:l_x, l_z + 1) = 1
    CASE (3) !BC: free
       IS2(1:l_x, 0) = 0
       IS2(1:l_x, l_z + 1) = 0
    END SELECT

    SELECT CASE (id_IC)
    CASE (1)
       IS2(1:l_x, 1:l_z/2) = 1
       IS2(1:l_x, l_z/2 + 1:l_z) = -1
    CASE (2)
       IS2(1:l_x, 1:l_z) = 1
    CASE (3)
       err = vslnewstream(str_sp, VSL_BRNG_MT19937, 50)
       DO z = 1, l_z, 1
          err = virnguniform(VSL_RNG_METHOD_UNIFORM_STD, str_sp, l_x, IS2(1:l_x, z), 0, 2)
       END DO
       err = vsldeletestream(str_sp)
       IS2(1:l_x, 1:l_z) = 2 * IS2(1:l_x, 1:l_z) - 1
    END SELECT
  END SUBROUTINE initSp_2d

  SUBROUTINE initSp_3d(IS3)
    INTEGER(4), INTENT(inout) :: IS3(0:, 0:, 0:)

    INTEGER(4) :: err, x, y, z
    TYPE(VSL_STREAM_STATE) :: str_sp

    IS3(0, 0, 0:l_z + 1) = 0
    IS3(0, l_y + 1, 0:l_z + 1) = 0
    IS3(l_x + 1, 0, 0:l_z + 1) = 0
    IS3(l_x + 1, l_y + 1, 0:l_z + 1) = 0

    SELECT CASE (id_BC)
    CASE (1) !BC: anti-parallel
       IS3(1:l_x, 1:l_y, 0) = 1
       IS3(1:l_x, 1:l_y, l_z + 1) = -1
    CASE (2) !BC: parallel
       IS3(1:l_x, 1:l_y, 0) = 1
       IS3(1:l_x, 1:l_y, l_z + 1) = 1
    CASE (3) !BC: free
       IS3(1:l_x, 1:l_y, 0) = 0
       IS3(1:l_x, 1:l_y, l_z + 1) = 0
    END SELECT

    SELECT CASE (id_IC)
    CASE (1)
       IS3(1:l_x, 1:l_y, 1:l_z/2) = 1
       IS3(1:l_x, 1:l_y, l_z/2 + 1:l_z) = -1
    CASE (2)
       IS3(1:l_x, 1:l_y, 1:l_z) = 1
    CASE (3)
       err = vslnewstream(str_sp, VSL_BRNG_MT19937, 50)
       DO z = 1, l_z, 1
          DO y = 1, l_y, 1
             err = virnguniform(VSL_RNG_METHOD_UNIFORM_STD, str_sp, l_x, IS3(1:l_x, y, z), 0, 2)
          END DO
       END DO
       err = vsldeletestream(str_sp)
       IS3(1:l_x, 1:l_y, 1:l_z) = 2 * IS3(1:l_x, 1:l_y, 1:l_z) - 1
    END SELECT
  END SUBROUTINE initSp_3d

  SUBROUTINE SSF_2d(x,z,p,IS2,eb_prv,eb_nxt,mb_prv,mb_nxt)
    INTEGER(4), INTENT(in) :: x, z
    REAL(8), INTENT(in) :: p
    INTEGER(4), INTENT(inout) :: IS2(0:,0:)
    INTEGER(4), INTENT(in) :: eb_prv, mb_prv
    INTEGER(4), INTENT(out) :: eb_nxt, mb_nxt

    INTEGER(4), POINTER :: e, w, s, n

    CALL setNeighbour_2d(IS2(0:l_x+1,0:l_z+1),x,z,e,w,s,n)

    IF (p <= p_2d(IS2(x,z),e,w,s,n)) THEN
       eb_nxt = eb_prv + 2 * IS2(x,z) * (e+w+s+n)
       mb_nxt = mb_prv - 2 * IS2(x,z)
       IS2(x,z) = - IS2(x,z)
    ELSE
       eb_nxt = eb_prv
       mb_nxt = mb_prv
    END IF
  END SUBROUTINE SSF_2d

  SUBROUTINE SSF_3d(x,y,z,p,IS3,eb_prv,eb_nxt,mb_prv,mb_nxt)
    INTEGER(4), INTENT(in) :: x, y, z
    REAL(8), INTENT(in) :: p
    INTEGER(4), INTENT(inout) :: IS3(0:,0:,0:)
    INTEGER(4), INTENT(in) :: eb_prv, mb_prv
    INTEGER(4), INTENT(out) :: eb_nxt, mb_nxt

    INTEGER(4), POINTER :: e, w, s, n, u, d

    CALL setNeighbour_3d(IS3(0:,0:,0:),x,y,z,e,w,s,n,u,d)

    IF (p <= p_3d(IS3(x,y,z),e,w,s,n,u,d)) THEN
       eb_nxt = eb_prv + 2 * IS3(x,y,z) * (e+w+s+n+u+d)
       mb_nxt = mb_prv - 2 * IS3(x,y,z)
       IS3(x,y,z) = - IS3(x,y,z)
    ELSE
       eb_nxt = eb_prv
       mb_nxt = mb_prv
    END IF
  END SUBROUTINE SSF_3d

  SUBROUTINE mSSFs_2d(r_x, r_z, r_p, IS2, eb, mb)
    INTEGER(4), INTENT(in) :: r_x(1:), r_z(1:)
    REAL(8), INTENT(in) :: r_p(1:)
    INTEGER(4), INTENT(inout) :: IS2(0:, 0:), eb(0:), mb(0:)

    INTEGER(4) :: i_st, rel

    DO i_st = 1, n_st, 1
       CALL SSF_2d(r_x(i_st), r_z(i_st), r_p(i_st), IS2(0:, 0:), eb(i_st - 1), eb(i_st), mb(i_st - 1), mb(i_st))
    END DO
  END SUBROUTINE mSSFs_2d

  SUBROUTINE mSSFs_3d(r_x, r_y, r_z, r_p, IS3, eb, mb)
    INTEGER(4), INTENT(in) :: r_x(1:), r_y(1:), r_z(1:)
    REAL(8), INTENT(in) :: r_p(1:)
    INTEGER(4), INTENT(inout) :: IS3(0:, 0:, 0:), eb(0:), mb(0:)

    INTEGER(4) :: i_st, rel

    DO i_st = 1, n_st, 1
       CALL SSF_3d(r_x(i_st), r_y(i_st), r_z(i_st), r_p(i_st), IS3(0:, 0:, 0:), eb(i_st - 1), eb(i_st), mb(i_st - 1), mb(i_st))
    END DO
  END SUBROUTINE mSSFs_3d

  SUBROUTINE shift_2d(IS2, pmp, eb)
    INTEGER(4), INTENT(inout) :: IS2(0:, 0:), eb
    INTEGER(4), INTENT(out) :: pmp

    INTEGER(4) :: eb_prv, eb_nxt

    CALL calcEE_2d(IS2(0:, 0:), eb_prv)
    IS2(1:l_x, 1:l_z/2) = CSHIFT(IS2(1:l_x, 1:l_z/2), 1, 1)
    CALL calcEE_2d(IS2(0:, 0:), eb_nxt)
    pmp = eb_nxt - eb_prv
    eb = eb + pmp
  END SUBROUTINE shift_2d

  SUBROUTINE shift_3d(IS3, pmp, eb)
    INTEGER(4), INTENT(inout) :: IS3(0:, 0:, 0:), eb
    INTEGER(4), INTENT(out) :: pmp

    INTEGER(4) :: eb_prv, eb_nxt

    CALL calcEE_3d(IS3(0:, 0:, 0:), eb_prv)
    IS3(1:l_x, 1:l_y, 1:l_z/2) = CSHIFT(IS3(1:l_x, 1:l_y, 1:l_z/2), 1, 1)
    CALL calcEE_3d(IS3(0:, 0:, 0:), eb_nxt)
    pmp = eb_nxt - eb_prv
    eb = eb + pmp
  END SUBROUTINE shift_3d

  SUBROUTINE calcAC(a, ac)
    INTEGER(4), INTENT(in) :: a(1:)
    REAL(8), INTENT(out) :: ac(1:)

    INTEGER(4) :: t, t_, l_t

    ac(1:l_t/2) = 0.0d0
    DO t = 1, l_t/2, 1
       DO t_ = 1, l_t - t, 1
          ac(t) = ac(t) + a(t_)*a(t_ + t)
       END DO
       ac(t) = ac(t)/DBLE(l_t - t) - DBLE(SUM(a(1:l_t)))/DBLE(l_t)
    END DO
  END SUBROUTINE calcAC

  SUBROUTINE countSamples(n_s0)
    INTEGER(4), INTENT(out) :: n_s0
    CHARACTER(4) :: ss

    INTEGER(4) :: s, st_sp

    DO s = 1, 5000000, 1
       WRITE (ss, '(i0.4)') s
       st_sp = access("eb_sweep/en_bulk_s"//ss//"_sweep.bin", " ")
       IF (st_sp > 0) THEN
          n_s0 = s - 1; EXIT
       END IF
    END DO
  END SUBROUTINE countSamples

  PURE SUBROUTINE calcAvgVar(n_s, sample, avg, var_sq)
    INTEGER(4), INTENT(in) :: n_s
    REAL(8), INTENT(in) :: sample(1:n_s)
    REAL(8), INTENT(out) :: avg, var_sq

    REAL(8) :: sq(1:n_s)

    avg = SUM(sample(1:n_s))/DBLE(n_s)
    sq(1:n_s) = sample(1:n_s)**2
    var_sq = (SUM(sq(1:n_s))/DBLE(n_s) - avg**2)/DBLE(n_s - 1)
  END SUBROUTINE calcAvgVar

  PURE SUBROUTINE convertStreamSamples(l_th, n_s, stream, sample)
    INTEGER(4), INTENT(in) ::  l_th, n_s
    REAL(8), INTENT(in) :: stream(1:)
    REAL(8), INTENT(out) :: sample(1:)

    INTEGER(4) :: s, l_s

    l_s = (SIZE(stream) - l_th)/n_s
    DO CONCURRENT(s=1:n_s:1)
       sample(s) = SUM(stream(1 + l_s*(s - 1):l_s*s))/DBLE(l_s)
    END DO
  END SUBROUTINE convertStreamSamples
END MODULE mod_proc
