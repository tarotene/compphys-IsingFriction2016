  INCLUDE 'mkl_vsl.f90'
  MODULE mod_proc
    USE mkl_vsl
    USE mkl_vsl_type
    USE IFPORT, ONLY:access

    IMPLICIT NONE
  CONTAINS
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

      err = vslsavestreamf(stream, filename)
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

    SUBROUTINE applyPBC_2d(l_x,l_z,ex,ez,wx,wz,sx,sz,nx,nz)
      INTEGER(4), INTENT(in) :: l_x, l_z
      INTEGER(4), INTENT(out) :: ex(1:l_x,1:l_z), ez(1:l_x,1:l_z)
      INTEGER(4), INTENT(out) :: wx(1:l_x,1:l_z), wz(1:l_x,1:l_z)
      INTEGER(4), INTENT(out) :: sx(1:l_x,1:l_z), sz(1:l_x,1:l_z)
      INTEGER(4), INTENT(out) :: nx(1:l_x,1:l_z), nz(1:l_x,1:l_z)

      INTEGER(4) :: x, z

      FORALL (x=1:l_x, z=1:l_z)
         ex(x,z) = MOD(x,l_x)+1;         ez(x,z) = z
         wx(x,z) = l_x-MOD(l_x-x+1,l_x); wz(x,z) = z
         sx(x,z) = x;                    sz(x,z) = z - 1
         nx(x,z) = x;                    nz(x,z) = z + 1
         ! sx(x,z) = x;                    sz(x,z) = l_z-MOD(l_z-z+1,l_z)
         ! nx(x,z) = x;                    nz(x,z) = MOD(z,l_z)+1
      END FORALL
    END SUBROUTINE applyPBC_2d

    SUBROUTINE makeBins(nb,lb,series,bin)
      INTEGER(4), INTENT(in) :: nb, lb
      REAL(8), INTENT(in) :: series(1:)
      REAL(8), INTENT(out) :: bin(1:)

      INTEGER(4) :: ib

      FORALL (ib=1:nb:1)
         bin(ib) = SUM(series(1+(ib-1)*lb:ib*lb)) / DBLE(lb)
      END FORALL
    END SUBROUTINE makeBins

    SUBROUTINE makeStats(nb,bin,mean,err)
      INTEGER(4), INTENT(in) :: nb
      REAL(8), INTENT(in) :: bin(1:)
      REAL(8), INTENT(out) :: mean, err

      mean = SUM(bin(1:nb)) / DBLE(nb)
      err = SQRT(SUM(bin(1:nb)**2)/DBLE(nb)-mean**2)/DBLE(nb-1)
    END SUBROUTINE makeStats

    SUBROUTINE countLines(slot,filename,n_lines)
      INTEGER(4), INTENT(in) :: slot
      CHARACTER(len=*), INTENT(in) :: filename
      INTEGER(4), INTENT(out) :: n_lines

      INTEGER(4) :: i_beta, istat

      OPEN(slot, file=filename//".dat", status="old")
      DO i_beta = 1, 32767, 1
         READ(slot, '()', iostat=istat)
         IF ( istat < 0 ) EXIT
         n_lines = n_lines + 1
      END DO
      CLOSE(slot)
    END SUBROUTINE countLines

    !TODO: 関数化
    SUBROUTINE readParams_2d(l_x,l_z,beta,vel,l_t,id_BC,id_IC,n_s)
      INTEGER(4), INTENT(out) :: l_x, l_z
      REAL(8), INTENT(out) :: beta
      INTEGER(4), INTENT(out) :: vel, l_t, id_BC, id_IC, n_s

      READ (*, *) l_x, l_z, beta, vel, l_t, id_BC, id_IC, n_s
    END SUBROUTINE readParams_2d

    SUBROUTINE readParams_3d(l_x,l_y,l_z,beta,vel,l_t,id_BC,id_IC,n_s)
      INTEGER(4), INTENT(out) :: l_x, l_y, l_z
      REAL(8), INTENT(out) :: beta
      INTEGER(4), INTENT(out) :: vel, l_t, id_BC, id_IC, n_s

      READ (*, *) l_x, l_y, l_z, beta, vel, l_t, id_BC, id_IC, n_s
    END SUBROUTINE readParams_3d

    SUBROUTINE readParams_2d_eq(l_x,l_z,beta,l_t0,l_t1,id_BC,id_IC,n_s)
      INTEGER(4), INTENT(out) :: l_x, l_z
      REAL(8), INTENT(out) :: beta
      INTEGER(4), INTENT(out) :: l_t0, l_t1, id_BC, id_IC, n_s
      READ (*, *) l_x, l_z, beta, l_t0, l_t1, id_BC, id_IC, n_s
    END SUBROUTINE readParams_2d_eq

    SUBROUTINE readParams_3d_eq(l_x,l_y,l_z,beta,l_t,id_BC,id_IC,n_s)
      INTEGER(4), INTENT(out) :: l_x, l_y, l_z
      REAL(8), INTENT(out) :: beta
      INTEGER(4), INTENT(out) :: l_t, id_BC, id_IC, n_s

      READ (*, *) l_x, l_y, l_z, beta, l_t, id_BC, id_IC, n_s
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

    SUBROUTINE MTC_readParams_2d(slot,filename,l_x,l_z,vel,l_t0,l_t1,id_BC,id_IC)
      INTEGER(4), INTENT(in) :: slot
      CHARACTER(len=*), INTENT(in) :: filename
      INTEGER(4), INTENT(out) :: l_x, l_z, vel
      INTEGER(4), INTENT(out) :: l_t0, l_t1
      INTEGER(4), INTENT(out) :: id_BC, id_IC

      OPEN(slot,file=filename//".dat",status="old")
      READ(slot, '()')
      READ(slot, *) l_x, l_z, vel, l_t0, l_t1, id_BC, id_IC
      CLOSE(slot)
    END SUBROUTINE MTC_readParams_2d

    SUBROUTINE MTC_readLB(slot,filename,lb)
      INTEGER(4), INTENT(in) :: slot
      CHARACTER(len=*), INTENT(in) :: filename
      INTEGER(4), INTENT(out) :: lb

      OPEN(slot,file=filename//".dat",status="old")
      READ(slot, '()')
      READ(slot, *) lb
      CLOSE(slot)
    END SUBROUTINE MTC_readLB

    SUBROUTINE MTC_readParams_3d(slot,filename,l_x,l_y,l_z,vel,l_t0,l_t1,id_BC,id_IC)
      INTEGER(4), INTENT(in) :: slot
      CHARACTER(len=*), INTENT(in) :: filename
      INTEGER(4), INTENT(out) :: l_x, l_y, l_z, vel
      INTEGER(4), INTENT(out) :: l_t0, l_t1
      INTEGER(4), INTENT(out) :: id_BC, id_IC

      OPEN(slot,file=filename//".dat",status="old")
      READ(slot, '()')
      READ(slot, *) l_x, l_y, l_z, vel, l_t0, l_t1, id_BC, id_IC
      CLOSE(slot)
    END SUBROUTINE MTC_readParams_3d

    SUBROUTINE MTC_readParams_2d_eq(slot,filename,l_x,l_z,l_t0,l_t1,id_BC,id_IC)
      INTEGER(4), INTENT(in) :: slot
      CHARACTER(len=*), INTENT(in) :: filename
      INTEGER(4), INTENT(out) :: l_x, l_z
      INTEGER(4), INTENT(out) :: l_t0, l_t1
      INTEGER(4), INTENT(out) :: id_BC, id_IC

      OPEN(slot,file=filename//".dat",status="old")
      READ(slot, '()')
      READ(slot, *) l_x, l_z, l_t0, l_t1, id_BC, id_IC
      CLOSE(slot)
    END SUBROUTINE MTC_readParams_2d_eq

    SUBROUTINE MTC_readParams_3d_eq(slot,filename,l_x,l_y,l_z,l_t0,l_t1,id_BC,id_IC)
      INTEGER(4), INTENT(in) :: slot
      CHARACTER(len=*), INTENT(in) :: filename
      INTEGER(4), INTENT(out) :: l_x, l_y, l_z
      INTEGER(4), INTENT(out) :: l_t0, l_t1
      INTEGER(4), INTENT(out) :: id_BC, id_IC

      OPEN(slot,file=filename//".dat",status="old")
      READ(slot, '()')
      READ(slot, *) l_x, l_y, l_z, l_t0, l_t1, id_BC, id_IC
      CLOSE(slot)
    END SUBROUTINE MTC_readParams_3d_eq

    SUBROUTINE MTC_readBetas(slot,filename,MSC_beta,n_w,n_betas)
      INTEGER(4), INTENT(in) :: slot
      CHARACTER(len=*), INTENT(in) :: filename
      REAL(8), INTENT(out) :: MSC_beta(1:,0:)
      INTEGER(4), INTENT(in) :: n_w
      INTEGER(4), INTENT(in) :: n_betas

      INTEGER(4) :: i_w, i_bit

      OPEN(slot,file=filename//".dat",status="old")
      DO i_w = 1, n_w, 1
         DO i_bit = 0, MIN(n_betas-64*(i_w-1),64) - 1, 1
            READ(slot, *) MSC_beta(i_w,i_bit)
         END DO
      END DO
      CLOSE(slot)
      MSC_beta(n_w,n_betas-64*(n_w-1):63) = MSC_beta(n_w,n_betas-64*(n_w-1)-1)
    END SUBROUTINE MTC_readBetas

    SUBROUTINE MSC_calcEB_2d(IW2,l_x,l_z,MSC_eb)
      INTEGER(8), INTENT(in) :: IW2(1:,0:)
      INTEGER(4), INTENT(in) :: l_x, l_z
      INTEGER(4), INTENT(out) :: MSC_eb(0:)

      INTEGER(8) :: el_half(0:1,1:l_x,1:l_z), el_quar(1:l_x)
      INTEGER(4) :: x, z
      INTEGER(4) :: i_bit

      !サイト(x,z)の南,東の2ボンド（開放端z=0を含む）
      DO z = 1, l_z, 1
         DO x = 1, l_x, 1
            el_half(0,x,z) = IEOR(IEOR(IW2(x,z-1),IW2(x,z)),IEOR(IW2(x,z),IW2(MOD(x,l_x)+1,z)))
            el_half(1,x,z) = IAND(IEOR(IW2(x,z-1),IW2(x,z)),IEOR(IW2(x,z),IW2(MOD(x,l_x)+1,z)))
         END DO
      END DO
      !開放端z=L_z+1
      el_quar(1:l_x) = IEOR(IW2(1:l_x,l_z),IW2(1:l_x,l_z+1))
      DO i_bit = 0, 63, 1
         MSC_eb(i_bit) = SUM(IBITS(el_half(0,1:l_x,1:l_z),i_bit,1))+2*SUM(IBITS(el_half(1,1:l_x,1:l_z),i_bit,1))+SUM(IBITS(el_quar(1:l_x),i_bit,1))
      END DO
      MSC_eb(0:63) = 2*MSC_eb(0:63) - l_x*(2*l_z + 1)
    END SUBROUTINE MSC_calcEB_2d

    SUBROUTINE MSC_calcMB_2d(IW2,l_x,l_z,MSC_mb)
      INTEGER(8), INTENT(inout) :: IW2(1:,1:)
      INTEGER(4), INTENT(in) :: l_x, l_z
      INTEGER(4), INTENT(out) :: MSC_mb(0:)

      INTEGER(4) :: i_bit

      DO i_bit = 0, 63, 1
         MSC_mb(i_bit) = 2*SUM(IAND(IW2(1:l_x,1:l_z),1)) - l_x*l_z
         IW2(1:l_x,1:l_z) = ISHFTC(IW2(1:l_x,1:l_z),-1)
      END DO
    END SUBROUTINE MSC_calcMB_2d

    SUBROUTINE MSC_initIW_2d(id_BC,id_IC,IW2,n_w,l_x,l_z)
      INTEGER(4), INTENT(in) :: id_BC, id_IC
      INTEGER(8), INTENT(inout) :: IW2(1:,1:,0:)
      INTEGER(4), INTENT(in) :: n_w
      INTEGER(4), INTENT(in) :: l_x, l_z

      SELECT CASE (id_BC)
      CASE (1) !BC: anti-parallel
         IW2(1:n_w, 1:l_x, 0) = -1_8
         IW2(1:n_w, 1:l_x, l_z + 1) = 0
      CASE (2) !BC: parallel
         IW2(1:n_w, 1:l_x, 0) = -1_8
         IW2(1:n_w, 1:l_x, l_z + 1) = -1_8
      END SELECT

      SELECT CASE (id_IC)
      CASE (1) !IC: domain-wall state
         IW2(1:n_w, 1:l_x, 1:l_z/2) = -1_8
         IW2(1:n_w, 1:l_x, l_z/2 + 1:l_z) = 0
      CASE (2) !IC: magnetized state
         IW2(1:n_w, 1:l_x, 1:l_z) = -1_8
      END SELECT
    END SUBROUTINE MSC_initIW_2d

    SUBROUTINE MTC_setRandomTable(MSC_beta,r_a,max_l,n_w)
      !For Metropolis
      REAL(8), INTENT(in) :: MSC_beta(1:,0:)
      INTEGER(8), INTENT(out) :: r_a(1:,0:,0:)
      INTEGER(4), INTENT(in) :: max_l
      INTEGER(4), INTENT(in) :: n_w

      INTEGER(4) :: nboltz8(1:n_w), nboltz4(1:n_w)
      INTEGER(4) :: i_bit, i_w

      r_a(1:n_w,0:3,0:max_l-1) = 0
      DO i_bit = 0, 63, 1
         nboltz8(1:n_w) = INT(DBLE(max_l)*EXP(-8.0d0*MSC_beta(1:n_w,i_bit)))
         nboltz4(1:n_w) = INT(DBLE(max_l)*EXP(-4.0d0*MSC_beta(1:n_w,i_bit)))
         DO i_w = 1, n_w, 1
            r_a(i_w,0,nboltz8(i_w)+1:nboltz4(i_w)) = r_a(i_w,0,nboltz8(i_w)+1:nboltz4(i_w)) + 1
            r_a(i_w,1,nboltz8(i_w)+1:nboltz4(i_w)) = r_a(i_w,1,nboltz8(i_w)+1:nboltz4(i_w)) + 1
            r_a(i_w,1,nboltz4(i_w)+1:max_l-1)      = r_a(i_w,1,nboltz4(i_w)+1:max_l-1) + 1
            r_a(i_w,2,nboltz8(i_w)+1:nboltz4(i_w)) = r_a(i_w,2,nboltz8(i_w)+1:nboltz4(i_w)) + 1
            r_a(i_w,2,nboltz4(i_w)+1:max_l-1)      = r_a(i_w,2,nboltz4(i_w)+1:max_l-1) + 1
            r_a(i_w,3,0:nboltz8(i_w))              = r_a(i_w,3,0:nboltz8(i_w)) + 1
         END DO
         r_a(1:n_w,0:3,0:max_l-1) = ISHFTC(r_a(1:n_w,0:3,0:max_l-1),-1)
      END DO
    END SUBROUTINE MTC_setRandomTable

    SUBROUTINE MSC_SSF_2d(c,e,w,s,n,r,a,b,b1,b2)
      INTEGER(8), INTENT(inout) :: c
      INTEGER(8), INTENT(in) :: e, w, s, n
      INTEGER(8), INTENT(in) :: r(0:)
      ! WORKING VARS
      INTEGER(8), INTENT(inout) :: a, b(0:), b1(0:), b2(0:)
      ! LOCAL VARS
      INTEGER(8) :: de, dw, ds, dn

      de = IEOR(c,e); dw = IEOR(c,w); ds = IEOR(c,s); dn = IEOR(c,n)
      ! ドメインウォールの有無
      b1(0) = IEOR(de,dw); b1(1) = IAND(de,dw)
      b2(0) = IEOR(ds,dn); b2(1) = IAND(ds,dn)
      ! b(0)+2*b(0)+4*b(2): ドメインウォールの有無（0~4）
      b(0) = IEOR(b1(0),b2(0))
      b(1) = IEOR(IEOR(b1(1),b2(1)),IAND(b1(0),b2(0)))
      b(2) = IOR(IAND(IEOR(b1(1),b2(1)),IAND(b1(0),b2(0))),IAND(b1(1),b2(1)))
      ! a:=乱数と局所エネルギーの和の3桁目（アクセプト時に初めて1）
      a = IEOR(r(3),IOR(IAND(IEOR(b(2),r(2)),IOR(IAND(IEOR(b(1),r(1)),IAND(b(0),r(0))),IAND(b(1),r(1)))),IAND(b(2),r(2))))
      c = IEOR(c,a)
    END SUBROUTINE MSC_SSF_2d

    SUBROUTINE MSC_mSSFs_2d_eq(r_l,r_x,r_z,n_st,ex,ez,wx,wz,sx,sz,nx,nz,r_a,IW2)
      INTEGER(4), INTENT(in) :: r_l(1:)
      INTEGER(4), INTENT(in) :: r_x(1:), r_z(1:)
      INTEGER(4), INTENT(in) :: n_st
      INTEGER(4), INTENT(in) :: ex(1:,1:), ez(1:,1:)
      INTEGER(4), INTENT(in) :: wx(1:,1:), wz(1:,1:)
      INTEGER(4), INTENT(in) :: sx(1:,1:), sz(1:,1:)
      INTEGER(4), INTENT(in) :: nx(1:,1:), nz(1:,1:)
      INTEGER(8), INTENT(in) :: r_a(0:,0:)
      INTEGER(8), TARGET, INTENT(inout) :: IW2(1:,0:)

      INTEGER(4) :: i_bit
      INTEGER(4) :: i_st
      INTEGER(8), POINTER :: c, e, w, s, n
      INTEGER(8) :: a, b(0:2), b1(0:1), b2(0:1)

      DO i_st = 1, n_st, 1
         c => IW2(r_x(i_st),r_z(i_st))
         e => IW2(ex(r_x(i_st),r_z(i_st)), ez(r_x(i_st),r_z(i_st)))
         w => IW2(wx(r_x(i_st),r_z(i_st)), wz(r_x(i_st),r_z(i_st)))
         s => IW2(sx(r_x(i_st),r_z(i_st)), sz(r_x(i_st),r_z(i_st)))
         n => IW2(nx(r_x(i_st),r_z(i_st)), nz(r_x(i_st),r_z(i_st)))
         CALL MSC_SSF_2d(c,e,w,s,n,r_a(0:3,r_l(i_st)),a,b(0:2),b1(0:1),b2(0:1))
      END DO
    END SUBROUTINE MSC_mSSFs_2d_eq

    SUBROUTINE MSC_mSSFs_2d_THERMALIZE(r_l,r_x,r_z,vel,n_st2,l_x,l_z,ex,ez,wx,wz,sx,sz,nx,nz,r_a,IW2)
      INTEGER(4), INTENT(in) :: r_l(1:)
      INTEGER(4), INTENT(in) :: r_x(1:), r_z(1:)
      INTEGER(4), INTENT(in) :: vel, n_st2, l_x, l_z
      INTEGER(4), INTENT(in) :: ex(1:,1:), ez(1:,1:)
      INTEGER(4), INTENT(in) :: wx(1:,1:), wz(1:,1:)
      INTEGER(4), INTENT(in) :: sx(1:,1:), sz(1:,1:)
      INTEGER(4), INTENT(in) :: nx(1:,1:), nz(1:,1:)
      INTEGER(8), INTENT(in) :: r_a(0:,0:)
      INTEGER(8), TARGET, INTENT(inout) :: IW2(1:,0:)

      INTEGER(4) :: i_vel, i_bit
      INTEGER(4) :: i_st
      INTEGER(8), POINTER :: c, e, w, s, n
      INTEGER(8) :: a, b(0:2), b1(0:1), b2(0:1)

      DO i_vel = 1, vel, 1
         IW2(1:l_x,1:l_z/2) = CSHIFT(IW2(1:l_x,1:l_z/2),1,1)
         DO i_st = 1, n_st2, 1
            c => IW2(r_x(i_st),r_z(i_st))
            e => IW2(ex(r_x(i_st),r_z(i_st)), ez(r_x(i_st),r_z(i_st)))
            w => IW2(wx(r_x(i_st),r_z(i_st)), wz(r_x(i_st),r_z(i_st)))
            s => IW2(sx(r_x(i_st),r_z(i_st)), sz(r_x(i_st),r_z(i_st)))
            n => IW2(nx(r_x(i_st),r_z(i_st)), nz(r_x(i_st),r_z(i_st)))
            CALL MSC_SSF_2d(c,e,w,s,n,r_a(0:3,r_l(i_st+(i_vel-1)*n_st2)),a,b(0:2),b1(0:1),b2(0:1))
         END DO
      END DO
    END SUBROUTINE MSC_mSSFs_2d_THERMALIZE

    SUBROUTINE MSC_calcEE_2d(IW2_,l_x,l_z,ee)
      INTEGER(8), INTENT(in) :: IW2_(1:,1:)
      INTEGER(4), INTENT(in) :: l_x, l_z
      INTEGER(4), INTENT(out) :: ee(0:)

      INTEGER(4) :: i_bit
      INTEGER(4) :: x

      ee(0:63) = 0
      DO i_bit = 0, 63, 1
         DO x = 1, l_x, 1
            ee(i_bit) = ee(i_bit) + IBITS(IEOR(IW2_(x,l_z/2),IW2_(x,l_z/2+1)),i_bit,1)
         END DO
      END DO
      ee(0:63) = 2 * ee(0:63) - l_x
    END SUBROUTINE MSC_calcEE_2d

    SUBROUTINE MSC_calcME_2d(IW2_,l_x,l_z,me)
      INTEGER(8), INTENT(in) :: IW2_(1:,1:)
      INTEGER(4), INTENT(in) :: l_x, l_z
      INTEGER(4), INTENT(out) :: me(0:)

      INTEGER(4) :: i_bit
      INTEGER(4) :: x

      me(0:63) = 0
      DO i_bit = 0, 63, 1
         me(i_bit) = SUM(IBITS(IW2_(1:l_x,l_z/2:l_z/2+1),i_bit,1))
      END DO
      me(0:63) = 2 * (me(0:63) - l_x)
    END SUBROUTINE MSC_calcME_2d

    SUBROUTINE MSC_mSSFs_2d_OBSERVE(r_l,r_x,r_z,vel,n_st2,l_x,l_z,ex,ez,wx,wz,sx,sz,nx,nz,r_a,IW2_,ee,pu)
      INTEGER(4), INTENT(in) :: r_l(1:)
      INTEGER(4), INTENT(in) :: r_x(1:), r_z(1:)
      INTEGER(4), INTENT(in) :: vel, n_st2, l_x, l_z
      INTEGER(4), INTENT(in) :: ex(1:,1:), ez(1:,1:)
      INTEGER(4), INTENT(in) :: wx(1:,1:), wz(1:,1:)
      INTEGER(4), INTENT(in) :: sx(1:,1:), sz(1:,1:)
      INTEGER(4), INTENT(in) :: nx(1:,1:), nz(1:,1:)
      INTEGER(8), INTENT(in) :: r_a(0:,0:)
      INTEGER(8), TARGET, INTENT(inout) :: IW2_(1:,0:)
      INTEGER(4), INTENT(inout) :: ee(0:)
      INTEGER(4), INTENT(out) :: pu(0:)

      INTEGER(4) :: i_vel, i_bit
      INTEGER(4) :: x, i_st
      INTEGER(4) :: ee_next(0:63)
      INTEGER(8), POINTER :: c, e, w, s, n
      INTEGER(8) :: a, b(0:2), b1(0:1), b2(0:1)

      pu(0:63) = 0
      DO i_vel = 1, vel, 1
         IW2_(1:l_x,1:l_z/2) = CSHIFT(IW2_(1:l_x,1:l_z/2),1,1)
         CALL MSC_calcEE_2d(IW2_(1:l_x,1:l_z),l_x,l_z,ee_next(0:63))
         pu(0:63) = pu(0:63) + ee_next(0:63) - ee(0:63)

         DO i_st = 1, n_st2, 1
            c => IW2_(r_x(i_st),r_z(i_st))
            e => IW2_(ex(r_x(i_st),r_z(i_st)),ez(r_x(i_st),r_z(i_st)))
            w => IW2_(wx(r_x(i_st),r_z(i_st)),wz(r_x(i_st),r_z(i_st)))
            s => IW2_(sx(r_x(i_st),r_z(i_st)),sz(r_x(i_st),r_z(i_st)))
            n => IW2_(nx(r_x(i_st),r_z(i_st)),nz(r_x(i_st),r_z(i_st)))
            CALL MSC_SSF_2d(c,e,w,s,n,r_a(0:3,r_l(i_st+(i_vel-1)*n_st2)),a,b(0:2),b1(0:1),b2(0:1))
         END DO

         CALL MSC_calcEE_2d(IW2_(1:l_x,1:l_z),l_x,l_z,ee(0:63))
      END DO
    END SUBROUTINE MSC_mSSFs_2d_OBSERVE

    SUBROUTINE MTC_makeLabelsBeta(MSC_beta,n_w,sbeta)
      REAL(8), INTENT(in) :: MSC_beta(1:,0:)
      INTEGER(4), INTENT(in) :: n_w
      CHARACTER(len=11), INTENT(out) :: sbeta(1:,0:)

      REAL(8) :: decbeta(1:n_w,0:63)
      CHARACTER(len=6) :: sdecbeta

      INTEGER(4) :: i_w, i_bit

      decbeta(1:n_w,0:63) = MSC_beta(1:n_w,0:63)-DBLE(INT(MSC_beta(1:n_w,0:63)))
      DO i_w = 1, n_w, 1
         DO i_bit = 0, 63, 1
            WRITE(sdecbeta,'(F6.4)') decbeta(i_w,i_bit)
            WRITE(sbeta(i_w,i_bit), '(A4,I2.2,A5)') "beta",INT(MSC_beta(i_w,i_bit)),sdecbeta(2:6)
         END DO
      END DO
    END SUBROUTINE MTC_makeLabelsBeta

    SUBROUTINE MSC_setSlots(n_w,MSC_sl_iw,MSC_sl_eb,MSC_sl_mb,MSC_sl_ee,MSC_sl_me,MSC_sl_pu)
      INTEGER(4), INTENT(in) :: n_w
      INTEGER(4), INTENT(out), OPTIONAL :: MSC_sl_iw(1:)
      INTEGER(4), INTENT(out), OPTIONAL :: MSC_sl_eb(1:,0:)
      INTEGER(4), INTENT(out), OPTIONAL :: MSC_sl_mb(1:,0:)
      INTEGER(4), INTENT(out), OPTIONAL :: MSC_sl_ee(1:,0:)
      INTEGER(4), INTENT(out), OPTIONAL :: MSC_sl_me(1:,0:)
      INTEGER(4), INTENT(out), OPTIONAL :: MSC_sl_pu(1:,0:)

      INTEGER(4) :: i_w, i_bit

      DO i_w = 1, n_w, 1
         IF(PRESENT(MSC_sl_iw)) MSC_sl_iw(i_w) = 20+i_w
         DO i_bit = 0, 63, 1
            IF(PRESENT(MSC_sl_eb)) MSC_sl_eb(i_w,i_bit) = 20+n_w+i_bit+1+64*(i_w-1)
            IF(PRESENT(MSC_sl_mb)) MSC_sl_mb(i_w,i_bit) = 20+n_w+i_bit+1+64*(i_w-1)+n_w*64
            IF(PRESENT(MSC_sl_ee)) MSC_sl_ee(i_w,i_bit) = 20+n_w+i_bit+1+64*(i_w-1)+2*n_w*64
            IF(PRESENT(MSC_sl_me)) MSC_sl_me(i_w,i_bit) = 20+n_w+i_bit+1+64*(i_w-1)+3*n_w*64
            IF(PRESENT(MSC_sl_pu)) MSC_sl_pu(i_w,i_bit) = 20+n_w+i_bit+1+64*(i_w-1)+4*n_w*64
         END DO
      END DO
    END SUBROUTINE MSC_setSlots

    SUBROUTINE MSC_openSlots_rep(n_w,n_betas,sbeta,MSC_sl_eb,MSC_sl_mb,MSC_sl_ee,MSC_sl_me,MSC_sl_pu)
      INTEGER(4), INTENT(in) :: n_w
      INTEGER(4), INTENT(in) :: n_betas
      CHARACTER(len=11), INTENT(in) :: sbeta(1:,0:)
      INTEGER(4), INTENT(in), OPTIONAL :: MSC_sl_eb(1:,0:)
      INTEGER(4), INTENT(in), OPTIONAL :: MSC_sl_mb(1:,0:)
      INTEGER(4), INTENT(in), OPTIONAL :: MSC_sl_ee(1:,0:)
      INTEGER(4), INTENT(in), OPTIONAL :: MSC_sl_me(1:,0:)
      INTEGER(4), INTENT(in), OPTIONAL :: MSC_sl_pu(1:,0:)

      INTEGER(4) :: i_w, i_bit

      DO i_w = 1, n_w, 1
         DO i_bit = 0, MIN(n_betas-64*(i_w-1),64) - 1, 1
            IF (PRESENT(MSC_sl_eb)) OPEN(MSC_sl_eb(i_w,i_bit),file="_eb_"//sbeta(i_w,i_bit)//".bin",access="stream",status="replace")
            IF (PRESENT(MSC_sl_mb)) OPEN(MSC_sl_mb(i_w,i_bit),file="_mb_"//sbeta(i_w,i_bit)//".bin",access="stream",status="replace")
            IF (PRESENT(MSC_sl_ee)) OPEN(MSC_sl_ee(i_w,i_bit),file="_ee_"//sbeta(i_w,i_bit)//".bin",access="stream",status="replace")
            IF (PRESENT(MSC_sl_me)) OPEN(MSC_sl_me(i_w,i_bit),file="_me_"//sbeta(i_w,i_bit)//".bin",access="stream",status="replace")
            IF (PRESENT(MSC_sl_pu)) OPEN(MSC_sl_pu(i_w,i_bit),file="_pu_"//sbeta(i_w,i_bit)//".bin",access="stream",status="replace")
         END DO
      END DO
    END SUBROUTINE MSC_openSlots_rep

    SUBROUTINE MSC_openSlots_old(n_w,n_betas,sbeta,MSC_sl_eb,MSC_sl_mb,MSC_sl_ee,MSC_sl_me,MSC_sl_pu)
      INTEGER(4), INTENT(in) :: n_w
      INTEGER(4), INTENT(in) :: n_betas
      CHARACTER(len=11), INTENT(in) :: sbeta(1:,0:)
      INTEGER(4), INTENT(in), OPTIONAL :: MSC_sl_eb(1:,0:)
      INTEGER(4), INTENT(in), OPTIONAL :: MSC_sl_mb(1:,0:)
      INTEGER(4), INTENT(in), OPTIONAL :: MSC_sl_ee(1:,0:)
      INTEGER(4), INTENT(in), OPTIONAL :: MSC_sl_me(1:,0:)
      INTEGER(4), INTENT(in), OPTIONAL :: MSC_sl_pu(1:,0:)

      INTEGER(4) :: i_w, i_bit

      DO i_w = 1, n_w, 1
         DO i_bit = 0, MIN(n_betas-64*(i_w-1),64) - 1, 1
            IF (PRESENT(MSC_sl_eb)) OPEN(MSC_sl_eb(i_w,i_bit),file="_eb_"//sbeta(i_w,i_bit)//".bin",access="stream",status="old")
            IF (PRESENT(MSC_sl_mb)) OPEN(MSC_sl_mb(i_w,i_bit),file="_mb_"//sbeta(i_w,i_bit)//".bin",access="stream",status="old")
            IF (PRESENT(MSC_sl_ee)) OPEN(MSC_sl_ee(i_w,i_bit),file="_ee_"//sbeta(i_w,i_bit)//".bin",access="stream",status="old")
            IF (PRESENT(MSC_sl_me)) OPEN(MSC_sl_me(i_w,i_bit),file="_me_"//sbeta(i_w,i_bit)//".bin",access="stream",status="old")
            IF (PRESENT(MSC_sl_pu)) OPEN(MSC_sl_pu(i_w,i_bit),file="_pu_"//sbeta(i_w,i_bit)//".bin",access="stream",status="old")
         END DO
      END DO
    END SUBROUTINE MSC_openSlots_old

    SUBROUTINE MSC_closeSlots(n_w,n_betas,MSC_sl_eb,MSC_sl_mb,MSC_sl_ee,MSC_sl_me,MSC_sl_pu)
      INTEGER(4), INTENT(in) :: n_w
      INTEGER(4), INTENT(in) :: n_betas
      INTEGER(4), INTENT(in), OPTIONAL :: MSC_sl_eb(1:,0:)
      INTEGER(4), INTENT(in), OPTIONAL :: MSC_sl_mb(1:,0:)
      INTEGER(4), INTENT(in), OPTIONAL :: MSC_sl_ee(1:,0:)
      INTEGER(4), INTENT(in), OPTIONAL :: MSC_sl_me(1:,0:)
      INTEGER(4), INTENT(in), OPTIONAL :: MSC_sl_pu(1:,0:)

      INTEGER(4) :: i_w, i_bit

      DO i_w = 1, n_w, 1
         DO i_bit = 0, MIN(n_betas-64*(i_w-1),64) - 1, 1
            IF (PRESENT(MSC_sl_eb)) CLOSE(MSC_sl_eb(i_w,i_bit))
            IF (PRESENT(MSC_sl_mb)) CLOSE(MSC_sl_mb(i_w,i_bit))
            IF (PRESENT(MSC_sl_ee)) CLOSE(MSC_sl_ee(i_w,i_bit))
            IF (PRESENT(MSC_sl_me)) CLOSE(MSC_sl_me(i_w,i_bit))
            IF (PRESENT(MSC_sl_pu)) CLOSE(MSC_sl_pu(i_w,i_bit))
         END DO
      END DO
    END SUBROUTINE MSC_closeSlots

    SUBROUTINE MSC_saveIsingWords_th(IW2,n_w,l_x,l_z,MSC_sl_iw)
      INTEGER(8), INTENT(in) :: IW2(1:,1:,1:)
      INTEGER(4), INTENT(in) :: n_w
      INTEGER(4), INTENT(in) :: l_x, l_z
      INTEGER(4), INTENT(in) :: MSC_sl_iw(1:)

      INTEGER(4) :: i_w
      CHARACTER(2) :: si_w

      DO i_w = 1, n_w, 1
         WRITE(si_w,'(i0.2)') i_w
         OPEN(MSC_sl_iw(i_w),file="_iw_th_i_w"//si_w//".bin",access="stream",status="replace")
         WRITE(MSC_sl_iw(i_w)) IW2(i_w,1:l_x,1:l_z)
         CLOSE(MSC_sl_iw(i_w))
      END DO
    END SUBROUTINE MSC_saveIsingWords_th

    SUBROUTINE MSC_saveIsingWords_ob(IW2,n_w,l_x,l_z,MSC_sl_iw)
      INTEGER(8), INTENT(in) :: IW2(1:,1:,1:)
      INTEGER(4), INTENT(in) :: n_w
      INTEGER(4), INTENT(in) :: l_x, l_z
      INTEGER(4), INTENT(in) :: MSC_sl_iw(1:)

      INTEGER(4) :: i_w
      CHARACTER(2) :: si_w

      DO i_w = 1, n_w, 1
         WRITE(si_w,'(i0.2)') i_w
         OPEN(MSC_sl_iw(i_w),file="_iw_ob_i_w"//si_w//".bin",access="stream",status="replace")
         WRITE(MSC_sl_iw(i_w)) IW2(i_w,1:l_x,1:l_z)
         CLOSE(MSC_sl_iw(i_w))
      END DO
    END SUBROUTINE MSC_saveIsingWords_ob

    SUBROUTINE MSC_setDifferenceTable(d_eb,d_mb)
      INTEGER(4), INTENT(out) :: d_eb(0:,0:,0:,0:), d_mb(0:,0:)

      INTEGER(4) :: ib0, ib1, ib2, ic

      d_eb(0,0:1,0:1,0:1) = 0
      d_mb(0,0:1) = 0
      FORALL (ib0=0:1:1,ib1=0:1:1,ib2=0:1:1)
         d_eb(1,ib0,ib1,ib2) = - 4 * (ib0+2*ib1+4*ib2-2)
      END FORALL
      FORALL (ic=0:1:1)
         d_mb(1,ic) = 2 * (2*ic-1)
      END FORALL
    END SUBROUTINE MSC_setDifferenceTable
  END MODULE mod_proc
