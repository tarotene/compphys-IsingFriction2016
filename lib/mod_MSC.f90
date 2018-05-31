MODULE mod_MSC
  USE mod_global

  IMPLICIT NONE

  ! MSC I/O
  INTEGER(4), ALLOCATABLE, SAVE :: MSC_sl_iw(:), MSC_sl_eb(:,:), MSC_sl_mb(:,:), MSC_sl_ee(:,:), MSC_sl_me(:,:)
  CHARACTER(len=2) :: si_w

  ! MSC spin variables
  INTEGER(8), ALLOCATABLE, SAVE :: IW2(:, :, :), IW3(:, :, :, :)
	INTEGER(8), ALLOCATABLE :: IW2_ini(:, :, :), IW3_ini(:, :, :, :)

  ! MSC physical quantities
  INTEGER(4), ALLOCATABLE :: MSC_eb(:, :), MSC_mb(:, :), MSC_ee(:, :), MSC_me(:, :), MSC_pu(:, :)

  ! MSC parameters
  INTEGER(4), SAVE :: n_betas, n_w, max_l
  REAL(8), ALLOCATABLE, SAVE :: MSC_beta(:,:), decbeta(:,:)
  CHARACTER(len=6) :: sdecbeta
  CHARACTER(len=11), ALLOCATABLE :: sbeta(:,:)
  CHARACTER(len=7), ALLOCATABLE :: sdirword(:)
  INTEGER(8), ALLOCATABLE, SAVE :: r_a(:,:,:)
  INTEGER(8), PARAMETER :: bitmask4(1:4) &	! Binary: [00010001..., 00100010..., 01000100..., 10001000...]
    = [1229782938247303441_8, 2459565876494606882_8, 4919131752989213764_8, -8608480567731124088_8]
  INTEGER(8), PARAMETER :: bitmask2(1:2) &	! Binary: [0101..., 1010...]
    = [6148914691236517205_8, -6148914691236517206_8]
  
CONTAINS
    SUBROUTINE MTC_readParams_2d(l_x,l_z,vel,l_t0,l_t1,id_IC,id_BC)
      INTEGER(4), INTENT(out) :: l_x, l_z
      INTEGER(4), INTENT(out) :: vel, l_t0, l_t1, id_IC, id_BC

      READ (*, *) l_x, l_z, vel, l_t0, l_t1, id_IC, id_BC
    END SUBROUTINE MTC_readParams_2d

    SUBROUTINE MTC_readParams_3d(l_x,l_y,l_z,vel,l_t0,l_t1,id_IC,id_BC)
      INTEGER(4), INTENT(out) :: l_x, l_y, l_z
      INTEGER(4), INTENT(out) :: vel, l_t0, l_t1, id_IC, id_BC

      READ (*, *) l_x, l_y, l_z, vel, l_t0, l_t1, id_IC, id_BC
    END SUBROUTINE MTC_readParams_3d

    SUBROUTINE MTC_readParams_2d_eq(l_x,l_z,l_t0,l_t1,id_IC,id_BC)
      INTEGER(4), INTENT(out) :: l_x, l_z
      INTEGER(4), INTENT(out) :: l_t0, l_t1, id_IC, id_BC

      READ (*, *) l_x, l_z, l_t0, l_t1, id_IC, id_BC
    END SUBROUTINE MTC_readParams_2d_eq

    SUBROUTINE MTC_readParams_3d_eq(l_x,l_y,l_z,l_t0,l_t1,id_IC,id_BC)
      INTEGER(4), INTENT(out) :: l_x, l_y, l_z
      INTEGER(4), INTENT(out) :: l_t0, l_t1, id_IC, id_BC

      READ (*, *) l_x, l_y, l_z, l_t0, l_t1, id_IC, id_BC
    END SUBROUTINE MTC_readParams_3d_eq

    SUBROUTINE MTC_readBetas(slot,filename,MSC_beta)
        INTEGER(4), INTENT(in) :: slot
        CHARACTER(len=13), INTENT(in) :: filename
        REAL(8), INTENT(out) :: MSC_beta(1:,0:)

        OPEN(slot,file=filename,status="old")
        DO i_w = 1, n_w, 1
          DO i_bit = 0, MIN(n_betas-64*(i_w-1),64) - 1, 1
            READ(slot, *) MSC_beta(i_w,i_bit)
          END DO
        END DO
        CLOSE(slot)
        MSC_beta(n_w,n_betas-64*(n_w-1):63) = MSC_beta(n_w,n_betas-64*(n_w-1)-1)
    END SUBROUTINE MTC_readBetas

    SUBROUTINE MSC_calcEB_2d(IW2,MSC_eb)
        INTEGER(8), INTENT(in) :: IW2(1:,0:)
        INTEGER(4), INTENT(out) :: MSC_eb(0:)

        INTEGER(8) :: el_half(0:1,1:l_x,1:l_z), el_quar(1:l_x) ! TODO: 作業用配列にする

        DO z = 1, l_z, 1
          DO x = 1, l_x, 1
            el_half(0,x,z) = IEOR(IEOR(IW2(x,z-1),IW2(x,z)),IEOR(IW2(x,z),IW2(MOD(x,l_x)+1,z)))
            el_half(1,x,z) = IAND(IEOR(IW2(x,z-1),IW2(x,z)),IEOR(IW2(x,z),IW2(MOD(x,l_x)+1,z)))
          END DO
        END DO
        el_quar(1:l_x) = IEOR(IW2(1:l_x,l_z),IW2(1:l_x,l_z+1))
        DO i_bit = 0, 63, 1
            MSC_eb(i_bit) = SUM(IAND(el_half(0,1:l_x,1:l_z),1))+2*SUM(IAND(el_half(1,1:l_x,1:l_z),1))+SUM(IAND(el_quar(1:l_x),1))
            el_half(0:1,1:l_x,1:l_z) = ISHFTC(el_half(0:1,1:l_x,1:l_z),-1); el_quar(1:l_x) = ISHFTC(el_quar(1:l_x),-1)
        END DO
        MSC_eb(0:63) = 2*MSC_eb(0:63) - l_x*(2*l_z + 1)
    END SUBROUTINE MSC_calcEB_2d

    SUBROUTINE MSC_calcMB_2d(IW2,MSC_mb)
        INTEGER(8), INTENT(inout) :: IW2(1:,1:)
        INTEGER(4), INTENT(out) :: MSC_mb(0:)

        DO i_bit = 0, 63, 1
            MSC_mb(i_bit) = 2*SUM(IAND(IW2(1:l_x,1:l_z),1)) - l_x*l_z
            IW2(1:l_x,1:l_z) = ISHFTC(IW2(1:l_x,1:l_z),-1)
        END DO
    END SUBROUTINE MSC_calcMB_2d

    SUBROUTINE MSC_initIS_2d(IW2)
        INTEGER(8), INTENT(inout) :: IW2(1:,0:)

        INTEGER(4) :: err, z
        TYPE(VSL_STREAM_STATE) :: str_sp

        SELECT CASE (id_BC)
        CASE (1) !BC: anti-parallel
        IW2(1:l_x, 0) = -1_8
        IW2(1:l_x, l_z + 1) = 0
        CASE (2) !BC: parallel
        IW2(1:l_x, 0) = -1_8
        IW2(1:l_x, l_z + 1) = -1_8
        END SELECT

        SELECT CASE (id_IC)
        CASE (1) !IC: domain-wall state
        IW2(1:l_x, 1:l_z/2) = -1_8
        IW2(1:l_x, l_z/2 + 1:l_z) = 0
        CASE (2) !IC: magnetized state
        IW2(1:l_x, 1:l_z) = -1_8
        END SELECT
    END SUBROUTINE MSC_initIS_2d

    SUBROUTINE MTC_setRandomTable(MSC_beta,r_a)
        !For Metropolis
        REAL(8), INTENT(in) :: MSC_beta(0:)
        INTEGER(8), INTENT(out) :: r_a(0:,0:)

        INTEGER(4) :: nboltz4, nboltz2

        r_a(0:3,0:max_l-1) = 0
        DO i_bit = 0, 63, 1
          nboltz4 = INT(DBLE(max_l)*exp(-4.0d0*MSC_beta(i_bit)))
          nboltz2 = INT(DBLE(max_l)*exp(-2.0d0*MSC_beta(i_bit)))

          r_a(0,nboltz4+1:nboltz2) = r_a(0,nboltz4+1:nboltz2) + 1
          r_a(1,nboltz4+1:nboltz2) = r_a(1,nboltz4+1:nboltz2) + 1
          r_a(1,nboltz2+1:max_l-1) = r_a(1,nboltz2+1:max_l-1) + 1
          r_a(2,nboltz4+1:nboltz2) = r_a(2,nboltz4+1:nboltz2) + 1
          r_a(2,nboltz2+1:max_l-1) = r_a(2,nboltz2+1:max_l-1) + 1
          r_a(3,0        :nboltz4) = r_a(3,0        :nboltz4) + 1

          r_a(0:3,0:max_l-1) = ISHFTC(r_a(0:3,0:max_l-1),-1)
        END DO
  END SUBROUTINE MTC_setRandomTable

  SUBROUTINE MSC_setNeighbour_2d(IW2,x,z,e,w,s,n)
  ! TODO: For a boundary x-open, z-periodic
    INTEGER(8), INTENT(in), TARGET :: IW2(1:, 0:)
    INTEGER(4), INTENT(in) :: x, z

    INTEGER(8), POINTER, INTENT(out) :: e, w, s, n

    e => IW2(MOD(x,l_x)+1,z)
    w => IW2(l_x-MOD(l_x-x+1,l_x),z)
    s => IW2(x,z-1)
    n => IW2(x,z+1)
  END SUBROUTINE MSC_setNeighbour_2d

  SUBROUTINE MSC_SSF_2d(c,e,w,s,n,r,IW2,a,b,b1,b2)
    INTEGER(8), INTENT(inout) :: c
    INTEGER(8), INTENT(in) :: e, w, s, n
    INTEGER(8), INTENT(in) :: r(0:)
    INTEGER(8), INTENT(inout) :: IW2(1:,0:)
    ! WORKING VARS
    INTEGER(8), INTENT(inout) :: a, b(0:), b1(0:), b2(0:)
    ! LOCAL VARS
    INTEGER(8) :: de, dw, ds, dn

    de = IEOR(c,e); dw = IEOR(c,w); ds = IEOR(c,s); dn = IEOR(c,n)
    b1(0) = IEOR(de,dw); b1(1) = IAND(de,dw)
    b2(0) = IEOR(ds,dn); b2(1) = IAND(ds,dn)

    b(0) = IEOR(b1(0),b2(0))
    b(1) = IEOR(IEOR(b1(1),b2(1)),IAND(b1(0),b2(0)))
    b(2) = IOR(IAND(IEOR(b1(1),b2(1)),IAND(b1(0),b2(0))),IAND(b1(1),b2(1)))
    ! a:=乱数と局所エネルギーの和の3桁目（アクセプト時に初めて1を返す）
    a = IEOR(r(3),IOR(IAND(IEOR(b(2),r(2)),IOR(IAND(IEOR(b(1),r(1)),IAND(b(0),r(0))),IAND(b(1),r(1)))),IAND(b(2),r(2))))
    c = IEOR(c,a)
  END SUBROUTINE MSC_SSF_2d

  SUBROUTINE MSC_mSSFs_2d_THERMALIZE(r_x,r_z,r_l,r_a,IW2)
    INTEGER(4), INTENT(in) :: r_x(1:), r_z(1:), r_l(1:)
    INTEGER(8), INTENT(in) :: r_a(0:,0:)

    INTEGER(8), TARGET, INTENT(inout) :: IW2(1:,0:)
    INTEGER(8), POINTER :: c, e, w, s, n

    INTEGER(8) :: a, b(0:2), b1(0:1), b2(0:1)

    DO i_st = 1, n_st, 1
      ! WRITE(0, '(a, i5, a, i5, a, i4, a, i4)') "[DEBUG] r_x(",i_st,"), r_z(",i_st,") = ",r_x(i_st),", ",r_z(i_st)
      c => IW2(r_x(i_st),r_z(i_st))
      CALL MSC_setNeighbour_2d(IW2(1:l_x,0:l_z+1),r_x(i_st),r_z(i_st),e,w,s,n)
      CALL MSC_SSF_2d(c,e,w,s,n,r_a(0:,r_l(i_st)),IW2(1:,0:),a,b(0:),b1(0:),b2(0:))
    END DO
  END SUBROUTINE MSC_mSSFs_2d_THERMALIZE

  SUBROUTINE MSC_mSSFs_2d_OBSERVE(r_x,r_z,r_l,r_a,IW2,MSC_eb,MSC_mb)
    INTEGER(4), INTENT(in) :: r_x(1:), r_z(1:), r_l(1:)
    INTEGER(8), INTENT(in) :: r_a(0:,0:)
    INTEGER(4), INTENT(inout) :: MSC_eb(0:), MSC_mb(0:)
    
    INTEGER(8), TARGET, INTENT(inout) :: IW2(1:,0:)
    INTEGER(8), POINTER :: c, e, w, s, n

    INTEGER(8) :: a, b(0:2), b1(0:1), b2(0:1)

    DO i_st = 1, n_st, 1
      c => IW2(r_x(i_st),r_z(i_st))
      CALL MSC_setNeighbour_2d(IW2(1:l_x,0:l_z+1),r_x(i_st),r_z(i_st),e,w,s,n)
      CALL MSC_SSF_2d(c,e,w,s,n,r_a(0:,r_l(i_st)),IW2(1:,0:),a,b(0:),b1(0:),b2(0:))

      b(0:2) = IAND(b(0:2),a)
      DO i_bit = 0, 63, 1
        MSC_eb(i_bit) = MSC_eb(i_bit) - 2 * (IAND(b(0),1) + 2*IAND(b(1),1) + 4*IAND(b(2),1) - 2); b(0:2) = ISHFT(b(0:2),-1)
        MSC_mb(i_bit) = MSC_mb(i_bit) + IAND(a,1) * (4 * IAND(IW2(r_x(i_st),r_z(i_st)),1) - 2); a = ISHFT(a,-1); IW2(r_x(i_st),r_z(i_st)) = ISHFTC(IW2(r_x(i_st),r_z(i_st)),-1)
      END DO
    END DO
  END SUBROUTINE MSC_mSSFs_2d_OBSERVE

END MODULE mod_MSC