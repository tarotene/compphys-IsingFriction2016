MODULE mod_MSC
  USE mod_global

  IMPLICIT NONE
CONTAINS
    SUBROUTINE MSC_calcEB_2d(IS2_64,arr_eb)
        INTEGER(8), INTENT(in) :: IS2_64(1:,1:)
        INTEGER(4), INTENT(out) :: arr_eb(1:)

        INTEGER(8) :: IS2_64_masked2(1:2,1:l_x,1:l_z)

        IS2_64_masked2(1,1:l_x,1:l_z) = IAND(IS2_64(1:l_x,1:l_z),bitmask2(1))
        IS2_64_masked2(2,1:l_x,1:l_z) = IAND(IS2_64(1:l_x,1:l_z),bitmask2(2))

        el_half(1:2,1:l_x,1:l_z) = IEOR(IS2_64_masked2(1:2,1:l_x,1:l_z),IS2_64_masked2(1:2,1:l_x,2:l_z+1))+IEOR(IS2_64_masked2(1:2,1:l_x,1:l_z),IS2_64_masked2(1:2,2:MOD(x,l_x)+1,1:l_z))
        el_quart(1:2,1:l_x) = IEOR(IS2_64_masked2(1:2,1:l_x,0),IS2_64_masked2(1:2,1:l_x,1))

        arr_eb(1:64) = 0
        DO i = 1, 63, 2
            arr_eb(i)   = SUM(IAND(el_half(1,1:l_x,1:l_z),3)
            arr_eb(i)   = arr_eb(i) + SUM(IAND(el_quart(1,1:l_x),3)
            arr_eb(i+1) = SUM(IAND(el_half(2,1:l_x,1:l_z),3)
            arr_eb(i+1) = arr_eb(i+1) + SUM(IAND(el_quart(2,1:l_x),3)
            el_half(1:2,1:l_x,1:l_z) = ISHFT(el_half(1:2,1:l_x,1:l_z),-2)
            el_quart(1:2,1:l_x,1:l_z) = ISHFT(el_quart(1:2,1:l_x,1:l_z),-2)
        END DO
    END SUBROUTINE MSC_calcEB_2d

    SUBROUTINE MSC_calcMB_2d(IS2_64,arr_mb)
        INTEGER(8), INTENT(inout) :: IS2_64(1:,1:)
        INTEGER(4), INTENT(out) :: arr_mb(1:)

        ! arr_mb(1:64) = 0
        DO i = 1, 64, 1
            arr_mb(i) = SUM(IAND(IS2_64(1:l_x,1:l_z),1))
            IS2_64(1:l_x,1:l_z) = ISHFTC(IS2_64(1:l_x,1:l_z),-1)
        END DO
    END SUBROUTINE MSC_calcMB_2d

    SUBROUTINE MSC_initIS_2d(IS2_64)
        INTEGER(8), INTENT(inout) :: IS2_64(0:,0:)

        INTEGER(4) :: err, z
        TYPE(VSL_STREAM_STATE) :: str_sp

        IS2_64(0, 0:l_z + 1) = 0
        IS2_64(l_x + 1, 0:l_z + 1) = 0

        SELECT CASE (id_BC)
        CASE (1) !BC: anti-parallel
        IS2_64(1:l_x, 0) = -1_8
        IS2_64(1:l_x, l_z + 1) = 0
        CASE (2) !BC: parallel
        IS2_64(1:l_x, 0) = -1_8
        IS2_64(1:l_x, l_z + 1) = -1_8

        SELECT CASE (id_IC)
        CASE (1) !IC: domain-wall state
        IS2_64(1:l_x, 1:l_z/2) = -1_8
        IS2_64(1:l_x, l_z/2 + 1:l_z) = 0
        CASE (2) !IC: magnetized state
        IS2_64(1:l_x, 1:l_z) = -1_8
        END SELECT
    END SUBROUTINE MSC_initIS_2d

    SUBROUTINE MTC_setMSC_P(betas,MSC_P)
  !For Metropolis
        REAL(8), INTENT(in) :: betas(1:)
        INTEGER(8), INTENT(out) :: MSC_P(1:,0:)

        INTEGER(4) :: i_bit, i
        REAL(8) :: boltz4(1:4), boltz2(1:4)
        INTEGER(4) :: lboltz4(1:4), lboltz2(1:4)
        
        DO i_bit = 1, 61, 4
          boltz4(1:4) = exp(-4*betas(i_bit:i_bit+3))
          boltz2(1:4) = exp(-2*betas(i_bit:i_bit+3))
          lboltz4(1:4) = NINT(boltz4(1:4)*DBLE(max_l))
          lboltz2(1:4) = NINT(boltz2(1:4)*DBLE(max_l))

          DO i = 1, 4, 1
            MSC_P(i,0:lboltz4(i)-1)          = 8
            MSC_P(i,lboltz4(i):lboltz2(i)-1) = 7
            MSC_P(i,lboltz2(i):max_l-1)      = 6
          END DO

          MSC_P(1:4,0:max_l-1) = ISHFT(MSC_P(1:4,0:max_l-1),16)
        END DO
  END SUBROUTINE MTC_setMSC_P

  SUBROUTINE MSC_setOrient_2d(IS64,x,z,e,w,s,n)
  ! TODO: For a boundary x-open, z-periodic
    INTEGER(8), INTENT(in), TARGET :: IS64(0:, 0:)
    INTEGER(4), INTENT(in) :: x, z
    INTEGER(8), POINTER :: e, w, s, n
    e => IS64(MOD(x,l_x)+1,z)
    IF (x == 1) THEN
       w => IS64(l_x,z)
    ELSE
       w => IS64(x-1,z)
    END IF
    s => IS64(x,z-1)
    n => IS64(x,z+1)
  END SUBROUTINE MSC_setOrient_2d

  SUBROUTINE MSC_SSF_2d_THERMALIZE(x,z,p,IS2_64)
    INTEGER(4), INTENT(in) :: x, z
    INTEGER(8), INTENT(in) :: p(1:4)
    INTEGER(8), INTENT(inout) :: IS2_64(0:,0:)

    INTEGER(4) :: i
    INTEGER(8), POINTER :: e, w, s, n
    INTEGER(8) :: le(1:4), ac(1:4), ae(1:4), aw(1:4), as(1:4), an(1:4)

    CALL MSC_setOrient_2d(IS2_64(0:l_x+1,0:l_z+1),x,z,e,w,s,n)
    ac(1:4) = IAND(IS2_64(x,z),bitmask4(1:4))
    ae(1:4) = IAND(e,bitmask4(1:4)); aw(1:4) = IAND(w,bitmask4(1:4)); as(1:4) = IAND(s,bitmask4(1:4)); an(1:4) = IAND(n,bitmask4(1:4))
    ! a*(1) = 000s000s, a*(2) = 00s000s0, a*(3) = 0s000s00, a*(4) = s000s000
    DO i = 1, 4, 1
      ae(i) = ISHFT(ae(i),1-i); aw(i) = ISHFT(aw(i),1-i); as(i) = ISHFT(as(i),1-i); an(i) = ISHFT(an(i),1-i)
    END DO
    ! a*(1:4) = 000s000s
    le(1:4) = IEOR(ac(1:4),ae(1:4))+IEOR(ac(1:4),aw(1:4))+IEOR(ac(1:4),as(1:4))+IEOR(ac(1:4),an(1:4))
    ! le(1:4) = 0eee0eee
    DO i = 1, 4, 1
      a(i) = ISHFT(IAND(le(i)+p(i),bitmask4(i)),i-4)
    END DO
    ! a(1) = 000a000a, a(2) = 00a000a0, a(3) = 0a000a00, a(4) = a000a000
    IS2_64(x,z) = IOR(IS2_64(x,z),SUM(a(1:4)))
  END SUBROUTINE MSC_SSF_2d_THERMALIZE

  SUBROUTINE MSC_SSF_2d_OBSERVE(x,z,p,IS2_64,arr_eb,arr_mb)
    INTEGER(4), INTENT(in) :: x, z
    INTEGER(8), INTENT(in) :: p(1:4)
    INTEGER(8), INTENT(inout) :: IS2_64(0:,0:)
    INTEGER(4), INTENT(inout) :: arr_eb(1:), arr_mb(1:)

    INTEGER(4) :: i
    INTEGER(8), POINTER :: e, w, s, n
    INTEGER(8) :: le(1:4), ac(1:4), ae(1:4), aw(1:4), as(1:4), an(1:4)

    CALL MSC_setOrient_2d(IS2_64(0:l_x+1,0:l_z+1),x,z,e,w,s,n)
    ac(1:4) = IAND(IS2_64(x,z),bitmask4(1:4))
    ae(1:4) = IAND(e,bitmask4(1:4)); aw(1:4) = IAND(w,bitmask4(1:4)); as(1:4) = IAND(s,bitmask4(1:4)); an(1:4) = IAND(n,bitmask4(1:4))
    ! a*(1) = 000s000s, a*(2) = 00s000s0, a*(3) = 0s000s00, a*(4) = s000s000
    DO i = 1, 4, 1
      ae(i) = ISHFT(ae(i),1-i); aw(i) = ISHFT(aw(i),1-i); as(i) = ISHFT(as(i),1-i); an(i) = ISHFT(an(i),1-i)
    END DO
    ! a*(1:4) = 000s000s
    le(1:4) = IEOR(ac(1:4),ae(1:4))+IEOR(ac(1:4),aw(1:4))+IEOR(ac(1:4),as(1:4))+IEOR(ac(1:4),an(1:4))
    ! le(1:4) = 0eee0eee
    DO i = 1, 4, 1
      a(i) = ISHFT(IAND(le(i)+p(i),bitmask4(i)),i-4)
    END DO
    ! a(1) = 000a000a, a(2) = 00a000a0, a(3) = 0a000a00, a(4) = a000a000
    IS2_64(x,z) = IOR(IS2_64(x,z),SUM(a(1:4)))
    DO i = 1, 4, 1
      ale(i) = IAND(ISHFT(a(i),1-i)*15,le(i))
    END DO
    ! ale(1:4) = 0eee0eee
    DO i = 1, 61, 4
      arr_eb(i:i+3) = arr_eb(i:i+3) - IAND(ale(1:4),15)
      ale(1:4) = ISHFT(ale(1:4),-4)
    END DO
    alm = IAND(IS2_64(x,z),SUM(a(1:4)))
    ! alm = mmmmmmmm
    DO i = 1, 64, 1
      arr_mb(i) = arr_mb(i) + 2 * IAND(alm,1) - 1
      alm = ISHFT(alm,-1)
    END DO
  END SUBROUTINE MSC_SSF_2d_OBSERVE

  SUBROUTINE MSC_mSSFs_2d_THERMALIZE(r_x,r_z,r_l,IS2_64)
    INTEGER(4), INTENT(in) :: r_x(1:), r_z(1:), r_l(1:)
    INTEGER(8), INTENT(inout) :: IS2_64(0:,0:)

    INTEGER(4) :: i_st

    DO i_st = 1, n_st, 1
       CALL MSC_SSF_2d_THERMALIZE(r_x(i_st),r_z(i_st),MSC_P(1:,r_l(i_st)),IS2_64(0:,0:))
    END DO
  END SUBROUTINE MSC_mSSFs_2d_THERMALIZE

END MODULE mod_MSC