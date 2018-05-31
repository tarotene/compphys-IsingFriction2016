PROGRAM main
  !$  USE omp_lib
  USE mod_proc
  USE mod_MSC

  IMPLICIT NONE

  ! debug vars
  INTEGER(4) :: value, l
  REAL(8) :: prob8, prob7, prob6

  ! STEP-00: Set Params except for Temperature
  WRITE(0,'(a)',advance='no') "Setting Params except for Temperature... "
  CALL MTC_readParams_2d_eq(l_x,l_z,l_t0,l_t1,id_IC,id_BC); n_st = l_x*l_z
  ALLOCATE(r_l(1:n_st),r_x(1:n_st),r_z(1:n_st))
  WRITE(0,'(a)') ": Done"

  ! STEP-00: Set Array of Temperatures
  WRITE(0,'(a)',advance='no') "Setting Array of Temperatures... "
  CALL countLines(10,"list_beta.dat",n_betas); n_w = n_betas/64; ALLOCATE(MSC_beta(1:n_w,0:63))
  CALL MTC_readBetas(10,"list_beta.dat",MSC_beta(1:n_w,0:63))
  WRITE(0,'(a)') ": Done"

  ! STEP-00-DEBUG
  ! DO i_w = 1, n_w, 1
  !   DO i_bit = 0, 63, 1
  !     WRITE(0,'(a,i3,a,i2,a,f0.4)') "[DEBUG] MSC_beta(i_w=",i_w,",i_bit=",i_bit,")=",MSC_beta(i_w,i_bit)
  !   END DO
  ! END DO
  ! STOP

  ! STEP-00: Make Labels of each Temperature.
  WRITE(0,'(a)',advance='no') "Making Labels of each Temperature... "
  ALLOCATE(decbeta(1:n_w,0:63),sbeta(1:n_w,0:63))
  decbeta(1:n_w,0:63) = MSC_beta(1:n_w,0:63)-DBLE(int(MSC_beta(1:n_w,0:63)))
  DO i_w = 1, n_w, 1
    DO i_bit = 0, 63, 1
      WRITE(sdecbeta,'(F6.4)') decbeta(i_w,i_bit)
      WRITE(sbeta(i_w,i_bit), '(A4,I2.2,A5)') "beta",int(MSC_beta(i_w,i_bit)),sdecbeta(2:6)
    END DO
  END DO
  WRITE(0,'(a)') ": Done"

  ! STEP-00-DEBUG
  ! DO i_w = 1, n_w, 1
  !   DO i_bit = 0, 63, 1
  !     WRITE(0,'(a,i2,a,i2,a,a)') "[DEBUG] sbeta(i_w=",i_w,",i_bit=",i_bit,")=",sbeta(i_w,i_bit)
  !   END DO
  ! END DO
  ! STOP

  ! STEP-00: Set Array of Slots
  WRITE(0,'(a)',advance='no') "Setting Array of Slots... "
  ALLOCATE(MSC_sl_iw(1:n_w),MSC_sl_eb(1:n_w,0:63),MSC_sl_mb(1:n_w,0:63))
  DO i_w = 1, n_w, 1
    MSC_sl_iw(i_w) = 20+i_w
    DO i_bit = 0, 63, 1
      MSC_sl_eb(i_w,i_bit) = 20+n_w+i_bit+1+64*(i_w-1); MSC_sl_mb(i_w,i_bit) = 20+n_w+i_bit+1+64*(i_w-1)+n_w*64
      ! MSC_sl_ee(i_w,i_bit) = 20+n_w+i_bit+1+64*(i_w-1)+2*n_w*64; MSC_sl_me(i_w,i_bit) = 20+n_w+i_bit+1+64*(i_w-1)+3*n_w*64; MSC_sl_pu(i_w,i_bit) = 20+n_w+i_bit+1+64*(i_w-1)+4*n_w*64
    END DO
  END DO
  WRITE(0,'(a)') ": Done"
  
  ! STEP-00-DEBUG
  ! DO i_w = 1, n_w, 1
  !   DO i_bit = 0, 63, 1
  !     WRITE(0,'(a,i2,a,i2,a,i4,a,i2,a,i2,a,i4)') "[DEBUG] MSC_sl_eb(i_w=",i_w,",i_bit=",i_bit,")=",MSC_sl_eb(i_w,i_bit),", MSC_sl_mb(i_w=",i_w,",i_bit=",i_bit,")=",MSC_sl_mb(i_w,i_bit)
  !   END DO
  ! END DO
  ! STOP

  ! STEP-00: Open All Slots
  WRITE(0,'(a)',advance='no') "Opening All Slots... "
  DO i_w = 1, n_w, 1
    DO i_bit = 0, MIN(n_betas-64*(i_w-1),64) - 1, 1
      ! OPEN(MSC_sl_iw,file="_iw_"//sbeta(i_w,i_bit)//".bin",access="stream",status="replace")
      OPEN(MSC_sl_eb(i_w,i_bit),file="_eb_"//sbeta(i_w,i_bit)//".bin",access="stream",status="replace")
      OPEN(MSC_sl_mb(i_w,i_bit),file="_mb_"//sbeta(i_w,i_bit)//".bin",access="stream",status="replace")
      ! OPEN(MSC_sl_ee(i_w,i_bit),file="_ee_"//sbeta(i_w,i_bit)//".bin",access="stream",status="replace")
      ! OPEN(MSC_sl_me(i_w,i_bit),file="_me_"//sbeta(i_w,i_bit)//".bin",access="stream",status="replace")
      ! OPEN(MSC_sl_pu(i_w,i_bit),file="_pu_"//sbeta(i_w,i_bit)//".bin",access="stream",status="replace")
    END DO
  END DO
  WRITE(0,'(a)') ": Done"
  
  ! STEP-00: Allocate Vars. and Obs.
  WRITE(0,'(a)',advance='no') "Allocating Vars. and Obs... "
  ALLOCATE(IW2(1:n_w,1:l_x,0:l_z+1),IW2_ini(1:n_w,1:l_x,0:l_z+1))
  ALLOCATE(MSC_eb(1:n_w,0:63),MSC_mb(1:n_w,0:63))
  ! ALLOCATE(MSC_ee(1:n_w,0:63),MSC_me(1:n_w,0:63),MSC_pu(1:n_w,0:63))
  WRITE(0,'(a)') ": Done"
  
  ! TODO: MPI or Other Script Lang. Implementation of Running Many Samples

  ! STEP-00: Initialize Ising Words.
  WRITE(0,'(a)',advance='no') "Initializing Ising Words..."
  DO i_w = 1, n_w, 1
    CALL MSC_initIS_2d(IW2_ini(i_w,1:l_x,0:l_z+1))
  END DO
  WRITE(0,'(a)') ": Done"

  ! STEP-00-DEBUG
  ! DO i_w = 1, n_w, 1
  !   DO i_bit = 0, 63, 1
  !     DO z = 0, l_z + 1, 1
  !       DO x = 1, l_x, 1
  !         WRITE(0,"(a,i2,a,i2,a,i4,a,i4,a,i2)") "[DEBUG] IS2_ini(i_w=",i_w,",i_bit=",i_bit,",x=",x,",z=",z,")=",2*IBITS(IW2_ini(i_w,x,z),i_bit,1)-1
  !       END DO
  !     END DO
  !   END DO
  ! END DO
  ! STOP

  ! TODO: 乱数テーブルの実装・テスト（WRITE文によるチェック）

  ! STEP-00: Create the Random Table
  max_l = ISHFT(1,8); ALLOCATE(r_a(1:n_w,0:3,0:max_l-1))
  WRITE(0, '(a)',advance='no') "Creating the Random Table... "
  DO i_w = 1, n_w, 1
    CALL MTC_setRandomTable(MSC_beta(i_w,0:63),r_a(i_w,0:3,0:max_l-1))
  END DO
  WRITE(0,'(a)') ": Done"

  ! STEP-00: DEBUG
  ! DO l = 0, max_l-1, 1
  !   WRITE(0,'(a,i20,a,i1,i1,i1,i1,a)') "[DEBUG] r_a(i_w=1,l=",l,") = [",IBITS(r_a(1,3,l),0,1),IBITS(r_a(1,2,l),0,1),IBITS(r_a(1,1,l),0,1),IBITS(r_a(1,0,l),0,1),"]"
  ! END DO
  ! STOP

  ! STEP-00: Accuracy Check and Debug of Random Table
  ! DO i_w = 1, n_w, 1
  !   DO i_bit = 0, 63, 1
  !     prob8 = 0.0d0; prob7 = 0.0d0; prob6 = 0.0d0
  !     DO l = 0, max_l-1, 1
  !       value = IBITS(r_a(i_w,0,l),i_bit,1) + 2 * IBITS(r_a(i_w,1,l),i_bit,1) + 4 * IBITS(r_a(i_w,2,l),i_bit,1) + 8 * IBITS(r_a(i_w,3,l),i_bit,1)
  !       if ( value == 8 ) prob8 = prob8 + 1.0d0
  !       if ( value == 7 ) prob7 = prob7 + 1.0d0
  !       if ( value == 6 ) prob6 = prob6 + 1.0d0
  !     END DO
  !     prob8 = prob8 / DBLE(max_l); prob7 = prob7 / DBLE(max_l); prob6 = prob6 / DBLE(max_l)
  !     WRITE(0,'(a,i2,a,i2,a,f0.4,a,i2,a,i2,a,f0.4,a,i2,a,i2,a,f0.4)') "[DEBUG] beta(i_w=",i_w,",i_bit=",i_bit,") = ",LOG(prob8)/(-4.0d0),", beta(i_w=",i_w,",i_bit=",i_bit,") = ",LOG(prob8+prob7)/(-2.0d0),", unity(i_w=",i_w,",i_bit=",i_bit,") = ",prob8+prob7+prob6
  !   END DO
  ! END DO
  ! STOP

  ! STEP-00: Initialize RNGs
  WRITE(0,'(a)',advance='no') "Initializing RNGs... "
  CALL constRand_SFMT19937(100+0,str_l)
  CALL constRand_SFMT19937(100+1,str_x)
  CALL constRand_SFMT19937(100+3,str_z)
  WRITE(0,'(a)') ": Done"

  ! STEP-01: Thermalize
  WRITE(0, '(a)',advance='no') "Thermalizing..."
  !$omp parallel do schedule(static,1) default(none) &
  !$omp shared(n_betas,n_w,l_x,l_z,l_t0,n_st,max_l,IW2) &
  !$omp shared(str_l,str_x,str_z,r_l,r_a,r_x,r_z)
  DO i_w = 1, n_w, 1
    DO t = 1, l_t0, 1
        CALL updateIRand_Uniform(str_l,n_st,0,max_l,r_l(1:n_st))
        CALL updateIRand_Uniform(str_x,n_st,1,l_x+1,r_x(1:n_st))
        CALL updateIRand_Uniform(str_z,n_st,1,l_z+1,r_z(1:n_st))
        CALL MSC_mSSFs_2d_THERMALIZE(r_x(1:n_st),r_z(1:n_st),r_l(1:n_st),r_a(i_w,0:3,0:max_l-1),IW2(i_w,1:l_x,0:l_z+1))
        IF ( MOD(t*20,l_t0) == 0 ) WRITE(0, '(a)',advance='no') "."
    END DO
  END DO
  !$omp end parallel do
  WRITE(0,'(a)') ": Done"

  ! STEP-01: Save Ising Words
  WRITE(0,'(a)',advance='no') "Saving Ising Words (at Thermalized)... "
  DO i_w = 1, n_w, 1
    WRITE(si_w,'(i0.2)') i_w
    OPEN(MSC_sl_iw(i_w),file="_iw_th_i_w"//si_w//".bin",access="stream",status="replace")
    WRITE(MSC_sl_iw(i_w)) IW2(i_w,1:l_x,1:l_z)
    CLOSE(MSC_sl_iw(i_w))
  END DO
  CALL saveRand(str_l,"_str_th_l.bin"); CALL saveRand(str_x,"_str_th_x.bin"); CALL saveRand(str_z,"_str_th_z.bin")
  WRITE(0,'(a)') ": Done"

  ! STEP-02: Initialize All Obs.
  DO i_w = 1, n_w, 1
    CALL MSC_calcEB_2d(IW2(i_w,1:l_x,0:l_z+1),MSC_eb(i_w,0:63))
    CALL MSC_calcMB_2d(IW2(i_w,1:l_x,1:l_z),MSC_mb(i_w,0:63))
  END DO

  ! STEP-02: DEBUG
  ! DO i_w = 1, n_w, 1
  !   DO i_bit = 0, 63, 1
  !     WRITE(0,'(a,i2,a,i2,a,i5)') "[DEBUG] MSC_eb(i_w=",i_w,",i_bit=",i_bit,") = ",MSC_eb(i_w,i_bit)
  !   END DO
  ! END DO
  ! DO i_w = 1, n_w, 1
  !   DO i_bit = 0, 63, 1
  !     WRITE(0,'(a,i2,a,i2,a,i5)') "[DEBUG] MSC_mb(i_w=",i_w,",i_bit=",i_bit,") = ",MSC_mb(i_w,i_bit)
  !   END DO
  ! END DO
  ! STOP

  ! TODO: 上記のテストが全て終わってから下記のコメントアウトの解除

  ! STEP-02: Observe
  WRITE(0, '(a)',advance='no') "Observing... "
  !$omp parallel do schedule(static,1) default(none) &
  !$omp shared(n_betas,n_w,l_x,l_z,l_t1,n_st,max_l,IW2,MSC_eb,MSC_mb) &
  !$omp shared(str_l,str_x,str_z,r_l,r_a,r_x,r_z,MSC_sl_eb,MSC_sl_mb)
  DO i_w = 1, n_w, 1
    DO t = 1, l_t1, 1
        CALL updateIRand_Uniform(str_l,n_st,0,max_l,r_l(1:n_st))
        CALL updateIRand_Uniform(str_x,n_st,1,l_x+1,r_x(1:n_st))
        CALL updateIRand_Uniform(str_z,n_st,1,l_z+1,r_z(1:n_st))
        CALL MSC_mSSFs_2d_OBSERVE(r_x(1:n_st),r_z(1:n_st),r_l(1:n_st),r_a(i_w,0:3,0:max_l-1),IW2(i_w,1:l_x,0:l_z+1),MSC_eb(i_w,0:63),MSC_mb(i_w,0:63))

        DO i_bit = 0, MIN(n_betas-64*(i_w-1),64) - 1, 1
            WRITE(MSC_sl_eb(i_w,i_bit)) MSC_eb(i_w,i_bit)
            WRITE(MSC_sl_mb(i_w,i_bit)) MSC_mb(i_w,i_bit)
        END DO

        ! CALL MSC_calcEE_2d(IW2(i_w,1:l_x,0:l_z+1), MSC_ee(i_w,0:63))
        ! CALL MSC_calcME_2d(IW2(i_w,1:l_x,0:l_z+1), MSC_me(i_w,0:63))
        ! CALL MSC_calcPU_2d(IW2(i_w,1:l_x,0:l_z+1), MSC_pu(i_w,0:63))
        ! DO i_beta = 1+(i_w-1)*64, MIN(n_betas,i_w*64), 1
        !     WRITE(MSC_sl_eb(MOD(64,i_beta-1)+1)) MSC_eb(i_w,MOD(64,i_beta-1)+1)
        !     WRITE(MSC_sl_mb(MOD(64,i_beta-1)+1)) MSC_mb(i_w,MOD(64,i_beta-1)+1)
        !     WRITE(MSC_sl_pu(MOD(64,i_beta-1)+1)) MSC_pu(i_w,MOD(64,i_beta-1)+1)
        ! END DO
        IF ( MOD(t*20,l_t1) == 0 ) WRITE(0, '(a)',advance='no') "."
    END DO
  END DO
  !$omp end parallel do
  WRITE(0,'(a)') ": Done"

  ! STEP-XX: Save Ising Words and Random Streams
  WRITE(0,'(a)',advance='no') "Saving Ising Words (at Stationarized)... "
  DO i_w = 1, n_w, 1
    WRITE(si_w,'(i0.2)') i_w
    OPEN(MSC_sl_iw(i_w),file="_iw_ob_i_w"//si_w//".bin",access="stream",status="replace")
    WRITE(MSC_sl_iw(i_w)) IW2(i_w,1:l_x,1:l_z)
    CLOSE(MSC_sl_iw(i_w))
  END DO
  CALL saveRand(str_l,"_str_ob_l.bin"); CALL saveRand(str_x,"_str_ob_x.bin"); CALL saveRand(str_z,"_str_ob_z.bin")
  CALL destRand(str_l); CALL destRand(str_x); CALL destRand(str_z)
  WRITE(0,'(a)') ": Done"

  ! STEP-XX: Close All Slots
  WRITE(0,'(a)',advance='no') "Closing All Slots... "
  DO i_w = 1, n_w, 1
    DO i_bit = 0, MIN(n_betas-64*(i_w-1),64) - 1, 1
      ! CLOSE(MSC_sl_iw(i_w))
      CLOSE(MSC_sl_eb(i_w,i_bit))
      CLOSE(MSC_sl_mb(i_w,i_bit))
      ! CLOSE(MSC_sl_ee(i_w,i_bit))
      ! CLOSE(MSC_sl_me(i_w,i_bit))
      ! CLOSE(MSC_sl_pu(i_w,i_bit))
    END DO
  END DO
  WRITE(0,'(a)') ": Done"
END PROGRAM main
