PROGRAM main
  !$  USE omp_lib
  USE mod_proc
  USE mod_MSC

  IMPLICIT NONE

  ! STEP-00: Set Params except for Temperature
  WRITE(0,'(a)',advance='no') "Setting Params except for Temperature..."
  CALL MTC_readParams_2d_eq(l_x,l_z,l_t0,l_t1,id_IC,id_BC); n_st = l_x*l_z; max_l = ISHFT(24,1)
  ALLOCATE(r_l(1:n_st),r_x(1:n_st),r_z(1:n_st),MSC_P(1:4,0:max_l-1))
  WRITE(0,'(a)') ": Done"

  ! STEP-00: Set Array of Temperatures
  WRITE(0,'(a)',advance='no') "Setting Array of Temperatures..."
  CALL countLines(10,"list_beta.dat",n_betas); n_w = n_betas/64+1; n_betas_c = 64*n_w
  ALLOCATE(IS2_64(1:n_w,0:l_x+1,0:l_z+1),IS2_ini64(1:n_w,0:l_x+1,0:l_z+1),betas(1:n_betas_c),arr_eb(1:n_w,1:64),arr_mb(1:n_w,1:64))
  CALL MTC_inputBetas(10,"list_beta.dat",betas(1:n_betas_c)); betas(n_betas + 1:n_betas_c) = betas(n_betas)
  WRITE(0,'(a)') ": Done"

  ! STEP-00: Make Directory, Temperature Deps.
  WRITE(0,'(a)',advance='no') "Making Directory, Temperature Deps..."
  ALLOCATE(decbeta(1:n_betas),sdirbeta(1:n_betas)); decbeta(1:n_betas) = betas(1:n_betas)-DBLE(int(betas(1:n_betas)))
  DO i_beta = 1, n_betas, 1
    WRITE(sdecbeta,'(F6.4)') decbeta(i_beta); WRITE(sdirbeta(i_beta), '(A4,I2.2,A5)') "beta",int(betas(i_beta)),sdecbeta(2:6)
    CALL system("mkdir "//sdirbeta(i_beta))
  END DO
  WRITE(0,'(a)') ": Done"

  ! STEP-00: Make Directory, Ising Spins (64-bit integer)
  WRITE(0,'(a)',advance='no') "Making Directory, Ising Spins (64-bit integer)..."
  DO i_w = 1, n_w, 1
    WRITE(sdirword(i_w), '(A4,I0.3)') "_is64",int(i_w)
    CALL system("mkdir "//sdirword(i_w))
  END DO
  
  WRITE(0,'(a)') ": Done"

  ! STEP-00: Set Array of Slots
  WRITE(0,'(a)',advance='no') "Setting Array of Slots..."
  MSC_sl_is = 20
  DO i_beta = 1, n_betas, 1
    MSC_sl_eb(i_beta) = 20+i_beta-1+1*n_betas; MSC_sl_mb(i_beta) = 20+i_beta-1+2*n_betas
    ! MSC_sl_ee(i_beta) = 20+i_beta-1+3*n_betas; MSC_sl_me(i_beta) = 20+i_beta-1+4*n_betas; MSC_sl_pu(i_beta) = 20+i_beta-1+5*n_betas
  END DO
  WRITE(0,'(a)') ": Done"

  ! STEP-00: Open All Slots
  WRITE(0,'(a)',advance='no') "Opening All Slots..."
  DO i_beta = 1, n_betas, 1
      ! OPEN(MSC_sl_is,file=sdirbeta(i_beta)//"is/is.bin",access="stream",status="replace")
      OPEN(MSC_sl_eb(i_beta),file=sdirbeta(i_beta)//"eb/eb.bin",access="stream",status="replace")
      OPEN(MSC_sl_mb(i_beta),file=sdirbeta(i_beta)//"mb/mb.bin",access="stream",status="replace")
      !  OPEN(MSC_sl_ee(i_beta),file=sdirbeta(i_beta)//"ee/ee.bin",access="stream",status="replace")
      !  OPEN(MSC_sl_me(i_beta),file=sdirbeta(i_beta)//"me/me.bin",access="stream",status="replace")
      !  OPEN(MSC_sl_pu(i_beta),file=sdirbeta(i_beta)//"pu/pu.bin",access="stream",status="replace")
  END DO
  WRITE(0,'(a)') ": Done"
  
  ! STEP-00: Set Array of Obs.
  WRITE(0,'(a)',advance='no') "Setting Array of Obs..."
  ALLOCATE(arr_ee(1:n_w,1:64),arr_me(1:n_w,1:64),arr_pu(1:n_w,1:64))
  WRITE(0,'(a)') ": Done"
  
  ! TODO: MPI Implementation of Running Many Samples

  ! STEP-00: Initialize All Vars.
  WRITE(0,'(a)',advance='no') "Initializing All Vars..."
  CALL MSC_initIS_2d(id_IC,id_BC,IS2_ini64(1:n_w,0:l_x+1,0:l_z+1))
  WRITE(0,'(a)') ": Done"

  ! STEP-00: Initialize The Array MSC_P
  WRITE(0, '(a)',advance='no') "Initializing The Array MSC_P..."
  CALL MTC_setMSC_P(betas(1:n_betas_c),MSC_P(1:4,0:max_l-1))
  WRITE(0,'(a)') ": Done"

  ! STEP-00: Initialize RNGs
  WRITE(0,'(a)',advance='no') "Initializing RNGs..."
  CALL constRand_SFMT19937(100+3*(s-1)+0,str_l)
  CALL constRand_SFMT19937(100+3*(s-1)+1,str_x)
  CALL constRand_SFMT19937(100+3*(s-1)+3,str_z)
  WRITE(0,'(a)') ": Done"

  ! STEP-01: Thermalize
  WRITE(0, '(a)',advance='no') "Thermalizing..."
  !$omp parallel do schedule(static,1) default(none) &
  !$omp shared(n_betas,n_w,l_x,l_z,l_t0,n_st,max_l,is2_64) &
  !$omp shared(str_l,str_x,str_z,r_l,r_x,r_z)
  DO i_w = 1, n_w, 1
    DO t = 1, l_t0, 1
        CALL updateIRand_Uniform(str_l,n_st,0,max_l,r_l(1:n_st))
        CALL updateIRand_Uniform(str_x,n_st,1,l_x+1,r_x(1:n_st))
        CALL updateIRand_Uniform(str_z,n_st,1,l_z+1,r_z(1:n_st))
        CALL MSC_mSSFs_2d_THERMALIZE(r_x(1:n_st),r_z(1:n_st),r_l(1:n_st),IS2_64(i_w,0:l_x+1,0:l_z+1))
        IF ( DBLE(t) / DBLE(l_t0) > 0.05d0 ) WRITE(0, '(a)',advance='no') "."
    END DO
  END DO
  !$omp end parallel do
  WRITE(0,'(a)') ": Done"

  ! STEP-00: Initialize All Obs.
  CALL MSC_calcEB_2d(IS2_64(1:n_w,0:l_x+1,0:l_z+1),arr_eb(1:n_w,1:64)); CALL MSC_calcMB_2d(IS2_64(1:n_w,0:l_x+1,0:l_z+1),arr_mb(1:n_w,1:64))

  ! STEP-02: Observe
  WRITE(0, '(a)',advance='no') "Observing..."
  !$omp parallel do schedule(static,1) default(none) &
  !$omp shared(n_betas,n_w,l_x,l_z,l_t1,n_st,max_l,is2_64,arr_eb,arr_mb) &
  !$omp shared(str_l,str_x,str_z,r_l,r_x,r_z,MSC_sl_eb,MSC_sl_mb)
  DO i_w = 1, n_w, 1
    DO t = 1, l_t1, 1
        CALL updateIRand_Uniform(str_l,n_st,0,max_l,r_l(1:n_st))
        CALL updateIRand_Uniform(str_x,n_st,1,l_x+1,r_x(1:n_st))
        CALL updateIRand_Uniform(str_z,n_st,1,l_z+1,r_z(1:n_st))
        CALL MSC_mSSFs_2d_OBSERVE(r_x(1:n_st),r_z(1:n_st),r_l(1:n_st),IS2_64(i_w,0:l_x+1,0:l_z+1),arr_eb(i_w,1:64),arr_mb(i_w,1:64))

        DO i_beta = 1+(i_w-1)*64, MIN(n_betas,i_w*64), 1
            WRITE(MSC_sl_eb(i_beta)) arr_eb(i_w,MOD(64,i_beta-1)+1); WRITE(MSC_sl_mb(i_beta)) arr_mb(i_w,MOD(64,i_beta-1)+1)
        END DO

        ! CALL MSC_calcEE_2d(IS2_64(i_w,0:l_x+1,0:l_z+1), arr_ee(i_w,1:64))
        ! CALL MSC_calcME_2d(IS2_64(i_w,0:l_x+1,0:l_z+1), arr_me(i_w,1:64))
        ! CALL MSC_calcPU_2d(IS2_64(i_w,0:l_x+1,0:l_z+1), arr_pu(i_w,1:64))
        ! DO i_beta = 1+(i_w-1)*64, MIN(n_betas,i_w*64), 1
        !     WRITE(MSC_sl_eb(MOD(64,i_beta-1)+1)) arr_eb(i_w,MOD(64,i_beta-1)+1)
        !     WRITE(MSC_sl_mb(MOD(64,i_beta-1)+1)) arr_mb(i_w,MOD(64,i_beta-1)+1)
        !     WRITE(MSC_sl_pu(MOD(64,i_beta-1)+1)) arr_pu(i_w,MOD(64,i_beta-1)+1)
        ! END DO
        IF ( DBLE(t) / DBLE(l_t1) > 0.05d0 ) WRITE(0,'(a)',advance='no') "."
    END DO
    CALL saveRand(str_l,"_ranst/str_l.bin"); CALL saveRand(str_x,"_ranst/str_x.bin"); CALL saveRand(str_z,"_ranst/str_z.bin")
    CALL destRand(str_l); CALL destRand(str_x); CALL destRand(str_z)
  END DO
  !$omp end parallel do
  WRITE(0,'(a)') ": Done"

  OPEN(MSC_sl_is,file="is/fin.bin",access="stream",status="replace")
  WRITE(MSC_sl_is) IS2_64(1:n_w,1:l_x,1:l_z)
  CLOSE(MSC_sl_is)

  DO i_beta = 1, n_betas, 1
    ! CLOSE(MSC_sl_is
    CLOSE(MSC_sl_eb(i_beta)); CLOSE(MSC_sl_mb(i_beta))
    ! CLOSE(MSC_sl_ee; CLOSE(MSC_sl_me(i_beta)); CLOSE(MSC_sl_pu(i_beta))
  END DO
END PROGRAM main
