PROGRAM main
  !$  USE omp_lib
  USE mod_global
  USE mod_proc

  IMPLICIT NONE

  ! counters
  INTEGER(4) :: i_w, i_bit
  INTEGER(4) :: t
  CHARACTER(len=2) :: si_w
  ! directions
  INTEGER(4), ALLOCATABLE :: ex(:,:), ez(:,:)
  INTEGER(4), ALLOCATABLE :: wx(:,:), wz(:,:)
  INTEGER(4), ALLOCATABLE :: sx(:,:), sz(:,:)
  INTEGER(4), ALLOCATABLE :: nx(:,:), nz(:,:)
  ! MSC spin vars
  INTEGER(8), ALLOCATABLE :: IW2(:,:,:)
  ! random streams and vars
  TYPE(VSL_STREAM_STATE) :: str_l, str_x, str_z
  INTEGER(4), ALLOCATABLE :: r_l(:), r_x(:), r_z(:)
  INTEGER(8), ALLOCATABLE :: r_a(:,:,:)
  ! MSC observables
  INTEGER(4), ALLOCATABLE :: MSC_eb(:,:), MSC_mb(:,:)
  INTEGER(4) :: d_eb(0:1,0:1,0:1,0:1),d_mb(0:1,0:1)

  CALL MSC_setDifferenceTable(d_eb(0:1,0:1,0:1,0:1),d_mb(0:1,0:1))

  ! STEP-00: Set Params except for Temperature
  WRITE(0,'(a)',advance='no') "Setting Params except for Temperature... "
  CALL MTC_readParams_2d_eq(10,"params_sim",l_x,l_z,l_t0,l_t1,id_IC,id_BC); n_st = l_x*l_z
  ALLOCATE(r_l(1:n_st),r_x(1:n_st),r_z(1:n_st))
  ALLOCATE(ex(1:l_x,1:l_z),ez(1:l_x,1:l_z),wx(1:l_x,1:l_z),wz(1:l_x,1:l_z),sx(1:l_x,1:l_z),sz(1:l_x,1:l_z),nx(1:l_x,1:l_z),nz(1:l_x,1:l_z))
  CALL applyPBC_2d(l_x,l_z,ex(1:l_x,1:l_z),ez(1:l_x,1:l_z),wx(1:l_x,1:l_z),wz(1:l_x,1:l_z),sx(1:l_x,1:l_z),sz(1:l_x,1:l_z),nx(1:l_x,1:l_z),nz(1:l_x,1:l_z))
  WRITE(0,'(a)') ": Done"

  ! STEP-00: Set Array and Make Label of Temperatures
  WRITE(0,'(a)',advance='no') "Setting Array and Making Labels of Temperatures... "
  CALL countLines(10,"list_beta",n_betas); n_w = n_betas/64
  ALLOCATE(MSC_beta(1:n_w,0:63),sbeta(1:n_w,0:63),MSC_sl_iw(1:n_w),MSC_sl_eb(1:n_w,0:63),MSC_sl_mb(1:n_w,0:63))
  CALL MTC_readBetas(10,"list_beta",MSC_beta(1:n_w,0:63),n_w,n_betas)
  CALL MTC_makeLabelsBeta(MSC_beta(1:n_w,0:63),n_w,sbeta(1:n_w,0:63))
  WRITE(0,'(a)') ": Done"

  ! STEP-00: Set Array of Slots
  WRITE(0,'(a)',advance='no') "Setting Array of Slots... "
  CALL MSC_setSlots(n_w,MSC_sl_iw(1:n_w),MSC_sl_eb(1:n_w,0:63),MSC_sl_mb(1:n_w,0:63))
  WRITE(0,'(a)') ": Done"

  ! STEP-00: Open All Slots
  WRITE(0,'(a)',advance='no') "Opening All Slots... "
  CALL MSC_openSlots_rep(n_w,n_betas,sbeta(1:n_w,0:63),MSC_sl_eb(1:n_w,0:63),MSC_sl_mb(1:n_w,0:63))
  WRITE(0,'(a)') ": Done"

  ! STEP-00: Allocate Vars. and Obs.
  WRITE(0,'(a)',advance='no') "Allocating Vars. and Obs... "
  ALLOCATE(IW2(1:n_w,1:l_x,0:l_z+1),MSC_eb(1:n_w,0:63),MSC_mb(1:n_w,0:63))
  ! ALLOCATE(MSC_ee(1:n_w,0:63),MSC_me(1:n_w,0:63),MSC_pu(1:n_w,0:63))
  WRITE(0,'(a)') ": Done"

  ! TODO: MPI or Other Script Lang. Implementation of Running Many Samples

  ! STEP-00: Initialize Ising Words.
  WRITE(0,'(a)',advance='no') "Initializing Ising Words..."
  CALL MSC_initIW_2d(id_BC,id_IC,IW2(1:n_w,1:l_x,0:l_z+1),n_w,l_x,l_z)
  WRITE(0,'(a)') ": Done"

  ! STEP-00: Create the Random Table
  max_l = ISHFT(1,20); ALLOCATE(r_a(1:n_w,0:3,0:max_l-1))
  WRITE(0, '(a)',advance='no') "Creating the Random Table... "
  CALL MTC_setRandomTable(MSC_beta(1:n_w,0:63),r_a(1:n_w,0:3,0:max_l-1),max_l,n_w)
  WRITE(0,'(a)') ": Done"

  ! STEP-00: Initialize RNGs
  WRITE(0,'(a)',advance='no') "Initializing RNGs... "
  CALL constRand_SFMT19937(100+0,str_l)
  CALL constRand_SFMT19937(100+1,str_x)
  CALL constRand_SFMT19937(100+3,str_z)
  WRITE(0,'(a)') ": Done"

  ! STEP-01: Thermalize
  WRITE(0, '(a)',advance='no') "Thermalizing..."
  !$omp parallel do schedule(static,1) default(none) &
  !$omp shared(n_w,l_t0,n_st,max_l,l_x,l_z) &
  !$omp shared(IW2,r_l,r_x,r_z,r_a,str_l,str_x,str_z) &
  !$omp shared(ex,ez,wx,wz,sx,sz,nx,nz) &
  !$omp private(t)
  DO i_w = 1, n_w, 1
     DO t = 1, l_t0, 1
        CALL updateIRand_Uniform(str_l,n_st,0,max_l,r_l(1:n_st))
        CALL updateIRand_Uniform(str_x,n_st,1,l_x+1,r_x(1:n_st))
        CALL updateIRand_Uniform(str_z,n_st,1,l_z+1,r_z(1:n_st))
        CALL MSC_mSSFs_2d_eq(r_l(1:n_st),r_x(1:n_st),r_z(1:n_st),n_st,&
             ex(1:l_x,1:l_z),ez(1:l_x,1:l_z),wx(1:l_x,1:l_z),wz(1:l_x,1:l_z),&
             sx(1:l_x,1:l_z),sz(1:l_x,1:l_z),nx(1:l_x,1:l_z),nz(1:l_x,1:l_z),&
             r_a(i_w,0:3,0:max_l-1),IW2(i_w,1:l_x,0:l_z+1))
     END DO
  END DO
  !$omp end parallel do
  WRITE(0,'(a)') ": Done"

  ! STEP-01: Save Ising Words and Random Streams
  WRITE(0,'(a)',advance='no') "Saving Ising Words and Random Streams (at Thermalized)... "
  CALL MSC_saveIsingWords_th(IW2(1:n_w,1:l_x,1:l_z),n_w,l_x,l_z,MSC_sl_iw(1:n_w))
  CALL saveRand(str_l,"_str_th_l.bin")
  CALL saveRand(str_x,"_str_th_x.bin")
  CALL saveRand(str_z,"_str_th_z.bin")
  WRITE(0,'(a)') ": Done"

  ! STEP-02: Initialize All Obs.
  DO i_w = 1, n_w, 1
     CALL MSC_calcEB_2d(IW2(i_w,1:l_x,0:l_z+1),l_x,l_z,MSC_eb(i_w,0:63))
     CALL MSC_calcMB_2d(IW2(i_w,1:l_x,1:l_z),l_x,l_z,MSC_mb(i_w,0:63))
  END DO

  ! DEBUG
  ! DO i_w = 1, n_w, 1
  !   DO i_bit = 0, 63, 1
  !     WRITE(0,'("eb(",i2,",",i2,")=",i0.5)') i_w,i_bit,MSC_eb(i_w,i_bit,1)
  !   END DO
  ! END DO
  ! DO i_w = 1, n_w, 1
  !   DO i_bit = 0, 63, 1
  !     WRITE(0,'("mb(",i2,",",i2,")=",i0.5)') i_w,i_bit,MSC_mb(i_w,i_bit,1)
  !   END DO
  ! END DO
  ! STOP

  ! STEP-02: Observe
  WRITE(0, '(a)',advance='no') "Observing... "
  !$omp parallel do schedule(static,1) default(none) &
  !$omp shared(n_w,l_t1,n_st,max_l,l_x,l_z) &
  !$omp shared(IW2,r_l,r_x,r_z,r_a,str_l,str_x,str_z) &
  !$omp shared(ex,ez,wx,wz,sx,sz,nx,nz) &
  !$omp shared(d_eb,d_mb,MSC_eb,MSC_mb,MSC_sl_eb,MSC_sl_mb) &
  !$omp private(t)
  DO i_w = 1, n_w, 1
     DO t = 1, l_t1, 1
        CALL updateIRand_Uniform(str_l,n_st,0,max_l,r_l(1:n_st))
        CALL updateIRand_Uniform(str_x,n_st,1,l_x+1,r_x(1:n_st))
        CALL updateIRand_Uniform(str_z,n_st,1,l_z+1,r_z(1:n_st))
        CALL MSC_mSSFs_2d_eq(r_l(1:n_st),r_x(1:n_st),r_z(1:n_st),n_st,&
             ex(1:l_x,1:l_z),ez(1:l_x,1:l_z),wx(1:l_x,1:l_z),wz(1:l_x,1:l_z),&
             sx(1:l_x,1:l_z),sz(1:l_x,1:l_z),nx(1:l_x,1:l_z),nz(1:l_x,1:l_z),&
             r_a(i_w,0:3,0:max_l-1),IW2(i_w,1:l_x,0:l_z+1))
             
        CALL MSC_calcEB_2d(IW2(i_w,1:l_x,0:l_z+1),l_x,l_z,MSC_eb(i_w,0:63))
        CALL MSC_calcMB_2d(IW2(i_w,1:l_x,1:l_z),l_x,l_z,MSC_mb(i_w,0:63))

        DO i_bit = 0, 63, 1
           WRITE(MSC_sl_eb(i_w,i_bit)) MSC_eb(i_w,i_bit)
           WRITE(MSC_sl_mb(i_w,i_bit)) MSC_mb(i_w,i_bit)
        END DO
     END DO
  END DO
  !$omp end parallel do

  WRITE(0,'(a)') ": Done"

  ! STEP-XX: Save Ising Words and Random Streams
  WRITE(0,'(a)',advance='no') "Saving Ising Words and Random Streams (at Observed)... "
  CALL MSC_saveIsingWords_ob(IW2(1:n_w,1:l_x,1:l_z),n_w,l_x,l_z,MSC_sl_iw(1:n_w))
  CALL saveRand(str_l,"_str_ob_l.bin"); CALL destRand(str_l)
  CALL saveRand(str_x,"_str_ob_x.bin"); CALL destRand(str_x)
  CALL saveRand(str_z,"_str_ob_z.bin"); CALL destRand(str_z)
  WRITE(0,'(a)') ": Done"

  ! STEP-XX: Close All Slots
  WRITE(0,'(a)',advance='no') "Closing All Slots... "
  CALL MSC_closeSlots(n_w,n_betas,MSC_sl_eb(1:n_w,0:63),MSC_sl_mb(1:n_w,0:63))
  WRITE(0,'(a)') ": Done"
END PROGRAM main
