PROGRAM main
  !$  USE omp_lib
  USE mod_global
  USE mod_proc

  IMPLICIT NONE
  ! counter
  INTEGER(1) :: i_w, ib, i_bit
  INTEGER(4) :: t
  ! statistical vars
  INTEGER(4) :: lb, nb
  REAL(8), ALLOCATABLE :: bin_eb(:,:,:), mean_eb(:,:), err_eb(:,:)
  REAL(8), ALLOCATABLE :: bin_mb(:,:,:), mean_mb(:,:), err_mb(:,:)
  REAL(8), ALLOCATABLE :: bin_cb(:,:,:), mean_cb(:,:), err_cb(:,:)
  REAL(8), ALLOCATABLE :: bin_xb(:,:,:), mean_xb(:,:), err_xb(:,:)
  REAL(8), ALLOCATABLE :: bin_ub(:,:,:), mean_ub(:,:), err_ub(:,:)
  REAL(8), ALLOCATABLE :: bin_ee(:,:,:), mean_ee(:,:), err_ee(:,:)
  REAL(8), ALLOCATABLE :: bin_me(:,:,:), mean_me(:,:), err_me(:,:)
  REAL(8), ALLOCATABLE :: bin_ce(:,:,:), mean_ce(:,:), err_ce(:,:)
  REAL(8), ALLOCATABLE :: bin_xe(:,:,:), mean_xe(:,:), err_xe(:,:)
  REAL(8), ALLOCATABLE :: bin_ue(:,:,:), mean_ue(:,:), err_ue(:,:)
  REAL(8), ALLOCATABLE :: bin_pu(:,:,:), mean_pu(:,:), err_pu(:,:)
  ! MSC timeseries
  INTEGER(4), ALLOCATABLE :: MSC_TS_eb(:,:,:), MSC_TS_mb(:,:,:)
  INTEGER(4), ALLOCATABLE :: MSC_TS_ee(:,:,:), MSC_TS_me(:,:,:)
  INTEGER(4), ALLOCATABLE :: MSC_TS_pu(:,:,:)

  ! STEP-00: Setting Params except for Temperature
  WRITE(0,'(a)',advance='no') "Setting Params except for Temperature... "
  CALL MTC_readParams_2d(10,"params_sim",l_x,l_z,vel,l_t0,l_t1,id_IC,id_BC)
  CALL MTC_readLB(10,"params_anal",lb); nb = l_t1 / lb
  WRITE(0,'(a)') ": Done"

  ! STEP-00: Set Array and Make Label of Temperatures
  WRITE(0,'(a)',advance='no') "Setting Array and Making Labels of Temperatures... "
  CALL countLines(10,"list_beta",n_betas); n_w = (n_betas-1)/64 + 1
  ALLOCATE(MSC_beta(1:n_w,0:63),sbeta(1:n_w,0:63),MSC_sl_iw(1:n_w),MSC_sl_eb(1:n_w,0:63),MSC_sl_mb(1:n_w,0:63),MSC_sl_ee(1:n_w,0:63),MSC_sl_me(1:n_w,0:63),MSC_sl_pu(1:n_w,0:63))
  CALL MTC_readBetas(10,"list_beta",MSC_beta(1:n_w,0:63),n_w,n_betas)
  CALL MTC_makeLabelsBeta(MSC_beta(1:n_w,0:63),n_w,sbeta(1:n_w,0:63))
  WRITE(0,'(a)') ": Done"

  ! STEP-00: Set Array of Slots
  WRITE(0,'(a)',advance='no') "Setting Array of Slots... "
	CALL MSC_setSlots(n_w,&
	MSC_sl_iw(1:n_w),MSC_sl_eb(1:n_w,0:63),MSC_sl_mb(1:n_w,0:63),MSC_sl_ee(1:n_w,0:63),MSC_sl_me(1:n_w,0:63),MSC_sl_pu(1:n_w,0:63))
  WRITE(0,'(a)') ": Done"

  ! STEP-00: Open All Slots
  WRITE(0,'(a)',advance='no') "Opening All Slots... "
  CALL MSC_openSlots_old(n_w,n_betas,sbeta(1:n_w,0:63),&
       MSC_sl_eb(1:n_w,0:63),MSC_sl_mb(1:n_w,0:63),MSC_sl_ee(1:n_w,0:63),MSC_sl_me(1:n_w,0:63),MSC_sl_pu(1:n_w,0:63))
  WRITE(0,'(a)') ": Done"

  ! STEP-02: Load files
  ALLOCATE(bin_eb(1:n_w,0:63,1:nb),bin_mb(1:n_w,0:63,1:nb),bin_cb(1:n_w,0:63,1:nb),bin_xb(1:n_w,0:63,1:nb),bin_ub(1:n_w,0:63,1:nb))
  ALLOCATE(bin_ee(1:n_w,0:63,1:nb),bin_me(1:n_w,0:63,1:nb),bin_ce(1:n_w,0:63,1:nb),bin_xe(1:n_w,0:63,1:nb),bin_ue(1:n_w,0:63,1:nb))
  ALLOCATE(bin_pu(1:n_w,0:63,1:nb))
  ALLOCATE(MSC_TS_eb(1:n_w,0:63,1:lb),MSC_TS_mb(1:n_w,0:63,1:lb))
  ALLOCATE(MSC_TS_ee(1:n_w,0:63,1:lb),MSC_TS_me(1:n_w,0:63,1:lb))
  ALLOCATE(MSC_TS_pu(1:n_w,0:63,1:lb))
  WRITE(0, '(a)',advance='no') "Loading files... "
  !$omp parallel do schedule(static,1) default(none) &
  !$omp shared(n_betas,nb,lb,MSC_beta) &
  !$omp shared(MSC_sl_eb,MSC_sl_mb,MSC_TS_eb,MSC_TS_mb,bin_eb,bin_mb,bin_cb,bin_xb,bin_ub) &
  !$omp shared(MSC_sl_ee,MSC_sl_me,MSC_TS_ee,MSC_TS_me,bin_ee,bin_me,bin_ce,bin_xe,bin_ue) &
  !$omp shared(MSC_sl_pu,MSC_TS_pu,bin_pu) &
  !$omp shared(n_w,l_t1,l_x,l_z) &
  !$omp private(i_bit,ib)
  DO i_w = 1, n_w, 1
     DO i_bit = 0, MIN(n_betas-64*(i_w-1),64) - 1, 1
        DO ib = 1, nb, 1
           READ(MSC_sl_eb(i_w,i_bit),pos=1+4*(ib-1)*lb) MSC_TS_eb(i_w,i_bit,1:lb)
           READ(MSC_sl_mb(i_w,i_bit),pos=1+4*(ib-1)*lb) MSC_TS_mb(i_w,i_bit,1:lb)
           READ(MSC_sl_ee(i_w,i_bit),pos=1+4*(ib-1)*lb) MSC_TS_ee(i_w,i_bit,1:lb)
           READ(MSC_sl_me(i_w,i_bit),pos=1+4*(ib-1)*lb) MSC_TS_me(i_w,i_bit,1:lb)
           READ(MSC_sl_pu(i_w,i_bit),pos=1+4*(ib-1)*lb) MSC_TS_pu(i_w,i_bit,1:lb)

           bin_eb(i_w,i_bit,ib) = SUM(DBLE(MSC_TS_eb(i_w,i_bit,1:lb))/DBLE(l_x*(2*l_z+1)*lb))
           bin_mb(i_w,i_bit,ib) = SUM(DBLE(ABS(MSC_TS_mb(i_w,i_bit,1:lb)))/DBLE(l_x*l_z*lb))
           bin_ee(i_w,i_bit,ib) = SUM(DBLE(MSC_TS_ee(i_w,i_bit,1:lb))/DBLE(l_x*lb))
           bin_me(i_w,i_bit,ib) = SUM(DBLE(ABS(MSC_TS_me(i_w,i_bit,1:lb)))/DBLE(2*l_x*lb))
           bin_pu(i_w,i_bit,ib) = SUM(DBLE(MSC_TS_pu(i_w,i_bit,1:lb))/DBLE(l_x*lb))

           bin_cb(i_w,i_bit,ib) = MSC_beta(i_w,i_bit) ** 2 * (SUM((DBLE(MSC_TS_eb(i_w,i_bit,1:lb))/DBLE(l_x*(2*l_z+1)))**2)/DBLE(lb) - bin_eb(i_w,i_bit,ib) ** 2) * DBLE(l_x*(2*l_z+1))
           bin_xb(i_w,i_bit,ib) = MSC_beta(i_w,i_bit) * (SUM((DBLE(MSC_TS_mb(i_w,i_bit,1:lb))/DBLE(l_x*l_z))**2)/DBLE(lb) - bin_mb(i_w,i_bit,ib) ** 2) * DBLE(l_x*l_z)
           bin_ub(i_w,i_bit,ib) = 1.5d0 - 0.5d0 * DBLE(lb) * SUM((DBLE(MSC_TS_mb(i_w,i_bit,1:lb))/DBLE(l_x*l_z))**4) / SUM((DBLE(MSC_TS_mb(i_w,i_bit,1:lb))/DBLE(l_x*l_z))**2) ** 2
           bin_ce(i_w,i_bit,ib) = MSC_beta(i_w,i_bit) ** 2 * (SUM((DBLE(MSC_TS_ee(i_w,i_bit,1:lb))/DBLE(l_x))**2)/DBLE(lb) - bin_ee(i_w,i_bit,ib) ** 2) * DBLE(l_x)
           bin_xe(i_w,i_bit,ib) = MSC_beta(i_w,i_bit) * (SUM((DBLE(MSC_TS_me(i_w,i_bit,1:lb))/DBLE(2 * l_x))**2)/DBLE(lb) - bin_me(i_w,i_bit,ib) ** 2) * DBLE(2 * l_x)
           bin_ue(i_w,i_bit,ib) = 1.5d0 - 0.5d0 * DBLE(lb) * SUM((DBLE(MSC_TS_me(i_w,i_bit,1:lb))/DBLE(2 * l_x))**4) / SUM((DBLE(MSC_TS_me(i_w,i_bit,1:lb))/DBLE(2 * l_x))**2) ** 2
        END DO
     END DO
  END DO
  !$omp end parallel do
  DEALLOCATE(MSC_TS_eb,MSC_TS_mb,MSC_TS_ee,MSC_TS_me,MSC_TS_pu)
  WRITE(0,'(a)') ": Done"

  ! STEP-03: Close All Slots
  WRITE(0,'(a)',advance='no') "Closing All Slots... "
  CALL MSC_closeSlots(n_w,n_betas,&
       MSC_sl_eb(1:n_w,0:63),MSC_sl_mb(1:n_w,0:63),MSC_sl_ee(1:n_w,0:63),MSC_sl_me(1:n_w,0:63),MSC_sl_pu(1:n_w,0:63))
  WRITE(0,'(a)') ": Done"

  ! !DEBUG
  ! DO i_w = 1, n_w, 1
  !    DO i_bit = 0, MIN(n_betas-64*(i_w-1),64) - 1, 1
  ! 		 DO ib = 1, nb, 1
  ! 			 WRITE(0, *) "bin_eb(",i_w,",",i_bit,",",ib,") =",bin_eb(i_w,i_bit,ib)
  ! 		 END DO
  ! 	 END DO
  ! END DO
  ! STOP

  ! STEP-04: Make Stats
  WRITE(0, '(a)',advance='no') "Making Stats... "
  ALLOCATE(mean_eb(1:n_w,0:63),err_eb(1:n_w,0:63))
  ALLOCATE(mean_mb(1:n_w,0:63),err_mb(1:n_w,0:63))
  ALLOCATE(mean_cb(1:n_w,0:63),err_cb(1:n_w,0:63))
  ALLOCATE(mean_xb(1:n_w,0:63),err_xb(1:n_w,0:63))
  ALLOCATE(mean_ub(1:n_w,0:63),err_ub(1:n_w,0:63))
  ALLOCATE(mean_ee(1:n_w,0:63),err_ee(1:n_w,0:63))
  ALLOCATE(mean_me(1:n_w,0:63),err_me(1:n_w,0:63))
  ALLOCATE(mean_ce(1:n_w,0:63),err_ce(1:n_w,0:63))
  ALLOCATE(mean_xe(1:n_w,0:63),err_xe(1:n_w,0:63))
  ALLOCATE(mean_ue(1:n_w,0:63),err_ue(1:n_w,0:63))
  ALLOCATE(mean_pu(1:n_w,0:63),err_pu(1:n_w,0:63))
  !$omp parallel do schedule(static,1) default(none) &
  !$omp shared(MSC_beta, n_betas, nb) &
  !$omp shared(bin_eb, mean_eb, err_eb) &
  !$omp shared(bin_mb, mean_mb, err_mb) &
  !$omp shared(bin_cb, mean_cb, err_cb) &
  !$omp shared(bin_xb, mean_xb, err_xb) &
  !$omp shared(bin_ub, mean_ub, err_ub) &
  !$omp shared(bin_ee, mean_ee, err_ee) &
  !$omp shared(bin_me, mean_me, err_me) &
  !$omp shared(bin_ce, mean_ce, err_ce) &
  !$omp shared(bin_xe, mean_xe, err_xe) &
  !$omp shared(bin_ue, mean_ue, err_ue) &
  !$omp shared(bin_pu, mean_pu, err_pu) &
  !$omp shared(n_w, l_t1, l_x, l_z) &
  !$omp private(i_bit)
  DO i_w = 1, n_w, 1
     DO i_bit = 0, MIN(n_betas-64*(i_w-1),64) - 1, 1
        CALL makeStats(nb,bin_eb(i_w,i_bit,1:nb),mean_eb(i_w,i_bit),err_eb(i_w,i_bit))
        CALL makeStats(nb,bin_mb(i_w,i_bit,1:nb),mean_mb(i_w,i_bit),err_mb(i_w,i_bit))
        CALL makeStats(nb,bin_cb(i_w,i_bit,1:nb),mean_cb(i_w,i_bit),err_cb(i_w,i_bit))
        CALL makeStats(nb,bin_xb(i_w,i_bit,1:nb),mean_xb(i_w,i_bit),err_xb(i_w,i_bit))
        CALL makeStats(nb,bin_ub(i_w,i_bit,1:nb),mean_ub(i_w,i_bit),err_ub(i_w,i_bit))
        CALL makeStats(nb,bin_ee(i_w,i_bit,1:nb),mean_ee(i_w,i_bit),err_ee(i_w,i_bit))
        CALL makeStats(nb,bin_me(i_w,i_bit,1:nb),mean_me(i_w,i_bit),err_me(i_w,i_bit))
        CALL makeStats(nb,bin_ce(i_w,i_bit,1:nb),mean_ce(i_w,i_bit),err_ce(i_w,i_bit))
        CALL makeStats(nb,bin_xe(i_w,i_bit,1:nb),mean_xe(i_w,i_bit),err_xe(i_w,i_bit))
        CALL makeStats(nb,bin_ue(i_w,i_bit,1:nb),mean_ue(i_w,i_bit),err_ue(i_w,i_bit))
        CALL makeStats(nb,bin_pu(i_w,i_bit,1:nb),mean_pu(i_w,i_bit),err_pu(i_w,i_bit))
     END DO
  END DO
  !$omp end parallel do
  DEALLOCATE(bin_eb,bin_cb,bin_mb,bin_xb,bin_ub)
  DEALLOCATE(bin_ee,bin_ce,bin_me,bin_xe,bin_ue)
  DEALLOCATE(bin_pu)
  WRITE(0,'(a)') ": Done"

  ! STEP-05: Write Stats
  WRITE(0, '(a)',advance='no') "Writing Stats... "
  OPEN(10, file="analysis_bulk.dat", status="replace")
  WRITE(10, '(a)') "# beta, eb, err_eb, mb, err_mb, cb, err_cb, xb, err_xb, ub, err_ub"
  DO i_w = 1, n_w, 1
     DO i_bit = 0, MIN(n_betas-64*(i_w-1),64) - 1, 1
        WRITE(10, '(f8.4,10(", ",f8.4))') &
             MSC_beta(i_w,i_bit),&
             mean_eb(i_w,i_bit),err_eb(i_w,i_bit),mean_mb(i_w,i_bit),err_mb(i_w,i_bit),&
             mean_cb(i_w,i_bit),err_cb(i_w,i_bit),mean_xb(i_w,i_bit),err_xb(i_w,i_bit),mean_ub(i_w,i_bit),err_ub(i_w,i_bit)
     END DO
  END DO
  CLOSE(10)
	OPEN(11, file="analysis_edge.dat", status="replace")
  WRITE(11, '(a)') "# beta, ee, err_ee, me, err_me, ce, err_ce, xe, err_xe, ue, err_ue, pu, err_pu"
  DO i_w = 1, n_w, 1
     DO i_bit = 0, MIN(n_betas-64*(i_w-1),64) - 1, 1
        WRITE(11, '(f8.4,12(", ",f8.4))') &
             MSC_beta(i_w,i_bit),&
             mean_ee(i_w,i_bit),err_ee(i_w,i_bit),mean_me(i_w,i_bit),err_me(i_w,i_bit),&
             mean_ce(i_w,i_bit),err_ce(i_w,i_bit),mean_xe(i_w,i_bit),err_xe(i_w,i_bit),mean_ue(i_w,i_bit),err_ue(i_w,i_bit),&
             mean_pu(i_w,i_bit),err_pu(i_w,i_bit)
     END DO
  END DO
  CLOSE(11)
  WRITE(0,'(a)') ": Done"

END PROGRAM main
