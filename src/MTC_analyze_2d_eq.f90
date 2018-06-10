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
  REAL(8), ALLOCATABLE :: bin_eb(:,:,:), bin_mb(:,:,:), bin_cb(:,:,:), bin_chib(:,:,:), bin_ub(:,:,:)
  REAL(8), ALLOCATABLE :: mean_eb(:,:), mean_mb(:,:), mean_cb(:,:), mean_chib(:,:), mean_ub(:,:)
  REAL(8), ALLOCATABLE :: err_eb(:,:), err_mb(:,:), err_cb(:,:), err_chib(:,:), err_ub(:,:)
  ! MSC timeseries
  INTEGER(4), ALLOCATABLE :: MSC_TS_eb(:,:,:), MSC_TS_mb(:,:,:)

  ! STEP-00: Setting Params except for Temperature
  WRITE(0,'(a)',advance='no') "Setting Params except for Temperature... "
  CALL MTC_readParams_2d_eq(10,"params_sim",l_x,l_z,l_t0,l_t1,id_IC,id_BC)
  CALL MTC_readLB(10,"params_anal",lb); nb = l_t1 / lb
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
  CALL MSC_setSlots(n_w,MSC_sl_eb=MSC_sl_eb(1:n_w,0:63),MSC_sl_mb=MSC_sl_mb(1:n_w,0:63))
  WRITE(0,'(a)') ": Done"

  ! STEP-00: Open All Slots
  WRITE(0,'(a)',advance='no') "Opening All Slots... "
  CALL MSC_openSlots_old(n_w,n_betas,sbeta(1:n_w,0:63),MSC_sl_eb(1:n_w,0:63),MSC_sl_mb(1:n_w,0:63))
  WRITE(0,'(a)') ": Done"

  ! STEP-02: Load files
  ALLOCATE(bin_eb(1:n_w,0:63,1:nb),bin_mb(1:n_w,0:63,1:nb),bin_cb(1:n_w,0:63,1:nb),bin_chib(1:n_w,0:63,1:nb),bin_ub(1:n_w,0:63,1:nb))
  ALLOCATE(MSC_TS_eb(1:n_w,0:63,1:lb),MSC_TS_mb(1:n_w,0:63,1:lb))
  WRITE(0, '(a)',advance='no') "Loading files... "
  !$omp parallel do schedule(static,1) default(none) &
  !$omp shared(n_betas,nb,lb,MSC_beta,MSC_sl_eb,MSC_sl_mb,MSC_TS_eb,MSC_TS_mb,bin_eb,bin_mb,bin_cb,bin_chib,bin_ub) &
  !$omp shared(n_w,l_t1,l_x,l_z) &
  !$omp private(i_bit,ib)
  DO i_w = 1, n_w, 1
     DO i_bit = 0, MIN(n_betas-64*(i_w-1),63), 1
        DO ib = 1, nb, 1
           READ(MSC_sl_eb(i_w,i_bit),pos=1+4*(ib-1)*lb) MSC_TS_eb(i_w,i_bit,1:lb)
           READ(MSC_sl_mb(i_w,i_bit),pos=1+4*(ib-1)*lb) MSC_TS_mb(i_w,i_bit,1:lb)
           bin_eb(i_w,i_bit,ib) = DBLE(SUM(MSC_TS_eb(i_w,i_bit,1:lb)))/DBLE(l_x*l_z*lb)
           bin_mb(i_w,i_bit,ib) = DBLE(SUM(ABS(MSC_TS_mb(i_w,i_bit,1:lb))))/DBLE(l_x*l_z*lb)

           bin_cb(i_w,i_bit,ib) = MSC_beta(i_w,i_bit) ** 2 * (SUM(DBLE(MSC_TS_eb(i_w,i_bit,1:lb)**2)/DBLE(l_x*l_z))/DBLE(lb) - DBLE(l_x*l_z) * bin_eb(i_w,i_bit,ib) ** 2)
           bin_chib(i_w,i_bit,ib) = MSC_beta(i_w,i_bit) * (SUM(DBLE(MSC_TS_mb(i_w,i_bit,1:lb)**2)/DBLE(l_x*l_z))/DBLE(lb) - DBLE(l_x*l_z) * bin_mb(i_w,i_bit,ib) ** 2)
           bin_ub(i_w,i_bit,ib) = 1.5d0 - 0.5d0 * DBLE(lb) * SUM((DBLE(MSC_TS_mb(i_w,i_bit,1:lb))/DBLE(l_x*l_z))**4) / SUM((DBLE(MSC_TS_mb(i_w,i_bit,1:lb))/DBLE(l_x*l_z))**2) ** 2
        END DO
     END DO
  END DO
  !$omp end parallel do
  DEALLOCATE(MSC_TS_eb,MSC_TS_mb)
  WRITE(0,'(a)') ": Done"

  ! STEP-03: Close All Slots
  WRITE(0,'(a)',advance='no') "Closing All Slots... "
  CALL MSC_closeSlots(n_w,n_betas,MSC_sl_eb(1:n_w,0:63),MSC_sl_mb(1:n_w,0:63))
  WRITE(0,'(a)') ": Done"

	! !DEBUG
	! DO i_w = 1, n_w, 1
  !    DO i_bit = 0, MIN(n_betas-64*(i_w-1),63), 1
	! 		 DO ib = 1, nb, 1
	! 			 WRITE(0, *) "bin_eb(",i_w,",",i_bit,",",ib,") =",bin_eb(i_w,i_bit,ib)
	! 		 END DO
	! 	 END DO
	! END DO
	! STOP

  ! STEP-04: Make Stats
  WRITE(0, '(a)',advance='no') "Making Stats... "
  ALLOCATE(mean_eb(1:n_w,0:63), mean_mb(1:n_w,0:63), mean_cb(1:n_w,0:63), mean_chib(1:n_w,0:63), mean_ub(1:n_w,0:63))
  ALLOCATE(err_eb(1:n_w,0:63), err_mb(1:n_w,0:63), err_cb(1:n_w,0:63), err_chib(1:n_w,0:63), err_ub(1:n_w,0:63))
  !$omp parallel do schedule(static,1) default(none) &
  !$omp shared(MSC_beta, n_betas, nb) &
  !$omp shared(bin_eb, bin_mb, bin_cb, bin_chib, bin_ub) &
  !$omp shared(mean_eb, mean_mb, mean_cb, mean_chib, mean_ub) &
  !$omp shared(err_eb, err_mb, err_cb, err_chib, err_ub) &
  !$omp shared(n_w, l_t1, l_x, l_z) &
  !$omp private(i_bit)
  DO i_w = 1, n_w, 1
     DO i_bit = 0, MIN(n_betas-64*(i_w-1),63), 1
        CALL makeStats(nb,bin_eb(i_w,i_bit,1:nb),mean_eb(i_w,i_bit),err_eb(i_w,i_bit))
        CALL makeStats(nb,bin_mb(i_w,i_bit,1:nb),mean_mb(i_w,i_bit),err_mb(i_w,i_bit))

				! mean_cb(i_w,i_bit) = MSC_beta(i_w,i_bit) ** 2 * (SUM(bin_eb(i_w,i_bit,1:nb)**2)/DBLE(nb) - mean_eb(i_w,i_bit) ** 2) * DBLE(l_x*l_z)
				! mean_chib(i_w,i_bit) = MSC_beta(i_w,i_bit) * (SUM(bin_mb(i_w,i_bit,1:nb)**2)/DBLE(nb) - mean_mb(i_w,i_bit) ** 2) * DBLE(l_x*l_z)
				! mean_ub = 1.5d0 - 0.5d0 * DBLE(nb) * SUM((bin_mb(i_w,i_bit,1:nb)/DBLE(l_x*l_z))**4) / SUM((bin_mb(i_w,i_bit,1:nb)/DBLE(l_x*l_z))**2) ** 2

        CALL makeStats(nb,bin_cb(i_w,i_bit,1:nb),mean_cb(i_w,i_bit),err_cb(i_w,i_bit))
        CALL makeStats(nb,bin_chib(i_w,i_bit,1:nb),mean_chib(i_w,i_bit),err_chib(i_w,i_bit))
        CALL makeStats(nb,bin_ub(i_w,i_bit,1:nb),mean_ub(i_w,i_bit),err_ub(i_w,i_bit))
     END DO
  END DO
  !$omp end parallel do
  DEALLOCATE(bin_eb,bin_cb,bin_mb,bin_chib,bin_ub)
  WRITE(0,'(a)') ": Done"

  ! STEP-05: Write Stats
  WRITE(0, '(a)',advance='no') "Writing Stats... "
  OPEN(10, file="analysis.dat", status="replace")
  WRITE(10, '(a)') "# beta , eb          , err_eb      , mb          , err_mb      , cb          , err_cb      , chib        , err_chib    , ub          , err_ub"
  DO i_w = 1, n_w, 1
     DO i_bit = 0, MIN(n_betas-64*(i_w-1),63), 1
        WRITE(10, '(f7.4,5(", ",f12.4,", ",f12.10))') &
             MSC_beta(i_w,i_bit),&
             mean_eb(i_w,i_bit),err_eb(i_w,i_bit),mean_mb(i_w,i_bit),err_mb(i_w,i_bit),&
             mean_cb(i_w,i_bit),err_cb(i_w,i_bit),mean_chib(i_w,i_bit),err_chib(i_w,i_bit),mean_ub(i_w,i_bit),err_ub(i_w,i_bit)
     END DO
  END DO
  CLOSE(10)
  WRITE(0,'(a)') ": Done"

END PROGRAM main
