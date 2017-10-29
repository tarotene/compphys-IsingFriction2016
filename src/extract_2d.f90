PROGRAM main
  USE mod_global
  USE mod_proc_int
  USE mod_proc_file
  IMPLICIT NONE

  ! INTEGER(kind = 4) :: i_sweep, i_dum, z, z_dum
  ! DOUBLE PRECISION, ALLOCATABLE :: pump(:, :), diss(:, :), energy(:, :)
  ! REAL(kind = 8) :: ave_pump, ave_diss, ave_energy
  ! REAL(kind = 8) :: err_pump, err_diss, err_energy
  ! CHARACTER(:), ALLOCATABLE :: filename
  ! CHARACTER(len = 4) :: si_sample

  ! loop variables
  INTEGER(kind = 4) :: i_sweep, i_dum, i_block

  ! observables
  REAL(kind = 8), ALLOCATABLE :: pump(:), diss(:), energy(:)
  REAL(kind = 8), ALLOCATABLE :: fluc_pump(:), fluc_diss(:), fluc_energy(:)
  REAL(kind = 8), ALLOCATABLE :: block_pump(:), block_diss(:), block_energy(:)
  REAL(kind = 8), ALLOCATABLE :: ave_pump, ave_diss, ave_energy
  REAL(kind = 8), ALLOCATABLE :: err_pump, err_diss, err_energy

  ! slot variables
  INTEGER(kind = 4) :: slot

  ! parameters
  INTEGER(kind = 4) :: waste, size_block

  ! reduced parameters
  INTEGER(kind = 4) :: num_blocks

  READ(*, *) beta, waste, size_block

  CALL getListParameters_2d(slot, "list_parameters.dat", &
       len_x, len_z, J, beta, vel, &
       n_sweeps_therm, n_sweeps_stead, id_IC, id_BC, n_samples0, &
       onoff_stream, onoff_m_z)

  num_blocks = (n_sweeps_stead - waste) / size_block

  ALLOCATE(pump(1:n_sweeps_stead))
  ALLOCATE(diss(1:n_sweeps_stead))
  ALLOCATE(energy(1:n_sweeps_stead))
  ALLOCATE(fluc_pump(1:n_sweeps_stead))
  ALLOCATE(fluc_diss(1:n_sweeps_stead))
  ALLOCATE(fluc_energy(1:n_sweeps_stead))

  OPEN(slot, file="stream.dat", status="old")
  READ(slot, '()')
  DO i_sweep = 1, n_sweeps_therm, 1
     READ(slot, '()')
  END DO
  READ(slot, '()')
  DO i_sweep = 1, n_sweeps_stead, 1
     READ(slot, *)  i_dum, &
          pump(i_sweep), diss(i_sweep), energy(i_sweep), &
          fluc_pump(i_sweep), fluc_diss(i_sweep), fluc_energy(i_sweep)
  END DO
  CLOSE(slot)

  ALLOCATE(block_pump(1:num_blocks))
  ALLOCATE(block_diss(1:num_blocks))
  ALLOCATE(block_energy(1:num_blocks))

  FORALL (i_block=1:num_blocks)
     block_pump(i_block) = &
          SUM(pump(waste+1+(i_block-1)*size_block:waste+i_block*size_block))/&
          DBLE(size_block)
     block_diss(i_block) = &
          SUM(diss(waste+1+(i_block-1)*size_block:waste+i_block*size_block))/&
          DBLE(size_block)
     block_energy(i_block) = &
          SUM(energy(waste+1+(i_block-1)*size_block:waste+i_block*size_block))/&
          DBLE(size_block)
  END FORALL

  ave_pump = SUM(block_pump(1:num_blocks)) / DBLE(num_blocks)
  ave_diss = SUM(block_diss(1:num_blocks)) / DBLE(num_blocks)
  ave_energy = SUM(block_energy(1:num_blocks)) / DBLE(num_blocks)

  err_pump = 0.0d0
  DO i_block = 1, num_blocks, 1
     err_pump = err_pump + (block_pump(i_block) - ave_pump) ** 2
  END DO
  err_pump = SQRT(err_pump / DBLE(num_blocks * (num_blocks - 1)))

  err_diss = 0.0d0
  DO i_block = 1, num_blocks, 1
     err_diss = err_diss + (block_diss(i_block) - ave_diss) ** 2
  END DO
  err_diss = SQRT(err_diss / DBLE(num_blocks * (num_blocks - 1)))

  err_energy = 0.0d0
  DO i_block = 1, num_blocks, 1
     err_energy = err_energy + (block_energy(i_block) - ave_energy) ** 2
  END DO
  err_energy = SQRT(err_energy / DBLE(num_blocks * (num_blocks - 1)))

  WRITE(*, '( f0.4, a, &
       f0.4, a, f0.4, a, &
       f0.4, a, f0.4, a, &
       f0.4, a, f0.4)') &
       beta, ", ", &
       ave_pump, ", ", err_pump, ", ", &
       ave_diss, ", ", err_diss, ", ", &
       ave_energy, ", ", err_energy
END PROGRAM main
