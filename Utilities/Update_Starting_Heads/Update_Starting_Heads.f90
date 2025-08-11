program Update_Starting_Heads
  implicit none
!-------------------------------------------------------------------------------------------------!
! Update_Starting_Heads
! Author: Leland Scantlebury, lscantle@ucdavis.edu
! Date: 2/12/2022
!
! Reads in head solution at specified SP, writes to arrays
! Written to replace Update_SVIHM_Starting_heads R script
!
! New in v0.0.2 (8/8/2022, by LS)
!  - Can set end SP, values will be averaged between start and end
!
! Command line arguments:
!  - heads file name (e.g., SVIHM.hds)
!  - (optional) desired stress period to get heads from
!  - (optional) end stress period to get heads from - heads will be averaged from start to end sp
!    (inclusive on both sides)
!
! Output: Files in the form of 'Starting_Heads_L*.txt' where * is the layer
!-------------------------------------------------------------------------------------------------!
  
  integer              :: i, j, k, ierr, kstp, kper, ncol, nrow, nsp, nlay, ilay, target_sp, end_sp
  integer              :: get_nlay
  real                 :: pertim, totim
  real,allocatable     :: temp_heads(:,:,:)
  real*8,allocatable   :: heads(:,:,:)
  character(len=16)    :: text, fmt
  character(len=60)    :: hds_file, outname
  
  write(*,*) '********************************************'
  write(*,*) '* Update_Starting_Heads - an SVIHM Utility *'
  write(*,*) '* v0.0.2                                   *'
  write(*,*) '********************************************'
  write(*,*) 'Status:'
  
  ! Intel-Fortran specific way of writing to same command line line
  open(6, carriagecontrol='fortran')
  
  ! Get heads filename from command line
  call getarg(1,hds_file)
  if (trim(hds_file)=='') then
    write(*,*) 'ERROR - No heads file passed. Stopping.'
    stop
  end if
  
  ! If passed, get target SP
  if (COMMAND_ARGUMENT_COUNT() > 1) then
    call getarg(2,text)
    read(text, '(i)') target_sp
  else
    ! Assume first SP is wanted
    target_sp = 1
  end if

  ! If passed, get END sp
  if (COMMAND_ARGUMENT_COUNT() > 2) then
    call getarg(3,text)
    read(text, '(i)') end_sp
    nsp = end_sp - target_sp + 1
  else
    end_sp = target_sp
    nsp = 1
  end if
  
  if (nsp == 1) then
    write(*,'(3x,a,i3)') 'Arrays will be from last entry from SP:', target_sp
  else
    write(*,'(3x,3(a,i3))') 'Arrays will be the average of SPs:', target_sp, '-', end_sp
  end if

  nlay = get_nlay(hds_file)
  
  ! Open heads file
  write(*,'(3x,2a)') 'Reading Heads File: ', trim(hds_file)
  open(10, file=trim(hds_file), form='binary', status='old', action='read', iostat=ierr)
  
  ! Read Heads File Header
  read(10,iostat=ierr) kstp, kper, pertim, totim, text, ncol, nrow, ilay
  write(*,'(3x,2(a,i4,2x))') 'Model Discretization | Rows: ', nrow, 'Columns: ', ncol
  
  ! Allocate, Initialize
  allocate(temp_heads(ncol, nrow, nlay), heads(ncol, nrow, nlay))
  heads = 0.0d0
  
  ! Get to it
  write(*,*) 'Reading:'
  k = 1
  do while (ierr == 0)
    ! Read 
    if (k>1) then
      read(10,iostat=ierr) kstp, kper, pertim, totim, text, ncol, nrow, ilay
    end if
    ! Report
    write(6,'("+",3x,3(a,i4,2x))') 'Time Step', kstp, 'Stress Period', kper, 'Layer', ilay
    ! If we've passed the target sp (all layers hopefully read) then exit
    if (kper > target_sp) then
      heads = heads + temp_heads
      target_sp = target_sp + 1
    end if
    read(10,iostat=ierr) temp_heads(1:ncol, 1:nrow, ilay)
    k = k + 1
    ! Second exit condition
    if (target_sp > end_sp) exit
  end do

  ! Average
  if (nsp > 1) then
    heads = heads / nsp
  end if

  ! Write
  do ilay=1, nlay
    write(outname,'("Starting_Heads_L",i1,".txt")') ilay
    write(fmt,'(a1,i3,"es16.8e2",a1)') "(", ncol, ")"
    write(*,*) 'Writing file ', outname
    open(20, file=outname, status='replace', action='write', iostat=ierr)
    write(20,fmt) heads(:,:,ilay)
    close(20)
  end do
  
  deallocate(heads)
  
  close(10)
  
  write(*,*) 'Done.'
  
end program Update_Starting_Heads

!-------------------------------------------------------------------------------------------------!

integer function get_nlay(filepath) result(nlay)
  implicit none

  integer              :: kstp, kper, ncol, nrow, ilay, ierr=0
  real                 :: pertim, totim
  real,allocatable     :: heads(:,:)
  character(len=16)    :: text, fmt
  character(*),intent(in) :: filepath

  ! Open file
  open(10, file=trim(filepath), form='binary', status='old', action='read', iostat=ierr)

  nlay = 0
  do while (ierr == 0)
    read(10,iostat=ierr) kstp, kper, pertim, totim, text, ncol, nrow, ilay
    if (allocated(heads)) then
      ! Nothing!
    else
      allocate(heads(ncol,nrow))
    end if
    if (ilay < nlay) exit  ! Not getting larger - nlay found
    nlay = ilay            ! Otherwise this is our new max layer
    read(10,iostat=ierr) heads(1:ncol, 1:nrow)
  end do

  close(10)

  return
end function get_nlay
