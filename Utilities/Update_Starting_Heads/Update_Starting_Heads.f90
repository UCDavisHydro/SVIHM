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
! Command line arguments: heads file name (e.g., SVIHM.hds)
! Output: Files in the form of 'Starting_Heads_L*.txt' where * is the layer
!-------------------------------------------------------------------------------------------------!
  
  integer              :: i, j, k, ierr, kstp, kper, ncol, nrow, ilay, target_sp
  real                 :: pertim, totim
  real,allocatable     :: heads(:,:)
  character(len=16)    :: text, fmt
  character(len=60)    :: hds_file, outname
  
  write(*,*) '********************************************'
  write(*,*) '* Update_Starting_Heads - an SVIHM Utility *'
  write(*,*) '* v0.0.1                                   *'
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
  
  write(*,'(3x,a,i3)') 'Arrays will be from last entry from SP:', target_sp
  
  ! Open heads file
  write(*,'(3x,2a)') 'Reading Heads File: ', trim(hds_file)
  open(10, file=trim(hds_file), form='binary', status='old', action='read', iostat=ierr)
  
  ! Read Heads File Header
  read(10,iostat=ierr) kstp, kper, pertim, totim, text, ncol, nrow, ilay
  write(*,'(3x,2(a,i4,2x))') 'Model Discretization | Rows: ', nrow, 'Columns: ', ncol
  
  ! Allocate
  allocate(heads(ncol, nrow))
  
  ! Read & Write
  write(*,*) 'Reading:'
  k = 1
  do while (ierr == 0)
    if (k>1) then
      read(10,iostat=ierr) kstp, kper, pertim, totim, text, ncol, nrow, ilay
    end if
        ! If we've passed the target sp (all layers hopefully written) then exit
    if (kper > target_sp) then
      exit
    end if
    ! Report
    write(6,'("+",3x,3(a,i4,2x))') 'Time Step', kstp, 'Stress Period', kper, 'Layer', ilay
    read(10,iostat=ierr) heads(1:ncol, 1:nrow)
    ! If it's the target sp, write it out
    if (kper == target_sp) then
      write(outname,'("Starting_Heads_L",i1,"_test.txt")') ilay
      write(fmt,'(a1,i3,"es16.8e2",a1)') "(", ncol, ")"  ! Nearly certian there's a smarter way...
      write(*,*) 'Writing file ', outname
      open(20, file=outname, status='replace', action='write', iostat=ierr)
      write(20,fmt) heads
      close(20)
    end if
    k = k +1
  end do
  
  deallocate(heads)
  
  close(10)
  
  write(*,*) 'Done.'
  
end program Update_Starting_Heads