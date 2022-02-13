program Update_Drain_Inflows
  implicit none
!-------------------------------------------------------------------------------------------------!
!
!-------------------------------------------------------------------------------------------------!
  integer              :: i, nsp, sp, ts, ierr, ipos
  integer,allocatable  :: nstp(:)
  real*8               :: splen, mult, drn_out, prev_out
  character(200)       :: line
  character(60)        :: mf_lst, swbm_drn
  character(2)         :: ss_flag
  
  write(*,*) '*******************************************'
  write(*,*) '* Update_Drain_Inflows - an SVIHM Utility *'
  write(*,*) '* v0.0.1                                  *'
  write(*,*) '*******************************************'
  write(*,*) 'Status:'
  
  ! Intel-Fortran specific way of writing to same command line line
  open(6, carriagecontrol='fortran')
  
  ! Read input file
  write(*,*) 'Reading Input File'
  open(10, file='Update_Drain_Inflows.in', status='old', action='read', iostat=ierr)
  ! Skip header line(s) denoted by pound symbol (#)
  do while (ierr == 0)
    read(10,*) line
    if (line(1:1) /= '#') then
      backspace(10)
      read(10,*,iostat=ierr) mf_lst
      read(10,*,iostat=ierr) swbm_drn
      exit
    end if
  end do
  close(10)
  
  ! Open MODFLOW LST file
  write(6,'("+",2x,2a)') 'Opening MODFLOW LST File: ', trim(mf_lst)
  open(10, file=trim(mf_lst), status='old', action='read', iostat=ierr)

  
  ! Get number of stress periods from LST file
  call FindLine(10, 'STRESS PERIOD(S) IN SIMULATION', .true.)
  read(10,*) nsp
  allocate(nstp(nsp))
  
  ! Find Stress Period Lengths, as written in LST file
  call FindLine(10, 'STRESS PERIOD', .false.)
  read(10, *)      ! table border
  do i=1, nsp
    read(10,*) sp, splen, ts, mult, ss_flag
    nstp(sp) = ts
  end do
  
  ! Open SWBM Drain file
  write(6,'("+",2x,2a)') 'Opening SWBM Drain Output File: ', trim(swbm_drn)
  open(20, file=trim(swbm_drn), status='replace', action='write', iostat=ierr)
  write(20,'(a)') 'Drain flow rate (m^3/day)'
  
  ! Read through LST file finding stress period cumulative DRN outputs
  write(6,'("+",a100)'), ' '  ! Clears line
  prev_out = 0.0
  do i=1, nsp
    write(6,'("+",2x,2(a,i4))') 'Processing SP: ', i, ' out of ', nsp
    call FindBudgetLine(10, i, nstp(i), .false.)
    ! First in the budget is the Drain IN, which is always zero
    call FindLine(10, 'DRAINS', .false.)
    ! Next is the Drain OUT
    call FindLine(10, 'DRAINS', .true.)
    ! Read in cumulative value
    read(10,'(a200)') line
    read(line(24:40), *) drn_out
    ! Write increase over previous SP, as TS average, to SWBM file
    write(20,'(f16.8)') (drn_out-prev_out)/nstp(i)
    prev_out = drn_out
  end do
  
  write(*,*) 'End of Stress Periods. Closing Files... '
  close(10)
  close(20)
  write(*,*) 'Done.'
  
  close(6)
  
end program Update_Drain_Inflows

!-------------------------------------------------------------------------------------------------!

subroutine FindLine(unitno, linetxt, move_back)
  implicit none
  
  integer                  :: ierr, success
  integer, intent(in)      :: unitno
  character(*), intent(in) :: linetxt
  character(200)           :: line
  logical, intent(in)      :: move_back
  
  success=0
  ierr=0
  do while (ierr == 0)
    read(10,'(a200)',iostat=ierr) line
    if (index(line,linetxt) > 0) then
      success = 1
      if (move_back) backspace(unitno)
      exit
    end if
  end do
  
  if (success == 0) then
    write(*,*) 'ERROR - substring ', trim(linetxt), ' not found. Stopping.'
    stop
  end if

end subroutine FindLine

!-------------------------------------------------------------------------------------------------!

subroutine FindBudgetLine(unitno, sp, ts, move_back)
  implicit none
  
  integer                  :: current_sp, current_ts, success
  integer, intent(in)      :: unitno, sp, ts
  character(200)           :: line
  logical, intent(in)      :: move_back
  
  success=0
  ! Loop over time step line
  do while (success == 0)
    call FindLine(10, 'VOLUMETRIC BUDGET', .true.)
    read(10,'(a200)') line
    read(line(57:61),*), current_ts
    read(line(80:85),*), current_sp
    if ((current_sp == sp).and.(current_ts == ts)) then
      success = 1
      if (move_back) backspace(unitno)
    else if (current_sp > sp) then
      write(*,*) 'ERROR - Passed target stress period and time step. Stopping.'
      stop
    end if
  end do
  
end subroutine FindBudgetLine