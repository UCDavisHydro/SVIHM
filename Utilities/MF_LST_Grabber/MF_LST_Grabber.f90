program MF_LST_Grabber
  implicit none
!-------------------------------------------------------------------------------------------------!
! MF_LST_Grabber - MODFLOW Listing File Budget Grabber
! Author: Leland Scantlebury, lscantle@ucdavis.edu
! Date: 10/25/2024
!
! Reads in MODFLOW listing (LST) file and obtains the IN, OUT, NET for the given budget entry at
! every available time step
!
! Command line arguments:
!  - listing file name (e.g., SVIHM.lst)
!  - Budget entry to report (e.g., STREAM LEAKAGE)
!
! Output: 
!-------------------------------------------------------------------------------------------------!
  integer              :: i, j, nsp, sp, ts, ierr, ls(10), rs(10), nwords
  integer,allocatable  :: nstp(:)
  real*8               :: splen, mult, item_in, item_out, item_net
  character(200)       :: line
  character(60)        :: mf_lst, out_file, budget_item, budget_item_print, header(3)
  character(2)         :: ss_flag
  logical              :: zero_net_negatives
  
  ! Get LST from command line
  call getarg(1,mf_lst)
  if (trim(mf_lst)=='') then
    write(*,*) 'ERROR - No LST file passed. Stopping.'
    stop
  end if

  ! Get zero negative entries option
  zero_net_negatives = 0
  if (COMMAND_ARGUMENT_COUNT() > 1) then
    call getarg(2, line)
    if (trim(line)=="1") zero_net_negatives = 1
  else
    write(*,*) 'ERROR - No zero negative or budget item passed. Stopping.'
    stop
  end if
  
  ! Get Budget entry, supporting multiple words
  if (COMMAND_ARGUMENT_COUNT() > 2) then
    call getarg(3, budget_item)
    call getarg(3, budget_item_print)
    nwords = 1
    do i = 4, COMMAND_ARGUMENT_COUNT()
      call getarg(i, line)
      budget_item       = trim(budget_item) // ' ' // adjustl(line)
      budget_item_print = trim(budget_item_print) // '_' // adjustl(line)
      nwords = nwords + 1
    end do
    budget_item       = trim(budget_item)  ! Remove trailing spaces
    budget_item_print = trim(budget_item_print)
    header(1)         = ' ' // trim(budget_item_print) // "_IN"
    header(2)         = trim(budget_item_print) // "_OUT"
    header(3)         = trim(budget_item_print) // "_NET"
  else
    write(*,*) 'ERROR - No budget item passed. Stopping.'
    stop
  end if
  
  write(*,*) '*******************************************'
  write(*,*) '*    MF_LST_Grabber - an SVIHM Utility    *'
  write(*,*) '*    v0.0.1                               *'
  write(*,*) '*******************************************'
  write(*,*) 'Status:'
  
  ! Intel-Fortran specific way of writing to same command line line
  open(6, carriagecontrol='fortran')
  
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
  
  ! Open output file
  write(out_file,'(3a)') 'Budget_Grab_', trim(budget_item_print), '.out'
  write(6,'("+",2x,2a)') 'Opening MF_LST_Grabber Output File: ', trim(out_file)
  open(20, file=trim(out_file), status='replace', action='write', iostat=ierr)
  write(20,'(2a6,3a20)') 'SP', 'TS', header(1:3)
  
  ! Read through LST file finding time step budget entries
  write(6,'("+",a100)'), ' '  ! Clears line
  do i=1, nsp
    do j=1, nstp(i)
      write(6,'("+",2x,2(a,i4))') 'Processing SP: ', i, ' TS: ', j
      call FindBudgetLine(10, i, j, .false.)
      ! Budget is IN
      call FindLine(10, budget_item, .true.)
      ! Read in time step value (not cumulative)
      read(10,'(a200)') line
      call multisplit(ierr,4+nwords*2,ls,rs,line)
      read(line(ls(4+nwords*2):rs(4+nwords*2)), *) item_in
      ! Next is the time step OUT
      call FindLine(10, budget_item, .true.)
      ! Read in time step value (not cumulative)
      read(10,'(a200)') line
      call multisplit(ierr,4+nwords*2,ls,rs,line)
      read(line(ls(4+nwords*2):rs(4+nwords*2)), *) item_out
      ! Write to output file
      item_net = item_out - item_in 
      if (zero_net_negatives.and.item_net<0.0d0) item_net = 0.0d0
      write(20,'(2i6,3f20.5)') i, j, item_in, item_out, item_net
    end do
  end do
  
  write(*,*) 'End of Stress Periods. Closing Files... '
  close(10)
  close(20)
  write(*,*) 'Done.'
  
  close(6)
  
end program MF_LST_Grabber

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
    if (index(line,trim(linetxt)) > 0) then
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
  
  integer                  :: current_sp, current_ts, success, ierr, ls(15), rs(15)
  integer, intent(in)      :: unitno, sp, ts
  character(200)           :: line
  logical, intent(in)      :: move_back
  
  success=0
  ! Loop over time step line
  do while (success == 0)
    call FindLine(10, 'VOLUMETRIC BUDGET', .true.)
    read(10,'(a200)') line
    call multisplit(ierr, 15, ls, rs, line)
    read(line(ls(11):rs(11)),*), current_ts
    ! Assume SP is last substring (index is inconsistent between MF versions)
    read(line(maxval(ls):maxval(rs)),*), current_sp
    if ((current_sp == sp).and.(current_ts == ts)) then
      success = 1
      if (move_back) backspace(unitno)
    else if (current_sp > sp) then
      write(*,*) 'ERROR - Passed target stress period and time step. Stopping.'
      stop
    end if
  end do
  
end subroutine FindBudgetLine

!-----------------------------------------------------------------------------!

    SUBROUTINE multisplit(IFAIL,NUM,LW,RW,CLINE)

! -- Subroutine multisplit splits a string into blank-delimited fragments.

! -- Subroutine arguments are as follows:-
!       ifail:    returned as non-zero in case of failure
!       num:      number of substrings to find in line (max?)
!       lw:       array of substring start indeces
!       rw:       array of substring end indeces
!       cline:    character string

! -- Author:-
!       John Doherty

       INTEGER IFAIL,NW,NBLC,J,I
       INTEGER NUM,NBLNK
       INTEGER LW(NUM),RW(NUM)
       CHARACTER*(*) CLINE
       IFAIL=0
       NW=0
       NBLC=LEN_TRIM(CLINE)
       IF((NBLC.NE.0).AND.(INDEX(CLINE,CHAR(9)).NE.0)) THEN
         CALL TABREM(CLINE)
         NBLC=LEN_TRIM(CLINE)
       ENDIF
       IF(NBLC.EQ.0) THEN
         IFAIL=-1
         RETURN
       END IF
       J=0
5      IF(NW.EQ.NUM) RETURN
       DO 10 I=J+1,NBLC
         IF((CLINE(I:I).NE.' ').AND.(CLINE(I:I).NE.',').AND.&
         (ICHAR(CLINE(I:I)).NE.9)) GO TO 20
10     CONTINUE
       IFAIL=1
       RETURN
20     NW=NW+1
       LW(NW)=I
       DO 30 I=LW(NW)+1,NBLC
         IF((CLINE(I:I).EQ.' ').OR.(CLINE(I:I).EQ.',').OR.&
         (ICHAR(CLINE(I:I)).EQ.9)) GO TO 40
30     CONTINUE
       RW(NW)=NBLC
       IF(NW.LT.NUM) IFAIL=1
       RETURN
40     RW(NW)=I-1
       J=RW(NW)
       GO TO 5

    END subroutine multisplit

!-----------------------------------------------------------------------------!
    
subroutine TABREM(CLINE)

! -- Subroutine TABREM removes tabs from a string.

! -- Subroutine arguments are as follows:-
!       cline:    character string


       INTEGER I
       CHARACTER*(*) CLINE

       DO 10 I=1,LEN(CLINE)
10     IF(ICHAR(CLINE(I:I)).EQ.9) CLINE(I:I)=' '

       RETURN
  end subroutine tabrem

!-----------------------------------------------------------------------------!