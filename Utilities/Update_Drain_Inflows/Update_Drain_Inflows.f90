program Update_Drain_Inflows
  implicit none
!-------------------------------------------------------------------------------------------------!
!
!-------------------------------------------------------------------------------------------------!
  integer              :: i, nsp, sp, ts, ierr, ipos, ls(10), rs(10)
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
    call multisplit(ierr,7,ls,rs,line)  ! Thanks Dr. Doherty
    read(line(ls(3):rs(3)), *) drn_out
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