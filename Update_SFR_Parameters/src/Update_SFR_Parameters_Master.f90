  PROGRAM Update_SFR_Parameters_Master
  
  ! Program for Updating SFR Parameters during paralled UCODE sensitivity analysis
  ! Need to have a file named 'Counter.dat' in the master folder (one above where this program is run) that is incremented for each parameter perturbation
  ! Need to have a file named 'SVIHM_Sensitivity.txt' that has the number of adjustible parameters, follwed by the name, initial value, and perturbed value for all adjustable parameters
  ! Need to have a JTF version of the SFR file.
  
  IMPLICIT NONE
  INTEGER :: NumParamSets, NumParams, ParamSetNumber, i, UCODE_Iteration, nline, io
  INTEGER :: StrIdxBedk1, StrIdxBedk2, StrIdxBedk3, StrIdxRough1, StrIdxRough2, StrIdxRough3
  INTEGER :: IdxBedk1, IdxBedk2, IdxBedk3, IdxRough1, IdxRough2, IdxRough3
  CHARACTER(10) :: rough1, rough2, rough3
  CHARACTER(12) :: bedk1, bedk2, bedk3
  CHARACTER(20) :: NumParamsText, ParamSetNumberText
  CHARACTER(100) :: CounterFile, SFR_Text
  CHARACTER(20), ALLOCATABLE, DIMENSION(:) :: ParamNames
  REAL, ALLOCATABLE, DIMENSION(:,:) :: ParamVals
  

  open(unit = 9, file = 'Counter.dat', status = 'new')
  write(9,'(i1)') 0
  open(unit = 10, file = 'SVIHM_Sensitivity.txt')
  read(10,*)NumParams                                                                       
  
  ALLOCATE(ParamNames(NumParams))
  ALLOCATE(ParamVals(2,NumParams))
    
  DO i=1,NumParams
  	read(10,*)ParamNames(i),ParamVals(1,i),ParamVals(2,i)
    IF (ParamNames(i) == 'bedk1') IdxBedk1 = i
    IF (ParamNames(i) == 'bedk2') IdxBedk2 = i
    IF (ParamNames(i) == 'bedk3') IdxBedk3 = i 
    IF (ParamNames(i) == 'rough1') IdxRough1 = i 
    IF (ParamNames(i) == 'rough2') IdxRough2 = i 
    IF (ParamNames(i) == 'rough3') IdxRough3 = i   	
  END DO
   
  open(unit = 102, file = 'SVIHM_SFR.jtf', status = 'old')
  open(unit = 103, file = 'SVIHM.sfr', status = 'replace')

  bedk1 = '@bedk1     @'
  bedk2 = '@bedk2     @'
  bedk3 = '@bedk3     @'
  rough1 = '@rough1  @'
  rough2 = '@rough2  @'
  rough3 = '@rough3  @'
  nline = 0
  
  DO
    READ(102,'(a)',iostat=io)SFR_Text
    IF (io/=0) EXIT
    nline = nline + 1
    StrIdxBedk1 = INDEX(SFR_Text,bedk1)                                                    ! Index location with string
    StrIdxBedk2 = INDEX(SFR_Text,bedk2)                                                    ! Index location with string
    StrIdxBedk3 = INDEX(SFR_Text,bedk3)                                                    ! Index location with string
    StrIdxRough1 = INDEX(SFR_Text,rough1)                                                  ! Index location with string
    StrIdxRough2 = INDEX(SFR_Text,rough2)                                                  ! Index location with string
    StrIdxRough3 = INDEX(SFR_Text,rough3)                                                  ! Index location with string

    IF (nline .ne. 1) THEN
        IF (StrIdxBedk1 .NE. 0) THEN                                                       ! Write SFR Parameter Values (Increase Bedk1)
          write(103,'(a,F10.5)')SFR_Text(1:StrIdxBedk1-3),ParamVals(1,IdxBedk1)         ! Write SFR Parameter Values (Increase Bedk1)
        ELSE IF (StrIdxBedk2 .NE. 0) THEN                                                  ! Write SFR Parameter Values (Increase Bedk1)
          write(103,'(a,F10.5)')SFR_Text(1:StrIdxBedk2-3),ParamVals(1,IdxBedk2)              ! Write SFR Parameter Values (Increase Bedk1)
        ELSE IF (StrIdxBedk3 .NE. 0) THEN                                                  ! Write SFR Parameter Values (Increase Bedk1)
          write(103,'(a,F10.5)')SFR_Text(1:StrIdxBedk3-3),ParamVals(1,IdxBedk3)              ! Write SFR Parameter Values (Increase Bedk1)
        ELSE IF (StrIdxRough1 .NE. 0) THEN                                                 ! Write SFR Parameter Values (Increase Bedk1)
          write(103,'(a,F10.5)')SFR_Text(1:StrIdxRough1-3),ParamVals(1,IdxRough1)            ! Write SFR Parameter Values (Increase Bedk1)
        ELSE IF (StrIdxRough2 .NE. 0) THEN                                                 ! Write SFR Parameter Values (Increase Bedk1)
          write(103,'(a,F10.5)')SFR_Text(1:StrIdxRough2-3),ParamVals(1,IdxRough2)            ! Write SFR Parameter Values (Increase Bedk1)
        ELSE IF (StrIdxRough3 .NE. 0) THEN                                                 ! Write SFR Parameter Values (Increase Bedk1)
          write(103,'(a,F10.5)')SFR_Text(1:StrIdxRough3-3),ParamVals(1,IdxRough3)            ! Write SFR Parameter Values (Increase Bedk1)
        ELSE                                                                               ! Write SFR Parameter Values (Increase Bedk1)
          write(103,'(a)')SFR_Text                                                         ! Write SFR Parameter Values (Increase Bedk1)
        END IF
      END IF
      
  END DO
  
  END PROGRAM Update_SFR_Parameters_Master
  
  