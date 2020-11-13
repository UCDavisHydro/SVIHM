 PROGRAM SWBM
  
! Scott valley soil water budget model (SWBM)
! 
! *****************************************************************************
! Input and Output is in Metric units:
! 
!                        [L]   =  meters
!                        [L^2] =  squared meters
!                        [L^3] =  cubic meters
!                        [T]   =  days
  
! subwn      : watershed name
!            : Scott=1, French=2, Etna=3, Patterson=4
!            : Kidder=5, Moffet=6, Mill=7, Shackleford =8, Scott Tailing=9
! landuse    : alfalfa 11, grain 12, pasture 2, ET/no_IRRIG 3, no_irrig/no_ET 4, water 6
! irr_type   : flood 1, sprinkler 2, centerpivot 3, 555= n* in the DWR categories and should be non irrigated, 999= Unknown irrig
! irrigation efficiency coefficients: irreff_flood, irreff_sprink, irreff_cp
! area       : Area of each polygon
! watersource: SW=1, GW=2, MIX=3, SUB=4, dry=5, 999 = unknown
! whc        : water holding capacity
! Water source unknown (999)-> Groundwater 
! Irrigation type unknown (999)-> Wheel Line
! Water source dry or sub or n* (555) with any irrig type -> et/noIRR

  USE define_poly
  USE irrigationmodule
  USE outputmodule
  
  IMPLICIT NONE

  INTEGER  :: nmonth, imonth, jday, i, im, ip, nrows, ncols
  INTEGER  :: dummy, nsegs, n_wel_param, num_daily_out, unit_num, num_MAR_fields, alf_irr_stop_mo, alf_irr_stop_day
  INTEGER, ALLOCATABLE, DIMENSION(:,:) :: zone_matrix, no_flow_matrix, output_zone_matrix, Discharge_Zone_Cells
  INTEGER, ALLOCATABLE, DIMENSION(:)   :: MAR_fields, ip_daily_out
  REAL   :: precip, Total_Ref_ET, MAR_vol
  REAL, ALLOCATABLE, DIMENSION(:)  :: drain_flow, max_MAR_field_rate, moisture_save, available_instream_flow_ratio
  REAL :: start, finish
  INTEGER, ALLOCATABLE, DIMENSION(:)  :: ndays
  CHARACTER(9) :: param_dummy
  CHARACTER(10)  :: SFR_Template, rch_scenario, flow_scenario, suffix
  CHARACTER(50), ALLOCATABLE, DIMENSION(:) :: daily_out_name
  INTEGER, DIMENSION(31) :: ET_Active
  LOGICAL :: MAR_active, ILR_active, instream_limits_active, daily_out_flag
  DOUBLE PRECISION :: eff_precip
 
  call cpu_time(start)
  open (unit=800, file='SWBM_log.rec')                         ! Open record file for screen output
  eff_precip = 0.
  Total_Ref_ET = 0.
  
  open(unit=10, file='general_inputs.txt', status='old')
  read(10, *) npoly, total_n_wells, nmonth, nrows, ncols, RD_Mult, SFR_Template
  read(10, *) rch_scenario, flow_scenario, alf_irr_stop_mo, alf_irr_stop_day
  write(*,*)'Unknown recharge scenario input in general_inputs.txt'
  write(800,*)'Unknown recharge scenario input in general_inputs.txt'

  if (trim(rch_scenario)=='basecase' .or. trim(rch_scenario)=='Basecase' .or. trim(rch_scenario)=='BASECASE') then            ! Set logicals for Recharge Scenario type
    MAR_active=  .FALSE.  
    ILR_active = .FALSE.
  else if(trim(rch_scenario)=='MAR' .or. trim(rch_scenario)=='mar') then
    MAR_active=  .TRUE. 
    ILR_active = .FALSE.
  else if (trim(rch_scenario)=='ILR' .or. trim(rch_scenario)=='ilr') then
    MAR_active=  .FALSE.
  	ILR_active = .TRUE.
  else if (trim(rch_scenario)=='MAR_ILR' .or. trim(rch_scenario)=='mar_ilr') then
    MAR_active=  .TRUE.       
    ILR_active = .TRUE.    
  else if (trim(rch_scenario).ne.'basecase' .or. trim(rch_scenario).ne.'Basecase' .or. trim(rch_scenario).ne.'BASECASE' &  ! Exit program if incorrect recharge scenario type
         .or. trim(rch_scenario).ne.'MAR' .or. trim(rch_scenario).ne.'mar' &
         .or. trim(rch_scenario).ne.'ILR' .or. trim(rch_scenario).ne.'ilr' &
         .or. trim(rch_scenario).ne.'MAR_ILR' .or. trim(rch_scenario).ne.'mar_ilr' ) then
    write(*,*)'Unknown recharge scenario input in general_inputs.txt'
    write(800,*)'Unknown recharge scenario input in general_inputs.txt'
    call EXIT
  end if

  if (trim(flow_scenario)=='basecase' .or. trim(flow_scenario)=='Basecase' .or. trim(flow_scenario)=='BASECASE') then 
    instream_limits_active = .FALSE.         ! Set logicals for Flow Scenario type 
  else if (trim(flow_scenario)=='flow_lims' .or. trim(flow_scenario)=='Flow_Lims' &
    .or. trim(flow_scenario)=='FLOW_LIMS')  then 
      instream_limits_active = .TRUE.         
  else if (trim(flow_scenario).ne.'basecase' .or. trim(flow_scenario).ne.'Basecase' &
    .or. trim(flow_scenario).ne.'BASECASE' .or. trim(flow_scenario).ne.'flow_lims' &
    .or. trim(flow_scenario).ne.'Flow_Lims' .or. trim(flow_scenario).ne.'FLOW_LIMS') then
      write(*,*)'Unknown flow scenario input in general_inputs.txt'
      write(800,*)'Unknown flow scenario input in general_inputs.txt'
      call EXIT
    end if
    
  write(*,'(2a19)')'Recharge Scenario: ',trim(rch_scenario)
  write(800,'(2a19)')'Recharge Scenario: ',trim(rch_scenario)
  write(*,'(2a15)')'Flow Scenario: ',trim(flow_scenario)
  write(800,'(2a15)')'Flow Scenario: ',trim(flow_scenario)
  write(*,'(A19,I6)') "Alfalfa end month: ", alf_irr_stop_mo
  write(800,'(A19,I6)') "Alfalfa end month: ", alf_irr_stop_mo
  write(*,'(A17,I6)') "Alfalfa end day: ", alf_irr_stop_day
  write(800,'(A17,I6)') "Alfalfa end day: ", alf_irr_stop_day
  SFR_Template = TRIM(SFR_Template)
  write(*,'(A27, A6)') 'SFR Template File Format = ',SFR_Template
  write(800,'(A27, A6)') 'SFR Template File Format = ',SFR_Template
  write(*,'(A5,I6,A15,I5,A8,I5,A23,F6.2)') "npoly", npoly, "total_n_wells", total_n_wells, "nmonth", nmonth,&
   "Root Depth Multiplier", RD_Mult
  write(800,'(A5,I6,A15,I5,A8,I5,A23,F6.2)') "npoly", npoly, "total_n_wells", total_n_wells, "nmonth", nmonth,&
   "Root Depth Multiplier", RD_Mult 
  close (10)
  open (unit=536, file="SVIHM_WEL_template.txt", status="old")     
  read(536,*) ! Read heading line into nothing
  read(536,*)param_dummy,n_wel_param  ! read in number of well parameters (for printing later)
  close(536)
  
  ALLOCATE(zone_matrix(nrows,ncols))
  ALLOCATE(no_flow_matrix(nrows,ncols))
  ALLOCATE(output_zone_matrix(nrows,ncols))
  ALLOCATE(Discharge_Zone_Cells(nrows,ncols))
  ALLOCATE(drain_flow(nmonth))
  ALLOCATE(ndays(nmonth))

  open(unit=211,file='Recharge_Zones_SVIHM.txt',status='old')      ! Read in MODFLOW recharge zone matrix
  read(211,*) zone_matrix
  open(unit=212,file='No_Flow_SVIHM.txt',status='old')       ! Read in MODFLOW no flow cell matrix
  read(212,*) no_flow_matrix  
  output_zone_matrix = zone_matrix * no_flow_matrix        ! Create Recharge Zone Matrix with zeros at no flow cells
  open(unit=213, file='SVIHM_SFR_template.txt', status='old')
  read(213,*) dummy,nsegs 
  open(unit=214,file='ET_Cells_DZ.txt',status='old')      ! Read in MODFLOW recharge zone matrix
  read(214,*) Discharge_Zone_Cells
  open(unit=888, file='stress_period_days.txt', status='old')      ! Read in vector with number of days in each stress period (month)
  read(888, *) ndays   

  close(211)
  close(212)
  close(213)
  close(214)
  close(888)
  
  call READ_KC_IRREFF                                ! Read in crop coefficients and irrigation efficiencies
  !call read_scenario_specs                           ! Read management actionsfor this scenario
  call readpoly(npoly, nrows, ncols, output_zone_matrix) ! Read in field info
  call read_well                                     ! Read in well info
  
  if (MAR_active) then
    open(unit=215,file='MAR_Fields.txt',status='old')      ! Read in MAR recharge matrix
    read(215,*) num_MAR_fields, MAR_vol
    ALLOCATE(MAR_fields(num_MAR_fields))                  ! Array of MAR field polygon IDs
    ALLOCATE(max_MAR_field_rate(num_MAR_fields))          ! Array of maximum infiltration rate for MAR fields (1/10th lowest SSURGO value)
    ALLOCATE(moisture_save(npoly))                        ! Array of soil-moisture needed to recalculate recharge for MAR fields
    moisture_save = 0.                                    ! Initialize array
    do i=1, num_MAR_fields
      read(215,*)MAR_fields(i), max_MAR_field_rate(i)
    end do
  end if

  if (instream_limits_active) then
    open(unit=216,file='instream_flow_available_ratio.txt') ! Read in available flow (inflow - CDFW recommended instream flows, m^3/month) (1 value for each month)
    ALLOCATE(available_instream_flow_ratio(nmonth)) ! vector instream_flow_lim has 1 entry for each month (stress period) in model record
  end if
  
  close(210) ! No unit = 210; delete?
  close(212)
  close(214) 
  
  open(unit=532,file='5daysdeficiency.dat')
!  open(unit=887,file='precip_m_LowBias_July2017.txt')         ! Missing data assumed to have value of zero
!  open(unit=887,file='precip_m_Replacement_July2017.txt')     ! Missing data replaced with value at other station (CAL and FJN)  
  open(unit=887,file='precip.txt', status = 'old')                    ! Missing data replaced with value obtained from regression using other stations
  open(unit=88,file='ref_et.txt', status = 'old')
  open(unit=79, file='kc_grain.txt', status = 'old')
  open(unit=80, file='kc_alfalfa.txt', status = 'old')
  open(unit=81, file='kc_pasture.txt', status = 'old')
  open(unit=888, file = 'stress_period_days.txt', status='old') ! WY month number, year month number, 3-letter name, number of days
  
  open(unit=60, file='subwatershed_area_m2.dat')
  write(60,'(" Month Scott French Etna Patterson Kidder Moffet Mill Shackleford Tailings")')
  open(unit=61, file='landuse_area_m2.dat')
  write(61,'(" Month Alfalfa Grain Pasture Et/noIrr noET/noIrr Water")')
  open(unit=62, file='landuse_area_m2_detailed.dat')
  write(62,'(" SP A_Irr A_n* A_SUB A_DRY G_Irr G_n* G_SUB G_DRY P_Irr P_n* P_SUB P_DRY ET/noIrr noET/noIrr Water Total")')
  open(unit=63, file='landuse_area_acres_detailed.dat')
  write(63,'(" SP A_Irr A_n* A_SUB A_DRY G_Irr G_n* G_SUB G_DRY P_Irr P_n* P_SUB P_DRY ET/noIrr noET/noIrr Water Total")')
  
  open(unit=599, file = 'daily_out.txt', status = 'old')
  read(599,*)num_daily_out, daily_out_flag
  ALLOCATE(ip_daily_out(num_daily_out))
  ALLOCATE(daily_out_name(num_daily_out))
  if (daily_out_flag) then
  	 do i=1, num_daily_out
  	 	 unit_num =  599 + i 
  	   read(599,*)ip_daily_out(i),daily_out_name(i)
  	   daily_out_name(i) = trim(daily_out_name(i)) // '_daily_out.dat'
  	   open(unit=unit_num, file=daily_out_name(i))
  	   write(unit_num,*)'field_id  precip_adj streamflow irrig  well rch moisture  ET',&
  	                    '  actualET  deficiency budget WC8 subwn landuse rotation'    
    end do
  end if
  
  open(unit=900, file='Recharge_Total.dat')   
  write(900,*)'Total_Recharge_m^3/day  Total_Recharge_m^3'            
  
  open(unit=84, file='SVIHM.rch', STATUS = 'REPLACE')
                                                      
  open(unit=91, file='monthly_gw_normalized.dat', STATUS = 'REPLACE')              
  write(91,*)'Monthly groundwater applied to each field normalized to the field area (m)'
  write(91,'(a14, 2119i5)')' Stress_Period',(/ (i, i = 1, npoly) /)
  open(unit=92, file='monthly_irrig_normalized.dat')
  write(92,*)'Monthly irrigation applied to each field normalized to the field area (m)'
  write(92,'(a14, 2119i5)')' Stress_Period',(/ (i, i = 1, npoly) /)    
  open(unit=93, file='monthly_pET_normalized.dat')
  write(93,*)'Monthly potential ET for each field normalized to the field area (m)'
  write(93,'(a14, 2119i5)')' Stress_Period',(/ (i, i = 1, npoly) /) 
  open(unit=94, file='monthly_recharge_normalized.dat')
  write(94,*)'Monthly groundwater recharge from each field normalized to the field area (m)'
  write(94,'(a14, 2119i5)')' Stress_Period',(/ (i, i = 1, npoly) /) 
  open(unit=95, file='monthly_storage_normalized.dat')
  write(95,*)'Storage at the end of the month for each field normalized to the field area (m)'
  write(95,'(a14, 2119i5)')' Stress_Period',(/ (i, i = 1, npoly) /) 
  open(unit=96, file='monthly_aET_normalized.dat')
  write(96,*)'Monthly actual ET for each field normalized to the field area (m)'
  write(96,'(a14, 2119i5)')' Stress_Period',(/ (i, i = 1, npoly) /) 
  open(unit=97, file='monthly_deficiency_normalized.dat')
  write(97,*)'Monthly groundwater recharge from each field normalized to the field area (m)'
  write(97,'(a14, 2119i5)')' Stress_Period',(/ (i, i = 1, npoly) /) 
  
  open(unit=200, file='monthly_gw_volume.dat', STATUS = 'REPLACE')              
  write(200,*)'Monthly groundwater volume applied to each field (m^3)'
  write(200,'(a14, 2119i5)')' Stress_Period',(/ (i, i = 1, npoly) /)
  open(unit=201, file='monthly_irrig_volume.dat')
  write(201,*)'Monthly irrigation volume applied to each field (m^3)'
  write(201,'(a14, 2119i5)')' Stress_Period',(/ (i, i = 1, npoly) /)    
  open(unit=202, file='monthly_pET_volume.dat')
  write(202,*)'Monthly potential ET for each field (m^3)'
  write(202,'(a14, 2119i5)')' Stress_Period',(/ (i, i = 1, npoly) /) 
  open(unit=203, file='monthly_recharge_volume.dat')
  write(203,*)'Monthly groundwater recharge from each field (m^3)'
  write(203,'(a14, 2119i5)')' Stress_Period',(/ (i, i = 1, npoly) /) 
  open(unit=204, file='monthly_storage_volume.dat')
  write(204,*)'Storage at the end of the month for each field (m^3)'
  write(204,'(a14, 2119i5)')' Stress_Period',(/ (i, i = 1, npoly) /) 
  open(unit=205, file='monthly_aET_volume.dat')
  write(205,*)'Monthly actual ET for each field (m^3)'
  write(205,'(a14, 2119i5)')' Stress_Period',(/ (i, i = 1, npoly) /) 
  open(unit=206, file='monthly_deficiency_volume.dat')
  write(206,*)'Monthly groundwater recharge from each field (m^3)'
  write(206,'(a14, 2119i5)')' Stress_Period',(/ (i, i = 1, npoly) /) 
                             
  open(unit=530, file='Monthly_Pumping_Volume_By_Well.dat')
  write(530,*)'Monthly Pumping Volume (m^3) by well'
  write(530,'(172i12)')single_well(:)%well_name
  open(unit=531, file='Monthly_Pumping_Rate_By_Well.dat')
  write(531,*)'Monthly Pumping Rate (m^3/day) by well'
  write(531,'(172i12)')single_well(:)%well_name
  open(unit=537, file='daily_pumping.dat')
  write(537, *)"Daily pumping volume (m^3) for each well"
  write(537,'(200i12)')single_well(:)%well_name
  open(unit=538, file='daily_average_RCH.dat')
  write(538, *)"Daily weighted averaged recharge caluclated in each well, to be averaged and used in stream depletion"
  open(unit=120, file='ET_Active_Days.dat')                       
  write(120,'("Number of Days ET is Active in each polyon")')    
  open(unit=66, file='streamflow_input.txt', status='old')
  read(66,*)                                                        ! Read Header into nothing
  
  open(unit=101, file='monthly_surfacewater_by_subw.dat')        
  open(unit=102, file='monthly_groundwater_by_subw.dat')      
  open(unit=103, file='monthly_irrigation_by_subw.dat')     
  open(unit=104, file='monthly_pET_by_subw.dat')     
  open(unit=105, file='monthly_aET_by_subw.dat')  
  open(unit=106, file='monthly_recharge_by_subw.dat')  
  open(unit=107, file='monthly_deficiency_by_subw.dat')
  open(unit=108, file='monthly_storage_by_subw.dat')   
  
  do i=101,108
    write(i,*)'Stress_Period  Scott  French  Etna  Patterson  Kidder  Moffet  Mill  Shackleford  Scott_Tailings'
  end do
                                                       
  open(unit=109, file='monthly_surfacewater_by_luse.dat')        
  open(unit=110, file='monthly_groundwater_by_luse.dat')      
  open(unit=111, file='monthly_irrigation_by_luse.dat')     
  open(unit=112, file='monthly_pET_by_luse.dat')     
  open(unit=113, file='monthly_aET_by_luse.dat')  
  open(unit=114, file='monthly_recharge_by_luse.dat')  
  open(unit=115, file='monthly_deficiency_by_luse.dat')
  open(unit=116, file='monthly_storage_by_luse.dat')  
  
  do i=109,116
    write(i,*)'Stress_Period  Alfalfa  Grain  Pasture  ET_NoIrr  NoET_NoIrr'
  end do
  
  open (unit=117, file='monthly_water_budget.dat')
  write(117,*)'Stress_Period Precip SW_Irr GW_Irr ET Recharge Storage'

   CALL EXECUTE_COMMAND_LINE('copy SVIHM_ETS_template.txt SVIHM.ets')
   CALL EXECUTE_COMMAND_LINE('copy SVIHM_SFR_template.txt SVIHM.sfr')
   CALL EXECUTE_COMMAND_LINE('copy SVIHM_WEL_template.txt SVIHM.wel')
   if (SFR_Template=='UCODE') then 
     CALL EXECUTE_COMMAND_LINE('copy SFR_UCODE_JTF.txt SVIHM_SFR.jtf')
   elseif (SFR_Template=='PEST') then
   	 CALL EXECUTE_COMMAND_LINE('copy SFR_PEST_TPL.txt SVIHM_SFR.tpl')
   else
     	write(*,*)'Invalid Template File Format Variable in general_inputs.txt'
     	write(800,*)'Invalid Template File Format Variable in general_inputs.txt'
     	CALL EXIT
   end if
   
   open (unit=220, file='Drains_m3day.txt')
   read(220,*)param_dummy     ! Read header comment line to dummy character string 
   poly%irr_flag = 0          ! Initialize irrigation flag array
   do im=1, nmonth            ! Loop over each month
     imonth=MOD(im,12)        ! Create repeating integers for months (Oct=1, Nov=2, ..., Aug=11, Sep=0)
     call zero_month                                 ! Zero out monthly accumulated volume
     if (imonth==1) call zero_year                             ! If October, Zero out yearly accumulated volume
     if (im==1) then 
       call do_rotation(im)                   ! populate initial poly%rotation values \
     else if (imonth==4 .and. im.ne.4) then
       call do_rotation(im)	                 ! Rotate alfalfa/grain in January, except for first year since rotation happened in October   
     end if                   
     call calc_area(im)                ! Calculate area for each month due to changing alfalfa/grain
     write(*,'(a15, i3,a13,i2,a18,i2)')'Stress Period: ',im,'   Month ID: ',imonth,'   Length (days): ', ndays(im)
     write(800,'(a15, i3,a13,i2,a18,i2)')'Stress Period: ',im,'   Month ID: ',imonth,'   Length (days): ', ndays(im)
     call zero_month                                 ! Zero out monthly accumulated volume
     if (imonth==1) then                             ! If October:
       call zero_year                                ! Zero out yearly accumulated volume
     endif    
     read(220,*) drain_flow(im)                       ! Read drain flow into array
     if(instream_limits_active) then                  
      read(216, *) available_instream_flow_ratio(im)        ! Read monthly available instream flow ratio (% of divertible trib flow) into array
     end if
     call read_streamflow(ndays(im), instream_limits_active, available_instream_flow_ratio(im))     ! Read in streamflow inputs

     do jday=1, ndays(im)                         ! Loop over days in each month
       if (jday==1) monthly%ET_active = 0            ! Set ET counter to 0 at the beginning of the month. Used for turning ET on and off in MODFLOW so it is not double counted.    
       daily%ET_active  = 0                                 ! Reset ET active counter
       daily%irrigation = 0.                                ! Reset daily irrigation value to zero
       daily%well       = 0.                                ! Reset daily pumping value to zero
       daily%effprecip  = 0.                                ! Reset daily effective precip value to zero
       daily%evapotrasp = 0.                                ! Reset daily ET value to zero
       daily%recharge   = 0.                                ! Reset daily recharge value to zero 
       read(88,*) REF_ET
       Total_Ref_ET = Total_Ref_ET + REF_ET                 
       read(79,*) kc_grain
       read (80,*) kc_alfalfa
       read(81,*)kc_pasture
       read(887,*) precip 
       if (precip .GE. (0.2*ref_et)) then
         eff_precip=precip
       else
         eff_precip=0.
       endif
		   do ip=1, npoly      
	       if (imonth==3 .and. jday==31 .and. ip==npoly) then ! If last day of the year, set irrigation flags and logical to zero
			     poly%irr_flag = 0         
			     irrigating = .false.           
		       print*, 'Irrigation Logical Reset'
		       write(800,*)'Irrigation Logical Reset'
		       call Update_Irr_Type(im)
		       print*, 'Irrigation Type Updated'    
           write(800,*)'Irrigation Type Updated'
		     end if
         if (ILR_active) then
           call IRRIGATION_ILR(ip, imonth, jday, eff_precip)
	       else
	         call IRRIGATION(ip, imonth, jday, eff_precip)
	       end if
	       call RECHARGE(ip,eff_precip,jday,imonth,moisture_save,MAR_active)
		     call deficiency_check(ip, imonth, jday)       
       enddo              ! End of polygon loop
       if (MAR_active) then 
         call MAR(imonth, num_MAR_fields, MAR_fields, max_MAR_field_rate, MAR_vol, eff_precip, jday, moisture_save)
       end if
       if (daily_out_flag) call daily_out(num_daily_out,ip_daily_out, eff_precip)              ! Print Daily Output for Selected Fields
       call pumping(ip, jday, total_n_wells, npoly)   ! Stream depletion subroutine
		   call monthly_SUM      ! add daily value to monthly total (e.g., monthly%irrigation = monthly%irrigation + daily%irrigation)
       call annual_SUM       ! add daily value to yearly total (e.g., yearly%irrigation = yearly%irrigation + daily%irrigation)
       if (jday==ndays(im)) then
         if (MAR_active) then
           call SFR_streamflow_w_MAR(ndays(im), imonth)   ! Convert remaining surface water to SFR inflows at end of the month
         else
           call SFR_streamflow(ndays(im), imonth)         ! Convert remaining surface water to SFR inflows at end of the month	
         end if
       end if
       if (MAR_active .and. jday==ndays(im)) then
         write(*,'(a13,f4.2,a6,f5.2,a13)')'MAR Volume = ',sum(monthly%MAR_vol)/1E6, ' Mm3 (', &
         sum(monthly%MAR_vol)*0.000408734569/ndays(im),' cfs per day)'
         write(*,*)
         write(800,'(a13,f4.2,a6,f5.2,a13)')'MAR Volume = ',sum(monthly%MAR_vol)/1E6, ' Mm3 (', &
         sum(monthly%MAR_vol)*0.000408734569/ndays(im),' cfs per day)'  
         write(800,*)
       end if
       enddo             ! End of day loop
       jday = jday -1 ! reset jday to number of days in month (incremented to ndays +1 in do loop above)
       call convert_length_to_volume
       call monthly_out_by_field(im)
       call monthly_pumping(im, jday, total_n_wells)
		   call ET_out_MODFLOW(im,imonth,ndays,nmonth, nrows,ncols,output_zone_matrix,Total_Ref_ET,Discharge_Zone_Cells,npoly)
		   Total_Ref_ET = 0.  ! Reset monthly Average ET
		   call recharge_out_MODFLOW(im,imonth,ndays, nmonth,nrows,ncols,output_zone_matrix)
       call monthly_volume_out		   
       call write_MODFLOW_SFR(im, nmonth, nsegs, SFR_Flows, drain_flow)
       call write_SFR_template (im, nmonth, nsegs, SFR_Flows, drain_flow, SFR_Template)   ! Write JTF file for UCODE 
       call write_MODFLOW_WEL(im,imonth,total_n_wells,n_wel_param)       
   enddo                  ! End of month loop
   close(84)
   close(60)
   close(61)		
   close(91)
   close(92)
   close(93)
   close(94)
   close(95)
   close(96)
   close(97)
   close(98)
   call cpu_time(finish)
   write(*,'(A7,F6.2,A8)')'Time = ',((finish-start)/60),' minutes'
   write(800,'(A7,F6.2,A8)')'Time = ',((finish-start)/60),' minutes'
   END PROGRAM SWBM

!*******************************************************************************************	
  SUBROUTINE deficiency_check(ip,imonth,iday)

  use define_poly

  integer :: ip, imonth, iday
  integer, save :: year = 0

  if ((poly(ip)%rotation == 12 .or. poly(ip)%landuse  == 2) .and. (0.5*poly(ip)%WC8.ne.0.)) then    ! if grain or pasture

    if ( (imonth == 4).and. (iday==1) ) then
       daily(ip)%daydef = 0
       if (ip==1) year = year + 1
    endif
    if (year == 0 ) return

    ! if ip has already had positive deficiency for > 5 days, do nothing
    if (daily(ip)%daydef < 0 ) return
    
    ! otherwise...
    
    if (daily(ip)%deficiency>0) then
        daily(ip)%daydef = daily(ip)%daydef + 1
    else
        daily(ip)%daydef = 0
    endif
    
    if (daily(ip)%daydef>4) then
      write(532,*) 'poly',ip,'date', iday, imonth, year
      daily(ip)%daydef = -100
    endif  
  
  elseif ((poly(ip)%rotation == 11 .or. poly(ip)%landuse  ==3) .and. (poly(ip)%WC8.ne.0.)) then  ! if alfalfa or native veg    

  if ( (imonth == 4).and. (iday==1) ) then
     daily(ip)%daydef = 0
     if (ip==1) year = year + 1
  endif
  if (year == 0 ) return

  ! if ip has already had positive deficiency for > 5 days, do nothing
  if (daily(ip)%daydef < 0 ) return

  ! otherwise...

  if (daily(ip)%deficiency>0) then
      daily(ip)%daydef = daily(ip)%daydef + 1
  else
      daily(ip)%daydef = 0
  endif
  
  if (daily(ip)%daydef>4) then
    write(532,*) 'poly',ip,'date', iday, imonth, year
    daily(ip)%daydef = -100
  endif  
  
  endif

  return
  END SUBROUTINE deficiency_check
!*******************************************************************************************	

  SUBROUTINE RECHARGE(ip,eff_precip,jday,imonth,moisture_save,MAR_active)

  use define_poly

  implicit none

  INTEGER :: ip, jday, imonth
  DOUBLE PRECISION:: RCH
  DOUBLE PRECISION :: eff_precip
  REAL, DIMENSION(npoly), INTENT(inout) :: moisture_save
  LOGICAL, INTENT(in)  :: MAR_active
  
  if (poly(ip)%landuse /= 6) then      ! No recharge for fields with water landuse
    daily(ip)%actualET=min(daily(ip)%evapotrasp,before(ip)%moisture+eff_precip+daily(ip)%irrigation) 
    daily(ip)%deficiency=daily(ip)%evapotrasp-daily(ip)%actualET
    if (daily(ip)%actualET > 0) daily(ip)%ET_active =  1   ! Set ET flag to 1 if ET is active that day
    if (poly(ip)%landuse==2) then                                    ! if pasture  
      rch = max(0., (before(ip)%moisture+eff_precip+daily(ip)%irrigation-daily(ip)%actualET)-0.5*poly(ip)%WC8 )
      daily(ip)%recharge = rch  
    else if ( poly(ip)%landuse==25 .or. poly(ip)%landuse==3 )  then  ! if alfalfa/grain/native veg 
      rch = max(0., (before(ip)%moisture+eff_precip+daily(ip)%irrigation-daily(ip)%actualET)-poly(ip)%WC8 )
      daily(ip)%recharge = rch 
    else if (poly(ip)%landuse==4) then  ! noET/NoIrr 
      daily(ip)%recharge = eff_precip
    endif
    daily(ip)%moisture=max(0.,before(ip)%moisture+eff_precip+daily(ip)%irrigation-daily(ip)%actualET-daily(ip)%recharge)
    call waterbudget(ip, eff_precip)
    if (MAR_active) moisture_save(ip) = before(ip)%moisture 
    before(ip)%moisture = daily(ip)%moisture
    daily(ip)%change_in_storage = eff_precip+daily(ip)%irrigation-daily(ip)%actualET-daily(ip)%recharge	
  end if  

  END SUBROUTINE RECHARGE
  


!****************************************************************************

  SUBROUTINE waterbudget(ip, eff_precip)

  use define_poly

  implicit none

  INTEGER :: ip
  DOUBLE PRECISION    :: eff_precip
!  DOUBLE PRECISION:: PRECIP, RCH

  
  daily(ip)%budget = daily(ip)%moisture-before(ip)%moisture+daily(ip)%actualET+daily(ip)%recharge- &
                     eff_precip-daily(ip)%irrigation       

  END SUBROUTINE 