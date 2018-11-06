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

  INTEGER  :: nmonth, numdays, imonth,jday,i,im,ip, nrows, ncols, dummy, nsegs, n_wel_param, num_daily_out, unit_num
  INTEGER  :: ip_AG_SW_Flood, ip_AG_SW_WL, ip_AG_SW_CP, ip_AG_GW_Flood, ip_AG_GW_WL, ip_AG_GW_CP                                    ! Field IDs for Daily Output
  INTEGER  :: ip_AG_MIX_Flood, ip_AG_MIX_WL, ip_AG_MIX_CP, ip_AG_SUB_DRY, ip_Pasture_SW_Flood, ip_Pasture_SW_WL                     ! Field IDs for Daily Output
  INTEGER  :: ip_Pasture_SW_CP, ip_Pasture_GW_Flood, ip_Pasture_GW_WL, ip_Pasture_GW_CP, ip_Pasture_MIX_Flood, ip_Pasture_MIX_WL    ! Field IDs for Daily Output
  INTEGER  :: ip_Pasture_MIX_CP, ip_Pasture_SUB_DRY, ip_ETnoIrr_Low_WC8, ip_ETnoIrr_High_WC8, ip_noETnoIrr, ip_Water_Landuse        ! Field IDs for Daily Output
  INTEGER  :: ip_SW_Flood_DZ
  INTEGER  :: num_MAR_fields
  INTEGER, ALLOCATABLE, DIMENSION(:,:) :: zone_matrix, no_flow_matrix, output_zone_matrix, Discharge_Zone_Cells
  REAL   :: precip, Total_Ref_ET, max_MAR_total_vol
  INTEGER, ALLOCATABLE, DIMENSION(:)  :: MAR_fields, ip_daily_out
  REAL, ALLOCATABLE, DIMENSION(:)  :: drain_flow, max_MAR_field_rate, moisture_save
  REAL, ALLOCATABLE, DIMENSION(:,:)  :: MAR_Matrix
  REAL :: start, finish
  INTEGER, DIMENSION(0:11)  :: nday
  CHARACTER(9) :: param_dummy
  CHARACTER(10)  :: SFR_Template
  CHARACTER(50), ALLOCATABLE, DIMENSION(:) :: daily_out_name
  INTEGER, DIMENSION(31) :: ET_Active
  LOGICAL :: daily_out_flag
! We defined precip adjusted after discussion with Rick Snyder and after looking at the FAO 56 document
! Precip_adjusted = Precip, if Precip > 0.2 * ref_et
! Precip_adjusted = 0, if Precip <= 0.2 * ref_et
! Small but significant difference! On a wet day, all precip counts. When it's a sprinkle, it doesn't count at all.
! Precip_adjusted will be used instead than effprecip
   call cpu_time(start)
   DATA nday / 30,31,30,31,31,28,31,30,31,30,31,31 /            ! The months are shifted by one because the first index location is zero due to use of MOD function
   open (unit=800, file='SWBM_log_MAR.rec')                         ! Open record file for screen output
 !  nmonth = 12*21
   precip_adjusted = 0.
   Total_Ref_ET = 0.
   
   open(unit=10, file='general_inputs.txt', status='old')
   read(10, *) npoly, total_n_wells, nmonth, nrows, ncols, RD_Mult, SFR_Template
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
   
   call READ_KC_IRREFF                        ! Read in crop coefficients and irrigation efficiencies
   call readpoly                              ! Read in field info
   call read_well                             ! Read in well info
   
   ALLOCATE(zone_matrix(nrows,ncols))
   ALLOCATE(no_flow_matrix(nrows,ncols))
   ALLOCATE(output_zone_matrix(nrows,ncols))
   ALLOCATE(Discharge_Zone_Cells(nrows,ncols))
   ALLOCATE(MAR_Matrix(nrows,ncols))
   ALLOCATE(drain_flow(nmonth))
   
   
   open(unit=213, file='SVIHM_SFR_template.txt', status='old')
   read(213,*) dummy,nsegs
   close(213)

   open (unit=210,file='Recharge_Zones_SVIHM.txt',status='old')      ! Read in MODFLOW recharge zone matrix
   read(210,*) zone_matrix
   open (unit=212,file='No_Flow_SVIHM.txt',status='old')       ! Read in MODFLOW no flow cell matrix
   read(212,*) no_flow_matrix   
   open (unit=214,file='ET_Cells_DZ.txt',status='old')      ! Read in MODFLOW recharge zone matrix
   read(214,*) Discharge_Zone_Cells
   ! open(unit=215,file='MAR_Array.txt',status='old')      ! Read in MAR recharge matrix
   ! read(215,*) MAR_Matrix
   open(unit=216,file='MAR_Fields.txt',status='old')      ! Read in MAR recharge matrix
   read(216,*) num_MAR_fields
   ALLOCATE(MAR_fields(num_MAR_fields))                  ! Array of MAR field polygon IDs
   ALLOCATE(max_MAR_field_rate(num_MAR_fields))          ! Array of maximum infiltration rate for MAR fields (1/10th lowest SSURGO value)
   ALLOCATE(moisture_save(npoly))                        ! Array of soil-moisture needed to recalculate recharge for MAR fields
   moisture_save = 0.                                    ! Initialize array
   do i=1, num_MAR_fields
     read(216,*)MAR_fields(i), max_MAR_field_rate(i)
   end do
   max_MAR_total_vol = 42.*2446.57554277                           ! Maximum MAR volume per day in m^3 (42 cfs over 1 day)
   close(210)
   close(212)
   close(214)
   ! close(215)
   close(216)
   
   output_zone_matrix = zone_matrix * no_flow_matrix        ! Create Recharge Zone Matrix with zeros at no flow cells
!    write(888,'(210i5)') rch_zone_matrix
!    write(889,'(210i2)') no_flow_matrix   
!    write(890,'(210i5)') output_zone_matrix   


   open(unit=532,file='5daysdeficiency_MAR.dat')
!   open(unit=887,file='precip_m_LowBias_July2017.txt')         ! Missing data assumed to have value of zero
!   open(unit=887,file='precip_m_Replacement_July2017.txt')     ! Missing data replaced with value at other station (CAL and FJN)  
   open(unit=887,file='precip.txt', status = 'old')                    ! Missing data replaced with value obtained from regression using other stations
   open(unit=88,file='ref_et.txt', status = 'old')
   open(unit=79, file='kc_grain.txt', status = 'old')
   open(unit=80, file='kc_alfalfa.txt', status = 'old')
   open(unit=81, file='kc_pasture.txt', status = 'old')
   
   open(unit=60, file='subwatershed_area_m2_MAR.dat')
   write(60,'(" Month Scott French Etna Patterson Kidder Moffet Mill Shackleford Tailings")')
   open(unit=61, file='landuse_area_m2_MAR.dat')
   write(61,'(" Month Alfalfa Grain Pasture Et/noIrr noET/noIrr Water")')
   open(unit=62, file='landuse_area_m2_detailed_MAR.dat')
   write(62,'(" SP A_Irr A_n* A_SUB A_DRY G_Irr G_n* G_SUB G_DRY P_Irr P_n* P_SUB P_DRY ET/noIrr noET/noIrr Water Total")')
   open(unit=63, file='landuse_area_acres_detailed_MAR.dat')
   write(63,'(" SP A_Irr A_n* A_SUB A_DRY G_Irr G_n* G_SUB G_DRY P_Irr P_n* P_SUB P_DRY ET/noIrr noET/noIrr Water Total")')
   
   open(unit=599, file = 'daily_out.txt', status = 'old')
   read(599,*)num_daily_out, daily_out_flag
   ALLOCATE(ip_daily_out(num_daily_out))
   ALLOCATE(daily_out_name(num_daily_out))
   if (daily_out_flag) then
   	 do i=1, num_daily_out
   	 	 unit_num =  599 + i 
   	   read(599,*)ip_daily_out(i),daily_out_name(i)
   	   daily_out_name(i) = trim(daily_out_name(i)) // '_daily_out_MAR.dat'
   	   open(unit=unit_num, file=daily_out_name(i))
   	   write(unit_num,*)'field_id  precip_adj streamflow irrig  well rch moisture  ET',&
   	                    '  actualET  deficiency budget WC8 subwn landuse rotation'    
     end do
   end if
   
   open(unit=900, file='Recharge_Total_MAR.dat')   
   write(900,'(a24)')'Total Recharge (m^3/day)'                          
                                                       
   open(unit=91, file='well_out_MAR.dat')              
   write(91,'("#WELL: amount of monthly water pumped in each polygon")')
   open(unit=530, file='Monthly_Pumping_Volume_By_Well_MAR.dat')
   write(530,*)'Monthly Pumping Volume (m^3) by well'
   write(530,'(172i12)')single_well(:)%well_name
   open(unit=531, file='Monthly_Pumping_Rate_By_Well_MAR.dat')
   write(531,*)'Monthly Pumping Rate (m^3/day) by well'
   write(531,'(172i12)')single_well(:)%well_name
   open(unit=537, file='daily_pumping_MAR.dat')
   write(537, *)"Daily pumping volume (m^3) for each well"
   write(537,'(200i12)')single_well(:)%well_name
   open(unit=538, file='daily_average_RCH_MAR.dat')
   write(538, *)"Daily weighted averaged recharge caluclated in each well, to be averaged and used in stream depletion"
   open(unit=92, file='irrig_out_MAR.dat')
   write(92,'("#IRRIGATION: amount of monthly irrigation in each polygon")')   
   open(unit=93, file='evapotrasp_out_MAR.dat')
   write(93,'("#Evapotrasp: amount of monthly evapotraspiration in each polygon")')
   open(unit=94, file='recharge_out_MAR.dat')
   write(94,'("#Recharge: amount of monthly recharge in each polygon")')
   open(unit=84, file='SVIHM.rch')
   open(unit=95, file='moisture_out_MAR.dat')
   write(95,'("#Moisture: amount of monthly moisture in each polygon")')
   open(unit=96, file='actualET_out_MAR.dat')
   write(96,'("#ActualET: amount of monthly actualET in each polygon")')
   open(unit=97, file='deficiency_out_MAR.dat')
   write(97,'("#Deficiency: amount of monthly deficiency (ET-actualET) in each polygon")')
   open(unit=98, file='well_out_flow_MAR.dat')
   write(98,'("#WELL: monthly FLOW pumped in each polygon")')
   open(unit=120, file='ET_Active_Days_MAR.dat')                       
   write(120,'("Number of Days ET is Active in each polyon")')    
   open(unit=66, file='streamflow_input.txt', status='old')
   read(66,*)                                                        ! Read Header into nothing

   open (unit=102, file='monthly_well_by_subw_MAR.dat')
   open (unit=103, file='monthly_irrig_by_subw_MAR.dat')
   open (unit=104, file='monthly_evapo_by_subw_MAR.dat')
   open (unit=105, file='monthly_recharge_by_subw_MAR.dat')
   open (unit=106, file='monthly_well_by_luse_MAR.dat')
   open (unit=107, file='monthly_irrig_by_luse_MAR.dat')
   open (unit=108, file='monthly_evapo_by_luse_MAR.dat')
   open (unit=109, file='monthly_recharge_by_luse_MAR.dat')
   open (unit=110, file='monthly_deficiency_by_subw_MAR.dat')
   open (unit=111, file='monthly_deficiency_by_luse_MAR.dat')
   open (unit=112, file='monthly_actualET_by_luse_MAR.dat')
   open (unit=113, file='monthly_actualET_by_subw_MAR.dat')
   open (unit=114, file='monthly_moisture_by_subw_MAR.dat') 
   open (unit=115, file='monthly_moisture_by_luse_MAR.dat')
   
   open (unit=116, file='monthly_water_budget_MAR.dat')
   write(116,*)'Stress_Period Precip SW_Irr GW_Irr ET Recharge Storage'
   
!   open (unit=202, file='yearly_well_by_subw_MAR.dat')
!   open (unit=203, file='yearly_irrig_by_subw_MAR.dat')
!   open (unit=204, file='yearly_evapo_by_subw_MAR.dat')
!   open (unit=205, file='yearly_recharge_by_subw_MAR.dat')
!   open (unit=206, file='yearly_well_by_luse_MAR.dat')
!   open (unit=207, file='yearly_irrig_by_luse_MAR.dat')
!   open (unit=208, file='yearly_evapo_by_luse_MAR.dat')
!   open (unit=209, file='yearly_recharge_by_luse_MAR.dat')

   CALL EXECUTE_COMMAND_LINE('copy SVIHM_ETS_template.txt SVIHM.ets')
   CALL EXECUTE_COMMAND_LINE('copy SVIHM_SFR_template.txt SVIHM.sfr')
   CALL EXECUTE_COMMAND_LINE('copy SVIHM_WEL_template.txt SVIHM.wel')
   if (SFR_Template=='UCODE') then 
     CALL EXECUTE_COMMAND_LINE('copy SFR_UCODE_JTF.txt SVIHM_SFR.jtf')
   elseif (SFR_Template=='PEST') then
   	 CALL EXECUTE_COMMAND_LINE('copy SFR_PEST_TPL.txt SVIHM_SFR.tpl')
   else
     	print *, 'Invalid Template File Format Variable in general_inputs.txt'
     	CALL EXIT
   end if
   
   open (unit=220, file='Drains_m3day.txt')
   read(220,*)param_dummy     ! Read header comment line to dummy character string 
   poly%irr_flag = 0          ! Initialize irrigation flag array
   do im=1, nmonth            ! Loop over each month
     imonth=MOD(im,12)        ! Create repeating integers for months (Oct=1, Nov=2, ..., Aug=11, Sep=0)
     numdays = nday(imonth)   ! Number of days in the current month
     if (im==1)then 
      call do_rotation(im)                   ! populate initial poly%rotation values 
     else if (imonth==4 .and. im.ne.4) then
       call do_rotation(im)	                 ! Rotate alfalfa/grain in January, except for first year since rotation happened in October   
     end if                   
     
     call calc_area(im)                ! Calculate area for each month due to changing alfalfa/grain
     call read_streamflow(numdays)     ! Read in streamflow inputs
     
     write(*,'(a15, i3,a13,i2,a18,i2)')'Stress Period: ',im,'   Month ID: ',imonth,'   Length (days): ', nday(imonth)
     write(800,'(a15, i3,a13,i2,a18,i2)')'Stress Period: ',im,'   Month ID: ',imonth,'   Length (days): ', nday(imonth)
     call zero_month                     ! Zero out monthly accumulated volume
     if (imonth==1) then                 ! If October:
       call zero_year                    ! Zero out yearly accumulated volume
       call open_yearly_file             ! Open new file for yearly output
     endif    
     read(220,*)drain_flow(im)     ! Read drain flow into array         
     
     do jday=1, nday(imonth)           ! Loop over days in each month
       if (jday==1) monthly%ET_active = 0            ! Set ET counter to 0 at the beginning of the month. Used for turning ET on and off in MODFLOW so it is not double counted.    
       daily%ET_active = 0
       read(88,*) REF_ET
       Total_Ref_ET = Total_Ref_ET + REF_ET                 
       read(79,*) kc_grain
       read (80,*)kc_alfalfa
       read(81,*)kc_pasture
       read(887,*) precip
       if (precip .GE. (0.2*ref_et)) then
         precip_adjusted=precip
       else
         precip_adjusted=0.
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
         call IRRIGATION(ip, imonth, jday, precip_adjusted)
		     call recharge(ip,precip_adjusted,jday,imonth,moisture_save)
         call deficiency_check(ip, imonth, jday)       
       enddo              ! End of polygon loop
       call MAR(imonth, num_MAR_fields, MAR_fields, max_MAR_field_rate, max_MAR_total_vol,&
                precip_adjusted,jday,moisture_save)
       if (daily_out_flag) call daily_out(num_daily_out,ip_daily_out)              ! Print Daily Output for Selected Fields
       call pumping(ip, jday, total_n_wells, npoly)   ! Stream depletion subroutine
		   call monthly_SUM      ! add daily value to monthly total (e.g., monthly%irrigation = monthly%irrigation + daily%irrigation)
       call annual_SUM       ! add daily value to yearly total (e.g., yearly%irrigation = yearly%irrigation + daily%irrigation)
       if (jday==numdays) call SFR_streamflow_w_MAR(numdays, imonth)   ! Convert remaining surface water to SFR inflows at end of the month
       if (jday==numdays) then
         write(*,'(a13,f4.2,a6,f5.2,a13)')'MAR Volume = ',sum(monthly%MAR_vol)/1E6, ' Mm3 (', &
         sum(monthly%MAR_vol)*0.000408734569/numdays,' cfs per day)'
         write(*,*)
         write(800,'(a13,f4.2,a6,f5.2,a13)')'MAR Volume = ',sum(monthly%MAR_vol)/1E6, ' Mm3 (', &
         sum(monthly%MAR_vol)*0.000408734569/numdays,' cfs per day)'  
         write(800,*)
       end if
       enddo             ! End of day loop
     jday = jday -1 ! reset jday to number of days in month (incremented to ndays +1 in do loop above)
     
     call monthly_out_length(im)
     call monthly_pumping(im, jday, total_n_wells)
		 call ET_out_MODFLOW(im,imonth,nday,nrows,ncols,output_zone_matrix,Total_Ref_ET,Discharge_Zone_Cells,npoly)
		 Total_Ref_ET = 0.  ! Reset monthly Average ET
     call recharge_out (im)
     call recharge_out_MODFLOW(im,imonth,nday,nrows,ncols,output_zone_matrix)
     call monthly_volume_out		   
     call write_MODFLOW_SFR(im, nmonth, nsegs, SFR_Flows, drain_flow)
     call write_SFR_template (im, nmonth, nsegs, SFR_Flows, drain_flow, SFR_Template)   ! Write JTF file for UCODE 
     call write_MODFLOW_WEL(im,total_n_wells,n_wel_param)       
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
   
   close(800)
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

  SUBROUTINE RECHARGE(ip,precip_adjusted,jday,imonth,moisture_save)

  use define_poly

  implicit none

  INTEGER :: ip, jday, imonth
  DOUBLE PRECISION:: RCH
  DOUBLE PRECISION :: precip_adjusted
  real, dimension(npoly), intent(inout) :: moisture_save
  
   
  daily(ip)%actualET=min(daily(ip)%evapotrasp,before(ip)%moisture+precip_adjusted+daily(ip)%irrigation) 
  daily(ip)%deficiency=daily(ip)%evapotrasp-daily(ip)%actualET
  if (daily(ip)%actualET > 0) daily(ip)%ET_active =  1   ! Set ET flag to 1 if ET is active that day

  if (poly(ip)%landuse==2) then                                    ! if pasture  
    rch = max(0., (before(ip)%moisture+precip_adjusted+daily(ip)%irrigation-daily(ip)%actualET)-0.5*poly(ip)%WC8 )
    daily(ip)%recharge = rch  

  else if ( poly(ip)%landuse==25 .or. poly(ip)%landuse==3 )  then  ! if alfalfa/grain/native veg 
    rch = max(0., (before(ip)%moisture+precip_adjusted+daily(ip)%irrigation-daily(ip)%actualET)-poly(ip)%WC8 )
    daily(ip)%recharge = rch 
  else if (poly(ip)%landuse==4) then  ! noET/NoIrr 
    daily(ip)%recharge = precip_adjusted
  endif
  daily(ip)%moisture=max(0.,before(ip)%moisture+precip_adjusted+daily(ip)%irrigation-daily(ip)%actualET-daily(ip)%recharge)
  call waterbudget(ip, precip_adjusted)
  moisture_save(ip) = before(ip)%moisture 
  before(ip)%moisture = daily(ip)%moisture
  daily(ip)%change_in_storage = precip_adjusted+daily(ip)%irrigation-daily(ip)%actualET-daily(ip)%recharge

  END SUBROUTINE RECHARGE

! ****************************************************************************
  SUBROUTINE waterbudget(ip, precip_adjusted)

  use define_poly

  implicit none

  INTEGER :: ip
  DOUBLE PRECISION    :: precip_adjusted
!  DOUBLE PRECISION:: PRECIP, RCH

  daily(ip)%budget = daily(ip)%moisture-before(ip)%moisture+daily(ip)%actualET+daily(ip)%recharge- &
                     precip_adjusted-daily(ip)%irrigation       

  END SUBROUTINE 