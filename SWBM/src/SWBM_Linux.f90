 PROGRAM SWRM
  
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

  INTEGER  :: nmonth, numdays, imonth,jday,i,im,ip, nrows, ncols, dummy, nsegs, n_wel_param
  INTEGER  :: ip_AG_SW_Flood, ip_AG_SW_WL, ip_AG_SW_CP, ip_AG_GW_Flood, ip_AG_GW_WL, ip_AG_GW_CP                                    ! Field IDs for Daily Output
  INTEGER  :: ip_AG_MIX_Flood, ip_AG_MIX_WL, ip_AG_MIX_CP, ip_AG_SUB_DRY, ip_Pasture_SW_Flood, ip_Pasture_SW_WL                     ! Field IDs for Daily Output
  INTEGER  :: ip_Pasture_SW_CP, ip_Pasture_GW_Flood, ip_Pasture_GW_WL, ip_Pasture_GW_CP, ip_Pasture_MIX_Flood, ip_Pasture_MIX_WL    ! Field IDs for Daily Output
  INTEGER  :: ip_Pasture_MIX_CP, ip_Pasture_SUB_DRY, ip_ETnoIrr_Low_WC8, ip_ETnoIrr_High_WC8, ip_noETnoIrr, ip_Water_Landuse        ! Field IDs for Daily Output
  INTEGER  :: ip_SW_Flood_DZ
  INTEGER, ALLOCATABLE, DIMENSION(:,:) :: zone_matrix, no_flow_matrix, output_zone_matrix, Discharge_Zone_Cells
  REAL   :: precip, Total_Ref_ET
  REAL, ALLOCATABLE, DIMENSION(:)  :: drain_flow
  REAL, ALLOCATABLE, DIMENSION(:,:)  :: MAR_Matrix
  REAL :: start, finish
  INTEGER, DIMENSION(0:11)  :: nday
  CHARACTER(9) :: param_dummy
  CHARACTER(10)  :: SFR_Template
  INTEGER, DIMENSION(31) :: ET_Active
! We defined precip adjusted after discussion with Rick Snyder and after looking at the FAO 56 document
! Precip_adjusted = Precip, if Precip > 0.2 * ref_et
! Precip_adjusted = 0, if Precip <= 0.2 * ref_et
! Small but significant difference! On a wet day, all precip counts. When it's a sprinkle, it doesn't count at all.
! Precip_adjusted will be used instead than effprecip
   call cpu_time(start)
   DATA nday / 30,31,30,31,31,28,31,30,31,30,31,31 /            ! The months are shifted by one because the first index location is zero due to use of MOD function
   open (unit=800, file='SWBM_log.rec')                         ! Open record file for screen output
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
   open(unit=215,file='MAR_Array.txt',status='old')      ! Read in MAR recharge matrix
   read(215,*) MAR_Matrix
   
   close(210)
   close(212)
   close(214)
   close(215)
   
   output_zone_matrix = zone_matrix * no_flow_matrix        ! Create Recharge Zone Matrix with zeros at no flow cells
!    write(888,'(210i5)') rch_zone_matrix
!    write(889,'(210i2)') no_flow_matrix   
!    write(890,'(210i5)') output_zone_matrix   


   open(unit=532,file='5daysdeficiency.dat')
!   open(unit=887,file='precip_m_LowBias_July2017.txt')         ! Missing data assumed to have value of zero
!   open(unit=887,file='precip_m_Replacement_July2017.txt')     ! Missing data replaced with value at other station (CAL and FJN)  
   open(unit=887,file='precip.txt', status = 'old')                    ! Missing data replaced with value obtained from regression using other stations
   open(unit=88,file='ref_et.txt', status = 'old')
   open(unit=79, file='kc_grain.txt', status = 'old')
   open(unit=80, file='kc_alfalfa.txt', status = 'old')
   open(unit=81, file='kc_pasture.txt', status = 'old')
   
   open(unit=60, file='subwatershed_area_m2.dat')
   write(60,'(" Month Scott French Etna Patterson Kidder Moffet Mill Shackleford Tailings")')
   open(unit=61, file='landuse_area_m2.dat')
   write(61,'(" Month Alfalfa Grain Pasture Et/noIrr noET/noIrr Water")')
   open(unit=62, file='landuse_area_m2_detailed.dat')
   write(62,'(" SP A_Irr A_n* A_SUB A_DRY G_Irr G_n* G_SUB G_DRY P_Irr P_n* P_SUB P_DRY ET/noIrr noET/noIrr Water Total")')
   open(unit=63, file='landuse_area_acres_detailed.dat')
   write(63,'(" SP A_Irr A_n* A_SUB A_DRY G_Irr G_n* G_SUB G_DRY P_Irr P_n* P_SUB P_DRY ET/noIrr noET/noIrr Water Total")')
   
   open(unit=600, file='Daily_out_Alfalfa-Grain_SW_Flood.dat')               ! Daily output for selected fields
   open(unit=601, file='Daily_out_Alfalfa-Grain_SW_WL.dat')                  ! Daily output for selected fields
   open(unit=602, file='Daily_out_Alfalfa-Grain_SW_CP.dat')                  ! Daily output for selected fields
   open(unit=603, file='Daily_out_Alfalfa-Grain_GW_Flood.dat')               ! Daily output for selected fields
   open(unit=604, file='Daily_out_Alfalfa-Grain_GW_WL.dat')                  ! Daily output for selected fields
   open(unit=605, file='Daily_out_Alfalfa-Grain_GW_CP.dat')                  ! Daily output for selected fields
   open(unit=606, file='Daily_out_Alfalfa-Grain_MIX_Flood.dat')              ! Daily output for selected fields
   open(unit=607, file='Daily_out_Alfalfa-Grain_MIX_WL.dat')                 ! Daily output for selected fields
   open(unit=608, file='Daily_out_Alfalfa-Grain_MIX_CP.dat')                 ! Daily output for selected fields
   open(unit=609, file='Daily_out_Alfalfa-Grain_SUB_DRY.dat')                ! Daily output for selected fields
   open(unit=610, file='Daily_out_Pasture_SW_Flood.dat')                     ! Daily output for selected fields
   open(unit=611, file='Daily_out_Pasture_SW_WL.dat')                        ! Daily output for selected fields
   open(unit=612, file='Daily_out_Pasture_SW_CP.dat')                        ! Daily output for selected fields
   open(unit=613, file='Daily_out_Pasture_GW_Flood.dat')                     ! Daily output for selected fields
   open(unit=614, file='Daily_out_Pasture_GW_WL.dat')                        ! Daily output for selected fields
   open(unit=615, file='Daily_out_Pasture_GW_CP.dat')                        ! Daily output for selected fields
   open(unit=616, file='Daily_out_Pasture_MIX_Flood.dat')                    ! Daily output for selected fields
   open(unit=617, file='Daily_out_Pasture_MIX_WL.dat')                       ! Daily output for selected fields
   open(unit=618, file='Daily_out_Pasture_MIX_CP.dat')                       ! Daily output for selected fields
   open(unit=619, file='Daily_out_Pasture_SUB_DRY.dat')                      ! Daily output for selected fields
   open(unit=620, file='Daily_out_ET-noIrr_Low_WC8.dat')                     ! Daily output for selected fields
   open(unit=621, file='Daily_out_ET-noIrr_High_WC8.dat')                    ! Daily output for selected fields
   open(unit=622, file='Daily_out_noET-noIrr.dat')                           ! Daily output for selected fields
   open(unit=623, file='Daily_out_Water_Landuse.dat')                        ! Daily output for selected fields
   open(unit=624, file='Daily_out_SW_Flood_DZ.dat')                          ! Daily output for selected fields

   write(600,'(" #id  precip_adj streamflow irrig  well rch moisture  ET  actualET  deficiency budget WC8 subwn landuse rotation")')
   write(601,'(" #id  precip_adj streamflow irrig  well rch moisture  ET  actualET  deficiency budget WC8 subwn landuse rotation")')
   write(602,'(" #id  precip_adj streamflow irrig  well rch moisture  ET  actualET  deficiency budget WC8 subwn landuse rotation")')
   write(603,'(" #id  precip_adj streamflow irrig  well rch moisture  ET  actualET  deficiency budget WC8 subwn landuse rotation")')
   write(604,'(" #id  precip_adj streamflow irrig  well rch moisture  ET  actualET  deficiency budget WC8 subwn landuse rotation")')
   write(605,'(" #id  precip_adj streamflow irrig  well rch moisture  ET  actualET  deficiency budget WC8 subwn landuse rotation")')
   write(606,'(" #id  precip_adj streamflow irrig  well rch moisture  ET  actualET  deficiency budget WC8 subwn landuse rotation")')
   write(607,'(" #id  precip_adj streamflow irrig  well rch moisture  ET  actualET  deficiency budget WC8 subwn landuse rotation")')
   write(608,'(" #id  precip_adj streamflow irrig  well rch moisture  ET  actualET  deficiency budget WC8 subwn landuse rotation")')
   write(609,'(" #id  precip_adj streamflow irrig  well rch moisture  ET  actualET  deficiency budget WC8 subwn landuse rotation")')
   write(610,'(" #id  precip_adj streamflow irrig  well rch moisture  ET  actualET  deficiency budget WC8 subwn landuse rotation")')
   write(611,'(" #id  precip_adj streamflow irrig  well rch moisture  ET  actualET  deficiency budget WC8 subwn landuse rotation")')
   write(612,'(" #id  precip_adj streamflow irrig  well rch moisture  ET  actualET  deficiency budget WC8 subwn landuse rotation")')
   write(613,'(" #id  precip_adj streamflow irrig  well rch moisture  ET  actualET  deficiency budget WC8 subwn landuse rotation")')
   write(614,'(" #id  precip_adj streamflow irrig  well rch moisture  ET  actualET  deficiency budget WC8 subwn landuse rotation")')
   write(615,'(" #id  precip_adj streamflow irrig  well rch moisture  ET  actualET  deficiency budget WC8 subwn landuse rotation")')
   write(616,'(" #id  precip_adj streamflow irrig  well rch moisture  ET  actualET  deficiency budget WC8 subwn landuse rotation")')
   write(617,'(" #id  precip_adj streamflow irrig  well rch moisture  ET  actualET  deficiency budget WC8 subwn landuse rotation")')
   write(618,'(" #id  precip_adj streamflow irrig  well rch moisture  ET  actualET  deficiency budget WC8 subwn landuse rotation")')
   write(619,'(" #id  precip_adj streamflow irrig  well rch moisture  ET  actualET  deficiency budget WC8 subwn landuse rotation")')
   write(620,'(" #id  precip_adj streamflow irrig  well rch moisture  ET  actualET  deficiency budget WC8 subwn landuse rotation")')
   write(621,'(" #id  precip_adj streamflow irrig  well rch moisture  ET  actualET  deficiency budget WC8 subwn landuse rotation")')
   write(622,'(" #id  precip_adj streamflow irrig  well rch moisture  ET  actualET  deficiency budget WC8 subwn landuse rotation")')
   write(623,'(" #id  precip_adj streamflow irrig  well rch moisture  ET  actualET  deficiency budget WC8 subwn landuse rotation")')
   write(624,'(" #id  precip_adj streamflow irrig  well rch moisture  ET  actualET  deficiency budget WC8 subwn landuse rotation")')
   
   open(unit=900, file='Recharge_Total.dat')   
   write(900,'(a24)')'Total Recharge (m^3/day)'
        
   ip_AG_SW_Flood=171           ! Polygon ID for Daily output
   ip_AG_SW_WL=437              ! Polygon ID for Daily output
   ip_AG_SW_CP=600              ! Polygon ID for Daily output
   ip_AG_GW_Flood=538           ! Polygon ID for Daily output
   ip_AG_GW_WL=456              ! Polygon ID for Daily output
   ip_AG_GW_CP=553              ! Polygon ID for Daily output
   ip_AG_MIX_Flood=136          ! Polygon ID for Daily output
   ip_AG_MIX_WL=392             ! Polygon ID for Daily output
   ip_AG_MIX_CP=387             ! Polygon ID for Daily output
   ip_AG_SUB_DRY=700            ! Polygon ID for Daily output
   ip_Pasture_SW_Flood=1914     ! Polygon ID for Daily output
   ip_Pasture_SW_WL=1936        ! Polygon ID for Daily output
   ip_Pasture_SW_CP=1913        ! Polygon ID for Daily output
   ip_Pasture_GW_Flood=1941     ! Polygon ID for Daily output
   ip_Pasture_GW_WL=1741        ! Polygon ID for Daily output
   ip_Pasture_GW_CP=1898        ! Polygon ID for Daily output
   ip_Pasture_MIX_Flood=2007    ! Polygon ID for Daily output
   ip_Pasture_MIX_WL=1935       ! Polygon ID for Daily output
   ip_Pasture_MIX_CP=1686       ! Polygon ID for Daily output
   ip_Pasture_SUB_DRY=1987      ! Polygon ID for Daily output
   ip_ETnoIrr_Low_WC8=911       ! Polygon ID for Daily output
   ip_ETnoIrr_High_WC8=902      ! Polygon ID for Daily output
   ip_noETnoIrr=1228            ! Polygon ID for Daily output
   ip_Water_Landuse=2100        ! Polygon ID for Daily output
   ip_SW_Flood_DZ = 1627        ! Polygon ID for Daily output                                
                                                       
   open(unit=91, file='well_out.dat')              
   write(91,'("#WELL: amount of monthly water pumped in each polygon")')
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
   open(unit=92, file='irrig_out.dat')
   write(92,'("#IRRIGATION: amount of monthly irrigation in each polygon")')   
   open(unit=93, file='evapotrasp_out.dat')
   write(93,'("#Evapotrasp: amount of monthly evapotraspiration in each polygon")')
   open(unit=94, file='recharge_out.dat')
   write(94,'("#Recharge: amount of monthly recharge in each polygon")')
   open(unit=84, file='SVIHM.rch')
   open(unit=95, file='moisture_out.dat')
   write(95,'("#Moisture: amount of monthly moisture in each polygon")')
   open(unit=96, file='actualET_out.dat')
   write(96,'("#ActualET: amount of monthly actualET in each polygon")')
   open(unit=97, file='deficiency_out.dat')
   write(97,'("#Deficiency: amount of monthly deficiency (ET-actualET) in each polygon")')
   open(unit=98, file='well_out_flow.dat')
   write(98,'("#WELL: monthly FLOW pumped in each polygon")')
   open(unit=120, file='ET_Active_Days.dat')                       
   write(120,'("Number of Days ET is Active in each polyon")')    
   open(unit=66, file='streamflow_input.txt', status='old')
   read(66,*)                                                        ! Read Header into nothing

   open (unit=102, file='monthly_well_by_subw.dat')
   open (unit=103, file='monthly_irrig_by_subw.dat')
   open (unit=104, file='monthly_evapo_by_subw.dat')
   open (unit=105, file='monthly_recharge_by_subw.dat')
   open (unit=106, file='monthly_well_by_luse.dat')
   open (unit=107, file='monthly_irrig_by_luse.dat')
   open (unit=108, file='monthly_evapo_by_luse.dat')
   open (unit=109, file='monthly_recharge_by_luse.dat')
   open (unit=110, file='monthly_deficiency_by_subw.dat')
   open (unit=111, file='monthly_deficiency_by_luse.dat')
   open (unit=112, file='monthly_actualET_by_luse.dat')
   open (unit=113, file='monthly_actualET_by_subw.dat')
   open (unit=114, file='monthly_moisture_by_subw.dat') 
   open (unit=115, file='monthly_moisture_by_luse.dat') 
   
!   open (unit=202, file='yearly_well_by_subw.dat')
!   open (unit=203, file='yearly_irrig_by_subw.dat')
!   open (unit=204, file='yearly_evapo_by_subw.dat')
!   open (unit=205, file='yearly_recharge_by_subw.dat')
!   open (unit=206, file='yearly_well_by_luse.dat')
!   open (unit=207, file='yearly_irrig_by_luse.dat')
!   open (unit=208, file='yearly_evapo_by_luse.dat')
!   open (unit=209, file='yearly_recharge_by_luse.dat')

   CALL EXECUTE_COMMAND_LINE('cp SVIHM_ETS_template.txt SVIHM.ets')
   CALL EXECUTE_COMMAND_LINE('cp SVIHM_SFR_template.txt SVIHM.sfr')
   CALL EXECUTE_COMMAND_LINE('cp SVIHM_WEL_template.txt SVIHM.wel')
   if (SFR_Template=='UCODE') then 
     CALL EXECUTE_COMMAND_LINE('cp SFR_UCODE_JTF.txt SVIHM_SFR.jtf')
   elseif (SFR_Template=='PEST') then
   	 CALL EXECUTE_COMMAND_LINE('cp SFR_PEST_TPL.txt SVIHM_SFR.tpl')
   else
     	print *, 'Invalid Template File Format Variable in general_inputs.txt'
     	CALL EXIT
   end if
   
   open (unit=220, file='Drains_m3day.txt')
   read(220,*)param_dummy     ! Read header comment line to dummy character string 
   
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
     call zero_month                   ! Zero out monthly accumulated volume
     if (imonth==1) then               ! If October:
       call zero_year                    ! Zero out yearly accumulated volume
       call open_yearly_file             ! Open new file for yearly output
     endif    
     read(220,*)drain_flow(im)     ! Read dran flow into array         
     
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
	       
         if (imonth==3 .and. jday==31 .and. ip==2119) then ! If last day of the year, set irrigation flags and logical to zero
			     poly%irr_flag = 0         
			     irrigating = .false.           
		       print*, 'Irrigation Logical Reset'
		       write(800,*)'Irrigation Logical Reset'
		       call Update_Irr_Type(im)
		       print*, 'Irrigation Type Updated'    
           write(800,*)'Irrigation Type Updated'

		     end if
        
         
         call IRRIGATION (ip, imonth, jday, precip_adjusted)    
		     call recharge(ip,precip_adjusted,jday,imonth)
         call deficiency_check(ip, imonth, jday)       

       enddo              ! End of polygon loop
       call daily_out(ip_AG_SW_Flood, ip_AG_SW_WL, ip_AG_SW_CP, ip_AG_GW_Flood, ip_AG_GW_WL, ip_AG_GW_CP, &                ! Print Daily Output for Selected Fields
                     ip_AG_MIX_Flood, ip_AG_MIX_WL, ip_AG_MIX_CP, ip_AG_SUB_DRY, ip_Pasture_SW_Flood, &                  ! Print Daily Output for Selected Fields
                     ip_Pasture_SW_WL, ip_Pasture_SW_CP, ip_Pasture_GW_Flood, ip_Pasture_GW_WL, &                        ! Print Daily Output for Selected Fields
                     ip_Pasture_GW_CP, ip_Pasture_MIX_Flood, ip_Pasture_MIX_WL, ip_Pasture_MIX_CP, &                     ! Print Daily Output for Selected Fields
                     ip_Pasture_SUB_DRY, ip_ETnoIrr_Low_WC8, ip_ETnoIrr_High_WC8, ip_noETnoIrr, ip_Water_Landuse, &      ! Print Daily Output for Selected Fields
                     ip_SW_Flood_DZ)                                                                                     ! Print Daily Output for Selected Fields
           
      

       call pumping(ip, jday, total_n_wells, npoly)   ! Stream depletion subroutine
		   call monthly_SUM      ! add daily value to monthly total (e.g., monthly%irrigation = monthly%irrigation + daily%irrigation)
       call annual_SUM       ! add daily value to yearly total (e.g., yearly%irrigation = yearly%irrigation + daily%irrigation)
       if (jday==numdays) call SFR_streamflow(numdays,imonth)   ! Convert remaining surface water to SFR inflows at end of the month
       
       enddo             ! End of day loop
       jday = jday -1 ! reset jday to number of days in month (incremented to ndays +1 in do loop above)
       
       call monthly_out_length(im)
       call monthly_pumping(im, jday, total_n_wells)
		   call ET_out_MODFLOW(im,imonth,nday,nrows,ncols,output_zone_matrix,Total_Ref_ET,Discharge_Zone_Cells,npoly)
		   Total_Ref_ET = 0.  ! Reset monthly Average ET
       call recharge_out (im)
		   call recharge_out_MODFLOW(im,imonth,nday,nrows,ncols,output_zone_matrix)    
       ! call recharge_out_MODFLOW_w_MAR(im,imonth,nday,nrows,ncols,output_zone_matrix,MAR_Matrix)
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
   stop
   contains

!*****************************************************************************************
   Subroutine final_averages

   integer id, ifile
   DOUBLE PRECISION, allocatable, dimension(:) :: dat, average, averagewell
   open(unit=91, file='well_out.dat')
   open(unit=92, file='irrig_out.dat')
   open(unit=93, file='evapotrasp_out.dat')
   open(unit=94, file='recharge_out.dat')
   open(unit=95, file='moisture_out.dat')
   open(unit=96, file='actualET_out.dat')
   open(unit=97, file='deficiency_out.dat')
   open(unit=98, file='well_out_flow.dat')

   allocate( average(npoly), dat(npoly) )
   allocate( averagewell(total_n_wells) )
   average = 0.
   averagewell = 0.
   
   do ifile = 1, 8
     read(90+ifile,*)
     do im = 1, nmonth 
       read(90+ifile,*) id, dat
       average = average + dat
     enddo
     average = average/nmonth
     write(876,*) "* Average *", 90+ifile
     write(876,*) average
     if (ifile == 4) poly(:)%av_recharge = average
     close(90+ifile)
   enddo

!   call flush(536)
!   rewind(536)
!   read(536,*)

!   do i = 1,nmonth  
!     read(536,*)
!     do im = 1, total_n_wells
!       read(536,*) single_well(i)%well_name, single_well(i)%coordx, &
!                    single_well(i)%coordy, single_well(i)%pumping
!       print *, i, im
!       averagewell = averagewell + single_well(i)%pumping
!     enddo
!     averagewell = averagewell/nmonth
!     write(877,*) single_well(i)%well_name, single_well(i)%coordx, &
!                    single_well(i)%coordy, averagewell
!   enddo
   return
   end Subroutine final_averages
!*****************************************************************************************
   SUBROUTINE AVERAGE_EVAPO
! AV_REF_ET_1a   : average reference ET for alfalfa
!                  with SW and flood irrigation.Average over March 25-Oct 15 in 1991-2011
! AV_REF_ET_1b   : average reference ET for grain with SW and flood irrigation. 
!                  Average over April 15-July 10 in 1991-2011
! AV_REF_ET_2    : average reference ET for pasture with SW and flood irrigation, 
!                  average over April 15-Oct 15 in 1991-2011
   implicit none

   integer :: counter1a, counter1b, counter2
   
   counter1a=0
   counter1b=0
   counter2 =0
   AV_REF_ET_1a = 0.
   AV_REF_ET_1b = 0.
   AV_REF_ET_2  = 0.
   open(unit=88,file="ref_et_new.txt", status = "old")
!   open(unit=79, file='Kc_grain.dat')
   do im=1, nmonth
      imonth=MOD(im,12)
      do jday=1, nday(imonth)
        read(88,*) REF_ET
!        read (79,*)kc_grain
        if((imonth==6 .and. jday>24).or.(imonth==1 .and. jday<16).or. &
          (imonth>6 .or. imonth<1)) then
            counter1a=counter1a+1
            AV_REF_ET_1a= AV_REF_ET_1a+ REF_ET
        endif
        if((imonth==6 .and. jday>14).or.(imonth==9 .and. jday<11).or. &
          (imonth>6 .or. imonth<9)) then
            counter1b=counter1b+1
            AV_REF_ET_1b= AV_REF_ET_1b+ REF_ET*kc_grain
        endif
        if((imonth==6 .and. jday>14).or.(imonth==1 .and. jday<16).or. &
           (imonth>6 .or. imonth<1)) then
            counter2=counter2+1
            AV_REF_ET_2= AV_REF_ET_2+ REF_ET
        endif
      enddo
   enddo
   AV_REF_ET_1a= AV_REF_ET_1a/counter1a
   AV_REF_ET_1b= AV_REF_ET_1b/counter1b
   AV_REF_ET_2= AV_REF_ET_2/counter2
  
   print *,' AV_REF_ET_1a', AV_REF_ET_1a,'AV_REF_ET_1b', AV_REF_ET_1b,'AV_REF_ET_2',AV_REF_ET_2  
   rewind(88)
!   rewind(79)
   return
   end SUBROUTINE AVERAGE_EVAPO

 end PROGRAM

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

  SUBROUTINE RECHARGE(ip,precip_adjusted,jday,imonth)

  use define_poly

  implicit none

  INTEGER :: ip, jday, imonth 
  DOUBLE PRECISION:: RCH
  DOUBLE PRECISION :: precip_adjusted
  

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

!  if (daily(ip)%moisture < 0. ) then
!      daily(ip)%moisture = 0.
!      daily(ip)%deficiency = daily(ip)%actualET
!  else
!      daily(ip)%deficiency = 0.
!  endif

        call waterbudget(ip, precip_adjusted)

  before(ip)%moisture = daily(ip)%moisture
  daily(ip)%change_in_storage = precip_adjusted+daily(ip)%irrigation-daily(ip)%actualET- daily(ip)%recharge 
  END SUBROUTINE RECHARGE

!****************************************************************************

  SUBROUTINE waterbudget(ip, precip_adjusted)

  use define_poly

  implicit none

  INTEGER :: ip
  DOUBLE PRECISION    :: precip_adjusted
!  DOUBLE PRECISION:: PRECIP, RCH

  daily(ip)%budget = daily(ip)%moisture-before(ip)%moisture+daily(ip)%actualET+daily(ip)%recharge- &
                     precip_adjusted-daily(ip)%irrigation       

  END SUBROUTINE 