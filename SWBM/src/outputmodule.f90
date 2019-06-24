 module outputmodule

 use define_poly
 use irrigationmodule
 
 implicit none

 DOUBLE PRECISION :: scott_area, french_area,etna_area, patterson_area
 DOUBLE PRECISION :: kidder_area, moffet_area, mill_area
 DOUBLE PRECISION :: shackleford_area, scott_tailing_area
 DOUBLE PRECISION :: alfalfa_irr_area, grain_irr_area, pasture_irr_area
 DOUBLE PRECISION :: et_noirr_area, noet_noirr_area, water_area
 DOUBLE PRECISION :: A_n_star_area, A_SUB_area, A_DRY_Area
 DOUBLE PRECISION :: G_n_star_area, G_SUB_area, G_DRY_Area
 DOUBLE PRECISION :: P_n_star_area, P_SUB_area, P_DRY_Area 
 DOUBLE PRECISION :: LU3_area, Total_area
 DOUBLE PRECISION, DIMENSION(nsubwn) :: subwnwell, subwnactualET, subwnsw, subwnmoisture, subwnstorage
 DOUBLE PRECISION, DIMENSION(nsubwn) :: subwnirrig, subwnevapo, subwnrecharge, subwndeficiency
 DOUBLE PRECISION, DIMENSION(nlanduse) :: landuseirrig, landuseevapo, landusedeficiency, landusesw
 DOUBLE PRECISION, DIMENSION(nlanduse) :: landusewell, landuserecharge,landuseactualET, landusemoisture, landusestorage
 contains
    
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    subroutine monthly_SUM

    ! Minor issue with change in storage and moisture. Change in storage is really the running total, need to fix it by differencing from month to month and also year to year.
    
    monthly%irrigation         = monthly%irrigation         + daily%irrigation        ! Add daily irrigation length to monthly total
    monthly%well               = monthly%well               + daily%well              ! Add daily pumping length to monthly total
    monthly%recharge           = monthly%recharge           + daily%recharge          ! Add daily recharge length to monthly total
    monthly%moisture           = monthly%moisture           + daily%change_in_storage !  
    monthly%evapotrasp         = monthly%evapotrasp         + daily%evapotrasp        ! Add daily ET length to monthly total
    monthly%actualET           = monthly%actualET           + daily%actualET          ! Add daily actualET length to monthly total
    monthly%deficiency         = monthly%deficiency         + daily%deficiency        ! Add daily deficiency length to monthly total
    monthly%ET_active          = monthly%ET_active          + daily%ET_active         ! Add daily ET length to monthly total    
    monthly%effprecip          = monthly%effprecip          + daily%effprecip         ! Add daily effective precip length to monthly total    
    monthly%change_in_storage  = monthly%change_in_storage  + daily%change_in_storage ! Add daily change in storage length to monthly total    
    monthly%MAR_vol            = monthly%MAR_vol            + daily%MAR_vol           ! Add daily MAR volume to monthly total
    
    end subroutine monthly_SUM

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    subroutine annual_SUM

    yearly%irrigation          = yearly%irrigation          + daily%irrigation         ! Add daily irrigation length to yearly total
    yearly%well                = yearly%well                + daily%well               ! Add daily pumping length to yearly total   
    yearly%recharge            = yearly%recharge            + daily%recharge           ! Add daily recharge length to yearly total  
    yearly%moisture            = yearly%moisture            + daily%change_in_storage  ! Add daily moisture length to yearly total  
    yearly%evapotrasp          = yearly%evapotrasp          + daily%evapotrasp         ! Add daily ET length to yearly total        
    yearly%actualET            = yearly%actualET            + daily%actualET           ! Add daily actualET length to yearly total  
    yearly%deficiency          = yearly%deficiency          + daily%deficiency         ! Add daily deficiency length to yearly total
    yearly%ET_active           = yearly%ET_active           + daily%ET_active          ! Add daily ET length to yearly total    
    yearly%effprecip           = yearly%effprecip           + daily%effprecip          ! Add daily effective precip length to yearly total    
    yearly%change_in_storage   = yearly%change_in_storage   + daily%change_in_storage  ! Add daily change in storage length to yearly total    
    yearly%MAR_vol             = yearly%MAR_vol             + daily%MAR_vol            ! Add daily MAR to annual total
    
    end subroutine annual_SUM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    subroutine monthly_volume_out

    integer       :: ip, ilanduse
    integer, save :: imonth = 0
    
    imonth = imonth+1
    subwnsw           = 0.
    subwnwell         = 0.                            
    subwnirrig        = 0.                         
    subwnevapo        = 0.                          
    subwnactualET     = 0.                       
    subwnrecharge     = 0.                    
    subwndeficiency   = 0.                 
    subwnmoisture     = 0.                  
     
    landusesw         = 0.                          
    landusewell       = 0.
    landuseirrig      = 0.
    landuseevapo      = 0.
    landuserecharge   = 0.
    landusedeficiency = 0.
    landuseactualET   = 0.
    landusemoisture   = 0.      
    
    do ip = 1, npoly
      subwnsw(poly(ip)%subwn)         = subwnsw(poly(ip)%subwn) &
                                        + (monthly(ip)%irrigation_vol - monthly(ip)%well_vol)
      subwnwell(poly(ip)%subwn)       = subwnwell(poly(ip)%subwn)       + monthly(ip)%well_vol  
      subwnirrig(poly(ip)%subwn)      = subwnirrig(poly(ip)%subwn)      + monthly(ip)%irrigation_vol
      subwnevapo(poly(ip)%subwn)      = subwnevapo(poly(ip)%subwn)      + monthly(ip)%evapotrasp_vol
      subwnactualET(poly(ip)%subwn)   = subwnactualET(poly(ip)%subwn)   + monthly(ip)%actualET_vol
      subwnrecharge(poly(ip)%subwn)   = subwnrecharge(poly(ip)%subwn)   + monthly(ip)%recharge_vol
      subwndeficiency(poly(ip)%subwn) = subwndeficiency(poly(ip)%subwn) + monthly(ip)%deficiency_vol   
      subwnmoisture(poly(ip)%subwn)   = subwnmoisture(poly(ip)%subwn)   + monthly(ip)%moisture_vol
    
      select case (poly(ip)%landuse)
        case (25)   !alfalfa / grain
          if (poly(ip)%irr_type == 555 .or. poly(ip)%water_source == 4 .or. poly(ip)%water_source == 5) then  ! If n* or SUB or DRY
            ilanduse = 4  ! ET_noIRRIG
          else
            if (poly(ip)%rotation==11) ilanduse = 1  ! Alfalfa
            if (poly(ip)%rotation==12) ilanduse = 2  ! Grain 
          endif
        case (2)
          if (poly(ip)%irr_type == 555 .or. poly(ip)%water_source == 4 .or. poly(ip)%water_source == 5) then  ! If n* or SUB or DRY
            ilanduse = 4  ! ET_noIRRIG
          else
            ilanduse = 3 ! pasture
          endif
        case(3)
           ilanduse = 4  ! ET_noIRRIG
           if (daily(ip)%irrigation .GT. 0) write(800,*)'Polygon',ip
           if (monthly(ip)%irrigation_vol .GT. 0) write(800,*)'Polygon',ip
        case (4)
           ilanduse = 5  ! noET_noIRRIG     
        end select 
      
      landusesw(ilanduse)         = landusesw(ilanduse)  &
                                    + (monthly(ip)%irrigation_vol - monthly(ip)%well_vol)
      landusewell(ilanduse)       = landusewell(ilanduse)       + monthly(ip)%well_vol 
      landuseirrig(ilanduse)      = landuseirrig(ilanduse)      + monthly(ip)%irrigation_vol
      landuseevapo(ilanduse)      = landuseevapo(ilanduse)      + monthly(ip)%evapotrasp_vol
      landuserecharge(ilanduse)   = landuserecharge(ilanduse)   + monthly(ip)%recharge_vol
      landusedeficiency(ilanduse) = landusedeficiency(ilanduse) + monthly(ip)%deficiency_vol
      landuseactualET(ilanduse)   = landuseactualET(ilanduse)   + monthly(ip)%actualET_vol   
      landusemoisture(ilanduse)   = landusemoisture(ilanduse)   + monthly(ip)%moisture_vol
    enddo    
                                          
    write(101,'(i4,9F20.8)') imonth, subwnsw(:)                         
    write(102,'(i4,9F20.8)') imonth, subwnwell(:)                     
    write(103,'(i4,9F20.8)') imonth, subwnirrig(:)                    
    write(104,'(i4,9F20.8)') imonth, subwnevapo(:)                    
    write(105,'(i4,9F20.8)') imonth, subwnactualET(:)                 
    write(106,'(i4,9F20.8)') imonth, subwnrecharge(:)                 
    write(107,'(i4,9F20.8)') imonth, subwndeficiency(:)               
    write(108,'(i4,9F20.8)') imonth, subwnmoisture(:)                 
                                                                      
    write(109,'(i4,5F20.8)') imonth, landusesw(:)                     
    write(110,'(i4,5F20.8)') imonth, landusewell(:)                   
    write(111,'(i4,5F20.8)') imonth, landuseirrig(:)                                                                    
    write(112,'(i4,5F20.8)') imonth, landuseevapo(:)                      
    write(113,'(i4,5F20.8)') imonth, landuseactualET(:)                   
    write(114,'(i4,5F20.8)') imonth, landuserecharge(:)                  
    write(115,'(i4,5F20.8)') imonth, landusedeficiency(:)               
    write(116,'(i4,5F20.8)') imonth, landusemoisture(:)                                          
                                                                        
    write(117,'(i4,6F20.0)')imonth, sum(monthly%effprecip_vol), (sum(monthly%irrigation_vol)-sum(monthly%well_vol)), &
    sum(monthly%well_vol), -sum(monthly%actualET_vol), -sum(monthly%recharge_vol), -sum(monthly%change_in_storage_vol)
    
    end subroutine monthly_volume_out
    
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    subroutine weight_av_recharge
! Averaged recharge for the stream depletion code
    
    integer :: i, ip, iwell
    DOUBLE PRECISION    :: wav_recharge(0:total_n_wells), areawell(0:total_n_wells)
  
    wav_recharge = 0.
    areawell = 0.

    do ip=1, npoly
       iwell = poly(ip)%id_well
       areawell(iwell) = areawell(iwell) + poly(ip)%MF_area
    enddo

    do ip=1, npoly
       iwell = poly(ip)%id_well
!       wav_recharge(iwell) = wav_recharge(iwell) + daily(ip)%recharge*poly(ip)%MF_area  *  &
!                             poly(ip)%MF_area/areawell(iwell)
!With or without weighted average???

		wav_recharge(iwell) = wav_recharge(iwell) + daily(ip)%recharge*poly(ip)%MF_area  
    enddo
    
!   do iwell = 1, total_n_wells  
!     wav_recharge(iwell) = wav_recharge(iwell)  !/areawell(iwell)
!   enddo
    write(538,*) wav_recharge(:)
!         do i = 1, total_n_wells
!         write(538, *)single_well(i)%well_name, single_well(i)%coordx, &
!                    single_well(i)%coordy, wav_recharge(i)
!		enddo

    end subroutine weight_av_recharge

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~	
subroutine pumping(ip, jday, total_n_wells, npoly)
! In this subroutine, daily values for pumping and recharge in the polygons with wells are saved.
! These values will then just need to be averaged over the 21 years to obtain the daily average.
    
    integer :: i, ip, iwell, total_n_wells, jday, npoly
    
    single_well%daily_well_vol = 0.  ! set daily value to zero
    if (jday ==1) then                       ! If first day of month, reset monthly values to zero
      single_well%monthly_well_vol = 0.
      single_well%monthly_well_rate = 0.
    end if
    
    do ip=1, npoly
         iwell = poly(ip)%id_well  ! obtain well index location
         single_well(iwell)%daily_well_vol =  single_well(iwell)%daily_well_vol + &
                                              daily(ip)%well*poly(ip)%MF_area   ! assign daily pumping volume
    enddo
    
    do iwell=1,total_n_wells
      single_well(iwell)%monthly_well_vol = single_well(iwell)%monthly_well_vol + single_well(iwell)%daily_well_vol ! add daily volume to monthly counter
    end do
    
    write(537,'(200es20.8)') single_well%daily_well_vol
end subroutine pumping 

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~	
subroutine monthly_pumping(im, jday, total_n_wells)

    integer :: im, jday, iwell, total_n_wells
  
    single_well(:)%monthly_well_rate = single_well(:)%monthly_well_vol / jday
    
    write(530,'(172es20.8)')single_well(:)%monthly_well_vol
    write(531,'(172es20.8)')single_well(:)%monthly_well_rate
    

end subroutine monthly_pumping
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  subroutine write_MODFLOW_WEL(im,imonth,total_n_wells,n_wel_param)
  
  INTEGER :: im, imonth, total_n_wells, n_wel_param, iwell
  
  open(unit=536, file='SVIHM.wel',Access = 'append', status='old')
  if (imonth == 1 .or. imonth == 2 .or. imonth == 3 .or. imonth == 4 .or. &                              ! If October-March
      imonth == 5 .or. imonth == 6) then
!    write(536,*)
    write(536,'(I10,I10,A28,I4)')total_n_wells, n_wel_param-2, '               Stress Period',im         ! Only MFR is active, subtract number of ditches represented 
  else if (imonth == 7 .or. imonth == 8) then                                                            ! If April-May
!    write(536,*)
    write(536,'(I10,I10,A28,I4)')total_n_wells, n_wel_param, '               Stress Period',im           ! MFR and Ditches are active, use all WEL parameters
  else if (imonth == 9 .or. imonth == 10) then                                                           ! If June - July
!    write(536,*)
    write(536,'(I10,I10,A28,I4)')total_n_wells, n_wel_param-7, '               Stress Period',im         ! Only Ditches are active, subtract number of MFR segments represented
  else if (imonth == 11 .or. imonth == 0) then                                                           ! If August-September
!    write(536,*)
    write(536,'(I10,I10,A28,I4)')total_n_wells, n_wel_param-9, '               Stress Period',im         ! Only Ditches are active, subtract number of MFR and Ditch segments represented                                              
  end if
  
  do iwell=1,total_n_wells
    write(536,'(3I10,ES15.3)')single_well(iwell)%layer, single_well(iwell)%well_row, &
     single_well(iwell)%well_col, -1*single_well(iwell)%monthly_well_rate
  end do
  
  if (imonth == 1 .or. imonth == 2 .or. imonth == 3 .or. imonth == 4 .or. &                              ! If October-March MFR is active
      imonth == 5 .or. imonth == 6) then                                                           
    write(536,*)'  MFR5'
    write(536,*)'  MFR6'
    write(536,*)'  MFR7'
    write(536,*)'  MFR8'
    write(536,*)'  MFR9'
    write(536,*)'  MFR10'
    write(536,*)'  MFR11'
  else if (imonth == 7 .or. imonth == 8) then                                                            ! If April-May MFR and Ditches are active
    write(536,*)'  MFR5'                                            
    write(536,*)'  MFR6'
    write(536,*)'  MFR7'
    write(536,*)'  MFR8'
    write(536,*)'  MFR9'
    write(536,*)'  MFR10'
    write(536,*)'  MFR11'
    write(536,*)'  FRMRSDitch'
    write(536,*)'  SVIDDitch'
  else if (imonth == 9 .or. imonth == 10) then                                                           ! If June-July ditches are active
    write(536,*)'  FRMRSDitch'
    write(536,*)'  SVIDDitch'
  end if
  
  end subroutine  write_MODFLOW_WEL
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
   subroutine daily_out(num_daily_out,ip_daily_out, eff_precip)
   
   INTEGER, INTENT(in) ::  num_daily_out
   INTEGER, DIMENSION(num_daily_out), INTENT(in) :: ip_daily_out
   INTEGER  :: unit_num, i
   DOUBLE PRECISION, INTENT(in) :: eff_precip
   
   do i=1,num_daily_out
     unit_num = 599+i
     write(unit_num,'(i5,11F20.8,3i4)') ip_daily_out(i), eff_precip, streamflow_in(poly(ip_daily_out(i))%subwn),      &
                                        daily(ip_daily_out(i))%irrigation, daily(ip_daily_out(i))%well,          &
                                        daily(ip_daily_out(i))%recharge, daily(ip_daily_out(i))%moisture,        & 
                                        daily(ip_daily_out(i))%evapotrasp,  daily(ip_daily_out(i))%actualET,     &  
                                        daily(ip_daily_out(i))%deficiency,daily(ip_daily_out(i))%budget,         &
                                        poly(ip_daily_out(i))%WC8,poly(ip_daily_out(i))%subwn,                   &
                                        poly(ip_daily_out(i))%landuse, poly(ip_daily_out(i))%rotation
   end do                                                                                                   ! Field IDs for Daily Output
   end subroutine daily_out
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! The following version of the daily out subroutine saves daily values for each polygon
! but makes the program very slow and should be done only when there is the specific 
! purpose of saving all these values

!    subroutine daily_out( ip, eff_precip)!

!      INTEGER::ip
!      DOUBLE PRECISION :: eff_precip
!      character(len=9) :: fileroot
!      character(len=13) :: outday
         
!      fileroot = "day_poly."

!      write(outday,'(a9,i4)') fileroot, ip
!     print *,outday

!      open (unit = 600, file = outday, position="append")

!      write(600,'(i5,10f16.5,2i4)') ip, eff_precip,streamflow_in(poly(ip)%subwn), &
!                               daily(ip)%irrigation, daily(ip)%well,       &
!                               daily(ip)%recharge, daily(ip)%moisture,     &
!                               daily(ip)%evapotrasp, daily(ip)%actualET,   &
!                               daily(ip)%deficiency,  poly(ip)%WC8,        &
!                               poly(ip)%subwn, poly(ip)%landuse
!      close(600)
!    end subroutine daily_out
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    subroutine monthly_out_by_field(im)                                  
                                                            
    INTEGER, INTENT(in) :: im                               
                                                                         
    write(91,'(i4,2119g12.5)')im, monthly%well               
    write(92,'(i4,2119g12.5)')im, monthly%irrigation      
    write(93,'(i4,2119g12.5)')im, monthly%evapotrasp                   
    write(94,'(i4,2119g12.5)')im, monthly%recharge        
    write(95,'(i4,2119g12.5)')im, daily%moisture             ! daily value used because that is the state of the field at the end of the month 
    write(96,'(i4,2119g12.5)')im, monthly%actualET        
    write(97,'(i4,2119g12.5)')im, monthly%deficiency                           
    write(120,'(i4,2119i4)')im, monthly%ET_active        
                                                                         
    write(200,'(i4,2119g12.5)')im, monthly%well_vol          
    write(201,'(i4,2119g12.5)')im, monthly%irrigation_vol 
    write(202,'(i4,2119g12.5)')im, monthly%evapotrasp_vol                  
    write(203,'(i4,2119g12.5)')im, monthly%recharge_vol   
    write(204,'(i4,2119g12.5)')im, daily%moisture*poly%MF_area       ! daily value used because that is the state of the field at the end of the month 
    write(205,'(i4,2119g12.5)')im, monthly%actualET_vol                    
    write(206,'(i4,2119g12.5)')im, monthly%deficiency_vol
        
    end subroutine monthly_out_by_field

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    subroutine ET_out_MODFLOW(im,imonth,nday,nrows,ncols,output_zone_matrix,Total_Ref_ET,Discharge_Zone_Cells, npoly)
 
    INTEGER, INTENT(IN) :: im,imonth,nrows,ncols, npoly
    INTEGER, INTENT(IN) :: output_zone_matrix(nrows,ncols), nday(0:11), Discharge_Zone_Cells(nrows,ncols)
    REAL, INTENT(IN) :: Total_Ref_ET
    REAL :: Avg_Ref_ET
    REAL, DIMENSION(npoly) :: ET_fraction
    INTEGER :: ip
    REAL, DIMENSION(nrows,ncols) :: Extinction_depth_matrix, ET_matrix_out
    
    ET_matrix_out = 0.
    Avg_Ref_ET = Total_Ref_ET/dble(nday(imonth))                                                   ! Calculate average Reference ET for populating ET package
    
    do ip=1,npoly
      ET_fraction(ip) = monthly(ip)%ET_active / dble(nday(imonth)) 
      where (output_zone_matrix(:,:) == ip)
        ET_matrix_out(:,:) = Avg_Ref_ET * (1 - ET_fraction(ip))  ! Scale Average monthly ET by the number of days ET was not active on the field.
        Extinction_depth_matrix(:,:) = 0.5
      end where
    end do
    
    ET_matrix_out = ET_matrix_out * Discharge_Zone_Cells
    Extinction_depth_matrix = Extinction_depth_matrix 
    
    if (im==1) then
    	open(unit=83, file='SVIHM.ets', status = 'old', position = 'append')
    	write(83, *)"       20   1.00000(10e14.6)                   -1     ET RATE"
      write(83,'(10e14.6)') ET_matrix_out                ! Write Max ET Rates 
      write(83,*)'       20   1.00000(10e14.6)                   -1     ET DEPTH'
      write(83,'(10e14.6)') Extinction_depth_matrix  ! Write ET Extinction Depth
      write(83,*)'        05.0000e-01(20F6.3)                    -1     PXDP Segment 1'    ! Linear decrease in ET from land surface to 0.5 m below surface
      write(83,*)'        05.0000e-01(20F6.3)                    -1     PETM Segment 1'    ! Linear decrease in ET from land surface to 0.5 m below surface
    else
      write(83,*)'       -1         1         1        -1        -1'	 
      write(83, *)"       20   1.00000(10e14.6)                   -1     ET RATE"
      write(83,'(10e14.6)') ET_matrix_out                ! Write Max ET Rates 
      write(83,*)'       20   1.00000(10e14.6)                   -1     ET DEPTH'
      write(83,'(10e14.6)') Extinction_depth_matrix  ! Write ET Extinction Depth
    end if
    end subroutine ET_out_MODFLOW	

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    subroutine recharge_out_MODFLOW(im,imonth,nday,nrows,ncols,output_zone_matrix)  ! Recharge file            

      
      INTEGER, INTENT(IN) :: im,imonth,nrows,ncols
      INTEGER, INTENT(IN) :: output_zone_matrix(nrows,ncols), nday(0:11)
      REAL, ALLOCATABLE, DIMENSION(:,:) ::  recharge_matrix
      INTEGER :: ip
      REAL :: ttl_rch
  
      ALLOCATE(recharge_matrix(nrows,ncols))
      recharge_matrix = 0.
      
      if (im == 1) then
      write(84,'(a31)')'# Recharge File written by SWBM'
      write(84,*)'PARAMETER  0'
      write(84,*)'3  50'
      end if
      
      write(84,*)'1  0'
      write(84,'(a63)')'        18   1.00000(10e14.6)                   -1     RECHARGE'
       
        do ip = 1, npoly
          where (output_zone_matrix(:,:) == ip) 
            recharge_matrix(:,:) = monthly(ip)%recharge / nday(imonth)
          end where
        end do
      write(84,'(10e14.6)') recharge_matrix   
      ttl_rch = sum(recharge_matrix*10000)
      write(900,*) ttl_rch, ttl_rch*nday(imonth)
    end subroutine recharge_out_MODFLOW
    
! ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
!     subroutine recharge_out_MODFLOW_w_MAR(im,imonth,nday,nrows,ncols,output_zone_matrix,MAR_Matrix)  ! Recharge file  with MAR added during Jan Feb and Mar          
! 
!       
!       INTEGER, INTENT(IN) :: im,imonth,nrows,ncols
!       INTEGER, INTENT(IN) :: output_zone_matrix(nrows,ncols), nday(0:11)
!       REAL, INTENT(IN) :: MAR_Matrix(nrows,ncols)
!       REAL, ALLOCATABLE, DIMENSION(:,:) ::  recharge_matrix
!       INTEGER :: ip
!       REAL :: rch_sum, MAR_sum, ttl_rch
!   
!       ALLOCATE(recharge_matrix(nrows,ncols))
!       recharge_matrix = 0.
!       
!       if (im == 1) then
!       write(84,'(a31)')'# Recharge File written by SWBM'
!       write(84,*)'PARAMETER  0'
!       write(84,*)'3  50'
!       end if
!       
!       write(84,*)'1  0'
!       write(84,'(a63)')'        18   1.00000(10e14.6)                   -1     RECHARGE'
!        
!         do ip = 1, npoly
!           where (output_zone_matrix(:,:) == ip) 
!             recharge_matrix(:,:) = monthly(ip)%recharge / nday(imonth)
!           end where
!         end do
!         if (imonth == 4 .or. imonth == 5 .or. imonth == 6) then          ! If Jan-Mar add recharge from MAR
!           rch_sum = sum(recharge_matrix*10000)                           ! Total of normal recharge rate in m^3/day
!           MAR_sum = sum(MAR_Matrix*10000)                                ! Total of MAR rate in m^3/day
!           ttl_rch = rch_sum + MAR_sum                                    ! Total recharge rate applied to MODFLOW
!           write(*,'(a15,f12.0,a8)')'Non-MAR rate = ',rch_sum,' m^3/day'
!           write(800,'(a15,f12.0,a8)')'Non-MAR rate = ',rch_sum,' m^3/day'
!           recharge_matrix(:,:) = recharge_matrix(:,:) + MAR_Matrix
!           write(*,'(a11,f12.0,a8)')'MAR rate = ',MAR_sum,' m^3/day'
!           write(800,'(a11,f12.0,a8)')'MAR rate = ',MAR_sum,' m^3/day'
!           write(*,'(a22,f12.0,a8)')'Total recharge rate = ',ttl_rch, ' m^3/day'
!           write(800,'(a22,f12.0,a8)')'Total recharge rate = ',ttl_rch, ' m^3/day'
!           write(*,*)''
!           write(800,*),''
!         else
!           ttl_rch = sum(recharge_matrix*10000)
!         end if
!       write(84,'(10e14.6)') recharge_matrix      
!       write(900,*) ttl_rch
!     end subroutine recharge_out_MODFLOW_w_MAR
!
!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
    subroutine convert_length_to_volume

    monthly%irrigation_vol         = monthly%irrigation        *poly%MF_area
    monthly%evapotrasp_vol         = monthly%evapotrasp        *poly%MF_area
    monthly%moisture_vol           = monthly%moisture          *poly%MF_area
    monthly%actualET_vol           = monthly%actualET          *poly%MF_area
    monthly%recharge_vol           = monthly%recharge          *poly%MF_area
    monthly%well_vol               = monthly%well              *poly%MF_area
    monthly%deficiency_vol         = monthly%deficiency        *poly%MF_area      
    monthly%effprecip_vol          = monthly%effprecip         *poly%MF_area      
    monthly%change_in_storage_vol  = monthly%change_in_storage *poly%MF_area

    yearly%irrigation_vol          = yearly%irrigation         *poly%MF_area
    yearly%evapotrasp_vol          = yearly%evapotrasp         *poly%MF_area
    yearly%moisture_vol            = yearly%moisture           *poly%MF_area
    yearly%actualET_vol            = yearly%actualET           *poly%MF_area
    yearly%recharge_vol            = yearly%recharge           *poly%MF_area
    yearly%well_vol                = yearly%well               *poly%MF_area
    yearly%deficiency_vol          = yearly%deficiency         *poly%MF_area
    yearly%effprecip_vol           = yearly%effprecip          *poly%MF_area      
    yearly%change_in_storage_vol   = yearly%change_in_storage  *poly%MF_area
    
   end subroutine convert_length_to_volume

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   subroutine calc_area(im)

    integer ::  ip, im
 
    scott_area         = 0.
    french_area        = 0.
    etna_area          = 0.
    patterson_area     = 0.
    kidder_area        = 0.
    moffet_area        = 0.
    mill_area          = 0.
    shackleford_area   = 0.
    scott_tailing_area = 0.

    alfalfa_irr_area   = 0.
    grain_irr_area     = 0.
    pasture_irr_area   = 0.
    et_noirr_area      = 0.
    noet_noirr_area    = 0.
	  water_area         = 0.
	  A_n_star_area      = 0.
	  A_SUB_area         = 0.
	  A_DRY_area         = 0.
	  G_n_star_area      = 0.
	  G_SUB_area         = 0.
	  G_DRY_area         = 0.
	  P_n_star_area      = 0.
	  P_SUB_area         = 0.
	  P_DRY_area         = 0.
    LU3_area           = 0.
    do ip = 1, npoly
    
       select case (poly(ip)%landuse)
       case (25)   ! alfalfa / grain
           if (poly(ip)%rotation==11) then
             if (poly(ip)%irr_type == 555) then               ! If n* (non-irrigated alfalfa)
               et_noirr_area = et_noirr_area + poly(ip)%MF_area	! Add area to ET/noIrr
               A_n_star_area = A_n_star_area + poly(ip)%MF_area  ! Add area to alfalfa n* sub category
             else if (poly(ip)%water_source == 4) then        ! If SUB (non-irrigated alfalfa)
           	   et_noirr_area = et_noirr_area + poly(ip)%MF_area	! Add area to ET/noIrr
               A_SUB_area = A_SUB_area + poly(ip)%MF_area	      ! Add area to  alfalfa sub-irrigated sub category
             else if (poly(ip)%water_source == 5) then        ! If DRY (non-irrigated alfalfa)
               et_noirr_area = et_noirr_area + poly(ip)%MF_area	! Add area to ET/noIrr
               A_DRY_area = A_DRY_area + poly(ip)%MF_area        ! Add area to alfalfa dry sub category
             else
             	 alfalfa_irr_area = alfalfa_irr_area + poly(ip)%MF_area    ! Add area to irrigated alfalfa category
             end if
           else if (poly(ip)%rotation==12) then
             if (poly(ip)%irr_type == 555) then               ! If n* (non-irrigated grain)
               et_noirr_area = et_noirr_area + poly(ip)%MF_area	! Add area to ET/noIrr
               G_n_star_area = G_n_star_area + poly(ip)%MF_area  ! Add area to  grain n* sub category
             else if (poly(ip)%water_source == 4) then        ! If SUB (non-irrigated grain)
           	   et_noirr_area = et_noirr_area + poly(ip)%MF_area	! Add area to ET/noIrr
               G_SUB_area = G_SUB_area + poly(ip)%MF_area	      ! Add area to grain sub-irrigated sub category
             else if (poly(ip)%water_source == 5) then        ! If DRY (non-irrigated grain)
               et_noirr_area = et_noirr_area + poly(ip)%MF_area	! Add area to ET/noIrr
               G_DRY_area = G_DRY_area + poly(ip)%MF_area        ! Add area to grain dry sub category
             else
             	 grain_irr_area = grain_irr_area + poly(ip)%MF_area	      ! Add area to irrigated grain category
             end if	  
           else
           	write(*,*)'SOMETHING IS ROTTEN IN DENMARK. ALFALFA-GRAIN ROTATION ERROR.'
           	write(800,*)'SOMETHING IS ROTTEN IN DENMARK. ALFALFA-GRAIN ROTATION ERROR.'
           	CALL EXIT
           end if
       case (2)    ! pasture
           if (poly(ip)%irr_type == 555) then               ! If n* (non-irrigated alfalfa/grain)
             et_noirr_area = et_noirr_area + poly(ip)%MF_area	! Add area to ET/noIrr
             P_n_star_area = P_n_star_area + poly(ip)%MF_area  ! Add area to n* sub category
           else if (poly(ip)%water_source == 4) then        ! If SUB (non-irrigated alfalfa/grain)
           	 et_noirr_area = et_noirr_area + poly(ip)%MF_area	! Add area to ET/noIrr
             P_SUB_area = P_SUB_area + poly(ip)%MF_area	      ! Add area to sub-irrigated sub category
           else if (poly(ip)%water_source == 5) then        ! If DRY (non-irrigated alfalfa/grain)
             et_noirr_area = et_noirr_area + poly(ip)%MF_area	! Add area to ET/noIrr
             P_DRY_area = P_DRY_area + poly(ip)%MF_area        ! Add area to dry sub category
           else                                             
             pasture_irr_area=pasture_irr_area + poly(ip)%MF_area      ! Add area to irrigated pasture category   
           endif
       case (3)    !ET_noIRR
         et_noirr_area  = et_noirr_area + poly(ip)%MF_area     ! Total area of ET/noIrr including n*, SUB, and DRY fields
         LU3_area  = LU3_area + poly(ip)%MF_area               ! Original area of Landuse = 3 in polygon input table    
       case (4)    !noET_noIRR
         noet_noirr_area  = noet_noirr_area + poly(ip)%MF_area
	   case (6)    !water
         water_area  = water_area + poly(ip)%MF_area
       end select

       select case ( poly(ip)%subwn)
        case (1)
         scott_area = scott_area + poly(ip)%MF_area 
        case (2)
         french_area =  french_area + poly(ip)%MF_area
         case (3)
       etna_area  =  etna_area + poly(ip)%MF_area
         case (4)
       patterson_area =  patterson_area + poly(ip)%MF_area
         case (5)
       kidder_area =  kidder_area + poly(ip)%MF_area
         case (6) 
       moffet_area =  moffet_area + poly(ip)%MF_area
         case (7)
       mill_area   =  mill_area + poly(ip)%MF_area
         case (8)
       shackleford_area = shackleford_area + poly(ip)%MF_area
         case (9)
       scott_tailing_area =  scott_tailing_area + poly(ip)%MF_area

       end select
	   
    enddo

    Total_area = alfalfa_irr_area + A_n_star_area + A_SUB_area + A_DRY_area + &
                                grain_irr_area + G_n_star_area + G_SUB_area + G_DRY_area + &
                                pasture_irr_area + P_n_star_area + P_SUB_area + P_DRY_area + &
                                LU3_area + noet_noirr_area + water_area
    write(60,'(i3,9F20.8)') im ,scott_area, french_area,  etna_area, patterson_area, kidder_area,  moffet_area,  mill_area, &
                            shackleford_area,  scott_tailing_area
    write(61,'(i3,6F20.8)') im, alfalfa_irr_area,  grain_irr_area,  pasture_irr_area,  et_noirr_area,  noet_noirr_area, water_area
    write(62,'(i4,16F20.4)') im, alfalfa_irr_area, A_n_star_area, A_SUB_area, A_DRY_area, &
                                grain_irr_area, G_n_star_area, G_SUB_area, G_DRY_area, &
                                pasture_irr_area, P_n_star_area, P_SUB_area, P_DRY_area, &
                                LU3_area, noet_noirr_area, water_area, Total_area
    
    alfalfa_irr_area  = alfalfa_irr_area * 0.000247105 ! Convert area from m^2 to acres
    A_n_star_area     = A_n_star_area    * 0.000247105 ! Convert area from m^2 to acres
    A_SUB_area        = A_SUB_area       * 0.000247105 ! Convert area from m^2 to acres
    A_DRY_area        = A_DRY_area       * 0.000247105 ! Convert area from m^2 to acres
    grain_irr_area    = grain_irr_area   * 0.000247105 ! Convert area from m^2 to acres
    G_n_star_area     = G_n_star_area    * 0.000247105 ! Convert area from m^2 to acres
    G_SUB_area        = G_SUB_area       * 0.000247105 ! Convert area from m^2 to acres
    G_DRY_area        = G_DRY_area       * 0.000247105 ! Convert area from m^2 to acres
    pasture_irr_area  = pasture_irr_area * 0.000247105 ! Convert area from m^2 to acres
    P_n_star_area     = P_n_star_area    * 0.000247105 ! Convert area from m^2 to acres
    P_SUB_area        = P_SUB_area       * 0.000247105 ! Convert area from m^2 to acres
    P_DRY_area        = P_DRY_area       * 0.000247105 ! Convert area from m^2 to acres
    LU3_area          = LU3_area         * 0.000247105 ! Convert area from m^2 to acres
    noet_noirr_area   = noet_noirr_area  * 0.000247105 ! Convert area from m^2 to acres
    water_area        = water_area       * 0.000247105 ! Convert area from m^2 to acres
    Total_area        = Total_area       * 0.000247105 ! Convert area from m^2 to acres
    
    write(63,'(i4,16F20.4)') im, alfalfa_irr_area, A_n_star_area, A_SUB_area, A_DRY_area, &
                                grain_irr_area, G_n_star_area, G_SUB_area, G_DRY_area, &
                                pasture_irr_area, P_n_star_area, P_SUB_area, P_DRY_area, &
                                LU3_area, noet_noirr_area, water_area, Total_area
   end subroutine calc_area
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  subroutine write_MODFLOW_SFR (im, nmonth, nsegs, SFR_Flows, drain_flow)
! In this subroutine the input file for the SFR package is created.  
  INTEGER :: im, i, j, nmonth, dummy, nsegs,  SR_width, Trib_width
  INTEGER,dimension(32,2) :: seg
  DOUBLE PRECISION, DIMENSION(1:32) :: SFR_Flows
  REAL, DIMENSION(nmonth) :: drain_flow
  
  
  seg(:,1) = (/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32/)    ! SFR Segment Number [nseg]
  seg(:,2) = (/3,3,4,5,9,8,8,9,10,12,12,14,14,23,17,17,19,19,22,26,22,23,25,25,26,30,29,29,30,0,0,0/)      ! SFR Inflow Segment [outseg]
  SR_width = 25
  Trib_width = 10

  if (im==1) then
    open(unit=213, file='SVIHM.sfr',Access = 'append', status='old')
    write(213,*)
    write(213,'(I4,A12)')nsegs,'  0  0  0  0'
  else
  	open(unit=213, file='SVIHM.sfr',Access = 'append', status='old')
    write(213,'(I4,A12)')nsegs,'  0  0  0  0'
  end if
  
  do j = 1, nsegs
    if (j==1) then  ! EF + SF Inflow  	    
      write(213,'(I3,A3,I3,A3,es10.2,A16)')j,'  1', seg(j,2),'  0',SFR_Flows(1),'  0  0  0  0.035'
      write(213,'(I4)')SR_width
      write(213,'(I4)')SR_width
    else if (j==2) then ! Sugar Creek Inflow 
      write(213,'(I3,A3,I3,A3,es10.2,A16)')j,'  1', seg(j,2),'  0', SFR_Flows(2),'  0  0  0  0.035'
      write(213,'(I4)')Trib_width
      write(213,'(I4)')Trib_width
    else if (j==3) then ! Last Tailings Segment 
      write(213,'(I3,A3,I3,A29)')j,'  1', seg(j,2),'  0  0.00E+00  0  0  0  0.035'
      write(213,'(I4)')SR_width
      write(213,'(I4)')SR_width
    else if (j==4 .or. j==5 .or. j==9 .or. j==10 .or. j==12 .or. j==14 .or. j==23 .or. j==25 .or. j==26 .or. j==30) then ! Scott River Segments
  	  if (j .le. 9) then
      	if (seg(j,2) .le. 9) then
          write(213,'(I3,A3,I3,A29)')j,'  1', seg(j,2),'  0  0.00E+00  0  0  0  0.035'
          write(213,'(I4)')SR_width
          write(213,'(I4)')SR_width
        else
      	  write(213,'(I3,A3,I4,A29)')j,'  1', seg(j,2),'  0  0.00E+00  0  0  0  0.035'
          write(213,'(I4)')SR_width
          write(213,'(I4)')SR_width
        end if
      else
        if (seg(j,2) .le. 9) then
          write(213,'(I4,A3,I3,A29)')j,'  1', seg(j,2),'  0  0.00E+00  0  0  0  0.035'
          write(213,'(I4)')SR_width
          write(213,'(I4)')SR_width
        else
      	  write(213,'(I4,A3,I4,A29)')j,'  1', seg(j,2),'  0  0.00E+00  0  0  0  0.035'
          write(213,'(I4)')SR_width
          write(213,'(I4)')SR_width
        end if
      end if
    else if (j==6) then ! French Creek Branch #1
      write(213,'(I3,A3,I3,A3,es10.2,A16)')j,'  1', seg(j,2),'  0', SFR_Flows(6),'  0  0  0  0.035'
      write(213,'(I4)')Trib_width
      write(213,'(I4)')Trib_width	
    else if (j==7) then ! French Creek Branch #2	
      write(213,'(I3,A3,I3,A3,es10.2,A16)')j,'  1', seg(j,2),'  0', SFR_Flows(7),'  0  0  0  0.035'
      write(213,'(I4)')Trib_width
      write(213,'(I4)')Trib_width	
    else if (j==8 .or. j==13 .or. j==17 .or. j==20 .or. j==22 .or. j==29) then ! Tributaries and Sloughs, Oro Fino Creek (no inflow)
      if (j .le. 9) then
      	if (seg(j,2) .le. 9) then
          write(213,'(I3,A3,I3,A29)')j,'  1', seg(j,2),'  0  0.00E+00  0  0  0  0.035'
          write(213,'(I4)')Trib_width
          write(213,'(I4)')Trib_width
        else
      	  write(213,'(I3,A3,I4,A29)')j,'  1', seg(j,2),'  0  0.00E+00  0  0  0  0.035'
          write(213,'(I4)')Trib_width
          write(213,'(I4)')Trib_width
        end if
      else
        if (seg(j,2) .le. 9) then
          write(213,'(I4,A3,I3,A29)')j,'  1', seg(j,2),'  0  0.00E+00  0  0  0  0.035'
          write(213,'(I4)')Trib_width
          write(213,'(I4)')Trib_width
        else
      	  write(213,'(I4,A3,I4,A29)')j,'  1', seg(j,2),'  0  0.00E+00  0  0  0  0.035'
          write(213,'(I4)')Trib_width
          write(213,'(I4)')Trib_width
        end if	
      end if
    else if (j==11) then ! Etna Creek
      write(213,'(I4,A3,I4,A3,es10.2,A16)')j,'  1', seg(j,2),'  0', SFR_Flows(11),'  0  0  0  0.035'
      write(213,'(I4)')Trib_width
      write(213,'(I4)')Trib_width	
    else if (j==15) then ! Johnson Creek
      write(213,'(I4,A3,I4,A3,es10.2,A16)')j,'  1', seg(j,2),'  0', SFR_Flows(15),'  0  0  0  0.035'
      write(213,'(I4)')Trib_width
      write(213,'(I4)')Trib_width
    else if (j==16) then ! Crystal Creek
      write(213,'(I4,A3,I4,A3,es10.2,A16)')j,'  1', seg(j,2),'  0', SFR_Flows(16),'  0  0  0  0.035'
      write(213,'(I4)')Trib_width
      write(213,'(I4)')Trib_width
    else if (j==18) then ! Patterson Creek
      write(213,'(I4,A3,I4,A3,es10.2,A16)')j,'  1', seg(j,2),'  0', SFR_Flows(18),'  0  0  0  0.035'
      write(213,'(I4)')Trib_width
      write(213,'(I4)')Trib_width
    else if (j==19) then ! Big Slough Drain inflow segment
      write(213,'(I4,A3,I4,A3,es10.2,A16)')j,'  1', seg(j,2),'  0', drain_flow(im),'  0  0  0  0.035'
      write(213,'(I4)')Trib_width
      write(213,'(I4)')Trib_width
    else if (j==21) then ! Kidder Creek
      write(213,'(I4,A3,I4,A3,es10.2,A16)')j,'  1', seg(j,2),'  0', SFR_Flows(21),'  0  0  0  0.035'
      write(213,'(I4)')Trib_width
      write(213,'(I4)')Trib_width
    else if (j==24) then ! Moffett Creek
      write(213,'(I4,A3,I4,A3,es10.2,A16)')j,'  1', seg(j,2),'  0', SFR_Flows(24),'  0  0  0  0.035'
      write(213,'(I4)')Trib_width
      write(213,'(I4)')Trib_width
    else if (j==27) then ! Mill Creek
      write(213,'(I4,A3,I4,A3,es10.2,A16)')j,'  1', seg(j,2),'  0', SFR_Flows(27),'  0  0  0  0.035'
      write(213,'(I4)')Trib_width
      write(213,'(I4)')Trib_width
    else if (j==28) then ! Shackleford Creek
      write(213,'(I4,A3,I4,A3,es10.2,A16)')j,'  1', seg(j,2),'  0', SFR_Flows(28),'  0  0  0  0.035'
      write(213,'(I4)')Trib_width
      write(213,'(I4)')Trib_width
    else if (j==31) then ! Farmers Ditch Diversion 
      write(213,'(I4,A3,I3,A6,es10.2,A16)')j,'  1', seg(j,2),'  3  0', SFR_Flows(31),'  0  0  0  0.035'
      write(213,*)'  2'
      write(213,*)'  2'  
    else if (j==32) then ! SVID Ditch Diversion
      write(213,'(I4,A3,I3,A7,es10.2,A16)')j,'  1', seg(j,2),'  10  0', SFR_Flows(32),'  0  0  0  0.035'
      write(213,*)'  2'
      write(213,*)'  2'      
    end if
  end do

  end subroutine  write_MODFLOW_SFR
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 subroutine write_SFR_template (im, nmonth, nsegs, SFR_Flows, drain_flow,SFR_Template)
! In this subroutine the input file for the SFR package is created.  
  INTEGER :: im, i, j, nmonth, dummy, nsegs,  SR_width, Trib_width
  INTEGER,dimension(32,2) :: seg
  DOUBLE PRECISION, DIMENSION (1:32) :: SFR_Flows
  CHARACTER(10) :: rough1,rough2,rough3
  CHARACTER(10)  ::  SFR_Template
  REAL, DIMENSION(nmonth) :: drain_flow
  
  seg(:,1) = (/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32/)    ! SFR Segment Number [nseg]
  seg(:,2) = (/3,3,4,5,9,8,8,9,10,12,12,14,14,23,17,17,19,19,22,26,22,23,25,25,26,30,29,29,30,0,0,0/)      ! SFR Inflow Segment [outseg]
  SR_width = 25
  Trib_width = 10
  rough1 = '@rough1  @'  ! Tailings
  rough2 = '@rough2  @'  ! Scott River
  rough3 = '@rough3  @'  ! Tributaies 

  if (SFR_Template=='UCODE') then
    open(unit=214, file='SVIHM_SFR.jtf',Access = 'append', status='old')         
    write(214,'(I4,A12)')nsegs,'  0  0  0  0'
  else 
    open(unit=214, file='SVIHM_SFR.tpl',Access = 'append', status='old')         
    write(214,'(I4,A12)')nsegs,'  0  0  0  0'  
  end if                                  
                                                                                 
!  if (im==1) then                                                               ! Keep in case the beginning of the transient data section doesn't start printing on a new line 
!    open(unit=214, file='SVIHM_SFR.jtf',Access = 'append', status='old')    ! Keep in case the beginning of the transient data section doesn't start printing on a new line
!    write(214,*)                                                                ! Keep in case the beginning of the transient data section doesn't start printing on a new line
!    write(214,'(I4,A12)')nsegs,'  0  0  0  0'                                   ! Keep in case the beginning of the transient data section doesn't start printing on a new line
!  else                                                                          ! Keep in case the beginning of the transient data section doesn't start printing on a new line
!  	open(unit=214, file='SVIHM_SFR.jtf',Access = 'append', status='old')     ! Keep in case the beginning of the transient data section doesn't start printing on a new line
!    write(214,'(I4,A12)')nsegs,'  0  0  0  0'                                   ! Keep in case the beginning of the transient data section doesn't start printing on a new line
!  end if                                                                        ! Keep in case the beginning of the transient data section doesn't start printing on a new line
  
  do j = 1, nsegs
    if (j==1) then  ! EF + SF Inflow  	    
      write(214,'(I3,A3,I3,A3,es10.2,A11,A10)')j,'  1', seg(j,2),'  0',SFR_Flows(1),'  0  0  0  ',rough1
      write(214,'(I4)')SR_width
      write(214,'(I4)')SR_width
    else if (j==2) then ! Sugar Creek Inflow 
      write(214,'(I3,A3,I3,A3,es10.2,A11,A10)')j,'  1', seg(j,2),'  0', SFR_Flows(2),'  0  0  0  ',rough1
      write(214,'(I4)')Trib_width
      write(214,'(I4)')Trib_width
    else if (j==3) then ! Last Tailings Segment 
      write(214,'(I3,A3,I3,A24,A10)')j,'  1', seg(j,2),'  0  0.00E+00  0  0  0  ',rough1
      write(214,'(I4)')SR_width
      write(214,'(I4)')SR_width
    else if (j==4 .or. j==5 .or. j==9 .or. j==10 .or. j==12 .or. j==14 .or. j==23 .or. j==25 .or. j==26 .or. j==30) then ! Scott River Segments
  	  if (j .le. 9) then
      	if (seg(j,2) .le. 9) then
          write(214,'(I3,A3,I3,A24,A10)')j,'  1', seg(j,2),'  0  0.00E+00  0  0  0  ',rough2
          write(214,'(I4)')SR_width
          write(214,'(I4)')SR_width
        else
      	  write(214,'(I3,A3,I4,A24,A10)')j,'  1', seg(j,2),'  0  0.00E+00  0  0  0  ',rough2
          write(214,'(I4)')SR_width
          write(214,'(I4)')SR_width
        end if
      else
        if (seg(j,2) .le. 9) then
          write(214,'(I4,A3,I3,A24,A10)')j,'  1', seg(j,2),'  0  0.00E+00  0  0  0  ',rough2
          write(214,'(I4)')SR_width
          write(214,'(I4)')SR_width
        else
      	  write(214,'(I4,A3,I4,A24,A10)')j,'  1', seg(j,2),'  0  0.00E+00  0  0  0  ',rough2
          write(214,'(I4)')SR_width
          write(214,'(I4)')SR_width
        end if
      end if
    else if (j==6) then ! French Creek Branch #1
      write(214,'(I3,A3,I3,A3,es10.2,A11,A10)')j,'  1', seg(j,2),'  0', SFR_Flows(6),'  0  0  0  ',rough3
      write(214,'(I4)')Trib_width
      write(214,'(I4)')Trib_width	
    else if (j==7) then ! French Creek Branch #2	
      write(214,'(I3,A3,I3,A3,es10.2,A11,A10)')j,'  1', seg(j,2),'  0', SFR_Flows(7),'  0  0  0  ',rough3
      write(214,'(I4)')Trib_width
      write(214,'(I4)')Trib_width	
    else if (j==8 .or. j==13 .or. j==17 .or. j==20 .or. j==22 .or. j==29) then ! Tributaries and Sloughs, Oro Fino Creek (no inflow)
      if (j .le. 9) then
      	if (seg(j,2) .le. 9) then
          write(214,'(I3,A3,I3,A24,A10)')j,'  1', seg(j,2),'  0  0.00E+00  0  0  0  ',rough3
          write(214,'(I4)')Trib_width
          write(214,'(I4)')Trib_width
        else
      	  write(214,'(I3,A3,I4,A24,A10)')j,'  1', seg(j,2),'  0  0.00E+00  0  0  0  ',rough3
          write(214,'(I4)')Trib_width
          write(214,'(I4)')Trib_width
        end if
      else
        if (seg(j,2) .le. 9) then
          write(214,'(I4,A3,I3,A24,A10)')j,'  1', seg(j,2),'  0  0.00E+00  0  0  0  ',rough3
          write(214,'(I4)')Trib_width
          write(214,'(I4)')Trib_width
        else
      	  write(214,'(I4,A3,I4,A24,A10)')j,'  1', seg(j,2),'  0  0.00E+00  0  0  0  ',rough3
          write(214,'(I4)')Trib_width
          write(214,'(I4)')Trib_width
        end if	
      end if
    else if (j==11) then ! Etna Creek
      write(214,'(I4,A3,I4,A3,es10.2,A11,A10)')j,'  1', seg(j,2),'  0', SFR_Flows(11),'  0  0  0  ',rough3
      write(214,'(I4)')Trib_width
      write(214,'(I4)')Trib_width	
    else if (j==15) then ! Johnson Creek
      write(214,'(I4,A3,I4,A3,es10.2,A11,A10)')j,'  1', seg(j,2),'  0', SFR_Flows(15),'  0  0  0  ',rough3
      write(214,'(I4)')Trib_width
      write(214,'(I4)')Trib_width
    else if (j==16) then ! Crystal Creek
      write(214,'(I4,A3,I4,A3,es10.2,A11,A10)')j,'  1', seg(j,2),'  0', SFR_Flows(16),'  0  0  0  ',rough3
      write(214,'(I4)')Trib_width
      write(214,'(I4)')Trib_width
    else if (j==18) then ! Patterson Creek
      write(214,'(I4,A3,I4,A3,es10.2,A11,A10)')j,'  1', seg(j,2),'  0', SFR_Flows(18),'  0  0  0  ',rough3
      write(214,'(I4)')Trib_width
      write(214,'(I4)')Trib_width
    else if (j==19) then ! Big Slough Drain inflow segment
      write(214,'(I4,A3,I4,A3,es10.2,A11,A10)')j,'  1', seg(j,2),'  0', drain_flow(im),'  0  0  0  ',rough3
      write(214,'(I4)')Trib_width
      write(214,'(I4)')Trib_width
    else if (j==21) then ! Kidder Creek
      write(214,'(I4,A3,I4,A3,es10.2,A11,A10)')j,'  1', seg(j,2),'  0', SFR_Flows(21),'  0  0  0  ',rough3
      write(214,'(I4)')Trib_width
      write(214,'(I4)')Trib_width
    else if (j==24) then ! Moffett Creek
      write(214,'(I4,A3,I4,A3,es10.2,A11,A10)')j,'  1', seg(j,2),'  0', SFR_Flows(24),'  0  0  0  ',rough3
      write(214,'(I4)')Trib_width
      write(214,'(I4)')Trib_width
    else if (j==27) then ! Mill Creek
      write(214,'(I4,A3,I4,A3,es10.2,A11,A10)')j,'  1', seg(j,2),'  0', SFR_Flows(27),'  0  0  0  ',rough3
      write(214,'(I4)')Trib_width
      write(214,'(I4)')Trib_width
    else if (j==28) then ! Shackleford Creek
      write(214,'(I4,A3,I4,A3,es10.2,A11,A10)')j,'  1', seg(j,2),'  0', SFR_Flows(28),'  0  0  0  ',rough3
      write(214,'(I4)')Trib_width
      write(214,'(I4)')Trib_width
    else if (j==31) then ! Farmers Ditch Diversion  
      write(214,'(I4,A3,I3,A6,es10.2,A16)')j,'  1', seg(j,2),'  3  0', SFR_Flows(31),'  0  0  0  0.035'
      write(214,*)'  2'
      write(214,*)'  2'  
    else if (j==32) then ! SVID Ditch Diversion
      write(214,'(I4,A3,I3,A7,es10.2,A16)')j,'  1', seg(j,2),'  10  0', SFR_Flows(32),'  0  0  0  0.035'
      write(214,*)'  2'
      write(214,*)'  2'      
    end if
  end do
  end subroutine  write_SFR_template

end module outputmodule
