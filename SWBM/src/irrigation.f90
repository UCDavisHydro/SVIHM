MODULE irrigationmodule
  
  use define_poly
  implicit none
  
  DOUBLE PRECISION:: kc_grain, kc_alfalfa,kc_alfalfa_mult, kc_grain_mult, kc_noirr
  DOUBLE PRECISION:: kc_pasture, kc_pasture_mult
  DOUBLE PRECISION:: irreff_flood, irreff_wl_LU25, irreff_cp_LU25, irreff_wl_LU2, irreff_cp_LU2
  DOUBLE PRECISION :: AV_REF_ET_1a, AV_REF_ET_1b, AV_REF_ET_2, REF_ET
  DOUBLE PRECISION :: precip_adjusted
  DOUBLE PRECISION :: EF_SF_Ratio, Sugar_Ratio, Johnson_Ratio, Crystal_Ratio, Patterson_Ratio
  INTEGER, parameter:: nsubwn = 9
  INTEGER, parameter:: nlanduse = 5
  
  LOGICAL :: irrigating 
  DOUBLE PRECISION, DIMENSION(nsubwn) :: streamflow_in, streamflow_out, sw_irr, subwnwell, subwnactualET
  DOUBLE PRECISION, DIMENSION(nsubwn) :: subwnirrig, subwnevapo, subwnrecharge, subwndeficiency, subwnmoisture
  DOUBLE PRECISION, DIMENSION(nlanduse) :: landuseirrig, landuseevapo, landusedeficiency
  DOUBLE PRECISION, DIMENSION(nlanduse) :: landusewell, landuserecharge,landuseactualET, landusemoisture
  DOUBLE PRECISION, DIMENSION (1:32) :: SFR_Flows
  
  contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE READ_KC_IRREFF 
  
    open(unit=10,file="irr_eff.txt",status="old")
    read(10,*)irreff_flood                     ! flood
    read(10,*)irreff_wl_LU25, irreff_cp_LU25   ! alfalfa/grain wheel line, alfalfa/grain center pivot
    read(10,*)irreff_wl_LU2, irreff_cp_LU2     ! pasture wheel line, pasture center pivot
    close (10)
    
    open(unit=11,file="crop_coeff_mult.txt",status="old")
    read(11,*)kc_alfalfa_mult, kc_grain_mult, kc_pasture_mult, kc_noirr
    close(11)
    write(*,'(A18,F4.2)') "kc_alfalfa_mult = ", kc_alfalfa_mult
    write(*,'(A16,F4.2)') "kc_grain_mult = ", kc_grain_mult
    write(*,'(A18,F4.2)') "kc_pasture_mult = ", kc_pasture_mult
    write(*,'(A11,F4.2)') "kc_noirr = ", kc_noirr
    write(800,'(A18,F4.2)') "kc_alfalfa_mult = ", kc_alfalfa_mult
    write(800,'(A16,F4.2)') "kc_grain_mult = ", kc_grain_mult
    write(800,'(A18,F4.2)') "kc_pasture_mult = ", kc_pasture_mult
    write(800,'(A11,F4.2)') "kc_noirr = ", kc_noirr
    return
  end subroutine read_kc_irreff
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE IRRIGATION(ip, imonth, jday, precip_adjusted)

  integer :: imonth, jday
  integer, intent(in) :: ip
  DOUBLE PRECISION :: precip_adjusted
  REAL             :: irreff_wl, irreff_cp
   
  if (sum(poly%irr_flag).ge.250) irrigating = .true.      ! If 20% of the fields are irrigating (by number, not area; 1251 irrigated fields), set logical to true

  daily(ip)%irrigation= 0.                                ! Reset daily irrigation value to zero
  daily(ip)%well=0.                                       ! Reset daily pumping value to zero

  select case (poly(ip)%landuse)
    case (25)   ! alfalfa / grain
      if(poly(ip)%rotation == 11) then
        daily(ip)%evapotrasp=REF_ET*Kc_alfalfa*kc_alfalfa_mult  ! Set ET to current value for the day
        irreff_wl = irreff_wl_LU25
        irreff_cp = irreff_cp_LU25
        if ((imonth==6 .and. jday.ge.25 ) .or. (imonth>6)) then  ! If  March 25 - August 31
          if ((daily(ip)%moisture.LT.(0.625*poly(ip)%WC8)) .or. (imonth==8 .and. jday.ge.15) .or. (imonth>8) .or. irrigating) then  ! If soil moisture is < 37.5% total soil moisture storage, or after May 15th, or 20% of fields have started irrigating  
            call IRRIGATION_RULESET(imonth, jday, ip, irreff_wl, irreff_cp)
          end if
        end if
      else if (poly(ip)%rotation == 12) then
        daily(ip)%evapotrasp=REF_ET*Kc_grain*kc_grain_mult  !Set ET to current value for the day
        irreff_wl = irreff_wl_LU25
        irreff_cp = irreff_cp_LU25
        if ((imonth==6 .and. jday.ge.16 ) .or. (imonth.ge.7 .and. imonth.le.9 ) .or. (imonth==10 .and. jday.le.10)) then  ! If  March 16 - July 10
          if ((daily(ip)%moisture.LT.(0.625*0.5*poly(ip)%WC8)) &
          .or. (imonth==8 .and. jday.ge.15) .or. (imonth>8) .or. irrigating) then  ! If soil moisture is < 18.75% of total soil moisture storage, or after May 15th, or 20% of fields have started irrigating	
      	    call IRRIGATION_RULESET(imonth, jday, ip, irreff_wl, irreff_cp)
      	  end if
      	end if
      end if
    case (2)    ! pasture
        daily(ip)%evapotrasp=REF_ET*Kc_pasture*kc_pasture_mult  !Set ET to current value for the day
        irreff_wl = irreff_wl_LU2
        irreff_cp = irreff_cp_LU2
        if ((imonth==7 .and. jday.ge.15 ) .or. (imonth.ge.8) .or. (imonth==0) .or. (imonth ==1 .and. jday.le.15)) then  ! If  April 15 - October 15
          if ((daily(ip)%moisture.LT.(0.45*0.5*poly(ip)%WC8)) &
           .or. (imonth==8 .and. jday.ge.15) .or. (imonth>8) .or. irrigating) then  ! If soil moisture is < 77.5% total moisture storage, or after May 15th, or 20% of fields have started irrigating
            call IRRIGATION_RULESET(imonth, jday, ip, irreff_wl, irreff_cp)
          end if
        end if
    case (3)    ! ET_noIRR
        call ET_noIRR(imonth, jday, ip)
    case (4)    ! noET_noIRR
        call noET_noIRR (imonth, jday, ip)
    case (6)    ! water landuse type
  	  ! do nothing
  end select
  
  end subroutine IRRIGATION

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
 SUBROUTINE IRRIGATION_RULESET(imonth, jday, ip, irreff_wl, irreff_cp)
   integer :: imonth, jday, ip
   REAL, intent(in)    :: irreff_wl, irreff_cp
   
   
   poly(ip)%irr_flag = 1  ! Field has started irrigating
   if (poly(ip)%irr_type==1) then ! Flood irrigation
     daily(ip)%irrigation=max (0.,(1/irreff_flood )*(daily(ip)%evapotrasp-precip_adjusted))   
     if (poly(ip)%water_source==1) then  ! Surface-water 
       if (poly(ip)%subwn == 1 .or. poly(ip)%subwn == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
         sw_irr(1) = sw_irr(1) + daily(ip)%irrigation * poly(ip)%area         ! Add daily irrigation to sw_irr counter
         sw_irr(9) = sw_irr(9) + daily(ip)%irrigation * poly(ip)%area         ! Add daily irrigation to sw_irr counter
       else
         sw_irr(poly(ip)%subwn) = sw_irr(poly(ip)%subwn) + daily(ip)%irrigation * poly(ip)%area  ! Add daily irrigation to sw_irr counter
       end if
       if (sw_irr(poly(ip)%subwn) > streamflow_in(poly(ip)%subwn)) then 
         if (poly(ip)%subwn == 1 .or. poly(ip)%subwn == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
           sw_irr(1) = sw_irr(1) - daily(ip)%irrigation * poly(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
           sw_irr(9) = sw_irr(9) - daily(ip)%irrigation * poly(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         else
           sw_irr(poly(ip)%subwn) = sw_irr(poly(ip)%subwn) - daily(ip)%irrigation * poly(ip)%area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         end if              
         daily(ip)%irrigation = 0  ! Irrigation set to zero when surface-water supplies are exceeded
       end if 
     else if (poly(ip)%water_source==2) then  ! Groundwater
       daily(ip)%well = daily(ip)%irrigation  ! All irrigation assigned to groundwater well     
     else if (poly(ip)%water_source==3) then  ! Mixed water source
       if (poly(ip)%subwn == 1 .or. poly(ip)%subwn == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
         sw_irr(1) = sw_irr(1) + daily(ip)%irrigation * poly(ip)%area         ! Add daily irrigation to sw_irr counter
         sw_irr(9) = sw_irr(9) + daily(ip)%irrigation * poly(ip)%area         ! Add daily irrigation to sw_irr counter
       else
         sw_irr(poly(ip)%subwn) = sw_irr(poly(ip)%subwn) + daily(ip)%irrigation * poly(ip)%area  ! Add daily irrigation to sw_irr counter
       end if
       if (sw_irr(poly(ip)%subwn) > streamflow_in(poly(ip)%subwn)) then 
         if (poly(ip)%subwn == 1 .or. poly(ip)%subwn == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
           sw_irr(1) = sw_irr(1) - daily(ip)%irrigation * poly(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
           sw_irr(9) = sw_irr(9) - daily(ip)%irrigation * poly(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         else
           sw_irr(poly(ip)%subwn) = sw_irr(poly(ip)%subwn) - daily(ip)%irrigation * poly(ip)%area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         end if              
         daily(ip)%well = daily(ip)%irrigation ! Irrigation assigned to well when surface-water supplies are exceeded
       end if
     else if (poly(ip)%water_source==4 .or. poly(ip)%water_source==5 .or. poly(ip)%water_source==6) then  ! Sub-irrigated, dry, or water
       daily(ip)%irrigation=0
     end if
   else if (poly(ip)%irr_type==2) then ! Wheel line irrigation
   	daily(ip)%irrigation=max (0.,(1/irreff_wl)*(daily(ip)%evapotrasp-precip_adjusted))   
   	if (poly(ip)%water_source==1) then  ! Surface-water 
       if (poly(ip)%subwn == 1 .or. poly(ip)%subwn == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
         sw_irr(1) = sw_irr(1) + daily(ip)%irrigation * poly(ip)%area         ! Add daily irrigation to sw_irr counter
         sw_irr(9) = sw_irr(9) + daily(ip)%irrigation * poly(ip)%area         ! Add daily irrigation to sw_irr counter
       else
         sw_irr(poly(ip)%subwn) = sw_irr(poly(ip)%subwn) + daily(ip)%irrigation * poly(ip)%area  ! Add daily irrigation to sw_irr counter
       end if
       if (sw_irr(poly(ip)%subwn) > streamflow_in(poly(ip)%subwn)) then 
         if (poly(ip)%subwn == 1 .or. poly(ip)%subwn == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
           sw_irr(1) = sw_irr(1) - daily(ip)%irrigation * poly(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
           sw_irr(9) = sw_irr(9) - daily(ip)%irrigation * poly(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         else
           sw_irr(poly(ip)%subwn) = sw_irr(poly(ip)%subwn) - daily(ip)%irrigation * poly(ip)%area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         end if              
         daily(ip)%irrigation = 0  ! Irrigation set to zero when surface-water supplies are exceeded
       end if 
     else if (poly(ip)%water_source==2) then  ! Groundwater
       daily(ip)%well = daily(ip)%irrigation  ! All irrigation assigned to groundwater well     
     else if (poly(ip)%water_source==3) then  ! Mixed water source
       if (poly(ip)%subwn == 1 .or. poly(ip)%subwn == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
         sw_irr(1) = sw_irr(1) + daily(ip)%irrigation * poly(ip)%area         ! Add daily irrigation to sw_irr counter
         sw_irr(9) = sw_irr(9) + daily(ip)%irrigation * poly(ip)%area         ! Add daily irrigation to sw_irr counter
       else
         sw_irr(poly(ip)%subwn) = sw_irr(poly(ip)%subwn) + daily(ip)%irrigation * poly(ip)%area  ! Add daily irrigation to sw_irr counter
       end if
       if (sw_irr(poly(ip)%subwn) > streamflow_in(poly(ip)%subwn)) then 
         if (poly(ip)%subwn == 1 .or. poly(ip)%subwn == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
           sw_irr(1) = sw_irr(1) - daily(ip)%irrigation * poly(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
           sw_irr(9) = sw_irr(9) - daily(ip)%irrigation * poly(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         else
           sw_irr(poly(ip)%subwn) = sw_irr(poly(ip)%subwn) - daily(ip)%irrigation * poly(ip)%area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         end if              
         daily(ip)%well = daily(ip)%irrigation ! Irrigation assigned to well when surface-water supplies are exceeded
       end if
     else if (poly(ip)%water_source==4 .or. poly(ip)%water_source==5 .or. poly(ip)%water_source==6) then  ! Sub-irrigated, dry, or water
       daily(ip)%irrigation=0
     end if
   else if (poly(ip)%irr_type==3) then ! Center Pivot irrigation
     daily(ip)%irrigation=max (0.,(1/irreff_cp)*(daily(ip)%evapotrasp-precip_adjusted))   
     if (poly(ip)%water_source==1) then  ! Surface-water 
       if (poly(ip)%subwn == 1 .or. poly(ip)%subwn == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
         sw_irr(1) = sw_irr(1) + daily(ip)%irrigation * poly(ip)%area         ! Add daily irrigation to sw_irr counter
         sw_irr(9) = sw_irr(9) + daily(ip)%irrigation * poly(ip)%area         ! Add daily irrigation to sw_irr counter
       else
         sw_irr(poly(ip)%subwn) = sw_irr(poly(ip)%subwn) + daily(ip)%irrigation * poly(ip)%area  ! Add daily irrigation to sw_irr counter
       end if
       if (sw_irr(poly(ip)%subwn) > streamflow_in(poly(ip)%subwn)) then 
         if (poly(ip)%subwn == 1 .or. poly(ip)%subwn == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
           sw_irr(1) = sw_irr(1) - daily(ip)%irrigation * poly(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
           sw_irr(9) = sw_irr(9) - daily(ip)%irrigation * poly(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         else
           sw_irr(poly(ip)%subwn) = sw_irr(poly(ip)%subwn) - daily(ip)%irrigation * poly(ip)%area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         end if              
         daily(ip)%irrigation = 0  ! Irrigation set to zero when surface-water supplies are exceeded
       end if 
     else if (poly(ip)%water_source==2) then  ! Groundwater
       daily(ip)%well = daily(ip)%irrigation  ! All irrigation assigned to groundwater well     
     else if (poly(ip)%water_source==3) then  ! Mixed water source
       if (poly(ip)%subwn == 1 .or. poly(ip)%subwn == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
         sw_irr(1) = sw_irr(1) + daily(ip)%irrigation * poly(ip)%area         ! Add daily irrigation to sw_irr counter
         sw_irr(9) = sw_irr(9) + daily(ip)%irrigation * poly(ip)%area         ! Add daily irrigation to sw_irr counter
       else
         sw_irr(poly(ip)%subwn) = sw_irr(poly(ip)%subwn) + daily(ip)%irrigation * poly(ip)%area  ! Add daily irrigation to sw_irr counter
       end if
       if (sw_irr(poly(ip)%subwn) > streamflow_in(poly(ip)%subwn)) then 
         if (poly(ip)%subwn == 1 .or. poly(ip)%subwn == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
           sw_irr(1) = sw_irr(1) - daily(ip)%irrigation * poly(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
           sw_irr(9) = sw_irr(9) - daily(ip)%irrigation * poly(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         else
           sw_irr(poly(ip)%subwn) = sw_irr(poly(ip)%subwn) - daily(ip)%irrigation * poly(ip)%area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         end if              
         daily(ip)%well = daily(ip)%irrigation ! Irrigation assigned to well when surface-water supplies are exceeded
       end if
     else if (poly(ip)%water_source==4 .or. poly(ip)%water_source==5 .or. poly(ip)%water_source==6) then  ! Sub-irrigated, dry, or water
       daily(ip)%irrigation=0
     end if
   else if (poly(ip)%irr_type==555) then ! Field has no water source
     daily(ip)%irrigation=0
   end if          
   return
 END SUBROUTINE IRRIGATION_RULESET

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  SUBROUTINE IRRIGATION_ILR(ip, imonth, jday, precip_adjusted)

  integer :: imonth, jday
  integer, intent(in) :: ip
  DOUBLE PRECISION :: precip_adjusted
  INTEGER, SAVE :: counter1 = 1, counter2 = 1
  REAL             :: irreff_wl, irreff_cp
  
  if (sum(poly%irr_flag).ge.250) irrigating = .true.      ! If 20% of the fields are irrigating (by number, not area; 1251 irrigated fields), set logical to true
  if (sum(sw_irr) .LT. sum(streamflow_in) .and. poly(ip)%ILR_Flag == 1) then
    poly(ip)%ILR_Active = .true.
  else if (sum(sw_irr) > sum(streamflow_in) .or. poly(ip)%ILR_Flag == 0) then
  	poly(ip)%ILR_Active = .false.
  else
  	write(*,*)'Invalid ILR Flag in Polygon Input File'
  	write(*,*)'Value of ILR Flag = ',poly(ip)%ILR_Flag
  	write(800,*)'Invalid ILR Flag in Polygon Input File'
  	write(800,*)'Value of ILR Flag = ',poly(ip)%ILR_Flag
  	call exit
  end if
  
  daily(ip)%irrigation= 0.                                ! Reset daily irrigation value to zero
  daily(ip)%well=0.                                       ! Reset daily pumping value to zero
  
  select case (poly(ip)%landuse)
    case (25)   ! alfalfa / grain
      if(poly(ip)%rotation == 11) then
        daily(ip)%evapotrasp=REF_ET*Kc_alfalfa*kc_alfalfa_mult  ! Set ET to current value for the day
        irreff_wl = irreff_wl_LU25
        irreff_cp = irreff_cp_LU25
        if ((imonth==6 .and. jday.ge.25 ) .or. (imonth>6)) then  ! If  March 25 - August 31
          if ((daily(ip)%moisture.LT.(0.625*poly(ip)%WC8)) .or. (imonth==8 .and. jday.ge.15) .or. (imonth>8) .or. irrigating) then  ! If soil moisture is < 37.5% total soil moisture storage, or after May 15th, or 20% of fields have started irrigating  
            call IRRIGATION_RULESET_ILR(imonth, jday, ip, irreff_wl, irreff_cp)
          end if
        end if
      else if (poly(ip)%rotation == 12) then
        daily(ip)%evapotrasp=REF_ET*Kc_grain*kc_grain_mult  !Set ET to current value for the day
        irreff_wl = irreff_wl_LU25
        irreff_cp = irreff_cp_LU25
        if ((imonth==6 .and. jday.ge.16 ) .or. (imonth.ge.7 .and. imonth.le.9 ) .or. (imonth==10 .and. jday.le.10)) then  ! If  March 16 - July 10
          if ((daily(ip)%moisture.LT.(0.625*0.5*poly(ip)%WC8)) &
          .or. (imonth==8 .and. jday.ge.15) .or. (imonth>8) .or. irrigating) then  ! If soil moisture is < 18.75% of total soil moisture storage, or after May 15th, or 20% of fields have started irrigating	
      	    call IRRIGATION_RULESET_ILR(imonth, jday, ip, irreff_wl, irreff_cp)
      	  end if
      	end if
      end if
    case (2)    ! pasture
        daily(ip)%evapotrasp=REF_ET*Kc_pasture*kc_pasture_mult  !Set ET to current value for the day
        irreff_wl = irreff_wl_LU2
        irreff_cp = irreff_cp_LU2
        if ((imonth==7 .and. jday.ge.15 ) .or. (imonth.ge.8) .or. (imonth==0) .or. (imonth ==1 .and. jday.le.15)) then  ! If  April 15 - October 15
          if ((daily(ip)%moisture.LT.(0.45*0.5*poly(ip)%WC8)) &
           .or. (imonth==8 .and. jday.ge.15) .or. (imonth>8) .or. irrigating) then  ! If soil moisture is < 77.5% total moisture storage, or after May 15th, or 20% of fields have started irrigating
            call IRRIGATION_RULESET_ILR(imonth, jday, ip, irreff_wl, irreff_cp)
          end if
        end if
    case (3)    ! ET_noIRR
        call ET_noIRR(imonth, jday, ip)
    case (4)    ! noET_noIRR
        call noET_noIRR (imonth, jday, ip)
    case (6)    ! water landuse type
  	  ! do nothing
  end select
  
  end subroutine IRRIGATION_ILR
  
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  SUBROUTINE IRRIGATION_RULESET_ILR(imonth, jday, ip, irreff_wl, irreff_cp)

  integer :: imonth, jday, ip
  REAL    :: irreff_wl, irreff_cp

  poly(ip)%irr_flag = 1
  if (poly(ip)%ILR_Active) then  ! Irrigation ruleset for when ILR is active
	  if (poly(ip)%irr_type==1 .or. poly(ip)%irr_type==555) then ! Flood irrigation or DRY
	    daily(ip)%irrigation=max (0.,(1/irreff_flood )*(daily(ip)%evapotrasp-precip_adjusted)*1.33)  ! Increase Irrigation by 33% (~ one additional irrigation)
	    if (poly(ip)%water_source==1 .or. poly(ip)%water_source==5) then  ! Surface-water or Dry
	      if (poly(ip)%subwn == 1 .or. poly(ip)%subwn == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
          sw_irr(1) = sw_irr(1) + daily(ip)%irrigation * poly(ip)%area         ! Add daily irrigation to sw_irr counter
          sw_irr(9) = sw_irr(9) + daily(ip)%irrigation * poly(ip)%area         ! Add daily irrigation to sw_irr counter
        else
          sw_irr(poly(ip)%subwn) = sw_irr(poly(ip)%subwn) + daily(ip)%irrigation * poly(ip)%area  ! Add daily irrigation to sw_irr counter
        end if
        if (sw_irr(poly(ip)%subwn) > streamflow_in(poly(ip)%subwn)) then 
          if (poly(ip)%subwn == 1 .or. poly(ip)%subwn == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
        	  sw_irr(1) = sw_irr(1) - daily(ip)%irrigation * poly(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
            sw_irr(9) = sw_irr(9) - daily(ip)%irrigation * poly(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
          else
        	  sw_irr(poly(ip)%subwn) = sw_irr(poly(ip)%subwn) - daily(ip)%irrigation * poly(ip)%area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplies have been exceeded
          end if              
          daily(ip)%irrigation = 0  ! Irrigation set to zero when surface-water supplies are exceeded 
        end if   
	    else if (poly(ip)%water_source==2 .or. poly(ip)%water_source==3) then  ! GW or Mixed water source
	      if (poly(ip)%subwn == 1 .or. poly(ip)%subwn == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
        	sw_irr(1) = sw_irr(1) + daily(ip)%irrigation * poly(ip)%area         ! Add daily irrigation to sw_irr counter
          sw_irr(9) = sw_irr(9) + daily(ip)%irrigation * poly(ip)%area         ! Add daily irrigation to sw_irr counter
        else
        	sw_irr( poly(ip)%subwn ) = sw_irr( poly(ip)%subwn ) + daily(ip)%irrigation * poly(ip)%area  ! Add daily irrigation to sw_irr counter
        end if 
        if ( sw_irr(poly(ip)%subwn)>streamflow_in(poly(ip)%subwn ) ) then
          if (poly(ip)%subwn == 1 .or. poly(ip)%subwn == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
            sw_irr(1) = sw_irr(1) - daily(ip)%irrigation * poly(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
            sw_irr(9) = sw_irr(9) - daily(ip)%irrigation * poly(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
          else
            sw_irr(poly(ip)%subwn) = sw_irr(poly(ip)%subwn) - daily(ip)%irrigation * poly(ip)%area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
          end if
          daily(ip)%irrigation=max (0.,(1/irreff_flood )*(daily(ip)%evapotrasp-precip_adjusted))  ! Convert irrigation rate back to normal (not increased by 33%)
          daily(ip)%well = daily(ip)%irrigation              
        end if
	    else if (poly(ip)%water_source==4) then  ! Sub-irrigated 	
        daily(ip)%irrigation=0
      end if
    else if (poly(ip)%irr_type==2) then ! Wheel line irrigation
      daily(ip)%irrigation=max (0.,(1/irreff_wl)*(daily(ip)%evapotrasp-precip_adjusted)*1.33)  ! Increase Irrigation by 33% (~ one additional irrigation)
      if (poly(ip)%water_source==1 .or. poly(ip)%water_source==5) then  ! Surface-water or Dry
	      if (poly(ip)%subwn == 1 .or. poly(ip)%subwn == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
          sw_irr(1) = sw_irr(1) + daily(ip)%irrigation * poly(ip)%area         ! Add daily irrigation to sw_irr counter
          sw_irr(9) = sw_irr(9) + daily(ip)%irrigation * poly(ip)%area         ! Add daily irrigation to sw_irr counter
        else
          sw_irr(poly(ip)%subwn) = sw_irr(poly(ip)%subwn) + daily(ip)%irrigation * poly(ip)%area  ! Add daily irrigation to sw_irr counter
        end if
        if (sw_irr(poly(ip)%subwn) > streamflow_in(poly(ip)%subwn)) then 
          if (poly(ip)%subwn == 1 .or. poly(ip)%subwn == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
        	  sw_irr(1) = sw_irr(1) - daily(ip)%irrigation * poly(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
            sw_irr(9) = sw_irr(9) - daily(ip)%irrigation * poly(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
          else
        	  sw_irr(poly(ip)%subwn) = sw_irr(poly(ip)%subwn) - daily(ip)%irrigation * poly(ip)%area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplies have been exceeded
          end if              
          daily(ip)%irrigation = 0  ! Irrigation set to zero when surface-water supplies are exceeded 
        end if   
	    else if (poly(ip)%water_source==2 .or. poly(ip)%water_source==3) then  ! GW or Mixed water source
	      if (poly(ip)%subwn == 1 .or. poly(ip)%subwn == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
        	sw_irr(1) = sw_irr(1) + daily(ip)%irrigation * poly(ip)%area         ! Add daily irrigation to sw_irr counter
          sw_irr(9) = sw_irr(9) + daily(ip)%irrigation * poly(ip)%area         ! Add daily irrigation to sw_irr counter
        else
        	sw_irr( poly(ip)%subwn ) = sw_irr( poly(ip)%subwn ) + daily(ip)%irrigation * poly(ip)%area  ! Add daily irrigation to sw_irr counter
        end if 
        if ( sw_irr(poly(ip)%subwn)>streamflow_in(poly(ip)%subwn ) ) then
          if (poly(ip)%subwn == 1 .or. poly(ip)%subwn == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
            sw_irr(1) = sw_irr(1) - daily(ip)%irrigation * poly(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
            sw_irr(9) = sw_irr(9) - daily(ip)%irrigation * poly(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
          else
            sw_irr(poly(ip)%subwn) = sw_irr(poly(ip)%subwn) - daily(ip)%irrigation * poly(ip)%area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
          end if
          daily(ip)%irrigation=max (0.,(1/irreff_wl)*(daily(ip)%evapotrasp-precip_adjusted))  ! Convert irrigation rate back to normal (not increased by 33%)
          daily(ip)%well = daily(ip)%irrigation              
        end if
	    else if (poly(ip)%water_source==4) then  ! Sub-irrigated 	
        daily(ip)%irrigation=0
      end if
    else if (poly(ip)%irr_type==3) then! Center pivot irrigation
      daily(ip)%irrigation=max (0.,(1/irreff_wl)*(daily(ip)%evapotrasp-precip_adjusted)*1.33)  ! Increase Irrigation by 33% (~ one additional irrigation)
      if (poly(ip)%water_source==1 .or. poly(ip)%water_source==5) then  ! Surface-water or Dry
	      if (poly(ip)%subwn == 1 .or. poly(ip)%subwn == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
          sw_irr(1) = sw_irr(1) + daily(ip)%irrigation * poly(ip)%area         ! Add daily irrigation to sw_irr counter
          sw_irr(9) = sw_irr(9) + daily(ip)%irrigation * poly(ip)%area         ! Add daily irrigation to sw_irr counter
        else
          sw_irr(poly(ip)%subwn) = sw_irr(poly(ip)%subwn) + daily(ip)%irrigation * poly(ip)%area  ! Add daily irrigation to sw_irr counter
        end if
        if (sw_irr(poly(ip)%subwn) > streamflow_in(poly(ip)%subwn)) then 
          if (poly(ip)%subwn == 1 .or. poly(ip)%subwn == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
        	  sw_irr(1) = sw_irr(1) - daily(ip)%irrigation * poly(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
            sw_irr(9) = sw_irr(9) - daily(ip)%irrigation * poly(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
          else
        	  sw_irr(poly(ip)%subwn) = sw_irr(poly(ip)%subwn) - daily(ip)%irrigation * poly(ip)%area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplies have been exceeded
          end if              
          daily(ip)%irrigation = 0  ! Irrigation set to zero when surface-water supplies are exceeded 
        end if   
	    else if (poly(ip)%water_source==2 .or. poly(ip)%water_source==3) then  ! GW or Mixed water source
	      if (poly(ip)%subwn == 1 .or. poly(ip)%subwn == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
        	sw_irr(1) = sw_irr(1) + daily(ip)%irrigation * poly(ip)%area         ! Add daily irrigation to sw_irr counter
          sw_irr(9) = sw_irr(9) + daily(ip)%irrigation * poly(ip)%area         ! Add daily irrigation to sw_irr counter
        else
        	sw_irr( poly(ip)%subwn ) = sw_irr( poly(ip)%subwn ) + daily(ip)%irrigation * poly(ip)%area  ! Add daily irrigation to sw_irr counter
        end if 
        if ( sw_irr(poly(ip)%subwn)>streamflow_in(poly(ip)%subwn ) ) then
          if (poly(ip)%subwn == 1 .or. poly(ip)%subwn == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
            sw_irr(1) = sw_irr(1) - daily(ip)%irrigation * poly(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
            sw_irr(9) = sw_irr(9) - daily(ip)%irrigation * poly(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
          else
            sw_irr(poly(ip)%subwn) = sw_irr(poly(ip)%subwn) - daily(ip)%irrigation * poly(ip)%area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
          end if
          daily(ip)%irrigation=max (0.,(1/irreff_wl)*(daily(ip)%evapotrasp-precip_adjusted))  ! Convert irrigation rate back to normal (not increased by 33%)
          daily(ip)%well = daily(ip)%irrigation              
        end if
	    else if (poly(ip)%water_source==4) then  ! Sub-irrigated 	
        daily(ip)%irrigation=0
      end if
    end if	
  else ! Irrigation ruleset for when ILR is not active
    if (poly(ip)%irr_type==1) then ! Flood irrigation
     daily(ip)%irrigation=max (0.,(1/irreff_flood )*(daily(ip)%evapotrasp-precip_adjusted))
    if (poly(ip)%water_source==1) then  ! Surface-water 
       if (poly(ip)%subwn == 1 .or. poly(ip)%subwn == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
         sw_irr(1) = sw_irr(1) + daily(ip)%irrigation * poly(ip)%area         ! Add daily irrigation to sw_irr counter
         sw_irr(9) = sw_irr(9) + daily(ip)%irrigation * poly(ip)%area         ! Add daily irrigation to sw_irr counter
       else
         sw_irr(poly(ip)%subwn) = sw_irr(poly(ip)%subwn) + daily(ip)%irrigation * poly(ip)%area  ! Add daily irrigation to sw_irr counter
       end if
       if (sw_irr(poly(ip)%subwn) > streamflow_in(poly(ip)%subwn)) then 
         if (poly(ip)%subwn == 1 .or. poly(ip)%subwn == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
           sw_irr(1) = sw_irr(1) - daily(ip)%irrigation * poly(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
           sw_irr(9) = sw_irr(9) - daily(ip)%irrigation * poly(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         else
           sw_irr(poly(ip)%subwn) = sw_irr(poly(ip)%subwn) - daily(ip)%irrigation * poly(ip)%area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         end if              
         daily(ip)%irrigation = 0  ! Irrigation set to zero when surface-water supplies are exceeded
       end if 
     else if (poly(ip)%water_source==2) then  ! Groundwater
       daily(ip)%well = daily(ip)%irrigation  ! All irrigation assigned to groundwater well     
     else if (poly(ip)%water_source==3) then  ! Mixed water source
       if (poly(ip)%subwn == 1 .or. poly(ip)%subwn == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
         sw_irr(1) = sw_irr(1) + daily(ip)%irrigation * poly(ip)%area         ! Add daily irrigation to sw_irr counter
         sw_irr(9) = sw_irr(9) + daily(ip)%irrigation * poly(ip)%area         ! Add daily irrigation to sw_irr counter
       else
         sw_irr(poly(ip)%subwn) = sw_irr(poly(ip)%subwn) + daily(ip)%irrigation * poly(ip)%area  ! Add daily irrigation to sw_irr counter
       end if
       if (sw_irr(poly(ip)%subwn) > streamflow_in(poly(ip)%subwn)) then 
         if (poly(ip)%subwn == 1 .or. poly(ip)%subwn == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
           sw_irr(1) = sw_irr(1) - daily(ip)%irrigation * poly(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
           sw_irr(9) = sw_irr(9) - daily(ip)%irrigation * poly(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         else
           sw_irr(poly(ip)%subwn) = sw_irr(poly(ip)%subwn) - daily(ip)%irrigation * poly(ip)%area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         end if              
         daily(ip)%well = daily(ip)%irrigation ! Irrigation assigned to well when surface-water supplies are exceeded
       end if
     else if (poly(ip)%water_source==4 .or. poly(ip)%water_source==5 .or. poly(ip)%water_source==6) then  ! Sub-irrigated, dry, or water
       daily(ip)%irrigation=0
     end if
   else if (poly(ip)%irr_type==2) then ! Wheel line irrigation
   	daily(ip)%irrigation=max (0.,(1/irreff_wl)*(daily(ip)%evapotrasp-precip_adjusted))   
   	if (poly(ip)%water_source==1) then  ! Surface-water 
       if (poly(ip)%subwn == 1 .or. poly(ip)%subwn == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
         sw_irr(1) = sw_irr(1) + daily(ip)%irrigation * poly(ip)%area         ! Add daily irrigation to sw_irr counter
         sw_irr(9) = sw_irr(9) + daily(ip)%irrigation * poly(ip)%area         ! Add daily irrigation to sw_irr counter
       else
         sw_irr(poly(ip)%subwn) = sw_irr(poly(ip)%subwn) + daily(ip)%irrigation * poly(ip)%area  ! Add daily irrigation to sw_irr counter
       end if
       if (sw_irr(poly(ip)%subwn) > streamflow_in(poly(ip)%subwn)) then 
         if (poly(ip)%subwn == 1 .or. poly(ip)%subwn == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
           sw_irr(1) = sw_irr(1) - daily(ip)%irrigation * poly(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
           sw_irr(9) = sw_irr(9) - daily(ip)%irrigation * poly(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         else
           sw_irr(poly(ip)%subwn) = sw_irr(poly(ip)%subwn) - daily(ip)%irrigation * poly(ip)%area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         end if              
         daily(ip)%irrigation = 0  ! Irrigation set to zero when surface-water supplies are exceeded
       end if 
     else if (poly(ip)%water_source==2) then  ! Groundwater
       daily(ip)%well = daily(ip)%irrigation  ! All irrigation assigned to groundwater well     
     else if (poly(ip)%water_source==3) then  ! Mixed water source
       if (poly(ip)%subwn == 1 .or. poly(ip)%subwn == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
         sw_irr(1) = sw_irr(1) + daily(ip)%irrigation * poly(ip)%area         ! Add daily irrigation to sw_irr counter
         sw_irr(9) = sw_irr(9) + daily(ip)%irrigation * poly(ip)%area         ! Add daily irrigation to sw_irr counter
       else
         sw_irr(poly(ip)%subwn) = sw_irr(poly(ip)%subwn) + daily(ip)%irrigation * poly(ip)%area  ! Add daily irrigation to sw_irr counter
       end if
       if (sw_irr(poly(ip)%subwn) > streamflow_in(poly(ip)%subwn)) then 
         if (poly(ip)%subwn == 1 .or. poly(ip)%subwn == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
           sw_irr(1) = sw_irr(1) - daily(ip)%irrigation * poly(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
           sw_irr(9) = sw_irr(9) - daily(ip)%irrigation * poly(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         else
           sw_irr(poly(ip)%subwn) = sw_irr(poly(ip)%subwn) - daily(ip)%irrigation * poly(ip)%area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         end if              
         daily(ip)%well = daily(ip)%irrigation ! Irrigation assigned to well when surface-water supplies are exceeded
       end if
     else if (poly(ip)%water_source==4 .or. poly(ip)%water_source==5 .or. poly(ip)%water_source==6) then  ! Sub-irrigated, dry, or water
       daily(ip)%irrigation=0
     end if
   else if (poly(ip)%irr_type==3) then ! Center Pivot irrigation
     daily(ip)%irrigation=max (0.,(1/irreff_cp)*(daily(ip)%evapotrasp-precip_adjusted))   
     if (poly(ip)%water_source==1) then  ! Surface-water 
       if (poly(ip)%subwn == 1 .or. poly(ip)%subwn == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
         sw_irr(1) = sw_irr(1) + daily(ip)%irrigation * poly(ip)%area         ! Add daily irrigation to sw_irr counter
         sw_irr(9) = sw_irr(9) + daily(ip)%irrigation * poly(ip)%area         ! Add daily irrigation to sw_irr counter
       else
         sw_irr(poly(ip)%subwn) = sw_irr(poly(ip)%subwn) + daily(ip)%irrigation * poly(ip)%area  ! Add daily irrigation to sw_irr counter
       end if
       if (sw_irr(poly(ip)%subwn) > streamflow_in(poly(ip)%subwn)) then 
         if (poly(ip)%subwn == 1 .or. poly(ip)%subwn == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
           sw_irr(1) = sw_irr(1) - daily(ip)%irrigation * poly(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
           sw_irr(9) = sw_irr(9) - daily(ip)%irrigation * poly(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         else
           sw_irr(poly(ip)%subwn) = sw_irr(poly(ip)%subwn) - daily(ip)%irrigation * poly(ip)%area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         end if              
         daily(ip)%irrigation = 0  ! Irrigation set to zero when surface-water supplies are exceeded
       end if 
     else if (poly(ip)%water_source==2) then  ! Groundwater
       daily(ip)%well = daily(ip)%irrigation  ! All irrigation assigned to groundwater well     
     else if (poly(ip)%water_source==3) then  ! Mixed water source
       if (poly(ip)%subwn == 1 .or. poly(ip)%subwn == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
         sw_irr(1) = sw_irr(1) + daily(ip)%irrigation * poly(ip)%area         ! Add daily irrigation to sw_irr counter
         sw_irr(9) = sw_irr(9) + daily(ip)%irrigation * poly(ip)%area         ! Add daily irrigation to sw_irr counter
       else
         sw_irr(poly(ip)%subwn) = sw_irr(poly(ip)%subwn) + daily(ip)%irrigation * poly(ip)%area  ! Add daily irrigation to sw_irr counter
       end if
       if (sw_irr(poly(ip)%subwn) > streamflow_in(poly(ip)%subwn)) then 
         if (poly(ip)%subwn == 1 .or. poly(ip)%subwn == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
           sw_irr(1) = sw_irr(1) - daily(ip)%irrigation * poly(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
           sw_irr(9) = sw_irr(9) - daily(ip)%irrigation * poly(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         else
           sw_irr(poly(ip)%subwn) = sw_irr(poly(ip)%subwn) - daily(ip)%irrigation * poly(ip)%area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         end if              
         daily(ip)%well = daily(ip)%irrigation ! Irrigation assigned to well when surface-water supplies are exceeded
       end if
     else if (poly(ip)%water_source==4 .or. poly(ip)%water_source==5 .or. poly(ip)%water_source==6) then  ! Sub-irrigated, dry, or water
       daily(ip)%irrigation=0
     end if
   else if (poly(ip)%irr_type==555) then ! Field has no water source
     daily(ip)%irrigation=0
   end if 
  end if    
  return
  END SUBROUTINE IRRIGATION_RULESET_ILR

!******************************************************************************************************************************************

  SUBROUTINE ET_noIRR(imonth, jday, ip) 
  
  integer :: imonth, jday, ip
                                                       
   daily(ip)%irrigation= 0.
   daily(ip)%well=0.
   daily(ip)%evapotrasp=kc_noirr*REF_ET
   if (daily(ip)%evapotrasp .ge. (before(ip)%moisture+precip_adjusted)) then 
	   daily(ip)%evapotrasp = before(ip)%moisture+precip_adjusted
	 end if
  return
  end subroutine ET_noIRR

!*************************************************************************************************************************************

  SUBROUTINE noET_noIRR(imonth, jday, ip)
  
    integer :: imonth, jday, ip
       
    daily(ip)%irrigation=0.
    daily(ip)%well=0.      
    daily(ip)%moisture=0.  
    daily(ip)%evapotrasp=0.
                                       
  return
  end SUBROUTINE noET_noIRR

 ! *************************************************************************************************************************************
  subroutine do_rotation(im)

    integer :: i, im, year
    integer :: rotcycle, ipr 
    integer :: ngrain
  
    ngrain = nrot/8
    year = im/12 + 1 
    rotcycle=mod(year,8)

    ipr = 0
    print*, ''
    write(*,'(a5,i2,a26,2i4)')"Year:", year,"   Grain Polygon ID Range:", rotcycle*ngrain+1, (rotcycle+1)*ngrain
    write(800,*) ''
    write(800,'(a5,i2,a26,2i4)')"Year:", year,"   Grain Polygon ID Range:", rotcycle*ngrain+1, (rotcycle+1)*ngrain
    do i = 1, npoly
      if (poly(i)%landuse == 25) then    
        ipr = ipr + 1
        poly(i)%rotation = 11 ! alfalfa
        if ( ipr>(rotcycle*ngrain) .and. ipr<=((rotcycle+1)*ngrain) ) then
          poly(i)%rotation = 12
        end if
      end if
    enddo

  end subroutine do_rotation
 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  subroutine read_streamflow(numdays)
    
    CHARACTER(10) :: date_dummy
    DOUBLE PRECISION :: EF, SF, Sugar, French, Etna, Johnson, Crystal, Patterson, Kidder, Moffett, Mill, Shackleford
    INTEGER :: numdays
    
    streamflow_in = 0.         ! Reset all streamflow to zero
    sw_irr = 0.                ! Reset surface-water irrigation to zero
    read(66,*)date_dummy,EF, SF, Sugar, French, Etna, Johnson, Crystal, Patterson, Kidder, Moffett, Mill, Shackleford  ! Read in average daily streamflow total for each subwatershed, m^3/day
    
    streamflow_in(1) = EF + SF + Sugar
    streamflow_in(2) = French
    streamflow_in(3) = Etna 
    streamflow_in(4) = Johnson + Crystal + Patterson 
    streamflow_in(5) = Kidder 
    streamflow_in(6) = Moffett 
    streamflow_in(7) = Mill 
    streamflow_in(8) = Shackleford 
    streamflow_in(9) = EF + SF + Sugar 
    
    streamflow_in = streamflow_in * numdays
    
    EF_SF_Ratio = (EF + SF)/(EF + SF + Sugar)
    Sugar_Ratio = Sugar/(EF + SF + Sugar)
    Johnson_Ratio = Johnson / (Johnson + Crystal + Patterson)
    Crystal_Ratio = Crystal / (Johnson + Crystal + Patterson)
    Patterson_Ratio = Patterson /  (Johnson + Crystal + Patterson)
    
    ! Set ratios to 0 if NaN, which will happen if the inflow is 0.
    if (isnan(EF_SF_Ratio)) EF_SF_Ratio= 0
    if (isnan(Sugar_Ratio)) Sugar_Ratio= 0
    if (isnan(Johnson_Ratio)) Johnson_Ratio= 0
    if (isnan(Crystal_Ratio)) Crystal_Ratio= 0
    if (isnan(Patterson_Ratio)) Patterson_Ratio= 0
    
  end subroutine read_streamflow
  
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  subroutine SFR_streamflow(numdays, imonth)
    INTEGER :: numdays, imonth    
    
    SFR_Flows = 0.            ! Reset all SFR flows to zero
    streamflow_out(:) = streamflow_in(:) - sw_irr(:)
    
    SFR_Flows(1)  = (streamflow_out(1) * EF_SF_Ratio) / numdays     ! EF + SF Inflow  	        
    SFR_Flows(2)  = (streamflow_out(1) * Sugar_Ratio) / numdays     ! Sugar Creek Inflow    
    SFR_Flows(6)  = (streamflow_out(2) * 0.5) / numdays              ! French Creek Branch #1
    SFR_Flows(7)  = (streamflow_out(2) * 0.5) / numdays              ! French Creek Branch #2
    SFR_Flows(11) =  streamflow_out(3)  / numdays                   ! Etna Creek           
    SFR_Flows(15) = (streamflow_out(4) * Johnson_Ratio) / numdays   ! Johnson Creek        
    SFR_Flows(16) = (streamflow_out(4) * Crystal_Ratio) / numdays   ! Crystal Creek        
    SFR_Flows(18) = (streamflow_out(4) * Patterson_Ratio) / numdays ! Patterson Creek      
    SFR_Flows(21) =  streamflow_out(5) / numdays                   ! Kidder Creek        
    SFR_Flows(24) =  streamflow_out(6) / numdays                    ! Moffett Creek        
    SFR_Flows(27) =  streamflow_out(7) / numdays                    ! Mill Creek           
    SFR_Flows(28) =  streamflow_out(8) / numdays                    ! Shackleford Creek
    if (imonth == 0 .or. imonth == 1 .or. imonth == 2 .or. imonth == 3 .or. imonth == 4 &
      .or. imonth == 5 .or. imonth == 6 .or. imonth == 11) then ! Ditches only active from April - July        
      SFR_Flows(31) = 0.
      SFR_Flows(32) = 0.
    else
      SFR_Flows(31) = 8.  * 2446.58                                 ! Farmers Ditch Diversion (~8 cfs total diversion, leakage rate is about 6 cfs, assumed 2 cfs consumptive use)
      SFR_Flows(32) = 16. * 2446.58                                 ! SVID Diversion (~16 cfs total diversion, leakage rate is about 14 cfs, assumed 2 cfs consumptive use)     	
    end if
  end subroutine SFR_streamflow
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  subroutine SFR_streamflow_w_MAR(numdays, imonth)
    INTEGER :: numdays, imonth    
    
    SFR_Flows = 0.            ! Reset all SFR flows to zero
    streamflow_out(:) = streamflow_in(:) - sw_irr(:)
    
    SFR_Flows(1)  = (streamflow_out(1) * EF_SF_Ratio) / numdays     ! EF + SF Inflow  	        
    SFR_Flows(2)  = (streamflow_out(1) * Sugar_Ratio) / numdays     ! Sugar Creek Inflow    
    SFR_Flows(6)  = (streamflow_out(2) * 0.5) / numdays              ! French Creek Branch #1
    SFR_Flows(7)  = (streamflow_out(2) * 0.5) / numdays              ! French Creek Branch #2
    SFR_Flows(11) =  streamflow_out(3)  / numdays                   ! Etna Creek           
    SFR_Flows(15) = (streamflow_out(4) * Johnson_Ratio) / numdays   ! Johnson Creek        
    SFR_Flows(16) = (streamflow_out(4) * Crystal_Ratio) / numdays   ! Crystal Creek        
    SFR_Flows(18) = (streamflow_out(4) * Patterson_Ratio) / numdays ! Patterson Creek      
    SFR_Flows(21) =  streamflow_out(5) / numdays                   ! Kidder Creek        
    SFR_Flows(24) =  streamflow_out(6) / numdays                    ! Moffett Creek        
    SFR_Flows(27) =  streamflow_out(7) / numdays                    ! Mill Creek           
    SFR_Flows(28) =  streamflow_out(8) / numdays                    ! Shackleford Creek    
    if (imonth == 0 .or. imonth == 1 .or. imonth == 2 .or. imonth == 3 .or. imonth == 11) then ! Ditches only active from April - July        
      SFR_Flows(31) = 0.
      SFR_Flows(32) = 0.
    else if (imonth == 4 .or. imonth == 5 .or. imonth == 6) then
      SFR_Flows(31) = 0.                                           ! No MAR Diversion from Farmer's Ditch
      SFR_Flows(32) = 42. * 2446.58                                 ! 42 cfs diversion from SVID
    else
      SFR_Flows(31) = 8.  * 2446.58                                 ! Farmers Ditch Diversion (~8 cfs total diversion, leakage rate is about 6 cfs, assumed 2 cfs consumptive use)
      SFR_Flows(32) = 16. * 2446.58                                 ! SVID Diversion (~16 cfs total diversion, leakage rate is about 14 cfs, assumed 2 cfs consumptive use)     	
    end if
  end subroutine SFR_streamflow_w_MAR
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
end module