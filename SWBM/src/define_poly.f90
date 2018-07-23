    MODULE define_poly

    type polygon
        INTEGER           :: subwn
        INTEGER           :: landuse
        INTEGER           :: irr_type
        REAL  :: area
        INTEGER           :: id_well
        INTEGER           :: water_source
        REAL  :: WC8
        INTEGER           :: rotation
        REAL  :: av_recharge
        INTEGER           :: irr_flag
        INTEGER           :: WL2CP_year
        INTEGER           :: ILR_Flag
        LOGICAL           :: ILR_Active
    end type
    
    type accumulator
    	REAL :: irrigation
    	REAL :: recharge
    	REAL :: well
    	REAL :: moisture
    	REAL :: evapotrasp
      REAL :: actualET
      REAL :: deficiency
      REAL :: change_in_storage
      REAL :: effprecip
      REAL :: budget
      INTEGER          :: daydef
      INTEGER          :: ET_active
      REAL :: irrigation_vol
    	REAL :: recharge_vol
    	REAL :: well_vol
    	REAL :: moisture_vol
    	REAL :: evapotrasp_vol
      REAL :: actualET_vol
      REAL :: deficiency_vol
      REAL :: change_in_storage_vol
    end type

    type well
        INTEGER :: well_name, poly_id, layer, well_row, well_col 
        REAL :: coordx, coordy, monthly_vol, monthly_rate
        REAL :: daily_well_vol, monthly_well_vol, monthly_well_rate
    end type
    
  INTEGER :: npoly, nrot, total_n_wells
  REAL :: RD_Mult
  type(polygon), allocatable, dimension(:) :: poly
  type(accumulator), allocatable, dimension(:):: monthly, daily, before, yearly
  type (well), allocatable, dimension(:) :: single_well
  
  contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     SUBROUTINE read_well                                                                           
                                                                                                    
     INTEGER i, ipoly, wellname, j                                                                  
                                                                                                    
     open (unit=535, file="well_summary_v1.02.txt", status="old")                                 
     open (unit=534, file="well_list_by_polygon_v1.02.txt", status="old")                                                  
                                                                                           
     allocate(single_well(total_n_wells))                                                           
                                                                 
     read(535,*)                                                                                               
     do i=1, total_n_wells                                                                          
      read(535,*)single_well(i)%well_name, single_well(i)%layer ,single_well(i)%well_row, single_well(i)%well_col, &
       single_well(i)%coordx, single_well(i)%coordy                                                              
                                                                                                    
! With this, we create a list that says that well "i" belongs to polygon "j"                        
     enddo                                                                                          
!     do ip = 1, npoly                                                                              
     poly%id_well = 0                                                                               
                                                                                                    
     read(534,*)  
     write(800,*)'Polygon_ID  Well_Number  Well_ID'
     do i = 1, 1254 ! 1254 is total number of alfalfa/grain and pasture polygons                                                                                                                     
       read(534,*) ip, wellname                                                                     
       do j = 1, total_n_wells                                                                      
         if (wellname==single_well(j)%well_name) poly(ip)%id_well = j                             
       enddo                                                                                        
       write(800,*) ip, poly(ip)%id_well, wellname                                                   
     enddo                                                                                          
 

     END subroutine read_well
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     SUBROUTINE readpoly

       INTEGER i, idummy

!       read(*,*)npoly
!       npoly= 2119

       allocate(poly(npoly), monthly(npoly), daily(npoly), yearly(npoly))
       allocate( before(npoly))
       daily%irrigation = 0.
       daily%daydef = 0
       call zero_before

       open(unit=10,file="polygons_table_v1.02.txt",status="old")
!       open(unit=10,file="polygons_table_v1.02_SW_only.txt",status="old")
       read(10,*)
       nrot = 0

       write(800,*)"Polygon_ID Subwatershed Landuse Irr_Type Polygon_Area Water_Source WC8 WL2CP_year ILR_Flag"
       do i=1, npoly
         read(10,*)idummy, poly(i)%subwn,poly(i)%landuse, poly(i)%irr_type,    &
                poly(i)%area, poly(i)%water_source, &
                poly(i)%WC8, poly(i)%WL2CP_year, poly(i)%ILR_Flag
         if (poly(i)%landuse==25 .and. poly(i)%water_source==1) poly(i)%water_source = 3   ! Alfalfa/Grain is never irrigated with surface-water only. Irrigation type is kept the same
         if (poly(i)%landuse == 25 .or. poly(i)%landuse == 3) then
           poly(i)%WC8 = poly(i)%WC8 * RD_Mult    ! Scale root zone depth by multiplier for alfalfa/grain and native veg (pasture fixed at 4 ft since WC8 is multiplied by 0.5 during the irrigation call)
         end if
         if ( poly(i)%landuse == 25 ) nrot = nrot + 1
         if (poly(i)%irr_type == 999) then 
           poly(i)%irr_type = 2                ! Change unknown irrigation type to wheel line
         endif
          
         if (poly(i)%irr_type == 555) then       ! Change non-irrigated field to dry irrigation type
           poly(i)%water_source = 5
         endif
          
         if (poly(i)%water_source == 999) then   ! Change unknown water source to groundwater
           poly(i)%water_source = 2
         endif
         write(800,'(i6,i3,i4,i5,f20.5,i5,f20.5,i6,i3)')idummy ,poly(i)%subwn,poly(i)%landuse,poly(i)%irr_type, &
         poly(i)%area, poly(i)%water_source, poly(i)%WC8, poly(i)%WL2CP_year, poly(i)%ILR_Flag
       enddo

       close(10)
       
       write(*,*) nrot, " polygons do grain/alfalfa rotation"
       write(*,*) total_n_wells, " irrigation wells"
       write(800,*)' '
       write(800,*) nrot, " polygons do grain/alfalfa rotation"
       write(800,*) total_n_wells, " irrigation wells"
       write(800,*)' '
       return
	   
     end subroutine readpoly

     subroutine zero_month
     
         monthly%irrigation = 0.
         monthly%recharge   = 0.
         monthly%well       = 0.
         monthly%moisture   = 0.
         monthly%evapotrasp = 0.
         monthly%actualET   = 0.
         monthly%deficiency = 0.
     
     end subroutine zero_month

 subroutine zero_year
     
         yearly%irrigation = 0.
         yearly%recharge   = 0.
         yearly%well       = 0.
         yearly%moisture   = 0.
         yearly%evapotrasp = 0.
         yearly%actualET   = 0.
         yearly%deficiency = 0.
     
     end subroutine zero_year
    
     subroutine zero_before
     
         before%irrigation = 0.
         before%recharge   = 0.
         before%well       = 0.
         before%moisture   = 0.
         before%evapotrasp = 0.
         before%actualET   = 0.
         before%deficiency = 0.
         
     end subroutine zero_before
     
 subroutine Update_Irr_Type(im)
   
   integer :: i, im, year
   
   year = im/12 + 1991     
   
   do i = 1, npoly
      if (poly(i)%WL2CP_year .LE. year .and. poly(i)%WL2CP_year .NE. 0) then    ! If WL2CP Year
        poly(i)%irr_type = 3                                                    ! Change irrigation type to center pivot
      end if
    enddo
    
     
     end subroutine Update_Irr_Type  
    
  end MODULE
