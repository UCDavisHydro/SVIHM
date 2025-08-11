
#Generate input files for SVIHM

library(lubridate)
library(stringr)
library(dplyr)
library(DBI)
#spatial tools
library(raster) 
library(rpostgis)
library(rgdal)
library(postGIStools) # pull from gis server
library(rgeos) # for buffer function
## dates in R are treated as YYYY-MM-DD for convenience. Dates in the model input files are DD-MM-YYYY.

rm(list = ls())

# Scenario Selection ------------------------------------------------------

# Future Climate Scenario
future_climate = "2070WMW" # "basecase", "2030" #"2070CT" "2070DEW" "2070WMW"


# Recharge and flow scenarios
recharge_scenario = "Basecase" # Can be Basecase/MAR/ILR/MAR_ILR/MAR_ILR_max
    if(recharge_scenario == "MAR_ILR_max"){max_infil_rate_for_unknowns = 0.035}
flow_scenario = "Basecase" # Can be Basecase/Flow_Lims/FlowLimsMAR. Flow limits on stream diversion specified in "available ratio" table.

# Irrigation demand: Different from the kc_CROP_mult values in crop_coeff_mult.txt, which is used for calibrating.
irr_demand_mult = 1 # Can be 1 (Basecase) or < 1 or > 1 (i.e., reduced or increased irrigation; assumes land use change)
# set nat veg kc under Land Use Scenario.

# Month and day of the final day of alfalfa irrigation season. 
# Default is Aug 31, 8/31
alf_irr_stop_mo = 8 # Month as month of year (7 = July)
alf_irr_stop_day = 31
early_cutoff_flag = "AllYears" # default is "AllYears" ; alt is "DryYearsOnly" for 91, 92, 94, 01, 09, 13, 14 and 18
# Convert to month of wy (Oct=1, Nov=2, ..., Jul = 10, Aug=11, Sep=0)
if(alf_irr_stop_mo<9){alf_irr_stop_mo = alf_irr_stop_mo + 3
}else{alf_irr_stop_mo = alf_irr_stop_mo - 9}

# Reservoir scenario
reservoir_scenario = "Basecase"#"French"#"Shackleford" #"Basecase" #"Shackleford","French", "Etna", "South_Fork"
reservoir_plus_pipeline = FALSE    # set pipeline status
reservoir_capacity =   "Basecase" #59.504 * 150 * 7.5 # Basecase or,  59.504 or 119.01 (30 or 60 cfs/day in AF) * 150 (days of dry season) * n years
reservoir_start = "empty" # "empty" "full" or a fraction of capacity, or a number of AF
dry_season_release_cfs = 30 # typically 30 or 60 cfs

# BDAs scenario
BDAs_scenario = "Basecase" # Can be Basecase/Tributaries/All_Streams/Scott_R_Mainstem
if(tolower(BDAs_scenario) != "basecase"){ stream_bed_elev_increase = 0.5} # set average stream bed elevation increase

# Irrigation Efficiency scenario
irr_eff_scenario = "Basecase" # Basecase, or set irr efficiency increase or decrease amount (not applied to flood irrigation)

#Land use scenario. 
landuse_scenario ="basecase" # Default: basecase. For attribution study: major_natveg
if(landuse_scenario=="major_natveg"){ # Default: 0.6. Set at 1.0 for major natveg scenarios. 
  # natveg_kc = 0.6
  natveg_kc = 1.0
  extinction_depth_value = 10 # 4.5 # extinction depth outside the discharge zone
} else if(tolower(landuse_scenario)=="basecase"){
  natveg_kc = 0.6
} 
# landuse_scenario_detail = "native veg, gw and mixed fields, outside adj"
# landuse_scenario_detail = "native veg outside adj"
# landuse_scenario_detail = "native veg, gw and mixed fields, inside adj"
# landuse_scenario_detail = "native veg inside adj"
# landuse_scenario_detail = "native veg, gw and mixed fields, all cultivated fields"
# landuse_scenario_detail = "native veg all cultivated fields"



# Overall scenario identifier. Also makes the directory name; must match folder
scenario_name = "WY22_71_cf2070WMW" # "WY22_71_cf2070DEW" "WY22_71_cf2070CT" "WY22_71_cf2030" "WY22_71_basecase"
# scenario_name = "mar_ilr" # "ilr" "mar"
# scenario_name = "mar_ilr_max_0.019" # Options: 0.035, 0.003, or 0.019 (the arithmetic mean) or 0.01 (the geometric mean)
# scenario_name = "mar_ilr_flowlims"#"flowlims"
# scenario_name = "irrig_0.8"#"irrig_0.9" #
# scenario_name = "alf_irr_stop_jul10"
# scenario_name = "alf_irr_stop_aug01"
# scenario_name = "alf_irr_stop_aug01_dry_yrs_only"
# scenario_name = "alf_irr_stop_aug15"
# scenario_name = "alf_irr_stop_aug15_dry_yrs_only"
# scenario_name = "natveg_outside_adj"
# scenario_name = "natveg_gwmixed_outside_adj"
# scenario_name = "natveg_inside_adj"
# scenario_name = "natveg_gwmixed_inside_adj"
# scenario_name = "natveg_all"
# scenario_name = "natveg_gwmixed_all"
# scenario_name = "reservoir_shackleford" # "reservoir_etna" "reservoir_sfork" "reservoir_shackleford"
# scenario_name = "reservoir_pipeline_etna"
# scenario_name = "reservoir_etna_29KAF"
# scenario_name = "reservoir_etna_134kAF_60cfs"
# scenario_name = "reservoir_pipeline_etna_29KAF"
# scenario_name = "reservoir_pipeline_etna_134kAF_60cfs"
# scenario_name = "bdas_all_streams" # "bdas_tribs" "bdas_scott_r"
# scenario_name = "irr_eff_improve_0.2"
# scenario_name = "natveg_all_et_check_1.0nvkc_10m_ext"



# SETUP -------------------------------------------------------------------


# 1) Set drives for collecting all SWBM input files and SVIHM modflow files

# 1a) Set project directory. 
#This code allows it to automatically detect the location of this R script.
isRStudio <- Sys.getenv("RSTUDIO") == "1"
if(isRStudio == TRUE){ library(rstudioapi); svihm_dir <- dirname(dirname(getActiveDocumentContext()$path))}
if(isRStudio == FALSE){ library(here); svihm_dir <- dirname(here::here("Update_SVIHM_Inputs.R"))}

# 1b) Set directories for data used in update and output file locations

## Data used in update
Stream_Regression_dir = file.path(svihm_dir, "Streamflow_Regression_Model")
input_files_dir = file.path(svihm_dir, "SVIHM_Input_Files")
time_indep_dir = file.path(svihm_dir, "SVIHM_Input_Files", "time_independent_input_files")
ref_data_dir = file.path(svihm_dir, "SVIHM_Input_Files", "reference_data")
## Directory used to archive the precip and ET files for different scenarios
scenario_dev_dir = file.path(svihm_dir, "SVIHM_Input_Files", "Scenario_Development")
# Directory for connecting to the database
dms_dir = file.path(dirname(svihm_dir), "SiskiyouGSP2022", "Data_Management_System")
# Folder for collecting outputs from various scenarios for comparison plots
results_dir = file.path(svihm_dir, "R_Files","Post-Processing","Results")
#Connect to Siskiyou DB (for generating SVIHM.hob. And precip, eventually ET and streamflow)
source(file.path(dms_dir, "connect_to_db.R"))


## Directories for running the scenarios (files copied at end of script)

# New file architecture
scenario_dir = file.path(svihm_dir, "Scenarios",scenario_name)
SWBM_file_dir = scenario_dir
MF_file_dir = scenario_dir
# Old file architecture
# SWBM_file_dir = file.path(svihm_dir, "SWBM", scenario_name)
# MF_file_dir = file.path(svihm_dir, "MODFLOW",scenario_name)


#SET MODEL RUN DATES
start_year = 2021 # WY 2022
end_year = start_year + 50 # through WY of this year
model_whole_water_years = TRUE

if(model_whole_water_years)
{start_month = "10"; start_day = "01"; end_month = "09"; end_day = "30"} 
if(!model_whole_water_years) {print("please define month and day for start and end of model run period")}

#Generate date vectors and number of stress periods
model_start_date = as.Date(paste(start_year, start_month, start_day, sep = "-"))
model_end_date = as.Date(paste(end_year, end_month, end_day, sep = "-"))
model_days = seq(from = model_start_date, to = model_end_date, by = "days")
model_months = seq(from = model_start_date, to = model_end_date, by = "month")
# Calculate number of days (time steps) in each month (stress period)
model_end_date_plus_one = as.Date(paste(end_year, as.numeric(end_month)+1, end_day, sep = "-"))
model_months_plus_one = seq(from = model_start_date, to = model_end_date_plus_one, by = "month")
num_days = as.numeric(diff(model_months_plus_one)) #number of days in each stress period/month

num_stress_periods = length(model_months)


#SET DATES FOR past climate record used to as future model data
# Using 2 words to refer to the two time periods
# 50-year climate record composed of past data: the "record" or stitched record
# 50-year period, WY 2022-2071: the "model" period

future_climate_years = read.csv(file.path(ref_data_dir, "model_climate_50yr.csv"))

#Generate date vectors and number of stress periods (months)
for(i in 1:nrow(future_climate_years)){
  record_yr = future_climate_years$climate_year[i]
  record_yr_days = seq(as.Date(paste(record_yr-1, 10, 1, sep = "-")), 
                       as.Date(paste(record_yr, 9, 30, sep = "-")),
                       by = "days")
  record_yr_months = seq(as.Date(paste(record_yr-1, 10, 1, sep = "-")), 
                         as.Date(paste(record_yr, 9, 30, sep = "-")),
                         by = "months")
  if(i==1){
    record_days = record_yr_days
    record_months = record_yr_months
  }
  if(i>1){
    record_days = c(record_days, record_yr_days)
    record_months = c(record_months, record_yr_months)
  }
}

# Calculate number of days (time steps) in each month (stress period)
rm_floor = floor_date(record_months, unit = "month")
rm_ceiling = ceiling_date(record_months, unit = "month")

record_num_days = as.numeric(rm_ceiling - rm_floor) #number of days in each stress period/month
record_num_stress_periods = length(record_months)

# check:
# record_num_days and num_days should be identical vectors
# record_num_stress_periods and num_stress_periods should be identical vectors

#.#############################################################################
#### CHANGE FACTOR CALCS ------------------------------------------------

if(tolower(future_climate) != "basecase"){
  ## Read in daily change factor files
  # Sources (2030 and 2070 central tendency): https://data.cnra.ca.gov/dataset/climate-change-projections-wsip-2030-2070
  # Sources (2070 extremes updated scenarios): https://data.cnra.ca.gov/dataset/extreme-climate-change-scenarios-for-water-supply-planning
  # Source (streamflow change factors)
  
  # Read in change factor data (ET and precip, streams)
  stm_cf = read.csv(file.path(ref_data_dir, "HUC8_18020108_MonthlyChangeFactors.csv"))
  stm_cf$ModelDate = as.Date(stm_cf$ModelDate, format = "%m/%d/%Y")
  # et_precip_cf = read.csv(file.path(ref_data_dir, "dwr_climate_change_factors_et_precip.csv"))
  # colnames(et_precip_cf) = c(colnames(et_precip_cf)[1:3],
  #                            "Precip2030", "ET2030", "Precip2070CT", "ET2070CT",
  #                            "Precip2070DEW", "ET2070DEW", "Precip2070WMW", "ET2070WMW")
  # etp_dates = matrix(unlist(strsplit(et_precip_cf$ModelDate, split = " ")),
  #                    nrow = nrow(et_precip_cf), ncol = 2, byrow = T )
  # et_precip_cf$ModelDate = as.Date(etp_dates[,1], format = "%m/%d/%Y")
  
  # # Area-weighted fractions for each overlapping vic cell
  # vic_grid = readOGR(dsn = ref_data_dir, layer = "i04_VICGRid")
  # # svihm_domain = get_postgis_query(siskiyou_spatial, "SELECT * FROM svihm_domain", geom_name = "geom")
  # vic_grid = spTransform(vic_grid, crs(svihm_domain))
  # vic_grid_scott = vic_grid[svihm_domain,]
  # # plot(vic_grid_scott)
  # # plot(svihm_domain, add=T)
  # pointLabel(x = gCentroid(vic_grid_scott, byid=T)@coords, labels = vic_grid_scott$VICGrid_ID)
  # #calculate overlap area for each cell
  # for(i in 1:length(vic_grid_scott$VICGrid_ID)){
  #   cell_id = vic_grid_scott$VICGrid_ID[i]
  #   cell = vic_grid_scott[vic_grid_scott$VICGrid_ID==cell_id,]
  #   vic_grid_scott$overlap_area_m3[i] = area(gIntersection(cell, svihm_domain))
  # }
  # vic_grid_scott$area_weight = round(vic_grid_scott$overlap_area_m3/area(svihm_domain), 3)
  # #Create dataframe with the weights
  # vic_grid_weights = vic_grid_scott@data[,c("VICGrid_ID","area_weight")]
  # #Write table
  # write.csv(vic_grid_weights, file = file.path(ref_data_dir, "vic_grid_weights_scott.csv"))
  
  #Read in VIC grid spatial weights
  vic_grid_weights = read.csv(file = file.path(ref_data_dir, "vic_grid_weights_scott.csv"))
  
  # Subset our giant table to the cells overlapping the valley and time periods in the stitched record
  # min_record_wy = min(future_climate_years$climate_year); max_record_wy = max(future_climate_years$climate_year)
  # etp_scott = et_precip_cf[et_precip_cf$VICGrid_ID %in% vic_grid_weights$VICGrid_ID &
  #                            et_precip_cf$ModelDate >= min(rm_floor) &
  #                            et_precip_cf$ModelDate < max(rm_ceiling) ,]
    # write.csv(etp_scott, file.path(ref_data_dir, "et_precip_cf_scott_wy_91_11.csv"))
  
  etp_scott = read.csv(file.path(ref_data_dir, "et_precip_cf_scott_wy_91_11.csv"))
  etp_scott$X = NULL; etp_scott$OID = NULL
  
  # Create 1 averaged precip and ET change factor for each month
  etp_scott_avgd = data.frame(ModelDate = as.Date(unique(etp_scott$ModelDate)),
                                "Precip2030"=NA, "ET2030"=NA,
                                "Precip2070CT"=NA, "ET2070CT"=NA,
                                "Precip2070DEW"=NA, "ET2070DEW"=NA,
                                "Precip2070WMW"=NA, "ET2070WMW"=NA)
  for(i in 1:nrow(etp_scott_avgd)){
    hist_date = etp_scott_avgd$ModelDate[i]
    etp_month = etp_scott[etp_scott$ModelDate==hist_date,]
    etp_month$weights = vic_grid_weights$area_weight[match(etp_month$VICGrid_ID, 
                                                           vic_grid_weights$VICGrid_ID)]
    etp_scott_avgd[i,2:9] = apply(X = etp_month[,3:10] * etp_month$weights, 
                                    MARGIN = 2,
                                    FUN = sum)
  }
  
  precip_col_index = grep(pattern = paste0("Precip",future_climate), 
                          x = colnames(etp_scott_avgd))
  et_col_index = grep(pattern = paste0("ET",future_climate), 
                          x = colnames(etp_scott_avgd))
  stm_col_index = grep(pattern = paste0("MonthlyFactor",future_climate), 
                       x = colnames(stm_cf))
}


#### SWBM INPUTS ------------------------------------------------

# Copy files that don't change if the time period gets extended 

# crop_coeff_mult.txt
# daily_out.txt
# irr_eff.txt
# MAR_Fields.txt
# No_Flow_SVIHM.txt
# Discharge_Zone_Cells.txt
# polygons_table.txt
# Recharge_Zones_SVIHM.txt
# well_list_by_polygon.txt
# well_summary.txt

  copy_these_files = c( "daily_out.txt",  "irr_eff.txt", "MAR_Fields.txt",
                       "No_Flow_SVIHM.txt", "Recharge_Zones_SVIHM.txt",
                       "well_list_by_polygon.txt", "well_summary.txt")

if(landuse_scenario %in% c("basecase","Basecase")){ copy_these_files = c(copy_these_files, "polygons_table.txt")}
if(natveg_kc==0.6){copy_these_files = c(copy_these_files, "crop_coeff_mult.txt")}
# if(tolower(recharge_scenario) != "mar_ilr_expanded"){ copy_these_files = c(copy_these_files, "MAR_Fields.txt")}


setwd(time_indep_dir)
file.copy(copy_these_files, SWBM_file_dir)

#Copy and rename discharge zone cells
file.copy(from = file.path(ref_data_dir,"Discharge_Zone_Cells.txt"), 
          to = file.path(SWBM_file_dir,"ET_Zone_Cells.txt"))


# CALIBRATION FILES -------------------------------------------------------

# TO DO: figure out these calibration files. They feed into the SWBM.
 # SFR_PEST_TPL.txt
 # SFR_UCODE_JTF.txt
 # SVIHM_ETS_template.txt
 # SVIHM_SFR_template.txt
 # SVIHM_WEL_template.txt

 
copy_these_files = c("SFR_PEST_TPL.txt", "SFR_UCODE_JTF.txt", "SVIHM_SFR_template.txt",
                     "SVIHM_ETS_template.txt", 'SVIHM_WEL_template.txt')

setwd(time_indep_dir)
file.copy(copy_these_files, SWBM_file_dir)



# crop_coeff_mult.txt -----------------------------------------------------


if(natveg_kc!=0.6){
  
  kc_alfalfa_mult = "1.05"
  kc_grain_mult = "1.00"
  kc_pasture_mult = "1.05"
  if(natveg_kc<1){kc_noirr = str_pad(natveg_kc, width = 4, pad = 0, side = "right")}
  if(natveg_kc>=1){kc_noirr = formatC(as.numeric(natveg_kc), format = 'f', flag='0', digits = 2)}

  notes_string="!kc_alfalfa_mult, kc_grain_mult, kc_pasture_mult, kc_noirr"
  
  kc_string = paste(kc_alfalfa_mult, kc_grain_mult,
               kc_pasture_mult, natveg_kc, 
               notes_string, sep = "    ")

write.table(kc_string, file = file.path(SWBM_file_dir, "crop_coeff_mult.txt"),
            sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)

  
  }



#  Drains_m3day.txt and Drains_initial_m3day.txt --------------------------------------

drains_vector = c("#Initial Drain Flow", rep(0, num_stress_periods))

write.table(drains_vector, file = file.path(SWBM_file_dir, "Drains_m3day.txt"),
            sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)
write.table(drains_vector, file = file.path(SWBM_file_dir, "Drains_initial_m3day.txt"),
            sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)


# ET_Cells_extinction_depth.txt,  polygons_table.txt ------------------------------------------------------------

if( !(landuse_scenario %in% c("basecase","Basecase"))){ 
  #If landuse is not basecase, amend polygons table
  

# ___polygons.txt ------------------------------------------------------------

  
  # Run spatial analysis
  
  # read in datasets
  poly = readOGR(dsn = ref_data_dir, layer = "Landuse_20190219")
  # adj = get_postgis_query(siskiyou_spatial, "SELECT * FROM scott_adjudicated_area", geom_name = "geom")
  # three_basins = get_postgis_query(siskiyou_spatial, "SELECT * FROM three_basin_boundaries_2018", geom_name = "geom")
  # basin = three_basins[three_basins$Basin_Name == "SCOTT RIVER VALLEY",]
  # writeOGR(adj, dsn = ref_data_dir, layer = "Adjudicated Area",driver = "ESRI Shapefile")
  adj = readOGR(dsn = ref_data_dir, layer = "Adjudicated Area")
  
  poly = spTransform(poly, crs(adj))
  
  #Calculate the fraction of each polygon *inside* the adjudicated zone.
  poly$fraction_in_adj = 0 # initialize new column
  
  for(i in 1:max(poly$Polynmbr)){
    selector = poly$Polynmbr==i
    field = poly[selector,]
    if(!gIsValid(field)){
      field = gBuffer(field, width = 0) # fix invalid geoms and warn about it
      print(paste("polygon number",i,"invalid"))} 
    
    if(gIntersects(adj, field)){
      overlap_poly = raster::intersect(field, adj)
      poly$fraction_in_adj[selector] = round(area(overlap_poly) / area(field), digits = 3) # otherwise get leftover digit junk
    }
  }
  
  # Divide fields
  
  # Note: this does not preserve "notes" column
  poly_column_classes = c(rep("integer",4),
                          "numeric","integer","numeric","numeric",
                          "integer","integer","character",
                          rep("NULL",16)) # get rid of empty columns in the text file
  
  poly_tab = read.table(file.path(time_indep_dir,"polygons_table.txt"),
                        header = T, comment.char = "!", 
                        fill = T, sep = "\t", colClasses = poly_column_classes)
  colnames(poly_tab) = c("Field_ID",colnames(poly_tab)[2:11])
  
  poly$wat_source_orig = poly_tab$Water_Source[match(poly$Polynmbr, poly_tab$Field_ID)]
  
  in_adj_threshold = 0.05 # lower numbers mean, just a sliver overlapping are included
  fields_inside_adj = poly$Polynmbr[poly$fraction_in_adj > in_adj_threshold]
  fields_outside_adj = poly$Polynmbr[!(poly$Polynmbr %in% fields_inside_adj)]
  
  
  fields_inside_adj = poly[poly$fraction_in_adj > in_adj_threshold,]
  fields_outside_adj =  poly[poly$fraction_in_adj <= in_adj_threshold,]
  
  # # make acreage tables
  # # make land use color table, associate colors with poly tab land uses (seems the most up to date)
  # inside_acreage = aggregate(area(fields_inside_adj),
  #                            by = list(fields_inside_adj$landuse_color,
  #                                      fields_inside_adj$wat_source_orig), FUN = sum)
  # colnames(inside_acreage) = c("color","wat_source","m_sq_in")
  # outside_acreage = aggregate(area(fields_outside_adj),
  #                             by = list(fields_outside_adj$landuse_color,
  #                                       fields_outside_adj$wat_source_orig), FUN = sum)
  # colnames(outside_acreage) = c("color","wat_source","m_sq_out")
  # acreage1 = expand.grid(color = lu_df$color, wat_source =unique(poly$wat_source_orig))
  # acreage2 = merge(x = acreage1, y = inside_acreage, by=c("color", "wat_source"), all.x = T)
  # acreage3 = merge(x = acreage2, y = outside_acreage, by=c("color", "wat_source"), all.x = T)
  # acreage3$landuse = lu_descrip[match(acreage3$color, lu_color)]
  # acreage3$wat_source_descrip = wat_source_df$descrip[match(acreage3$wat_source,wat_source_df$ws_code)]
  # acreage3$acres_in = round(acreage3$m_sq_in / 4046.86)
  # acreage3$acres_out = round(acreage3$m_sq_out / 4046.86)
  # acreage = acreage3[,5:8]
  # write.csv(acreage, "adj_zone_acreage.csv")
  # 
  # lu_descrip = c("Alfalfa/Grain","Pasture",
  #                "ET/No Irrigation","No ET/No Irrigation")
  # lu_color = c("forestgreen","darkolivegreen2","wheat","red")
  # poly$landuse_color2b = lu_color[match(poly$LNDU_SIM2b, lu_descrip)]
  # poly$landuse_color1 = lu_color[match(poly$LNDU_SIM1, lu_descrip)]
  
  # poly_saved = poly
  # # poly = poly_saved
  # poly_tab_saved = poly_tab
  
  # png(filename = "parcels_adj_zone_for_scenario.png", height=9.5, width = 6.2, units = "in", res = 300)
  # plot(basin, lwd = 2, main = "Fields considered within Adjudicated Area for SVIHM Scenario")
  # plot(fields_inside_adj, add=T, col = rgb(0,0,1, 0.5))
  # plot(fields_outside_adj, add=T, col = rgb(0,1,0,0.3))
  # plot(adj, add=T, border = "red", lwd = 2)#col = rgb(1,0,0,0.3))
  # legend(x = "bottomleft",
  #        lwd = c(2, 2, NA,NA), pch = c(NA,NA, 15, 15),
  #        col = c("black","red","blue", "green"),
  #        legend = c("Groundwater Basin", "Adjudicated Zone", "Inside Fields", "Outside Fields"))
  # dev.off()
  
  # sq_m_per_acre = 4046.86
  # sum(area(fields_inside_adj))/ unique(area(basin)) #sum(area(poly))
  # sum(area(fields_inside_adj)) / sq_m_per_acre # convert to acres
  # sum(area(fields_outside_adj))/ sum(area(poly)) #unique(area(basin)) #
  # sum(area(fields_outside_adj)) / sq_m_per_acre # convert to acres
  
  # Explore/visualize
  # area(adj)/sum(area(poly))*100 # covers 20.0% of the land area in this shapefile
  # area(adj)/area(basin)*100 # covers 15.7% of the land area of the basin
  # plot(poly)
  # plot(adj, add=T, col=rgb(.5,.5,.5,0.5))
  
  
  #Data exploration - visualize 3 categories of field. All in, all out, or overlapping
  
  # fields_totally_in_adj = poly[poly$fraction_in_adj==1,]
  # fields_totally_outside_adj = poly[poly$fraction_in_adj==0,]
  # fields_partially_in_adj = poly[poly$fraction_in_adj !=0 & poly$fraction_in_adj !=1 ,]
  
  # sum(area(fields_totally_in_adj)) / sq_m_per_acre # convert to acres
  # sum(area(fields_totally_outside_adj)) / sq_m_per_acre # convert to acres
  # sum(area(fields_partially_in_adj)) / sq_m_per_acre # convert to acres
  
  
  # png(filename = "parcels_adj_zone.png", height=9.5, width = 6.2, units = "in", res = 300)
  # plot(basin, lwd = 2, main = "Relation of Parcels to the Adjudicated Zone")
  # plot(fields_totally_in_adj, add=T, col = rgb(0,0,1,.7), border = "gray")
  # plot(fields_totally_outside_adj, add=T, col = rgb(0,1,0,.7), border = "gray")
  # plot(fields_partially_in_adj, add=T, col = rgb(1,0,0,.7), border = "gray")
  # legend(x = "bottomleft",
  #        col = c("black","blue","green","red"),
  #        pch=c(NA,rep(15,3)),
  #        lwd = c(2,rep(NA,3)),
  #        legend = c("Basin Boundary","Completely Inside", "Completely Outside", "Overlaps Zone Border"))
  # dev.off()
  
  # # fractions of the basin area
  # "Totally inside adjudicated zone:"
  # sum(area(fields_totally_in_adj)) / sum(area(poly))
  # sum(area(fields_totally_in_adj)) / area(basin)
  # "Totally outside adjudicated zone:"
  # sum(area(fields_totally_outside_adj)) / sum(area(poly))
  # sum(area(fields_totally_outside_adj)) / area(basin)
  # "Overlapping adjudicated zone boundary:"
  # sum(area(fields_partially_in_adj))/ sum(area(poly))
  # sum(area(fields_partially_in_adj))/ area(basin)
  
  
  # #Color by water source - initialize columns
  # poly$wat_source_from_svihm = NA
  # poly$wat_source_from_svihm_color = NA
  # 
  # # make water source color table
  # wat_source = c(1,2,3,4,5,999)
  # wat_source_descrip = c("SW","GW","Mixed", "Sub-irrigated","Dry","Unknown")
  # wat_source_color = c("dodgerblue","firebrick2","darkorchid1","green","yellow","gray")
  # 
  # wat_source_df = data.frame(ws_code = wat_source,
  #                            descrip = wat_source_descrip,
  #                            color = wat_source_color)
  # #match codes and colors
  # poly$wat_source_from_svihm = poly_tab$Water_Source[match(poly$Polynmbr, poly_tab$Field_ID)]
  # poly$wat_source_from_svihm_color = wat_source_df$color[match(poly$wat_source_from_svihm, wat_source_df$ws_code)]
  # 
  # # #plot
  # png(filename = "parcels_wat_source.png", height=9.5, width = 6.2, units = "in", res = 300)
  # plot(basin, lwd = 2, main = "Irrigation Water Sources")
  # plot(poly, add=T, col = poly$wat_source_from_svihm_color, border = "darkgray")
  # legend(x = "bottomleft", legend = wat_source_df$descrip,
  #        col = wat_source_df$color, pch = rep(15,6))
  # 
  # # plot(adj, add=T, col= rgb(0.5,0.5,0.5,0.5))
  # dev.off()
  
  #Color by land use - initialize columns
  # poly$landuse = NA
  # poly$landuse_color = NA
  # 
  # #Land use key:
  # # alfalfa = 25; palture = 2; ET_noIrr = 3 (native veg, assumes kc of 0.6);  noET_noIrr = 4; water = 6
  # 
  # # make a land use/crop type color table
  lu = c(25,2,3,4,6)
  lu_descrip = c("Alfalfa","Pasture","ET_noIrr","noET_noIrr", "Water")
  lu_color = c("forestgreen","darkolivegreen2","wheat","red","dodgerblue")

  lu_df = data.frame(lu_code = lu,
                     descrip = lu_descrip,
                     color = lu_color)
  #match codes and colors
  # poly$landuse_from_svihm = poly_tab$Landuse[match(poly$Polynmbr, poly_tab$Field_ID)]
  # poly$landuse_color = lu_df$color[match(poly$landuse_from_svihm, lu_df$lu_code)]
  #
  # # #plot
  # png(filename = "parcels_landuse_basecase.png", height=9.5, width = 6.2, units = "in", res = 300)
  # plot(basin, lwd = 2, main = "Land Use / Crop Type: Basecase")
  # plot(poly, add=T, col = poly$landuse_color, border = "darkgray", lwd=0.5)
  # legend(x = "bottomleft",
  #        legend = c(lu_df$descrip[1:2],"ET, No Irr. (Native Veg.)",
  #                   "No ET, No Irr. (e.g. Tailings)","Water"),
  #        col = lu_df$color, pch = rep(15,6))
  # 
  # # plot(adj, add=T, col= rgb(0.5,0.5,0.5,0.5))
  # dev.off()
  
  
  # #Water source fraction of parcel area  
  # wat_source_area = aggregate(area(poly), by = list(poly$wat_source_from_svihm), FUN = "sum")
  # wat_source_area$frac_parcels = round(wat_source_area$x / sum(area(poly)),digits = 3)
  # wat_source_area$frac_basin = round(wat_source_area$x / unique(area(basin)),digits = 3)
  
  # #proportion of fields in each category 
  # par(mfrow = c(3,1))
  # pie(summary(poly_tab$Landuse), main = "all fields")
  # pie(summary(poly_tab$Landuse[poly_tab$Field_ID %in% fields_inside_adj]), 
  #     main = paste("fields in adj,", in_adj_threshold, "overlap needed"))
  # pie(summary(poly_tab$Landuse[!(poly_tab$Field_ID %in% fields_inside_adj)]), 
  #     main = paste("fields outside adj,", in_adj_threshold, "overlap needed"))
  
  
  
  # # find weird parcels
  # pdf(file = "weird_parcel_finder.pdf",width = 8.5, height = 11)
  # for(i in 1:length(poly$Polynmbr)){
  #   field = poly[poly$Polynmbr==i,]
  #   # print(area(field))
  #   if(area(field) > 10^5 & i %in% fields_partially_in_adj$Polynmbr) {
  #     plot(basin, main = i)
  #     plot(field, col = "pink", border = "red", lwd = 2,add=T)
  #     
  #   }
  # }
  # dev.off()
  
  ## Weird: 714. a couple others.
  #in the find-weird-parcels analysis, I decided to exclude parcels with 5% or less of their area in the adjudicated zone.
  
  
  # Amend the SVIHM polygons table and write
  poly_tab_amended = poly_tab
  
  #Land use key:
  # alfalfa = 25
  # palture = 2
  # ET_noIrr = 3 (native veg, assumes kc of 0.6 default or 1.0 for major-nat-veg scenarios w/ shallow water table)
  # noET_noIrr = 4
  # water = 6
  
  # # make water source color table
  # wat_source = c(1,2,3,4,5,999)
  # wat_source_descrip = c("SW","GW","Mixed", "Sub-irrigated","Dry","Unknown")
  # wat_source_color = c("dodgerblue","firebrick2","darkorchid1","green","yellow","gray")
  
  # Note: Does not convert urban land or water surfaces to native vegetation. Converts only alfalfa and pasture (25, 2)
  if(landuse_scenario_detail == "native veg outside adj"){
    poly_tab_amended$Landuse[poly_tab$Landuse %in% c(25, 2) &
                               poly_tab$Field_ID %in% fields_outside_adj$Polynmbr] = 3
  } else if(landuse_scenario_detail == "native veg, gw and mixed fields, outside adj"){
    poly_tab_amended$Landuse[poly_tab$Landuse %in% c(25, 2) &
                               poly_tab$Field_ID %in% fields_outside_adj$Polynmbr &
                               poly_tab$Water_Source %in% c(2, 3)] = 3
  }else if(landuse_scenario_detail=="native veg inside adj"){
    poly_tab_amended$Landuse[poly_tab$Landuse %in% c(25, 2) &
                               poly_tab$Field_ID %in% fields_inside_adj$Polynmbr] = 3
  } else if(landuse_scenario_detail=="native veg, gw and mixed fields, inside adj"){
    poly_tab_amended$Landuse[poly_tab$Landuse %in% c(25, 2)  &
                               poly_tab$Field_ID %in% fields_inside_adj$Polynmbr &
                               poly_tab$Water_Source %in% c(2, 3)] = 3
  } else if(landuse_scenario_detail=="native veg all cultivated fields") {
    poly_tab_amended$Landuse[poly_tab$Landuse %in% c(25, 2)] = 3 
  } else if(landuse_scenario_detail=="native veg, gw and mixed fields, all cultivated fields"){
    poly_tab_amended$Landuse[poly_tab$Landuse %in% c(25, 2)  &
                               poly_tab$Water_Source %in% c(2, 3)] = 3
  }
  
  
  
  # png(filename = "parcels_landuse_natveg_gwmixed_outside_adj.png", height=9.5, width = 6.2, units = "in", res = 300)
  # # png(filename = "parcels_landuse_natveg_outside_adj.png", height=9.5, width = 6.2, units = "in", res = 300)
  # #match codes and colors
  # poly$landuse_from_svihm = poly_tab_amended$Landuse[match(poly$Polynmbr, poly_tab_amended$Field_ID)]
  # poly$landuse_color = lu_df$color[match(poly$landuse_from_svihm, lu_df$lu_code)]
  # # 
  # # 
  # plot(basin, lwd = 2, main = "Land Use / Crop Type: NV GWM OA")
  # plot(poly, add=T, col = poly$landuse_color,
  #      border = "darkgray", lwd=0.5)
  # legend(x = "bottomleft",
  #        legend = c(lu_df$descrip[1:2],"ET, No Irr. (Native Veg.)",
  #                   "No ET, No Irr. (e.g. Tailings)","Water"),
  #        col = lu_df$color, pch = rep(15,6))
  # # plot(adj, add=T, col= rgb(0.5,0.5,0.5,0.5))
  # dev.off()
  
  
  # poly$landuse = poly_tab_amended$Landuse[match(poly$Polynmbr, poly_tab_amended$Field_ID)]
  # plot(poly, col = poly$landuse)
  
  write.table(x = poly_tab_amended, quote = F,
              file = file.path(SWBM_file_dir, "polygons_table.txt"),
              sep = "\t",col.names=TRUE, row.names = F)

# ___ET_Cells_NV.txt ---------------------------------------------------------
  
  # Spatial operation to designate cells which fall outside the Discharge Zone but 
  # more than 50% within Natural Vegetation areas

  # # Read in shapefile for precise outline of model domain
  # domain = readOGR(dsn = ref_data_dir, layer = "Model_Domain_20180222")
  # legacy_dir = "C:/Users/Claire/Box/Scott_Only/Legacy Work Products/Scott_Legacy_GIS"
  # domain = readOGR(dsn = legacy_dir, layer = "Model_Domain_20180222")
  
  # Read in Discharge Zone Cells 
  dz_cells = as.matrix(read.table(file.path(ref_data_dir,"Discharge_Zone_Cells.txt"),header=F))
  
  #Use DZ Cells and the domain bounding box to create a raster
  
  # # This is a problem because domain doesn't cover the whole model grid. gives us 96 m x-dir, 92 m y-dir
  # dz = raster(x=dz_cells, crs = crs(domain),
  #                xmn = bbox(domain)[1,1], xmx = bbox(domain)[1,2],
  #                ymn = bbox(domain)[2,1], ymx = bbox(domain)[2,2])
  # This is slightly better. Still short in the y-direction. 99 m x-dir, 95 m y-dir. 
  # I'm missing a bunch of rows of 0 no-flow cells at the top of the model domain.
  # dz = raster(x=dz_cells, crs = crs(utm_poly),
  #                xmn = bbox(utm_poly)[1,1], xmx = bbox(utm_poly)[1,2],
  #                ymn = bbox(utm_poly)[2,1], ymx = bbox(utm_poly)[2,2])
  
  # fixed: found the damn vector version of the model grid.
  # legacy_dir = "C:/Users/Claire/Box/Scott_Only/Legacy Work Products/Scott_Legacy_GIS"
  # model_grid = readOGR(dsn = legacy_dir, layer = "100m_grid_UTM_20180126")
  # ext_depth = raster(x=extd_cells, crs = crs(model_grid),
  #                xmn = bbox(model_grid)[1,1], xmx = bbox(model_grid)[1,2],
  #                ymn = bbox(model_grid)[2,1], ymx = bbox(model_grid)[2,2])
  # writeRaster(x = ext_depth, filename = file.path(legacy_dir, "ET_Extinction_Depth_raster"))
  
  ## Visualize
  # plot(dz)
  # plot(domain, add=T, lwd = 2)
  # utm_poly = spTransform(poly, crs(domain))
  # plot(utm_poly, border = "darkgray", add=T, col = utm_poly$landuse_color)
  
  # # Rasterize the polygons that are natural vegetation
  # poly_nv = utm_poly[utm_poly$landuse_from_svihm == 3,]
  # nv_poly_raster = rasterize(x = poly_nv, y = dz, field = 1, background = 0)
  # 
  # # Make combination extinction depth matrix: 0.5 in the Discharge Zone, 4.5 m in the nat veg outside DZ
  # plot(dz)
  # combo_raster = dz
  # combo_raster[combo_raster == 0] = nv_poly_raster[combo_raster == 0]*4.5
  
  
  # Alternatively, we could skip the rasterizing process and use Recharge_Zones_SVIHM.txt,
  # which already has a spatial relationship between each field and the modflow gridcells.
  poly_nv = poly_tab_amended[poly_tab_amended$Landuse == 3,]
  poly_cells = as.matrix(read.table(file = file.path(time_indep_dir, "Recharge_Zones_SVIHM.txt"),
                                    header = F))
  # Make extinction depth matrix
  extinction_depth_matrix = dz_cells
  # Assign cells in natural vegetation fields a 4.5 m extinction depth
  extinction_depth_matrix[poly_cells %in% unique(poly_nv$Field_ID)] = extinction_depth_value
  # Assign Discharge Zone cells an extinction depth of 0.5 m
  extinction_depth_matrix[dz_cells == 1] = 0.5
  
  #Make a table of 1s and 0s, 1 indicating ET-from-gw is active in this cell
  et_zone_cells = extinction_depth_matrix
  et_zone_cells[extinction_depth_matrix!=0]=1
  
  # Save  extinction depth matrix
  write.table(x =extinction_depth_matrix, 
              file = file.path(SWBM_file_dir, "ET_Cells_Extinction_Depth.txt"),
              row.names = F, col.names = F)
  #Overwrite ET_Zone_Cells.txt with new ET_Zone_Cells
  file.remove(file = file.path(SWBM_file_dir, "ET_Zone_Cells.txt"))
  write.table(x =et_zone_cells, 
              file = file.path(SWBM_file_dir, "ET_Zone_Cells.txt"),
              row.names = F, col.names = F)
  
  
} else if(landuse_scenario %in% c("basecase","Basecase")){
    # If the land use is basecase, keep the standard polygon file, and 
  # write a ET_Cells_Extinction_Depth file for only the discharge zone
  
  # Read in Discharge Zone Cells 
  dz_cells = as.matrix(read.table(file.path(ref_data_dir,"Discharge_Zone_Cells.txt"),header=F))
  # Make extinction depth matrix
  extinction_depth = dz_cells
  # Assign Discharge Zone cells an extinction depth of 0.5 m
  extinction_depth[dz_cells == 1] = 0.5
  # Save  extinction depth matrix
  write.table(x =extinction_depth, 
              file = file.path(SWBM_file_dir, "ET_Cells_Extinction_Depth.txt"),
              row.names = F, col.names = F)
  }



#  general_inputs.txt ------------------------------------------------

#Update number of stress periods and scenario information
# Does not include irr_demand_mult, since that gets incorporated into the kc files
gen_inputs = c(
  paste("2119  167", num_stress_periods, 
        "440  210  1.4 UCODE",
        "! num_fields, num_irr_wells, num_stress_periods, nrow, ncol, RD_Mult, UCODE/PEST", 
        sep = "  "),
  paste(recharge_scenario, flow_scenario, 
        "! Basecase/MAR/ILR/MAR_ILR, Basecase/Flow_Lims",
        sep = "  "),
  paste(alf_irr_stop_mo, alf_irr_stop_day, early_cutoff_flag,
        "! alf_irr_stop_mo  alf_irr_stop_day early_alf_cutoff_scenario",
        sep = "  "),
  paste(landuse_scenario, "! Basecase/Major_NatVeg")
  )

write.table(gen_inputs, file = file.path(SWBM_file_dir, "general_inputs.txt"),
            sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)



# irr_eff.txt -------------------------------------------------------------

if(tolower(irr_eff_scenario)!="basecase"){
  filename1 = file.path(SWBM_file_dir,'irr_eff.txt')
  
  irr_eff_lines = readLines(filename1)
  irr_eff_text_alfalfa = substr(irr_eff_lines[2], start = 1, stop = 6)
  irr_eff_text_pasture = substr(irr_eff_lines[3], start = 1, stop = 9)
  
  irr_eff_vals_alfalfa = as.numeric(unlist(strsplit(irr_eff_text_alfalfa, split = " ")))
  irr_eff_vals_pasture = as.numeric(unlist(strsplit(irr_eff_text_pasture, split = " ")))
  
  irr_eff_vals_alfalfa_amended = irr_eff_vals_alfalfa + irr_eff_scenario
  irr_eff_vals_pasture_amended = irr_eff_vals_pasture + irr_eff_scenario
  
  irr_eff_vals_alfalfa_amended_text = paste(irr_eff_vals_alfalfa_amended, collapse = " ")
  irr_eff_vals_pasture_amended_text = paste(irr_eff_vals_pasture_amended, collapse = " ")
  
  irr_eff_lines_amended = irr_eff_lines
  irr_eff_lines_amended[2] = gsub(pattern = irr_eff_text_alfalfa, 
                                  replacement = irr_eff_vals_alfalfa_amended_text, 
                                  x = irr_eff_lines[2])
  irr_eff_lines_amended[3] = gsub(pattern = irr_eff_text_pasture, 
                                  replacement = irr_eff_vals_pasture_amended_text, 
                                  x = irr_eff_lines[3])

  # overwrite the irr_eff.txt
  writeLines(text = irr_eff_lines_amended, filename1)
  
}

#  kc_alfalfa.txt -------------------------------------------------
kc_alf_dormant = 0
kc_alf_growing = 0.9
growing_season_start_month = 3
growing_season_start_day = 1 #march 1st
growing_season_end_month = 11
growing_season_end_day = 14 #nov 14th

kc_alf_days = rep(kc_alf_growing, length(model_days))
#days before or after the growing season are assigned a dormant kc value 
#TO DO: there must be a more elegant way to do this
kc_alf_days[(month(model_days) < growing_season_start_month) | 
              (month(model_days) == growing_season_start_month & day(model_days) < growing_season_start_day)| 
                       (month(model_days) > growing_season_end_month) |
              (month(model_days) == growing_season_end_month & day(model_days) > growing_season_end_day)
            ] = kc_alf_dormant

#Pad single-digit month, day values with 0s (e.g. "2" becomes "02"), then concatenate date strings
kc_alfalfa_df = data.frame(kc_alf_days)
kc_alfalfa_df$day = paste(str_pad(day(model_days), 2, pad = "0"), 
                          str_pad(month(model_days), 2, pad = "0"), 
                          year(model_days), sep = "/")

# Multiply the k_c by the demand multiplier (if irr_demand_mult!= 1, assumes land use change from alfalfa)
kc_alfalfa_df$kc_alf_days = kc_alfalfa_df$kc_alf_days * irr_demand_mult

#write alfalfa kc file
write.table(kc_alfalfa_df, file = file.path(SWBM_file_dir, "kc_alfalfa.txt"),
            sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)

#  kc_pasture.txt ------------------------------------------------

kc_pas_dormant = 0
kc_pas_growing = 0.9
growing_season_start_month = 3
growing_season_start_day = 1 #march 1st
growing_season_end_month = 11
growing_season_end_day = 14 #nov 14th

kc_pas_days = rep(kc_pas_growing, length(model_days))
#days before or after the growing season are assigned a dormant kc value 
#TO DO: there must be a more elegant way to do this
kc_pas_days[(month(model_days) < growing_season_start_month) | 
              (month(model_days) == growing_season_start_month & day(model_days) < growing_season_start_day)| 
              (month(model_days) > growing_season_end_month) |
              (month(model_days) == growing_season_end_month & day(model_days) > growing_season_end_day)
            ] = kc_pas_dormant


#Pad single-digit month, day values with 0s (e.g. "2" becomes "02"), then concatenate date strings
kc_pasture_df = data.frame(kc_pas_days)
kc_pasture_df$day = paste(str_pad(day(model_days), 2, pad = "0"), 
                          str_pad(month(model_days), 2, pad = "0"), 
                          year(model_days), sep = "/")

# Multiply the k_c by the demand multiplier (if irr_demand_mult!= 1, assumes land use change from pasture)
kc_pasture_df$kc_pas_days = kc_pasture_df$kc_pas_days * irr_demand_mult

#write pasture kc file
write.table(kc_pasture_df, file = file.path(SWBM_file_dir, "kc_pasture.txt"),
            sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)


#  kc_grain.txt --------------------------------------------------
# To Do: how did y'all generate kc_grain? why are all the start dates different?

#Build grain kc curve
kc_grain_dormant = 0
kc_grain_by_crop_stage = c(0, 0.27, 1.15, 1.15, 0)  
days_in_crop_stage = c(38, 26, 32, 36) # includes start and end days in each stage

#initialize growing season kc profile based on growth stage
kc_grain_growing_season = rep(0, (sum(days_in_crop_stage) - 3)) # minus 3 fenceposts
crop_stage_index = 1
for(i in 1:length(days_in_crop_stage)){
  stage_length = days_in_crop_stage[i]
  start_kc = kc_grain_by_crop_stage[i]
  end_kc = kc_grain_by_crop_stage[i+1]
  kc_grain_growing_season[crop_stage_index: (crop_stage_index + stage_length -1)] = 
    seq(start_kc, end_kc, length.out = stage_length)
  
  crop_stage_index = crop_stage_index + stage_length - 1
  
}

#assign start of growing season (currently uniform for all years)
growing_season_start_month = 3
growing_season_start_day = 14

#initialize full model period kc vector
kc_grain_days = rep(kc_grain_dormant, length(model_days))

#find the index of the growing season start for each year and assign the growing season curve
for(yr in (start_year+1):end_year){
  start_day_index = which(year(model_days) == yr & 
                            month(model_days) == growing_season_start_month &
                            day(model_days) == growing_season_start_day)
  kc_grain_days[start_day_index:
                  (start_day_index + sum(days_in_crop_stage) - 3)] = kc_grain_growing_season
}

# Build data frame with date formatted for file-writing
kc_grain_days = round(kc_grain_days, 4)
kc_grain_df = data.frame(kc_grain_days)
#Pad single-digit month, day values with 0s (e.g. "2" becomes "02"), then concatenate date strings
kc_grain_df$day = paste(str_pad(day(model_days), 2, pad = "0"), 
                          str_pad(month(model_days), 2, pad = "0"), 
                          year(model_days), sep = "/")



# Multiply the k_c by the demand multiplier (if irr_demand_mult!= 1, assumes land use change from grain)
kc_grain_df$kc_grain_days = kc_grain_df$kc_grain_days * irr_demand_mult

#write grain kc file
write.table(kc_grain_df, file = file.path(SWBM_file_dir, "kc_grain.txt"),
            sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)



# MAR_Fields.txt and polygons.txt----------------------------------------------------------

if(tolower(recharge_scenario) == "mar_ilr_max"){
  #Intention: "max out" the MAR and ILR artificial recharge capacity of the valley. bookend scenario.
  
  # Note: this does not preserve "notes" column
  poly_column_classes = c(rep("integer",4),
                          "numeric","integer","numeric","numeric",
                          "integer","integer","character",
                          rep("NULL",16)) # get rid of empty columns in the text file
  
  # Read in the existing polygons table (may have been amended for this scenario) to amend it further)
  poly_tab = read.table(file.path(SWBM_file_dir,"polygons_table.txt"),
                        header = T, comment.char = "!", 
                        fill = T, sep = "\t", colClasses = poly_column_classes)
  colnames(poly_tab) = c("Field_ID",colnames(poly_tab)[2:11])
  
  #Read in existing MAR fields in table form
  mar_fields = read.table(file.path(time_indep_dir, "MAR_Fields.txt"), skip = 1, comment.char = "!")
  colnames(mar_fields) = c("Field_ID","max_infil_m_day")
  
  # Designate all fields that have a surface water hookup as MAR and ILR fields.
  # make water source color table
  wat_source = c(1,2,3,4,5,999)
  wat_source_descrip = c("SW","GW","Mixed", "Sub-irrigated","Dry","Unknown")
  wat_source_color = c("dodgerblue","firebrick2","darkorchid1","green","yellow","gray")

  wat_source_df = data.frame(ws_code = wat_source,
                             descrip = wat_source_descrip,
                             color = wat_source_color)
  
  # Check overlaps. Did Gus have MAR or ILR happening on some non-SW-accessing fields?
  # Looks like yes.
  # But for this scenario purpose we'll just assign MAR and ILR to surface water or mixed-source fields.
  ilrs = poly_tab$Field_ID[poly_tab$ILR_Flag==1]
  mars = mar_fields$Field_ID
  sw_and_ms = poly_tab$Field_ID[poly_tab$Water_Source %in% c(1,3)]
  gws = poly_tab$Field_ID[poly_tab$Water_Source ==2]

  # length(intersect(mars, gws))
  # length(intersect(mars, sw_and_ms))
  #Initialize new table
  poly_tab_amended = poly_tab
  # Reset all SW-accessing fields to have an ILR flag
  poly_tab_amended$ILR_Flag[poly_tab_amended$Field_ID %in% sw_and_ms] = 1
  
  #write amended polygons table
  file.remove(file.path(SWBM_file_dir, "polygons_table.txt")) #get rid of old one
  write.table(x = poly_tab_amended, quote = F,
              file = file.path(SWBM_file_dir, "polygons_table.txt"),
              sep = "\t",col.names=TRUE, row.names = F)
  
  
  #Write new MAR_Fields.txt files
  # List field IDs that have surface water access but aren't already in the MAR_Fields.txt file
  new_mar_fields = sw_and_ms[!(sw_and_ms %in% mars)]
  
  #Prep other MAR_Fields.txt lines for updated number of fields
  mar_fields_all_lines = readLines(file.path(time_indep_dir, "MAR_Fields.txt"))
  mar_fields_line1 = mar_fields_all_lines[1]
  mar_fields_other_lines = mar_fields_all_lines[2:length(mar_fields_all_lines)]
  new_plus_old_mar_fields = length(mars)+length(new_mar_fields)
  mar_fields_line1_amended = gsub(pattern = "32", x = mar_fields_line1,
                                  as.character(new_plus_old_mar_fields))
  
  # 
  file.remove(file.path(SWBM_file_dir, "MAR_Fields.txt"))
  write.table(x = mar_fields_line1_amended,
              file= file.path(SWBM_file_dir, "MAR_Fields.txt"), 
              quote = F, row.names = F, col.names = F, sep = "  ",
              append = F)
  write.table(x = mar_fields_other_lines,
              file= file.path(SWBM_file_dir, "MAR_Fields.txt"), 
              quote = F, row.names = F, col.names = F, sep = "  ",
              append = T)
  
  # Recharge rate decided up at top of scenario picking section
  # Options: 0.035, 0.003, or 0.019 (the arithmetic mean) or 0.01 (the geometric mean)
  # max_infil_rate_for_unknowns = 0.019
  # Write those fields at the end of the file;
  
  mar_fields_txt_addendum = as.matrix(cbind(new_mar_fields, rep(max_infil_rate_for_unknowns, length(new_mar_fields))))
  
  # write amended MAR_Fields.txt
  write.table(x = mar_fields_txt_addendum,
              file= file.path(SWBM_file_dir, "MAR_Fields.txt"), 
              quote = F, row.names = F, col.names = F, sep = "  ",
              append = T)
  
}


#  precip.txt ----------------------------------------------------

ppt_filename1=file.path(scenario_dev_dir,"precip_regressed.txt")
ppt_filename2=file.path(SWBM_file_dir,"precip.txt")

if(!file.exists(ppt_filename1)){
  declare_dir_in_analyses_script = FALSE #Prevents the input_analyses script from overwriting SWBM_dir
  source(file.path(input_files_dir,'SVIHM_input_analyses.R'))
  write_swbm_precip_input_file()
}

precip_record_values = read.table(ppt_filename1)
colnames(precip_record_values) = c("precip_m", "Date")
precip_record_values$Date = as.Date(precip_record_values$Date, format = "%d/%m/%Y")

# Initialize data that will hold flow data for the 50-year record
precip_record = data.frame(Date = rep(NA, times = sum(num_days)),
                           Precip_m = rep(NA, times = sum(num_days)))
# Insert stitched-together dates vector for "Date"
precip_record$record_date = record_days
# Match flow values in the relevant dates to create a 50-year stitched flow record
precip_record$Precip_m = precip_record_values$precip_m[match(precip_record$record_date, 
                                                             precip_record_values$Date)]
# Create future precip record from repeating past data
precip = precip_record
precip$Date = model_days

#### *change factors for precip ####
if(tolower(future_climate) != "basecase" ){
  precip$cf = NA
  precip$year_month_01 = floor_date(precip$record_date, unit = "months")
  precip$cf = etp_scott_avgd[match(precip$year_month_01, etp_scott_avgd$ModelDate),
                             precip_col_index]
  precip$Precip_m = precip$Precip_m * precip$cf
  precip$cf = NULL; precip$year_month_01 = NULL
}


#format to match the original precip file, and write as text file
precip$record_date=NULL; 
precip = precip[,2:1]

precip$Date = paste(str_pad(day(precip$Date), 2, pad="0"),
                    str_pad(month(precip$Date), 2, pad="0"),
                    year(precip$Date), sep = "/")

write.table(precip, file = ppt_filename2,
            sep = " ", quote = FALSE, 
            col.names = FALSE, row.names = FALSE)

#  ref_et.txt ----------------------------------------------------

et_filename1=file.path(scenario_dev_dir,"ref_et_monthly.txt")
et_filename2=file.path(SWBM_file_dir,"ref_et.txt")

if(!file.exists(et_filename1)){
  declare_dir_in_analyses_script = FALSE #Prevents the input_analyses script from overwriting SWBM_dir
  source(file.path(input_files_dir,'SVIHM_input_analyses.R'))
  write_swbm_et_input_file()
}

et_record_values = read.table(et_filename1)
colnames(et_record_values) = c("et_m", "et_in","Date")
et_record_values$Date = as.Date(et_record_values$Date, format = "%d/%m/%Y")

# Initialize data that will hold flow data for the 50-year record
et_record = data.frame(Date = rep(NA, times = sum(num_days)),
                       ET_m = rep(NA, times = sum(num_days)))
# Insert stitched-together dates vector for "Date"
et_record$record_date = record_days
# Match values in the relevant dates to create a 50-year stitched record
et_record$ET_m = et_record_values$et_m[match(et_record$record_date, 
                                             et_record_values$Date)]
# Create future et record from repeating past data
et = et_record
et$Date = model_days

#### *change factors for ref ET ####
if(tolower(future_climate) != "basecase" ){
  et$cf = NA
  et$year_month_01 = floor_date(et$record_date, unit = "months")
  et$cf = etp_scott_avgd[match(et$year_month_01, etp_scott_avgd$ModelDate),
                             et_col_index]
  et$ET_m = et$ET_m * et$cf
  et$cf = NULL; et$year_month_01 = NULL
}

#format to match the original et file, and write as text file
et$record_date=NULL
et$ET_in = et$ET_m*39.3701
et = et[,c(2,3,1)]

et$Date = paste(str_pad(day(et$Date), 2, pad="0"),
                    str_pad(month(et$Date), 2, pad="0"),
                    year(et$Date), sep = "/")

write.table(et, file = et_filename2,
            sep = " ", quote = FALSE, 
            col.names = FALSE, row.names = FALSE)

#  streamflow_input.txt and available flow ratio ------------------------------------------

# To update, EITHER: 
# a) Update Fort Jones gauge record is Streamflow_Regression_Model folder, OR
# b) build webscraper for latest stream data?
# "https://waterdata.usgs.gov/ca/nwis/dv?cb_00060=on&format=rdb&site_no=11519500&referred_module=sw&period=&begin_date=1941-10-01&end_date=2019-05-19"
#pull down from server, since it webscraped recently?;
#THEN: change the Fort Jones USGS file reference in the Regression script.

# Pull streamflow_input file
stm_filename1 = file.path(Stream_Regression_dir,'streamflow_input.txt')
stm_filename2 = file.path(SWBM_file_dir, 'streamflow_input.txt')

if(!file.exists(stm_filename1)){
  source(file.path(Stream_Regression_dir,'SVIHM_Streamflow_Regression_Model.R'))
  generate_streamflow_input_txt(end_date = model_end_date)
}

# Revise streamflows to reflect 50-year stitched record

stm_record_values = read.table(stm_filename1, header=T)
stm_record_values$Month = as.Date(stm_record_values$Month)

# Initialize data that will hold flow data for the 50-year record
stm_record = data.frame(matrix(data = NA, nrow = num_stress_periods, ncol = ncol(stm_record_values)))
colnames(stm_record)=colnames(stm_record_values)
# Insert stitched-together dates vector for "Month"
stm_record$record_month = record_months
# Match values in the relevant dates to create a 50-year stitched record
stitch_these_cols = grepl("m3day", colnames(stm_record))
stm_record[,stitch_these_cols] = 
  stm_record_values[match(stm_record$record_month, stm_record_values$Month), 
                    stitch_these_cols]
# Create future et record from repeating past data
stm = stm_record
stm$Month = model_months


#### *change factors for tributaries ####
if(tolower(future_climate) != "basecase" ){
  stm$cf = NA
  stm$cf = stm_cf[match(stm$record_month, stm_cf$ModelDate),
                         stm_col_index]
  stm[2:13] = stm[2:13] * stm$cf
  stm$cf = NULL; 
}


#format to match the original et file, and write as text file
stm$record_month=NULL
stm$Month = paste(year(stm$Month),
                  str_pad(month(stm$Month), 2, pad="0"),
                  str_pad(day(stm$Month), 2, pad="0"),
                  sep = "-")

write.table(stm, file = stm_filename2,
            sep = "\t", quote = FALSE, 
            col.names = TRUE, row.names = FALSE)



#__instream_flow_available_ratio.txt ------------------------------------------------

#read in FJ flow and CDFW recommended flow for FJ gauge
library(dataRetrieval)
library(lubridate)
fj_num = "11519500"
fjd_all = readNWISdv(siteNumbers = fj_num, parameterCd="00060" )
fjd_all = renameNWISColumns(fjd_all)
# Pull data for model period
fjd_record_values = fjd_all[fjd_all$Date >= min(record_days) & 
                              fjd_all$Date <= max(record_days),]
# Initialize data that will hold flow data for the 50-year record
fjd_record = data.frame(Date = rep(NA, times = sum(num_days)),
                        Flow = rep(NA, times = sum(num_days)))
# Insert stitched-together dates vector for "Date"
fjd_record$record_date = record_days
# Match flow values in the relevant dates to create a 50-year stitched flow record
fjd_record$Flow = fjd_record_values$Flow[match(fjd_record$record_date, fjd_record_values$Date)]

#### *change factors for fj flow ####

# Create future streamflow record from repeating past data
fjd = fjd_record
fjd$Date = model_days


# Read in CDFW data to calculate "available" water (used in flow limits scenarios)
cdfw_tab = read.csv(file.path(ref_data_dir,"cdfw_2017_instream_flows.csv"))

#build calendar of cdfw rec flow start and end dates for the years covering the model period
model_yrs = year(seq(from = model_start_date, to = model_end_date+365, by = "year")) #Add 365 to keep ending year in the sequence
cdfw_start_dates = as.Date(paste(sort(rep(model_yrs, dim(cdfw_tab)[1])), 
                                 cdfw_tab$Start.date.month, cdfw_tab$start.date.day, sep ="-"))
cdfw_end_dates = as.Date(paste(sort(rep(model_yrs, dim(cdfw_tab)[1])), 
                               cdfw_tab$End.date.month, cdfw_tab$End.date.day, sep ="-"))
leap_years = seq(from=1900, to = 2100, by = 4)
leapday_selector = day(cdfw_end_dates) == 28 & year(cdfw_end_dates) %in% leap_years
cdfw_end_dates[leapday_selector] = 1 + cdfw_end_dates[leapday_selector] # adjust to include all of Feb
cdfw_rec_flows = rep(cdfw_tab$Recommended.flow.cfs, length(model_yrs))

# check for correctness
# cdfw_expanded = data.frame(start_dates = cdfw_start_dates, end_dates = cdfw_end_dates,
# cdfw_rec_flows = cdfw_rec_flows)

instream_rec = data.frame(dates = model_days, cdfw_rec_flow_cfs = NA)

for(i in 1:length(cdfw_start_dates)){
  selector = instream_rec$dates >= cdfw_start_dates[i] &
    instream_rec$dates <= cdfw_end_dates[i]
  instream_rec$cdfw_rec_flow_cfs[selector] = cdfw_rec_flows[i]
}

# Add FJ flow to this dataframe and find difference (available flow)
instream_rec$fj_flow_cfs = fjd$Flow[match(instream_rec$dates, fjd$Date)]

# Convert to cubic meters per day
cfs_to_m3d = 1/35.3147 * 86400 # 1 m3/x ft3 * x seconds/day
instream_rec$cdfw_rec_flow_m3d = instream_rec$cdfw_rec_flow_cfs * cfs_to_m3d
instream_rec$fj_flow_m3d = instream_rec$fj_flow_cfs * cfs_to_m3d
# Calculate m3d available for expanded MAR+ILR scenario
instream_rec$avail_m3d = instream_rec$fj_flow_m3d - instream_rec$cdfw_rec_flow_m3d
instream_rec$avail_m3d[instream_rec$avail_m3d<0] = 0

# Add the stress period and day of month 
instream_rec$month = floor_date(instream_rec$dates, unit = "month")

#calculate aggregate daily average flow, by month, for whole record
rec_monthly = aggregate(instream_rec$cdfw_rec_flow_m3d, by = list(instream_rec$month), FUN = sum)
fj_monthly = aggregate(instream_rec$fj_flow_m3d, by = list(instream_rec$month), FUN = sum)
avail_monthly = merge(rec_monthly, fj_monthly, by.x = "Group.1", by.y = "Group.1")
colnames(avail_monthly) = c("month","cdfw_rec_flow_m3","fj_flow_m3")
avail_monthly$avail_flow_m3 = avail_monthly$fj_flow_m3 - avail_monthly$cdfw_rec_flow_m3
avail_monthly$avail_flow_m3[avail_monthly$avail_flow_m3 <0] = 0
avail_monthly$avail_ratio = round(avail_monthly$avail_flow_m3 / avail_monthly$fj_flow_m3,3)


#format for table
avail_monthly[,colnames(avail_monthly) != "avail_ratio"] = NULL

write.table(avail_monthly, file = file.path(SWBM_file_dir, "instream_flow_available_ratio.txt"),
            sep = " ", quote = FALSE, col.names = F, row.names = FALSE)

if(tolower(recharge_scenario)=="mar_ilr_max"){
  avail_per_day = instream_rec$avail_m3d
  # Write a daily available flow volume file
  
  write.table(avail_per_day, file = file.path(SWBM_file_dir, "instream_flow_available_m3d.txt"),
              sep = " ", quote = FALSE, col.names = F, row.names = FALSE)
  
}


# __reservoir streamflow changes ----------------------------------------------------

if(reservoir_scenario %in% c("Basecase","basecase","BASECASE")){
  # file.copy(filename1, filename2)  #Keep basecase tributary input flows
  # Not needed since this file got copied into the scenario folder above
} else {
  stm = read.table(stm_filename2, header = T) #read in the 50-year stitched version
  
  # Very simple reservoir simulation
  
  #Convert values to AF per day
  stm_AFday = stm
  m3day_to_AFday = 1/4046.86 * 3.28084
  
  convert_these_columns = grepl(pattern = "m3day", x = colnames(stm_AFday))
  stm_AFday[,convert_these_columns] = stm[,convert_these_columns] * m3day_to_AFday
  #update column names
  colnames(stm_AFday)[convert_these_columns] = 
    sub("m3day", "AFday" , colnames(stm_AFday)[convert_these_columns])
  
  #Reservoir parameters 
  cfs_to_AFday = 2.29568411*10^-5 * 86400
  cfs_goal = dry_season_release_cfs
  D_daily = cfs_goal * cfs_to_AFday # Target demand during dry season (fish flow releases)
  # Assume demand during the dry season is about 20 cfs for ~150 days (July 1 to Dec 1)
  K = D_daily * 150 # Reservoir capacity. Rough estimate: low-flow releases for dry season.
  
  if(tolower(reservoir_capacity) != "basecase"){K = reservoir_capacity} # set at 5 years of capacity.
  # TO DO: check how realistic this would be (9 TAF capacity?)
  
  #Initialize inflow time series
  Q_daily_avg = stm_AFday[,grepl(pattern = reservoir_scenario, x = colnames(stm_AFday))]
  Q = Q_daily_avg * num_days # convert to monthly volume, AF/month
  nmonth = length(Q) # number of months
  
  S = rep_len(0, nmonth)  # Storage
  R = rep_len(0, nmonth)  # Discharge from reservoir to stream
  P = rep_len(0, nmonth)  # Discharge from reservoir to pipeline 
  shortage = rep_len(0, nmonth)
  
  # Initialize output arrays
  if(tolower(reservoir_start) == "empty"){  S[1] = 0 }                 # start simulation at empty
  if(tolower(reservoir_start) == "full"){   S[1] = K }                 # start simulation full
  if(is.numeric(reservoir_start) & reservoir_start <= 1){S[1] = K*reservoir_start} # Assume fraction full units
  if(is.numeric(reservoir_start) & reservoir_start > 1){S[1] = reservoir_start}    # Assume AF units
  R[1] = 0 
  P[1] = 0
  met_demand = 0  # counter
  
  
  for(t in 2:nmonth){
    
    # new storage: mass balance. Max value is K
    S[t] = min(S[t-1] + Q[t-1] - R[t-1] - P[t-1], K)
    # Calculate monthly demand
    D = D_daily*num_days[t]
    
    
    if(t%%12 %in% 0:3){
      # In Dec-Mar, release the minimum (demand) to the STREAM until the reservoir is full, then let flow bypass reservoir
      if(S[t] + Q[t] <= K){ 
        R[t] = min(D, S[t]+Q[t])               # If storage + inflow is less than capacity, release demand or all avail. water to stream
        P[t] = 0                               # No pipeline releases in winter months
      }else{
        R[t] = S[t] + Q[t] - K # If storage is full or nearly full, release inflow or fraction of inflow
        P[t] = 0                               # No pipeline releases in winter months
      }
    }
    
    if(t%%12 %in% 4:6){
      # In Apr-June, let flow bypass reservoir for irrigation (but keep stored volume in reserve)
        R[t] = Q[t]
        P[t] = 0                               # No pipeline releases in growing season months
    }

    if(t%%12 %in% 7:11){
      # In July-Nov, release water (no test for low-flow threshold)
      # release demand amount to PIPELINE if enough water is available to meet demand
      # Hold back remaining water; no releases to stream unless reservoir is full
      
      if((S[t] + Q[t]) > D & (S[t] + Q[t] - D) < K){
        P[t] = D
        R[t] = 0
        met_demand = met_demand + 1
      } else if((S[t] + Q[t]) > D & (S[t] + Q[t] - D) > K) { # If the reservoir is full, release demand to pipeline and overage to stream
        P[t] = D
        R[t] = (S[t] + Q[t] - D) - K
        met_demand = met_demand + 1
      } else {
        # release all available water to pipeline if not enough to meet demand
        P[t] = S[t] + Q[t]
        R[t] = 0
      }
    }
    # after each month, calculate shortage
    shortage[t] = max(D - P[t], 0)
  }
  
  # Evaluate reservoir performance in terms of meeting flow release target
  dry_months = sum(1:nmonth%%12 %in% 7:11) #number of months in which we want to meet demand
  reliability = met_demand / dry_months
  
  # Plot inflow, discharge, and storage
  # plot(model_months, Q/num_days/cfs_to_AFday, type = "l", ylab = "Inflow, cfs")
  # plot(model_months, R/num_days/cfs_to_AFday, type = "l", ylab = "Stream Discharge, cfs")
  # plot(model_months, P/num_days/cfs_to_AFday, type = "l", ylab = "Pipeline Discharge, cfs")
  # plot(model_months, S, type = "l", ylab = "Storage, AF", ylim = c(0,K), main = paste(reservoir_scenario, cfs_goal, "cfs demand, ",round(K)," AF"))
  
  # notes
  #summarize by WY type, indicate on figure?
  
  
  # #Diagnostic plots: Plot inflow, discharge, and storage for 3 years
  # start_month = 22*12+1
  # 
  # #Inflow
  # plot(model_months[start_month + 1:36], Q[start_month+1:36]/num_days[start_month+1:36]/cfs_to_AFday, type = "o", ylab = "Inflow, cfs")
  # abline(v=model_months[as.integer(seq(from=4, by=12, length.out=28))], col = "green4", lwd = 2, lty = 2)
  # abline(v=model_months[as.integer(seq(from=7, by=12, length.out=28))], col = "orange", lwd = 2, lty = 2)
  # abline(v=model_months[as.integer(seq(from=12, by=12, length.out=28))], col = "dodgerblue", lwd = 2, lty = 2)
  # 
  # #R and P
  # plot(model_months[start_month+1:36], R[start_month+1:36]/num_days[start_month+1:36]/cfs_to_AFday, type = "o", ylab = "Stream Discharge, cfs")
  # lines(model_months[start_month+1:36], P[start_month+1:36]/num_days[start_month+1:36]/cfs_to_AFday, col = "brown", ylab = "Pipeline Discharge, cfs")
  # abline(v=model_months[as.integer(seq(from=4, by=12, length.out=28))], col = "green4", lwd = 2, lty = 2)
  # abline(v=model_months[as.integer(seq(from=7, by=12, length.out=28))], col = "orange", lwd = 2, lty = 2)
  # abline(v=model_months[as.integer(seq(from=12, by=12, length.out=28))], col = "dodgerblue", lwd = 2, lty = 2)
  # abline(v=model_months[as.integer(seq(from=9, by=12, length.out=28))], col = "gray", lwd = 1, lty = 2)
  # 
  # #R + P.
  # RP = P+R
  # plot(model_months[start_month+1:36], RP[start_month+1:36]/num_days[start_month+1:36]/cfs_to_AFday, type = "o", ylab = "Stream + Pipeline Discharge, cfs")
  # # lines(model_months[start_month+1:36], P[start_month+1:36]/num_days[start_month+1:36]/cfs_to_AFday, col = "brown", ylab = "Pipeline Discharge, cfs")
  # abline(v=model_months[as.integer(seq(from=4, by=12, length.out=28))], col = "green4", lwd = 2, lty = 2)
  # abline(v=model_months[as.integer(seq(from=7, by=12, length.out=28))], col = "orange", lwd = 2, lty = 2)
  # abline(v=model_months[as.integer(seq(from=12, by=12, length.out=28))], col = "dodgerblue", lwd = 2, lty = 2)
  # abline(v=model_months[as.integer(seq(from=9, by=12, length.out=28))], col = "gray", lwd = 1, lty = 2)
  # # 
  # plot(model_months[start_month+1:36], P[start_month+1:36]/num_days[start_month+1:36]/cfs_to_AFday, type = "o", ylab = "Pipeline Discharge, cfs")
  # abline(v=model_months[as.integer(seq(from=4, by=12, length.out=28))], col = "green4", lwd = 2, lty = 2)
  # abline(v=model_months[as.integer(seq(from=7, by=12, length.out=28))], col = "orange", lwd = 2, lty = 2)
  # abline(v=model_months[as.integer(seq(from=12, by=12, length.out=28))], col = "dodgerblue", lwd = 2, lty = 2)
  # 
  # plot(model_months[start_month+1:36], S[start_month+1:36], type = "l", ylab = "Storage, AF", main = paste(reservoir_scenario, cfs_goal, "cfs demand"))
  # abline(v=model_months[as.integer(seq(from=4, by=12, length.out=28))], col = "green4", lwd = 2, lty = 2)
  # abline(v=model_months[as.integer(seq(from=7, by=12, length.out=28))], col = "orange", lwd = 2, lty = 2)
  # abline(v=model_months[as.integer(seq(from=12, by=12, length.out=28))], col = "dodgerblue", lwd = 2, lty = 2)
  # 
  # #What's going on in September? diagnostics
  # RP_sept = RP[seq(from=9, by = 12, length.out = 28)] # how much are we releasing in september
  # hist(RP_sept/30/cfs_to_AFday, breaks = seq(0,250,10), xlab = "cfs R + P release", main = "September reservoir releases")
  # P_sept = P[seq(from=9, by = 12, length.out = 28)] # how much are we releasing in september
  # hist(P_sept/30/cfs_to_AFday, breaks = seq(0,250,10), xlab = "cfs P release", main = "September reservoir pipe releases")
  # R_sept = R[seq(from=9, by = 12, length.out = 28)] # how much are we releasing in september
  # hist(R_sept/30/cfs_to_AFday, breaks = seq(0,250,10), xlab = "cfs R release", main = "September reservoir releases")
  
  
  # Revise streamflow_input.txt
  
  if(reservoir_plus_pipeline == FALSE) {R = R+P} #If no pipeline, all discharge went through the stream
  
  # Replace the inflow on the designated tributary with the outflow from the reservoir
  replace_this_column = grepl(pattern = reservoir_scenario, x = colnames(stm_AFday))
  stm_AFday[,replace_this_column] = R / num_days # convert to AF/day
  
  #Convert back to m3day
  stm_m3day = stm_AFday
  stm_m3day[,convert_these_columns] = stm_AFday[,convert_these_columns] / m3day_to_AFday
  #update column names
  colnames(stm_m3day)[convert_these_columns] = 
    sub( "AFday", "m3day", colnames(stm_AFday)[convert_these_columns])
  
  
  file.remove(stm_filename2)# delete original 50-yr streamflow file, replace with updated version
  write.table(stm_m3day,file = stm_filename2, append = F, quote = F, row.names = F, col.names = T, sep = '\t')
  
  #Convert P to m3day
  if(reservoir_plus_pipeline == TRUE){
    P_m3day = round(P / num_days / m3day_to_AFday,2)
    filename3 = file.path(SWBM_file_dir, paste0("pipe_flow_",reservoir_scenario,".txt"))

    write.table(P_m3day, file = filename3, row.names = F, col.names = F) 
  }
  
}




# stress_period_days.txt --------------------------------------------------

write.table(num_days, file = file.path(SWBM_file_dir, "stress_period_days.txt"),
            sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)



# SVIHM_SFR_template.txt --------------------------------------------------

if(tolower(BDAs_scenario)!="basecase"){
  # raise the elevation of the parts of the stream system where you want there to be BDAs
  
  sfr_template_line1 = readLines(file.path(SWBM_file_dir, "SVIHM_SFR_template.txt"), n=1)
  sfr_template = read.table(file.path(SWBM_file_dir, "SVIHM_SFR_template.txt"), skip = 1)
  
  colnames_sfr_template = c("KRCH", "IRCH", "JRCH", "ISEG","IREACH", "RCHLEN",
                             "STRTOP","SLOPE","STRTHICK","STRHC1")
  colnames(sfr_template) = colnames_sfr_template
  
  tribs_segments = c(1, 2, 6, 7, 11, 15, 16, 18, 19, 21, 24, 27, 28) 
  no_inflow_tribs = c(8, 13, 17, 20, 22, 29)
  scott_river_segments = c(3, 4, 5, 9, 10, 12, 14, 23, 25, 26, 30)
  
  # set the stream reaches on which you want to build BDAs
  if(tolower(BDAs_scenario)=="tributaries"){
    change_these_elevs = sfr_template$ISEG %in% tribs_segments
  } else if(tolower(BDAs_scenario)=="scott_r_mainstem"){
    change_these_elevs = sfr_template$ISEG %in% scott_river_segments
  } else if(tolower(BDAs_scenario)=="all_streams"){
    change_these_elevs = sfr_template$ISEG %in% unique(sfr_template$ISEG)
  }
  
  # initialize new table of SFR segments
  sfr_template_amended = sfr_template
  
  sfr_template_amended$STRTOP[change_these_elevs] = 
    sfr_template_amended$STRTOP[change_these_elevs] + stream_bed_elev_increase
  filler_col = rep("", nrow(sfr_template_amended))
  sfr_template_amended$filler = filler_col
  sfr_template_amended = sfr_template_amended[,c("filler", colnames_sfr_template)]
  
  write.table(sfr_template_line1,
              file.path(SWBM_file_dir, "SVIHM_SFR_template.txt"), quote = F, col.names = F, row.names = F)
  write.table(sfr_template_amended,
              file.path(SWBM_file_dir, "SVIHM_SFR_template.txt"), 
              quote = F, col.names = F, row.names = F, append = T, sep = "  ")
  

}

# SWBM.exe ----------------------------------------------------------------

file.copy(file.path(svihm_dir,"SWBM","bin",'SWBM.exe'), SWBM_file_dir)




# If in 2 folders: OPERATOR: RUN SWBM -------


#.#############################################################################
# ### MODFLOW INPUTS ------------------------------------------------

# If running SWBM and Modflow in 2 separate folders, copy over SWBM outputs
# #After SWBM done running, copy the files written by SWBM into the modflow directory.
# copy_these_files = c("SVIHM.wel","SVIHM.sfr", "SVIHM.ets", "SVIHM.rch")
# setwd(SWBM_file_dir)
# file.copy(copy_these_files, MF_file_dir)


#Copy files that don't change if the time period gets extended 
# SVIHM.bas
# SVIHM.gag
# SVIHM.nam # need to edit this one for future water budget and take out .hob
# SVIHM.nwt
# SVIHM.pvl
# SVIHM.upw
# SVIHM.zone
# Starting_Heads_L1.txt
# Starting_Heads_L2.txt

copy_these_files = c("SVIHM.bas", "SVIHM.gag", "SVIHM.nwt", "SVIHM.pvl", "SVIHM.upw","SVIHM.zone",
                     "Starting_Heads_L1.txt", "Starting_Heads_L2.txt")

setwd(time_indep_dir)
file.copy(copy_these_files, MF_file_dir)


# SVIHM.dis ---------------------------------------------------------------

#Requires a file SVIHM.dis in position in the reference folder.

# 1) Update the number of stress periods in the header
dis_text = readLines(con = file.path(ref_data_dir, "SVIHM_dis_reference.txt"), n = -1)

#2) Update/extend the list of time steps in each stress period at end of file (dataset 7)
start_of_dataset7 = which(dis_text == "  31  31  1  TR")[1] #Locate first line of dataset 7 (specifying Oct 1990)
dataset_7 = paste0("  ", num_days, "  ", num_days, "  1  TR") #Create new dataset 7 using updated model period

# 3) Put the .dis file text back together and write to file.
dis_text_updated = c(dis_text[1],  #First line of .dis file
                     paste0(" 2  440  210  ", num_stress_periods,"  4  2"),
                     dis_text[4:start_of_dataset7-1], #really puzzled why a 4-index calls line 3 in this, but this works
                     dataset_7)
writeLines(dis_text_updated, con = file.path(MF_file_dir, "SVIHM.dis"))

# SVIHM.drn ---------------------------------------------------------------

setwd(time_indep_dir)

#Assign an elevation, conductance and layer for every cell in the Discharge Zone
DZ_Cells = read.table(file.path(ref_data_dir,"drn_ET_Cells_Discharge_Zone.txt"), header = T, sep = ",")
Model_Surface = matrix(t(read.table(file.path(ref_data_dir,'drn_Layer_1_top_z.txt'))),
                       nrow = 440, ncol = 210, byrow = T)
Elevation = matrix(NaN,length(DZ_Cells$row))
for (i in 1:length(DZ_Cells$row)){
  Elevation[i] = Model_Surface[DZ_Cells$row[i],DZ_Cells$column[i]]
}
Conductance = matrix(10000,length(DZ_Cells$row))
Layer = matrix(1,length(DZ_Cells$row))
Drains = cbind(Layer, DZ_Cells$row, DZ_Cells$column, round(Elevation,2), Conductance)
rep_drains = matrix(-1, num_stress_periods-1)   # repeat value of -1 for n-1 Stress periods to resuse drains specified in first stress period

setwd(MF_file_dir)
#Write preamble
write('# MODFLOW Drain Package File - Drains applied at land surface within discharge zone',file = 'SVIHM.drn', append = F)
write('PARAMETER    1  2869
        2869        50   AUX IFACE
DZ   DRN    1  2869',file = 'SVIHM.drn', append = T)

#Define DZ - layer, row, column, elevation, and conductance for each cell with a drain in it
cat(sprintf("%10i%10i%10i%10.2f%10.3e\n", Drains[,1], Drains[,2],Drains[,3],Drains[,4],Drains[,5]), file = 'SVIHM.drn', append = T)

#Specify DZ for each stress period
for (i in 1:num_stress_periods){
  write(paste("     0          1          Stress Period", i), file = 'SVIHM.drn', append = T)
  write("     DZ                         ", file = 'SVIHM.drn', append = T)
}


# Currently spits out an extra space for each line after the first in the DZ definition. does this matter??


# SVIHM.nam --------------------------------------------------------------
# Remove the .hob file from the name file
nam_lines = readLines(file.path(time_indep_dir, "SVIHM.nam"))
hob_line_index = grep("HOB", nam_lines)
nam_lines_amended = nam_lines[-hob_line_index]

writeLines(nam_lines_amended, file.path(MF_file_dir, "SVIHM.nam"))

# SVIHM.obs ---------------------------------------------------------------


# SVIHM.oc ----------------------------------------------------------------
# Output control package
setwd(MF_file_dir)
preamble = c("  HEAD SAVE UNIT 30", "  HEAD PRINT FORMAT 0", "  DRAWDOWN SAVE UNIT 31", 
             "  DRAWDOWN PRINT FORMAT 0", "  COMPACT BUDGET AUX")
write(preamble, file = 'SVIHM.oc', append = F)

for(i in 1:num_stress_periods){
  period_label = paste0("PERIOD ",i,"  STEP ")
  ndays = as.numeric(num_days[i])
  stress_period_block = paste0(rep(period_label, ndays), 1:ndays)
  write(stress_period_block, file = 'SVIHM.oc', append = T)
  
  stress_end_block = c("     SAVE HEAD", "     SAVE DRAWDOWN", "     SAVE BUDGET", "     PRINT BUDGET")
  write(stress_end_block, file = 'SVIHM.oc', append = T)
}



# MF_OWHM.exe -------------------------------------------------------------

file.copy(file.path(svihm_dir,"MODFLOW",'MF_OWHM.exe'), MF_file_dir)


# If in 2 folders: OPERATOR: RUN MODFLOW ------------------------------------------------------



# Run_SVIHM_forward.bat and input update R scripts ---------------------------------------------------

if(reservoir_plus_pipeline == FALSE){file.copy(file.path(svihm_dir,"Batch_Scripts",'Run_SVIHM_forward.bat'), MF_file_dir)}
if(reservoir_plus_pipeline == TRUE){
  file.copy(file.path(svihm_dir,"Batch_Scripts",'Run_SVIHM_forward_res_w_pipe.bat'), MF_file_dir)
  file.copy(file.path(svihm_dir,"R_Files","Model",'Add_Reservoir_Pipe_Releases_to_SFR_Inputs.R'), MF_file_dir)
  # Don't forget to change the working directory in this file
}
  file.copy(file.path(svihm_dir,"R_Files","Model",'Update_SVIHM_Drain_Inflows.R'), MF_file_dir)
  file.copy(file.path(svihm_dir,"R_Files","Model",'Update_SVIHM_Starting_Heads.R'), MF_file_dir)
  

  
  
  # OPTIONAL: copy output to Results folder for post-processing -------------

# # Scenario Selection ------------------------------------------------------
# recharge_scenario = "Basecase" # Can be Basecase/MAR/ILR/MAR_ILR
# flow_scenario = "Basecase" # Can be Basecase/Flow_Lims. Flow limits on stream diversion specified in "available ratio" table.
# irr_demand_mult = 0.9 # Can be 1 (Basecase) or < 1 or > 1 (i.e., reduced or increased irrigation; assumes land use change)(increased irrigation)
# 
# # # Scenario name for SWBM and MODFLOW
# scenario_name = "mar_ilr_expanded_0.019_reservoir_french" #also makes the directory name; must match folder
# # # 
# # # New file architecture
# scenario_dir = file.path(svihm_dir, "Scenarios",scenario_name)
# SWBM_file_dir = scenario_dir
# MF_file_dir = scenario_dir

# ## Directories for running the scenarios (files copied at end of script)
# 
# SWBM_file_dir = file.path(svihm_dir, "SWBM", scenario_name)
# MF_file_dir = file.path(svihm_dir, "MODFLOW",scenario_name)

# #Copy flow tables on the mainstem
# file.copy(from = file.path(MF_file_dir,"Streamflow_FJ_SVIHM.dat"),
#           to = file.path(results_dir,paste0("Streamflow_FJ_SVIHM_",scenario_name,".dat")),
#           overwrite=T)
# file.copy(from = file.path(MF_file_dir,"Streamflow_Pred_Loc_2.dat"), 
#           to = file.path(results_dir,paste0("Streamflow_Pred_Loc_2_",scenario_name,".dat")),
#           overwrite=T)
# file.copy(from = file.path(MF_file_dir,"Streamflow_Pred_Loc_3.dat"), 
#           to = file.path(results_dir,paste0("Streamflow_Pred_Loc_3_",scenario_name,".dat")),
#           overwrite=T)
# 
# file.copy(from = file.path(MF_file_dir,"SVIHM.sfr"), 
#           to = file.path(results_dir,paste0("SVIHM_",scenario_name,".sfr")),
#           overwrite=T)
# file.copy(from = file.path(SWBM_file_dir,"monthly_groundwater_by_luse.dat"), 
#           to = file.path(results_dir,paste0("monthly_groundwater_by_luse_",scenario_name,".dat")),
#           overwrite=T)
# file.copy(from = file.path(SWBM_file_dir,"monthly_deficiency_by_luse.dat"), 
#           to = file.path(results_dir,paste0("monthly_deficiency_by_luse_",scenario_name,".dat")),
#           overwrite=T)










# Scratch work ------------------------------------------------------------

# #plot DWR_1 through 5
# wells = read.table ("C:/Users/ckouba/Git/SVIHM/SVIHM/SWBM/up2018/well_summary.txt", header = T)
# head(wells)
# 
# library(raster)
# library(maptools)
# mon = shapefile("C:/Users/ckouba/Documents/UCD/SiskiyouSGMA/Data_Exploration/Scott_Legacy_GIS/Monitoring_Wells_All.shp")
# 
# dwr_names = c("DWR_1","DWR_2","DWR_3","DWR_4","DWR_5")
# dwr_in = mon[mon$Well_ID %in% dwr_names,]
# dwr_not_in = mon[mon$In_SVIHM == "No",]
# plot(mon)
# plot(dwr_in, pch = 19, col = "blue", add=T)
# plot(dwr_not_in, pch = 19, col = "red", add=T)
# pointLabel(dwr_in@coords, labels = dwr_in$Well_ID)
# 
# pointLabel(dwr_not_in@coords, labels = dwr_in$Well_ID_2)
# write.csv(mon@data, file.path(ref_data_dir, "Monitoring_Wells_Names.csv"), row.names = F)
# 
#. ####################################
# # Original Precip File Writing attempt
# #to do: web scraper
# # CDEC data
# #FJN daily
# "http://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?Stations=SFJ&SensorNums=41&dur_code=D&Start=1990-10-01&End=2019-05-20"
# #CHA hourly accumulated
# "http://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?Stations=CHA&SensorNums=2&dur_code=H&Start=1990-10-01T00%3A00&End=2019-05-20"
# 
# #NOAA data
# #daily data ftp site: https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/
# #daily data ftp readme: https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt
# #daily data documentation: https://www1.ncdc.noaa.gov/pub/data/cdo/documentation/GHCND_documentation.pdf
# 
# 
# #https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/all/USC00041316.dly
# #https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/all/USC00043182.dly
# 
# 
# noaa = read.csv(file.path(ref_data_dir,"noaa_precip_fjn_cal.csv"))
# noaa$DATE = as.Date(noaa$DATE)
# 
# cal = subset(noaa, STATION=="USC00041316" & DATE >= model_start_date & DATE <= model_end_date)
# cal = data.frame(DATE = cal$DATE, PRCP = cal$PRCP)
# fj = subset(noaa, STATION=="USC00043182" & DATE >= model_start_date & DATE <= model_end_date)
# fj = data.frame(DATE = fj$DATE, PRCP = fj$PRCP)
# 
# # # Compare visually
# # # plot(noaa$DATE, noaa$PRCP, xlim = as.Date(c("1991-10-01","2018-09-01")), type = "l")
# # plot(cal$DATE, cal$PRCP, type = "l", col = "red")
# # lines(fj$DATE, fj$PRCP, #xlim = as.Date(c("1991-10-01","2018-09-01")),
# #      type = "l", col = "blue", add=T)
# 
# ### COMPARISON TABLE FOR 2 STATIONS
# daily_precip = data.frame(model_days)
# daily_precip = merge(x = daily_precip, y = cal, by.x = "model_days", by.y = "DATE", all=TRUE)
# daily_precip = merge(x = daily_precip, y = fj, by.x = "model_days", by.y = "DATE", all=TRUE)
# colnames(daily_precip)=c("Date","PRCP_mm_cal", "PRCP_mm_fj")
# daily_precip$mean_PRCP = apply(X = daily_precip[,2:3], MARGIN = 1, FUN = mean, na.rm=T)
# 
# #isolate the precip data after the end of the original model
# daily_precip_update = subset(daily_precip, Date >= as.Date("2011-10-01"))
# daily_precip_update = subset(daily_precip, Date >= as.Date("2011-10-01"))
# 
# ### HANDLE NAs
# 
# # #check the NA values
# # which(is.na(daily_precip_update$mean_PRCP))
# # daily_precip_update[is.na(daily_precip_update$mean_PRCP),]
# #shit that's a huge gap in december 2012. to do: find alternate precip data source for this gap
# #TEMPORARY SOLUTION FOR NOW: 
# #just put the average of all December dates in that window
# # dec 4-30th
# 
# #calculate average precip values for that day in december over the whole model record (wy1991+)
# precip_dec = subset(daily_precip, month(Date) == 12 & day(Date) >=4 & day(Date) <=30)
# precip_dec$day = day(precip_dec$Date)
# daily_precip_dec = aggregate(precip_dec$mean_PRCP, by=list(precip_dec$day), FUN=mean, na.rm=T)
# 
# #replace NAN values in Dec 2012 with average values over whole record
# daily_precip_update$mean_PRCP[daily_precip_update$Date >= as.Date("2012-12-04") 
#                               & daily_precip_update$Date <= as.Date("2012-12-30")]=daily_precip_dec$x
# 
# #set remaining days with NA in both records to 0
# daily_precip_update$mean_PRCP[is.na(daily_precip_update$mean_PRCP)] = 0
# 
# 
# ### FORMAT AND WRITE PRECIP FILE
# #Format the update to attach to the original precip file
# daily_precip_update=data.frame(mean_PRCP = daily_precip_update$mean_PRCP, Date = daily_precip_update$Date)
# daily_precip_update$Date = paste(str_pad(day(daily_precip_update$Date), 2, pad="0"),
#                                  str_pad(month(daily_precip_update$Date), 2, pad="0"),
#                                  year(daily_precip_update$Date), sep = "/")
# daily_precip_update$mean_PRCP = daily_precip_update$mean_PRCP / 1000 #convert to meters
# 
# #read in original data (wys 1991-2011)
# daily_precip_orig = read.table(file.path(ref_data_dir,"precip_orig.txt"))
# colnames(daily_precip_orig) = colnames(daily_precip_update)
# 
# #combine and write as text file
# daily_precip_updated = rbind(daily_precip_orig, daily_precip_update)
# write.table(daily_precip_updated, file = file.path(SWBM_file_dir, "precip.txt"),
#             sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)
# 
# 
#. ############################################
# # Initial RefET writing attempt
# #to do: webscrape cimis? (login?)
# #units? 
# et_dl_may2019 = read.csv(file.path(ref_data_dir,"spatial_eto_report.csv"))
# 
# ##generate new et record
# et_dl_may2019$Date = as.Date(et_dl_may2019$Date, format = "%m/%d/%Y")
# et = subset(et_dl_may2019, Date >= model_start_date & Date <= model_end_date)
# et = data.frame(Date = et$Date, ETo_mm = et$ETo..mm.day.)
# 
# #Update existing record
# #subset update for ET, build update dataframe in same format as original input file
# et_update_allcol = subset(et_dl_may2019, Date >= as.Date("2011-10-01") & Date <= model_end_date)
# ETo_m = et_update_allcol$ETo..mm.day./1000
# et_update = data.frame(ETo_m)
# et_update$ETo_in = et_update$ETo_m * 39.3701 #convert to inches
# et_update$Date = et_update_allcol$Date = paste(str_pad(day(et_update_allcol$Date), 2, pad="0"),
#                                                str_pad(month(et_update_allcol$Date), 2, pad="0"),
#                                                year(et_update_allcol$Date), sep = "/")
# 
# #Read in original file
# ref_et_orig = read.table(file.path(ref_data_dir,"ref_et_orig.txt"))
# # head(ref_et_orig)
# # plot(as.Date(ref_et_orig$V3, format = "%d/%m/%Y"),ref_et_orig$V1, type = "l")
# 
# #Combine into updated ET record, check for continuity, and write file
# colnames(ref_et_orig) = colnames(et_update)
# ref_et_updated = rbind(ref_et_orig, et_update)
# # plot(as.Date(ref_et_updated$Date, format = "%d/%m/%Y"),ref_et_updated$ETo_m, type = "l")
# # sum(is.na(ref_et_updated$ETo_m))
# write.table(ref_et_updated, file = file.path(SWBM_file_dir, "ref_et.txt"),
#             sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)


# .polygons_table.txt ------------------------------------------------------

#Plots for this analysis:
# Fields completely in, completely out, or partially in the adjudicated zone
# Fields "inside" (defined as >5% field area inside adj zone) and outside adj zone
# Pie charts - more alfalfa in adjudicated zone that outside, but pretty diverse
# Water source map
# Area calculations

# if(landuse_scenario != "basecase"){
#   # Run spatial analysis
#   
#   # read in datasets
#   poly = readOGR(dsn = ref_data_dir, layer = "Landuse_20190219")
#   # adj = get_postgis_query(siskiyou_spatial, "SELECT * FROM scott_adjudicated_area", geom_name = "geom")
#   # writeOGR(adj, dsn = ref_data_dir, layer = "Adjudicated Area",driver = "ESRI Shapefile")
#   adj = readOGR(dsn = ref_data_dir, layer = "Adjudicated Area")
#   
#   poly = spTransform(poly, crs(adj))
#   
#   #Calculate the fraction of each polygon *inside* the adjudicated zone.
#   poly$fraction_in_adj = 0 # initialize new column
#   
#   for(i in 1:max(poly$Polynmbr)){
#     selector = poly$Polynmbr==i
#     field = poly[selector,]
#     if(!gIsValid(field)){
#       field = gBuffer(field, width = 0) # fix invalid geoms and warn about it
#       print(paste("polygon number",i,"invalid"))} 
#     
#     if(gIntersects(adj, field)){
#       overlap_poly = intersect(field, adj)
#       poly$fraction_in_adj[selector] = round(area(overlap_poly) / area(field), digits = 3) # otherwise get leftover digit junk
#     }
#   }
#   
#   # Amend polygons.txt table
#   
#   # Note: this does not preserve "notes" column
#   poly_column_classes = c(rep("integer",4),
#                           "numeric","integer","numeric","numeric",
#                           "integer","integer","character",
#                           rep("NULL",16)) # get rid of empty columns in the text file
#   
#   poly_tab = read.table(file.path(time_indep_dir,"polygons_table.txt"),
#                         header = T, comment.char = "!", 
#                         fill = T, sep = "\t", colClasses = poly_column_classes)
#   colnames(poly_tab) = c("Field_ID",colnames(poly_tab)[2:11])
#   
#   
#   in_adj_threshold = 0.05 # lower numbers mean, just a sliver overlapping are included
#   fields_inside_adj = poly$Polynmbr[poly$fraction_in_adj > in_adj_threshold]
#   fields_outside_adj = poly$Polynmbr[!(poly$Polynmbr %in% fields_inside_adj)]
#   
#   
#   fields_inside_adj = poly[poly$fraction_in_adj > in_adj_threshold,]
#   fields_outside_adj =  poly[poly$fraction_in_adj <= in_adj_threshold,]
#   
#   # poly_saved = poly
#   # # poly = poly_saved
#   # poly_tab_saved = poly_tab
#   
#   # png(filename = "parcels_adj_zone_for_scenario.png", height=9.5, width = 6.2, units = "in", res = 300)
#   # plot(basin, lwd = 2, main = "Fields considered within Adjudicated Area for SVIHM Scenario")
#   # plot(fields_inside_adj, add=T, col = rgb(0,0,1, 0.5))
#   # plot(fields_outside_adj, add=T, col = rgb(0,1,0,0.3))
#   # plot(adj, add=T, border = "red", lwd = 2)#col = rgb(1,0,0,0.3))
#   # legend(x = "bottomleft",
#   #        lwd = c(2, 2, NA,NA), pch = c(NA,NA, 15, 15),
#   #        col = c("black","red","blue", "green"),
#   #        legend = c("Groundwater Basin", "Adjudicated Zone", "Inside Fields", "Outside Fields"))
#   # dev.off()
#   
#   # sq_m_per_acre = 4046.86
#   # sum(area(fields_inside_adj))/ unique(area(basin)) #sum(area(poly))
#   # sum(area(fields_inside_adj)) / sq_m_per_acre # convert to acres
#   # sum(area(fields_outside_adj))/ sum(area(poly)) #unique(area(basin)) #
#   # sum(area(fields_outside_adj)) / sq_m_per_acre # convert to acres
#   
#   # Explore/visualize
#   # area(adj)/sum(area(poly))*100 # covers 20.0% of the land area in this shapefile
#   # area(adj)/area(basin)*100 # covers 15.7% of the land area of the basin
#   # plot(poly)
#   # plot(adj, add=T, col=rgb(.5,.5,.5,0.5))
#   
#   
#   #Data exploration - visualize 3 categories of field. All in, all out, or overlapping
#   
#   # fields_totally_in_adj = poly[poly$fraction_in_adj==1,]
#   # fields_totally_outside_adj = poly[poly$fraction_in_adj==0,]
#   # fields_partially_in_adj = poly[poly$fraction_in_adj !=0 & poly$fraction_in_adj !=1 ,]
#   
#   # sum(area(fields_totally_in_adj)) / sq_m_per_acre # convert to acres
#   # sum(area(fields_totally_outside_adj)) / sq_m_per_acre # convert to acres
#   # sum(area(fields_partially_in_adj)) / sq_m_per_acre # convert to acres
#   
#   
#   # png(filename = "parcels_adj_zone.png", height=9.5, width = 6.2, units = "in", res = 300)
#   # plot(basin, lwd = 2, main = "Relation of Parcels to the Adjudicated Zone")
#   # plot(fields_totally_in_adj, add=T, col = rgb(0,0,1,.7), border = "gray")
#   # plot(fields_totally_outside_adj, add=T, col = rgb(0,1,0,.7), border = "gray")
#   # plot(fields_partially_in_adj, add=T, col = rgb(1,0,0,.7), border = "gray")
#   # legend(x = "bottomleft",
#   #        col = c("black","blue","green","red"),
#   #        pch=c(NA,rep(15,3)),
#   #        lwd = c(2,rep(NA,3)),
#   #        legend = c("Basin Boundary","Completely Inside", "Completely Outside", "Overlaps Zone Border"))
#   # dev.off()
#   
#   # # fractions of the basin area
#   # "Totally inside adjudicated zone:"
#   # sum(area(fields_totally_in_adj)) / sum(area(poly))
#   # sum(area(fields_totally_in_adj)) / area(basin)
#   # "Totally outside adjudicated zone:"
#   # sum(area(fields_totally_outside_adj)) / sum(area(poly))
#   # sum(area(fields_totally_outside_adj)) / area(basin)
#   # "Overlapping adjudicated zone boundary:"
#   # sum(area(fields_partially_in_adj))/ sum(area(poly))
#   # sum(area(fields_partially_in_adj))/ area(basin)
#   
#   
#   # #Color by water source - initialize columns
#   # poly$wat_source_from_svihm = NA
#   # poly$wat_source_from_svihm_color = NA
#   # 
#   # # make water source color table
#   # wat_source = c(1,2,3,4,5,999)
#   # wat_source_descrip = c("SW","GW","Mixed", "Sub-irrigated","Dry","Unknown")
#   # wat_source_color = c("dodgerblue","firebrick2","darkorchid1","green","yellow","gray")
#   # 
#   # wat_source_df = data.frame(ws_code = wat_source,
#   #                            descrip = wat_source_descrip,
#   #                            color = wat_source_color)
#   # #match codes and colors
#   # poly$wat_source_from_svihm = poly_tab$Water_Source[match(poly$Polynmbr, poly_tab$Field_ID)]
#   # poly$wat_source_from_svihm_color = wat_source_df$color[match(poly$wat_source_from_svihm, wat_source_df$ws_code)]
#   # 
#   # # #plot
#   # png(filename = "parcels_wat_source.png", height=9.5, width = 6.2, units = "in", res = 300)
#   # plot(basin, lwd = 2, main = "Irrigation Water Sources")
#   # plot(poly, add=T, col = poly$wat_source_from_svihm_color, border = "darkgray")
#   # legend(x = "bottomleft", legend = wat_source_df$descrip,
#   #        col = wat_source_df$color, pch = rep(15,6))
#   # 
#   # # plot(adj, add=T, col= rgb(0.5,0.5,0.5,0.5))
#   # dev.off()
#   
#   #Color by land use - initialize columns
#   # poly$landuse = NA
#   # poly$landuse_color = NA
#   # 
#   # #Land use key:
#   # # alfalfa = 25; palture = 2; ET_noIrr = 3 (native veg, assumes kc of 0.6);  noET_noIrr = 4; water = 6
#   # 
#   # # make a land use/crop type color table
#   # lu = c(25,2,3,4,6)
#   # lu_descrip = c("Alfalfa","Pasture","ET_noIrr","noET_noIrr", "Water")
#   # lu_color = c("forestgreen","darkolivegreen2","wheat","red","dodgerblue")
#   # 
#   # lu_df = data.frame(lu_code = lu,
#   #                    descrip = lu_descrip,
#   #                    color = lu_color)
#   # #match codes and colors
#   # poly$landuse_from_svihm = poly_tab$Landuse[match(poly$Polynmbr, poly_tab$Field_ID)]
#   # poly$landuse_color = lu_df$color[match(poly$landuse_from_svihm, lu_df$lu_code)]
#   # 
#   # # #plot
#   # png(filename = "parcels_landuse_basecase.png", height=9.5, width = 6.2, units = "in", res = 300)
#   # plot(basin, lwd = 2, main = "Land Use / Crop Type: Basecase")
#   # plot(poly, add=T, col = poly$landuse_color, border = "darkgray", lwd=0.5)
#   # legend(x = "bottomleft", 
#   #        legend = c(lu_df$descrip[1:2],"ET, No Irr. (Native Veg.)",
#   #                   "No ET, No Irr. (e.g. Tailings)","Water"),
#   #        col = lu_df$color, pch = rep(15,6))
#   # 
#   # # plot(adj, add=T, col= rgb(0.5,0.5,0.5,0.5))
#   # dev.off()
#   
#   
#   # #Water source fraction of parcel area  
#   # wat_source_area = aggregate(area(poly), by = list(poly$wat_source_from_svihm), FUN = "sum")
#   # wat_source_area$frac_parcels = round(wat_source_area$x / sum(area(poly)),digits = 3)
#   # wat_source_area$frac_basin = round(wat_source_area$x / unique(area(basin)),digits = 3)
#   
#   # #proportion of fields in each category 
#   # par(mfrow = c(3,1))
#   # pie(summary(poly_tab$Landuse), main = "all fields")
#   # pie(summary(poly_tab$Landuse[poly_tab$Field_ID %in% fields_inside_adj]), 
#   #     main = paste("fields in adj,", in_adj_threshold, "overlap needed"))
#   # pie(summary(poly_tab$Landuse[!(poly_tab$Field_ID %in% fields_inside_adj)]), 
#   #     main = paste("fields outside adj,", in_adj_threshold, "overlap needed"))
#   
#   
#   
#   # # find weird parcels
#   # pdf(file = "weird_parcel_finder.pdf",width = 8.5, height = 11)
#   # for(i in 1:length(poly$Polynmbr)){
#   #   field = poly[poly$Polynmbr==i,]
#   #   # print(area(field))
#   #   if(area(field) > 10^5 & i %in% fields_partially_in_adj$Polynmbr) {
#   #     plot(basin, main = i)
#   #     plot(field, col = "pink", border = "red", lwd = 2,add=T)
#   #     
#   #   }
#   # }
#   # dev.off()
#   
#   ## Weird: 714. a couple others.
#   #in the find-weird-parcels analysis, I decided to exclude parcels with 5% or less of their area in the adjudicated zone.
#   
#   
#   # Amend the SVIHM polygons table and write
#   poly_tab_amended = poly_tab
#   
#   #Land use key:
#   # alfalfa = 25
#   # palture = 2
#   # ET_noIrr = 3 (native veg, assumes kc of 0)
#   # noET_noIrr = 4
#   # water = 6
#   
#   # # make water source color table
#   # wat_source = c(1,2,3,4,5,999)
#   # wat_source_descrip = c("SW","GW","Mixed", "Sub-irrigated","Dry","Unknown")
#   # wat_source_color = c("dodgerblue","firebrick2","darkorchid1","green","yellow","gray")
#   
#   
#   if(landuse_scenario == "native veg outside adj"){
#     poly_tab_amended$Landuse[poly_tab_amended$Field_ID %in% fields_outside_adj$Polynmbr] = 3
#   } else if(landuse_scenario == "native veg, gw and mixed fields, outside adj"){
#     poly_tab_amended$Landuse[poly_tab_amended$Field_ID %in% fields_outside_adj$Polynmbr &
#                                poly_tab_amended$Water_Source %in% c(2, 3)] = 3
#   }
#   
#   # png(filename = "parcels_landuse_natveg_gwmixed_outside_adj.png", height=9.5, width = 6.2, units = "in", res = 300)
#   # # png(filename = "parcels_landuse_natveg_outside_adj.png", height=9.5, width = 6.2, units = "in", res = 300)
#   # #match codes and colors
#   # poly$landuse_from_svihm = poly_tab_amended$Landuse[match(poly$Polynmbr, poly_tab_amended$Field_ID)]
#   # poly$landuse_color = lu_df$color[match(poly$landuse_from_svihm, lu_df$lu_code)]
#   # 
#   # 
#   # plot(basin, lwd = 2, main = "Land Use / Crop Type: NV GWM OA")
#   # plot(poly, add=T, col = poly$landuse_color,
#   #      border = "darkgray", lwd=0.5)
#   # legend(x = "bottomleft", 
#   #        legend = c(lu_df$descrip[1:2],"ET, No Irr. (Native Veg.)",
#   #                   "No ET, No Irr. (e.g. Tailings)","Water"),
#   #        col = lu_df$color, pch = rep(15,6))  
#   # # plot(adj, add=T, col= rgb(0.5,0.5,0.5,0.5))
#   # dev.off()
#   
#   
#   # poly$landuse = poly_tab_amended$Landuse[match(poly$Polynmbr, poly_tab_amended$Field_ID)]
#   # plot(poly, col = poly$landuse)
#   
#   write.table(x = poly_tab_amended, quote = F,
#               file = file.path(SWBM_file_dir, "polygons_table.txt"),
#               sep = "\t",col.names=TRUE, row.names = F)
# }

