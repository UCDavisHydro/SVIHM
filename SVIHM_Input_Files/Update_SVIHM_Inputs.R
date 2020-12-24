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

# rm(list = ls())

# Scenario Selection ------------------------------------------------------

# Recharge and flow scenarios
recharge_scenario = "Basecase" # Can be Basecase/MAR/ILR/MAR_ILR
flow_scenario = "Basecase" # Can be Basecase/Flow_Lims. Flow limits on stream diversion specified in "available ratio" table.

# Irrigation demand: Different from the kc_CROP_mult values in crop_coeff_mult.txt, which is used for calibrating.
irr_demand_mult = 1 # Can be 1 (Basecase) or < 1 or > 1 (i.e., reduced or increased irrigation; assumes land use change)
natveg_kc = 0.6 # Default: 0.6. Set at 1.0 for major natveg scenarios. 

# Month and day of the final day of alfalfa irrigation season. 
# Default is Aug 31, 8/31
alf_irr_stop_mo = 8 # Month as month of year (7 = July)
alf_irr_stop_day = 1 
# Convert to month of wy (Oct=1, Nov=2, ..., Jul = 10, Aug=11, Sep=0)
if(alf_irr_stop_mo<9){alf_irr_stop_mo = alf_irr_stop_mo + 3
}else{alf_irr_stop_mo = alf_irr_stop_mo - 9}

#Land use scenario. 
landuse_scenario = "Basecase" # Default: Basecase. For attribution study: major_natveg
# landuse_scenario_detail = "native veg, gw and mixed fields, outside adj"
# landuse_scenario_detail = "native veg outside adj"
# landuse_scenario_detail = "native veg, gw and mixed fields, inside adj"
# landuse_scenario_detail = "native veg inside adj"
# landuse_scenario_detail = "native veg, gw and mixed fields, all cultivated fields"
# landuse_scenario_detail = "native veg all cultivated fields"



# Overall scenario identifier. Also makes the directory name; must match folder
# scenario_name = "basecase"
# scenario_name = "mar_ilr" # "ilr" "mar" 
# scenario_name = "flowlims" #"irrig_0.9" "irrig_0.8
# scenario_name = "alf_irr_stop_jul10" 
scenario_name = "alf_irr_stop_aug01" 
# scenario_name = "natveg_outside_adj"
# scenario_name = "natveg_gwmixed_outside_adj"
# scenario_name = "natveg_inside_adj" 
# scenario_name = "natveg_gwmixed_inside_adj" 



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
start_year = 1990 # WY 1991; do not change
end_year = 2018 # through WY of this year
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
num_days = diff(model_months_plus_one) #number of days in each stress period/month

num_stress_periods = length(model_months)



#.#############################################################################
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

  copy_these_files = c( "daily_out.txt", #"Discharge_Zone_Cells.txt",
                        "irr_eff.txt", "MAR_Fields.txt",
                       "No_Flow_SVIHM.txt", "Recharge_Zones_SVIHM.txt",
                       #"polygons_table.txt", "crop_coeff_mult.txt",
                       "well_list_by_polygon.txt", "well_summary.txt")

if(landuse_scenario %in% c("basecase","Basecase")){ copy_these_files = c(copy_these_files, "polygons_table.txt")}
if(natveg_kc==0.6){copy_these_files = c(copy_these_files, "crop_coeff_mult.txt")}


setwd(time_indep_dir)
file.copy(copy_these_files, SWBM_file_dir)



# CALIBRATION FILES -------------------------------------------------------

# TO DO: figure out these calibration files. They feed into the SWBM.
 # SFR_PEST_TPL.txt
 # SFR_UCODE_JTF.txt
 # SVIHM_ETS_template.txt
 # SVIHM_SFR_template.txt
 # SVIHM_WEL_template.txt

 
copy_these_files = c("SFR_PEST_TPL.txt", "SFR_UCODE_JTF.txt", "SVIHM_ETS_template.txt", 
                     "SVIHM_SFR_template.txt", 'SVIHM_WEL_template.txt')

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
      overlap_poly = intersect(field, adj)
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
  
  
  in_adj_threshold = 0.05 # lower numbers mean, just a sliver overlapping are included
  fields_inside_adj = poly$Polynmbr[poly$fraction_in_adj > in_adj_threshold]
  fields_outside_adj = poly$Polynmbr[!(poly$Polynmbr %in% fields_inside_adj)]
  
  
  fields_inside_adj = poly[poly$fraction_in_adj > in_adj_threshold,]
  fields_outside_adj =  poly[poly$fraction_in_adj <= in_adj_threshold,]
  
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
  poly$landuse_from_svihm = poly_tab$Landuse[match(poly$Polynmbr, poly_tab$Field_ID)]
  poly$landuse_color = lu_df$color[match(poly$landuse_from_svihm, lu_df$lu_code)]
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
  # 
  # 
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
  extinction_depth = dz_cells
  # Assign cells in natural vegetation fields a 4.5 m extinction depth
  extinction_depth[poly_cells %in% unique(poly_nv$Polynmbr)] = 4.5
  # Assign Discharge Zone cells an extinction depth of 0.5 m
  extinction_depth[dz_cells == 1] = 0.5
  
  # Save  extinction depth matrix
  write.table(x =extinction_depth, 
              file = file.path(SWBM_file_dir, "ET_Cells_Extinction_Depth.txt"),
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



# TO DO: Calculate cells that are nat veg and need to get the 15-foot extinction depth


# Write file as NatVeg_Outside_DZ_Cells.txt

#  general_inputs.txt ------------------------------------------------

#Update number of stress periods and scenario information
# Does not include irr_demand_mult, since that gets incorporated into the kc files
gen_inputs = c(
  paste("2119  167", num_stress_periods, 
        "440  210  1.4 UCODE",
        "! num_fields, num_irr_wells, num_stress_periods, nrow, ncol, RD_Mult, UCODE/PEST", 
        sep = "  "),
  paste(recharge_scenario, flow_scenario,     
        alf_irr_stop_mo, alf_irr_stop_day,
        "! Basecase/MAR/ILR/MAR_ILR, Basecase/Flow_Lims, alf_irr_stop_mo  alf_irr_stop_day",
        sep = "  "),
  paste(landuse_scenario, "! Basecase/Major_NatVeg")
  )

write.table(gen_inputs, file = file.path(SWBM_file_dir, "general_inputs.txt"),
            sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)


# stress_period_days.txt --------------------------------------------------

write.table(num_days, file = file.path(SWBM_file_dir, "stress_period_days.txt"),
            sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)


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
days_in_crop_stage = c(37, 26, 32, 36) # includes start and end days in each stage

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

# #Check overall Kc sums. checks out. 
# sum(as.numeric(as.character(test$V1)), na.rm=T)
# sum(kc_grain_df$kc_grain_days[model_days < as.Date("2011-10-01")])


#  precip.txt ----------------------------------------------------

#BEFORE USE: check to see that updated end model year propogates to analyses script (?)

file1=file.path(scenario_dev_dir,"precip_regressed.txt")
file2=file.path(SWBM_file_dir,"precip.txt")

if(!file.exists(file1)){
  declare_dir_in_analyses_script = FALSE #Prevents the input_analyses script from overwriting SWBM_dir
  source(file.path(input_files_dir,'SVIHM_input_analyses.R'))
  write_swbm_precip_input_file()
}

file.copy(from=file1, to = file2, overwrite=T)




#  ref_et.txt ----------------------------------------------------

#BEFORE USE: check to see that updated end model year propogates to analyses script (?)
# declare_dir_in_analyses_script = FALSE #Prevents the input_analyses script from overwriting directories
# source(file.path(input_files_dir,'SVIHM_input_analyses.R'))

file1=file.path(scenario_dev_dir,"ref_et_monthly.txt")
file2=file.path(SWBM_file_dir,"ref_et.txt")

if(!file.exists(file1)){
  declare_dir_in_analyses_script = FALSE #Prevents the input_analyses script from overwriting SWBM_dir
  source(file.path(input_files_dir,'SVIHM_input_analyses.R'))
  write_swbm_et_input_file()
}

file.copy(from=file1, to = file2)


#  streamflow_input.txt ------------------------------------------

# To update, EITHER: 
# a) Update Fort Jones gauge record is Streamflow_Regression_Model folder, OR
# b) build webscraper for latest stream data?
# "https://waterdata.usgs.gov/ca/nwis/dv?cb_00060=on&format=rdb&site_no=11519500&referred_module=sw&period=&begin_date=1941-10-01&end_date=2019-05-19"
#pull down from server, since it webscraped recently?
#THEN: change the Fort Jones USGS file reference in the Regression script.

file1=file.path(Stream_Regression_dir,'streamflow_input.txt')
file2 = file.path(SWBM_file_dir, 'streamflow_input.txt')
if(!file.exists(file1)){
  source(file.path(Stream_Regression_dir,'SVIHM_Streamflow_Regression_Model.R'))
  generate_streamflow_input_txt(end_date = model_end_date)
}
file.copy(file1, file2)



# instream_flow_available_ratio.txt ------------------------------------------------

#read in FJ flow and CDFW recommended flow for FJ gauge
library(dataRetrieval)
library(lubridate)
fj_num = "11519500"
fjd_all = readNWISdv(siteNumbers = fj_num, parameterCd="00060" )
fjd_all = renameNWISColumns(fjd_all)
# Subset for model period
fjd = fjd_all[fjd_all$Date >= model_start_date & fjd_all$Date <= model_end_date,]

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

# plot(avail_monthly$Group.1, avail_monthly$x/fj_monthly$x, type = "l", ylab = "avail / rec ratio", ylim = c(0,1))
# plot(avail_monthly$stress_period,avail_monthly$avail_flow_m3_month, type = "l")
# grid()

# avail_monthly$stress_period = as.numeric(as.factor(avail_monthly$stress_period)) #converts to 1 to 336 for each stress period

#format for table
avail_monthly[,colnames(avail_monthly) != "avail_ratio"] = NULL

write.table(avail_monthly, file = file.path(SWBM_file_dir, "instream_flow_available_ratio.txt"),
            sep = " ", quote = FALSE, col.names = F, row.names = FALSE)

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
# SVIHM.nam
# SVIHM.nwt
# SVIHM.pvl
# SVIHM.upw
# SVIHM.zone
# Starting_Heads_L1.txt
# Starting_Heads_L2.txt

copy_these_files = c("SVIHM.bas", "SVIHM.gag", "SVIHM.nam", "SVIHM.nwt", "SVIHM.pvl", "SVIHM.upw","SVIHM.zone",
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


# Now working here: get the fkn wl data from somewhere so you can make a hob file and run the modflow hist model



# SVIHM.ets ---------------------------------------------------------------

# TO DO: 

#Look at the deficit, take an average deficit (by month), and assign that 
# deficit to an ET package (ET package will only extract water from GW 
# table according to extinction formula.). ET deficit assigned by SP, by cell.
#At 6 ft depth, 100% of potential/target ET
# At 15 ft depth, 0% of potential ET/target 
#(linearly interpolate between) 

#Updated to do: adjusted this in SWBM; now writes ETS with multiple different extinction depths
# but only simulates ETS in cells with either a) discharge zone or b) nat veg in major-natveg scenarios

# SVIHM.hob ---------------------------------------------------------------
#Head Observation Package

#Currently, hacking in a hard-coded contour drive on my local computer. non-transferrable.
#To do: pull wl down from the damn data base eventually!


### TO DO: Join additional wells to the model grid and add their well loc. info to reference hob_info table.

### 1) Get a cleaned water level dataframe
wl = data.frame(tbl(siskiyou_tables, "wl_observations"))
stations = data.frame(tbl(siskiyou_tables, "wl_data_wells"))

#merge SWN (long well names) onto wl obs table.
stations_swn = stations[,c('well_code', 'swn')]
wl$swn = NA
wl$swn = stations_swn$swn[match(wl$well_code, stations_swn$well_code)]

# Clean for SVIHM 
#### 1a) Make A4_1 match the name in the water level file (A41). (This avoids confusion on unique WL observation IDs in modflow.)
wl$well_code = as.character(wl$well_code)
wl$well_code[wl$well_code == "A41"] = "A4_1"

#### 1b) Convert longer DWR well names (in the WL file) to the abbreviations in SVIHM hob_info table
mon_info = read.csv( file.path(ref_data_dir, "Monitoring_Wells_Names.csv"))
dwr_in_model_short_names = c("DWR_1","DWR_2","DWR_3","DWR_4","DWR_5")
dwr_in_model_long_names = as.character(mon_info$Well_ID_2[mon_info$Well_ID %in% dwr_in_model_short_names])
# Select based on State Well Number (SWN), which is listed as the local well number in the VMP data. 
# CASGEM "local well number" is sometimes the SWN but in the cases of DWR_1 and DWR_3 is an unrelated abbreviation.
replaceables = unique(wl$swn[wl$swn %in% dwr_in_model_long_names])
replaceables_selector = wl$swn %in% replaceables
#Replace local well numbers of replaceables with the abbreviations (DWR_1 etc) of replaceable SWNs
wl$well_code[replaceables_selector] = 
  as.character(mon_info$Well_ID[match(wl$swn[replaceables_selector],mon_info$Well_ID_2)])

### 2) Read in  .hob info file (well location information)
hob_info = read.table(file.path(ref_data_dir,"hob_wells.txt"), header = F, skip = 4)
colnames(hob_info) = c('OBSNAM', 'LAYER', 'ROW', 'COLUMN', 'IREFSP', 'TOFFSET', 'ROFF', 'COFF', 'HOBS', 'STATISTIC', 'STAT-FLAG', 'PLOT-SYMBOL')


### 3) Retain just the water level obs (no NAs) from Scott Valley in the model period
### TEMPORARY: retain just the ones that have hob_info.
### TO DO: Join additional wells to the model grid and add their well loc. info to reference hob_info table.
# wl = wl[wl$basin == "Scott River Valley" & !is.na(wl$wse_ft) & wl$well_code %in% hob_info$OBSNAM &
#           wl$date >= model_start_date & wl$date <= model_end_date,]
wl = wl[!is.na(wl$wse_ft) & wl$well_code %in% hob_info$OBSNAM &
          wl$date >= model_start_date & wl$date <= model_end_date,]

### 4) Update total number of well observations and write preamble for .hob file.
num_wl_obs = dim(wl)[1] # Calculate number of observations for preamble
preamble = c('# MODFLOW2000 Head Observation File','# Groundwater Vistas also writes drawdown targets here',
paste0('  ',num_wl_obs,'  0  0 500 -999'), '  1.0  1.0')
setwd(MF_file_dir)
write(preamble, file = 'SVIHM.hob', append = F)


### 5) For each observation point, write a) topline of well info and b) details for each observation
for(i in 1:length(hob_info$OBSNAM)){
  #### 5a) Info for each well location
  ##### 5a1) Calculate number of observations for an individual well loc ("IREFSP" neg values in Dataset 3)
  obs_loc = hob_info$OBSNAM[i]
  IREFSP = sum(wl$well_code == obs_loc, na.rm=T) #calculate number of measurements for this well
  if(IREFSP < 1){next} #skip it if there's no observations for this well name. It's currently happening for A4_1 and the DWR wells.
  
  #### 5a2) Update IREFSP and write topline (info for each well location)
  topline = paste(as.character(hob_info[i,1]), hob_info[i,2], hob_info[i,3], hob_info[i,4],
                  -1*IREFSP, format(hob_info[i,6],nsmall = 1), format(hob_info[i,7],nsmall = 6), 
                  format(hob_info[i,8],nsmall = 6), format(hob_info[i,9],nsmall = 6), 
                  format(hob_info[i,10],nsmall = 6), hob_info[i,11], hob_info[i,12], sep = "  ")
  write(topline, file = 'SVIHM.hob', append = T)
  
  #### 5a3) Write "1" to signify hydraulic heads = obs in Data Set 5
  write("  1", file = 'SVIHM.hob', append = T)
  
  #### 5b) write up the observations for each well
  ##### 5b1) Create unique observation ids 
  wl_subset = wl[wl$well_code == obs_loc & !is.na(wl$well_code),]
  obs_id_num = 1:IREFSP
  obs_id = paste0(obs_loc, obs_id_num)
  ##### 5b2) convert sample date to stress period and time offset
  dates = wl_subset$date
  samp_years = year(dates)
  samp_months = month(dates)
  stress_periods = (samp_years - year(model_start_date))*12 + samp_months - (month(model_start_date)-1)
  offset_days = day(dates)
  ##### 5b3) Convert wse_ft from feet to meters
  meters_asl = 0.3048 * wl_subset$wse_ft #0.3048006096012 * wl_subset$wse_ft
  #### 5b4) Write the vectors into the file. Attach a bunch of 1s as Modflow flags
  cat(sprintf("%12s%12i%12.6f%12.6f%12.6f%12.6f%8i%8i\n", 
              obs_id, stress_periods, offset_days, meters_asl,
              rep(1, IREFSP), rep(1, IREFSP), rep(1, IREFSP), rep(1, IREFSP)), file = 'SVIHM.hob', append = T)
}


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

file.copy(file.path(svihm_dir,"Batch_Scripts",'Run_SVIHM_forward.bat'), MF_file_dir)
file.copy(file.path(svihm_dir,"R_Files","Model",'Update_SVIHM_Drain_Inflows.R'), MF_file_dir)
file.copy(file.path(svihm_dir,"R_Files","Model",'Update_SVIHM_Starting_Heads.R'), MF_file_dir)


# OPTIONAL: copy output to Results folder for post-processing -------------

# # Scenario Selection ------------------------------------------------------
# recharge_scenario = "Basecase" # Can be Basecase/MAR/ILR/MAR_ILR
# flow_scenario = "Basecase" # Can be Basecase/Flow_Lims. Flow limits on stream diversion specified in "available ratio" table.
# irr_demand_mult = 0.9 # Can be 1 (Basecase) or < 1 or > 1 (i.e., reduced or increased irrigation; assumes land use change)(increased irrigation)
# 
# # Scenario name for SWBM and MODFLOW
# scenario_name = "irrig_0.9" #also makes the directory name; must match folder
# 
# ## Directories for running the scenarios (files copied at end of script)
# 
# SWBM_file_dir = file.path(svihm_dir, "SWBM", scenario_name)
# MF_file_dir = file.path(svihm_dir, "MODFLOW",scenario_name)

#Copy flow tables on the mainstem
file.copy(from = file.path(MF_file_dir,"Streamflow_FJ_SVIHM.dat"),
          to = file.path(results_dir,paste0("Streamflow_FJ_SVIHM_",scenario_name,".dat")))
file.copy(from = file.path(MF_file_dir,"Streamflow_Pred_Loc_2.dat"), 
          to = file.path(results_dir,paste0("Streamflow_Pred_Loc_2_",scenario_name,".dat")))
file.copy(from = file.path(MF_file_dir,"Streamflow_Pred_Loc_3.dat"), 
          to = file.path(results_dir,paste0("Streamflow_Pred_Loc_3_",scenario_name,".dat")))

file.copy(from = file.path(MF_file_dir,"SVIHM.sfr"), 
          to = file.path(results_dir,paste0("SVIHM_",scenario_name,".sfr")))
file.copy(from = file.path(SWBM_file_dir,"monthly_groundwater_by_luse.dat"), 
          to = file.path(results_dir,paste0("monthly_groundwater_by_luse_",scenario_name,".dat")))










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

