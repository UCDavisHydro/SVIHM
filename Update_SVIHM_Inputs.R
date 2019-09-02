#Generate input files for SVIHM
#Goal: Use this R script to generate ALL the text files that go into the SWBM and the SVIHM modflow model. 
#This script must be in the outer SVIHM directory to access all the other files (Streamflow Regression, SWBM, MODFLOW)

library(lubridate)
library(stringr)
library(dplyr)
#Rules:
## dates in R are treated as YYYY-MM-DD for convenience. Dates in the model input files are DD-MM-YYYY.

# rm(list = ls())

# SETUP -------------------------------------------------------------------


# 1) Set drives for collecting all SWBM input files and SVIHM modflow files

# 1a) Set project directory. 
#This code allows it to automatically detect the location of this R script.
isRStudio <- Sys.getenv("RSTUDIO") == "1"
if(isRStudio == TRUE){ library(rstudioapi); proj_dir <- dirname(getActiveDocumentContext()$path)}
if(isRStudio == FALSE){ library(here); proj_dir <- dirname(here::here("Update_SVIHM_Inputs.R"))}

# 1b) Set directories for data used in update and output file locations

## Data used in update
Stream_Regression_dir = file.path(proj_dir, "Streamflow_Regression_Model")
time_indep_dir = file.path(proj_dir, "SVIHM_Input_Files", "time_independent_input_files")
ref_data_dir = file.path(proj_dir, "SVIHM_Input_Files", "reference_data")
## Directory used to archive the input files for each scenario
model_inputs_dir = file.path(proj_dir, "SVIHM_Input_Files","Historical_WY1991_2018")
## Directories for running the scenarios (files copied at end of script)
SWBM_file_dir = file.path(proj_dir, "SWBM", "hist")
MF_file_dir = file.path(proj_dir, "MODFLOW","hist")


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
# ET_Cells_DZ.txt
# irr_eff.txt
# MAR_Fields.txt
# No_Flow_SVIHM.txt
# polygons_table.txt
# Recharge_Zones_SVIHM.txt
# well_list_by_polygon.txt
# well_summary.txt

copy_these_files = c("crop_coeff_mult.txt", "daily_out.txt", "ET_Cells_DZ.txt", "irr_eff.txt", "MAR_Fields.txt",
                     "No_Flow_SVIHM.txt", "polygons_table.txt", "Recharge_Zones_SVIHM.txt", 
                     "well_list_by_polygon.txt", "well_summary.txt")

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


#  Drains_m3day.txt and Drains_initial_m3day.txt --------------------------------------

drains_vector = c("#Initial Drain Flow", rep(0, num_stress_periods))

write.table(drains_vector, file = file.path(SWBM_file_dir, "Drains_m3day.txt"),
            sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)
write.table(drains_vector, file = file.path(SWBM_file_dir, "Drains_initial_m3day.txt"),
            sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)

#  general_inputs.txt ------------------------------------------------

#Update number of stress periods
gen_inputs = c(paste0("2119  167  ", num_stress_periods, "  440  210  1.4 UCODE Basecase"),
"! num_fields, num_irr_wells, num_stress_periods, nrow, ncol, RD_Mult, UCODE/PEST, Basecase/MAR/ILR/MAR_ILR")
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

#write grain kc file
write.table(kc_grain_df, file = file.path(SWBM_file_dir, "kc_grain.txt"),
            sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)

# #Check overall Kc sums. checks out. 
# sum(as.numeric(as.character(test$V1)), na.rm=T)
# sum(kc_grain_df$kc_grain_days[model_days < as.Date("2011-10-01")])


#  precip.txt ----------------------------------------------------
# to do: Make precip writing callable from SVIHM_input_analyses

file.copy()

#  ref_et.txt ----------------------------------------------------
# to do: Make precip writing callable from SVIHM_input_analyses



#  streamflow_input.txt ------------------------------------------

# To update, EITHER: 
# a) Update Fort Jones gauge record is Streamflow_Regression_Model folder, OR
# b) build webscraper for latest stream data?
# "https://waterdata.usgs.gov/ca/nwis/dv?cb_00060=on&format=rdb&site_no=11519500&referred_module=sw&period=&begin_date=1941-10-01&end_date=2019-05-19"
#pull down from server, since it webscraped recently?
#THEN: change the Fort Jones USGS file reference in the Regression script.

setwd(Stream_Regression_dir)
source(file.path(Stream_Regression_dir,'SVIHM_Streamflow_Regression_Model.R'))
generate_streamflow_input_txt(end_date = model_end_date)

file.copy('streamflow_input.txt', SWBM_file_dir)


# OPERATOR: RUN SWBM ------------------------------------------------------

#After it is done running, copy the files written by SWBM into the modflow directory.
copy_these_files = c("SVIHM.wel","SVIHM.sfr", "SVIHM.ets", "SVIHM.rch")
setwd(SWBM_file_dir)
file.copy(copy_these_files, MF_file_dir)

#.#############################################################################
# ### MODFLOW INPUTS ------------------------------------------------

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


# SVIHM.hob ---------------------------------------------------------------
#Head Observation Package

#Currently, hacking in a hard-coded contour drive on my local computer. non-transferrable.
#To do: pull wl down from the damn data base eventually!
### TO DO: Join additional wells to the model grid and add their well loc. info to reference hob_info table.

### 1) Get a cleaned water level dataframe. Use same cleaning protocol as for contours
# Get WL data from DMS folder
# FOR NOW: Don't run the whole script; just selectively run the setup (db connection) and WL web scraper data
# SiskiyouGSP2022/Data_Management_System/tabular_data_upload.R
wl_items = get_wl_data(VMP = TRUE, DWR = TRUE, clean = TRUE)
wl = wl_items[[1]]
stations = wl_items[[2]]

#merge SWN (long well names) onto wl obs table.
stations_swn = select(stations, well_code, swn)
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



# Scratch work ------------------------------------------------------------

#plot DWR_1 through 5
wells = read.table ("C:/Users/ckouba/Git/SVIHM/SVIHM/SWBM/up2018/well_summary.txt", header = T)
head(wells)

library(raster)
library(maptools)
mon = shapefile("C:/Users/ckouba/Documents/UCD/SiskiyouSGMA/Data_Exploration/Scott_Legacy_GIS/Monitoring_Wells_All.shp")

dwr_names = c("DWR_1","DWR_2","DWR_3","DWR_4","DWR_5")
dwr_in = mon[mon$Well_ID %in% dwr_names,]
dwr_not_in = mon[mon$In_SVIHM == "No",]
plot(mon)
plot(dwr_in, pch = 19, col = "blue", add=T)
plot(dwr_not_in, pch = 19, col = "red", add=T)
pointLabel(dwr_in@coords, labels = dwr_in$Well_ID)

pointLabel(dwr_not_in@coords, labels = dwr_in$Well_ID_2)
write.csv(mon@data, file.path(ref_data_dir, "Monitoring_Wells_Names.csv"), row.names = F)

####################################
# Original Precip File Writing attempt
#to do: web scraper
# CDEC data
#FJN daily
"http://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?Stations=SFJ&SensorNums=41&dur_code=D&Start=1990-10-01&End=2019-05-20"
#CHA hourly accumulated
"http://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?Stations=CHA&SensorNums=2&dur_code=H&Start=1990-10-01T00%3A00&End=2019-05-20"

#NOAA data
#daily data ftp site: https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/
#daily data ftp readme: https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt
#daily data documentation: https://www1.ncdc.noaa.gov/pub/data/cdo/documentation/GHCND_documentation.pdf


#https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/all/USC00041316.dly
#https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/all/USC00043182.dly


noaa = read.csv(file.path(ref_data_dir,"noaa_precip_fjn_cal.csv"))
noaa$DATE = as.Date(noaa$DATE)

cal = subset(noaa, STATION=="USC00041316" & DATE >= model_start_date & DATE <= model_end_date)
cal = data.frame(DATE = cal$DATE, PRCP = cal$PRCP)
fj = subset(noaa, STATION=="USC00043182" & DATE >= model_start_date & DATE <= model_end_date)
fj = data.frame(DATE = fj$DATE, PRCP = fj$PRCP)

# # Compare visually
# # plot(noaa$DATE, noaa$PRCP, xlim = as.Date(c("1991-10-01","2018-09-01")), type = "l")
# plot(cal$DATE, cal$PRCP, type = "l", col = "red")
# lines(fj$DATE, fj$PRCP, #xlim = as.Date(c("1991-10-01","2018-09-01")),
#      type = "l", col = "blue", add=T)

### COMPARISON TABLE FOR 2 STATIONS
daily_precip = data.frame(model_days)
daily_precip = merge(x = daily_precip, y = cal, by.x = "model_days", by.y = "DATE", all=TRUE)
daily_precip = merge(x = daily_precip, y = fj, by.x = "model_days", by.y = "DATE", all=TRUE)
colnames(daily_precip)=c("Date","PRCP_mm_cal", "PRCP_mm_fj")
daily_precip$mean_PRCP = apply(X = daily_precip[,2:3], MARGIN = 1, FUN = mean, na.rm=T)

#isolate the precip data after the end of the original model
daily_precip_update = subset(daily_precip, Date >= as.Date("2011-10-01"))
daily_precip_update = subset(daily_precip, Date >= as.Date("2011-10-01"))

### HANDLE NAs

# #check the NA values
# which(is.na(daily_precip_update$mean_PRCP))
# daily_precip_update[is.na(daily_precip_update$mean_PRCP),]
#shit that's a huge gap in december 2012. to do: find alternate precip data source for this gap
#TEMPORARY SOLUTION FOR NOW: 
#just put the average of all December dates in that window
# dec 4-30th

#calculate average precip values for that day in december over the whole model record (wy1991+)
precip_dec = subset(daily_precip, month(Date) == 12 & day(Date) >=4 & day(Date) <=30)
precip_dec$day = day(precip_dec$Date)
daily_precip_dec = aggregate(precip_dec$mean_PRCP, by=list(precip_dec$day), FUN=mean, na.rm=T)

#replace NAN values in Dec 2012 with average values over whole record
daily_precip_update$mean_PRCP[daily_precip_update$Date >= as.Date("2012-12-04") 
                              & daily_precip_update$Date <= as.Date("2012-12-30")]=daily_precip_dec$x

#set remaining days with NA in both records to 0
daily_precip_update$mean_PRCP[is.na(daily_precip_update$mean_PRCP)] = 0


### FORMAT AND WRITE PRECIP FILE
#Format the update to attach to the original precip file
daily_precip_update=data.frame(mean_PRCP = daily_precip_update$mean_PRCP, Date = daily_precip_update$Date)
daily_precip_update$Date = paste(str_pad(day(daily_precip_update$Date), 2, pad="0"),
                                 str_pad(month(daily_precip_update$Date), 2, pad="0"),
                                 year(daily_precip_update$Date), sep = "/")
daily_precip_update$mean_PRCP = daily_precip_update$mean_PRCP / 1000 #convert to meters

#read in original data (wys 1991-2011)
daily_precip_orig = read.table(file.path(ref_data_dir,"precip_orig.txt"))
colnames(daily_precip_orig) = colnames(daily_precip_update)

#combine and write as text file
daily_precip_updated = rbind(daily_precip_orig, daily_precip_update)
write.table(daily_precip_updated, file = file.path(SWBM_file_dir, "precip.txt"),
            sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)


############################################
# Initial RefET writing attempt
#to do: webscrape cimis? (login?)
#units? 
et_dl_may2019 = read.csv(file.path(ref_data_dir,"spatial_eto_report.csv"))

##generate new et record
et_dl_may2019$Date = as.Date(et_dl_may2019$Date, format = "%m/%d/%Y")
et = subset(et_dl_may2019, Date >= model_start_date & Date <= model_end_date)
et = data.frame(Date = et$Date, ETo_mm = et$ETo..mm.day.)

#Update existing record
#subset update for ET, build update dataframe in same format as original input file
et_update_allcol = subset(et_dl_may2019, Date >= as.Date("2011-10-01") & Date <= model_end_date)
ETo_m = et_update_allcol$ETo..mm.day./1000
et_update = data.frame(ETo_m)
et_update$ETo_in = et_update$ETo_m * 39.3701 #convert to inches
et_update$Date = et_update_allcol$Date = paste(str_pad(day(et_update_allcol$Date), 2, pad="0"),
                                               str_pad(month(et_update_allcol$Date), 2, pad="0"),
                                               year(et_update_allcol$Date), sep = "/")

#Read in original file
ref_et_orig = read.table(file.path(ref_data_dir,"ref_et_orig.txt"))
# head(ref_et_orig)
# plot(as.Date(ref_et_orig$V3, format = "%d/%m/%Y"),ref_et_orig$V1, type = "l")

#Combine into updated ET record, check for continuity, and write file
colnames(ref_et_orig) = colnames(et_update)
ref_et_updated = rbind(ref_et_orig, et_update)
# plot(as.Date(ref_et_updated$Date, format = "%d/%m/%Y"),ref_et_updated$ETo_m, type = "l")
# sum(is.na(ref_et_updated$ETo_m))
write.table(ref_et_updated, file = file.path(SWBM_file_dir, "ref_et.txt"),
            sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)