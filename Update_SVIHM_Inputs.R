#Generate input files for SVIHM
#Goal: Use this R script to generate ALL the text files that go into the SWBM and the SVIHM modflow model. 
#This script must be in the outer SVIHM directory to access all the other files (Streamflow Regression, SWBM, MODFLOW)

library(lubridate)
library(stringr)
library(dplyr)
#Rules:
## dates in R are treated as YYYY-MM-DD for convenience. Dates in the model input files are DD-MM-YYYY.


# SET VARIABLES -----------------------------------------------------------


#Set drive for collecting all SWBM input files and SVIHM modflow files
isRStudio <- Sys.getenv("RSTUDIO") == "1"
if(isRStudio == TRUE){
  library(rstudioapi)
  proj_dir <- dirname(getActiveDocumentContext()$path)
}
if(isRStudio == FALSE){
  library(here)
  proj_dir <- dirname(here::here("SVIHM_Streamflow_Regression_Model.R"))
}

Stream_Regression_dir = file.path(proj_dir, "Streamflow_Regression_Model")
time_indep_dir = file.path(proj_dir, "SVIHM_Input_Files", "time_independent_input_files")
SWBM_file_dir = file.path(proj_dir, "SWBM")
MF_file_dir = file.path(proj_dir, "SVIHM_Input_Files","Historical_WY1991_2018")


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
num_days = diff(model_months_plus_one)

num_stress_periods = length(model_months)


#.#############################################################################
# ### SWBM INPUTS ------------------------------------------------
#.###########################################################################

# Copy files that don't change if the time period gets extended 

# crop_coeff_mult.txt
# daily_out.txt
# ET_Cells_DZ.txt
# irr_eff.txt
# MAR_Fields.txt
# No_Flow_SVIHM.txt
# polygons_table.txt
# Recharge_Zones_SVIHM.txt
#  well_list_by_polygon.txt
#  well_summary.txt 


#  Drains_m3day.txt and Drains_initial_m3day.txt --------------------------------------

drains_vector = rep(0, num_stress_periods)

write.table(drains_vector, file = file.path(SWBM_file_dir, "Drains_initial_m3day.txt"),
            sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)
write.table(drains_vector, file = file.path(SWBM_file_dir, "Drains_m3day.txt"),
            sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)

#  general_inputs.txt ------------------------------------------------

#Update number of stress periods
gen_inputs = c(paste0("2119  167  ", num_stress_periods, "  440  210  1.4 UCODE Basecase"),
"! num_fields, num_irr_wells, num_stress_periods, nrow, ncol, RD_Mult, UCODE/PEST, Basecase/MAR/ILR/MAR_ILR")
write.table(gen_inputs, file = file.path(SWBM_file_dir, "general_inputs.txt"),
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

noaa = read.csv("C:/Users/ckouba/Git/SVIHM/SVIHM/SWBM/update2018notes/noaa_precip_fjn_cal.csv")
head(noaa)
noaa$DATE = as.Date(noaa$DATE)

cal = subset(noaa, STATION=="USC00041316" & DATE >= model_start_date & DATE <= model_end_date)
cal = select(cal, DATE, PRCP)
fj = subset(noaa, STATION=="USC00043182" & DATE >= model_start_date & DATE <= model_end_date)
fj = select(fj, DATE, PRCP)

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

#check the NA values
which(is.na(daily_precip_update$mean_PRCP))
daily_precip_update[is.na(daily_precip_update$mean_PRCP),]
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
daily_precip_update=select(daily_precip_update, mean_PRCP, Date)
daily_precip_update$Date = paste(str_pad(day(daily_precip_update$Date), 2, pad="0"),
                                 str_pad(month(daily_precip_update$Date), 2, pad="0"),
                                 year(daily_precip_update$Date), sep = "/")
daily_precip_update$mean_PRCP = daily_precip_update$mean_PRCP / 1000 #convert to meters

#read in original data (wys 1991-2011)
daily_precip_orig = read.table("C:/Users/ckouba/Git/SVIHM/SVIHM/SWBM/input/precip.txt")
colnames(daily_precip_orig) = colnames(daily_precip_update)

#combine and write as text file
daily_precip_updated = rbind(daily_precip_orig, daily_precip_update)
write.table(daily_precip_updated, file = file.path(SWBM_file_dir, "precip.txt"),
            sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)



#  ref_et.txt ----------------------------------------------------

#to do: webscrape cimis? (login?)
#units? 
et_dl_may2019 = read.csv("C:/Users/ckouba/Git/SVIHM/SVIHM/SWBM/update2018notes/spatial_eto_report.csv")

##generate new et record
et_dl_may2019$Date = as.Date(et_dl_may2019$Date, format = "%m/%d/%Y")
et = subset(et_dl_may2019, Date >= model_start_date & Date <= model_end_date)
et = select(et, Date, ETo..mm.day.)
colnames(et) = c("ETo_mm","Date")

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
ref_et_orig = read.table("C:/Users/ckouba/Git/SVIHM/SVIHM/SWBM/input/ref_et.txt")
# head(ref_et_orig)
# plot(as.Date(ref_et_orig$V3, format = "%d/%m/%Y"),ref_et_orig$V1, type = "l")

#Combine into updated ET record, check for continuity, and write file
colnames(ref_et_orig) = colnames(et_update)
ref_et_updated = rbind(ref_et_orig, et_update)
# plot(as.Date(ref_et_updated$Date, format = "%d/%m/%Y"),ref_et_updated$ETo_m, type = "l")
# sum(is.na(ref_et_updated$ETo_m))
write.table(ref_et_updated, file = file.path(SWBM_file_dir, "ref_et.txt"),
            sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)


#  SFR_PEST_TPL.txt --------------------------------------------------
# TO DO: figure out these calibration files

#  SFR_UCODE_JTF.txt -------------------------------------------------
# TO DO: figure out these calibration files


#  streamflow_input.txt ------------------------------------------

# To update, EITHER: 
# a) Update Fort Jones gauge record is Streamflow_Regression_Model folder, OR
# b) build webscraper for latest stream data?
"https://waterdata.usgs.gov/ca/nwis/dv?cb_00060=on&format=rdb&site_no=11519500&referred_module=sw&period=&begin_date=1941-10-01&end_date=2019-05-19"
#pull down from server, since it webscraped recently?
#THEN: change the Fort Jones USGS file reference in the Regression script.

setwd(Stream_Regression_dir)
source(file.path(Stream_Regression_dir,'SVIHM_Streamflow_Regression_Model.R'))
generate_streamflow_input_txt(end_date = as.Date("2018/9/30"))

#To do: Copy streamflow_input.txt to the SWBM inputs file.


#  SVIHM_ETS_template.txt --------------------------------------------



#  SVIHM_SFR_template.txt -------------------------------------------



#  SVIHM_WEL_template.txt --------------------------------------------


#.#############################################################################
# ### MODFLOW INPUTS ------------------------------------------------

#Copy files that don't change if the time period gets extended 
# SVIHM.bas
# SVIHM.gag




# SVIHM.dis ---------------------------------------------------------------

#Requires a file SVIHM.dis in position in the MF_file_dir.

#Update the number of stress periods in the header
#Update the number of time steps in each stress period at end of file (dataset 7)
dis_text = readLines(con = file.path(MF_file_dir, "SVIHM.dis"), n = -1)
start_of_dataset7 = which(dis_text == "  31  31  1  TR")[1] #Locate first line of data set 7 (specifying Oct 1990)
dataset_7 = paste0("  ", num_days, "  ", num_days, "  1  TR")

dis_text_updated = c(dis_text[1], 
                     paste0(" 2  440  210  ", num_stress_periods,"  4  2"),
                     dis_text[4:start_of_dataset7-1], #really puzzled why a 4-index calls line 3 in this, but this works
                     dataset_7)
writeLines(dis_text_updated, con = file.path(MF_file_dir, "SVIHM.dis"))

# SVIHM.drn ---------------------------------------------------------------

setwd(time_indep_dir)

#Assign an elevation, conductance and layer for every cell in the Discharge Zone
DZ_Cells = read.table(file.path(time_indep_dir,"Reference_files","drn_ET_Cells_Discharge_Zone.txt"), header = T, sep = ",")
Model_Surface = matrix(t(read.table(file.path(time_indep_dir,"Reference_files",'drn_Layer_1_top_z.txt'))),
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




# SVIHM.hob ---------------------------------------------------------------

#Currently, hacking in a hard-coded contour drive on my local computer. non-transferrable.
#To do: convert the data cleaning script into a utility. 
# store wl data and this data-cleaning utility script in the model folders.
# make it callable from this input-generating script.
# OR: pull it down from the damn data base eventually!

# To do: match DWR_1 values with casgem IDs
# to do: add more wells to the hob file. need to locate grid cell and offset. 
#to do: check - can I trust the "basin" designation on here? Maybe assign that with a spatial join in the cleaning script

### 1) Get a cleaned water level dataframe. Use same cleaning protocol as for contours
source('C:/Users/ckouba/Git/Contours/02_clean_data.R')
wl = clean_data_for_contours(CASGEM = TRUE, VMP = TRUE, WDL = FALSE, CGWL = TRUE, contours = TRUE, hydrographs = FALSE)

### 2) Retain just the water level obs from Scott Valley in the model period
wl = wl[wl$basin == "Scott River Valley" & !is.na(wl$wse) & wl$date >= model_start_date & wl$date <= model_end_date,]
wl$local_well_number = as.character(wl$local_well_number)

### 3) Update total number of well observations and write preamble for .hob file.
num_wl_obs = dim(wl)[1] # Calculate number of observations for preamble
preamble = c('# MODFLOW2000 Head Observation File','# Groundwater Vistas also writes drawdown targets here',
paste0('  ',num_wl_obs,'  0  0 500 -999'), '  1.0  1.0')
setwd(MF_file_dir)
write(preamble, file = 'SVIHM.hob', append = F)

### 4) Read in existing .hob info file
hob_info = read.table(file.path(time_indep_dir,"Reference_files","hob_wells.txt"), header = F, skip = 4)
colnames(hob_info) = c('OBSNAM', 'LAYER', 'ROW', 'COLUMN', 'IREFSP', 'TOFFSET', 'ROFF', 'COFF', 'HOBS', 'STATISTIC', 'STAT-FLAG', 'PLOT-SYMBOL')

### 5) For each observation point, write a) topline of well info and b) details for each observation
for(i in 1:length(hob_info$OBSNAM)){
  #### 5a) Calculate how many observations are present for each well ("IREFSP" neg values in Dataset 6?)
  obs_loc = hob_info$OBSNAM[i]
  IREFSP = sum(wl$local_well_number == obs_loc, na.rm=T) #calculate number of measurements for this well
  #### 5b) Update IREFSP and write topline (info for each well location)
  topline = paste(as.character(hob_info[i,1]), hob_info[i,2], hob_info[i,3], hob_info[i,4],
                  -1*IREFSP, format(hob_info[i,6],nsmall = 1), format(hob_info[i,7],nsmall = 6), 
                  format(hob_info[i,8],nsmall = 6), format(hob_info[i,9],nsmall = 6), 
                  format(hob_info[i,10],nsmall = 6), hob_info[i,11], hob_info[i,12], sep = "  ")
  write(topline, file = 'SVIHM.hob', append = T)
  
  #### 5c) write up the observations for each well
  ##### 5c1) Create unique observation ids 
  wl_subset = wl[wl$local_well_number == obs_loc & !is.na(wl$local_well_number),]
  obs_id_num = 1:IREFSP
  obs_id = paste0(obs_loc, obs_id_num)
  ##### 5c2) convert sample date to stress period and time offset
  samp_years = year(dates)
  samp_months = month(dates)
  stress_periods = (samp_years - year(model_start_date))*12 + samp_months - (month(model_start_date)-1)
  offset_days = day(dates)
  ##### 5c3) Convert wse from feet to meters
  meters_asl = 0.30480 * wl_subset$wse
  #### 5c4) Write the vectors into the file. Attach a bunch of 1s as Modflow flags
  cat(sprintf("%12s%12i%12.6f%12.6f%12.6f%12.6f%8i%8i\n", 
              obs_id, stress_periods, offset_days, meters_asl,
              rep(1, IREFSP), rep(1, IREFSP), rep(1, IREFSP), rep(1, IREFSP)), file = 'SVIHM.hob', append = T)
#CURRENT TO DO ITEM: DEBUG SPRINTF FUNCTION
    
}


# SVIHM.nam ---------------------------------------------------------------



# SVIHM.nwt ---------------------------------------------------------------



# SVIHM.obs ---------------------------------------------------------------


# SVIHM.oc ----------------------------------------------------------------



# SVIHM.pvl ---------------------------------------------------------------



# SVIHM.upw ---------------------------------------------------------------




# SVIHM.zone --------------------------------------------------------------



# SVIHM_bc.nam ------------------------------------------------------------



# SVIHM_scen.nam ----------------------------------------------------------



# Starting_Heads_L1.txt ---------------------------------------------------



# Starting_Heads_L2.txt ---------------------------------------------------



