#Generate input files for SWBM
#Goal: Use this R script to generate ALL the text files that go into the SWBM. 

library(lubridate)
library(stringr)
library(dplyr)
#Rules:
## dates in R are treated as YYYY-MM-DD for convenience. Dates in the model input files are DD-MM-YYYY.


#Set drive for collecting all SWBM input files
input_file_table_dir = "C:/Users/ckouba/Git/SVIHM/SVIHM/R_Files/Pre-Processing/Input_File_Generation_Tables"
SWBM_file_dir = "C:/Users/ckouba/Git/SVIHM/SVIHM/SWBM/write_input_files_test_2019.05.19"

#Create date vectors
start_year = 1990 # WY 1991
end_year = 2018
model_whole_water_years = TRUE

if(model_whole_water_years)
{start_month = "10"; start_day = "01"; end_month = "09"; end_day = "30"} 
if(!model_whole_water_years) {print("please define month and day for start and end of model run period")}

model_start_date = as.Date(paste(start_year, start_month, start_day, sep = "-"))
model_end_date = as.Date(paste(end_year, end_month, end_day, sep = "-"))
model_days = seq(from = model_start_date, to = model_end_date, by = "days")
model_months = seq(from = model_start_date, to = model_end_date, by = "month")


# Filename: kc_alfalfa.txt -------------------------------------------------
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

kc_alfalfa_df = data.frame(kc_alf_days)
kc_alfalfa_df$day = paste(str_pad(day(model_days), 2, pad = "0"), 
                          str_pad(month(model_days), 2, pad = "0"), 
                          year(model_days), sep = "/")

#write alfalfa kc file
write.table(kc_alfalfa_df, file = file.path(SWBM_file_dir, "kc_alfalfa.txt"),
            sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)



# Filename: kc_pasture.txt ------------------------------------------------

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

kc_pasture_df = data.frame(kc_pas_days)
kc_pasture_df$day = paste(str_pad(day(model_days), 2, pad = "0"), 
                          str_pad(month(model_days), 2, pad = "0"), 
                          year(model_days), sep = "/")

#write pasture kc file
write.table(kc_pasture_df, file = file.path(SWBM_file_dir, "kc_pasture.txt"),
            sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)


# Filename: kc_grain.txt --------------------------------------------------
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
kc_grain_df$day = paste(str_pad(day(model_days), 2, pad = "0"), 
                          str_pad(month(model_days), 2, pad = "0"), 
                          year(model_days), sep = "/")

#write grain kc file
write.table(kc_grain_df, file = file.path(SWBM_file_dir, "kc_grain.txt"),
            sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)

# #Check overall Kc sums. checks out. 
# sum(as.numeric(as.character(test$V1)), na.rm=T)
# sum(kc_grain_df$kc_grain_days[model_days < as.Date("2011-10-01")])


# Filename: precip.txt ----------------------------------------------------

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


# Filename: ref_et.txt ----------------------------------------------------

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
 plot(as.Date(ref_et_orig$V3, format = "%d/%m/%Y"),ref_et_orig$V1, type = "l")

#Combine into updated ET record, check for continuity, and write file
colnames(ref_et_orig) = colnames(et_update)
ref_et_updated = rbind(ref_et_orig, et_update)
plot(as.Date(ref_et_updated$Date, format = "%d/%m/%Y"),ref_et_updated$ETo_m, type = "l")

# sum(is.na(ref_et_updated$ETo_m))
write.table(ref_et_updated, file = file.path(SWBM_file_dir, "ref_et.txt"),
            sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)

# Filename: streamflow_input.txt ------------------------------------------
#To do: build webscraper for latest stream data?
"https://waterdata.usgs.gov/ca/nwis/dv?cb_00060=on&format=rdb&site_no=11519500&referred_module=sw&period=&begin_date=1941-10-01&end_date=2019-05-19"
#pull down from server, since it webscraped recently?

#Convert the streamflow regression model to be callable from this script. 
# Specify end date, I guess. 



# Filename: Drains_initial_m3day.txt --------------------------------------

drains_orig = read.csv("C:/Users/ckouba/Git/SVIHM/SVIHM/SWBM/input/Drains_initial_m3day.txt")
dim(drains_orig)

drains_vector = rep(0, length(model_months)+1)
write.table(drains_vector, file = file.path(SWBM_file_dir, "Drains_initial_m3day.txt"),
            sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)

write.table(drains_vector, file = file.path(SWBM_file_dir, "Drains_m3day.txt"),
            sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)



# ** Modflow input files -----------------------------------------------------

strper_data = read.csv(file.path(input_file_table_dir,"stress_period_days.csv"))
nstr = max(strper_data$stress_period)


# SVIHM.dis ---------------------------------------------------------------

 #did this by hand/in excel. to do: code thi sup


# SVIHM.hob ---------------------------------------------------------------

#To do: add head observations

# SVIHM.drn ---------------------------------------------------------------
#Script that writes drain package input file for discahrge zone cells in SVIHMv3.1

DZ_Cells = read.csv(file.path(input_file_table_dir,'ET_Cells_Discharge_Zone.txt'))
Model_Surface = matrix(t(read.table(file.path(input_file_table_dir,'Layer_1_top_z.txt'))),nrow = 440, ncol = 210, byrow = T)
Elevation = matrix(NaN,length(DZ_Cells$row))
for (i in 1:length(DZ_Cells$row)){
  Elevation[i] = Model_Surface[DZ_Cells$row[i],DZ_Cells$column[i]]
}

Conductance = matrix(10000,length(DZ_Cells$row))
Layer = matrix(1,length(DZ_Cells$row))
Drains = cbind(Layer, DZ_Cells$row, DZ_Cells$column, round(Elevation,2), Conductance)
rep_drains = matrix(-1, 251)   # repeat value of -1 for n-1 Stress periods to resuse drains specified in first stress period

#open drain package file and write the top block
drain_file_path = file.path(SWBM_file_dir, "SVIHM.drn")
write('# MODFLOW Drain Package File - Drains applied at land surface within discharge zone',
      file = drain_file_path, append = F)
# write('PARAMETER  0  0', file = 'SVIHM.drn', append = T) #MXACTD IDRNCB
write('        2869        50', file = drain_file_path, append = T) #MXACTD IDRNCB
for (i in 1:nstr){
  write(paste('         2869         0                      Stress Period',i), file = drain_file_path, append = T, sep = NA)  #ITMP  NP
  cat(sprintf(' '), file = drain_file_path, append = T)
  cat(sprintf("%10i%10i%10i%10.2f%10.3e\n", Drains[,1], Drains[,2],Drains[,3],Drains[,4],Drains[,5]), file = drain_file_path, append = T)
}

# drain_file <- file(file.path(SWBM_file_dir, "SVIHM.drn"))
# #close file
# close(drain_file)




# SVIHM.oc ----------------------------------------------------------------

strper_data = read.csv(file.path(input_file_table_dir,"stress_period_days.csv"))

#define blocks for before and after stress periods
top_block =   c("  HEAD SAVE UNIT 30","  HEAD PRINT FORMAT 0",
                "  DRAWDOWN SAVE UNIT 31","  DRAWDOWN PRINT FORMAT 0","  COMPACT BUDGET AUX")
# after_each_str_per_block =  c("         SAVE HEAD","        SAVE DRAWDOWN","         SAVE BUDGET","        PRINT BUDGET")
after_each_str_per_block =  c("         SAVE HEAD","         SAVE BUDGET","        PRINT BUDGET")


#initialize oc file
oc_file = top_block

for(i in 1:nstr){
  nday = strper_data$days_in_month[strper_data$stress_period == i]
  str_per = rep(i, nday)
  tstp = 1:nday
  str_per_block = paste0("  PERIOD ", str_per, "  STEP ",tstp)
  
  oc_file = append(oc_file, str_per_block)
  oc_file = append(oc_file, after_each_str_per_block)
}
write.table(oc_file, file = file.path(SWBM_file_dir, "SVIHM.oc"),
            sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)



#SVIHM.rch?

