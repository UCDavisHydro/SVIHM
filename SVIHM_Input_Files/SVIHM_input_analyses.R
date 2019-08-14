# Precip and ET input analysis

#Rules:
## dates in R are treated as YYYY-MM-DD for convenience. Dates in the model input files are DD-MM-YYYY.

# SETUP -------------------------------------------------------------------

library(stringr)
library(lubridate)
library(dplyr)

# 1) Set drives for collecting all SWBM input files and SVIHM modflow files

# 1a) Set project directory. 
#This code allows it to automatically detect the location of this R script.
isRStudio <- Sys.getenv("RSTUDIO") == "1"
if(isRStudio == TRUE){ library(rstudioapi); proj_dir <- dirname(dirname(getActiveDocumentContext()$path))}
if(isRStudio == FALSE){ library(here); proj_dir <- dirname(here::here("Update_SVIHM_Inputs.R"))}

# 1b) Set directories for data used in update and output file locations

## Data used in update
Stream_Regression_dir = file.path(proj_dir, "Streamflow_Regression_Model")
time_indep_dir = file.path(proj_dir, "SVIHM_Input_Files", "time_independent_input_files")
ref_data_dir = file.path(proj_dir, "SVIHM_Input_Files", "reference_data")
## Directory used to archive the input files for each scenario
model_inputs_dir = file.path(proj_dir, "SVIHM_Input_Files","Historical_WY1991_2018")
## Directories for running the scenarios (files copied at end of script)
SWBM_file_dir = file.path(proj_dir, "SWBM", "up2018")
MF_file_dir = file.path(proj_dir, "MODFLOW","up2018")


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

#station abbreviation, number in the NOAA dataset, and column number in the daily_precip table
station_id_table = data.frame(abbrev = c("cal", "fj", "et", "gv"),
                              station = c("USC00041316", "USC00043182", "USC00042899", "USC00043614"),
                              col_num = c(1,2,3,4) + 1)


# Subfunctions ------------------------------------------------------------

make_daily_precip = function(wx_table,
                             record_start_date = as.Date("1943-01-01"), 
                             record_end_date = model_end_date){
  
  record_days = seq(from = record_start_date, to = record_end_date, by = "days")
  
  
  #Subset data into stations
  noaa=wx_table
  cal = subset(noaa, STATION=="USC00041316" & DATE >= record_start_date & DATE <= record_end_date)
  cal = data.frame(DATE = cal$DATE, PRCP = cal$PRCP)
  fj = subset(noaa, STATION=="USC00043182" & DATE >= record_start_date & DATE <= record_end_date)
  fj = data.frame(DATE = fj$DATE, PRCP = fj$PRCP)
  et = subset(noaa, STATION == "USC00042899" & DATE >= record_start_date & DATE <= record_end_date)
  et =data.frame(DATE = et$DATE, PRCP = et$PRCP)
  gv = subset(noaa, STATION == "USC00043614" & DATE >= record_start_date & DATE <= record_end_date)
  gv =data.frame(DATE = gv$DATE, PRCP = gv$PRCP)

  #read in original data (wys 1991-2011)
  daily_precip_orig = read.table(file.path(ref_data_dir,"precip_orig.txt"))
  colnames(daily_precip_orig) = c("PRCP", "Date")
  daily_precip_orig$Date = as.Date(daily_precip_orig$Date, format = "%d/%m/%Y")
  daily_precip_orig$PRCP = daily_precip_orig$PRCP*1000
  
  
  ### COMPARISON TABLE FOR 2 STATIONS AND ORIG DATA
  daily_precip = data.frame(record_days)
  daily_precip = merge(x = daily_precip, y = cal, by.x = "record_days", by.y = "DATE", all=TRUE)
  daily_precip = merge(x = daily_precip, y = fj, by.x = "record_days", by.y = "DATE", all=TRUE)
  daily_precip = merge(x = daily_precip, y = et, by.x = "record_days", by.y = "DATE", all=TRUE)
  daily_precip = merge(x = daily_precip, y = gv, by.x = "record_days", by.y = "DATE", all=TRUE)
  daily_precip = merge(x = daily_precip, y = daily_precip_orig, by.x = "record_days", by.y = "Date", all=TRUE)
  colnames(daily_precip)=c("Date","PRCP_mm_cal", "PRCP_mm_fj","PRCP_mm_et","PRCP_mm_gv","PRCP_mm_orig")
  
  # Compare original data to FJ-Cal mean
  daily_precip$mean_PRCP_fjcal = apply(X = daily_precip[,2:3], MARGIN = 1, FUN = mean, na.rm=T)
  
  #Add aggregation columns
  daily_precip$month_day1 = floor_date(daily_precip$Date, "month")
  daily_precip$water_year = year(daily_precip$Date)
  daily_precip$water_year[month(daily_precip$Date) > 9] = daily_precip$water_year[month(daily_precip$Date) > 9]+1
  
  return(daily_precip)
}

make_model_coeffs_table = function(months = 1:12, 
                                   ys = c("fj", "cal"), 
                                   xs = c("fj", "cal", "gv", "et")){  
  
  model_coeff = expand.grid(months = months, Y_var = ys, X_var = xs)
  #take out matching combinations (e.g. x = fj, y = fj)
  model_coeff = model_coeff[as.character(model_coeff$Y_var) != as.character(model_coeff$X_var),]
  #Add model parameter columns
  model_coeff$intercept=NA; model_coeff$coeff_m=NA; model_coeff$r2=NA
  
  for(mnth in months)
    for(y in ys){
      for(x in xs){
        if(y == x){next} #no need for autoregression
        #Find the output table row
        model_coeff_index = which(model_coeff$months==mnth & model_coeff$X_var==x & model_coeff$Y_var==y)
        #Find the daily precip column number for X and Y stations
        col_num_x = station_id_table$col_num[station_id_table$abbrev == x]
        col_num_y = station_id_table$col_num[station_id_table$abbrev == y]
        #Declare X and Y in daily precip
        X = daily_precip[month(daily_precip$Date) == mnth, col_num_x]
        Y = daily_precip[month(daily_precip$Date) == mnth, col_num_y]
        # 
        model_name = paste(y, "on", x, "in", month.abb[mnth] )
        model = lm(Y~ 0 + X)
        if(length(coef(model)) ==2){model_coeff[model_coeff_index, 4:5] = coef(model)}
        if(length(coef(model)) ==1){
          model_coeff[model_coeff_index, 4] = 0; model_coeff[model_coeff_index, 5] = coef(model)}
        model_coeff$r2[model_coeff_index] = summary(model)$r.sq
      }
    }
  return(model_coeff)
}

fill_fj_cal_gaps_regression_table = function(model_coeff){

    #make list of regressions (Y, X or Xes)
  # for each month
  # for FJ and Cal (Ys)
  # for FJ, Cal, ET, and GV (Xs)
  ## But not combinations of X?
  # Then, rank each month-Y combo according to correlation coeff
  
  #Initialize table of model coefficients

  
  # use regressions to generate filled-in precip records for FJ and Cal
  p_record = daily_precip[daily_precip$Date >= model_start_date & daily_precip$Date <= model_end_date,]
  
  #Fill in gaps in FJ
  p_record$fj_interp = p_record$PRCP_mm_fj
  for(i in 1:length(p_record$fj_interp)){
    if(is.na(p_record$fj_interp[i])){
      # figure out what case this is ()
      #find the coefs for the best regression for fj
      coefs = model_coeff[model_coeff$Y_var == "fj" & model_coeff$months == month(p_record$Date[i]), ]
      
      i_best = which.max(coefs$r2)
      i_worst = which.min(coefs$r2)
      i_2ndbest = setdiff(1:3, c(i_best, i_worst))
      
      x_best = coefs$X_var[i_best]
      col_best = station_id_table$col_num[station_id_table$abbrev == x_best]
      x_2ndbest = coefs$X_var[i_2ndbest]
      col_2ndbest = station_id_table$col_num[station_id_table$abbrev == x_2ndbest]
      x_worst = coefs$X_var[i_worst]
      col_worst = station_id_table$col_num[station_id_table$abbrev == x_worst]
      
      #predict the rainfall
      if(!is.na(p_record[i,col_best])){
        p_record$fj_interp[i] =  coefs$intercept[i_best] + p_record[i,col_best] * coefs$coeff_m[i_best]
      } else if(!is.na(p_record[i,col_2ndbest])) {
        p_record$fj_interp[i] =  coefs$intercept[i_2ndbest] + p_record[i,col_2ndbest] * coefs$coeff_m[i_2ndbest]
      } else if(!is.na(p_record[i,col_worst])){
        p_record$fj_interp[i] =  coefs$intercept[i_worst] + p_record[i,col_worst] * coefs$coeff_m[i_worst]
      }
      
    }
  }
  
  #Fill in gaps in Cal
  p_record$cal_interp = p_record$PRCP_mm_cal
  for(i in 1:length(p_record$cal_interp)){
    if(is.na(p_record$cal_interp[i])){
      # figure out what case this is ()
      #find the coefs for the best regression for cal
      coefs = model_coeff[model_coeff$Y_var == "cal" & model_coeff$months == month(p_record$Date[i]), ]
      
      i_best = which.max(coefs$r2)
      i_worst = which.min(coefs$r2)
      i_2ndbest = setdiff(1:3, c(i_best, i_worst))
      
      x_best = coefs$X_var[i_best]
      col_best = station_id_table$col_num[station_id_table$abbrev == x_best]
      x_2ndbest = coefs$X_var[i_2ndbest]
      col_2ndbest = station_id_table$col_num[station_id_table$abbrev == x_2ndbest]
      x_worst = coefs$X_var[i_worst]
      col_worst = station_id_table$col_num[station_id_table$abbrev == x_worst]
      
      #predict the rainfall
      if(!is.na(p_record[i,col_best])){
        p_record$cal_interp[i] =  coefs$intercept[i_best] + p_record[i,col_best] * coefs$coeff_m[i_best]
      } else if(!is.na(p_record[i,col_2ndbest])) {
        p_record$cal_interp[i] =  coefs$intercept[i_2ndbest] + p_record[i,col_2ndbest] * coefs$coeff_m[i_2ndbest]
      } else if(!is.na(p_record[i,col_worst])){
        p_record$cal_interp[i] =  coefs$intercept[i_worst] + p_record[i,col_worst] * coefs$coeff_m[i_worst]
      }
      
    }
  }
  return(p_record)
}



# Precip ------------------------------------------------------------------

# Step 1. run setup and the NOAA data retrieval section in tabular_data_upload.R
rm(list = ls()[!(ls() %in% c("wx")) ]) #Remove all variables other than the weather dataframe
# Step 2. run the setup and subfunctions sections of this script to get model_start_date, etc.

daily_precip = make_daily_precip(wx_table = wx,
                  record_start_date = as.Date("1943-01-01"), 
                  record_end_date = as.Date("2011-09-30"))
                  # record_end_date = model_end_date)

model_coeff = make_model_coeffs_table(months = 1:12, 
                                      ys = c("fj", "cal"), 
                                      xs = c("fj", "cal", "gv", "et"))

p_record = fill_fj_cal_gaps_regression_table(model_coeff)


# _Did I replicate original 1991-2011 precip record ------------------------------


# average interp-fj and interp-cal records and compare to original
p_record$interp_cal_fj_mean =  apply(X = dplyr::select(p_record, fj_interp, cal_interp), 
                                     MARGIN = 1, FUN = mean, na.rm=T)
par(mfrow = c(3,1))
plot(p_record$PRCP_mm_orig,p_record$interp_cal_fj_mean); abline(0,1, col = "red")
cor(p_record$PRCP_mm_orig,p_record$interp_cal_fj_mean, use = "pairwise.complete.obs") #0.85
plot(p_record$PRCP_mm_cal, p_record$interp_cal_fj_mean); abline(0,1, col = "red")
plot(p_record$PRCP_mm_fj, p_record$interp_cal_fj_mean); abline(0,1, col = "red")


# That is... a reasonable reproduction of the original values. 

# plot monthly values to see if they match 
monthly_interp = aggregate(p_record$interp_cal_fj_mean, by = list(p_record$month_day1), FUN = sum)
monthly_orig = aggregate(p_record$PRCP_mm_orig, by = list(p_record$month_day1), FUN = sum)
cor(monthly_orig$x, monthly_interp$x, use = "pairwise.complete.obs") #0.87
plot(monthly_orig$x, monthly_interp$x)
abline(0,1, col = "red")

# 3 dates above 300 mm / month: 2005-02 is a serious outlier
View(monthly_interp)

# FOR NOW: this is our precip record, eh
par(mfrow = c(3,1))
plot(p_record$Date, p_record$PRCP_mm_fj, type = "l", ylim = c(0, 120))
plot(p_record$Date, p_record$PRCP_mm_cal, type = "l", ylim = c(0, 120))
plot(p_record$Date, p_record$interp_cal_fj_mean, type = "l", ylim = c(0, 120))



# _exceedance curves ------------------------------------------------------

legend_labels = c("Callahan", "Fort Jones","Greenview", 
                  "Original 91-11", "Cal-FJ Mean",
                  "Gap-filled Callahan", "Gap-filled Fort Jones",
                  "Gap-filled Cal-FJ Mean")
#initialize plot
plot(x = NA, y = NA, xlim = c(0.6, 1), ylim = c(0, 120), xlab = "Exceedance probabylity", ylab = "log daily precip, mm")

col_names = colnames(p_record)[!(colnames(p_record) %in% c("Date","month_day1", "water_year", "PRCP_mm_et"))]
for(i in 1:length(col_names)){
  col_name = col_names[i]
  record = select(p_record, col_name)
  record = record[!is.na(record)]
  record_exceedance = (record[order(record)])
  t = length(record_exceedance)
  lines(x = (1:t)/t, y = record_exceedance, type = "l", col = i, main = col_name)#,
       # xlab = "Exceedance probabylity", ylab = "log daily precip, mm")
}
legend(x = "topleft", legend = legend_labels, cex = 0.8, lwd = rep(1, length(col_names)), col = 1:length(col_names))



# _histograms -------------------------------------------------------------

legend_labels = c("Callahan", "Fort Jones","Greenview", 
                  "Original 91-11", "Cal-FJ Mean",
                  "Gap-filled Callahan", "Gap-filled Fort Jones",
                  "Gap-filled Cal-FJ Mean")
#initialize plot
# plot(x = NA, y = NA, xlim = c(0.6, 1), ylim = c(0, 120), xlab = "Exceedance probabylity", ylab = "log daily precip, mm")

#need to fix these to account for 0s
par(mfrow = c(2,4))
col_names = colnames(p_record)[!(colnames(p_record) %in% c("Date","month_day1", "water_year", "PRCP_mm_et"))]
for(i in 1:length(col_names)){
  col_name = col_names[i]
  record = select(p_record, col_name)
  record = (record[!is.na(record)])
  hist(record, main = col_name, col = i,
       freq = F)#, xlim = c(-2.5, 6), ylim = c(0, 0.4))
  abline(v = mean(record), lty = 2)
  print(mean(record))
}






# _How many days are 0 ppt? ----------------------------------------


# precip_for_tab = daily_precip[daily_precip$Date >= model_start_date & daily_precip$Date <= "2011-09-30",]
precip_for_tab = p_record; precip_for_tab$month_day1=NULL; precip_for_tab$water_year=NULL

num_cols = dim(precip_for_tab)[2]
num_nas = apply(precip_for_tab[2:num_cols], MARGIN = 2, function(x) sum(is.na(x)))
num_0s = apply(precip_for_tab[2:num_cols], MARGIN = 2, function(x) sum(x==0, na.rm=T))
num_gt0 = apply(precip_for_tab[2:num_cols], MARGIN = 2, function(x) sum(x>0, na.rm=T))

p_tab = data.frame( num_nas, num_0s, num_gt0)

total_days = as.numeric(unique(p_tab[1]+p_tab[2]+p_tab[3]))
p_tab$perc_0 = round(p_tab$num_0s / total_days, 2)*100
p_tab$perc_na = round(p_tab$num_nas / total_days, 2)*100
p_tab$perc_gt0 = round(p_tab$num_gt0 / total_days, 2)*100
p_tab$num0_div_gt0 = round(p_tab$num_0s / p_tab$num_gt0, 2) 



# Precip: how different are exceedance curves for each station? For orig precip input?
#starting point: histograms

# plot(p_record$Date, p_record$PRCP_mm_orig, ylim = c(0, 120))
# plot(p_record$Date, p_record$PRCP_mm_fj, ylim = c(0, 120))
# plot(p_record$Date, p_record$PRCP_mm_cal, ylim = c(0, 120))
par(mfrow = c(2,2))

# need to fix these to account for 0 values becoming -Inf. proportion of 0 values. 
hist(log10(p_record$PRCP_mm_fj), xlab = "log10 of FJ daily precip", main = NA, col = "wheat", xlim = c(-1.5,2), ylim = c(0,0.6), freq = F)
hist(log10(p_record$PRCP_mm_cal), xlab = "log10 of Cal daily precip", main = NA, col = "wheat", xlim = c(-1.5,2), ylim = c(0,0.6),freq = F)
hist(log10(p_record$PRCP_mm_orig), xlab = "log10 of Original Input daily precip", main = NA, col = "wheat", xlim = c(-1.5,2), ylim = c(0,0.6),freq = F)
hist(log10(p_record$interp_cal_fj_mean), xlab = "log10 of Interp FJ-Cal daily precip", main = NA, col = "wheat", xlim = c(-1.5,2), ylim = c(0,0.6),freq = F)



# _stitch orig and update together ----------------------------------------



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


# Visual comparison (over time)
par(mfrow = c(2,1))
plot(daily_precip$Date, daily_precip$PRCP_mm_orig-daily_precip$PRCP_mm_cal, type = "l", main = "Original minus Cal", xlab = "Date", ylab = "mm precip")
plot(daily_precip$Date, daily_precip$PRCP_mm_orig-daily_precip$PRCP_mm_fj, type = "l", main = "Original minus FJ", xlab = "Date", ylab = "mm precip")
plot(daily_precip$Date, daily_precip$PRCP_mm_orig-daily_precip$mean_PRCP, type = "l", main = "Original minus Cal, FJ mean", xlab = "Date", ylab = "mm precip")


# Visual comparison (1:1 line)
par(mfrow = c(2,2))
plot(daily_precip$PRCP_mm_orig, daily_precip$PRCP_mm_cal, pch = 18, main = "Original vs Cal", xlab = "orig", ylab = "mm precip")
abline(0,1, col = "red", lwd = 2)
plot(daily_precip$PRCP_mm_orig, daily_precip$PRCP_mm_fj, pch = 18, main = "Original vs FJ", xlab = "orig", ylab = "mm precip")
abline(0,1, col = "red", lwd = 2)
plot(daily_precip$PRCP_mm_orig, daily_precip$mean_PRCP, pch = 18, main = "Original vs Cal, FJ mean", xlab = "orig", ylab = "mm precip")
abline(0,1, col = "red", lwd = 2)
plot(daily_precip$PRCP_mm_cal, daily_precip$PRCP_mm_fj, pch = 18, main = "Cal vs FJ", xlab = "Cal", ylab = "FJ")
abline(0,1, col = "red", lwd = 2)


plot(daily_precip$Date, daily_precip$PRCP_mm_fj, type = "l", main = "Fort Jones station", xlab = "Date", ylab = "mm precip")

plot(daily_precip$Date, daily_precip$PRCP_mm_fj - daily_precip$PRCP_mm_cal, type = "l", 
     main = "Fort Jones minus Callahan", xlab = "Date", ylab = "mm precip")

#Histogram of differences between Callahan and FJ
hist(daily_precip$PRCP_mm_fj - daily_precip$PRCP_mm_cal, breaks = seq(-80,90,5))
nonzero_diffs = daily_precip$PRCP_mm_fj - daily_precip$PRCP_mm_cal; nonzero_diffs = nonzero_diffs[abs(nonzero_diffs)>5]
hist(nonzero_diffs,breaks = seq(-80,90,5), main = "Num. days with Cal-FJ difference of >5 mm", xlab = "mm difference, Cal-FJ")


#combine and write as text file
daily_precip_updated = rbind(daily_precip_orig, daily_precip_update)
write.table(daily_precip_updated, file = file.path(SWBM_file_dir, "precip.txt"),
            sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)


# ET ----------------------------------------------------------------------

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



# SWBM overview plot ------------------------------------------------------

# first: run the subfunctions in eci273_scenario_plotting.

#Read in SWBM scenario outputs: monthly water budget 
setwd("C:/Users/Claire/Documents/GitHub/SVIHM/SWBM/up2018")
monthly_water_budget_hist = read.table("monthly_water_budget.dat", header = TRUE)
mwb_hist = monthly_water_budget_hist

plot_water_budget_overview(mwb_hist, "Historical")




# scratchwork -------------------------------------------------------------


# _make monthly_precip 

#Test 1: Don't remove NA
monthly_precip1 = aggregate(daily_precip$PRCP_mm_orig, by = list(daily_precip$month_day1), FUN=sum)
fj_m = aggregate(daily_precip$PRCP_mm_fj, by = list(daily_precip$month_day1), FUN=sum)
cal_m = aggregate(daily_precip$PRCP_mm_cal, by = list(daily_precip$month_day1), FUN=sum)
monthly_precip1=merge(monthly_precip1, fj_m, by.x = "Group.1", by.y="Group.1")
monthly_precip1=merge(monthly_precip1, cal_m, by.x = "Group.1", by.y="Group.1")
colnames(monthly_precip1) = c("Month", "PRCP_mm_orig", "PRCP_mm_fj", "PRCP_mm_cal")
monthly_precip1$mean_PRCP = apply(X = monthly_precip1[,2:3], MARGIN = 1, FUN = mean, na.rm=T)

#Test 2: NA = 0
daily_precip2 = daily_precip
daily_precip2$PRCP_mm_cal[is.na(daily_precip2$PRCP_mm_cal)] = 0
daily_precip2$PRCP_mm_fj[is.na(daily_precip2$PRCP_mm_fj)] = 0

monthly_precip2 = aggregate(daily_precip2$PRCP_mm_orig, by = list(daily_precip2$month_day1), FUN=sum)
fj_m = aggregate(daily_precip2$PRCP_mm_fj, by = list(daily_precip2$month_day1), FUN=sum)
cal_m = aggregate(daily_precip2$PRCP_mm_cal, by = list(daily_precip2$month_day1), FUN=sum)
monthly_precip2=merge(monthly_precip2, fj_m, by.x = "Group.1", by.y="Group.1")
monthly_precip2=merge(monthly_precip2, cal_m, by.x = "Group.1", by.y="Group.1")
colnames(monthly_precip2) = c("Month", "PRCP_mm_orig", "PRCP_mm_fj", "PRCP_mm_cal")
monthly_precip2$mean_PRCP = apply(X = monthly_precip2[,2:3], MARGIN = 1, FUN = mean, na.rm=T)

#Test 3: Remove NAs
monthly_precip3 = aggregate(daily_precip$PRCP_mm_orig, by = list(daily_precip$month_day1), FUN=sum, na.rm=T)
fj_m = aggregate(daily_precip$PRCP_mm_fj, by = list(daily_precip$month_day1), FUN=sum, na.rm=T)
cal_m = aggregate(daily_precip$PRCP_mm_cal, by = list(daily_precip$month_day1), FUN=sum, na.rm=T)
monthly_precip3=merge(monthly_precip3, fj_m, by.x = "Group.1", by.y="Group.1")
monthly_precip3=merge(monthly_precip3, cal_m, by.x = "Group.1", by.y="Group.1")
colnames(monthly_precip3) = c("Month", "PRCP_mm_orig", "PRCP_mm_fj", "PRCP_mm_cal")
monthly_precip3$mean_PRCP = apply(X = monthly_precip3[,2:3], MARGIN = 1, FUN = mean, na.rm=T)


# _make yearly_precip 


#Calculate yearly average
yearly_precip = aggregate(daily_precip$PRCP_mm_orig, by = list(daily_precip$water_year), FUN = sum)


# _visual comparison 



# # Visual comparison - over time
# par(mfrow = c(2,2))
# plot(daily_precip$Date, daily_precip$PRCP_mm_cal, type = "l", main = "Callahan station", xlab = "Date", ylab = "mm precip")
# plot(daily_precip$Date, daily_precip$PRCP_mm_fj, type = "l", main = "Fort Jones station", xlab = "Date", ylab = "mm precip")
# plot(daily_precip$Date, daily_precip$mean_PRCP, type = "l", main = "Callahan, FJ average", xlab = "Date", ylab = "mm precip")
# plot(daily_precip$Date, daily_precip$PRCP_mm_fj - daily_precip$PRCP_mm_cal, type = "l", 
#      main = "Fort Jones minus Callahan", xlab = "Date", ylab = "mm precip")

# #Histogram of differences between Callahan and FJ
# hist(daily_precip$PRCP_mm_fj - daily_precip$PRCP_mm_cal, breaks = seq(-80,90,5))
# nonzero_diffs = daily_precip$PRCP_mm_fj - daily_precip$PRCP_mm_cal; nonzero_diffs = nonzero_diffs[abs(nonzero_diffs)>5]
# hist(nonzero_diffs,breaks = seq(-80,90,5), main = "Num. days with Cal-FJ difference of >5 mm", xlab = "mm difference, Cal-FJ")

# Visual comparison (1:1 line): daily precip values
par(mfrow = c(2,2))
plot(daily_precip$PRCP_mm_orig, daily_precip$PRCP_mm_cal, pch = 18, main = "Original vs Cal", xlab = "orig", ylab = "daily mm precip")
cor(daily_precip$PRCP_mm_orig, daily_precip$PRCP_mm_cal, use="pairwise.complete.obs") # 0.21
abline(0,1, col = "red", lwd = 2)
plot(daily_precip$PRCP_mm_orig, daily_precip$PRCP_mm_fj, pch = 18, main = "Original vs FJ", xlab = "orig", ylab = "mm precip")
cor(daily_precip$PRCP_mm_orig, daily_precip$PRCP_mm_fj, use="pairwise.complete.obs") # 0.19
abline(0,1, col = "red", lwd = 2)
plot(daily_precip$PRCP_mm_orig, daily_precip$mean_PRCP, pch = 18, main = "Original vs Cal, FJ mean", xlab = "orig", ylab = "mm precip")
cor(daily_precip$PRCP_mm_orig, daily_precip$mean_PRCP, use="pairwise.complete.obs") # 0.22
abline(0,1, col = "red", lwd = 2)
plot(daily_precip$PRCP_mm_cal, daily_precip$PRCP_mm_fj, pch = 18, main = "Cal vs FJ", xlab = "Cal", ylab = "FJ")
cor(daily_precip$PRCP_mm_cal, daily_precip$PRCP_mm_fj, use="pairwise.complete.obs") # 0.67
abline(0,1, col = "red", lwd = 2)

# Shitty match-up in daily values for orig v FJ AND orig v Cal AND orig v FJ-Cal mean. (cor of 0.19-0.22) 
# Much better, but still pretty bad, match-up in daily values for Cal vs FJ. (cor of 0.67)


# Visual comparison (1:1 line): monthly precip values, Test 1, don't remove NAs
par(mfrow = c(2,2))
plot(monthly_precip1$PRCP_mm_orig, monthly_precip1$PRCP_mm_cal, pch = 18, main = "Orig vs Cal, keep NAs", xlab = "orig", ylab = "mm precip")
cor(monthly_precip1$PRCP_mm_orig, monthly_precip1$PRCP_mm_cal, use="pairwise.complete.obs") #0.81
abline(0,1, col = "red", lwd = 2)
plot(monthly_precip1$PRCP_mm_orig, monthly_precip1$PRCP_mm_fj, pch = 18, main = "Original vs FJ", xlab = "orig", ylab = "mm precip")
cor(monthly_precip1$PRCP_mm_orig, monthly_precip1$PRCP_mm_fj, use="pairwise.complete.obs") #0.68
abline(0,1, col = "red", lwd = 2)
plot(monthly_precip1$PRCP_mm_orig, monthly_precip1$mean_PRCP, pch = 18, main = "Original vs Cal, FJ mean", xlab = "orig", ylab = "mm precip")
cor(monthly_precip1$PRCP_mm_orig, monthly_precip1$mean_PRCP, use="pairwise.complete.obs") #0.998
abline(0,1, col = "red", lwd = 2)
plot(monthly_precip1$PRCP_mm_cal, monthly_precip1$PRCP_mm_fj, pch = 18, main = "Cal vs FJ", xlab = "Cal", ylab = "FJ")
cor(monthly_precip1$PRCP_mm_cal, monthly_precip1$PRCP_mm_fj, use="pairwise.complete.obs") #0.87
abline(0,1, col = "red", lwd = 2)


# Visual comparison (1:1 line): monthly precip values, Test 2, NAs = 0
par(mfrow = c(2,2))
plot(monthly_precip2$PRCP_mm_orig, monthly_precip2$PRCP_mm_cal, pch = 18, main = "Orig vs Cal, NAs = 0", xlab = "orig", ylab = "mm precip")
cor(monthly_precip2$PRCP_mm_orig, monthly_precip2$PRCP_mm_cal, use="pairwise.complete.obs") #0.83
abline(0,1, col = "red", lwd = 2)
plot(monthly_precip2$PRCP_mm_orig, monthly_precip2$PRCP_mm_fj, pch = 18, main = "Original vs FJ", xlab = "orig", ylab = "mm precip")
cor(monthly_precip2$PRCP_mm_orig, monthly_precip2$PRCP_mm_fj, use="pairwise.complete.obs") #0.73
abline(0,1, col = "red", lwd = 2)
plot(monthly_precip2$PRCP_mm_orig, monthly_precip2$mean_PRCP, pch = 18, main = "Original vs Cal, FJ mean", xlab = "orig", ylab = "mm precip")
cor(monthly_precip2$PRCP_mm_orig, monthly_precip2$mean_PRCP, use="pairwise.complete.obs") #0.95
abline(0,1, col = "red", lwd = 2)
plot(monthly_precip2$PRCP_mm_cal, monthly_precip2$PRCP_mm_fj, pch = 18, main = "Cal vs FJ", xlab = "Cal", ylab = "FJ")
cor(monthly_precip2$PRCP_mm_cal, monthly_precip2$PRCP_mm_fj, use="pairwise.complete.obs") #0.79
abline(0,1, col = "red", lwd = 2)

# Visual comparison (1:1 line): monthly precip values, Test 3, remove NAs
par(mfrow = c(2,2))
plot(monthly_precip3$PRCP_mm_orig, monthly_precip3$PRCP_mm_cal, pch = 18, main = "Orig vs Cal, remove NAs", xlab = "orig", ylab = "mm precip")
cor(monthly_precip3$PRCP_mm_orig, monthly_precip3$PRCP_mm_cal, use="pairwise.complete.obs") #0.70
abline(0,1, col = "red", lwd = 2)
plot(monthly_precip3$PRCP_mm_orig, monthly_precip3$PRCP_mm_fj, pch = 18, main = "Original vs FJ", xlab = "orig", ylab = "mm precip")
cor(monthly_precip3$PRCP_mm_orig, monthly_precip3$PRCP_mm_fj, use="pairwise.complete.obs") #0.66
abline(0,1, col = "red", lwd = 2)
plot(monthly_precip3$PRCP_mm_orig, monthly_precip3$mean_PRCP, pch = 18, main = "Original vs Cal, FJ mean", xlab = "orig", ylab = "mm precip")
cor(monthly_precip3$PRCP_mm_orig, monthly_precip3$mean_PRCP, use="pairwise.complete.obs") #0.94
abline(0,1, col = "red", lwd = 2)
plot(monthly_precip3$PRCP_mm_cal, monthly_precip3$PRCP_mm_fj, pch = 18, main = "Cal vs FJ", xlab = "Cal", ylab = "FJ")
cor(monthly_precip3$PRCP_mm_cal, monthly_precip3$PRCP_mm_fj, use="pairwise.complete.obs") #0.79
abline(0,1, col = "red", lwd = 2)



#Visual comparison - over time
par(mfrow = c(2,2))
plot(monthly_precip$Month, monthly_precip$PRCP_mm_orig, type = "l", main = "Original", xlab = "Date", ylab = "mm precip")
plot(monthly_precip$Month, monthly_precip$PRCP_mm_fj, type = "l", main = "FJ monthly", xlab = "Month", ylab = "mm precip")
plot(monthly_precip$Month, monthly_precip$PRCP_mm_cal, type = "l", main = "Cal", xlab = "Month", ylab = "mm precip")
plot(monthly_precip$Month, monthly_precip$mean_PRCP, type = "l", main = "Cal, FJ mean", xlab = "Month", ylab = "mm precip")


# plot(monthly_precip$Month, monthly_precip$PRCP_mm_orig-monthly_precip$PRCP_mm_cal, type = "l", main = "Original minus Cal", xlab = "Month", ylab = "mm precip")
# plot(monthly_precip$Month, monthly_precip$PRCP_mm_orig-monthly_precip$PRCP_mm_fj, type = "l", main = "Original minus FJ", xlab = "Month", ylab = "mm precip")
plot(monthly_precip$Month, monthly_precip$PRCP_mm_orig-monthly_precip$mean_PRCP, type = "l", main = "Original minus Cal, FJ mean", xlab = "Month", ylab = "mm precip")


#By forgetting to remove NA values I think I must have stumbled on what they
#used to make the precip data.
#
