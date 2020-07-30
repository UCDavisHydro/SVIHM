# Precip and ET input analysis

#Rules:
## dates in R are treated as YYYY-MM-DD for convenience. Dates in the model input files are DD-MM-YYYY.

# SETUP -------------------------------------------------------------------

library(stringr)
library(lubridate)
library(dplyr)
library(Matrix)
library(rpostgis)
library(postGIStools)
library(dbplyr)
library(raster)
library(ggplot2)
library(reshape2)


# 1) Set drives for collecting all SWBM input files and SVIHM modflow files

# Set project directory. 
# Option 1: this script is being called by Update_SVIHM_Inputs. 
#   Declare directories and model dates, and connect to db, in outer script. 

#Option 2: this script is the active document in RStudio:
# dev_mode = TRUE #defaults to false, when called from update_SVIHM_Inputs

if(dev_mode){ 
  library(rstudioapi)
  svihm_dir <- dirname(dirname(getActiveDocumentContext()$path))
  ## Data used in update
  Stream_Regression_dir = file.path(svihm_dir, "Streamflow_Regression_Model")
  time_indep_dir = file.path(svihm_dir, "SVIHM_Input_Files", "time_independent_input_files")
  ref_data_dir = file.path(svihm_dir, "SVIHM_Input_Files", "reference_data")
  ## Directory used to archive the input files for each scenario
  model_inputs_dir = file.path(svihm_dir, "SVIHM_Input_Files","Historical_WY1991_2018")
  scenario_dev_dir = file.path(svihm_dir, "SVIHM_Input_Files", "Scenario_Development")
  ## Directories for running the scenarios (files copied at end of script)
  SWBM_file_dir = file.path(svihm_dir, "SWBM", "up2018")
  MF_file_dir = file.path(svihm_dir, "MODFLOW","up2018")
  # Directory for connecting to the database
  dms_dir = file.path(dirname(svihm_dir), "SiskiyouGSP2022", "Data_Management_System")
  #Connect to Siskiyou DB
  source(file.path(dms_dir, "connect_to_db.R"))
  
  #load weather station for rainfall
  # noaa = data.frame(tbl(siskiyou_tables, "noaa_daily_data"))
  
  

# SET MODEL RUN DATES -----------------------------------------------------
  
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
  
}

if(exists("calling_from_gsp_repo")){
  svihm_dir = file.path(dirname(gsp_dir), "SVIHM")
  ref_data_dir = file.path(svihm_dir, "SVIHM_Input_Files", "reference_data")
}

# if(isRStudio == FALSE){ library(here); svihm_dir <- dirname(here::here("Update_SVIHM_Inputs.R"))}
# here() doesn't really work on this computer.





# Subfunctions ------------------------------------------------------------

make_station_table = function(weather_table){
  #station abbreviation, number in the NOAA dataset, and column number in the daily_precip table
  # station_table = data.frame(abbrev = c("cal", "fj", "et", "gv"),
  #                               station = c("USC00041316", "USC00043182", "USC00042899", "USC00043614"),
  #                               col_num = c(1,2,3,4) + 1)
  station_table = data.frame(abbrev = c("cal", "fj", "et", "gv","yr", "y2"),
                             station = c("USC00041316", "USC00043182", "USC00042899", "USC00043614","USC00049866","US1CASK0005"),
                             col_num = c(1,2,3,4,5,6) + 1,
                             lat = NA, lon = NA)
  
  #Assign coordinates to Station Locations from NOAA data
  for(i in 1:dim(station_table)[1]){
    station = station_table$station[i]
    station_table$lat[i] = unique(weather_table$LATITUDE[weather_table$STATION==station])
    station_table$lon[i] = unique(weather_table$LONGITUDE[weather_table$STATION==station])
  }
  #Assign a color to each station, for later visual rep of which station is used for which precip gap filling
  station_table$color = topo.colors(n = dim(station_table)[1])
  #Make station_table a SpatialPointsDataFrame and add proj
  coordinates(station_table) = ~lon + lat
  proj4string(station_table)=crs("+init=epsg:4326")
  
  #Use coordinates to calculate distance between all the stations
  station_dist = pointDistance(station_table, lonlat=T,  allpairs = T)
  #Make the matrix easier to query later
  station_dist = as.matrix(forceSymmetric(station_dist, uplo = "L"))
  diag(station_dist) = NA
  station_dist = as.data.frame(station_dist)
  rownames(station_dist) = station_table$abbrev; colnames(station_dist) = station_table$abbrev
  
  
  return(list(station_table, station_dist))
}


make_daily_precip = function(weather_table = noaa,
                             daily_precip_start_date = as.Date("1943-01-01"), 
                             daily_precip_end_date = model_end_date){
  
  record_days = seq(from = daily_precip_start_date, to = daily_precip_end_date, by = "days")
  
  #Subset data into stations
  cal = subset(weather_table, STATION=="USC00041316" & DATE >= daily_precip_start_date & DATE <= daily_precip_end_date)
  cal = data.frame(DATE = cal$DATE, PRCP = cal$PRCP)
  fj = subset(weather_table, STATION=="USC00043182" & DATE >= daily_precip_start_date & DATE <= daily_precip_end_date)
  fj = data.frame(DATE = fj$DATE, PRCP = fj$PRCP)
  et = subset(weather_table, STATION == "USC00042899" & DATE >= daily_precip_start_date & DATE <= daily_precip_end_date)
  et =data.frame(DATE = et$DATE, PRCP = et$PRCP)
  gv = subset(weather_table, STATION == "USC00043614" & DATE >= daily_precip_start_date & DATE <= daily_precip_end_date)
  gv =data.frame(DATE = gv$DATE, PRCP = gv$PRCP)
  yr = subset(weather_table, STATION == "USC00049866" & DATE >= daily_precip_start_date & DATE <= daily_precip_end_date)
  yr =data.frame(DATE = yr$DATE, PRCP = yr$PRCP) 
  y2 = subset(weather_table, STATION == "US1CASK0005" & DATE >= daily_precip_start_date & DATE <= daily_precip_end_date)
  y2 =data.frame(DATE = y2$DATE, PRCP = y2$PRCP) 
  
  
  #read in original data (wys 1991-2011)
  # print(ref_data_dir)
  daily_precip_orig = read.table(file.path(ref_data_dir,"precip_1991_2011.txt"))
  colnames(daily_precip_orig) = c("PRCP", "Date")
  daily_precip_orig$Date = as.Date(daily_precip_orig$Date, format = "%d/%m/%Y")
  daily_precip_orig$PRCP = daily_precip_orig$PRCP*1000
  
  
  ### COMPARISON TABLE FOR 2 STATIONS AND ORIG DATA
  daily_precip = data.frame(record_days)
  daily_precip = merge(x = daily_precip, y = cal, by.x = "record_days", by.y = "DATE", all=TRUE)
  daily_precip = merge(x = daily_precip, y = fj, by.x = "record_days", by.y = "DATE", all=TRUE)
  daily_precip = merge(x = daily_precip, y = et, by.x = "record_days", by.y = "DATE", all=TRUE)
  daily_precip = merge(x = daily_precip, y = gv, by.x = "record_days", by.y = "DATE", all=TRUE)
  daily_precip = merge(x = daily_precip, y = yr, by.x = "record_days", by.y = "DATE", all=TRUE)
  daily_precip = merge(x = daily_precip, y = y2, by.x = "record_days", by.y = "DATE", all=TRUE)
  daily_precip = merge(x = daily_precip, y = daily_precip_orig, by.x = "record_days", by.y = "Date", all=TRUE)
  colnames(daily_precip)=c("Date","PRCP_mm_cal", "PRCP_mm_fj","PRCP_mm_et","PRCP_mm_gv",
                           "PRCP_mm_yr", "PRCP_mm_y2","PRCP_mm_orig")
  
  # Compare original data to FJ-Cal mean
  daily_precip$mean_PRCP_fjcal = apply(X = daily_precip[,2:3], MARGIN = 1, FUN = mean, na.rm=T)
  
  #Add aggregation columns
  daily_precip$month_day1 = floor_date(daily_precip$Date, "month")
  daily_precip$water_year = year(daily_precip$Date)
  daily_precip$water_year[month(daily_precip$Date) > 9] = daily_precip$water_year[month(daily_precip$Date) > 9]+1
  
  return(daily_precip)
}

make_model_coeffs_table = function(months = 1:12, 
                                   station_table = station_table,
                                   daily_precip = daily_precip,
                                   ys = c("fj", "cal"), 
                                   xs = c("fj", "cal", "gv", "et", "yr", "y2")){  
  
  model_coeff = expand.grid(months = months, Y_var = ys, X_var = xs)
  #take out matching combinations (e.g. x = fj, y = fj)
  model_coeff = model_coeff[as.character(model_coeff$Y_var) != as.character(model_coeff$X_var),]
  #Add model parameter columns
  model_coeff$intercept=NA; model_coeff$coeff_m=NA; model_coeff$r2=NA
  
  for(mnth in months){
    for(y in ys){
      for(x in xs){
        if(y == x){next} #no need for autoregression
        #Find the output table row
        model_coeff_index = which(model_coeff$months==mnth & model_coeff$X_var==x & model_coeff$Y_var==y)
        #Find the daily precip column number for X and Y stations
        col_num_x = station_table$col_num[station_table$abbrev == x]
        col_num_y = station_table$col_num[station_table$abbrev == y]
        #Declare X and Y in daily precip
        X = daily_precip[month(daily_precip$Date) == mnth, col_num_x]
        Y = daily_precip[month(daily_precip$Date) == mnth, col_num_y]
        # 
        model_name = paste(y, "on", x, "in", month.abb[mnth] )
        
        #check to see if there are 0 matching pairs
        if(sum(!is.na(X) & !is.na(Y))==0){next}
        
        model = lm(Y~ 0 + X)
        # CASE DESCRIPTION?
        if(length(coef(model)) ==2){print(paste("2 coeff of this model", mnth,X,Y))
          model_coeff[model_coeff_index, 4:5] = coef(model)}
        if(length(coef(model)) ==1){
          model_coeff[model_coeff_index, 4] = 0; model_coeff[model_coeff_index, 5] = coef(model)}
        model_coeff$r2[model_coeff_index] = summary(model)$r.sq
      }
    }
  }
  return(model_coeff)
}

fill_fj_cal_gv_gaps_regression_table = function(model_coeff, 
                                             station_table = station_table,
                                             daily_precip = daily_precip,
                                             start_date = model_start_date,
                                             end_date = model_end_date){
  #Function: use best available regression to fill in gaps in precip records for FJ and Cal
  
  #Rename daily precip table and restrict dates
  p_record = daily_precip[daily_precip$Date >= start_date & daily_precip$Date <= end_date,]
  
  ### Fill in gaps in FJ
  # Initialize the interpolated record as the official FJ record (with gaps)
  p_record$fj_interp = p_record$PRCP_mm_fj
  # Assign colors for the station attribution plot, indicating that we got these values from the FJ station.
  p_record$fj_interp_color[!is.na(p_record$fj_interp)] = station_table$color[station_table$abbrev == "fj"]
  
  # For each NA daily precip value in the FJ record, predict the value in the FJ record using the regression 
  # coefficients from the station with the best R^2 value for predicting FJ. 
  # (If that station also has a gap on that day, use the one with the next-best R^2 value, until the gap is filled.)
  for(i in 1:length(p_record$fj_interp)){
    if(is.na(p_record$fj_interp[i])){
      #find the appropriate regression coefficients for this month of fj data
      coefs = model_coeff[model_coeff$Y_var == "fj" & model_coeff$months == month(p_record$Date[i]), ]
      index_of_ranks = rev(order(coefs$r2)) # test the regressions in order of best R2 to worst
      
      #Predict rainfall
      for(j in 1:length(index_of_ranks)){
        if(is.na(p_record$fj_interp[i])){ #If this daily value didn't get filled by a previous calc in this for loop
          # Use coef matrix to assign X variable station for the regression, based on ranked R^2
          coef_index = index_of_ranks[j] 
          coef_x = coefs$X_var[coef_index] 
          # Assign the column number and indicator color of the relevant predictor station
          daily_precip_column_num = station_table$col_num[station_table$abbrev == coef_x]
          daily_precip_x_var_color = station_table$color[station_table$abbrev == coef_x]
          # If there's data for that X variable, use it to predict rainfall at FJ for that day. also assign color.
          if(!is.na(p_record[i,daily_precip_column_num])){
            p_record$fj_interp[i] =  coefs$intercept[coef_index] + (p_record[i, daily_precip_column_num] * coefs$coeff_m[coef_index])
            p_record$fj_interp_color[i] = daily_precip_x_var_color
          }
          #If there's no data for the best-ranked variable, it will go back to the beginning of the for loop and try again with other stations
        }
      }

    }
  }
  
  ### Fill in gaps in Cal
  # Initialize the interpolated record as the official Cal record (with gaps)
  p_record$cal_interp = p_record$PRCP_mm_cal
  # Assign colors for the station attribution plot, indicating that we got these values from the Cal station.
  p_record$cal_interp_color[!is.na(p_record$cal_interp)] = station_table$color[station_table$abbrev == "cal"]
  
  for(i in 1:length(p_record$cal_interp)){
    if(is.na(p_record$cal_interp[i])){
      #find the appropriate regression coefficients for this month of Cal data
      coefs = model_coeff[model_coeff$Y_var == "cal" & model_coeff$months == month(p_record$Date[i]), ]
      index_of_ranks = rev(order(coefs$r2)) # test the regressions in order of best R2 to worst
      
      #Predict rainfall
      for(j in 1:length(index_of_ranks)){
        if(is.na(p_record$cal_interp[i])){ #If this daily value didn't get filled by a previous calc in this for loop
          # Use coef matrix to assign X variable station for the regression, based on ranked R^2
          coef_index = index_of_ranks[j] 
          coef_x = coefs$X_var[coef_index] 
          daily_precip_column_num = station_table$col_num[station_table$abbrev == coef_x]
          daily_precip_x_var_color = station_table$color[station_table$abbrev == coef_x]
          # If there's data for that X variable, use it to predict rainfall at cal for that day
          if(!is.na(p_record[i,daily_precip_column_num])){
            p_record$cal_interp[i] =  coefs$intercept[coef_index] + (p_record[i, daily_precip_column_num] * coefs$coeff_m[coef_index])
            p_record$cal_interp_color[i] =daily_precip_x_var_color
          }
          #If there's no data for the best-ranked variable, it will go back to the beginning of the for loop and try again with other stations
        }
      }
      
    }
  }

  ### Fill in gaps in Greenview
  # Initialize the interpolated record as the official Greenview record (with gaps)
  p_record$gv_interp = p_record$PRCP_mm_gv
  # Assign colors for the station attribution plot, indicating that we got these values from the Greenview station.
  p_record$gv_interp_color[!is.na(p_record$gv_interp)] = station_table$color[station_table$abbrev == "gv"]
  
  for(i in 1:length(p_record$gv_interp)){
    if(is.na(p_record$gv_interp[i])){
      #find the appropriate regression coefficients for this month of Greenview data
      coefs = model_coeff[model_coeff$Y_var == "gv" & model_coeff$months == month(p_record$Date[i]), ]
      index_of_ranks = rev(order(coefs$r2)) # test the regressions in order of best R2 to worst
      
      #Predict rainfall
      for(j in 1:length(index_of_ranks)){
        if(is.na(p_record$gv_interp[i])){ #If this daily value didn't get filled by a previous calc in this for loop
          # Use coef matrix to assign X variable station for the regression, based on ranked R^2
          coef_index = index_of_ranks[j] 
          coef_x = coefs$X_var[coef_index] 
          daily_precip_column_num = station_table$col_num[station_table$abbrev == coef_x]
          daily_precip_x_var_color = station_table$color[station_table$abbrev == coef_x]
          # If there's data for that X variable, use it to predict rainfall at gv for that day
          if(!is.na(p_record[i,daily_precip_column_num])){
            p_record$gv_interp[i] =  coefs$intercept[coef_index] + (p_record[i, daily_precip_column_num] * coefs$coeff_m[coef_index])
            p_record$gv_interp_color[i] =daily_precip_x_var_color
          }
          #If there's no data for the best-ranked variable, it will go back to the beginning of the for loop and try again with other stations
        }
      }
      
    }
  }  
  
  return(p_record)
}

fill_fj_cal_gaps_distance = function(station_dist = station_dist,  
                                             daily_precip = daily_precip,
                                             start_date = model_start_date,
                                             end_date = model_end_date){
  
  # use regressions to generate filled-in precip records for FJ and Cal
  p_record = daily_precip[daily_precip$Date >= start_date & daily_precip$Date <= end_date,]
  
  #Fill in gaps in FJ
  p_record$fj_interp = p_record$PRCP_mm_fj
  p_record$cal_interp = p_record$PRCP_mm_cal
  colnums = range(station_table$col_num)

  #Fill in gaps in FJ
  for(i in 1:length(p_record$fj_interp)){
    if(is.na(p_record$fj_interp[i])){
      
      dist = station_dist$fj #reset each time
      available = !(is.na(p_record[i, colnums[1]:colnums[2]]))
      dist[!available]=NA

      closest_index = which.min(dist)
      closest_colnum = station_table$col_num[closest_index]
      
      p_record$fj_interp[i] = p_record[i, closest_colnum]
      }
    }
      
  #Fill in gaps in Cal
  for(i in 1:length(p_record$cal_interp)){
    if(is.na(p_record$cal_interp[i])){
      
      dist = station_dist$cal #reset each time
      available = !(is.na(p_record[i, colnums[1]:colnums[2]]))
      dist[!available]=NA
      
      closest_index = which.min(dist)
      closest_colnum = station_table$col_num[closest_index]
      
      p_record$cal_interp[i] = p_record[i, closest_colnum]
    }
  }

  return(p_record)
}




# Precip ------------------------------------------------------------------


get_daily_precip_table=function(final_table_start_date=model_start_date, 
                                final_table_end_date = model_end_date){
  # Step 1. run setup and the NOAA data retrieval section in tabular_data_upload.R
  noaa = data.frame(tbl(siskiyou_tables, "noaa_daily_data"))
  # Step 2. run the setup and subfunctions sections of this script to get model_start_date, etc.
  
  station_info = make_station_table(weather_table = noaa)
  station_table = station_info[[1]]
  station_dist = station_info[[2]]
  
  #Generate daily precip table (same daterange as in original methodology) to make model coefficients
  #Each station is a column (plus a couple extra columns); each row is a date, WY 1944-2019
  daily_precip_regression = make_daily_precip(weather_table = noaa,
                                                                daily_precip_start_date = as.Date("1943-10-01"), 
                                                                daily_precip_end_date = as.Date("2018-09-30"))
  model_coeff = make_model_coeffs_table(months = 1:12, 
                                        station_table = station_table,
                                        daily_precip = daily_precip_regression,
                                        ys = c("fj", "cal", "gv"), 
                                        xs = c("fj", "cal", "gv", "et", "yr", "y2"))
  #Generate daily precip table to make the gap-filled records
  daily_precip_p_record = make_daily_precip(weather_table = noaa,
                                                              daily_precip_start_date = final_table_start_date, 
                                                              daily_precip_end_date = final_table_end_date)
  p_record = fill_fj_cal_gv_gaps_regression_table(model_coeff=model_coeff, 
                                               station_table = station_table,
                                               daily_precip = daily_precip_p_record,
                                               start_date = final_table_start_date, 
                                               end_date = final_table_end_date)
  
  # average interp-fj and interp-cal records and compare to original
  # p_record$interp_cal_fj_gv_mean =  apply(X = dplyr::select(p_record, fj_interp, cal_interp, gv_interp), 
  #                                              MARGIN = 1, FUN = mean, na.rm=T)
  
  p_record$interp_cal_fj_gv_mean =  apply(X = dplyr::select(p_record, PRCP_mm_fj, PRCP_mm_cal, PRCP_mm_gv), 
                                          MARGIN = 1, FUN = mean, na.rm=T)
  p_record$interp_cal_fj_mean =  apply(X = dplyr::select(p_record, PRCP_mm_fj, PRCP_mm_cal), 
                                          MARGIN = 1, FUN = mean, na.rm=T)
  p_record$interp_cal_gv_mean =  apply(X = dplyr::select(p_record, PRCP_mm_gv, PRCP_mm_cal), 
                                       MARGIN = 1, FUN = mean, na.rm=T)
  
  # Prepare to combine original precip and new regressed gap-filled fj-cal average
  p_record$stitched = p_record$PRCP_mm_orig
  p_record$stitched[is.na(p_record$PRCP_mm_orig)] = p_record$interp_cal_fj_mean[is.na(p_record$PRCP_mm_orig)]
  
  #Note: this include filling the leap days missing in the original record :)

  # orig_record_end_date = as.Date("2011-09-30"); orig_record_start_date = as.Date("1990-10-01")
  # orig_record = p_record$Date <= orig_record_end_date & p_record$Date >= orig_record_start_date
  # updated_record = p_record$Date > orig_record_end_date
  # 
  # #Fill in 5 leap days in the original record using the gap-filled cal-FJ record
  # leap_day_finder_pre2011 = orig_record & is.na(p_record$PRCP_mm_orig)
  # p_record$PRCP_mm_orig[leap_day_finder_pre2011] = p_record$interp_cal_fj_mean[leap_day_finder_pre2011]
  # 
  # #Subset original record
  # p_record$stitched = NA
  # p_record$stitched[orig_record] = p_record$PRCP_mm_orig[orig_record]
  # p_record$stitched[updated_record] = p_record$interp_cal_fj_mean[updated_record]
  
  return(p_record)
}

#Test run for AGU figures, 2019:
# p_record_all_fj = get_daily_precip_table(final_table_start_date = as.Date("1935-10-01"), 
#                               final_table_end_date = as.Date("2018-09-30"))
# wy_annual_totals = aggregate(p_record_all_fj$fj_interp, by=list(p_record_all_fj$water_year), FUN="sum")
# wyat=wy_annual_totals
# colnames(wyat)=c("wy", "PRCP_mm")
# plot(wyat$wy, wyat$PRCP_mm, type= "o", pch=19, lwd=2, col="darkblue")
# grid()
# agu_yrs = c(2010, 2014, 2015, 2017)
# points(wyat$wy[wyat$wy %in% agu_yrs], wyat$PRCP_mm[wyat$wy %in% agu_yrs], pch=19, col="red")
# write.csv(p_record_all_fj, file.path(agu_figure_dir, "interp_precip_1935_2018.csv"))


write_swbm_precip_input_file=function(){
  #read in original data (wys 1991-2011)
  # daily_precip_orig = read.table(file.path(ref_data_dir,"precip_1991_2011.txt"))
  # colnames(daily_precip_orig) = c("PRCP", "Date")
  
  p_record = get_daily_precip_table()
  
  daily_precip_updated = data.frame(PRCP = p_record$stitched, Date = p_record$Date)
  
  #format to match the original precip file, and write as text file
  daily_precip_updated$Date = paste(str_pad(day(daily_precip_updated$Date), 2, pad="0"),
                                    str_pad(month(daily_precip_updated$Date), 2, pad="0"),
                                    year(daily_precip_updated$Date), sep = "/")
  daily_precip_updated$PRCP = as.character(daily_precip_updated$PRCP / 1000) #convert to meters
  
  write.table(daily_precip_updated, file = file.path(scenario_dev_dir, "precip_regressed.txt"),
              sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)
  
}

# write_swbm_precip_input_file()



# ET ----------------------------------------------------------------------

#to do: webscrape cimis? (login?)

# to do: explore date range of spatial cimis; create daily and aggregatated refET inputs; run SWBM with both daily and aggregated refET 
#^ DONE. Makes no difference. Sticking with monthly ET values

# From _visualize yearly:
# WY 2011 is very low ET. I guess it was a high-rainfall year. It's not as low as 1997.
# Man, whenever the spatial CIMIS and the NWSETO original record overlap, the original record has
# this spike in the summer that is absent from the summer CIMIS record.
# Are we systematically overestimating ET in the original record?



write_swbm_et_input_file=function(){
  
  # 1. Read in 3 ET sources
  
  #225 station
  et225_dl_aug2019 = read.csv(file.path(ref_data_dir,"eto_daily_225.csv"))
  et225 = data.frame(Date = et225_dl_aug2019$Date, ETo_mm = et225_dl_aug2019$ETo..mm)
  et225$Date = as.Date(et225$Date, format = "%m/%d/%Y")
  et225$month1 = floor_date(et225$Date, unit = "month")
  et225_monthly = aggregate(et225$ETo_mm, by = list(et225$month1), FUN = sum, na.rm=T); colnames(et225_monthly) = c("Date", "ETo_mm")
  par(mfrow = c(2,1))
  plot(et225_monthly$Date, et225_monthly$ETo_mm, type ="l")
  plot(et225$Date, et225$ETo_mm, col = "red", type ="l")
  
  #spatial cimis
  et_dl_aug2019 = read.csv(file.path(ref_data_dir,"spatial_eto_report_aug2019.csv"))
  et_sp = data.frame(Date = et_dl_aug2019$Date, ETo_mm = et_dl_aug2019$ETo..mm)
  et_sp$Date = as.Date(et_sp$Date, format = "%m/%d/%Y")
  et_sp$month1 = floor_date(et_sp$Date, unit = "month")
  et_sp_monthly = aggregate(et_sp$ETo_mm, by = list(et_sp$month1), FUN = sum, na.rm=T); colnames(et_sp_monthly) = c("Date", "ETo_mm")
  par(mfrow = c(2,1))
  plot(et_sp_monthly$Date, et_sp_monthly$ETo_mm, type ="l")
  plot(et_sp$Date, et_sp$ETo_mm, col = "red", type ="l")
  
  
  #Original eto record
  et_orig_input = read.table(file.path(ref_data_dir,"ref_et_1990_2011.txt"))
  et_orig = data.frame(Date = as.Date(et_orig_input$V3, format = "%d/%m/%Y"), ETo_mm = et_orig_input$V1*1000)


# 2. processing (leap days, avg daily by month)
  
  #Add leap days to et record. This is easy, because they are constant values for each month.
  leap_days = as.Date(paste0(c(1992, 1996, 2000, 2004, 2008), "-02-29"))
  leap_day_values = rep(NA, 5)
  for(i in 1:length(leap_days)){ leap_day_values[i] = et_orig$ETo_mm[et_orig$Date == leap_days[i]-1]} #assign the Feb 28 value to Feb 29
  #Jeez, it's the same for each february except 2004. What weird formula made this?
  et_orig = rbind(et_orig, data.frame(Date = leap_days, ETo_mm = leap_day_values)) #append leap days
  et_orig = et_orig[order(et_orig$Date),]
  
  #add number of days per month to monthly data frames
  num_days_df = data.frame(month_day1 = model_months, num_days = num_days)
  et_sp_monthly$num_days = as.numeric(num_days_df$num_days[match(et_sp_monthly$Date, num_days_df$month_day1)])
  et225_monthly$num_days = as.numeric(num_days_df$num_days[match(et225_monthly$Date, num_days_df$month_day1)])
  #Calculate monthly averages
  et_sp_monthly$daily_avg_by_mo = et_sp_monthly$ETo_mm / et_sp_monthly$num_days
  et225_monthly$daily_avg_by_mo = et225_monthly$ETo_mm / et225_monthly$num_days
  

# 3. stitch 3 ET sources

  #Initialize stitched original-monthly record
  et_stitched_2 = data.frame(Date = model_days)
  et_stitched_2$ETo_mm = NA
  
  # 3a) Suture dates:
  # last plausible ET record for the original input is June 2011. After that it flatlines for some reason.
  # first plausible CIMIS225 record is Apr 21, 2015. first full month is May 2015.
  # So, the stitched record will be:
  # original record 1991-June 30 2011, spatial CIMIS June 30 2011-Apr 20 2015, and 225 CIMIS Apr 21 2015-Sep 30 2018.
  # So, the stitched record will be:
  # original record 1991-June 30 2011, spatial CIMIS June 30 2011-Apr 20 2015, and 225 CIMIS Apr 21 2015-Sep 30 2018.
  end_orig = as.Date("2011-06-30"); end_sp = as.Date("2015-04-20")
  
  #Assign original record
  et_stitched_2$ETo_mm[1:which(et_stitched_2$Date == end_orig)] = et_orig$ETo_mm[which(et_orig$Date==model_start_date):which(et_orig$Date == end_orig)]
  
  #Assigng spatial CIMIS record
  #declare indices for the spatial section of the CIMIS record
  sp_indices_stitched = which(et_stitched_2$Date == (end_orig+1)):which(et_stitched_2$Date ==end_sp)
  # Assign each day in the Spatial Cimis chunk of the record the monthly average ET value from the et_sp_monthly table.
  # Generate indices by matching the floor_date of each day in stitched_2 with the date in et_sp_monthly.
  et_stitched_2$ETo_mm[sp_indices_stitched] = 
    et_sp_monthly$daily_avg_by_mo[match(floor_date(et_stitched_2$Date[sp_indices_stitched], unit="month"), et_sp_monthly$Date )]
  
  # Assign CIMIS225 record
  # declare indices
  indices225_stitched = which(et_stitched_2$Date == (end_sp+1)):length(et_stitched_2$Date)
  # indices225_daily = which(et225$Date== (end_sp +1)):which(et_sp$Date == model_end_date)
  et_stitched_2$ETo_mm[indices225_stitched] = 
    et225_monthly$daily_avg_by_mo[match(floor_date(et_stitched_2$Date[indices225_stitched], unit="month"), et225_monthly$Date )]
  
# 4. write et input file
  et_input_2 = data.frame(ETo_m = et_stitched_2$ETo_mm /1000, 
                          ETo_in = et_stitched_2$ETo_mm / 25.4, #convert to inches
                          Date = et_stitched_2$Date)
  et_input_2$Date = paste(str_pad(day(et_input_2$Date), 2, pad="0"),
                          str_pad(month(et_input_2$Date), 2, pad="0"),
                          year(et_input_2$Date), sep = "/")
  write.table(et_input_2, file = file.path(scenario_dev_dir, "ref_et_monthly.txt"),
              sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)

}

# write_swbm_et_input_file()

# scratchwork -------------------------------------------------------------

# SWBM overview plot ------------------------------------------------------

# first: run the subfunctions in eci273_scenario_plotting.

# #Read in SWBM scenario outputs: monthly water budget 
# setwd("C:/Users/Claire/Documents/GitHub/SVIHM/SWBM/up2018")
# monthly_water_budget_hist = read.table("monthly_water_budget.dat", header = TRUE)
# mwb_hist = monthly_water_budget_hist
# 
# plot_water_budget_overview(mwb_hist, "Historical")


# _make monthly_precip 

# #Test 1: Don't remove NA
# monthly_precip1 = aggregate(daily_precip$PRCP_mm_orig, by = list(daily_precip$month_day1), FUN=sum)
# fj_m = aggregate(daily_precip$PRCP_mm_fj, by = list(daily_precip$month_day1), FUN=sum)
# cal_m = aggregate(daily_precip$PRCP_mm_cal, by = list(daily_precip$month_day1), FUN=sum)
# monthly_precip1=merge(monthly_precip1, fj_m, by.x = "Group.1", by.y="Group.1")
# monthly_precip1=merge(monthly_precip1, cal_m, by.x = "Group.1", by.y="Group.1")
# colnames(monthly_precip1) = c("Month", "PRCP_mm_orig", "PRCP_mm_fj", "PRCP_mm_cal")
# monthly_precip1$mean_PRCP = apply(X = monthly_precip1[,2:3], MARGIN = 1, FUN = mean, na.rm=T)
# 
# #Test 2: NA = 0
# daily_precip2 = daily_precip
# daily_precip2$PRCP_mm_cal[is.na(daily_precip2$PRCP_mm_cal)] = 0
# daily_precip2$PRCP_mm_fj[is.na(daily_precip2$PRCP_mm_fj)] = 0
# 
# monthly_precip2 = aggregate(daily_precip2$PRCP_mm_orig, by = list(daily_precip2$month_day1), FUN=sum)
# fj_m = aggregate(daily_precip2$PRCP_mm_fj, by = list(daily_precip2$month_day1), FUN=sum)
# cal_m = aggregate(daily_precip2$PRCP_mm_cal, by = list(daily_precip2$month_day1), FUN=sum)
# monthly_precip2=merge(monthly_precip2, fj_m, by.x = "Group.1", by.y="Group.1")
# monthly_precip2=merge(monthly_precip2, cal_m, by.x = "Group.1", by.y="Group.1")
# colnames(monthly_precip2) = c("Month", "PRCP_mm_orig", "PRCP_mm_fj", "PRCP_mm_cal")
# monthly_precip2$mean_PRCP = apply(X = monthly_precip2[,2:3], MARGIN = 1, FUN = mean, na.rm=T)
# 
# #Test 3: Remove NAs
# monthly_precip3 = aggregate(daily_precip$PRCP_mm_orig, by = list(daily_precip$month_day1), FUN=sum, na.rm=T)
# fj_m = aggregate(daily_precip$PRCP_mm_fj, by = list(daily_precip$month_day1), FUN=sum, na.rm=T)
# cal_m = aggregate(daily_precip$PRCP_mm_cal, by = list(daily_precip$month_day1), FUN=sum, na.rm=T)
# monthly_precip3=merge(monthly_precip3, fj_m, by.x = "Group.1", by.y="Group.1")
# monthly_precip3=merge(monthly_precip3, cal_m, by.x = "Group.1", by.y="Group.1")
# colnames(monthly_precip3) = c("Month", "PRCP_mm_orig", "PRCP_mm_fj", "PRCP_mm_cal")
# monthly_precip3$mean_PRCP = apply(X = monthly_precip3[,2:3], MARGIN = 1, FUN = mean, na.rm=T)
# 
# 
# # _make yearly_precip 
# 
# 
# #Calculate yearly average
# yearly_precip = aggregate(daily_precip$PRCP_mm_orig, by = list(daily_precip$water_year), FUN = sum)


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

# # Visual comparison (1:1 line): daily precip values
# par(mfrow = c(2,2))
# plot(daily_precip$PRCP_mm_orig, daily_precip$PRCP_mm_cal, pch = 18, main = "Original vs Cal", xlab = "orig", ylab = "daily mm precip")
# cor(daily_precip$PRCP_mm_orig, daily_precip$PRCP_mm_cal, use="pairwise.complete.obs") # 0.21
# abline(0,1, col = "red", lwd = 2)
# plot(daily_precip$PRCP_mm_orig, daily_precip$PRCP_mm_fj, pch = 18, main = "Original vs FJ", xlab = "orig", ylab = "mm precip")
# cor(daily_precip$PRCP_mm_orig, daily_precip$PRCP_mm_fj, use="pairwise.complete.obs") # 0.19
# abline(0,1, col = "red", lwd = 2)
# plot(daily_precip$PRCP_mm_orig, daily_precip$mean_PRCP, pch = 18, main = "Original vs Cal, FJ mean", xlab = "orig", ylab = "mm precip")
# cor(daily_precip$PRCP_mm_orig, daily_precip$mean_PRCP, use="pairwise.complete.obs") # 0.22
# abline(0,1, col = "red", lwd = 2)
# plot(daily_precip$PRCP_mm_cal, daily_precip$PRCP_mm_fj, pch = 18, main = "Cal vs FJ", xlab = "Cal", ylab = "FJ")
# cor(daily_precip$PRCP_mm_cal, daily_precip$PRCP_mm_fj, use="pairwise.complete.obs") # 0.67
# abline(0,1, col = "red", lwd = 2)
# 
# # Shitty match-up in daily values for orig v FJ AND orig v Cal AND orig v FJ-Cal mean. (cor of 0.19-0.22) 
# # Much better, but still pretty bad, match-up in daily values for Cal vs FJ. (cor of 0.67)
# 
# 
# # Visual comparison (1:1 line): monthly precip values, Test 1, don't remove NAs
# par(mfrow = c(2,2))
# plot(monthly_precip1$PRCP_mm_orig, monthly_precip1$PRCP_mm_cal, pch = 18, main = "Orig vs Cal, keep NAs", xlab = "orig", ylab = "mm precip")
# cor(monthly_precip1$PRCP_mm_orig, monthly_precip1$PRCP_mm_cal, use="pairwise.complete.obs") #0.81
# abline(0,1, col = "red", lwd = 2)
# plot(monthly_precip1$PRCP_mm_orig, monthly_precip1$PRCP_mm_fj, pch = 18, main = "Original vs FJ", xlab = "orig", ylab = "mm precip")
# cor(monthly_precip1$PRCP_mm_orig, monthly_precip1$PRCP_mm_fj, use="pairwise.complete.obs") #0.68
# abline(0,1, col = "red", lwd = 2)
# plot(monthly_precip1$PRCP_mm_orig, monthly_precip1$mean_PRCP, pch = 18, main = "Original vs Cal, FJ mean", xlab = "orig", ylab = "mm precip")
# cor(monthly_precip1$PRCP_mm_orig, monthly_precip1$mean_PRCP, use="pairwise.complete.obs") #0.998
# abline(0,1, col = "red", lwd = 2)
# plot(monthly_precip1$PRCP_mm_cal, monthly_precip1$PRCP_mm_fj, pch = 18, main = "Cal vs FJ", xlab = "Cal", ylab = "FJ")
# cor(monthly_precip1$PRCP_mm_cal, monthly_precip1$PRCP_mm_fj, use="pairwise.complete.obs") #0.87
# abline(0,1, col = "red", lwd = 2)
# 
# 
# # Visual comparison (1:1 line): monthly precip values, Test 2, NAs = 0
# par(mfrow = c(2,2))
# plot(monthly_precip2$PRCP_mm_orig, monthly_precip2$PRCP_mm_cal, pch = 18, main = "Orig vs Cal, NAs = 0", xlab = "orig", ylab = "mm precip")
# cor(monthly_precip2$PRCP_mm_orig, monthly_precip2$PRCP_mm_cal, use="pairwise.complete.obs") #0.83
# abline(0,1, col = "red", lwd = 2)
# plot(monthly_precip2$PRCP_mm_orig, monthly_precip2$PRCP_mm_fj, pch = 18, main = "Original vs FJ", xlab = "orig", ylab = "mm precip")
# cor(monthly_precip2$PRCP_mm_orig, monthly_precip2$PRCP_mm_fj, use="pairwise.complete.obs") #0.73
# abline(0,1, col = "red", lwd = 2)
# plot(monthly_precip2$PRCP_mm_orig, monthly_precip2$mean_PRCP, pch = 18, main = "Original vs Cal, FJ mean", xlab = "orig", ylab = "mm precip")
# cor(monthly_precip2$PRCP_mm_orig, monthly_precip2$mean_PRCP, use="pairwise.complete.obs") #0.95
# abline(0,1, col = "red", lwd = 2)
# plot(monthly_precip2$PRCP_mm_cal, monthly_precip2$PRCP_mm_fj, pch = 18, main = "Cal vs FJ", xlab = "Cal", ylab = "FJ")
# cor(monthly_precip2$PRCP_mm_cal, monthly_precip2$PRCP_mm_fj, use="pairwise.complete.obs") #0.79
# abline(0,1, col = "red", lwd = 2)
# 
# # Visual comparison (1:1 line): monthly precip values, Test 3, remove NAs
# par(mfrow = c(2,2))
# plot(monthly_precip3$PRCP_mm_orig, monthly_precip3$PRCP_mm_cal, pch = 18, main = "Orig vs Cal, remove NAs", xlab = "orig", ylab = "mm precip")
# cor(monthly_precip3$PRCP_mm_orig, monthly_precip3$PRCP_mm_cal, use="pairwise.complete.obs") #0.70
# abline(0,1, col = "red", lwd = 2)
# plot(monthly_precip3$PRCP_mm_orig, monthly_precip3$PRCP_mm_fj, pch = 18, main = "Original vs FJ", xlab = "orig", ylab = "mm precip")
# cor(monthly_precip3$PRCP_mm_orig, monthly_precip3$PRCP_mm_fj, use="pairwise.complete.obs") #0.66
# abline(0,1, col = "red", lwd = 2)
# plot(monthly_precip3$PRCP_mm_orig, monthly_precip3$mean_PRCP, pch = 18, main = "Original vs Cal, FJ mean", xlab = "orig", ylab = "mm precip")
# cor(monthly_precip3$PRCP_mm_orig, monthly_precip3$mean_PRCP, use="pairwise.complete.obs") #0.94
# abline(0,1, col = "red", lwd = 2)
# plot(monthly_precip3$PRCP_mm_cal, monthly_precip3$PRCP_mm_fj, pch = 18, main = "Cal vs FJ", xlab = "Cal", ylab = "FJ")
# cor(monthly_precip3$PRCP_mm_cal, monthly_precip3$PRCP_mm_fj, use="pairwise.complete.obs") #0.79
# abline(0,1, col = "red", lwd = 2)
# 
# 
# 
# #Visual comparison - over time
# par(mfrow = c(2,2))
# plot(monthly_precip$Month, monthly_precip$PRCP_mm_orig, type = "l", main = "Original", xlab = "Date", ylab = "mm precip")
# plot(monthly_precip$Month, monthly_precip$PRCP_mm_fj, type = "l", main = "FJ monthly", xlab = "Month", ylab = "mm precip")
# plot(monthly_precip$Month, monthly_precip$PRCP_mm_cal, type = "l", main = "Cal", xlab = "Month", ylab = "mm precip")
# plot(monthly_precip$Month, monthly_precip$mean_PRCP, type = "l", main = "Cal, FJ mean", xlab = "Month", ylab = "mm precip")
# 
# 
# # plot(monthly_precip$Month, monthly_precip$PRCP_mm_orig-monthly_precip$PRCP_mm_cal, type = "l", main = "Original minus Cal", xlab = "Month", ylab = "mm precip")
# # plot(monthly_precip$Month, monthly_precip$PRCP_mm_orig-monthly_precip$PRCP_mm_fj, type = "l", main = "Original minus FJ", xlab = "Month", ylab = "mm precip")
# plot(monthly_precip$Month, monthly_precip$PRCP_mm_orig-monthly_precip$mean_PRCP, type = "l", main = "Original minus Cal, FJ mean", xlab = "Month", ylab = "mm precip")
# 
# 
# #By forgetting to remove NA values I think I must have stumbled on what they
# #used to make the precip data.
# #
# 
# 
# 
# # May 2019 precip effort - visualization 
# 
# # Visual comparison (over time)
# par(mfrow = c(2,1))
# plot(daily_precip$Date, daily_precip$PRCP_mm_orig-daily_precip$PRCP_mm_cal, type = "l", main = "Original minus Cal", xlab = "Date", ylab = "mm precip")
# plot(daily_precip$Date, daily_precip$PRCP_mm_orig-daily_precip$PRCP_mm_fj, type = "l", main = "Original minus FJ", xlab = "Date", ylab = "mm precip")
# plot(daily_precip$Date, daily_precip$PRCP_mm_orig-daily_precip$mean_PRCP, type = "l", main = "Original minus Cal, FJ mean", xlab = "Date", ylab = "mm precip")
# 
# 
# # Visual comparison (1:1 line)
# par(mfrow = c(2,2))
# plot(daily_precip$PRCP_mm_orig, daily_precip$PRCP_mm_cal, pch = 18, main = "Original vs Cal", xlab = "orig", ylab = "mm precip")
# abline(0,1, col = "red", lwd = 2)
# plot(daily_precip$PRCP_mm_orig, daily_precip$PRCP_mm_fj, pch = 18, main = "Original vs FJ", xlab = "orig", ylab = "mm precip")
# abline(0,1, col = "red", lwd = 2)
# plot(daily_precip$PRCP_mm_orig, daily_precip$mean_PRCP, pch = 18, main = "Original vs Cal, FJ mean", xlab = "orig", ylab = "mm precip")
# abline(0,1, col = "red", lwd = 2)
# plot(daily_precip$PRCP_mm_cal, daily_precip$PRCP_mm_fj, pch = 18, main = "Cal vs FJ", xlab = "Cal", ylab = "FJ")
# abline(0,1, col = "red", lwd = 2)
# 
# 
# plot(daily_precip$Date, daily_precip$PRCP_mm_fj, type = "l", main = "Fort Jones station", xlab = "Date", ylab = "mm precip")
# 
# plot(daily_precip$Date, daily_precip$PRCP_mm_fj - daily_precip$PRCP_mm_cal, type = "l", 
#      main = "Fort Jones minus Callahan", xlab = "Date", ylab = "mm precip")
# 
# #Histogram of differences between Callahan and FJ
# hist(daily_precip$PRCP_mm_fj - daily_precip$PRCP_mm_cal, breaks = seq(-80,90,5))
# nonzero_diffs = daily_precip$PRCP_mm_fj - daily_precip$PRCP_mm_cal; nonzero_diffs = nonzero_diffs[abs(nonzero_diffs)>5]
# hist(nonzero_diffs,breaks = seq(-80,90,5), main = "Num. days with Cal-FJ difference of >5 mm", xlab = "mm difference, Cal-FJ")
# 
# 



# Precip scratchwork ------------------------------------------------------


# _long term precip trends ------------------------------------------------

# #First, retrieve the interpolated records of everything
# 
# # Step 1. run setup and the NOAA data retrieval section in tabular_data_upload.R
# noaa = data.frame(tbl(siskiyou_tables, "noaa_daily_data"))
# # Step 2. run the setup and subfunctions sections of this script to get model_start_date, etc.
# 
# station_info = make_station_table(weather_table = noaa)
# station_table = station_info[[1]]
# station_dist = station_info[[2]]
# 
# #Generate daily precip table (same daterange as in original methodology) to make model coefficients
# #Each station is a column (plus a couple extra columns); each row is a date, WY 1944-2019
# daily_precip_regression = make_daily_precip(weather_table = noaa,
#                                                               daily_precip_start_date = as.Date("1943-10-01"), 
#                                                               daily_precip_end_date = as.Date("2018-09-30")) #changed from 2011 for orig record
# model_coeff = make_model_coeffs_table(months = 1:12, 
#                                       station_table = station_table,
#                                       daily_precip = daily_precip_regression,
#                                       ys = c("fj", "cal", "gv"), 
#                                       xs = c("fj", "cal", "gv", "et", "yr", "y2"))
# #Generate daily precip table to make the gap-filled records (can be for a diff time period)
# daily_precip_p_record = make_daily_precip(weather_table = noaa,
#                                                             daily_precip_start_date = as.Date("1943-10-01"), 
#                                                             daily_precip_end_date = as.Date("2018-09-30"))
# p_record = fill_fj_cal_gv_gaps_regression_table(model_coeff=model_coeff, 
#                                                 station_table = station_table,
#                                                 daily_precip = daily_precip_p_record,
#                                                 start_date = as.Date("1943-10-01"), 
#                                                 end_date =  as.Date("2018-09-30"))
# 
# # average interp-fj and interp-cal records and compare to original
# p_record$interp_cal_fj_gv_mean =  apply(X = dplyr::select(p_record, fj_interp, cal_interp, gv_interp), 
#                                         MARGIN = 1, FUN = mean, na.rm=F)
# 
# #check for nas
# sum(is.na(p_record$gv_interp))
# 
# #Then, aggregate by month and by water year
# #monthly
# p_monthly = p_record %>% 
#   dplyr::select(PRCP_mm_cal, PRCP_mm_fj, PRCP_mm_et, PRCP_mm_gv, 
#          PRCP_mm_yr,PRCP_mm_y2, PRCP_mm_orig, 
#          fj_interp, cal_interp, gv_interp, month_day1) %>%
#   group_by(month_day1) %>%
#   summarise_all(funs(sum), na.rm=TRUE)
# 
# p_monthly_melt = melt(p_monthly, id.vars = "month_day1")
# p <- ggplot(p_monthly_melt, aes(month_day1, value, color = variable)) +
#   geom_line(aes(group=variable))
# p
# 
# #Then, aggregate by month and by water year
# #yearly
# p_wy = p_record %>% 
#   dplyr::select(#PRCP_mm_cal, PRCP_mm_fj, PRCP_mm_gv, # PRCP_mm_et, 
#                  # PRCP_mm_yr,PRCP_mm_y2, 
#                 # interp_cal_fj_gv_mean,
#                 PRCP_mm_orig,
#                 fj_interp, cal_interp, gv_interp,
#                 water_year) %>%
#   group_by(water_year) %>%
#   summarise_all(funs(sum), na.rm=TRUE)
# 
# p_yearly_melt = melt(p_wy, id.vars = "water_year")
# p <- ggplot(p_yearly_melt, aes(water_year, value/25.4, color = variable)) +
#   geom_line(aes(group=variable))
# p
# 
# #_Visualize model coeffs ---------------------------------------
# 
# model_coeff1 = model_coeff[model_coeff$months == 1,]
# model_coeff1 = model_coeff1[order(model_coeff$Y_var),]
# 
# #_Visualize raw and interp records  (annual) ------------------------------------
# # First: generate p_record going line-by-line through the "get_daily_precip_table()" function
# cal_raw = aggregate(x = p_record$PRCP_mm_cal/25.4, by=list(p_record$water_year),FUN = sum, na.rm=T)
# cal_interp = aggregate(x = p_record$cal_interp/25.4, by=list(p_record$water_year),FUN = sum, na.rm=T)
# fj_raw =  aggregate(x = p_record$PRCP_mm_fj/25.4, by=list(p_record$water_year),FUN = sum, na.rm=T)
# fj_interp =  aggregate(x = p_record$fj_interp/25.4, by=list(p_record$water_year),FUN = sum, na.rm=T)
# gv_raw = aggregate(x = p_record$PRCP_mm_gv/25.4, by=list(p_record$water_year),FUN = sum, na.rm=T)
# gv_interp = aggregate(x = p_record$gv_interp/25.4, by=list(p_record$water_year),FUN = sum, na.rm=T)
# orig = aggregate(x = p_record$PRCP_mm_orig/25.4, by=list(p_record$water_year),FUN = sum, na.rm=T)
# 
# wys = cal_interp$Group.1
# plot(wys, cal_interp$x, col = "blue", lwd = 2, type = "o", ylim = c(0,70),
#      xlab = "Water Years",ylab="Annual Precip (in)")
# lines(wys, cal_raw$x, lty = 2,  col = "cyan")
# lines(wys, fj_raw$x, col = "firebrick3", lty = 2)
# lines(wys, fj_interp$x, col = "brown",lwd = 2)
# lines(wys, gv_raw$x, col = "green", lwd = 2)
# lines(wys, gv_interp$x, col = "darkgreen", lwd = 2, lty = 2)
# lines(wys, orig$x, col = "darkgoldenrod3",lwd = 2)
# grid()
# legend(x = "topright",lwd = 2, lty = rep(c(1,2),3), 
#        legend = paste(c("Cal","Cal","FJ","FJ","GV","GV"),rep(c("raw","interp"),3)),
#        col = c("blue", "cyan","firebrick3","brown","green","darkgreen"))
# 
# # What the hell was happening at Greenview in year 1998?
# 
# wy = 1998
# date_lims = c(as.Date(paste0(wy-1,"-10-01")),as.Date(paste0(wy,"-09-30")))
# date_selector = p_record$Date >= date_lims[1] & p_record$Date<=date_lims[2]
# dates = p_record$Date[date_selector]
# orig = p_record$PRCP_mm_orig[p_record$Date >= date_lims[1] & p_record$Date<=date_lims[2]]
# 
# plot(x = dates, y = orig, type = "l",
#      xlim = date_lims, ylim = c(0,60), 
#      xlab = paste("Date in wy",wy),ylab = "Daily precip")
# lines(x = dates, y = p_record$PRCP_mm_cal[date_selector], col = "blue")
# lines(x = dates, y = p_record$PRCP_mm_fj[date_selector], col = "brown")
# lines(x = dates, y = p_record$PRCP_mm_gv[date_selector], col = "green")
# 
# #_Average annual rainfall for WY 1991-2018
# round(c(mean(cal_raw$x), mean(cal_interp$x), mean(fj_raw$x), mean(fj_interp$x)), 1)
# # Cal raw, Cal interp and FJ interp are pretty much in agreement (20.9, 20.1, and 19.9),
# # though FJ interp is consistently about 2 inches lower than cal interp.
# # FJ raw has so much missing data that its mean annual precip is 12.8.
# 
# # Test the annual averages with data from the full record running the regression
# daily_precip_regression = make_daily_precip(weather_table = noaa,
#                                                               daily_precip_start_date = as.Date("1943-10-01"), 
#                                                               daily_precip_end_date = as.Date("2018-09-30"))
# 
# p_rec_test = fill_fj_cal_gv_gaps_regression_table(model_coeff=model_coeff, 
#                                              station_table = station_table,
#                                              daily_precip = daily_precip_regression,
#                                              start_date = as.Date("1943-10-01"), 
#                                              end_date = final_table_end_date)
# p_record = p_rec_test

# Averages for WY 1944-2018: With filled-in records, cal_interp annual 
# average is 21.0, and fj_interp annual average is 20.7.
#This has dropped by 0.5 and 0.5, respectively.

##_Color-code which station used to gap-fill -------------------------------
## First: generate p_record going line-by-line through the "get_daily_precip_table()" function
# x_labs = p_record$Date
# keep_these = month(x_labs) == 10 & day(x_labs) == 1 & (year(x_labs) %% 5 == 0)
# x_labs = x_labs[keep_these]
# x_labs[!keep_these] = NA
# 
# barplot(height = p_record$fj_interp, width = .81,
#         border = p_record$fj_interp_color,
#         xaxt = "n", xlim = c(0,length(p_record$fj_interp)))
# axis(side=1,at=which(keep_these),labels=x_labs, las = 2)
# 
# legend(x = "topright", col = station_table$color, legend = station_table$abbrev, pch = 19)
# test1 = aggregate(p_record$fj_interp_color, by=list(p_record$fj_interp_color), FUN = length)
# test2 = merge(station_table@data, test1, by.x = "color", by.y="Group.1")
# 
# plot(x = p_record$Date, y = p_record$fj_interp, main = "FJ daily precip data source",
#         col = p_record$fj_interp_color, pch = 18,
#      xlab = "Date", ylab = "")
# legend(x = "topright", col = station_table$color, legend = station_table$abbrev, pch = 19)
# how_many_each_stn = aggregate(p_record$fj_interp_color, by=list(p_record$fj_interp_color), FUN = length)
# colnames(how_many_each_stn) = c("color","num_fj_interp")
# how_many_each_stn = merge(how_many_each_stn, station_table, by.x = "color",by.y="color")
# 
# plot(x = p_record$Date, y = p_record$cal_interp,
#      col = p_record$cal_interp_color, pch = 18,
#      xlab = "Date", ylab = "")
# legend(x = "topright", col = station_table$color, legend = station_table$abbrev, pch = 19)
# 
# # 
# # # _Did I replicate original 1991-2011 precip record ------------------------------
# # 
# 
# # "represented by the arithmetic average of the measured daily Fort Jones and Callahan or at Fort Jones, Callahan, and Greenview, with missing data replaced by the regression estimated data."
# p_record$cal_fj_mean =  apply(X = dplyr::select(p_record, PRCP_mm_fj, PRCP_mm_cal), 
#                                         MARGIN = 1, FUN = mean, na.rm=F)
# p_record$cal_fj_gv_mean =  apply(X = dplyr::select(p_record, PRCP_mm_fj, PRCP_mm_cal, PRCP_mm_gv), 
#                                        MARGIN = 1, FUN = mean, na.rm=F)
# #Assign: 1) avg of cal, fj, gv; 2) avg of cal and fj when gv not avail; 3) avg of INTERP cal, fj, gv data?
# p_record$orig_repro = p_record$cal_fj_gv_mean # case 1
# case_2_selector = is.na(p_record$cal_fj_gv_mean) & !is.na(p_record$cal_fj_mean)
# p_record$orig_repro[case_2_selector] = p_record$cal_fj_mean[case_2_selector]
# 
# case_3_selector = is.na(p_record$orig_repro)
# interp_cal_fj_gv_mean =  apply(X = dplyr::select(p_record, fj_interp, cal_interp, gv_interp), 
#                                  MARGIN = 1, FUN = mean, na.rm=F)
# p_record$orig_repro[case_3_selector] = interp_cal_fj_gv_mean[case_3_selector]

# 
# par(mfrow = c(3,1))
# plot(p_record$PRCP_mm_orig,p_record$orig_repro); abline(0,1, col = "red")
# cor(p_record$PRCP_mm_orig,p_record$orig_repro, use = "pairwise.complete.obs") #0.85
# plot(p_record$PRCP_mm_orig,p_record$cal_interp); abline(0,1, col = "red")
# cor(p_record$PRCP_mm_orig,p_record$cal_interp, use = "pairwise.complete.obs") #0.82
# plot(p_record$PRCP_mm_orig,p_record$fj_interp); abline(0,1, col = "red")
# cor(p_record$PRCP_mm_orig,p_record$fj_interp, use = "pairwise.complete.obs") #0.74
# 
# ## WHAT IS GOING ON
# ##NOTE: we are using a TOTALLY DIFFERENT greenview record (me and Courtney.)
# # Courtney was using a greenview record where "For the Greenview station, regression equations were used to generate daily data from monthly total reported precipitation. Daily
# 
# 
# p_record_test = p_record %>% 
#   dplyr::select(Date,PRCP_mm_orig,PRCP_mm_cal,PRCP_mm_fj,PRCP_mm_gv,
#                 cal_interp, fj_interp, gv_interp,
#                 cal_fj_mean,cal_fj_gv_mean)
# 
# p_record_test = p_record_test[p_record_test$Date >= as.Date("1990-10-18"),][1:50,]


# Did I replicate the original with my simple cal-fj interp? (no)
# 
# par(mfrow = c(3,1))
# plot(p_record$PRCP_mm_orig,p_record$interp_cal_fj_mean); abline(0,1, col = "red")
# cor(p_record$PRCP_mm_orig,p_record$interp_cal_fj_mean, use = "pairwise.complete.obs") #0.85
# plot(p_record$PRCP_mm_orig,p_record$cal_interp); abline(0,1, col = "red")
# cor(p_record$PRCP_mm_orig,p_record$cal_interp, use = "pairwise.complete.obs") #0.82
# plot(p_record$PRCP_mm_orig,p_record$fj_interp); abline(0,1, col = "red")
# cor(p_record$PRCP_mm_orig,p_record$fj_interp, use = "pairwise.complete.obs") #0.74


# plot(p_record$PRCP_mm_cal, p_record$interp_cal_fj_mean); abline(0,1, col = "red")
# plot(p_record$PRCP_mm_fj, p_record$interp_cal_fj_mean); abline(0,1, col = "red")



# 
# 
# # That is... a reasonable reproduction of the original values?
# 
# # plot monthly values to see if they match
# monthly_interp = aggregate(p_record$interp_cal_fj_mean, by = list(p_record$month_day1), FUN = sum)
# monthly_orig = aggregate(p_record$PRCP_mm_orig, by = list(p_record$month_day1), FUN = sum)
# cor(monthly_orig$x, monthly_interp$x, use = "pairwise.complete.obs") #0.87
# plot(monthly_orig$x, monthly_interp$x)
# abline(0,1, col = "red")
# 
# # 3 dates above 300 mm / month: 2005-02 is a serious outlier
# View(monthly_interp)
# 
# # FOR NOW: this is our precip record, eh
# par(mfrow = c(3,1))
# plot(p_record$Date, p_record$PRCP_mm_fj, type = "l", ylim = c(0, 120))
# plot(p_record$Date, p_record$PRCP_mm_cal, type = "l", ylim = c(0, 120))
# plot(p_record$Date, p_record$interp_cal_fj_mean, type = "l", ylim = c(0, 120))



# # _exceedance curves ------------------------------------------------------
# 
# legend_labels = c("Callahan", "Fort Jones",
#                   "Greenview", "Yreka", "Yreka NW",
#                   "Original 91-11", "Cal-FJ Mean",
#                   "Gap-filled Fort Jones", "Gap-filled Callahan",
#                   "Gap-filled Cal-FJ Mean")
# legend_colors = c("firebrick3","gold3", 
#                   "forestgreen", "dodgerblue4", "dodgerblue1",
#                   "black","gray",
#                   "gold1","firebrick1",
#                   "green")
# 
# 
# 
# #initialize plot
# par(mfrow = c(1,1))
# plot(x = NA, y = NA, xlim = c(0.6, 1), ylim = c(-3, 5), xlab = "Exceedance probability", ylab = "log daily precip, mm")
# 
# col_names = colnames(p_record)[!(colnames(p_record) %in% c("Date","month_day1", "water_year", "PRCP_mm_et","fj_interp_color","cal_interp_color"))]
# for(i in 1:length(col_names)){
#   # i=i+1
#   col_name = col_names[i]
#   record = dplyr::select(p_record, col_name)
#   record = log(record[!is.na(record)])
#   record_exceedance = (record[order(record)])
#   t = length(record_exceedance)
#   print(paste(col_name,"has", t,"non-NA and non-0 values"))
#   lines(x = (1:t)/t, y = record_exceedance, type = "l", 
#         col = legend_colors[i], main = col_name, lwd = 2)#,
#   # xlab = "Exceedance probability", ylab = "log daily precip, mm")
# }
# legend(x = "topleft", legend = legend_labels, cex = 0.8, 
#        lwd = rep(5, length(col_names)), col = legend_colors)




# # _visualize regression relationships --------------------------------------
# 

# months = 1:12; ys = c("fj", "cal", "gv"); xs = c("fj", "cal", "gv", #"et",
#                                            "yr", "y2")
# # 
# plotter = expand.grid(months = months, Y_var = ys, X_var = xs)
# # plotter = expand.grid(Y_var = ys, X_var = xs)
# #take out matching combinations (e.g. x = fj, y = fj)
# plotter = plotter[as.character(plotter$Y_var) != as.character(plotter$X_var),]
# # 
# 
# pdf("precip regression relats.pdf", width = 7, height = 7)
# par(mfrow = c(3,4))
# 
# for(i in 1:dim(plotter)[1]){
#   mnth = as.numeric(plotter$month[i])
#   y_abbrev = as.character(plotter$Y_var[i])
#   x_abbrev = as.character(plotter$X_var[i])
# 
#   y_record = p_record[month(p_record$Date) == mnth,
#                       station_table$col_num[station_table$abbrev == y_abbrev]]
#   x_record = p_record[month(p_record$Date) == mnth,
#                       station_table$col_num[station_table$abbrev == x_abbrev]]
#   
#   cor_xy = cor(x_record, y_record, use = "pairwise.complete.obs")
#   relat = lm(y_record~x_record)
# 
#   plot(x_record, y_record, xlab = x_abbrev, ylab = y_abbrev, main = mnth)
#   
#   #color code the line
#   if(is.na(cor_xy)){best_fit_col = "darkgray"} else {
#     if(cor_xy > 0.75){best_fit_col = "darkgreen"}
#     if(cor_xy > 0.5 & cor_xy <= 0.75){best_fit_col = "goldenrod3"}
#     if(cor_xy > 0.25 & cor_xy <= 0.5){best_fit_col = "orangered"}
#     if(cor_xy < 0.25){best_fit_col = "darkred"} }
#   abline(lm(y_record~x_record), col= best_fit_col, lwd = 2.5)
#   abline(0,1, col = "darkgray", lwd = 2, lty = 2)
#   legend(x = "topleft", cex = 0.7,
#          legend = paste0("m=",round(relat$coefficients[2],2),
#                         "; R2=",round(summary(relat)$r.squared,digits = 2)))
#   
# }
# dev.off()
# 
# 
# 
# # _visualize distance vs regression method --------------------------------
# par(mfrow = c(3,1))
# plot(p_record_regress$interp_cal_fj_mean, p_record_dist$interp_cal_fj_mean)
# cor(p_record_regress$interp_cal_fj_mean, p_record_dist$interp_cal_fj_mean, use="pairwise.complete.obs") # 0.91
# plot(p_record_regress$PRCP_mm_orig, p_record_regress$interp_cal_fj_mean)
# plot(p_record_dist$PRCP_mm_orig, p_record_dist$interp_cal_fj_mean)
# 
# plot(p_record_regress$Date,p_record_regress$PRCP_mm_orig, type = "l")
# plot(p_record_regress$Date,p_record_regress$interp_cal_fj_mean, type = "l")
# plot(p_record_dist$Date,p_record_dist$interp_cal_fj_mean, type = "l")
# 
# tot_reg = sum(p_record_regress$interp_cal_fj_mean, na.rm=T)
# tot_dist = sum(p_record_dist$interp_cal_fj_mean, na.rm=T)
# 
# tot_dist / tot_reg # the distance one yields about 9.5% more overall. Probably because Etna is close to both Cal and FJ.
# 
# # _histograms -------------------------------------------------------------
# 
# legend_labels = c("Callahan", "Fort Jones","Greenview", 
#                   "Original 91-11", "Cal-FJ Mean",
#                   "Gap-filled Callahan", "Gap-filled Fort Jones",
#                   "Gap-filled Cal-FJ Mean")
# #initialize plot
# # plot(x = NA, y = NA, xlim = c(0.6, 1), ylim = c(0, 120), xlab = "Exceedance probabylity", ylab = "log daily precip, mm")
# 
# #need to fix these to account for 0s
# par(mfrow = c(2,4))
# col_names = colnames(p_record)[!(colnames(p_record) %in% c("Date","month_day1", "water_year", "PRCP_mm_et"))]
# for(i in 1:length(col_names)){
#   col_name = col_names[i]
#   record = select(p_record, col_name)
#   record = log(record[!is.na(record)])
#   hist(record, main = col_name, col = i,
#        freq = F)#, xlim = c(-2.5, 6), ylim = c(0, 0.4))
#   abline(v = mean(record), lty = 2)
#   print(mean(record))
# }
# 
# 
# 
# 
# 
# 
# # _How many days are 0 ppt? ----------------------------------------
# 
# 
# # precip_for_tab = daily_precip[daily_precip$Date >= model_start_date & daily_precip$Date <= "2011-09-30",]
# precip_for_tab = p_record; precip_for_tab$month_day1=NULL; precip_for_tab$water_year=NULL
# 
# num_cols = dim(precip_for_tab)[2]
# num_nas = apply(precip_for_tab[2:num_cols], MARGIN = 2, function(x) sum(is.na(x)))
# num_0s = apply(precip_for_tab[2:num_cols], MARGIN = 2, function(x) sum(x==0, na.rm=T))
# num_gt0 = apply(precip_for_tab[2:num_cols], MARGIN = 2, function(x) sum(x>0, na.rm=T))
# 
# p_tab = data.frame( num_nas, num_0s, num_gt0)
# 
# total_days = as.numeric(unique(p_tab[1]+p_tab[2]+p_tab[3]))
# p_tab$perc_0 = round(p_tab$num_0s / total_days, 2)*100
# p_tab$perc_na = round(p_tab$num_nas / total_days, 2)*100
# p_tab$perc_gt0 = round(p_tab$num_gt0 / total_days, 2)*100
# p_tab$num0_div_gt0 = round(p_tab$num_0s / p_tab$num_gt0, 2) 
# 
# 
# 
# # Precip: how different are exceedance curves for each station? For orig precip input?
# #starting point: histograms
# 
# # plot(p_record$Date, p_record$PRCP_mm_orig, ylim = c(0, 120))
# # plot(p_record$Date, p_record$PRCP_mm_fj, ylim = c(0, 120))
# # plot(p_record$Date, p_record$PRCP_mm_cal, ylim = c(0, 120))
# par(mfrow = c(2,2))
# 
# # need to fix these to account for 0 values becoming -Inf. proportion of 0 values. 
# hist(log10(p_record$PRCP_mm_fj), xlab = "log10 of FJ daily precip", main = NA, col = "wheat", xlim = c(-1.5,2), ylim = c(0,0.6), freq = F)
# hist(log10(p_record$PRCP_mm_cal), xlab = "log10 of Cal daily precip", main = NA, col = "wheat", xlim = c(-1.5,2), ylim = c(0,0.6),freq = F)
# hist(log10(p_record$PRCP_mm_orig), xlab = "log10 of Original Input daily precip", main = NA, col = "wheat", xlim = c(-1.5,2), ylim = c(0,0.6),freq = F)
# hist(log10(p_record$interp_cal_fj_mean), xlab = "log10 of Interp FJ-Cal daily precip", main = NA, col = "wheat", xlim = c(-1.5,2), ylim = c(0,0.6),freq = F)
# 
# 
# #_Precip (geography) ------------------------------------------------------
# 
# # Step 1. run setup and the NOAA data retrieval section in tabular_data_upload.R
# rm(list = ls()[!(ls() %in% c("wx")) ]) #Remove all variables other than the weather dataframe
# # Step 2. run the setup and subfunctions sections of this script to get model_start_date, etc.
# 
# 
# station_info = make_station_table()
# station_table = station_info[[1]]; station_dist = station_info[[2]]
# 
# daily_precip_p_record = make_daily_precip(weather_table = wx,
#                                                             record_start_date = model_start_date, 
#                                                             daily_precip_end_date = model_end_date)
# 
# p_record = fill_fj_cal_gaps_distance(station_dist = station_dist, 
#                                      daily_precip = daily_precip_p_record,
#                                      start_date = model_start_date, 
#                                      end_date = model_end_date)
# 
# # average interp-fj and interp-cal records and compare to original
# p_record$interp_cal_fj_mean =  apply(X = dplyr::select(p_record, fj_interp, cal_interp), 
#                                      MARGIN = 1, FUN = mean, na.rm=T)







# ET scratchwork ----------------------------------------------------------

# _stitch together orig and update ET----------------------------------------


# #Attempt in May 2019
# #units? 
# et_dl_may2019 = read.csv(file.path(ref_data_dir,"eto_spatial_report.csv"))
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


# _visualize records ------------------------------------------------------

# 
# #Set date bounds/plot titles
# # sd = as.Date("1990-10-01"); ed = as.Date("2019-09-30")
# sd = as.Date("2017-03-01"); ed = as.Date("2017-09-01")
# name_string = "ETo 2017"
# 
# #Initialize pdf
# pdf(file = paste0(name_string,".pdf"), 11, 8.5)
# 
# #Plot params
# par(mfrow = c(2,1))
# # vert_lines = seq(sd, ed, by = "year")
# vert_lines = seq(sd, ed, by = "month")
# horz_lines_daily = seq(0, 10, 2); horz_lines_monthly = seq(0,300,50)
# 
# #daily data
# 
# plot(et_orig$Date, et_orig$ETo_mm, type ="l", col = "black", xlim = c(sd, ed), ylim = c(0,12),
#      xlab = "Date", ylab = "Daily Reference ET (mm)", main = paste("Daily", name_string))
# abline(v = vert_lines, h = horz_lines_daily, col = "darkgray")
# lines(et_orig$Date, et_orig$ETo_mm, type ="l", col = "black", xlim = c(sd, ed))
# lines(et_sp$Date, et_sp$ETo_mm, type ="l", col = rgb(1,0,0,0.6), xlim = c(sd, ed))
# lines(et225$Date, et225$ETo_mm, type ="l", col = rgb(0,0,1,0.5), xlim = c(sd, ed))
# axis(side = 1, at = vert_lines, labels=F)
# legend(x = "topright", lwd = c(1,1,1), col = c("black",rgb(1,0,0,0.7), rgb(0,0,1,0.5)), cex = 0.9,bg="white",
#        legend = c("1991-2011 SVIHM ETo", "Spatial CIMIS circa 225", "CIMIS Station 225 data"))
# 
# 
# # monthly data
# plot(et_orig_monthly$Date, et_orig_monthly$ETo_mm, type ="l", lwd=2, col = "black", xlim = c(sd, ed), ylim = c(0,300),
#      xlab = "Date", ylab = "Monthly Reference ET (mm)", main = paste("Monthly", name_string))
# lines(et_sp_monthly$Date, et_sp_monthly$ETo_mm, type = "l", lwd=2, col = rgb(1,0,0,0.7), xlim = c(sd, ed))
# lines(et225_monthly$Date, et225_monthly$ETo_mm, type = "l", lwd=2, col = rgb(0,0,1,0.5), xlim = c(sd, ed))
# abline(v = vert_lines, h = horz_lines_monthly, col = "darkgray")
# legend(x = "topright", lwd = c(2,2,2), col = c("black",rgb(1,0,0,0.7), rgb(0,0,1,0.5)), cex = 0.9, bg="white",
#        legend = c("1991-2011 SVIHM ETo", "Spatial CIMIS circa 225", "CIMIS Station 225 data"))
# 
# dev.off()



# _stitch original-daily record (aug 2019) -----------------------------------------------------------------------

# #Need to match this format.
# 
# last plausible ET record for the original input is June 2011. After that it flatlines for some reason.
# first plausible CIMIS225 record is Apr 21, 2015. first full month is May 2015.
# So, the stitched record will be:
# original record 1991-June 30 2011, spatial CIMIS June 30 2011-Apr 20 2015, and 225 CIMIS Apr 21 2015-Sep 30 2018.

# end_orig = as.Date("2011-06-30"); end_sp = as.Date("2015-04-20")
# et_orig = data.frame(Date = as.Date(et_orig_input$Date, format = "%d/%m/%Y"), ETo_mm = et_orig_input$ETo_m*1000)

# #Initialize stitched record
# et_stitched_1 = data.frame(Date = model_days); et_stitched_1$ETo_mm = NA
# 
# #Attempt to assign original record to 1991-2011 period
# # et_stitched_1$ETo_mm[1:which(et_stitched_1$Date == end_orig)] = et_orig$ETo_mm[which(et_orig$Date==model_start_date): which(et_orig$Date == end_orig)]
# # This ^ doesn't work because they didn't include the mother effing leap days
# 
# #Add leap days to et record. This is easy, because they are constant values for each month. 
# leap_days = as.Date(paste0(c(1992, 1996, 2000, 2004, 2008), "-02-29"))
# leap_day_values = rep(NA, 5)
# for(i in 1:length(leap_days)){ leap_day_values[i] = et_orig$ETo_mm[et_orig$Date == leap_days[i]-1]} #assign the Feb 28 value to Feb 29
# #Jeez, it's the same for each february except 2004. What weird formula made this?
# et_orig = rbind(et_orig, data.frame(Date = leap_days, ETo_mm = leap_day_values)) #append leap days
# et_orig = et_orig[order(et_orig$Date),]
# #Now it works to assign the original record:
# #Original record
# et_stitched_1$ETo_mm[1:which(et_stitched_1$Date == end_orig)] = et_orig$ETo_mm[which(et_orig$Date==model_start_date):which(et_orig$Date == end_orig)]
# #Spatial CIMIS record
# et_stitched_1$ETo_mm[which(et_stitched_1$Date == (end_orig+1)):which(et_stitched_1$Date ==end_sp)] = 
#   et_sp$ETo_mm[which(et_sp$Date== (end_orig +1)):which(et_sp$Date == end_sp)]
# # CIMIS225 record
# et_stitched_1$ETo_mm[which(et_stitched_1$Date == (end_sp+1)):length(et_stitched_1$Date)] = 
#   et225$ETo_mm[which(et225$Date==(end_sp +1)): which(et225$Date == model_end_date)]


# # _stitch original-monthly record (aug 2019) -------------------------------
# 
# #add number of days per month to monthly data frames
# num_days_df = data.frame(month_day1 = model_months, num_days = num_days)
# et_sp_monthly$num_days = as.numeric(num_days_df$num_days[match(et_sp_monthly$Date, num_days_df$month_day1)])
# et225_monthly$num_days = as.numeric(num_days_df$num_days[match(et225_monthly$Date, num_days_df$month_day1)])
# #Calculate monthly averages
# et_sp_monthly$daily_avg_by_mo = et_sp_monthly$ETo_mm / et_sp_monthly$num_days
# et225_monthly$daily_avg_by_mo = et225_monthly$ETo_mm / et225_monthly$num_days
# 
# #Initialize stitched original-monthly record
# et_stitched_2 = data.frame(Date = model_days)
# et_stitched_2$ETo_mm = NA
# 
# #Original record
# et_stitched_2$ETo_mm[1:which(et_stitched_2$Date == end_orig)] = et_orig$ETo_mm[which(et_orig$Date==model_start_date):which(et_orig$Date == end_orig)]
# 
# #Spatial CIMIS record
# #declare indices for the spatial section of the CIMIS record
# sp_indices_stitched = which(et_stitched_2$Date == (end_orig+1)):which(et_stitched_2$Date ==end_sp)
# # Assign each day in the Spatial Cimis chunk of the record the monthly average ET value from the et_sp_monthly table.
# # Generate indices by matching the floor_date of each day in stitched_2 with the date in et_sp_monthly.
# et_stitched_2$ETo_mm[sp_indices_stitched] = 
#   et_sp_monthly$daily_avg_by_mo[match(floor_date(et_stitched_2$Date[sp_indices_stitched], unit="month"), et_sp_monthly$Date )]
# 
# # CIMIS225 record
# # declare indices
# indices225_stitched = which(et_stitched_2$Date == (end_sp+1)):length(et_stitched_2$Date)
# # indices225_daily = which(et225$Date== (end_sp +1)):which(et_sp$Date == model_end_date)
# et_stitched_2$ETo_mm[indices225_stitched] = 
#   et225_monthly$daily_avg_by_mo[match(floor_date(et_stitched_2$Date[indices225_stitched], unit="month"), et225_monthly$Date )]


# # _visualize stitched records ---------------------------------------------
# 
# 
# par(mfrow = c(2,1))
# plot(et_stitched_1$Date, et_stitched_1$ETo_mm, type = "l")
# plot(et_stitched_2$Date, et_stitched_2$ETo_mm, type = "l")
# 
# #Set date bounds/plot titles
# # sd = as.Date("1990-10-01"); ed = as.Date("2019-09-30")
# sd = as.Date("2017-03-01"); ed = as.Date("2017-09-01")
# name_string = "ETo 2017"
# 
# #Initialize pdf
# pdf(file = paste0(name_string,".pdf"), 11, 8.5)
# 
# #Plot params
# par(mfrow = c(2,1))
# # vert_lines = seq(sd, ed, by = "year")
# vert_lines = seq(sd, ed, by = "month")
# horz_lines_daily = seq(0, 10, 2); horz_lines_monthly = seq(0,300,50)
# 
# #daily data
# 
# plot(et_orig$Date, et_orig$ETo_mm, type ="l", col = "black", xlim = c(sd, ed), ylim = c(0,12),
#      xlab = "Date", ylab = "Daily Reference ET (mm)", main = paste("Daily", name_string))
# abline(v = vert_lines, h = horz_lines_daily, col = "darkgray")
# lines(et_orig$Date, et_orig$ETo_mm, type ="l", col = "black", xlim = c(sd, ed))
# lines(et_sp$Date, et_sp$ETo_mm, type ="l", col = rgb(1,0,0,0.6), xlim = c(sd, ed))
# lines(et225$Date, et225$ETo_mm, type ="l", col = rgb(0,0,1,0.5), xlim = c(sd, ed))
# axis(side = 1, at = vert_lines, labels=F)
# legend(x = "topright", lwd = c(1,1,1), col = c("black",rgb(1,0,0,0.7), rgb(0,0,1,0.5)), cex = 0.9,bg="white",
#        legend = c("1991-2011 SVIHM ETo", "Spatial CIMIS circa 225", "CIMIS Station 225 data"))


# # _visualize yearly -------------------------------------------------------
# 
# et_stitched_1$wy = year(et_stitched_1$Date)
# et_stitched_1$wy[month(et_stitched_1$Date) > 9] = et_stitched_1$wy[month(et_stitched_1$Date) > 9] +1
# et_yearly = aggregate(et_stitched_1$ETo_mm, by = list(et_stitched_1$wy), FUN = sum)
# colnames(et_yearly) = c("Date", "ETo_mm")
# # barplot(height = et_yearly$ETo_mm, names.arg = et_yearly$Date)
# plot(et_yearly$Date, et_yearly$ETo_mm, ylim = c(0, 1500))
# grid()
# 
# # WY 2011 is very low ET. I guess it was a high-rainfall year. It's not as low as 1997.
# # Man, whenever the spatial CIMIS and the NWSETO original record overlap, the original record has
# # this spike in the summer that is absent from the summer CIMIS record. 
# # Are we systematically overestimating ET in the original record? 

# _write ET output files ---------------------------------------------------

# et_input_1 = data.frame(ETo_m = round(et_stitched_1$ETo_mm /1000, 9), #convert to meters
#                         ETo_in = round(et_stitched_1$ETo_mm / 25.4, 2), #convert to inches
#                         Date = et_stitched_1$Date)
# et_input_1$Date = paste(str_pad(day(et_input_1$Date), 2, pad="0"),
#                                                str_pad(month(et_input_1$Date), 2, pad="0"),
#                                                year(et_input_1$Date), sep = "/")
# write.table(et_input_1, file = file.path(scenario_dev_dir, "ref_et_daily.txt"),
#             sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)

# et_input_2 = data.frame(ETo_m = et_stitched_2$ETo_mm /1000, 
#                         ETo_in = et_stitched_2$ETo_mm / 25.4, #convert to inches
#                         Date = et_stitched_2$Date)
# et_input_2$Date = paste(str_pad(day(et_input_2$Date), 2, pad="0"),
#                         str_pad(month(et_input_2$Date), 2, pad="0"),
#                         year(et_input_2$Date), sep = "/")
# write.table(et_input_2, file = file.path(scenario_dev_dir, "ref_et_monthly.txt"),
#             sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)

