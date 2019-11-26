# Precip record analysis and scenario generation.
# Based on a project for ECI273.
library(lubridate)
library(dplyr)
library(rstudioapi)
library(moments) #to calculate skewness of rainfall records

#Input : daily rainfall record, (depth in m) from Oct 1 1990 - Sep 30 2011 (WY 1991-2011)

dev_mode = TRUE #this is false if calling from another script. flip to true if working in this script

if(dev_mode){
  rm(list = ls())
  
  svihm_dir = dirname(dirname(getActiveDocumentContext()$path ))
  ref_data_dir = file.path(svihm_dir, "SVIHM_Input_Files", "reference_data")
  pdf_dir = file.path(svihm_dir,"SVIHM_Input_Files","Scenario_Development")#,"comparison_pdfs")
  swbm_dir = file.path(svihm_dir, "SWBM")
}
# declare this directory - location for historical precip - regardless
scenario_dev_dir = file.path(svihm_dir,"SVIHM_Input_Files", "Scenario_Development")

#Read in water year type table (Sacramento Valley Water Index)
# See also Deas analysis saying that Scott Valley water year type tracks sac valley
wy_type = read.csv(file.path(ref_data_dir,"sac_valley_wyi.csv"), fileEncoding="UTF-8-BOM")

#Read in precip data
ppt_hist = as.data.frame(read.table(file.path(scenario_dev_dir,"precip_regressed_2019.08.19.txt")))
colnames(ppt_hist) = c("precip_m", "date")
ppt_hist$date = as.Date(ppt_hist$date, format = "%d/%m/%Y")

stm_hist = as.data.frame(read.table(file.path(scenario_dev_dir,"streamflow_input_hist.txt"), header = TRUE))
colnames(stm_hist)[colnames(stm_hist) == "Month"] = "date"
stm_hist$date = as.Date(as.character(stm_hist$date), format = "%Y-%m-%d")

P = ppt_hist #short name

#source SVIHM input analyses to get the filled-in rainy season
declare_dir_in_analyses_script = TRUE
source(file.path(svihm_dir,"SVIHM_Input_Files","SVIHM_input_analyses.R"))


#Add month, year, and water year info for analysis
# P$mo = as.numeric(strftime(P$date, "%m"))
# P$yr = as.numeric(strftime(P$date, "%Y"))
# P$wy = P$yr
# P$wy[P$mo > 9] = P$yr[P$mo > 9] + 1



# 1. Rainy season characteristics  -----------------------------------------------------


#Step 1a. calculate length of rainy season for each water year. 
# Visually check rainy season definition

#Functions
rainy_season_cumsum = function(P){ # assume 2 columns: 'precip_m' (daily), and 'date' format "%d/%m/%Y"
  ##Add month, year, and water year info for analysis, if you haven't already
  if(!is.Date(P$date)){
    P$date = as.Date(P$date, format = "%d/%m/%Y")
  } #make date-type if it is not
  if(length(setdiff(c("mo","yr","wy"), colnames(P))) > 0){ #If mo, yr, and wy are not in P's column names, add them
    P$mo = as.numeric(strftime(P$date, "%m"))
    P$yr = as.numeric(strftime(P$date, "%Y"))
    P$wy = P$yr
    P$wy[P$mo > 9] = P$yr[P$mo > 9] + 1
  }
  
  wys = unique(P$wy)
  
  for(wy in wys){
    selector = P$wy == wy
    P$cum_precip_m[selector] = cumsum(P$precip_m[selector])
    total = sum(P$precip_m[selector])
    
    P$cum_frac_precip[selector] = P$cum_precip[selector] / total
  }
  return(P)
}

plot_by_wy_type = function(Y, rs_bounds=c(0.1,0.9)){ 
  #for y, assumes dataframe with date-type column 'date' and y-value to be plotted
  #for rs_bounds, assume vector of 2 values between 0 and 1 defining rainy season start and end as fraction of total water year rainfall
  
  y_all_yrs = Y[,colnames(Y)!="date"] # assign y-value
  y_label = colnames(Y)[colnames(Y) != "date"] # for titles of pdfs and figures
  
  if(length(setdiff(c("mo","yr","wy"), colnames(Y))) > 0){ #If mo, yr, and wy are not in Y's column names, add them
    Y$mo = as.numeric(strftime(Y$date, "%m"))
    Y$yr = as.numeric(strftime(Y$date, "%Y"))
    Y$wy = Y$yr
    Y$wy[Y$mo > 9] = Y$yr[Y$mo > 9] + 1
  }
  
  wys = unique(Y$wy)
  
  #Read in water year type and color-coding information
  setwd(ref_data_dir)
  wy_type_table = read.csv("sac_valley_wyi.csv", fileEncoding="UTF-8-BOM")
  wy_color = as.data.frame(cbind(c("W", "AN", "BN", "D", "C"),
                                 c("Wet","Above Normal", "Below Normal", "Dry", "Critical"),
                                       c("darkblue","darkcyan", "goldenrod", 
                                         "darkorange2", "firebrick3")))
  colnames(wy_color) = c("type","descrip","color")
  
  #Initialize pdf
  setwd(pdf_dir)
  pdfname = paste0("plot_by_wy_",y_label,".pdf")
  pdf(pdfname, 7.5, 9)
  par(mfrow = c(5,1))
  
  for(wy_type_for_plot in wy_color$type){
    #Initialize plot
    plot(x = NA, y = NA,
         main = paste0(y_label,", Sacramento Valley water year type ",wy_type_for_plot),
         xlim = c(1, 365), ylim = c(min(y_all_yrs, na.rm=T), max(y_all_yrs, na.rm=T)),
         xlab = "day of water year (starting Oct 1st)", ylab = y_label)
    grid()
    
    #For each type of water year (Wet, Above and Below Normal, Dry, Critical), plot lines for each wy
    wy_in_this_category = wy_type_table$wat_yr[wy_type_table$wat_yr %in% wys
                                               & wy_type_table$yr_type == wy_type_for_plot]
    
    #add lines for each year, colored by water year type
    for(wy in wy_in_this_category){ #for all water years in this category
      color_for_plot = as.character(wy_color$color[wy_color$type == wy_type_for_plot])
      selector = Y$wy == wy
      
      #assign x (day of year) and y (plotted value)
      if(sum(selector) == 366){x = 1:366} else {x = 1:365} #assign x. leap year case acommodated
      y = y_all_yrs[selector]
      
      lines(x, y, col = color_for_plot) 
      if(substr(y_label,1,3)=="cum"){ #If plotting cumulative values, make rainy season bounds lines
        #calculate x-values for y-values of 10% and 90% of cumulative total
        total = max(y)
        x_start = min(x[y>= rs_bounds[1] * total]) #first x value where y >= 10% 
        x_end = min(x[y>= rs_bounds[2] * total])
        abline(v = c(x_start, x_end), lty = 2, col = c("pink", "blue"), lwd = 2)
      }
    }
      
  }
  dev.off()
}

rainy_season_table = function(P, rs_bounds = c(0.1, 0.9)){ 
  # for P, assume dataframe with 2 columns: 'precip_m' (daily), and 'date' format "%d/%m/%Y"
  # for rs_bounds, assume vector of 2 values between 0 and 1 defining rainy season start and end as fraction of total water year rainfall
  
  if(!is.Date(P$date)){P$date = as.Date(P$date, format = "%d/%m/%Y")} #make date-type if it is not
  if(length(setdiff(c("mo","yr","wy"), colnames(P))) > 0){ #If mo, yr, and wy are not in P's column names, add them
    P$mo = as.numeric(strftime(P$date, "%m"))
    P$yr = as.numeric(strftime(P$date, "%Y"))
    P$wy = P$yr
    P$wy[P$mo > 9] = P$yr[P$mo > 9] + 1
  }
  
  #read in water year type
  setwd(ref_data_dir); wy_type = read.csv("sac_valley_wyi.csv", fileEncoding="UTF-8-BOM")
  
  wat_yr = unique(P$wy) #for all water years in precip dataframe
  #Initialize table
  rs_table = as.data.frame(wat_yr)
  
  for(wy in wat_yr){
    selector = P$wy == wy #water year selector vector
    
    #calculate cumulative rainfall time series stats for water year
    P$cum_precip_m[selector] = cumsum(P$precip_m[selector])
    total = sum(P$precip_m[selector])
    # P$cum_frac_precip[selector] = P$cum_precip[selector] / total
    
    #assign x (day of year) and y (cumulative precip) vectors
    if(sum(selector) == 366){x = 1:366} else {x = 1:365} #leap year case accommodated
    y = P$cum_precip_m[selector] 
    
    #calculate x-values for y-values of start and end of rainy season
    y_gt_0 = min(y[y >= 0]) #first day of water year with non-zero rainfall
    y_start = min(y[y >= rs_bounds[1] * total])
    x_start = x[y == y_start][1] #first x value where y crosses rainy season start threshold
    y_end = min(y[y>= rs_bounds[2] *total])
    x_end = x[y == y_end][1] #first x value where y crosses rainy season end threshold
    
    rs_table$total_precip_m[wat_yr == wy] = total
    rs_table$wy_day_start[wat_yr == wy] = x_start
    rs_table$wy_day_end[wat_yr == wy] = x_end
    rs_table$duration_days[wat_yr == wy] = x_end - x_start + 1
  }
  rs_table$intensity = rs_table$total_precip_m / rs_table$duration_days 
  #add water year type to table
  rs_table$wat_yr_type = wy_type$yr_type[match(rs_table$wat_yr, wy_type$wat_yr)]
  
  return(rs_table)
}


#Definition based on date of 10% and 90% of cumulative rainfall
# xlimits = as.Date(c("1990-10-01", "1993-10-01"))
# plot(P$date, P$precip_m, type = "l", xlim = xlimits) #test plot

#Attach cumulative sum of rainfall to dataframe
P = rainy_season_cumsum(P)
# plot(P$cum_precip, type = "l", main = "cumulative rainy season precip for 21 water years")
# plot(P$cum_frac_precip, type = "l", main = "cumulative fraction of rainy season precip for 21 water years")

# Identify rainy season for each year
rainy_season_bounds = c(0.1, 0.9) #i.e., "rainy season" defined as the part of year between 10% and 90% of cumulative rain fallen
#rain_season_table = rainy_season_table(select(P, date, precip_m), rainy_season_bounds)

## What's the distribution for rainy season duration?
#hist(rain_season_table$wy_duration_days /30.5, breaks=(1:12), 
#     xlab = "Number of months (approx)", 
#     main = "Rainy season duration - Water Years 1991-2011")

#Plot precip time series, cumulative precip and cumulative fraction by water year
#Saves a pdf for each
# plot_by_wy_type(select(P, date, precip_m)) #select extracts just specified columns as a dataframe
# plot_by_wy_type(select(P, date, cum_precip_m), rainy_season_bounds) #select extracts just specified columns as a dataframe
# plot_by_wy_type(select(P, date, cum_frac_precip), rainy_season_bounds) #select extracts just specified columns as a dataframe


# Step 1b. calculate overall rainy season intensity (depth total season precip / length rainy season)

rain_season_table = rainy_season_table(select(P, date, precip_m), rainy_season_bounds)

# rain_season_table$intensity = rain_season_table$total_precip_m / rain_season_table$duration_days 
## What's the distribution of rainy season intensities?
# hist(rain_season_table$intensity * 1000, breaks=(seq(0, 6, 0.5)), 
#      xlab = "Millimeters per day (approx)", main = "Rainy season intensity - Water Years 1991-2011")

# #add water year type to table - did this in the function
# rain_season_table$wat_yr_type = wy_type$yr_type[match(rain_season_table$wat_yr, wy_type$wat_yr)]

# #Any relationship between water year type and intensity, or duration? make boxplots
# plot(rain_season_table$wat_yr_type, rain_season_table$intensity,
#      main = "Rainy season intensity  (m/day) by water year type")
# plot(rain_season_table$wat_yr_type, rain_season_table$duration_days,
#      main = "Rainy season duration  (days) by water year type")

# Store table (file name will change for different bounds)
# table_name = paste0("rainy_season_stats_def_",rainy_season_bounds[1], "_", rainy_season_bounds[2], ".csv")
# setwd(pdf_dir)
# write.csv(rain_season_table, table_name)


# 2. Rain event characteristics ------------------------------------------------

#Functions
precip_interval_tables = function(P, rain_day_threshold = 0, return_daily = TRUE){
  #Function is BINARY. Days must be assigned either "wet/rainy" or "dry" designations.
  # for P, assume dataframe with 2 columns: 'precip_m' (daily), and 'date' format "%d/%m/%Y"
  # for rain day threshold, assume one numerical value above which it counts as "wet"
  # OR a daily time series of thresholds
  
  ndays = length(P$date)
  #Assign threshold value (if it's a single value)
  if(length(rain_day_threshold)==1){threshold = rain_day_threshold}
  
  #Initialize counters and identifier columns for rain periods, dry periods
  P$rain_event_id = NA
  P$dry_period_id = NA
  rain_counter = 1
  dry_counter = 1
  
  for(i in 1:(ndays-1)){
    #Assign threshold value (if it's a timeseries)
    if(length(rain_day_threshold)>1){threshold = rain_day_threshold[i]} 
    
    #iterate over days and identify them with a rain event id or a dry period id. rain events or dry events are continuous days above or below the threshold.  
    if(P$precip_m[i] > threshold){ #if it is a rainy day
      P$rain_event_id[i] = rain_counter #tag that day with a rain event id
      if(P$precip_m[i+1] <= threshold) { #... and if the next day is not rainy
        rain_counter = rain_counter + 1 #increment the rain counter for the next rain event
      }
    } 
    if(P$precip_m[i] <= threshold){ #otherwise, if it is a dry day
      P$dry_period_id[i] = dry_counter #tag that day with a dry period id
      if(P$precip_m[i+1] > threshold) { #... and if the next day is not dry
        dry_counter = dry_counter + 1 #increment the dry counter for the next dry period
      }
    }
  }
  #last day
  if(P$precip_m[ndays] > threshold){ P$rain_event_id[ndays] = rain_counter
  } else {P$dry_period_id[ndays] = dry_counter} 
  
  #Initialize rain event stats table. historical storm_counter = 840(839?)
  nrain = max(P$rain_event_id, na.rm=T); ndry = max(P$dry_period_id, na.rm=T)
  rain_events = as.data.frame(1:nrain); colnames(rain_events) = "rain_event_id"
  #Aggregate
  depth_m = aggregate(P$precip_m, by = list(P$rain_event_id), FUN = sum)
  start_day = aggregate(P$date, by = list(P$rain_event_id), FUN = min)
  end_day = aggregate(P$date, by = list(P$rain_event_id), FUN = max)
  #Build table
  rain_events$depth_m = depth_m$x[match(rain_events$rain_event_id, depth_m$Group.1)]
  rain_events$start_day = start_day$x[match(rain_events$rain_event_id, start_day$Group.1)]
  rain_events$end_day = end_day$x[match(rain_events$rain_event_id, end_day$Group.1)]
  #Calculate other stats from aggregated stats
  rain_events$duration_days  = rain_events$end_day - rain_events$start_day + 1
  rain_events$start_mo = as.numeric(strftime(rain_events$start_day, "%m"))
  rain_events$start_yr = as.numeric(strftime(rain_events$start_day, "%Y"))
  rain_events$start_wy = rain_events$start_yr
  rain_events$start_wy[rain_events$start_mo > 9] = rain_events$start_yr[rain_events$start_mo > 9]+1
  
  #Initialize dry period stats table. historical rain_event_counter = 840
  dry_periods = data.frame(1:ndry); colnames(dry_periods) = "dry_period_id"
  #Aggregate
  start_day = aggregate(P$date, by = list(P$dry_period_id), FUN = min)
  end_day = aggregate(P$date, by = list(P$dry_period_id), FUN = max)
  #Build table
  dry_periods$start_day = start_day$x[match(dry_periods$dry_period_id, start_day$Group.1)]
  dry_periods$end_day = end_day$x[match(dry_periods$dry_period_id, end_day$Group.1)]
  #Calculate other stats from aggregated stats
  dry_periods$duration_days  = dry_periods$end_day - dry_periods$start_day + 1
  dry_periods$start_mo = as.numeric(strftime(dry_periods$start_day, "%m"))
  dry_periods$start_yr = as.numeric(strftime(dry_periods$start_day, "%Y"))
  dry_periods$start_wy = dry_periods$start_yr
  dry_periods$start_wy[dry_periods$start_mo > 9] = dry_periods$start_yr[dry_periods$start_mo > 9]+1
  
  if(return_daily == TRUE){return(list(P, rain_events, dry_periods))} else {
  return(list(rain_events, dry_periods))}
  
}

# Step 2a. Identify rain events and dry periods
rain_day_threshold = 0 # Any rain counts as a rainy day
# Alternative: define as, e.g., 20% of ref ET (used elsewhere in the SWBM)
interval_tables = precip_interval_tables(select(P, date, precip_m),
                                         rain_day_threshold)
rain_stats = interval_tables[[1]]
dry_stats = interval_tables[[2]]

# Define return interval as the windown between start dates of rain events
# Probably need a different term for this

#Step 2b. Calculate frequency in water years and in water year rainy seasons
# setwd(pdf_dir)
# rsea = read.csv("rainy_season_stats_def_0.1_0.9.csv") #read in rainy season stats table

additional_visualization = "nah" #Observe table-munging and question-asking

if(additional_visualization == "sure"){
  #Calculate dates of start and end of rainy season for comparison purposes
  rsea$date_start = as.Date(paste0( (rsea$wat_yr-1),"-10-01"))+ (rsea$wy_day_start - 1)
  rsea$date_end = as.Date(paste0( (rsea$wat_yr-1),"-10-01"))+ (rsea$wy_day_end - 1)
  #Append rainy season start and end dates to each rain event
  rain_stats$rsea_start = rsea$date_start[match(rain_stats$start_wy, rsea$wat_yr)]
  rain_stats$rsea_end = rsea$date_end[match(rain_stats$start_wy, rsea$wat_yr)]
  #Flag whether each rain event occurred during the rainy season for that water year
  rain_stats$rain_in_rainy_season = rain_stats$start_day >= rain_stats$rsea_start &
    rain_stats$start_day <= rain_stats$rsea_end #If the rain event starts inside the rainy season, it counts
  
  #Calculate frequency of rain events over the whole water year
  num_rain_events = aggregate(rain_stats$rain_event_id, by = list(rain_stats$start_wy), FUN = length)
  #Append to rainy season table
  rsea$wy_num_ev = num_rain_events$x
  rsea$freq_wy_ev_per_day = num_rain_events$x / 365
  
  #Calculate frequency of rain events during the rainy season
  rain_stats_in_rainy_season = rain_stats[rain_stats$rain_in_rainy_season == TRUE,]
  rsirs = rain_stats_in_rainy_season
  num_rain_events = aggregate(rsirs$rain_event_id, by = list(rsirs$start_wy), FUN = length)
  #Append to rainy season table
  rsea$rsea_num_ev = num_rain_events$x
  rsea$freq_rsea_ev_per_day = num_rain_events$x / rsea$duration_days
  
  #Ad-hoc seasonality index for rain events: frequency during rainy season / frequency over whole year
  rsea$seasonality_index = rsea$freq_rsea_ev_per_day / rsea$freq_wy_ev_per_day

  
#Step 2c. Check for relationships between various precip metrics. 

#Relationship between seasonality and number of events? ... no. Clearly a not interpretable metric.
plot(rsea$rsea_num_ev, rsea$seasonality_index)

# Relationship between total precip and seasonality? ... no.
plot(rsea$total_precip_m, rsea$seasonality_index)

#Relationship between seasonality and water year type? ... not really. 
# Critical has highest seasonality by far. And nearly matches lowest.
plot(as.factor(rsea$wat_yr_type), rsea$seasonality_index)
# max seasonality is WY 1994 (critically dry). Min seasonality is WY 2004 (below normal). 

plot(P$date[P$wy == 1994], P$precip_m[P$wy == 1994], type = "l", col = "pink", ylim = c(0, 0.03))
plot(P$date[P$wy == 2004], P$precip_m[P$wy == 2004], type = "l", col = "blue", ylim = c(0, 0.03))


# Step 2d. Plot some features of the storm events and rainy seasons. 

# Visualize distribution of interval between storms ( future work: during rainy season only?)
hist(log(as.numeric(dry_stats$duration_days)))
mean_dry_duration_by_wy = aggregate(dry_stats$duration_days, 
                                    by = list(dry_stats$start_wy), FUN = mean)
# Visualize distribution of storm depth
hist(log(rain_stats$depth_m)) #lognormal
mean_depth_by_wy = aggregate(rain_stats$depth_m, by = list(rain_stats$start_wy), FUN = mean)

# Visualize distribution of storm duration
hist(log(as.numeric(rain_stats$duration_days)))
# super-skewed; still a stair-step after logging

# Visualize distribution of storm intensity (depth / duration)
# hist(log(rain_stats$depth_m)) 
# hist(log(as.numeric(rain_stats$duration_days)))
# hist(rain_stats$depth_m / as.numeric(rain_stats$duration_days))
hist(log(rain_stats$depth_m / as.numeric(rain_stats$duration_days)))

#Future work
# hysteresis effect? calculate whether more rain came at beginning or end of rainy season?
# Antecedent Moisture Condition effect? calculate what fraction of rain fell when 5-day antecedent was X
#
}


# 3. Precip record generation ------------------------------------------------

check_sums = function(P1,P2) {
  one_over_two = sum(P1$precip_m) / sum(P2$precip_m)
  print(paste("Precip record 1 is", one_over_two, "times greater than precip record 2"))
}

# Create 3 altered precip records: Scenarios A, B, and C. 
## Scenario A. Increased storm intensity 
## (decision variable: number of large storms) 

generate_scenario_a=function(P, num_large_storms){
  #Assumes P = dataframe with "date" and "precip_m" columns (date- and numeric-type respectively)
  
  interval_tables = precip_interval_tables(select(P, date, precip_m),
                                           rain_day_threshold, return_daily = TRUE)
  P_rain_dry = interval_tables[[1]]; Prd = P_rain_dry
  #Create Scenario A daily precip values column and assign default 0s
  Prd$sca = 0
  rain_stats = interval_tables[[2]]
  dry_stats = interval_tables[[3]]
  
  rsea = rainy_season_table(select(P, date, precip_m), rainy_season_bounds)
  
  wys = rsea$wat_yr
  
  for(wy in wys){
    rain_wy = rain_stats[rain_stats$start_wy == wy,]
    largest = rain_wy$rain_event_id[order(rain_wy$depth_m, decreasing = TRUE)][1:num_large_storms]
    
    #assign Scenario A daily values for the 10 largest storms
    days_in_largest_storms = Prd$rain_event_id %in% largest
    # Prd$sca[days_in_largest_storms] = Prd$precip_m[days_in_largest_storms]
    
    #Calculate rain volumes in large and small storms
    large_rain_volume = sum(rain_wy$depth_m[rain_wy$rain_event_id %in% largest])
    small_rain_volume = sum(rain_wy$depth_m[!(rain_wy$rain_event_id %in% largest)])
    #Distribute small storm volume to larger storms
    storm_day_added_volume = small_rain_volume / sum(days_in_largest_storms)
    Prd$sca[days_in_largest_storms] = Prd$precip_m[days_in_largest_storms] + storm_day_added_volume
  }
  return(Prd)
}

#Storm threshold and rainy season bounds
rain_day_threshold = 0 # Any rain counts as a rainy day
rainy_season_bounds = c(0.1, 0.9)

if(dev_mode){
  #Decide number of large storms and generate Scenario A records
  num_large_storms = 5
  scenario_folder_name = "pvar_a05"
  
  P_sca = generate_scenario_a(select(ppt_hist, date, precip_m), num_large_storms)
  # P_sca = select(P_sca, date, sca); colnames(P_sca) = c("date", "precip_m")
  # check_sums(ppt_hist, P_sca)
  
  ##Plot time series of Scenario A records by water year
  ## Track number of large storms in the column name and plot titles.
  # scenario_name = paste0("sca_",num_large_storms,"_large")
  # colnames(P_sca)[colnames(P_sca) == "sca"] = scenario_name
  # plot_by_wy_type(select(P_sca, date, scenario_name))
  
  #Write precip to a text file for SWMB run.
  sca_for_txt = select(P_sca, sca, date)
  sca_for_txt$date = paste0(strftime(P_sca$date, "%d"), "/",
                            strftime(P_sca$date, "%m"), "/",
                            strftime(P_sca$date, "%Y"))
  
  txt_file_name=paste0("sca_precip_",num_large_storms,"large.txt")
  write.table(sca_for_txt, file.path(swbm_dir, scenario_folder_name, txt_file_name), 
              col.names=FALSE, row.names=FALSE, sep = "/t", quote = FALSE)
}

generate_scenario_a_extreme_days=function(P, 
                                          big_little_divide = 0.95, 
                                          storm_increase_fraction=0.07){
  #Assumes P = dataframe with "date" and "precip_m" columns (date- and numeric-type respectively)

  # big_little_divide = 0.95 # what fraction of days with rain count as "small" (water taken from them) vs "large" (water added to them)?
  # storm_increase_fraction = 0.07 # the amount of water (as a fraction of the daily total) that will get added to each large day
  
  interval_tables = precip_interval_tables(P[,c("date", "precip_m")],
                                           rain_day_threshold, return_daily = TRUE)
  P_rain_dry = interval_tables[[1]]; Prd = P_rain_dry
  #Create Scenario A daily precip values column and assign default 0s
  Prd$sca_ext = 0
  #Add water year attribute
  Prd$wy = year(Prd$date); Prd$wy[month(Prd$date)>9] = Prd$wy[month(Prd$date)>9]+1
  
  rain_stats = interval_tables[[2]]
  dry_stats = interval_tables[[3]]
  rsea = rainy_season_table(P[,c("date", "precip_m")], rainy_season_bounds)
  wys = rsea$wat_yr

  for(wy in wys){
    rain_wy = rain_stats[rain_stats$start_wy == wy,]
    
    #assume that what we are calculating is:
    # - the amount of rainfall arriving on days with > threshold amount ("extreme" days)
    Prd$wy = year(Prd$date); Prd$wy[month(Prd$date)>9] = Prd$wy[month(Prd$date)>9]+1
    precip_wy = Prd$precip_m[Prd$wy == wy] 

    precip_wy = Prd$precip_m[Prd$wy == wy]     #subset for a single water year
    #Find the precip value such that XX% (e.g. 95%) of all daily rainfall amounts have less than it
    precip_wy_sorted = precip_wy[order(precip_wy)] 
    bld_index = round(length(precip_wy_sorted) * big_little_divide) # 
    bld_value = precip_wy_sorted[bld_index]
    large_day_selector = precip_wy >= bld_value
    
    #move water from small days to large days
    #total amount of water needed to augment the large days:
    tot_water_large_days = sum(precip_wy[large_day_selector])
    water_needed = tot_water_large_days * storm_increase_fraction

    # take it from the small days proportionally
    small_day_selector = precip_wy< bld_value & precip_wy >0
    tot_water_small_days = sum(precip_wy[small_day_selector])
    small_day_proportions = precip_wy[small_day_selector]/tot_water_small_days
    water_subtractions = water_needed*small_day_proportions
    
    new_small_day_precip = precip_wy[small_day_selector] - water_subtractions
    new_large_day_precip = precip_wy[large_day_selector]*(1+storm_increase_fraction)
    
    #initialize and build new water year precip record
    new_precip_wy = precip_wy
    new_precip_wy[small_day_selector] = new_small_day_precip
    new_precip_wy[large_day_selector] = new_large_day_precip
    
    # #testing - can we build a distribution of rain in each rain year? (neglecting timing)
    # #answer: not with log-normal. Next: try gamma, keeping all the 0's. 
    # Prd$wy = year(Prd$date); Prd$wy[month(Prd$date)>9] = Prd$wy[month(Prd$date)>9]+1
    # precip_wy_mm = Prd$precip_m[Prd$wy == wy] * 1000
    # hist(precip_wy_mm, col = "wheat",  breaks = seq(0,20,1), freq = F,
    #      #xlim = c(-2, 2), ylim = c(0, 1),
    #      xlab = "daily precipitation (mm)",
    #      main = paste0("Non-zero daily precipitation in ", wy))
    # grid()
    # lambda = 2
    # x = seq(-0,20,0.5)
    # lines(x, lambda*exp(-lambda*x), lwd = 1.5)
    
    Prd$sca_ext[Prd$wy == wy] = new_precip_wy  #convert back to meters for consistency
    
  }
  return(Prd)
}


#Storm threshold and rainy season bounds
rain_day_threshold = 0 # Any rain counts as a rainy day
rainy_season_bounds = c(0.1, 0.9)

if(dev_mode){ 
  #Decide number of large storms and generate Scenario A records
  scenario_folder_name = "pvar_a_extr_95_07"
  
  P_sca_ext = generate_scenario_a_extreme_days(ppt_hist[,c("date", "precip_m")], 
                                           big_little_divide = 0.95, storm_increase_fraction=0.07)
  # P_sca = P_sca[,c("date", "sca_ext")]; colnames(P_sca) = c("date", "precip_m")
  # check_sums(ppt_hist, P_sca)
  
  #Visualize
  for(plot_wy in 1991:2018){
    # plot_wy = 1991
    xlims = as.Date(c(paste0(plot_wy-1, "-10-01"), paste0(plot_wy,"-09-30")))
    # xlims = as.Date(c(paste0(plot_wy-1, "-11-01"), paste0(plot_wy,"-04-30")))
    
    plot(P_sca$date, P_sca$precip_m, type = "l", lwd=3, col = "darkgoldenrod",
         xlim = xlims, main = paste("Water year", plot_wy))
    lines(P_sca$date, P_sca$sca_ext, type = "l", lwd=1, col = "black")
    legend(x="topright", lwd = c(3,1), col = c("darkgoldenrod", "black"),
           legend = c("Historical Rainfall", "Altered Rainfall"))
  }
  
  #Write precip to a text file for SWBM run.
  sca_for_txt = P_sca[,c("sca_ext", "date")]
  sca_for_txt$date = paste0(strftime(P_sca$date, "%d"), "/",
                            strftime(P_sca$date, "%m"), "/",
                            strftime(P_sca$date, "%Y"))
  
  paste0("sca_extreme_",big_little_divide, "_",storm_increase_fraction)
  txt_file_name=paste0("sca_extreme_",big_little_divide, "_",storm_increase_fraction,".txt")
  write.table(sca_for_txt, file.path(swbm_dir, scenario_folder_name, txt_file_name), 
              col.names=FALSE, row.names=FALSE, sep = "/t", quote = FALSE)
}

calculate_fraction_precip_on_extreme_days=function(percentile_threshold = 95,
                                                   hist_record,
                                                   comp_record){
  #Assumes historical and comparison records with column "Date" and "precip_m" (in meters)
  # Assumes both records start Oct 1st and end Sept 30 
  
  # #dev mode: source SVIHM_input_analyses
  # hist_record = P #read in from above
  # comp_record = P_sca[,c("date","sca_ext")]; colnames(comp_record)=c("date","precip_m")
  
  #calculate precip value of the Xth percentile. Sort, find 95th index, and then find value.
  hist_record_sorted = hist_record[order(hist_record$precip_m),]
  threshold_index = round(dim(hist_record)[1] * (percentile_threshold/100))
  threshold_value = hist_record_sorted$precip_m[threshold_index]
  
  #For each water year, calculate proportion of rain volume
  #falling on days with rain # X number threshold. 
  # Average over all water years
  comp_record$wy = year(comp_record$date)
  comp_record$wy[month(comp_record$date)>9] = comp_record$wy[month(comp_record$date)>9]+1
  wys = unique(comp_record$wy)
  #initialize table
  prop_on_ext_days=data.frame("wy" = wys); prop_on_ext_days$proportion = NA
  for(i in 1:length(wys)){
    wy = wys[i]
    comp_wy = comp_record[comp_record$wy == wy,]
    proportion = sum(comp_wy$precip_m[comp_wy$precip_m > threshold_value]) / sum(comp_wy$precip_m)
    prop_on_ext_days$proportion[i] = proportion
  }
  return(prop_on_ext_days)
}

if(dev_mode){
  
  #read in P from above and generate P_sca extreme days
  # P_sca = P_sca[,c("date","sca_ext")]; colnames(P_sca)=c("date","precip_m")
  
  
  #Geeta analysis Nov 2019:
  #Calculate proportion of rain falling on extreme days for historical and altered record
  hist_prop = calculate_fraction_precip_on_extreme_days(hist_record = P, 
                                                        comp_record = P, 
                                                        percentile_threshold = 95)
  sca_ext_prop = calculate_fraction_precip_on_extreme_days(hist_record = P, comp_record = P_sca, percentile_threshold = 95)
  proportion_table = merge(hist_prop, sca_ext_prop, by="wy")
  colnames(proportion_table) = c("wy", "historical", "ext_days_95_07")
  
  plot(proportion_table$wy, proportion_table$historical, type = "l", col = "blue")
  lines(proportion_table$wy, proportion_table$ext_days_95_07, col = "red")
  
  proportion_table$diff = proportion_table$ext_days_95_07-proportion_table$historical
  
  mean(proportion_table$diff)
}


## Scenario B. Shorter wet season
## Decision variable: fraction of historical wet season length) 

generate_scenario_b = function(P, fraction_season_length){
  #Assumes P = dataframe with "date" and "precip_m" columns (date- and numeric-type respectively)
  
  interval_tables = precip_interval_tables(select(P, date, precip_m),
                                           rain_day_threshold, return_daily = TRUE)
  P_rain_dry = interval_tables[[1]]; Prd = P_rain_dry
  rain_stats = interval_tables[[2]]
  dry_stats = interval_tables[[3]]

  #Initialize Scenario B daily precip values and new start rain event start date
  Prd$scb = 0
  rain_stats$scb_start = NA
  
  #Add dates in day-of-water-year terms
  Prd$wy = as.numeric(strftime(Prd$date, "%Y"))
  Prd$wy[month(Prd$date) > 9] = 1 + as.numeric(strftime(Prd$date[month(Prd$date) > 9], "%Y"))
  Prd$day_of_wy = 1 + as.numeric(Prd$date - as.Date(paste0(Prd$wy - 1, "-10-01")) ) # plus one to avoid zero-indexing
  rain_stats$start_ev_day_of_wy = 1+ as.numeric(rain_stats$start_day - 
                                               as.Date(paste0(rain_stats$start_wy - 1, "-10-01")) )
  #Using "start" in rain_stats to denote we're going by the *start* date of a rain event.
    
  rsea = rainy_season_table(select(P, date, precip_m), rainy_season_bounds)
  
  wys = rsea$wat_yr 
  
  #Operate on each water year
  for(i in 1:length(wys)){
    wy = wys[i]
    #Subset rain event stats for water year
    rain_stats_wy = rain_stats[rain_stats$start_wy == wy,]
    #Declare day-of-water-year characteristics of original precip record
    rs_start = rsea$wy_day_start[i]
    rs_end =  rsea$wy_day_end[i]
    rs_midpoint = sum(rs_start, rs_end)/2
    rs_duration = rs_end - rs_start + 1
    
    #Calculate new rainy season characteristics in day-of-water year-terms
    rs_duration_scb = round(rs_duration * fraction_season_length) #round to integer value
    rs_start_scb = rs_midpoint - rs_duration_scb/2
    rs_end_scb = rs_midpoint + rs_duration_scb/2
    
    #calculate the index, in the daily record, where this water year starts
    rsea_P_start_index = which(Prd$date == as.Date(paste0(wy - 1, "-10-01")))

    #Operate on each rain event
    #Re-assign rain events to proportional points in the new rainy season
    # New location proportional to distance from  midpoint. Overlap is OK.
    for(j in 1:dim(rain_stats_wy)[1]){ 
      #Calculate new start date for each rain event
      old_start = rain_stats_wy$start_ev_day_of_wy[j]
      mid_diff = rs_midpoint - old_start
      new_start = round(rs_midpoint - (mid_diff * fraction_season_length)) #round to integer
      #Store new_start date
      rain_stats_wy$scb_start[j] = new_start
      
      # Take the days of rain associated with that rain event and add them to the 
      # corresponding days after the new calculated start date
      scb_start_index = rsea_P_start_index + new_start - 1
      scb_end_index = scb_start_index + as.numeric(rain_stats_wy$duration_days[j]) - 1
      
      Prd$scb[scb_start_index : scb_end_index] = #the Scenario B record starting from the new start date
        Prd$scb[scb_start_index : scb_end_index] + # is the existing Scenario B record, PLUS
        Prd$precip_m[!is.na(Prd$rain_event_id) & #  the historical precip record (for days that are wet)
                       Prd$rain_event_id == rain_stats_wy$rain_event_id[j]] # for this rain event id
    }
    #Store new rain event start dates in overall rain_stats table
    rain_stats$scb_start[rain_stats$start_wy == wy] = rain_stats_wy$scb_start
  }
  
  return(list(rain_stats, Prd))
}

if(dev_mode){
  rain_day_threshold = 0 # Any rain counts as a rainy day
  rainy_season_bounds = c(0.1, 0.9)
  fraction_season_length = 0.9 #try 90, 80, 70
  scenario_folder_name = "pvar_b90"
  
  scb_tables = generate_scenario_b(select(ppt_hist, date, precip_m), fraction_season_length)
  
  rain_stats_scb = scb_tables[[1]]
  P_scb = scb_tables[[2]]
  
  check_sums(ppt_hist, P_scb)

  #Write precip to a text file for SWMB run.
  scb_for_txt = select(P_scb, scb, date)
  scb_for_txt$date = paste0(strftime(P_scb$date, "%d"), "/",
                            strftime(P_scb$date, "%m"), "/",
                            strftime(P_scb$date, "%Y"))
  
  txt_file_name = paste0("scb_precip_",fraction_season_length,"_season.txt")
  write.table(scb_for_txt, file.path(swbm_dir, scenario_folder_name, txt_file_name), 
              col.names=FALSE, row.names=FALSE, sep = "/t", quote = FALSE)
}

## Scenario C. More whiplash
## Decision variables: 1) percent more extreme, 2) "dry" and "wet" year thresholds (% of max annual precip)
## Increased "whiplash" year. Drier dries, wetter wet years. (decision variable: % more extreme) scenario c (scc)
### Divide into wet and dry by sac valley index, or by water year totals?
### Water year totals

generate_scenario_c = function(P, S, percent_drier, dry_wet_thresholds){
  #Assumes P = dataframe with "date" and "precip_m" columns (date- and numeric-type respectively)
  #Assumes S = dataframe with "date" (date-type) and columns for each tributary
  
  #Generate rainy season table and classify wet and dry years per threshold inputs
  rsea = rainy_season_table(select(P, date, precip_m), rainy_season_bounds)
  dw_thr_m = mean(rsea$total_precip_m) * dry_wet_thresholds
  rsea$dry_or_wet = NA
  rsea$dry_or_wet[rsea$total_precip_m < dw_thr_m[1]] = "dry"
  rsea$dry_or_wet[rsea$total_precip_m > dw_thr_m[2]] = "wet"
  #Make lists
  wet_years = rsea$wat_yr[rsea$dry_or_wet == "wet"]
  dry_years = rsea$wat_yr[rsea$dry_or_wet == "dry"]
  
  #Initialize Scenario C daily precip values and new start rain event start date
  P$scc = P$precip_m # for years not defined as wet or dry, daily values will be unchanged
  
  #Add water year info
  P$wy = as.numeric(strftime(P$date, "%Y"))
  P$wy[month(P$date) > 9] = 1 + as.numeric(strftime(P$date[month(P$date) > 9], "%Y"))
  
  #Create dry year record - reduce daily rainfall by fraction specified
  P$scc[P$wy %in% dry_years] = P$precip_m[P$wy %in% dry_years] * (1 - percent_drier/100)
  
  # Create wet year record - calculate total volume harvested from making dry years drier
  volume_harvest = (percent_drier/100) * sum(rsea$total_precip_m[rsea$dry_or_wet == "dry"])
  # Distribute harvested volume evenly across all rainy days in wet years. 
  # (This means years with more wet days get more.)
  wet_days_in_wet_years_index = P$wy %in% wet_years & P$precip_m > 0
  wdiwy = wet_days_in_wet_years_index #short name
  extra_volume_per_wet_day = volume_harvest / sum(wdiwy)
  P$scc[wdiwy] = P$precip_m[wdiwy] + extra_volume_per_wet_day
  
  ### Calculate streamflow. Percent increase or decrease tracks with total precip.
  # Calculate change in total precip by water year
  # Using the handy rainy_season_table function, yeah
  P_scc = select(P, date, scc); colnames(P_scc) = c("date", "precip_m")
  rsea_scc = rainy_season_table(P_scc, rs_bounds = c(0.1, 0.9))
  rsea$total_precip_multiplier = rsea_scc$total_precip_m / rsea$total_precip_m

  #Assign water years to streamflow  
  S$wy = as.numeric(strftime(S$date, "%Y"))
  S$wy[month(S$date) > 9] = 1 + as.numeric(strftime(S$date[month(S$date) > 9], "%Y"))
  #Attach precip multiplier to each month accocrding to water year
  S$total_precip_multiplier = rsea$total_precip_multiplier[match(S$wy, rsea$wat_yr)]
  
  #Increment tributary flow values with the precip multiplier for the relevant water year
  flow_columns = !(colnames(S) %in% c("date", "wy", "total_precip_multiplier"))
  S[, flow_columns] = S[, flow_columns] * S$total_precip_multiplier

  return(list(P, S))
}

if(dev_mode){
  rain_day_threshold = 0 # Any rain counts as a rainy day
  rainy_season_bounds = c(0.1, 0.9) #fraction of total precip that defines wet season
  dry_wet_thresholds = c(1.0, 1.0) # mult. of mean annual precip. dry must <= wet threshold.
  percent_drier = 30 #try 10, 20, 30
  scenario_folder_name = "pvar_c30"
  
  P = select(ppt_hist, date, precip_m)
  S = stm_hist
  
  scc = generate_scenario_c( P, S, percent_drier = 10, dry_wet_thresholds)
  
  #Write precip to a text file for SWMB run.
  P_scc = scc[[1]]
  scc_for_txt = select(P_scc, scc, date)
  scc_for_txt$date = paste0(strftime(P_scc$date, "%d"), "/",
                            strftime(P_scc$date, "%m"), "/",
                            strftime(P_scc$date, "%Y"))
  txt_file_name = paste0("scc_precip", percent_drier,".txt")
  write.table(scc_for_txt, file.path(swbm_dir, scenario_folder_name, txt_file_name), 
              col.names=FALSE, row.names=FALSE, sep = " ", quote = FALSE)
  
  S_scc = scc[[2]]
  S_scc$wy = NULL; S_scc$total_precip_multiplier = NULL; colnames(S_scc)[1] = "Month"
  
  txt_file_name = paste0("scc_streamflow_input", percent_drier,".txt")
  write.table(S_scc, file.path(swbm_dir, scenario_folder_name, txt_file_name), 
              col.names=TRUE, row.names=FALSE, sep = " ", quote = FALSE)
  
# Get some bare-bones analyses of new precip records

# Run new records through SWBM
# make some plots and tables
# Write the damn thing



# Precip and streamflow and RefET plots ---------------------------------------------
  #make table of plotting parameters
  #for GRA sept 2019
  scenario = c("hist", "sca", "scb", "scc")
  scenario_descrip = c("Historical Precip","Scenario A (higher storm intensity)", 
                       "Scenario B (shorter rainy season)", "Scenario C (wetter wet / drier dry years)")
  sc_color = c("gray25", "red", "blue", "green4")
  sc_plotting_table = data.frame(scenario, scenario_descrip, sc_color, stringsAsFactors = FALSE)
  plot_precip_compare = function(sc1, sc2, sc1df, sc2df){
    col1 = sc_plotting_table$sc_color[sc_plotting_table$scenario == sc1]
    col2 = sc_plotting_table$sc_color[sc_plotting_table$scenario == sc2]
    leglab1 = sc_plotting_table$scenario_descrip[sc_plotting_table$scenario == sc1]
    leglab2 = sc_plotting_table$scenario_descrip[sc_plotting_table$scenario == sc2]
    
    for(i in 1:21){
      wy = 1990 + i
      x_limits = as.Date(c(paste0(wy-1,"-10-01"), paste0(wy,"-09-30")))
      # plot(sc2df$date, sc2df$precip_m, type = "l", lwd = 1, col = col2, 
      #      xlim = x_limits, main = paste("Water Year", wy), xlab = "Date", ylab = "Daily precipitation (in)")#, ylim = c(0, 0.04))
      # lines(sc1df$date, sc1df$precip_m, type = "l", lwd = 2, col = col1, xlim = x_limits)
      plot(sc1df$date, sc1df$precip_m, type = "l", lwd = 2.5, col = col1, xlim = x_limits)
      lines(sc2df$date, sc2df$precip_m, type = "l", lwd = 1, col = col2, 
           xlim = x_limits, main = paste("Water Year", wy), xlab = "Date", ylab = "Daily precipitation (in)")#, ylim = c(0, 0.04))
      legend(x = "topright", lwd= c(2,2), col = c(col1, col2), legend = c(leglab1, leglab2))
    }
  }
  
  # plot_precip_compare("hist", "scb", ppt_hist, P_scb)
  # plot_precip_compare("hist", "scc", ppt_hist, P_scc)

  
  
  # For Geeta Nov 2019
  #Generate scenario A, extreme days, 95% divider, 7% increase from function above
  scenario = c("hist", "sca_ext")
  scenario_descrip = c("Historical Precip","Altered Precip (7% more on extreme days)")
  sc_color = c("gray25", "red")
  sc_plotting_table = data.frame(scenario, scenario_descrip, sc_color, stringsAsFactors = FALSE)
  
  
  plot_precip_compare_points = function(sc1, sc2, sc1df, sc2df, plot_wy=NA){
    col1 = sc_plotting_table$sc_color[sc_plotting_table$scenario == sc1]
    col2 = sc_plotting_table$sc_color[sc_plotting_table$scenario == sc2]
    leglab1 = sc_plotting_table$scenario_descrip[sc_plotting_table$scenario == sc1]
    leglab2 = sc_plotting_table$scenario_descrip[sc_plotting_table$scenario == sc2]
    
    if(!is.na(plot_wy)){wys = plot_wy} #if water year(s) specified, make the graph only for that/those water years
    
    for(wy in wys){
      x_limits = as.Date(c(paste0(wy-1,"-10-01"), paste0(wy,"-09-30")))
      plot(sc1df$date, sc1df$precip_m*1000, pch=1, type="o", col = col1, xlim = x_limits, 
           xlab= paste("Month in water year",wy), ylab = "Daily Precipitation (mm)")
      points(sc2df$date, sc2df$precip_m*1000, pch=4, lwd = 1, col = col2, 
            xlim = x_limits, main = paste("Water Year", wy), xlab = "Date", ylab = "Daily precipitation (in)")#, ylim = c(0, 0.04))
      grid(lty = 2, col = "darkgray")
      abline(h=0, col="black" )
      if(is.na(plot_wy)){
        legend(x = "topright", pch= c(1,4), col = c(col1, col2), lwd=c(1,NA),
             legend = c(leglab1, leglab2), bg="white")
      }
    }
  }
  
  alteration_fig_dir = "C:/Users/Claire/Documents/UCD/Presentations or Talks or Workshops or mini-projects/2019.06-12 Geeta Precip Alteration project"
  # pdf(file.path(alteration_fig_dir, "precip water years.pdf"), width = 8.5, height = 11/2)
  png(file.path(alteration_fig_dir, "precip water years.png"), width = 7.5, height = 6, units = "in", res = 300)
  par(mfrow = c(2,2))
  plot_precip_compare_points("hist", "sca_ext", P, P_sca, plot_wy = c(2014, 2017, 2010, 2015))
  dev.off()
  
  # Tables for Latex --------------------------------------------------------
  
  
  #Rainy season stats
  rs_for_latex = rainy_season_table(ppt_hist, c(0.1, 0.9))
  rs_for_latex$start_date = rs_for_latex$wy_day_start + 
    as.Date(paste0(rs_for_latex$wat_yr - 1, "-10-01"))
  rs_for_latex$end_date = rs_for_latex$wy_day_end + 
    as.Date(paste0(rs_for_latex$wat_yr - 1, "-10-01"))
  rs_for_latex$total_precip_m = round(rs_for_latex$total_precip_m,3)
  # rs_for_latex$intensity = round(rs_for_latex$intensity,4)
  rs_for_latex$intensity = NULL
  
  prd_tables = precip_interval_tables(ppt_hist)
  rain_stats = prd_tables[[2]]
  rain_events_per_wy = aggregate(rain_stats$rain_event_id, by = list(rain_stats$start_wy), FUN = length)
  rs_for_latex$num_rain_ev = rain_events_per_wy$x
  
  rs_for_txt = select(rs_for_latex, wat_yr, total_precip_m, start_date, end_date, duration_days, num_rain_ev, wat_yr_type)
  rs_for_txt$latex_end = "\\"
  
  setwd(pdf_dir)
  write.table(rs_for_txt, "rainy_season_for_latex.txt",
              col.names = TRUE, row.names = FALSE, sep = " & ", quote = FALSE)
  
  
  
  #Rainy season stats plots
  rs = rainy_season_table(ppt_hist, c(0.1, 0.9))
  rs$intensity = rain_season_table$total_precip_m / rain_season_table$duration_days 
  rs$num_rain_ev = rain_events_per_wy$x
  # #add water year type to table - did this in the function
  wy_type$yr_type = factor(wy_type$yr_type, levels = c("W","AN","BN","D","C"))
  rs$wat_yr_type = wy_type$yr_type[match(rs$wat_yr, wy_type$wat_yr)]
  
  
  ## What's the distribution of rainy season intensities?
  # hist(rs$intensity * 1000, breaks=(seq(0, 6, 0.5)), col = "lightblue",
  #      xlab = "Millimeters per day", main = "Rainy season intensity - Water Years 1991-2011")
  setwd(pdf_dir)
  pdf("hist_rain_plots.pdf", 6.5, 4)
  par(mfrow = c(2,3))
  hist(rs$total_precip_m, col = "lightblue", #breaks=(seq(0, 6, 0.5)),
       xlab = "Total annual precip (m)", main = NULL)#"Total Annual Precip - Water Years 1991-2011")
  text(.95,6, "a")
  hist(rs$duration_days, col = "lightblue", #breaks=(seq(0, 6, 0.5)),
       xlab = "Rainy season duration (days)", main =  NULL)#"Rainy Season Duration - Water Years 1991-2011")
  text(250,4, "b")
  hist(rs$num_rain_ev, col = "lightblue", #breaks=(seq(0, 6, 0.5)),
       xlab = "Number of rain events", main =  NULL)#"Number of Rain Events - Water Years 1991-2011")
  text(55,7, "c")
  
  # #Any relationship between water year type and intensity, or duration? make boxplots
  plot(rs$wat_yr_type, rs$total_precip_m, col = "lightblue",
       ylab = "Total annual rainfall (m)")
  text(5.3,.9, "d")
  plot(rs$wat_yr_type, rs$duration_days, col = "lightblue",
       ylab = "Rainy season duration (days)")
  text(0.6,245, "e")
  plot(rs$wat_yr_type, rs$num_rain_ev, col = "lightblue",
       ylab = "Number of rain events")
  text(5.3,49, "f")
  
  dev.off()
  
  #Any relat between num events and total precip? No. Nor with duration.
  # plot(rs$num_rain_ev, rs$total_precip_m, pch = 19, col = "lightblue",
  #      main = "Number of rain events vs Total Precip")
  # plot(rs$num_rain_ev, rs$duration_days, pch = 19, col = "lightblue",
  #      main = "Number of rain events vs rainy season duration")
  
  #Rain event plots
  prd_tables = precip_interval_tables(ppt_hist)
  rain_stats = prd_tables[[2]]
  dry_stats = prd_tables[[3]]
  
  setwd(pdf_dir)
  pdf("rain_event_stats.pdf",6.5, 3)
  par(mfrow = c(1,3))
  hist(log(rain_stats$depth_m,10), 
       xlab = "Log_10 of rainfall depth (m)", col = "lightblue",main = NULL)
  text(-.3, 175, "a")
  hist(as.numeric(rain_stats$duration_days), 
       xlab = "Rain event duration (days)", col = "lightblue",main = NULL)
  text(25, 500, "b")
  hist(as.numeric(dry_stats$duration_days), 
       xlab = "Dry period duration (days)", col = "goldenrod",main = NULL)
  text(75,650, "c")
  dev.off()
  
  # #Table of rain event durations
  rain_dur_for_ltx = aggregate(rain_stats$rain_event_id, by = list(rain_stats$duration_days), FUN = length)
  #how many days had longer duration than x?
  sum(rain_dur_for_ltx$x[as.numeric(rain_dur_for_ltx$Group.1) > 7])
  tot = data.frame(t(c("Total events", dim(rain_stats)[1]))); colnames(tot) = c("Group.1","x");
  rain_dur_for_ltx = rbind(rain_dur_for_ltx, tot)
  rain_dur_for_ltx$latex_end = "\\"
  # setwd(pdf_dir)
  # write.table(rain_dur_for_ltx, "rain_ev_duration_for_latex.txt",
  #             col.names = TRUE, row.names = FALSE, sep = " & ", quote = FALSE)
  
  
  
  # Add num rain events to rainy season table for latex
  rain_events_per_wy = aggregate(rain_stats$rain_event_id, by = list(rain_stats$start_wy), FUN = length)
  rs_for_latex$num_rain_ev = rain_events_per_wy$x
  
  rs_for_txt = select(rs_for_latex, wat_yr, total_precip_m, start_date, end_date, duration_days, num_rain_ev, wat_yr_type)
  rs_for_txt$latex_end = "\\"
  
  setwd(pdf_dir)
  write.table(rs_for_txt, "rainy_season_for_latex.txt",
              col.names = TRUE, row.names = FALSE, sep = " & ", quote = FALSE)
  
}

#Scratchwork

#make a log-normal histogram of daily precip of just days with rain
# log_ppt_wy_mm = log10(precip_wy_mm)
# log_ppt_wy_mm[log_ppt_wy_mm== -Inf] = NA
# par(mfrow=c(2,1))
# hist(log_ppt_wy_mm, col = "lightskyblue", freq = F, breaks = seq(-2,2,0.25),
#      xlim = c(-2, 2), ylim = c(0, 1), xlab = "log10 of daily precipitation (mm)",
#      main = paste0("Non-zero daily precipitation in ", wy))
# grid()
# mu = mean(log_ppt_wy_mm, na.rm=T); sigma = sd(log_ppt_wy_mm, na.rm=T)
# abline(v = mu, lwd = 2, col = "brown")
# abline(v = c(mu-sigma, mu+sigma), col = "goldenrod")
# x = seq(-2,2,0.05)
# lines(x, (1/sqrt(2*pi*sigma^2))*exp(-(x-mu)^2/(2*sigma^2)), lwd = 1.5)
# text(x=-2, y = 1, pos = 4,
#      labels = paste("skew:",round(skewness(log_ppt_wy_mm, na.rm=T),2)))
# text(x=-2, y=0.95, pos = 4,
#      labels = paste("n rainy days:", sum(!is.na(log_ppt_wy_mm))))
# text(x=-2, y=0.9, pos = 4, labels = paste("avg bar height:"))

# ugh. fraction *of rainfall* falling on days > threshold or *of days* with rainfall > threshold?
# -  divided by total rainfall (convert to fraction)
# - over the *entire record*? or calculated individually for each water year?
# - then, later, we will calcuate the FRACTION of annual precip CHANGE in that amount between 
