# Script for post-processing changes to streamflow resulting from implementing different conjunctive use scenarios

# rm(list=ls())

library(ggplot2)
library(lubridate)
library(magrittr)
library(reshape2)
library(dplyr)
library(grid)
library(stringr)
library(tidyr)
#. ############################################################################################
############################              USER INPUT             ############################
#. ############################################################################################

# scenario_ids = c("Basecase","MAR")
# scenario_ids = c("Basecase","ILR")
# scenario_ids = c("MAR","ILR","MAR_ILR")
# scenario_ids = c("flowlims","MAR_ILR","mar_ilr_flowlims")
# scenario_ids = c("MAR_ILR","mar_ilr_flowlims", "irrig_0.8", "irrig_0.9")
# scenario_ids = c("Basecase","irrig_0.8","irrig_0.9")
# scenario_ids = c("Basecase","alf_ir_stop_jul10")


# Scenario ids should correspond to folder names
if(!exists("scenario_ids")){
  scenario_ids = c("basecase",
                   "mar","ilr","mar_ilr","flowlims","mar_ilr_flowlims",
                   "mar_ilr_max_0.003","mar_ilr_max_0.019","mar_ilr_max_0.035",
                   "irrig_0.8","irrig_0.9",
                    "irr_eff_improve_0.1", "irr_eff_improve_0.2", "irr_eff_worse_0.1",
                   "alf_irr_stop_jul10",
                   "alf_irr_stop_aug01","alf_irr_stop_aug01_dry_yrs_only", 
                   "alf_irr_stop_aug15","alf_irr_stop_aug15_dry_yrs_only",
                   "natveg_outside_adj","natveg_outside_adj_dzgwET_only",  
                   "natveg_gwmixed_outside_adj", "natveg_gwmixed_outside_adj_dzgwET_only", 
                   "natveg_inside_adj", "natveg_inside_adj_dzgwET_only",
                   "natveg_gwmixed_inside_adj", "natveg_gwmixed_inside_adj_dzgwET_only",
                   "natveg_all", "natveg_all_dzgwET_only",
                   "natveg_gwmixed_all", "natveg_gwmixed_all_dzgwET_only",
                   "reservoir_shackleford", "reservoir_etna",
                   "reservoir_french", "reservoir_sfork",
                   "reservoir_pipeline_etna", "reservoir_pipeline_french",
                   "bdas_tribs","bdas_all_streams",
                   "reservoir_etna_29kAF",
                   "reservoir_etna_134kAF_60cfs",
                    "reservoir_pipeline_etna_29kAF",
                   "reservoir_pipeline_etna_134kAF_60cfs"
  )
}
n_scenarios = length(scenario_ids)

# COMPARE_MAR = TRUE
# COMPARE_ILR = TRUE
# COMPARE_MAR_ILR = TRUE
graphics_type = 'png'    #output type for graphics, currently pdf or png

# Select Flow Change location
  flow_loc = 'Streamflow_FJ_SVIHM'; flow_loc_short = "FJ"
# flow_loc = 'Streamflow_Pred_Loc_2'; flow_loc_short = "PL2"
# flow_loc = 'Streamflow_Pred_Loc_3'; flow_loc_short = "PL3"

# results_dir = "C:/Users/Claire/Documents/GitHub/SVIHM/R_Files/Post-Processing/Results"
# setwd(results_dir)
scenario_dir = "C:/Users/Claire/Documents/GitHub/SVIHM/Scenarios"
scenario_dir_2 = "E:/SVIHM Scenarios Storage"
start_date = as.Date("1990-10-01")
end_date = as.Date("2018-09-30")
start_wy = 1991
end_wy = 2018
num_stress_periods = length(seq(start_date, end_date, by="month")); nsp = num_stress_periods

dif_lim = c(-80,200) #standard for all scenarios
# dif_lim = c(-40, 80)
#.############################################################################################
############################             IMPORT DATA             ############################
#.############################################################################################


#Import flow data for scenarios
DP_flows_scenarios = list()
for(i in 1:length(scenario_ids)){
  scenario_id = scenario_ids[i]
  scenario_filename = file.path(scenario_dir, scenario_id,paste0(flow_loc,".dat"))
  if(!file.exists(scenario_filename)){
    scenario_filename = file.path(scenario_dir_2, scenario_id,paste0(flow_loc,".dat"))
  }
  
  # print(scenario_filename)
  DP_flow_scenario = data.frame(Date = seq(start_date, end_date, "days"), 
                                Flow_m3day = read.table(scenario_filename, skip = 2)[,3],
                                Flow_cfs = read.table(scenario_filename, skip = 2)[,3]*0.000408734569)
  DP_flows_scenarios[[i]] = DP_flow_scenario 
}
names(DP_flows_scenarios) = scenario_ids


# Make a table for Thomas and his fish reconnection estimate -------------------------------------------------
  
  making_this_table = F
  if(making_this_table == T){
    flow_record_subset_wide = function(daily_flow_tables, scenario_ids){
      for(i in 1:length(daily_flow_tables)){
        df_tab = daily_flow_tables[[i]]
        
        subset1 = df_tab[month(df_tab$Date)>=tab_start_month & 
                           day(df_tab$Date)>=tab_start_day &
                           month(df_tab$Date) <=tab_end_month &
                           day(df_tab$Date) <=tab_end_day ,]
        subset1$year = year(subset1$Date)
        subset1$month_day = paste(str_pad(month(subset1$Date), width = 2, side = "left", pad = 0),
                                  str_pad(day(subset1$Date), width = 2, side = "left", pad = 0),
                                  sep = "-")
        subset1 = subset1[,3:5]
        
        wide = pivot_wider(data = subset1, id_cols = c(month_day), 
                           names_from = year, values_from = Flow_cfs)
        wide=wide[order(wide$month_day),]
        
        wide_subset_tables[[i]] = wide
      }
      return(wide_subset_tables)
    }
    
    tab_start_month = 8; tab_start_day = 1
    tab_end_month = 12; tab_end_day = 31
    
    wide_subset_tables = list() #initialize return list
    daily_flow_tables = list(DP_flows_scenarios$basecase, 
                             DP_MAR_flow, DP_ILR_flow, 
                             DP_MAR_ILR_flow,
                             #DP_Basecase_fl_flow, #DP_MAR_ILR_fl_flow,
                             #DP_0.8_flow, DP_0.9_flow, 
                             #DP_natveg_outside_adj_flow,
                             DP_natveg_gwmixed_outside_adj_flow)
    scenario_ids = c("basecase",
                     "mar","ilr",
                     "mar_ilr",
                     #"flowlims",# "mar_ilr_flowlims",
                     #"irrig_0.8","irrig_0.9", 
                     #"nvoa", 
                     "nvgwmoa")
    
    tabs= flow_record_subset_wide(daily_flow_tables = daily_flow_tables,
                                  scenario_ids = scenario_ids)
    
    # for(i in 1:length(tabs)){
    #   tab = tabs[[i]]
    #  # print(head(tab))
    #    write.csv(x=tab, paste(scenario_ids[i], ".csv"))
    # }
  }
  

  
  # make_flow_diff_daily_tab(basecase_flow_table = DP_flows_scenarios$basecase,
  #                                          daily_flow_tables = list(DP_MAR_flow, 
  #                                                                   DP_ILR_flow, 
  #                                                                   DP_MAR_ILR_flow,
  #                                                                   DP_Basecase_fl_flow, 
  #                                                                   #DP_MAR_ILR_fl_flow,
  #                                                                   DP_0.8_flow, DP_0.9_flow),
  #                                          scenario_ids = c("mar","ilr","mar_ilr", 
  #                                                           "flowlims",# "mar_ilr_flowlims", 
  #                                                           "irrig_0.8","irrig_0.9"))



#Function to process SFR file of scenarios specified in list of scenario ids
read_inflows = function(results_dir, scenario_ids){
  #Read in basecase and declare Seg1 and Seg32 inflow dataframes
  Inflow_Basecase = readLines('SVIHM_basecase.sfr')
  Inflow_Seg1_Basecase = as.numeric(sapply(lapply(strsplit(Inflow_Basecase[grep(x = Inflow_Basecase, pattern = '1  1  3  0')],' '),function(x){x[!x ==""]}),"[[",5))
  MODFLOW_Seg1_Inflows = data.frame(Date = seq(start_date, by = "month", length.out = nsp),
                                    Basecase = Inflow_Seg1_Basecase)
  
  Inflow_Seg32_Basecase = as.numeric(sapply(lapply(strsplit(Inflow_Basecase[grep(x = Inflow_Basecase, pattern = '32  1  0  10  0')],' '),function(x){x[!x ==""]}),"[[",6))
  MODFLOW_Seg32_Inflows = data.frame(Date = seq(start_date, by = "month", length.out = nsp),
                                     Basecase = Inflow_Seg32_Basecase)
  
  # Read and process scenario data, and append to inflow dataframes
  n_scenarios = length(scenario_ids)
  for(i in 1:n_scenarios){
    scenario_id = scenario_ids[i]
    if(str_to_lower(scenario_id) %in% c("mar","ilr","mar_ilr")){scenario_id = str_to_upper(scenario_ids[i])}
    sfr_file = file.path(results_dir, paste0("SVIHM_",scenario_id, ".sfr"))
    
    # Read in SFR file for scenario
    Inflow = readLines(sfr_file)
    # Read in inflows for Seg 1
    Inflow_Seg1 = as.numeric(sapply(lapply(strsplit(Inflow[grep(x = Inflow, pattern = '1  1  3  0')],' '),function(x){x[!x ==""]}),"[[",5))
    Inflow_Seg1_diff = Inflow_Seg1 - Inflow_Seg1_Basecase
    #Attach this column to the Seg1 Inflows dataframe; rename columns
    MODFLOW_Seg1_Inflows$scenario = Inflow_Seg1
    MODFLOW_Seg1_Inflows$scenario_diff = Inflow_Seg1_diff
    colname_endings = c("","_diff")
    colnames(MODFLOW_Seg1_Inflows)[(2*i+1):(2*i+2)] = c(paste0(scenario_id,colname_endings))

    # Read in inflows for Seg 32
    Inflow_Seg32 = as.numeric(sapply(lapply(strsplit(Inflow[grep(x = Inflow, pattern = '32  1  0  10  0')],' '),function(x){x[!x ==""]}),"[[",6))
    Inflow_Seg32_diff = Inflow_Seg32 - Inflow_Seg32_Basecase
    # Attach this column to the Seg32 Inflows dataframe; rename columns
    MODFLOW_Seg32_Inflows$scenario = Inflow_Seg32
    MODFLOW_Seg32_Inflows$scenario_diff = Inflow_Seg32_diff
    colname_endings = c("","_diff")
    colnames(MODFLOW_Seg32_Inflows)[(2*i+1):(2*i+2)] = c(paste0(scenario_id,colname_endings))
    
  }
  return(list(MODFLOW_Seg1_Inflows, MODFLOW_Seg32_Inflows))
}

# # Parse SFR file and create Seg1 and Seg32 monthly flows table
# inflow_diff_tables = read_inflows(results_dir = results_dir, scenario_ids = scenario_ids)
# MODFLOW_Seg1_Inflows = inflow_diff_tables[[1]]; MODFLOW_Seg32_Inlows = inflow_diff_tables[[2]]

# was here: if statements reading in Seg1 and 32 inflows for mar, ilr, and mar_ilr

# Read in pumping data (in progress)

read_pumping_data = function(results_dir, scenario_ids){
  pumping = read.table('monthly_groundwater_by_luse_basecase.dat', header = T)
  
  pumping_monthly = vector(mode = "list", length = 0)
  pumping_yearly = vector(mode = "list", length = 0)
  for(i in 1:length(scenario_ids)){
    scenario_id = scenario_ids[i]
    if(str_to_lower(scenario_id) %in% c("mar","ilr","mar_ilr")){scenario_id = str_to_upper(scenario_ids[i])}
    pump_file = file.path(results_dir, paste0('monthly_groundwater_by_luse_',scenario_id,'.dat'))
    pumping_scenario = read.table(pump_file, header = T)
    # colnames(pumping_scenario)[2:6] = paste(colnames(pumping_scenario[2:6]), scenario_id, sep = "_")
    #Append to list of monthly pumping scenario tables
    pumping_monthly = Append(pumping_monthly, pumping_scenario)
    
    #aggregate by year and append to list
    pumping_yr = pumping_scenario
    pumping_yr$Stress_Period = rep(seq(start_wy,end_wy),each = 12) # converts to year
    pumping_yr = aggregate(.~Stress_Period, pumping_yr, FUN = sum)
    pumping_yr = rowSums(pumping_yr[,-1])*0.000000810714
    
    pumping_yearly = append(pumping_yearly, pumping_scenario)
  }
    
  pumping_daily = pumping #store for later
  
  
  return(pumping)
}

# pumping_bc = read.table('monthly_groundwater_by_luse_basecase.dat', header = T)
# pumping_MAR = read.table('monthly_groundwater_by_luse_MAR.dat', header = T)
# pumping_ILR = read.table('monthly_groundwater_by_luse_ILR.dat', header = T)
# pumping_MAR_ILR = read.table('monthly_groundwater_by_luse_MAR_ILR.dat', header = T)
# 
# pumping_flowlims = read.table('monthly_groundwater_by_luse_flowlims.dat', header = T)
# pumping_MAR_ILR_flowlims = read.table('monthly_groundwater_by_luse_MAR_ILR_flowlims.dat', header = T)
# 
# pumping_bc$Stress_Period = rep(seq(start_wy,end_wy),each = 12) # converts to year
# pumping_MAR$Stress_Period = rep(seq(start_wy,end_wy),each = 12)
# pumping_ILR$Stress_Period = rep(seq(start_wy,end_wy),each = 12)
# pumping_flowlims$Stress_Period = rep(seq(start_wy,end_wy),each = 12)
# pumping_MAR_ILR_flowlims$Stress_Period = rep(seq(start_wy,end_wy),each = 12)
# 
# 
# pumping_bc = aggregate(.~Stress_Period, pumping_bc, FUN = sum)
# pumping_bc = rowSums(pumping_bc[,-1])*0.000000810714
# pumping_MAR = aggregate(.~Stress_Period, pumping_MAR, FUN = sum)
# pumping_MAR = rowSums(pumping_MAR[,-1])*0.000000810714
# pumping_ILR = aggregate(.~Stress_Period, pumping_ILR, FUN = sum)
# pumping_ILR = rowSums(pumping_ILR[,-1])*0.000000810714
# pumping_MAR_ILR = aggregate(.~Stress_Period, pumping_MAR_ILR, FUN = sum)
# pumping_MAR_ILR = rowSums(pumping_MAR_ILR[,-1])*0.000000810714
# pumping_flowlims = aggregate(.~Stress_Period, pumping_flowlims, FUN = sum)
# pumping_flowlims = rowSums(pumping_flowlims[,-1])*0.000000810714
# pumping_MAR_ILR_flowlims = aggregate(.~Stress_Period, pumping_MAR_ILR_flowlims, FUN = sum)
# pumping_MAR_ILR_flowlims = rowSums(pumping_MAR_ILR_flowlims[,-1])*0.000000810714
# 
# MAR_pumping_diff = pumping_bc - pumping_MAR
# ILR_GW_Reduction = data.frame(Year=seq(start_wy,end_wy),
#                               Reduction_TAF = (pumping_bc - pumping_ILR),
#                               Reduction_pct = (pumping_bc - pumping_ILR)/pumping_bc*100)
# 
# MAR_ILR_pumping_diff = pumping_bc - pumping_MAR_ILR


#.############################################################################################
##########################             DATA PROCESSING             ##########################
#.############################################################################################

make_flow_diff_daily_tab = function(basecase_flow_table = DP_flows_scenarios$basecase,
                                    daily_flow_tables = DP_flows_scenarios[scenario_ids[-1]],
                                    scen_ids = scenario_ids[-1]){
  Flow_Diff_Daily = data.frame(Date = seq(start_date, end_date, "days"))                          # Calculate daily difference from basecase condition
  
  for(i in 1:length(daily_flow_tables)){
    scenario_flow_table = daily_flow_tables[[i]]
    scenario_id = scen_ids[i]
    # if(str_to_lower(scenario_id) %in% c("mar","ilr","mar_ilr")){scenario_id = str_to_upper(scenario_id)}
    
    Flow_Diff_Daily$scen_difference_m3day = scenario_flow_table$Flow_m3day - basecase_flow_table$Flow_m3day
    Flow_Diff_Daily$scen_difference_cfs = scenario_flow_table$Flow_cfs - basecase_flow_table$Flow_cfs
    
    unit_strings = c("m3day","cfs")
    colnames(Flow_Diff_Daily)[(2*i):(2*i+1)] = c(paste(scenario_id,"difference", unit_strings, sep = "_"))
  }
  return(Flow_Diff_Daily)
}

make_daily_flow_tab = function(daily_flow_tables = DP_flows_scenarios, 
                               scen_ids = scenario_ids){
  Daily_Flow = data.frame(Date = seq(start_date, end_date, "days"))

  for(i in 1:length(daily_flow_tables)){
    scenario_flow_table = daily_flow_tables[[i]]
    scenario_id = scen_ids[i]
    # if(str_to_lower(scenario_id) %in% c("mar","ilr","mar_ilr")){scenario_id = str_to_upper(scenario_id)}

    Daily_Flow$scen_difference_m3day = scenario_flow_table$Flow_m3day
    Daily_Flow$scen_difference_cfs = scenario_flow_table$Flow_cfs

    unit_strings = c("m3day","cfs")
    colnames(Daily_Flow)[(2*i):(2*i+1)] = c(paste(scenario_id, unit_strings, sep = "_"))
  }

return(Daily_Flow)

}

# FLOW DIFFERENCES

#Specify tables and scenario IDs (for column name differentiation) for daily diff table
Flow_Diff_Daily = make_flow_diff_daily_tab()

# Average difference in flow for each Stress Period
Flow_Diff_SP_Avg = Flow_Diff_Daily# Copy daily data frame
Flow_Diff_SP_Avg$Date = format(Flow_Diff_SP_Avg$Date, '%b-%y')
Flow_Diff_SP_Avg = aggregate(.~Date,data = Flow_Diff_SP_Avg,  FUN = mean)
Flow_Diff_SP_Avg$Date = as.Date(paste0(Flow_Diff_SP_Avg$Date,'-01'), format = '%b-%y-%d')
Flow_Diff_SP_Avg = Flow_Diff_SP_Avg[order(Flow_Diff_SP_Avg$Date),]

# Average DIFFERENCE in flow for each calendar month
Flow_Diff_Monthly_Avg = Flow_Diff_Daily[seq(-1,-92),]   # Exclude first three months when MAR is not active                                                                            
Flow_Diff_Monthly_Avg$Date = format(Flow_Diff_Monthly_Avg$Date, '%b')
Flow_Diff_Monthly_Avg_2 = Flow_Diff_Monthly_Avg                                      # Copy daily data frame
Flow_Diff_Monthly_Avg = aggregate(.~Date,data =Flow_Diff_Monthly_Avg,  FUN = mean)
Flow_Diff_Monthly_SD = aggregate(.~Date,data =Flow_Diff_Monthly_Avg_2,  FUN = sd)

names(Flow_Diff_Monthly_SD) = c('Date',paste0(names(Flow_Diff_Monthly_Avg[-1]),'_SD'))

Flow_Diff_Monthly_Avg = merge(Flow_Diff_Monthly_Avg,Flow_Diff_Monthly_SD)
Flow_Diff_Monthly_Avg$Date  = factor(Flow_Diff_Monthly_Avg$Date , levels = as.character(format(seq(as.Date("1990/1/1"), as.Date("1990/12/31"), "month"), '%b')))
Flow_Diff_Monthly_Avg = Flow_Diff_Monthly_Avg[order(Flow_Diff_Monthly_Avg$Date), ]

# DAILY FLOW and depletion --------------------------------------------------------

Daily_Flow = make_daily_flow_tab(
  daily_flow_tables = DP_flows_scenarios,
  scen_ids = scenario_ids)
# write.csv(Daily_Flow, file = "Scenarios Daily Flow.csv")

no_pump_ref_case = "natveg_outside_adj_dzgwET_only" # Set the No Pump Ref Case
# no_pump_ref_case = "natveg_gwmixed_all_dzgwET_only" # Set the No Pump Ref Case
# make depletion table
depletion_tab = data.frame(Date = Daily_Flow$Date,
                           no_pump_ref_case_m3day = Daily_Flow[,paste0(no_pump_ref_case,"_m3day")],
                           no_pump_ref_case_cfs = Daily_Flow[,paste0(no_pump_ref_case,"_cfs")],
                           basecase_m3day = Daily_Flow$basecase_m3day,
                           basecase_cfs = Daily_Flow$basecase_cfs,
                           mar_ilr_m3day = Daily_Flow$mar_ilr_m3day,
                           mar_ilr_cfs = Daily_Flow$mar_ilr_cfs)
depletion_tab$total_depletion_m3day = depletion_tab$no_pump_ref_case_m3day - depletion_tab$basecase_m3day
depletion_tab$total_depletion_cfs = depletion_tab$no_pump_ref_case_cfs - depletion_tab$basecase_cfs
depletion_tab$mar_ilr_dep_rev_m3day = depletion_tab$mar_ilr_m3day - depletion_tab$basecase_m3day
depletion_tab$mar_ilr_dep_rev_cfs = depletion_tab$mar_ilr_cfs - depletion_tab$basecase_cfs

# write.csv(depletion_tab, "Total Depl. and MAR_ILR Dep. Rev.csv")

# dep_tab_s_n = depletion_tab[month(depletion_tab$Date) %in% 9:11,]
# sum(dep_tab_s_n$total_depletion_m3day) / 1233.48 / 1000

# Make tables for making scenario analysis plots
# Average flow for each Stress Period
Flow_SP_Avg = Daily_Flow# Copy daily data frame
Flow_SP_Avg$Date = format(Flow_SP_Avg$Date, '%b-%y')
Flow_SP_Avg = aggregate(.~Date,data = Flow_SP_Avg,  FUN = mean)
Flow_SP_Avg$Date = as.Date(paste0(Flow_SP_Avg$Date,'-01'), format = '%b-%y-%d')
Flow_SP_Avg = Flow_SP_Avg[order(Flow_SP_Avg$Date),]


# Average daily flow for each calendar month, over whole model period
Flow_Monthly_Avg = DP_flows_scenarios$basecase[seq(-1,-92),]   # Exclude first three months when MAR is not active                                                                            
Flow_Monthly_Avg$Date = format(Flow_Monthly_Avg$Date, '%b')
Flow_Monthly_Avg_2 = Flow_Monthly_Avg                                      # Copy daily data frame
Flow_Monthly_Avg = aggregate(.~Date,data =Flow_Monthly_Avg,  FUN = mean)
Flow_Monthly_SD = aggregate(.~Date, data =Flow_Monthly_Avg_2,  FUN = sd)
names(Flow_Monthly_SD) = c('Date',paste0(names(Flow_Monthly_Avg[-1]),'_SD'))

Flow_Monthly_Avg = merge(x = Flow_Monthly_Avg, y = Flow_Monthly_SD, by.x = "Date", by.y = "Date")
Flow_Monthly_Avg$Date  = factor(Flow_Monthly_Avg$Date , levels = as.character(format(seq(as.Date("1990/1/1"), as.Date("1990/12/31"), "month"), '%b')))
Flow_Monthly_Avg = Flow_Monthly_Avg[order(Flow_Monthly_Avg$Date), ]




geom_ribbon_maker = function(y_start){
  x_start = 1:12

  #Add extrta entries where the line crosses 0
  x_out = x_start; y_out = y_start; crosses_pt1 = c()
  out_add = 0
  for(i in 2:12){
    if(y_start[i-1] > 0 & y_start[i] <0 | y_start[i-1] < 0 & y_start[i] >0){
      x_out = c(x_out[1:(i-1+out_add)], NA, x_start[i:12])
      y_out = c(y_out[1:(i-1+out_add)], 0, y_start[i:12])
      crosses_pt1 = c(crosses_pt1, i-1)
      out_add = out_add + 1
    }
  }
  #calculate x-values of 0-crossing line
  if(length(x_out) != length(x_start)){
    fill_in_these_xes = which(is.na(x_out))
    for(i in 1:length(crosses_pt1)){
      j = crosses_pt1[i]
      y_pt1 = y_start[j]; y_pt2 = y_start[j+1]
      x_out[fill_in_these_xes[i]] = j - (y_pt1 - 0) / (y_pt2 - y_pt1) 
    }
  }
  
  #create red and blue ribbons
  x_for_ribbon = x_out
  blue_y = y_out
  blue_y[y_out<0] = 0
  red_y = y_out
  red_y [y_out>0] = 0
  
  geom_ribbon = data.frame(ribbon_x = x_for_ribbon, blue_y = blue_y, red_y = red_y)
  return(geom_ribbon)
}

# mar_ribbon_tab = geom_ribbon_maker(y_start = Flow_Diff_Monthly_Avg$MAR_difference_cfs)

# mar_ilr_ribbon_tab = geom_ribbon_maker(y_start = Flow_Diff_Monthly_Avg$MAR_ILR_difference_cfs)

#Initialize ystart

geom_ribbon_list = list()
# print(scenario_ids)

for(i in 2:length(scenario_ids)){
  scenario_id = scenario_ids[i]
  # if(tolower(scenario_id) %in% c("mar","ilr", "mar_ilr")){scenario_id = toupper(scenario_id)}

  scenario_colname = paste(scenario_id, "difference","cfs", sep = "_")
  
  geom_ribbon_list[[i]] = geom_ribbon_maker(y_start = Flow_Diff_Monthly_Avg[,scenario_colname])
}
 
# Scenario-comparison metrics table ---------------------------------------


# colnames_metrics = c(fall_days_gained_20cfs, monthly_flow_chg_avg_Sept_cfs, monthly_flow_chg_val_Sept_cfs,
#                      crop_water_deficit_TAF_per_yr, acres_affected)


#.############################################################################################
####################        Scenario Comparison Metric Table       ####################
#.############################################################################################

# Average flow increase in dry period
# Reconnection days gained
# 

#.############################################################################################
############################                PLOTS                ############################
#.############################################################################################


#.############################################################################################
####################        Universal plotter scripts       ####################
#.############################################################################################


# Type 1: Monthly streamflow difference, 28-yr avg
# head(Flow_Diff_Monthly_Avg)


flow_diff_monthly_avg_fig = function(Flow_Diff_Monthly_Avg = Flow_Diff_Monthly_Avg,
                                     geom_ribbon_data = mar_ribbon_tab, 
                                     scenario_colname = "mar_difference_cfs"){
  
  scenario_yvals = Flow_Diff_Monthly_Avg[,scenario_colname]
  sd_colname = paste(scenario_colname, "SD", sep = "_")
  sd_yvals = Flow_Diff_Monthly_Avg[,sd_colname]
  sd_min_yvals = scenario_yvals - sd_yvals
  sd_max_yvals = scenario_yvals + sd_yvals
  
  monthly_avg_plot = ggplot(data = Flow_Diff_Monthly_Avg) + 
    geom_ribbon(data = geom_ribbon_data, aes(x = ribbon_x,ymin = red_y, ymax = 0), fill = 'red', alpha=  0.5) +
    geom_ribbon(data = geom_ribbon_data, aes(x = ribbon_x,ymin = 0, ymax = blue_y), fill = 'dodgerblue', alpha=  0.5) + 
    geom_hline(yintercept = 0) +
    ylab("Streamflow Difference (cfs)") +
    geom_line(aes(x = seq(1,12), y = scenario_yvals, group = 1)) +
    geom_errorbar(aes(x = seq(1,12), ymin = sd_min_yvals, 
                      ymax = sd_max_yvals, group = 1), width = 0.25) +
    geom_point(aes(x = seq(1,12), y = scenario_yvals, group = 1),size = 1.5) +
    scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = Flow_Diff_Monthly_Avg$Date) +
    scale_y_continuous(limits = dif_lim, 
                       breaks = seq(dif_lim[1], dif_lim[2],by = 20), expand = c(0,0)) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(fill=NA, color = 'black'),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
          axis.text.y = element_text(size = 8),
          axis.ticks = element_line(size = 0.2),
          plot.title = element_text(hjust = 0.5, size = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 8))
  
  return(monthly_avg_plot)
}

# Type 2: Monthly streamflow difference, wet avg dry

#Subset Flow Diff tables for Dry Avg Wet figures
Flow_Diff_SP_Dry_Avg_Wet = subset(Flow_Diff_SP_Avg,#select = c('Date','MAR_difference_cfs', 'ILR_difference_cfs', 'MAR_ILR_difference_cfs'), 
                                  Date %in%c(seq(as.Date("2010/1/1"), by = "month", length.out = 12),
                                             seq(as.Date("2014/1/1"), by = "month", length.out = 12),
                                             seq(as.Date("2017/1/1"), by = "month", length.out = 12)))
Flow_Diff_SP_Dry_Avg_Wet$Year_Type = rep(c('Average (2010)','Dry (2014)','Wet (2017)'), each=12)
Flow_Diff_SP_Dry_Avg_Wet$Year_Type = factor(Flow_Diff_SP_Dry_Avg_Wet$Year_Type, levels = c('Dry (2014)','Average (2010)','Wet (2017)'))
Flow_Diff_SP_Dry_Avg_Wet = Flow_Diff_SP_Dry_Avg_Wet[order(Flow_Diff_SP_Dry_Avg_Wet$Year_Type),]
Flow_Diff_SP_Dry_Avg_Wet$Date = format(Flow_Diff_SP_Dry_Avg_Wet$Date, '%m')


flow_diff_DAW_fig = function(Flow_Diff_SP_Dry_Avg_Wet = Flow_Diff_SP_Dry_Avg_Wet,
                             scenario_colname = "MAR_difference_cfs"){
  
  scenario_yvals = Flow_Diff_SP_Dry_Avg_Wet[,scenario_colname]
  # # sd_colname = paste(scenario_colname, "SD", sep = "_")
  # # sd_yvals = Flow_Diff_SP_Dry_Avg_Wet[,sd_colname]
  # sd_min_yvals = scenario_yvals - sd_yvals
  # sd_max_yvals = scenario_yvals + sd_yvals
  
  
  Dry_Avg_Wet_Diff_Plot = ggplot(data = Flow_Diff_SP_Dry_Avg_Wet, 
                                 aes(x = as.numeric(Date), y = scenario_yvals, group = Year_Type, color = Year_Type)) +
     geom_hline(yintercept = 0) +
     geom_line(size = 1) +
     geom_point(size = 1.5) +
     scale_y_continuous(limits = dif_lim, 
                        breaks = seq(dif_lim[1], dif_lim[2],by = 20), expand = c(0,0)) +
     scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = format(seq(as.Date("2001/1/1"), by = "month", length.out = 12),'%b')) +
     scale_color_manual(values = c('orangered','darkgoldenrod2','cornflowerblue')) +
     theme(panel.background = element_blank(),
           panel.border = element_rect(fill=NA, color = 'black'),
           axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
           axis.text.y = element_text(size = 8),
           axis.ticks = element_line(size = 0.2),
           plot.title = element_text(hjust = 0.5, size = 10),
           axis.title.x = element_blank(),
           axis.title.y = element_text(size = 8),
           legend.key = element_blank(),
           legend.title = element_blank(),
           legend.position = c(0.80,0.15),
           legend.background = element_blank())
  
  return(Dry_Avg_Wet_Diff_Plot)
}


# 
# MAR_month_avg_diff_fig = flow_diff_monthly_avg_fig(Flow_Diff_Monthly_Avg = Flow_Diff_Monthly_Avg,
#                                                    geom_ribbon_data = MAR_geom_ribbon_data,
#                                                    scenario_colname = "MAR_difference_cfs")
# 
# ILR_month_avg_diff_fig = flow_diff_monthly_avg_fig(Flow_Diff_Monthly_Avg = Flow_Diff_Monthly_Avg,
#                                                    geom_ribbon_data = ILR_geom_ribbon_data,
#                                                    scenario_colname = "ILR_difference_cfs")




# Type 3: Monthly streamflows, 28-yr avg

# Process so that the error bars show up
Flow_Monthly_Avg$Flow_cfs_SD_down = Flow_Monthly_Avg$Flow_cfs_SD
sd_bigger_than_avg = Flow_Monthly_Avg$Flow_cfs_SD_down>Flow_Monthly_Avg$Flow_cfs
Flow_Monthly_Avg$Flow_cfs_SD_down[sd_bigger_than_avg] = Flow_Monthly_Avg$Flow_cfs[sd_bigger_than_avg] -1

Basecase_Monthly_Avg_Flow_Plot = ggplot(data = Flow_Monthly_Avg) + 
    geom_hline(yintercept = 0) +
    geom_line(aes(x = seq(1,12), y = Flow_cfs, group = 1)) +
    geom_point(aes(x = seq(1,12), y = Flow_cfs, group = 1),size = 1.5) +
    geom_errorbar(aes(x = seq(1,12), ymin = Flow_cfs-Flow_cfs_SD_down, 
                      ymax = Flow_cfs+Flow_cfs_SD, group = 1), width = 0.25) +
    scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = Flow_Diff_Monthly_Avg$Date) +
    scale_y_continuous(limits = c(1,2000), 
                       trans="log10", breaks = 10^seq(0,4,by = 1),) +
    annotation_logticks(sides = "l")+
    theme(#panel.background = element_blank(),
      panel.border = element_rect(fill=NA, color = 'black'),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
      axis.text.y = element_text(size = 8),
      axis.ticks = element_line(size = 0.2),
      plot.title = element_text(hjust = 0.5, size = 10),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 8))


# Type 4: Monthly streamflows, wet avg dry

#Process flows for this figure
Flow_SP_Dry_Avg_Wet = subset(Flow_SP_Avg,
                             #select = c('Date','MAR_difference_cfs', 'ILR_difference_cfs', 'MAR_ILR_difference_cfs'), 
                             Date %in%c(seq(as.Date("2010/1/1"), by = "month", length.out = 12),
                                        seq(as.Date("2014/1/1"), by = "month", length.out = 12),
                                        seq(as.Date("2017/1/1"), by = "month", length.out = 12)))
Flow_SP_Dry_Avg_Wet$Year_Type = rep(c('Average (2010)','Dry (2014)','Wet (2017)'), each=12)
Flow_SP_Dry_Avg_Wet$Year_Type = factor(Flow_SP_Dry_Avg_Wet$Year_Type, levels = c('Dry (2014)','Average (2010)','Wet (2017)'))
Flow_SP_Dry_Avg_Wet = Flow_SP_Dry_Avg_Wet[order(Flow_SP_Dry_Avg_Wet$Year_Type),]
Flow_SP_Dry_Avg_Wet$Date = format(Flow_SP_Dry_Avg_Wet$Date, '%m')

# Generate historical flow monthly avg plot

monthly_flows_DAW_fig = function(Flow_SP_Dry_Avg_Wet = Flow_SP_Dry_Avg_Wet,
                                 scenario_colname = "MAR_cfs"){
  scenario_yvals = Flow_SP_Dry_Avg_Wet[,scenario_colname]
  basecase_yvals = Flow_SP_Dry_Avg_Wet[,"basecase_cfs"]
  
  Monthly_Flows_DAW_fig = ggplot()+
    geom_line(data = Flow_SP_Dry_Avg_Wet, aes(x = as.numeric(Date), y = scenario_yvals, group = Year_Type, color = Year_Type), size = 1.5) +
    geom_line(data = Flow_SP_Dry_Avg_Wet, aes(x = as.numeric(Date), y = basecase_yvals, group = Year_Type, color = Year_Type), linetype = "dashed", size = 0.8) +
    geom_hline(yintercept = 0) +
    # geom_line(size = 1) +
    geom_point(size = 1.5) +
    # scale_y_continuous(limits = dif_lim, 
    #                    breaks = seq(dif_lim[1], dif_lim[2],by = 20), expand = c(0,0)) +
    scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = format(seq(as.Date("2001/1/1"), by = "month", length.out = 12),'%b')) +
    scale_y_continuous(limits = c(1,3000), 
                       trans="log10", breaks = 10^seq(0,4,by = 1),) +
    annotation_logticks(sides = "l")+
    scale_color_manual(values = c('orangered','darkgoldenrod2','cornflowerblue')) +
    
    theme(#panel.background = element_blank(),
      panel.border = element_rect(fill=NA, color = 'black'),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
      axis.text.y = element_text(size = 8),
      axis.ticks = element_line(size = 0.2),
      plot.title = element_text(hjust = 0.5, size = 10),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 8),
      legend.key = element_blank(),
      legend.title = element_blank(),
      legend.position = c(0.25,0.15),
      legend.background = element_blank())
  
  return(Monthly_Flows_DAW_fig)
}


# Scenario Results - 4 panels. Figs type 1-4
make_all_plots = FALSE
if(make_all_plots == TRUE){
  for(i in 2:length(scenario_ids)){
    # i=17 # For specifics scenarios, pick which one you want
    scenario_id = scenario_ids[i]
    # if(tolower(scenario_id) %in% c("mar","ilr", "mar_ilr")){scenario_id = toupper(scenario_id)}
    scenario_colname_diff = paste(scenario_id, "difference","cfs", sep = "_")
    scenario_colname_flow = paste(scenario_id, "cfs", sep = "_")
    
    fdmaf = flow_diff_monthly_avg_fig(Flow_Diff_Monthly_Avg = Flow_Diff_Monthly_Avg,
                                      geom_ribbon_data = geom_ribbon_list[[i]],
                                      scenario_colname = scenario_colname_diff)
    fddawf = flow_diff_DAW_fig(Flow_Diff_SP_Dry_Avg_Wet = Flow_Diff_SP_Dry_Avg_Wet,
                               scenario_colname = scenario_colname_diff)
    mfdawf = monthly_flows_DAW_fig(Flow_SP_Dry_Avg_Wet = Flow_SP_Dry_Avg_Wet,
                                   scenario_colname = scenario_colname_flow)
    
    fig_name = paste(scenario_id, "scenario results_4 panels.png")
    
    png(fig_name, width = 7, height = 6, units = "in", res = 300)
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(2,2)))
    vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
    print(fdmaf +
            ylab('Average Streamflow Difference (cfs)'),
          vp = vplayout(1,1))
    print(fddawf +
            ylab(''),
          vp = vplayout(1,2))
    print(Basecase_Monthly_Avg_Flow_Plot +
            ylab('Monthly Average Streamflow (cfs)'),
          vp = vplayout(2,1))
    print(mfdawf +
            ylab(''),
          vp = vplayout(2,2))
    graphics.off()
  }
}



# Scenario results - panels, flow diffs. Figs Type 1 and 2.
# for(i in 1:length(scenario_ids)){
#   scenario_id = scenario_ids[i]
#   if(tolower(scenario_id) %in% c("mar","ilr", "mar_ilr")){scenario_id = toupper(scenario_id)}
#   scenario_colname = paste(scenario_id, "difference", "cfs", sep = "_")
#   
#   fdmaf = flow_diff_monthly_avg_fig(Flow_Diff_Monthly_Avg = Flow_Diff_Monthly_Avg,
#                                     geom_ribbon_data = geom_ribbon_list[i],
#                                     scenario_colname = scenario_colname)
#   fddawf = flow_diff_DAW_fig(Flow_Diff_SP_Dry_Avg_Wet = Flow_Diff_SP_Dry_Avg_Wet,
#                              scenario_colname = scenario_colname)
#   
#   
#   fig_name = paste(scenario_id, "flow diff monthly avg.png")
#   
#   png(fig_name, width = 7, height = 3, units = "in", res = 300)
#   grid.newpage()
#   pushViewport(viewport(layout = grid.layout(1,2)))
#   vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
#   print(fdmaf + 
#           ylab('Average Streamflow Difference (cfs)'),
#         vp = vplayout(1,1))
#   print(fddawf +
#           ylab(''),
#         vp = vplayout(1,2))
#   graphics.off()
#   
# }

## Scenario results 2 panels, flows. Figl type 3 and 4
# for(i in 1:length(scenario_ids)){
#   scenario_id = scenario_ids[i]
#   if(tolower(scenario_id) %in% c("mar","ilr", "mar_ilr")){scenario_id = toupper(scenario_id)}
#   scenario_colname = paste(scenario_id, "cfs", sep = "_")
# 
#   mfdawf = monthly_flows_DAW_fig(Flow_SP_Dry_Avg_Wet = Flow_SP_Dry_Avg_Wet,
#                              scenario_colname = scenario_colname)
# 
# 
#   fig_name = paste(scenario_id, "flows monthly avg.png")
# 
#   png(fig_name, width = 7, height = 3, units = "in", res = 300)
#   grid.newpage()
#   pushViewport(viewport(layout = grid.layout(2,2)))
#   vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
#   print(Basecase_Monthly_Avg_Flow_Plot +
#           ylab('Average Daily Streamflow (cfs)'),
#         vp = vplayout(1,1))
#   print(mfdawf +
#           ylab(''),
#         vp = vplayout(1,2))
#   graphics.off()
# 
# }




# Stream Depletion Reversal Calculation -----------------------------------

m3_to_TAF = 1/4046.86*3.28084/1000 # 1acre/4046.86 m2 * 3.28084 ft/m * 1TAF / 1000 AF

save_stream_dep_csv = function(Flow_Diff_Daily = Flow_Diff_Daily,
                               start_month = 6, end_month = 12){
  # Calculate total depletion. nprc = No Pumping Reference Case
  nprc_colname = "natveg_gwmixed_outside_adj_difference_m3day"
  fdd_nprc = Flow_Diff_Daily[month(Flow_Diff_Daily$Date) %in% c(start_month:end_month), nprc_colname]
  total_depletion = sum(fdd_nprc) * m3_to_TAF
  
  #pull water year types for wyt breakdown
  wyt = read.csv("C:/Users/Claire/Documents/GitHub/SiskiyouGSP2022/GSP_Analyses/Scott_Groundwater_Conditions/sgma_wyt_dataset.csv")
  dry_yrs = wyt$WY[wyt$WYT %in% c("Critical", "Dry")]
  wet_yrs = wyt$WY[wyt$WYT %in% c("Wet")]
  
  scen_dep_tab = data.frame(scenario_id = scenario_ids[2:n_scenarios],
                            neg_dep_TAF = NA,
                            pos_dep_TAF = NA,
                            scen_dep_TAF = NA,
                            relative_dep = NA,
                            dry_yr_dep_rev_TAF = NA,
                            wet_yr_dep_rev_TAF = NA)
  
  for(i in 2:length(scenario_ids)){
    scenario_id = scenario_ids[i]
    
    # Calculate scenario depletion. 
    scen_colname = paste0(scenario_id,"_difference_m3day")
    
    #fdd = Flow Difference Daily
    fdd_scen_date_window = Flow_Diff_Daily[month(Flow_Diff_Daily$Date) %in% c(start_month:end_month),scen_colname]
    # hist(fdd_scen_date_window)
    gt0 = sum(fdd_scen_date_window[fdd_scen_date_window>0]) * m3_to_TAF
    lt0 = sum(fdd_scen_date_window[fdd_scen_date_window<0]) * m3_to_TAF
    # check for a bunch of negative depletion reversal
    if(abs(lt0/gt0) > 0.05)
    {print(paste0(scenario_id,
                  " - Negative depletion reversal exceeds 5%: ",
                  round(lt0/gt0*100,2),"%"))
    }
    
    scen_dep_tab$neg_dep_TAF[scen_dep_tab$scenario_id==scenario_id] = round(lt0,1)
    scen_dep_tab$pos_dep_TAF[scen_dep_tab$scenario_id==scenario_id] = round(gt0,1)
    
    scen_depletion = round(sum(fdd_scen_date_window) * m3_to_TAF,1)
    
    scen_dep_tab$scen_dep_TAF[scen_dep_tab$scenario_id==scenario_id] = scen_depletion
    
    # breakdown by wyt
    fdd_dates = Flow_Diff_Daily$Date[month(Flow_Diff_Daily$Date) %in% c(start_month:end_month)]
    fdd_year = year(fdd_dates)
    fdd_wyt = wyt$WYT[match(fdd_year, wyt$WY)]
    fdd_wyt[fdd_wyt == "Critical"] = "Dry" #store critical as Dry for this exercise
    scen_dep_by_wyt = aggregate(fdd_scen_date_window, by = list(fdd_wyt), FUN = sum)
    num_days_by_wyt = aggregate(fdd_scen_date_window, by = list(fdd_wyt), FUN = length)
    
    scen_dep_cfs_by_wyt = round(scen_dep_by_wyt$x / num_days_by_wyt$x * m3d_to_cfs, 2)
    
    scen_dep_tab$dry_yr_dep_rev_cfs[scen_dep_tab$scenario_id==scenario_id] = scen_dep_cfs_by_wyt[scen_dep_by_wyt$Group.1 == "Dry"]
    scen_dep_tab$wet_yr_dep_rev_cfs[scen_dep_tab$scenario_id==scenario_id] = scen_dep_cfs_by_wyt[scen_dep_by_wyt$Group.1 == "Wet"]
    
  }
  
  scen_dep_tab$relative_dep = round(scen_dep_tab$scen_dep_TAF / total_depletion, 2)
  
  scen_dep_tab$rel_dep_category = "High"
  scen_dep_tab$rel_dep_category[scen_dep_tab$relative_dep < .25] = "Low"
  scen_dep_tab$rel_dep_category[scen_dep_tab$relative_dep >= .25 & scen_dep_tab$relative_dep < .5] = "Medium"
  scen_dep_tab$rel_dep_category[scen_dep_tab$relative_dep >=1 ] = "Very High"
  
  write.csv(scen_dep_tab, paste("months",start_month,"-",end_month,"Scenario Depletion Volume.csv"), row.names = F, quote = F)
}

write_these_tables = F
if(write_these_tables == TRUE){
  save_stream_dep_csv(start_month = 9, end_month = 11,  Flow_Diff_Daily = Flow_Diff_Daily)
  save_stream_dep_csv(start_month = 9, end_month = 10,  Flow_Diff_Daily = Flow_Diff_Daily)
  save_stream_dep_csv(start_month = 8, end_month = 11,  Flow_Diff_Daily = Flow_Diff_Daily)
  save_stream_dep_csv(start_month = 6, end_month = 12,  Flow_Diff_Daily = Flow_Diff_Daily)
  save_stream_dep_csv(start_month = 7, end_month = 11,  Flow_Diff_Daily = Flow_Diff_Daily)
  
}


# #.############################################################################################
# ####################        STRESS PERIOD AVERAGE DIFFERENCE PLOTS       ####################
# #.############################################################################################
# (MAR_Monthly_Diff_Plot = ggplot(Flow_Diff_SP_Avg, aes(x = Date, y = MAR_difference_cfs)) +
#     geom_hline(yintercept = 0) +
#     geom_line() +
#     geom_point() +
#     scale_y_continuous(limits = dif_lim, 
#                        breaks = seq(dif_lim[1], dif_lim[2],by = 10), 
#                        expand = c(0,0)) +
#    scale_x_date(limits = c(start_date,
#                            end_date),
#                 breaks = seq(start_date, by = "2 years", length.out = 22), expand = c(0,0),
#                 date_labels = ('%b-%y')) +
#     theme(panel.background = element_blank(),
#           panel.border = element_rect(fill=NA, color = 'black'),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#           axis.text.y = element_text(size = 8),
#           axis.ticks = element_line(size = 0.2),
#           plot.title = element_text(hjust = 0.5, size = 10),
#           axis.title.x = element_blank(),
#           axis.title.y = element_text(size = 8))
# )
# (ILR_Monthly_Diff_Plot = ggplot(Flow_Diff_SP_Avg, aes(x = Date, y = ILR_difference_cfs)) +
#     geom_hline(yintercept = 0) +
#     geom_line() +
#     geom_point() +
#     scale_y_continuous(limits = dif_lim, 
#                        breaks = seq(dif_lim[1], dif_lim[2],by = 10), expand = c(0,0)) +
#     scale_x_date(limits = c(start_date,
#                             end_date),
#                  breaks = seq(start_date, by = "2 years", length.out = 22), expand = c(0,0),
#                  date_labels = ('%b-%y')) +
#     theme(panel.background = element_blank(),
#           panel.border = element_rect(fill=NA, color = 'black'),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#           axis.text.y = element_text(size = 8),
#           axis.ticks = element_line(size = 0.2),
#           plot.title = element_text(hjust = 0.5, size = 10),
#           axis.title.x = element_blank(),
#           axis.title.y = element_text(size = 8))
# )
# (MAR_ILR_Monthly_Diff_Plot = ggplot(Flow_Diff_SP_Avg, aes(x = Date, y = MAR_ILR_difference_cfs)) +
#     geom_hline(yintercept = 0) +
#     geom_line() +
#     geom_point() +
#     scale_y_continuous(limits = dif_lim, 
#                        breaks = seq(dif_lim[1], dif_lim[2],by = 10), expand = c(0,0)) +
#     scale_x_date(limits = c(start_date,
#                             end_date),
#                  breaks = seq(start_date, by = "2 years", length.out = 22), expand = c(0,0),
#                  date_labels = ('%b-%y')) +
#     theme(panel.background = element_blank(),
#           panel.border = element_rect(fill=NA, color = 'black'),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#           axis.text.y = element_text(size = 8),
#           axis.ticks = element_line(size = 0.2),
#           plot.title = element_text(hjust = 0.5, size = 10),
#           axis.title.x = element_blank(),
#           axis.title.y = element_text(size = 8))
# )
# 
# (Flowlim_Monthly_Diff_Plot = ggplot(Flow_Diff_SP_Avg, 
#                                     aes(x = Date, y = flowlims_difference_cfs)) +
#     geom_hline(yintercept = 0) +
#     geom_line() +
#     geom_point() +
#     scale_y_continuous(limits = dif_lim, 
#                        breaks = seq(dif_lim[1], dif_lim[2],by = 10), expand = c(0,0)) +
#     scale_x_date(limits = c(start_date,
#                             end_date),
#                  breaks = seq(start_date, by = "2 years", length.out = 22), expand = c(0,0),
#                  date_labels = ('%b-%y')) +
#     theme(panel.background = element_blank(),
#           panel.border = element_rect(fill=NA, color = 'black'),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#           axis.text.y = element_text(size = 8),
#           axis.ticks = element_line(size = 0.2),
#           plot.title = element_text(hjust = 0.5, size = 10),
#           axis.title.x = element_blank(),
#           axis.title.y = element_text(size = 8))
# )
# 
# (MAR_ILR_Flowlim_Monthly_Diff_Plot = ggplot(Flow_Diff_SP_Avg, 
#                                     aes(x = Date, y = mar_ilr_flowlims_difference_cfs)) +
#     geom_hline(yintercept = 0) +
#     geom_line() +
#     geom_point() +
#     scale_y_continuous(limits = dif_lim, 
#                        breaks = seq(dif_lim[1], dif_lim[2],by = 10), expand = c(0,0)) +
#     scale_x_date(limits = c(start_date,
#                             end_date),
#                  breaks = seq(start_date, by = "2 years", length.out = 22), expand = c(0,0),
#                  date_labels = ('%b-%y')) +
#     theme(panel.background = element_blank(),
#           panel.border = element_rect(fill=NA, color = 'black'),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#           axis.text.y = element_text(size = 8),
#           axis.ticks = element_line(size = 0.2),
#           plot.title = element_text(hjust = 0.5, size = 10),
#           axis.title.x = element_blank(),
#           axis.title.y = element_text(size = 8))
# )
# #.############################################################################################
# ####################           AVERAGE MONTHLY DIFFERENCE PLOTS          ####################
# #.############################################################################################
# (MAR_Monthly_Avg_Plot = ggplot(data = Flow_Diff_Monthly_Avg) + 
#     geom_ribbon(data = MAR_geom_ribbon_data[1:4,], aes(x = x,ymin = y, ymax = 0), fill = 'red', alpha=  0.5) +
#     geom_ribbon(data = MAR_geom_ribbon_data[4:13,], aes(x = x,ymin = 0, ymax = y), fill = 'dodgerblue', alpha=  0.5) + 
#     geom_hline(yintercept = 0) +
#     geom_line(aes(x = seq(1,12), y = MAR_difference_cfs, group = 1)) +
#     geom_errorbar(aes(x = seq(1,12), ymin = MAR_difference_cfs-MAR_difference_cfs_SD, 
#                       ymax = MAR_difference_cfs+MAR_difference_cfs_SD, group = 1), width = 0.25) +
#     geom_point(aes(x = seq(1,12), y = MAR_difference_cfs, group = 1),size = 1.5) +
#     scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = Flow_Diff_Monthly_Avg$Date) +
#     scale_y_continuous(limits = dif_lim, 
#                        breaks = seq(dif_lim[1], dif_lim[2],by = 20), expand = c(0,0)) +
#     theme(panel.background = element_blank(),
#           panel.border = element_rect(fill=NA, color = 'black'),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#           axis.text.y = element_text(size = 8),
#           axis.ticks = element_line(size = 0.2),
#           plot.title = element_text(hjust = 0.5, size = 10),
#           axis.title.x = element_blank(),
#           axis.title.y = element_text(size = 8))
# )
# (ILR_Monthly_Avg_Plot = ggplot(data = Flow_Diff_Monthly_Avg) + 
#     geom_ribbon(data = ILR_geom_ribbon_data[1:4,], aes(x = x,ymin = 0, ymax = y), fill = 'dodgerblue', alpha=  0.5) +
#     geom_ribbon(data = ILR_geom_ribbon_data[4:8,], aes(x = x,ymin = y, ymax = 0), fill = 'red', alpha=  0.5) +
#     geom_ribbon(data = ILR_geom_ribbon_data[8:14,], aes(x = x,ymin = 0, ymax = y), fill = 'dodgerblue', alpha=  0.5) +
#     geom_hline(yintercept = 0) +
#     geom_line(aes(x = seq(1,12), y = ILR_difference_cfs, group = 1)) +
#     geom_errorbar(aes(x = seq(1,12), ymin = ILR_difference_cfs-ILR_difference_cfs_SD, 
#                       ymax = ILR_difference_cfs+ILR_difference_cfs_SD, group = 1), width = 0.25) +
#     geom_point(aes(x = seq(1,12), y = ILR_difference_cfs, group = 1),size = 1.5) +
#     scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = Flow_Diff_Monthly_Avg$Date) +
#     scale_y_continuous(limits = dif_lim, 
#                        breaks = seq(dif_lim[1], dif_lim[2],by = 20), expand = c(0,0)) +
#     theme(panel.background = element_blank(),
#           panel.border = element_rect(fill=NA, color = 'black'),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#           axis.text.y = element_text(size = 8),
#           axis.ticks = element_line(size = 0.2),
#           plot.title = element_text(hjust = 0.5, size = 10),
#           axis.title.x = element_blank(),
#           axis.title.y = element_text(size = 8))
# )
# (MAR_ILR_Monthly_Avg_Plot = ggplot(data = Flow_Diff_Monthly_Avg) + 
#     geom_ribbon(data = MAR_ILR_geom_ribbon_data[1:4,], aes(x = x,ymin = 0, ymax = y), fill = 'red', alpha=  0.5) +
#     geom_ribbon(data = MAR_ILR_geom_ribbon_data[4:6,], aes(x = x,ymin = y, ymax = 0), fill = 'dodgerblue', alpha=  0.5) +
#     geom_ribbon(data = MAR_ILR_geom_ribbon_data[6:9,], aes(x = x,ymin = 0, ymax = y), fill = 'red', alpha=  0.5) +
#     geom_ribbon(data = MAR_ILR_geom_ribbon_data[9:15,], aes(x = x,ymin = 0, ymax = y), fill = 'dodgerblue', alpha=  0.5) +
#     geom_hline(yintercept = 0) +
#     geom_line(aes(x = seq(1,12), y = MAR_ILR_difference_cfs, group = 1)) +
#     geom_point(aes(x = seq(1,12), y = MAR_ILR_difference_cfs, group = 1),size = 1.5) +
#     geom_errorbar(aes(x = seq(1,12), ymin = MAR_ILR_difference_cfs-MAR_ILR_difference_cfs_SD, 
#                       ymax = MAR_ILR_difference_cfs+MAR_ILR_difference_cfs_SD, group = 1), width = 0.25) +
#     scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = Flow_Diff_Monthly_Avg$Date) +
#     scale_y_continuous(limits = dif_lim, 
#                        breaks = seq(dif_lim[1], dif_lim[2],by = 20), expand = c(0,0)) +
#     theme(panel.background = element_blank(),
#           panel.border = element_rect(fill=NA, color = 'black'),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#           axis.text.y = element_text(size = 8),
#           axis.ticks = element_line(size = 0.2),
#           plot.title = element_text(hjust = 0.5, size = 10),
#           axis.title.x = element_blank(),
#           axis.title.y = element_text(size = 8))
# )
# 
# (Flowlims_Monthly_Avg_Plot = ggplot(data = Flow_Diff_Monthly_Avg) + 
#     geom_ribbon(data = flowlims_geom_ribbon_data[1:12,], aes(x = x,ymin = y, ymax = 0), fill = 'dodgerblue', alpha=  0.5) +
#     geom_hline(yintercept = 0) +
#     geom_line(aes(x = seq(1,12), y = flowlims_difference_cfs, group = 1)) +
#     geom_point(aes(x = seq(1,12), y = flowlims_difference_cfs, group = 1),size = 1.5) +
#     geom_errorbar(aes(x = seq(1,12), ymin = flowlims_difference_cfs-flowlims_difference_cfs_SD, 
#                       ymax = flowlims_difference_cfs+flowlims_difference_cfs_SD, group = 1), width = 0.25) +
#     scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = Flow_Diff_Monthly_Avg$Date) +
#     scale_y_continuous(limits = dif_lim, 
#                        breaks = seq(dif_lim[1], dif_lim[2],by = 20), expand = c(0,0)) +
#     theme(panel.background = element_blank(),
#           panel.border = element_rect(fill=NA, color = 'black'),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#           axis.text.y = element_text(size = 8),
#           axis.ticks = element_line(size = 0.2),
#           plot.title = element_text(hjust = 0.5, size = 10),
#           axis.title.x = element_blank(),
#           axis.title.y = element_text(size = 8))
# )
# 
# (MAR_ILR_Flowlims_Monthly_Avg_Plot = ggplot(data = Flow_Diff_Monthly_Avg) + 
#     geom_ribbon(data = MAR_ILR_Flowlims_geom_ribbon_data[1:4,], aes(x = x,ymin = 0, ymax = y), fill = 'red', alpha=  0.5) +
#     geom_ribbon(data = MAR_ILR_Flowlims_geom_ribbon_data[4:6,], aes(x = x,ymin = y, ymax = 0), fill = 'dodgerblue', alpha=  0.5) +
#     geom_ribbon(data = MAR_ILR_Flowlims_geom_ribbon_data[6:8,], aes(x = x,ymin = 0, ymax = y), fill = 'red', alpha=  0.5) +
#     geom_ribbon(data = MAR_ILR_Flowlims_geom_ribbon_data[8:15,], aes(x = x,ymin = 0, ymax = y), fill = 'dodgerblue', alpha=  0.5) +
#     geom_hline(yintercept = 0) +
#     geom_line(aes(x = seq(1,12), y = mar_ilr_flowlims_difference_cfs, group = 1)) +
#     geom_point(aes(x = seq(1,12), y = mar_ilr_flowlims_difference_cfs, group = 1),size = 1.5) +
#     geom_errorbar(aes(x = seq(1,12), ymin = mar_ilr_flowlims_difference_cfs-mar_ilr_flowlims_difference_cfs_SD, 
#                       ymax = mar_ilr_flowlims_difference_cfs+mar_ilr_flowlims_difference_cfs_SD, group = 1), width = 0.25) +
#     scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = Flow_Diff_Monthly_Avg$Date) +
#     scale_y_continuous(limits = dif_lim, 
#                        breaks = seq(dif_lim[1], dif_lim[2],by = 20), expand = c(0,0)) +
#     theme(panel.background = element_blank(),
#           panel.border = element_rect(fill=NA, color = 'black'),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#           axis.text.y = element_text(size = 8),
#           axis.ticks = element_line(size = 0.2),
#           plot.title = element_text(hjust = 0.5, size = 10),
#           axis.title.x = element_blank(),
#           axis.title.y = element_text(size = 8))
# )
# (Irrig_0.8_Monthly_Avg_Plot = ggplot(data = Flow_Diff_Monthly_Avg) + 
#     geom_ribbon(data = irrig_0.8_geom_ribbon_data[1:12,], aes(x = x,ymin = y, ymax = 0), fill = 'dodgerblue', alpha=  0.5) +
#     geom_hline(yintercept = 0) +
#     geom_line(aes(x = seq(1,12), y = irrig_0.8_difference_cfs, group = 1)) +
#     geom_point(aes(x = seq(1,12), y = irrig_0.8_difference_cfs, group = 1),size = 1.5) +
#     geom_errorbar(aes(x = seq(1,12), ymin = irrig_0.8_difference_cfs-irrig_0.8_difference_cfs_SD, 
#                       ymax = irrig_0.8_difference_cfs+irrig_0.8_difference_cfs_SD, group = 1), width = 0.25) +
#     scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = Flow_Diff_Monthly_Avg$Date) +
#     scale_y_continuous(limits = dif_lim, 
#                        breaks = seq(dif_lim[1], dif_lim[2],by = 20), expand = c(0,0)) +
#     theme(panel.background = element_blank(),
#           panel.border = element_rect(fill=NA, color = 'black'),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#           axis.text.y = element_text(size = 8),
#           axis.ticks = element_line(size = 0.2),
#           plot.title = element_text(hjust = 0.5, size = 10),
#           axis.title.x = element_blank(),
#           axis.title.y = element_text(size = 8))
# )
# (Irrig_0.9_Monthly_Avg_Plot = ggplot(data = Flow_Diff_Monthly_Avg) + 
#     geom_ribbon(data = irrig_0.9_geom_ribbon_data[1:12,], aes(x = x,ymin = y, ymax = 0), fill = 'dodgerblue', alpha=  0.5) +
#     geom_hline(yintercept = 0) +
#     geom_line(aes(x = seq(1,12), y = irrig_0.9_difference_cfs, group = 1)) +
#     geom_point(aes(x = seq(1,12), y = irrig_0.9_difference_cfs, group = 1),size = 1.5) +
#     geom_errorbar(aes(x = seq(1,12), ymin = irrig_0.9_difference_cfs-irrig_0.9_difference_cfs_SD, 
#                       ymax = irrig_0.9_difference_cfs+irrig_0.9_difference_cfs_SD, group = 1), width = 0.25) +
#     scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = Flow_Diff_Monthly_Avg$Date) +
#     scale_y_continuous(limits = dif_lim, 
#                        breaks = seq(dif_lim[1], dif_lim[2],by = 20), expand = c(0,0)) +
#     theme(panel.background = element_blank(),
#           panel.border = element_rect(fill=NA, color = 'black'),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#           axis.text.y = element_text(size = 8),
#           axis.ticks = element_line(size = 0.2),
#           plot.title = element_text(hjust = 0.5, size = 10),
#           axis.title.x = element_blank(),
#           axis.title.y = element_text(size = 8))
# )
# 
# 
# nvoa_Monthly_Avg_Plot = ggplot(data = Flow_Diff_Monthly_Avg) + 
#     geom_ribbon(data = nvoa_geom_ribbon_data[1:4,], aes(x = x,ymin = y, ymax = 0), fill = 'red', alpha=  0.5) +
#     geom_ribbon(data = nvoa_geom_ribbon_data[4:13,], aes(x = x,ymin = 0, ymax = y), fill = 'dodgerblue', alpha=  0.5) + 
#     geom_hline(yintercept = 0) +
#     geom_line(aes(x = seq(1,12), y = nvoa_difference_cfs, group = 1)) +
#     geom_point(aes(x = seq(1,12), y = nvoa_difference_cfs, group = 1),size = 1.5) +
#     geom_errorbar(aes(x = seq(1,12), ymin = nvoa_difference_cfs-nvoa_difference_cfs_SD, 
#                       ymax = nvoa_difference_cfs+nvoa_difference_cfs_SD, group = 1), width = 0.25) +
#     scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = Flow_Diff_Monthly_Avg$Date) +
#     scale_y_continuous(limits = dif_lim, 
#                        breaks = seq(dif_lim[1], dif_lim[2],by = 20), expand = c(0,0)) +
#     theme(panel.background = element_blank(),
#           panel.border = element_rect(fill=NA, color = 'black'),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#           axis.text.y = element_text(size = 8),
#           axis.ticks = element_line(size = 0.2),
#           plot.title = element_text(hjust = 0.5, size = 10),
#           axis.title.x = element_blank(),
#           axis.title.y = element_text(size = 8))
# 
# 
# nvgwmoa_Monthly_Avg_Plot = ggplot(data = Flow_Diff_Monthly_Avg) + 
#     geom_ribbon(data = nvgwmoa_geom_ribbon_data[1:2,], aes(x = x,ymin = 0, ymax = y), fill = 'dodgerblue', alpha=  0.5) + 
#     geom_ribbon(data = nvgwmoa_geom_ribbon_data[2:4,], aes(x = x,ymin = y, ymax = 0), fill = 'red', alpha=  0.5) +
#     geom_ribbon(data = nvgwmoa_geom_ribbon_data[4:14,], aes(x = x,ymin = 0, ymax = y), fill = 'dodgerblue', alpha=  0.5) +
#     geom_hline(yintercept = 0) +
#     geom_line(aes(x = seq(1,12), y = nvgwmoa_difference_cfs, group = 1)) +
#     geom_point(aes(x = seq(1,12), y = nvgwmoa_difference_cfs, group = 1),size = 1.5) +
#     geom_errorbar(aes(x = seq(1,12), ymin = nvgwmoa_difference_cfs-nvgwmoa_difference_cfs_SD, 
#                       ymax = nvgwmoa_difference_cfs+nvgwmoa_difference_cfs_SD, group = 1), width = 0.25) +
#     scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = Flow_Diff_Monthly_Avg$Date) +
#     scale_y_continuous(limits = dif_lim, 
#                        breaks = seq(dif_lim[1], dif_lim[2],by = 20), expand = c(0,0)) +
#     theme(panel.background = element_blank(),
#           panel.border = element_rect(fill=NA, color = 'black'),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#           axis.text.y = element_text(size = 8),
#           axis.ticks = element_line(size = 0.2),
#           plot.title = element_text(hjust = 0.5, size = 10),
#           axis.title.x = element_blank(),
#           axis.title.y = element_text(size = 8))

# nvoa_1.0_Monthly_Avg_Plot = ggplot(data = Flow_Diff_Monthly_Avg) + 
#   geom_ribbon(data = nvoa_1.0_geom_ribbon_data[1:4,], aes(x = x,ymin = y, ymax = 0), fill = 'red', alpha=  0.5) +
#   geom_ribbon(data = nvoa_1.0_geom_ribbon_data[4:13,], aes(x = x,ymin = 0, ymax = y), fill = 'dodgerblue', alpha=  0.5) + 
#   geom_hline(yintercept = 0) +
#   geom_line(aes(x = seq(1,12), y = nvoa_1.0_difference_cfs, group = 1)) +
#   geom_point(aes(x = seq(1,12), y = nvoa_1.0_difference_cfs, group = 1),size = 1.5) +
#   geom_errorbar(aes(x = seq(1,12), ymin = nvoa_1.0_difference_cfs-nvoa_1.0_difference_cfs_SD, 
#                     ymax = nvoa_1.0_difference_cfs+nvoa_1.0_difference_cfs_SD, group = 1), width = 0.25) +
#   scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = Flow_Diff_Monthly_Avg$Date) +
#   scale_y_continuous(limits = dif_lim, 
#                      breaks = seq(dif_lim[1], dif_lim[2],by = 20), expand = c(0,0)) +
#   theme(panel.background = element_blank(),
#         panel.border = element_rect(fill=NA, color = 'black'),
#         axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#         axis.text.y = element_text(size = 8),
#         axis.ticks = element_line(size = 0.2),
#         plot.title = element_text(hjust = 0.5, size = 10),
#         axis.title.x = element_blank(),
#         axis.title.y = element_text(size = 8))
# 
# nvgwmoa_1.0_Monthly_Avg_Plot = ggplot(data = Flow_Diff_Monthly_Avg) + 
#   geom_ribbon(data = nvgwmoa_1.0_geom_ribbon_data[1:4,], aes(x = x,ymin = y, ymax = 0), fill = 'red', alpha=  0.5) +
#   geom_ribbon(data = nvgwmoa_1.0_geom_ribbon_data[4:13,], aes(x = x,ymin = 0, ymax = y), fill = 'dodgerblue', alpha=  0.5) + 
#   geom_hline(yintercept = 0) +
#   geom_line(aes(x = seq(1,12), y = nvgwmoa_1.0_difference_cfs, group = 1)) +
#   geom_point(aes(x = seq(1,12), y = nvgwmoa_1.0_difference_cfs, group = 1),size = 1.5) +
#   geom_errorbar(aes(x = seq(1,12), ymin = nvgwmoa_1.0_difference_cfs-nvgwmoa_1.0_difference_cfs_SD, 
#                     ymax = nvgwmoa_1.0_difference_cfs+nvgwmoa_1.0_difference_cfs_SD, group = 1), width = 0.25) +
#   scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = Flow_Diff_Monthly_Avg$Date) +
#   scale_y_continuous(limits = dif_lim, 
#                      breaks = seq(dif_lim[1], dif_lim[2],by = 20), expand = c(0,0)) +
#   theme(panel.background = element_blank(),
#         panel.border = element_rect(fill=NA, color = 'black'),
#         axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#         axis.text.y = element_text(size = 8),
#         axis.ticks = element_line(size = 0.2),
#         plot.title = element_text(hjust = 0.5, size = 10),
#         axis.title.x = element_blank(),
#         axis.title.y = element_text(size = 8))


# #.############################################################################################
# ###############           DAILY FLOW MONTHLY AVERAGES PLOTS         ###############
# #.############################################################################################
# 
# # Process so that the error bars show up
# Flow_Monthly_Avg$Flow_cfs_SD_down = Flow_Monthly_Avg$Flow_cfs_SD
# sd_bigger_than_avg = Flow_Monthly_Avg$Flow_cfs_SD_down>Flow_Monthly_Avg$Flow_cfs
# Flow_Monthly_Avg$Flow_cfs_SD_down[sd_bigger_than_avg] = Flow_Monthly_Avg$Flow_cfs[sd_bigger_than_avg] -1
# 
# (Monthly_Avg_Flow_Plot = ggplot(data = Flow_Monthly_Avg) + 
#    # geom_ribbon(data = nvgwmoa_geom_ribbon_data[1:2,], aes(x = x,ymin = 0, ymax = y), fill = 'dodgerblue', alpha=  0.5) + 
#    # geom_ribbon(data = nvgwmoa_geom_ribbon_data[2:4,], aes(x = x,ymin = y, ymax = 0), fill = 'red', alpha=  0.5) +
#    # geom_ribbon(data = nvgwmoa_geom_ribbon_data[4:14,], aes(x = x,ymin = 0, ymax = y), fill = 'dodgerblue', alpha=  0.5) +
#    geom_hline(yintercept = 0) +
#    geom_line(aes(x = seq(1,12), y = Flow_cfs, group = 1)) +
#    geom_point(aes(x = seq(1,12), y = Flow_cfs, group = 1),size = 1.5) +
#    geom_errorbar(aes(x = seq(1,12), ymin = Flow_cfs-Flow_cfs_SD_down, 
#                      ymax = Flow_cfs+Flow_cfs_SD, group = 1), width = 0.25) +
#    scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = Flow_Diff_Monthly_Avg$Date) +
#    scale_y_continuous(limits = c(1,2000), 
#                       trans="log10", breaks = 10^seq(0,4,by = 1),) +
#     annotation_logticks(sides = "l")+
#    theme(#panel.background = element_blank(),
#          panel.border = element_rect(fill=NA, color = 'black'),
#          axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#          axis.text.y = element_text(size = 8),
#          axis.ticks = element_line(size = 0.2),
#          plot.title = element_text(hjust = 0.5, size = 10),
#          axis.title.x = element_blank(),
#          axis.title.y = element_text(size = 8))
# )
# 
# #.############################################################################################
# ###############           DRY, AVERAGE, AND WET YEAR DIFFERENCE PLOTS         ###############
# #.############################################################################################
# Flow_Diff_SP_Dry_Avg_Wet = subset(Flow_Diff_SP_Avg,#select = c('Date','MAR_difference_cfs', 'ILR_difference_cfs', 'MAR_ILR_difference_cfs'), 
#                                   Date %in%c(seq(as.Date("2010/1/1"), by = "month", length.out = 12),
#                                              seq(as.Date("2014/1/1"), by = "month", length.out = 12),
#                                              seq(as.Date("2017/1/1"), by = "month", length.out = 12)))
# Flow_Diff_SP_Dry_Avg_Wet$Year_Type = rep(c('Average (2010)','Dry (2014)','Wet (2017)'), each=12)
# Flow_Diff_SP_Dry_Avg_Wet$Year_Type = factor(Flow_Diff_SP_Dry_Avg_Wet$Year_Type, levels = c('Dry (2014)','Average (2010)','Wet (2017)'))
# Flow_Diff_SP_Dry_Avg_Wet = Flow_Diff_SP_Dry_Avg_Wet[order(Flow_Diff_SP_Dry_Avg_Wet$Year_Type),]
# Flow_Diff_SP_Dry_Avg_Wet$Date = format(Flow_Diff_SP_Dry_Avg_Wet$Date, '%m')
# 
# (MAR_Dry_Avg_Wet_Diff_Plot = ggplot(data = Flow_Diff_SP_Dry_Avg_Wet, aes(x = as.numeric(Date), y = MAR_difference_cfs, group = Year_Type, color = Year_Type)) +
#    geom_hline(yintercept = 0) +
#    geom_line(size = 1) +
#    geom_point(size = 1.5) +
#    scale_y_continuous(limits = dif_lim, 
#                       breaks = seq(dif_lim[1], dif_lim[2],by = 20), expand = c(0,0)) +
#    scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = format(seq(as.Date("2001/1/1"), by = "month", length.out = 12),'%b')) +
#    scale_color_manual(values = c('orangered','darkgoldenrod2','cornflowerblue')) +
#    theme(panel.background = element_blank(),
#          panel.border = element_rect(fill=NA, color = 'black'),
#          axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#          axis.text.y = element_text(size = 8),
#          axis.ticks = element_line(size = 0.2),
#          plot.title = element_text(hjust = 0.5, size = 10),
#          axis.title.x = element_blank(),
#          axis.title.y = element_text(size = 8),
#          legend.key = element_blank(),
#          legend.title = element_blank(),
#          legend.position = c(0.80,0.15),
#          legend.background = element_blank())
# )
# (ILR_Dry_Avg_Wet_Diff_Plot = ggplot(data = Flow_Diff_SP_Dry_Avg_Wet, aes(x = as.numeric(Date), y = ILR_difference_cfs, group = Year_Type, color = Year_Type)) +
#     geom_hline(yintercept = 0) +
#     geom_line(size = 1) +
#     geom_point(size = 1.5) +
#     scale_y_continuous(limits = dif_lim, 
#                        breaks = seq(dif_lim[1], dif_lim[2],by = 20), expand = c(0,0)) +
#     scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = format(seq(as.Date("2001/1/1"), by = "month", length.out = 12),'%b')) +
#     scale_color_manual(values = c('orangered','darkgoldenrod2','cornflowerblue')) +
#     theme(panel.background = element_blank(),
#           panel.border = element_rect(fill=NA, color = 'black'),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#           axis.text.y = element_text(size = 8),
#           axis.ticks = element_line(size = 0.2),
#           plot.title = element_text(hjust = 0.5, size = 10),
#           axis.title.x = element_blank(),
#           axis.title.y = element_text(size = 8),
#           legend.key = element_blank(),
#           legend.title = element_blank(),
#           legend.position = c(0.80,0.15),
#           legend.background = element_blank())
# )
# (MAR_ILR_Dry_Avg_Wet_Diff_Plot = ggplot(data = Flow_Diff_SP_Dry_Avg_Wet, aes(x = as.numeric(Date), y = MAR_ILR_difference_cfs, group = Year_Type, color = Year_Type)) +
#     geom_hline(yintercept = 0) +
#     geom_line(size = 1) +
#     geom_point(size = 1.5) +
#     scale_y_continuous(limits = dif_lim, 
#                        breaks = seq(dif_lim[1], dif_lim[2],by = 20), expand = c(0,0)) +
#     scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = format(seq(as.Date("2001/1/1"), by = "month", length.out = 12),'%b')) +
#     scale_color_manual(values = c('orangered','darkgoldenrod2','cornflowerblue')) +
#     theme(panel.background = element_blank(),
#           panel.border = element_rect(fill=NA, color = 'black'),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#           axis.text.y = element_text(size = 8),
#           axis.ticks = element_line(size = 0.2),
#           plot.title = element_text(hjust = 0.5, size = 10),
#           axis.title.x = element_blank(),
#           axis.title.y = element_text(size = 8),
#           legend.key = element_blank(),
#           legend.title = element_blank(),
#           legend.position = c(0.80,0.15),
#           legend.background = element_blank())
# )
# 
# (Flowlims_Dry_Avg_Wet_Diff_Plot = ggplot(data = Flow_Diff_SP_Dry_Avg_Wet, aes(x = as.numeric(Date), y = flowlims_difference_cfs, group = Year_Type, color = Year_Type)) +
#     geom_hline(yintercept = 0) +
#     geom_line(size = 1) +
#     geom_point(size = 1.5) +
#     scale_y_continuous(limits = dif_lim, 
#                        breaks = seq(dif_lim[1], dif_lim[2],by = 20), expand = c(0,0)) +
#     scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = format(seq(as.Date("2001/1/1"), by = "month", length.out = 12),'%b')) +
#     scale_color_manual(values = c('orangered','darkgoldenrod2','cornflowerblue')) +
#     theme(panel.background = element_blank(),
#           panel.border = element_rect(fill=NA, color = 'black'),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#           axis.text.y = element_text(size = 8),
#           axis.ticks = element_line(size = 0.2),
#           plot.title = element_text(hjust = 0.5, size = 10),
#           axis.title.x = element_blank(),
#           axis.title.y = element_text(size = 8),
#           legend.key = element_blank(),
#           legend.title = element_blank(),
#           legend.position = c(0.80,0.15),
#           legend.background = element_blank())
# )
# 
# (MAR_ILR_Flowlims_Dry_Avg_Wet_Diff_Plot = ggplot(data = Flow_Diff_SP_Dry_Avg_Wet, aes(x = as.numeric(Date), y = mar_ilr_flowlims_difference_cfs, group = Year_Type, color = Year_Type)) +
#     geom_hline(yintercept = 0) +
#     geom_line(size = 1) +
#     geom_point(size = 1.5) +
#     scale_y_continuous(limits = dif_lim, 
#                        breaks = seq(dif_lim[1], dif_lim[2],by = 20), expand = c(0,0)) +
#     scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = format(seq(as.Date("2001/1/1"), by = "month", length.out = 12),'%b')) +
#     scale_color_manual(values = c('orangered','darkgoldenrod2','cornflowerblue')) +
#     theme(panel.background = element_blank(),
#           panel.border = element_rect(fill=NA, color = 'black'),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#           axis.text.y = element_text(size = 8),
#           axis.ticks = element_line(size = 0.2),
#           plot.title = element_text(hjust = 0.5, size = 10),
#           axis.title.x = element_blank(),
#           axis.title.y = element_text(size = 8),
#           legend.key = element_blank(),
#           legend.title = element_blank(),
#           legend.position = c(0.80,0.15),
#           legend.background = element_blank())
# )
# 
# (Irrig_0.8_Dry_Avg_Wet_Diff_Plot = ggplot(data = Flow_Diff_SP_Dry_Avg_Wet, aes(x = as.numeric(Date), y = irrig_0.8_difference_cfs, group = Year_Type, color = Year_Type)) +
#     geom_hline(yintercept = 0) +
#     geom_line(size = 1) +
#     geom_point(size = 1.5) +
#     scale_y_continuous(limits = dif_lim, 
#                        breaks = seq(dif_lim[1], dif_lim[2],by = 20), expand = c(0,0)) +
#     scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = format(seq(as.Date("2001/1/1"), by = "month", length.out = 12),'%b')) +
#     scale_color_manual(values = c('orangered','darkgoldenrod2','cornflowerblue')) +
#     theme(panel.background = element_blank(),
#           panel.border = element_rect(fill=NA, color = 'black'),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#           axis.text.y = element_text(size = 8),
#           axis.ticks = element_line(size = 0.2),
#           plot.title = element_text(hjust = 0.5, size = 10),
#           axis.title.x = element_blank(),
#           axis.title.y = element_text(size = 8),
#           legend.key = element_blank(),
#           legend.title = element_blank(),
#           legend.position = c(0.80,0.15),
#           legend.background = element_blank())
# )
# 
# (Irrig_0.9_Dry_Avg_Wet_Diff_Plot = ggplot(data = Flow_Diff_SP_Dry_Avg_Wet, 
#                                           aes(x = as.numeric(Date), 
#                                               y = irrig_0.9_difference_cfs, 
#                                               group = Year_Type, color = Year_Type)) +
#     geom_hline(yintercept = 0) +
#     geom_line(size = 1) +
#     geom_point(size = 1.5) +
#     scale_y_continuous(limits = dif_lim, 
#                        breaks = seq(dif_lim[1], dif_lim[2],by = 20), expand = c(0,0)) +
#     scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = format(seq(as.Date("2001/1/1"), by = "month", length.out = 12),'%b')) +
#     scale_color_manual(values = c('orangered','darkgoldenrod2','cornflowerblue')) +
#     theme(panel.background = element_blank(),
#           panel.border = element_rect(fill=NA, color = 'black'),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#           axis.text.y = element_text(size = 8),
#           axis.ticks = element_line(size = 0.2),
#           plot.title = element_text(hjust = 0.5, size = 10),
#           axis.title.x = element_blank(),
#           axis.title.y = element_text(size = 8),
#           legend.key = element_blank(),
#           legend.title = element_blank(),
#           legend.position = c(0.80,0.15),
#           legend.background = element_blank())
# )
# 
# (nvoa_Dry_Avg_Wet_Diff_Plot = ggplot(data = Flow_Diff_SP_Dry_Avg_Wet, 
#                                           aes(x = as.numeric(Date), 
#                                               y = nvoa_difference_cfs, 
#                                               group = Year_Type, color = Year_Type)) +
#     geom_hline(yintercept = 0) +
#     geom_line(size = 1) +
#     geom_point(size = 1.5) +
#     scale_y_continuous(limits = dif_lim, 
#                        breaks = seq(dif_lim[1], dif_lim[2],by = 20), expand = c(0,0)) +
#     scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = format(seq(as.Date("2001/1/1"), by = "month", length.out = 12),'%b')) +
#     scale_color_manual(values = c('orangered','darkgoldenrod2','cornflowerblue')) +
#     theme(panel.background = element_blank(),
#           panel.border = element_rect(fill=NA, color = 'black'),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#           axis.text.y = element_text(size = 8),
#           axis.ticks = element_line(size = 0.2),
#           plot.title = element_text(hjust = 0.5, size = 10),
#           axis.title.x = element_blank(),
#           axis.title.y = element_text(size = 8),
#           legend.key = element_blank(),
#           legend.title = element_blank(),
#           legend.position = c(0.80,0.15),
#           legend.background = element_blank())
# )
# 
# (nvgwmoa_Dry_Avg_Wet_Diff_Plot = ggplot(data = Flow_Diff_SP_Dry_Avg_Wet, 
#                                          aes(x = as.numeric(Date), 
#                                              y = nvgwmoa_difference_cfs, 
#                                              group = Year_Type, color = Year_Type)) +
#     geom_hline(yintercept = 0) +
#     geom_line(size = 1) +
#     geom_point(size = 1.5) +
#     scale_y_continuous(limits = dif_lim, 
#                        breaks = seq(dif_lim[1], dif_lim[2],by = 20), expand = c(0,0)) +
#     scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = format(seq(as.Date("2001/1/1"), by = "month", length.out = 12),'%b')) +
#     scale_color_manual(values = c('orangered','darkgoldenrod2','cornflowerblue')) +
#     theme(panel.background = element_blank(),
#           panel.border = element_rect(fill=NA, color = 'black'),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#           axis.text.y = element_text(size = 8),
#           axis.ticks = element_line(size = 0.2),
#           plot.title = element_text(hjust = 0.5, size = 10),
#           axis.title.x = element_blank(),
#           axis.title.y = element_text(size = 8),
#           legend.key = element_blank(),
#           legend.title = element_blank(),
#           legend.position = c(0.80,0.15),
#           legend.background = element_blank())
# )

# (nvoa_1.0_Dry_Avg_Wet_Diff_Plot = ggplot(data = Flow_Diff_SP_Dry_Avg_Wet, 
#                                             aes(x = as.numeric(Date), 
#                                                 y = nvoa_1.0_difference_cfs, 
#                                                 group = Year_Type, color = Year_Type)) +
#     geom_hline(yintercept = 0) +
#     geom_line(size = 1) +
#     geom_point(size = 1.5) +
#     scale_y_continuous(limits = dif_lim, 
#                        breaks = seq(dif_lim[1], dif_lim[2],by = 20), expand = c(0,0)) +
#     scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = format(seq(as.Date("2001/1/1"), by = "month", length.out = 12),'%b')) +
#     scale_color_manual(values = c('orangered','darkgoldenrod2','cornflowerblue')) +
#     theme(panel.background = element_blank(),
#           panel.border = element_rect(fill=NA, color = 'black'),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#           axis.text.y = element_text(size = 8),
#           axis.ticks = element_line(size = 0.2),
#           plot.title = element_text(hjust = 0.5, size = 10),
#           axis.title.x = element_blank(),
#           axis.title.y = element_text(size = 8),
#           legend.key = element_blank(),
#           legend.title = element_blank(),
#           legend.position = c(0.80,0.15),
#           legend.background = element_blank())
# )
# 
# (nvgwmoa_1.0_Dry_Avg_Wet_Diff_Plot = ggplot(data = Flow_Diff_SP_Dry_Avg_Wet, 
#                                              aes(x = as.numeric(Date), 
#                                                  y = nvgwmoa_1.0_difference_cfs, 
#                                                  group = Year_Type, color = Year_Type)) +
#     geom_hline(yintercept = 0) +
#     geom_line(size = 1) +
#     geom_point(size = 1.5) +
#     scale_y_continuous(limits = dif_lim, 
#                        breaks = seq(dif_lim[1], dif_lim[2],by = 20), expand = c(0,0)) +
#     scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = format(seq(as.Date("2001/1/1"), by = "month", length.out = 12),'%b')) +
#     scale_color_manual(values = c('orangered','darkgoldenrod2','cornflowerblue')) +
#     theme(panel.background = element_blank(),
#           panel.border = element_rect(fill=NA, color = 'black'),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#           axis.text.y = element_text(size = 8),
#           axis.ticks = element_line(size = 0.2),
#           plot.title = element_text(hjust = 0.5, size = 10),
#           axis.title.x = element_blank(),
#           axis.title.y = element_text(size = 8),
#           legend.key = element_blank(),
#           legend.title = element_blank(),
#           legend.position = c(0.80,0.15),
#           legend.background = element_blank())
# )


# #.############################################################################################
# ###############           DRY, AVERAGE, AND WET YEAR FLOW PLOTS         ###############
# #.############################################################################################
# 
# Flow_SP_Dry_Avg_Wet = subset(Flow_SP_Avg,
#                              #select = c('Date','MAR_difference_cfs', 'ILR_difference_cfs', 'MAR_ILR_difference_cfs'), 
#                              Date %in%c(seq(as.Date("2010/1/1"), by = "month", length.out = 12),
#                                         seq(as.Date("2014/1/1"), by = "month", length.out = 12),
#                                         seq(as.Date("2017/1/1"), by = "month", length.out = 12)))
# Flow_SP_Dry_Avg_Wet$Year_Type = rep(c('Average (2010)','Dry (2014)','Wet (2017)'), each=12)
# Flow_SP_Dry_Avg_Wet$Year_Type = factor(Flow_SP_Dry_Avg_Wet$Year_Type, levels = c('Dry (2014)','Average (2010)','Wet (2017)'))
# Flow_SP_Dry_Avg_Wet = Flow_SP_Dry_Avg_Wet[order(Flow_SP_Dry_Avg_Wet$Year_Type),]
# Flow_SP_Dry_Avg_Wet$Date = format(Flow_SP_Dry_Avg_Wet$Date, '%m')
# 
# (basecase_Dry_Avg_Wet_Flow_Plot = ggplot()+
#     # geom_line(data = Flow_SP_Dry_Avg_Wet, aes(x = as.numeric(Date), y = nvgwmoa_cfs, group = Year_Type, color = Year_Type), size = 1.5) +
#     geom_line(data = Flow_SP_Dry_Avg_Wet, aes(x = as.numeric(Date), y = basecase_cfs, group = Year_Type, color = Year_Type), linetype = "dashed", size = 0.8) +
#     geom_hline(yintercept = 0) +
#     # geom_line(size = 1) +
#     geom_point(size = 1.5) +
#     # scale_y_continuous(limits = dif_lim, 
#     #                    breaks = seq(dif_lim[1], dif_lim[2],by = 20), expand = c(0,0)) +
#     scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = format(seq(as.Date("2001/1/1"), by = "month", length.out = 12),'%b')) +
#     scale_y_continuous(limits = c(1,3000), 
#                        trans="log10", breaks = 10^seq(0,4,by = 1),) +
#     annotation_logticks(sides = "l")+
#     scale_color_manual(values = c('orangered','darkgoldenrod2','cornflowerblue')) +
# 
#     theme(#panel.background = element_blank(),
#           panel.border = element_rect(fill=NA, color = 'black'),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#           axis.text.y = element_text(size = 8),
#           axis.ticks = element_line(size = 0.2),
#           plot.title = element_text(hjust = 0.5, size = 10),
#           axis.title.x = element_blank(),
#           axis.title.y = element_text(size = 8),
#           legend.key = element_blank(),
#           legend.title = element_blank(),
#           legend.position = c(0.25,0.15),
#           legend.background = element_blank())
#   
# )
# 
# (MAR_Dry_Avg_Wet_Flow_Plot = ggplot()+
#     geom_line(data = Flow_SP_Dry_Avg_Wet, aes(x = as.numeric(Date), y = MAR_cfs, group = Year_Type, color = Year_Type), size = 1.5) +
#     geom_line(data = Flow_SP_Dry_Avg_Wet, aes(x = as.numeric(Date), y = basecase_cfs, group = Year_Type, color = Year_Type), linetype = "dashed", size = 0.8) +
#     geom_hline(yintercept = 0) +
#     # geom_line(size = 1) +
#     geom_point(size = 1.5) +
#     # scale_y_continuous(limits = dif_lim, 
#     #                    breaks = seq(dif_lim[1], dif_lim[2],by = 20), expand = c(0,0)) +
#     scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = format(seq(as.Date("2001/1/1"), by = "month", length.out = 12),'%b')) +
#     scale_y_continuous(limits = c(1,3000), 
#                        trans="log10", breaks = 10^seq(0,4,by = 1),) +
#     annotation_logticks(sides = "l")+
#     scale_color_manual(values = c('orangered','darkgoldenrod2','cornflowerblue')) +
#     theme(#panel.background = element_blank(),
#       panel.border = element_rect(fill=NA, color = 'black'),
#       axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#       axis.text.y = element_text(size = 8),
#       axis.ticks = element_line(size = 0.2),
#       plot.title = element_text(hjust = 0.5, size = 10),
#       axis.title.x = element_blank(),
#       axis.title.y = element_text(size = 8),
#       legend.key = element_blank(),
#       legend.title = element_blank(),
#       legend.position = c(0.25,0.15),
#       legend.background = element_blank())
# )
# 
# (ILR_Dry_Avg_Wet_Flow_Plot = ggplot()+
#     geom_line(data = Flow_SP_Dry_Avg_Wet, aes(x = as.numeric(Date), y = ILR_cfs, group = Year_Type, color = Year_Type), size = 1.5) +
#     geom_line(data = Flow_SP_Dry_Avg_Wet, aes(x = as.numeric(Date), y = basecase_cfs, group = Year_Type, color = Year_Type), linetype = "dashed", size = 0.8) +
#     geom_hline(yintercept = 0) +
#     # geom_line(size = 1) +
#     geom_point(size = 1.5) +
#     # scale_y_continuous(limits = dif_lim, 
#     #                    breaks = seq(dif_lim[1], dif_lim[2],by = 20), expand = c(0,0)) +
#     scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = format(seq(as.Date("2001/1/1"), by = "month", length.out = 12),'%b')) +
#     scale_y_continuous(limits = c(1,3000), 
#                        trans="log10", breaks = 10^seq(0,4,by = 1),) +
#     annotation_logticks(sides = "l")+
#     scale_color_manual(values = c('orangered','darkgoldenrod2','cornflowerblue')) +
#     theme(#panel.background = element_blank(),
#       panel.border = element_rect(fill=NA, color = 'black'),
#       axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#       axis.text.y = element_text(size = 8),
#       axis.ticks = element_line(size = 0.2),
#       plot.title = element_text(hjust = 0.5, size = 10),
#       axis.title.x = element_blank(),
#       axis.title.y = element_text(size = 8),
#       legend.key = element_blank(),
#       legend.title = element_blank(),
#       legend.position = c(0.25,0.15),
#       legend.background = element_blank())
# )
# 
# 
# 
# (MAR_ILR_Dry_Avg_Wet_Flow_Plot = ggplot()+
#     geom_line(data = Flow_SP_Dry_Avg_Wet, aes(x = as.numeric(Date), y = MAR_ILR_cfs, group = Year_Type, color = Year_Type), size = 1.5) +
#     geom_line(data = Flow_SP_Dry_Avg_Wet, aes(x = as.numeric(Date), y = basecase_cfs, group = Year_Type, color = Year_Type), linetype = "dashed", size = 0.8) +
#     geom_hline(yintercept = 0) +
#     # geom_line(size = 1) +
#     geom_point(size = 1.5) +
#     # scale_y_continuous(limits = dif_lim, 
#     #                    breaks = seq(dif_lim[1], dif_lim[2],by = 20), expand = c(0,0)) +
#     scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = format(seq(as.Date("2001/1/1"), by = "month", length.out = 12),'%b')) +
#     scale_y_continuous(limits = c(1,3000), 
#                        trans="log10", breaks = 10^seq(0,4,by = 1),) +
#     annotation_logticks(sides = "l")+
#     scale_color_manual(values = c('orangered','darkgoldenrod2','cornflowerblue')) +
#     theme(#panel.background = element_blank(),
#       panel.border = element_rect(fill=NA, color = 'black'),
#       axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#       axis.text.y = element_text(size = 8),
#       axis.ticks = element_line(size = 0.2),
#       plot.title = element_text(hjust = 0.5, size = 10),
#       axis.title.x = element_blank(),
#       axis.title.y = element_text(size = 8),
#       legend.key = element_blank(),
#       legend.title = element_blank(),
#       legend.position = c(0.25,0.15),
#       legend.background = element_blank())
# )
# 
# (irrig_0.9_Dry_Avg_Wet_Flow_Plot = ggplot()+
#     geom_line(data = Flow_SP_Dry_Avg_Wet, aes(x = as.numeric(Date), y = irrig_0.9_cfs, group = Year_Type, color = Year_Type), size = 1.5) +
#     geom_line(data = Flow_SP_Dry_Avg_Wet, aes(x = as.numeric(Date), y = basecase_cfs, group = Year_Type, color = Year_Type), linetype = "dashed", size = 0.8) +
#     geom_hline(yintercept = 0) +
#     # geom_line(size = 1) +
#     geom_point(size = 1.5) +
#     # scale_y_continuous(limits = dif_lim, 
#     #                    breaks = seq(dif_lim[1], dif_lim[2],by = 20), expand = c(0,0)) +
#     scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = format(seq(as.Date("2001/1/1"), by = "month", length.out = 12),'%b')) +
#     scale_y_continuous(limits = c(1,3000), 
#                        trans="log10", breaks = 10^seq(0,4,by = 1),) +
#     annotation_logticks(sides = "l")+
#     scale_color_manual(values = c('orangered','darkgoldenrod2','cornflowerblue')) +
#     theme(#panel.background = element_blank(),
#       panel.border = element_rect(fill=NA, color = 'black'),
#       axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#       axis.text.y = element_text(size = 8),
#       axis.ticks = element_line(size = 0.2),
#       plot.title = element_text(hjust = 0.5, size = 10),
#       axis.title.x = element_blank(),
#       axis.title.y = element_text(size = 8),
#       legend.key = element_blank(),
#       legend.title = element_blank(),
#       legend.position = c(0.25,0.15),
#       legend.background = element_blank())
# )
# 
# 
# (nvoa_Dry_Avg_Wet_Flow_Plot = ggplot()+
#     geom_line(data = Flow_SP_Dry_Avg_Wet, 
#               aes(x = as.numeric(Date), y = nvoa_cfs, group = Year_Type, color = Year_Type), size = 1.5) +
#     geom_line(data = Flow_SP_Dry_Avg_Wet, aes(x = as.numeric(Date), y = basecase_cfs, group = Year_Type, color = Year_Type), linetype = "dashed", size = 0.8) +
#     geom_hline(yintercept = 0) +
#     # geom_line(size = 1) +
#     geom_point(size = 1.5) +
#     # scale_y_continuous(limits = dif_lim, 
#     #                    breaks = seq(dif_lim[1], dif_lim[2],by = 20), expand = c(0,0)) +
#     scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = format(seq(as.Date("2001/1/1"), by = "month", length.out = 12),'%b')) +
#     scale_y_continuous(limits = c(1,3000), 
#                        trans="log10", breaks = 10^seq(0,4,by = 1),) +
#     annotation_logticks(sides = "l")+
#     scale_color_manual(values = c('orangered','darkgoldenrod2','cornflowerblue')) +
#     theme(#panel.background = element_blank(),
#       panel.border = element_rect(fill=NA, color = 'black'),
#       axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#       axis.text.y = element_text(size = 8),
#       axis.ticks = element_line(size = 0.2),
#       plot.title = element_text(hjust = 0.5, size = 10),
#       axis.title.x = element_blank(),
#       axis.title.y = element_text(size = 8),
#       legend.key = element_blank(),
#       legend.title = element_blank(),
#       legend.position = c(0.25,0.15),
#       legend.background = element_blank())
#   
# )
#   
# 
# (nvgwmoa_Dry_Avg_Wet_Flow_Plot = ggplot()+
#     geom_line(data = Flow_SP_Dry_Avg_Wet, aes(x = as.numeric(Date), y = nvgwmoa_cfs, group = Year_Type, color = Year_Type), size = 1.5) +
#     geom_line(data = Flow_SP_Dry_Avg_Wet, aes(x = as.numeric(Date), y = basecase_cfs, group = Year_Type, color = Year_Type), linetype = "dashed", size = 0.8) +
#     geom_hline(yintercept = 0) +
#     # geom_line(size = 1) +
#     geom_point(size = 1.5) +
#     # scale_y_continuous(limits = dif_lim, 
#     #                    breaks = seq(dif_lim[1], dif_lim[2],by = 20), expand = c(0,0)) +
#     scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = format(seq(as.Date("2001/1/1"), by = "month", length.out = 12),'%b')) +
#     scale_y_continuous(limits = c(1,3000), 
#                        trans="log10", breaks = 10^seq(0,4,by = 1),) +
#     annotation_logticks(sides = "l")+
#     scale_color_manual(values = c('orangered','darkgoldenrod2','cornflowerblue')) +
#     theme(#panel.background = element_blank(),
#       panel.border = element_rect(fill=NA, color = 'black'),
#       axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#       axis.text.y = element_text(size = 8),
#       axis.ticks = element_line(size = 0.2),
#       plot.title = element_text(hjust = 0.5, size = 10),
#       axis.title.x = element_blank(),
#       axis.title.y = element_text(size = 8),
#       legend.key = element_blank(),
#       legend.title = element_blank(),
#       legend.position = c(0.25,0.15),
#       legend.background = element_blank())
# )

# (nvoa_1.0_Dry_Avg_Wet_Flow_Plot = ggplot()+
#     geom_line(data = Flow_SP_Dry_Avg_Wet, 
#               aes(x = as.numeric(Date), y = nvoa_1.0_cfs, group = Year_Type, color = Year_Type), size = 1.5) +
#     geom_line(data = Flow_SP_Dry_Avg_Wet, aes(x = as.numeric(Date), y = basecase_cfs, group = Year_Type, color = Year_Type), linetype = "dashed", size = 0.8) +
#     geom_hline(yintercept = 0) +
#     # geom_line(size = 1) +
#     geom_point(size = 1.5) +
#     # scale_y_continuous(limits = dif_lim, 
#     #                    breaks = seq(dif_lim[1], dif_lim[2],by = 20), expand = c(0,0)) +
#     scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = format(seq(as.Date("2001/1/1"), by = "month", length.out = 12),'%b')) +
#     scale_y_continuous(limits = c(1,3000), 
#                        trans="log10", breaks = 10^seq(0,4,by = 1),) +
#     annotation_logticks(sides = "l")+
#     scale_color_manual(values = c('orangered','darkgoldenrod2','cornflowerblue')) +
#     theme(#panel.background = element_blank(),
#       panel.border = element_rect(fill=NA, color = 'black'),
#       axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#       axis.text.y = element_text(size = 8),
#       axis.ticks = element_line(size = 0.2),
#       plot.title = element_text(hjust = 0.5, size = 10),
#       axis.title.x = element_blank(),
#       axis.title.y = element_text(size = 8),
#       legend.key = element_blank(),
#       legend.title = element_blank(),
#       legend.position = c(0.25,0.15),
#       legend.background = element_blank())
#   
# )
# 
# 
# (nvgwmoa_1.0_Dry_Avg_Wet_Flow_Plot = 
#     ggplot()+
#     geom_line(data = Flow_SP_Dry_Avg_Wet, aes(x = as.numeric(Date), y = nvgwmoa_1.0_cfs, group = Year_Type, color = Year_Type), size = 1.5) +
#     geom_line(data = Flow_SP_Dry_Avg_Wet, aes(x = as.numeric(Date), y = basecase_cfs, group = Year_Type, color = Year_Type), linetype = "dashed", size = 0.8) +
#     geom_hline(yintercept = 0) +
#     # geom_line(size = 1) +
#     geom_point(size = 1.5) +
#     # scale_y_continuous(limits = dif_lim, 
#     #                    breaks = seq(dif_lim[1], dif_lim[2],by = 20), expand = c(0,0)) +
#     scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = format(seq(as.Date("2001/1/1"), by = "month", length.out = 12),'%b')) +
#     scale_y_continuous(limits = c(1,3000), 
#                        trans="log10", breaks = 10^seq(0,4,by = 1),) +
#     annotation_logticks(sides = "l")+
#     scale_color_manual(values = c('orangered','darkgoldenrod2','cornflowerblue')) +
#     theme(#panel.background = element_blank(),
#       panel.border = element_rect(fill=NA, color = 'black'),
#       axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#       axis.text.y = element_text(size = 8),
#       axis.ticks = element_line(size = 0.2),
#       plot.title = element_text(hjust = 0.5, size = 10),
#       axis.title.x = element_blank(),
#       axis.title.y = element_text(size = 8),
#       legend.key = element_blank(),
#       legend.title = element_blank(),
#       legend.position = c(0.25,0.15),
#       legend.background = element_blank())
# )

# Print figures to file ---------------------------------------------------

# if (graphics_type == 'pdf'){pdf(paste0(flow_loc_short,'_Avg_Flow_cfs.pdf'), width = 3.5, height = 3)
# } else if (graphics_type == 'png'){png(paste0(flow_loc_short,'_Avg_Flow_cfs.png'), width = 3.5, height = 2, units = 'in', res = 600 )}
# grid.newpage()
# # pushViewport(viewport(layout = grid.layout(1,2)))
# # vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# print(Monthly_Avg_Flow_Plot + 
#         ylab('Average Daily Streamflow (cfs)'),
#       # vp = vplayout(1,1)
#       )
# graphics.off()
# 
# 
# if (graphics_type == 'pdf'){pdf(paste0(flow_loc_short,'_basecase_Flow_cfs.pdf'), width = 7, height = 3)
# } else if (graphics_type == 'png'){png(paste0(flow_loc_short,'_basecase_Flow_cfs.png'), width = 7, height = 3, units = 'in', res = 600 )}
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(1,2)))
# vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# print(Monthly_Avg_Flow_Plot + 
#         ylab('Average Daily Streamflow (cfs)'),
#       vp = vplayout(1,1))
# print(basecase_Dry_Avg_Wet_Flow_Plot +
#         ylab(''),
#       vp = vplayout(1,2))
# graphics.off()
# 
# 
# 
# if (graphics_type == 'pdf'){pdf(paste0(flow_loc_short,'_MAR_Flow_Diff_cfs.pdf'), width = 7, height = 3)
# } else if (graphics_type == 'png'){png(paste0(flow_loc_short,'_MAR_Flow_Diff_cfs.png'), width = 7, height = 3, units = 'in', res = 600 )}
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(1,2)))
# vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# print(MAR_Monthly_Avg_Plot + 
#       ylab('Streamflow Difference (cfs)'),
#       vp = vplayout(1,1))
# print(MAR_Dry_Avg_Wet_Diff_Plot +
#        ylab(''),
#       vp = vplayout(1,2))
# graphics.off()
# 
# if (graphics_type == 'pdf'){pdf('ILR_Flow_Diff_cfs.pdf', width = 7, height = 3)
# } else if (graphics_type == 'png'){png(paste0(flow_loc_short,'_ILR_Flow_Diff_cfs.png'), width = 7, height = 3, units = 'in', res = 600 )}
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(1,2)))
# vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# print(ILR_Monthly_Avg_Plot + 
#         ylab('Streamflow Difference (cfs)'),
#       vp = vplayout(1,1))
# print(ILR_Dry_Avg_Wet_Diff_Plot +
#         ylab(''),
#       vp = vplayout(1,2))
# graphics.off()
# 
# if (graphics_type == 'pdf'){pdf('MAR_ILR_Flow_Diff_cfs.pdf', width = 7, height = 3)
# } else if (graphics_type == 'png'){ png(paste0(flow_loc_short,'_MAR_ILR_Flow_Diff_cfs.png'), width = 7, height = 3, units = 'in', res = 600 )}
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(1,2)))
# vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# print(MAR_ILR_Monthly_Avg_Plot + 
#         ylab('Streamflow Difference (cfs)'),
#       vp = vplayout(1,1))
# print(MAR_ILR_Dry_Avg_Wet_Diff_Plot +
#         ylab(''),
#       vp = vplayout(1,2))
# graphics.off()
# 
# if (graphics_type == 'pdf'){pdf('MAR_ILR_Flowlims_Flow_Diff_cfs.pdf', width = 7, height = 3)
# } else if (graphics_type == 'png'){ png(paste0(flow_loc_short,'_MAR_ILR_Flowlims_Flow_Diff_cfs.png'), width = 7, height = 3, units = 'in', res = 600 )}
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(1,2)))
# vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# print(MAR_ILR_Flowlims_Monthly_Avg_Plot + 
#         ylab('Streamflow Difference (cfs)'),
#       vp = vplayout(1,1))
# print(MAR_ILR_Flowlims_Dry_Avg_Wet_Diff_Plot +
#         ylab(''),
#       vp = vplayout(1,2))
# graphics.off()
# 
# if (graphics_type == 'pdf'){pdf('Flowlims_Flow_Diff_cfs.pdf', width = 7, height = 3)
# } else if (graphics_type == 'png'){ png(paste0(flow_loc_short,'_Flowlims_Flow_Diff_cfs.png'), width = 7, height = 3, units = 'in', res = 600 )}
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(1,2)))
# vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# print(Flowlims_Monthly_Avg_Plot + 
#         ylab('Streamflow Difference (cfs)'),
#       vp = vplayout(1,1))
# print(Flowlims_Dry_Avg_Wet_Diff_Plot +
#         ylab(''),
#       vp = vplayout(1,2))
# graphics.off()
# 
# if (graphics_type == 'pdf'){pdf('Irrig_0.8_Flow_Diff_cfs.pdf', width = 7, height = 3)
# } else if (graphics_type == 'png'){ png(paste0(flow_loc_short,'_Irrig_0.8_Flow_Diff_cfs.png'), width = 7, height = 3, units = 'in', res = 600 )}
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(1,2)))
# vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# print(Irrig_0.8_Monthly_Avg_Plot + 
#         ylab('Streamflow Difference (cfs)'),
#       vp = vplayout(1,1))
# print(Irrig_0.8_Dry_Avg_Wet_Diff_Plot +
#         ylab(''),
#       vp = vplayout(1,2))
# graphics.off()
# 
# if (graphics_type == 'pdf'){pdf('Irrig_0.9_Flow_Diff_cfs.pdf', width = 7, height = 3)
# } else if (graphics_type == 'png'){ png(paste0(flow_loc_short,'Irrig_0.9_Flow_Diff_cfs.png'), width = 7, height = 3, units = 'in', res = 600 )}
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(1,2)))
# vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# print(Irrig_0.9_Monthly_Avg_Plot + 
#         ylab('Streamflow Difference (cfs)'),
#       vp = vplayout(1,1))
# print(Irrig_0.9_Dry_Avg_Wet_Diff_Plot +
#         ylab(''),
#       vp = vplayout(1,2))
# graphics.off()
# 
# 
# if (graphics_type == 'pdf'){pdf('nvoa_Flow_Diff_cfs.pdf', width = 7, height = 3)
# } else if (graphics_type == 'png'){ png(paste0(flow_loc_short,'nvoa_Flow_Diff_cfs.png'), width = 7, height = 3, units = 'in', res = 600 )}
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(1,2)))
# vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# print(nvoa_Monthly_Avg_Plot + 
#         ylab('Streamflow Difference (cfs)'),
#       vp = vplayout(1,1))
# print(nvoa_Dry_Avg_Wet_Diff_Plot +
#         ylab(''),
#       vp = vplayout(1,2))
# graphics.off()
# 
# if (graphics_type == 'pdf'){pdf('nvgwmoa_Flow_Diff_cfs.pdf', width = 7, height = 3)
# } else if (graphics_type == 'png'){ png(paste0(flow_loc_short,'nvgwmoa_Flow_Diff_cfs.png'), width = 7, height = 3, units = 'in', res = 600 )}
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(1,2)))
# vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# print(nvgwmoa_Monthly_Avg_Plot + 
#         ylab('Streamflow Difference (cfs)'),
#       vp = vplayout(1,1))
# print(nvgwmoa_Dry_Avg_Wet_Diff_Plot +
#         ylab(''),
#       vp = vplayout(1,2))
# graphics.off()


# if (graphics_type == 'pdf'){pdf('nvoa_1.0_Flow_Diff_cfs.pdf', width = 7, height = 3)
# } else if (graphics_type == 'png'){ png(paste0(flow_loc_short,'nvoa__1.0_Flow_Diff_cfs.png'), width = 7, height = 3, units = 'in', res = 600 )}
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(1,2)))
# vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# print(nvoa_1.0_Monthly_Avg_Plot + 
#         ylab('Streamflow Difference (cfs)'),
#       vp = vplayout(1,1))
# print(nvoa_1.0_Dry_Avg_Wet_Diff_Plot +
#         ylab(''),
#       vp = vplayout(1,2))
# graphics.off()
# 
# if (graphics_type == 'pdf'){pdf('nvgwmoa_1.0_Flow_Diff_cfs.pdf', width = 7, height = 3)
# } else if (graphics_type == 'png'){ png(paste0(flow_loc_short,'nvgwmoa_1.0_Flow_Diff_cfs.png'), width = 7, height = 3, units = 'in', res = 600 )}
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(1,2)))
# vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# print(nvgwmoa_1.0_Monthly_Avg_Plot + 
#         ylab('Streamflow Difference (cfs)'),
#       vp = vplayout(1,1))
# print(nvgwmoa_1.0_Dry_Avg_Wet_Diff_Plot +
#         ylab(''),
#       vp = vplayout(1,2))
# graphics.off()


# if (graphics_type == 'pdf'){pdf(paste0(flow_loc_short,'_nvoa_Flow_cfs.pdf'), width = 7, height = 3)
# } else if (graphics_type == 'png'){png(paste0(flow_loc_short,'_nvoa_Flow_cfs.png'), width = 7, height = 3, units = 'in', res = 600 )}
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(1,2)))
# vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# print(Monthly_Avg_Flow_Plot + 
#         ylab('Average Daily Streamflow (cfs)'),
#       vp = vplayout(1,1))
# print(nvoa_Dry_Avg_Wet_Flow_Plot +
#         ylab(''),
#       vp = vplayout(1,2))
# graphics.off()
# 
# if (graphics_type == 'pdf'){pdf(paste0(flow_loc_short,'_MAR_Flow_cfs.pdf'), width = 7, height = 3)
# } else if (graphics_type == 'png'){png(paste0(flow_loc_short,'_MAR_Flow_cfs.png'), width = 7, height = 3, units = 'in', res = 600 )}
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(1,2)))
# vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# print(Monthly_Avg_Flow_Plot + 
#         ylab('Average Daily Streamflow (cfs)'),
#       vp = vplayout(1,1))
# print(MAR_Dry_Avg_Wet_Flow_Plot +
#         ylab(''),
#       vp = vplayout(1,2))
# graphics.off()
# 
# if (graphics_type == 'pdf'){pdf(paste0(flow_loc_short,'_ILR_Flow_cfs.pdf'), width = 7, height = 3)
# } else if (graphics_type == 'png'){png(paste0(flow_loc_short,'_ILR_Flow_cfs.png'), width = 7, height = 3, units = 'in', res = 600 )}
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(1,2)))
# vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# print(Monthly_Avg_Flow_Plot + 
#         ylab('Average Daily Streamflow (cfs)'),
#       vp = vplayout(1,1))
# print(ILR_Dry_Avg_Wet_Flow_Plot +
#         ylab(''),
#       vp = vplayout(1,2))
# graphics.off()
# 
# if (graphics_type == 'pdf'){pdf(paste0(flow_loc_short,'_MAR_ILR_Flow_cfs.pdf'), width = 7, height = 3)
# } else if (graphics_type == 'png'){png(paste0(flow_loc_short,'_MAR_ILR_Flow_cfs.png'), width = 7, height = 3, units = 'in', res = 600 )}
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(1,2)))
# vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# print(Monthly_Avg_Flow_Plot + 
#         ylab('Average Daily Streamflow (cfs)'),
#       vp = vplayout(1,1))
# print(MAR_ILR_Dry_Avg_Wet_Flow_Plot +
#         ylab(''),
#       vp = vplayout(1,2))
# graphics.off()
# 
# 
# if (graphics_type == 'pdf'){pdf(paste0(flow_loc_short,'_irrig_0.9_Flow_cfs.pdf'), width = 7, height = 3)
# } else if (graphics_type == 'png'){png(paste0(flow_loc_short,'_irrig_0.9_Flow_cfs.png'), width = 7, height = 3, units = 'in', res = 600 )}
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(1,2)))
# vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# print(Monthly_Avg_Flow_Plot + 
#         ylab('Average Daily Streamflow (cfs)'),
#       vp = vplayout(1,1))
# print(irrig_0.9_Dry_Avg_Wet_Flow_Plot +
#         ylab(''),
#       vp = vplayout(1,2))
# graphics.off()
# 
# 
# if (graphics_type == 'pdf'){pdf(paste0(flow_loc_short,'_nvgwmoa_Flow_cfs.pdf'), width = 7, height = 3)
# } else if (graphics_type == 'png'){png(paste0(flow_loc_short,'_nvgwmoa_Flow_cfs.png'), width = 7, height = 3, units = 'in', res = 600 )}
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(1,2)))
# vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# print(Monthly_Avg_Flow_Plot + 
#         ylab('Average Daily Streamflow (cfs)'),
#       vp = vplayout(1,1))
# print(nvgwmoa_Dry_Avg_Wet_Flow_Plot +
#         ylab(''),
#       vp = vplayout(1,2))
# graphics.off()





# ILR_Pumping_Reduction_Vol_Plot = ggplot(ILR_GW_Reduction, aes(x=Year, y = Reduction_TAF)) + 
#   geom_line() +
#   ylab('Volume (TAF)')+
#   ggtitle('ILR Groundwater Pumping Reduction') +
#   scale_x_continuous(limits = c(1990.5,end_wy+5), breaks = seq(start_wy,end_wy, by = 2), expand = c(0,0)) +
#   scale_y_continuous(limits = c(0,8), breaks = seq(0,8,by = 2), expand = c(0,0)) +
#   theme(panel.background = element_blank(),
#         panel.border = element_rect(fill=NA, color = 'black'),
#         axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 10),
#         axis.text.y = element_text(size = 10),
#         axis.ticks = element_line(size = 0.2),
#         plot.title = element_text(hjust = 0.5, size = 10),
#         axis.title.x = element_blank(),
#         axis.title.y = element_text(size = 12),
#         legend.key = element_blank(),
#         legend.title = element_blank(),
#         legend.position = c(0.80,0.15),
#         legend.background = element_blank())
# 
# ILR_Pumping_Reduction_Pct_Plot = ggplot(ILR_GW_Reduction, aes(x=Year, y = Reduction_pct)) + 
#   geom_line() +
#   ylab('Percent')+
#   ggtitle('ILR Groundwater Pumping Reduction') +
#   scale_x_continuous(limits = c(1990.5,end_wy+.5), breaks = seq(start_wy,end_wy, by = 2), expand = c(0,0)) +
#   scale_y_continuous(limits = c(0,20), breaks = seq(0,20,by = 5), expand = c(0,0)) +
#   theme(panel.background = element_blank(),
#         panel.border = element_rect(fill=NA, color = 'black'),
#         axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 10),
#         axis.text.y = element_text(size = 10),
#         axis.ticks = element_line(size = 0.2),
#         plot.title = element_text(hjust = 0.5, size = 10),
#         axis.title.x = element_blank(),
#         axis.title.y = element_text(size = 12),
#         legend.key = element_blank(),
#         legend.title = element_blank(),
#         legend.position = c(0.80,0.15),
#         legend.background = element_blank())
# 
# png('ILR_Pumping_Reductions.png', width = 7, height = 3, units = 'in', res = 600 )
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(1,2)))
# vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# print(ILR_Pumping_Reduction_Vol_Plot, vp = vplayout(1,1))
# print(ILR_Pumping_Reduction_Pct_Plot, vp = vplayout(1,2))
# graphics.off()


# png(paste0('MAR_Flow_Diff_cfs_Monthly_Avg.png'), res = 600, width = 5, height = 4, units = 'in')
# MAR_Monthly_Avg_Plot + ylab('Streamflow Difference (cfs)') + ggtitle('MAR')
# graphics.off()
# png(paste0('ILR_Flow_Diff_cfs_Monthly_Avg.png'), res = 600, width = 5, height = 4, units = 'in')
# ILR_Monthly_Avg_Plot + ylab('Streamflow Difference (cfs)') + ggtitle('ILR')
# graphics.off()
# png(paste0('MAR_ILR_Flow_Diff_cfs_Monthly_Avg.png'), res = 600, width = 5, height = 4, units = 'in')
# MAR_ILR_Monthly_Avg_Plot + ylab('Streamflow Difference (cfs)') + ggtitle('MAR + ILR')
# graphics.off()
# 
# png(paste0('MAR_Flow_Diff_Dry_Avg_Wet.png'), res = 600, width = 5, height = 4, units = 'in')
# MAR_Dry_Avg_Wet_Diff_Plot + ylab('Streamflow Difference (cfs)') + ggtitle('MAR')
# graphics.off()
# png(paste0('ILR_Flow_Diff_Dry_Avg_Wet.png'), res = 600, width = 5, height = 4, units = 'in')
# ILR_Dry_Avg_Wet_Diff_Plot + ylab('Streamflow Difference (cfs)') + ggtitle('ILR')
# graphics.off()
# png(paste0('MAR_ILR_Flow_Diff_Dry_Avg_Wet.png'), res = 600, width = 5, height = 4, units = 'in')
# MAR_ILR_Dry_Avg_Wet_Diff_Plot + ylab('Streamflow Difference (cfs)') + ggtitle('MAR + ILR')
# graphics.off()




# Setting a MT example ----------------------------------------------------


# dry_basecase = Flow_SP_Dry_Avg_Wet$basecase_cfs[Flow_SP_Dry_Avg_Wet$Year_Type == "Dry (2014)"]
# dry_nvgwmoa = Flow_SP_Dry_Avg_Wet$nvgwmoa_cfs[Flow_SP_Dry_Avg_Wet$Year_Type == "Dry (2014)"]
# dry_mar_ilr = Flow_SP_Dry_Avg_Wet$MAR_ILR_cfs[Flow_SP_Dry_Avg_Wet$Year_Type == "Dry (2014)"]
# avg_basecase = Flow_SP_Dry_Avg_Wet$basecase_cfs[Flow_SP_Dry_Avg_Wet$Year_Type == "Average (2010)"]
# avg_nvgwmoa = Flow_SP_Dry_Avg_Wet$nvgwmoa_cfs[Flow_SP_Dry_Avg_Wet$Year_Type == "Average (2010)"]
# avg_mar_ilr = Flow_SP_Dry_Avg_Wet$MAR_ILR_cfs[Flow_SP_Dry_Avg_Wet$Year_Type == "Average (2010)"]
# wet_basecase = Flow_SP_Dry_Avg_Wet$basecase_cfs[Flow_SP_Dry_Avg_Wet$Year_Type == "Wet (2017)"]
# wet_nvgwmoa = Flow_SP_Dry_Avg_Wet$nvgwmoa_cfs[Flow_SP_Dry_Avg_Wet$Year_Type == "Wet (2017)"]
# wet_mar_ilr = Flow_SP_Dry_Avg_Wet$MAR_ILR_cfs[Flow_SP_Dry_Avg_Wet$Year_Type == "Wet (2017)"]
# 
# png(filename = "MT mar_ilr 2.png", width = 8.9, height = 6.1, units = "in", res = 300)
# 
# par(mfrow = c(2,2))
# 
# mar=c(5,4,4,2)
# plot(dry_basecase, type = "l", lty = 1, log = "y", xaxt = "n",
#      main = "FJ Streamflow, NV-GWM-OA and Basecase", xlab = "Month",
#      col = "red", ylim = c(1, 2600), ylab = "Avg Daily Streamflow (cfs)")
# lines(dry_nvgwmoa, lty = 2, col = "red")
# lines(dry_mar_ilr, lty = 3, col = "red")
# lines(avg_basecase, lty = 1, col = "goldenrod")
# lines(avg_nvgwmoa, lty = 2, col = "goldenrod")
# lines(avg_mar_ilr, lty = 3, col = "goldenrod")
# lines(wet_basecase, lty = 1, col = "dodgerblue")
# lines(wet_nvgwmoa, lty = 2, col = "dodgerblue")
# lines(wet_mar_ilr, lty = 3, col = "dodgerblue")
# axis(side = 1, at = 1:12, labels = month.abb[1:12])
# grid()
# legend(x = "bottomleft" , lty = c(1,2,3), legend = c("Basecase", "NV-GWM-OA", "MAR+ILR"))
# 
# dmi_diff = dry_mar_ilr-dry_basecase; dmi_diff[dmi_diff <0]=0 
# dnv_diff = dry_nvgwmoa-dry_basecase; dnv_diff[dnv_diff<0]=0 
# round(dmi_diff/dnv_diff*100)
# 
# ami_diff = avg_mar_ilr-avg_basecase; ami_diff[ami_diff <0]=0 
# anv_diff = avg_nvgwmoa-avg_basecase; anv_diff[anv_diff<0]=0 
# round(ami_diff/anv_diff*100)
# 
# wmi_diff = wet_mar_ilr-wet_basecase; wmi_diff[wmi_diff <0]=0 
# wnv_diff = wet_nvgwmoa-wet_basecase; wnv_diff[wnv_diff<0]=0 
# round(wmi_diff/wnv_diff*100)
# 
# round((dry_mar_ilr-dry_basecase) / (dry_nvgwmoa-dry_basecase) * 100 )
# round((avg_mar_ilr-avg_basecase) / (avg_nvgwmoa-avg_basecase) * 100 )
# round((wet_mar_ilr-dry_basecase) / (wet_nvgwmoa-dry_basecase) * 100 )
# 
# 
# # Schematic MT setting
# 
# mar=c(3,3,2,2)
# plot(1:12, dry_nvgwmoa-dry_basecase, type = "l", #log = "y", 
#      col = "red", lwd = 2, lty = 1, ylim = c(-10, 80), xaxt = "n", xlab = "",
#      # main = "Total Stream Depletion, NVGWMOA - Basecase",
#      ylab = "Avg Daily Depletion (cfs)")
# lines(1:12, 0.75*(dry_nvgwmoa-dry_basecase), lty = 2, lwd = 2, col = "red")
# lines(1:12, 0.50*(dry_nvgwmoa-dry_basecase), lty = 3, lwd = 2, col = "red")
# axis(side = 1, at = 1:12, labels = month.abb[1:12])
# grid()
#  legend(x = "topleft", lty = c(1,2,3), legend = c("Total Depletion", "75% Reversed Depletion MT","50% Reversed Depletion MT"))
# abline(h=0, lwd=2, col = "darkgray")
# 
# plot(1:12, avg_nvgwmoa-avg_basecase, type = "l",  xaxt = "n", xlab = "", 
#      lwd = 2, col = "goldenrod", ylim = c(-10, 80),
#      #main = 
#      ylab = "Avg Daily Depletion (cfs)")
# lines(1:12, 0.75*(avg_nvgwmoa-avg_basecase), lty = 2, lwd = 2, col = "goldenrod")
# lines(1:12, 0.50*(avg_nvgwmoa-avg_basecase), lty = 3, lwd = 2, col = "goldenrod")
# axis(side = 1, at = 1:12, labels = month.abb[1:12])
# grid()
# # legend(x = "bottomright", lty = c(2,1), legend = c("Total Depletion", "50% Reversed Depletion MT"))
# abline(h=0, lwd=2, col = "darkgray")
# 
# 
# plot(1:12, wet_nvgwmoa-wet_basecase, lwd = 2, col = "dodgerblue",
#      ylim = c(-10, 80), type = "l", xaxt = "n", xlab = "",
#      ylab = "Avg Daily Depletion (cfs)")
# lines(1:12, 0.75*(wet_nvgwmoa-wet_basecase), lty = 2, lwd = 2, col = "dodgerblue")
# lines(1:12, 0.50*(wet_nvgwmoa-wet_basecase), lty = 3, lwd = 2, col = "dodgerblue")
# axis(side = 1, at = 1:12, labels = month.abb[1:12])
# grid()
# # legend(x = "bottomright", lty = c(2,1), legend = c("Total Depletion", "50% Reversed Depletion MT"))
# abline(h=0, lwd=2, col = "darkgray")
# 
# dev.off()
# 
# 
# 
# # Example MT setting - MAR + ILR scenario
# 
# png(filename = "MT mar_ilr 3.png", width = 8.9, height = 6.1, units = "in", res = 300)
# 
# mar=c(3,3,2,2)
# plot(1:12, dry_nvgwmoa-dry_basecase, type = "l", #log = "y", 
#      col = "red", lwd = 2, lty = 1, ylim = c(-10, 80), xaxt = "n", xlab = "",
#      # main = "Total Stream Depletion, NVGWMOA - Basecase",
#      ylab = "Avg Daily Depletion (cfs)")
# lines(1:12, (dry_mar_ilr-dry_basecase), lty = 2, lwd = 2, col = "red")
# # lines(1:12, 0.75*(dry_mar_ilr-dry_basecase), lty = 2, lwd = 2, col = "red")
# # lines(1:12, 0.50*(dry_nvgwmoa-dry_basecase), lty = 3, lwd = 2, col = "red")
# axis(side = 1, at = 1:12, labels = month.abb[1:12])
# grid()
# legend(x = "topleft", lty = c(1,2,3), legend = c("Total Depletion", "MAR+ILR Depletion"))
# # legend(x = "topleft", lty = c(1,2,3), legend = c("Total Depletion", "25% Reversed Depletion MT","50% Reversed Depletion MT"))
# abline(h=0, lwd=2, col = "darkgray")
# 
# plot(1:12, avg_nvgwmoa-avg_basecase, type = "l",  xaxt = "n", xlab = "", 
#      lwd = 2, col = "goldenrod", ylim = c(-10, 80),
#      #main = 
#      ylab = "Avg Daily Depletion (cfs)")
# lines(1:12, (avg_mar_ilr-avg_basecase), lty = 2, lwd = 2, col = "goldenrod")
# # lines(1:12, 0.75*(avg_nvgwmoa-avg_basecase), lty = 2, lwd = 2, col = "goldenrod")
# # lines(1:12, 0.50*(avg_nvgwmoa-avg_basecase), lty = 3, lwd = 2, col = "goldenrod")
# axis(side = 1, at = 1:12, labels = month.abb[1:12])
# grid()
# # legend(x = "bottomright", lty = c(2,1), legend = c("Total Depletion", "50% Reversed Depletion MT"))
# abline(h=0, lwd=2, col = "darkgray")
# 
# 
# plot(1:12, wet_nvgwmoa-wet_basecase, lwd = 2, col = "dodgerblue",
#      ylim = c(-10, 80), type = "l", xaxt = "n", xlab = "",
#      ylab = "Avg Daily Depletion (cfs)")
# lines(1:12, (wet_mar_ilr-wet_basecase), lty = 2, lwd = 2, col = "dodgerblue")
# # lines(1:12, 0.75*(wet_nvgwmoa-wet_basecase), lty = 2, lwd = 2, col = "dodgerblue")
# # lines(1:12, 0.50*(wet_nvgwmoa-wet_basecase), lty = 1, lwd = 2, col = "dodgerblue")
# axis(side = 1, at = 1:12, labels = month.abb[1:12])
# grid()
# # legend(x = "bottomright", lty = c(2,1), legend = c("Total Depletion", "50% Reversed Depletion MT"))
# abline(h=0, lwd=2, col = "darkgray")
# 
# dev.off()




# scratchwork -------------------------------------------------------------



# plot(1:12, dry_nvgwmoa-dry_basecase, type = "l", #log = "y", 
#      col = "red", lwd = 2, lty = 3, ylim = c(-30, 80), 
#      main = "Total Stream Depletion, NVGWMOA - Basecase",
#      ylab = "Average Daily Streamflow Difference (cfs)", xlab = "Month (Jan-Dec)")
# lines(1:12, avg_nvgwmoa-avg_basecase, lwd = 2, lty = 3, col = "goldenrod")
# lines(1:12, wet_nvgwmoa-wet_basecase, lwd = 2, lty = 3, col = "dodgerblue")
# grid()
# 
# lines(1:12, 0.5*(dry_nvgwmoa-dry_basecase), lty = 4, lwd = 2, col = "red")
# lines(1:12, 0.5*(avg_nvgwmoa-avg_basecase), lty = 4, lwd = 2, col = "goldenrod")
# lines(1:12, 0.5*(wet_nvgwmoa-wet_basecase), lty = 4, lwd = 2, col = "dodgerblue")







# if (COMPARE_MAR==TRUE){
#   Inflow_MAR = readLines('SVIHM_MAR.sfr')
#   Inflow_Seg1_MAR = as.numeric(sapply(lapply(strsplit(Inflow_MAR[grep(x = Inflow_MAR, pattern = '1  1  3  0')],' '),function(x){x[!x ==""]}),"[[",5))
#   Inflow_Seg1_MAR_diff = Inflow_Seg1_MAR - Inflow_Seg1_Basecase
#   MODFLOW_Seg1_Inflows$MAR = Inflow_Seg1_MAR
#   MODFLOW_Seg1_Inflows$MAR_diff = Inflow_Seg1_MAR_diff
#   
#   Inflow_Seg32_MAR = as.numeric(sapply(lapply(strsplit(Inflow_MAR[grep(x = Inflow_MAR, pattern = '32  1  0  10  0')],' '),function(x){x[!x ==""]}),"[[",6))
#   Inflow_Seg32_MAR_diff = Inflow_Seg32_MAR - Inflow_Seg32_Basecase
#   MODFLOW_Seg32_Inflows$MAR = Inflow_Seg32_MAR
#   MODFLOW_Seg32_Inflows$MAR_diff = Inflow_Seg32_MAR_diff
#   }
# if (COMPARE_ILR==TRUE){
#   Inflow_ILR = readLines('SVIHM_ILR.sfr')
#   Inflow_Seg1_ILR = as.numeric(sapply(lapply(strsplit(Inflow_ILR[grep(x = Inflow_ILR, pattern = '1  1  3  0')],' '),function(x){x[!x ==""]}),"[[",5))
#   Inflow_Seg1_ILR_diff = Inflow_Seg1_ILR - Inflow_Seg1_Basecase
#   MODFLOW_Seg1_Inflows$ILR = Inflow_Seg1_ILR
#   MODFLOW_Seg1_Inflows$ILR_diff = Inflow_Seg1_ILR_diff
#   
#   Inflow_Seg32_ILR = as.numeric(sapply(lapply(strsplit(Inflow_ILR[grep(x = Inflow_ILR, pattern = '32  1  0  10  0')],' '),function(x){x[!x ==""]}),"[[",6))
#   Inflow_Seg32_ILR_diff = Inflow_Seg32_ILR - Inflow_Seg32_Basecase
#   MODFLOW_Seg32_Inflows$ILR = Inflow_Seg32_ILR
#   MODFLOW_Seg32_Inflows$ILR_diff = Inflow_Seg32_ILR_diff
#   }
# if (COMPARE_MAR_ILR==TRUE){
#   Inflow_MAR_ILR = readLines('SVIHM_MAR_ILR.sfr')
#   Inflow_Seg1_MAR_ILR = as.numeric(sapply(lapply(strsplit(Inflow_MAR_ILR[grep(x = Inflow_MAR_ILR, pattern = '1  1  3  0')],' '),function(x){x[!x ==""]}),"[[",5))
#   Inflow_Seg1_MAR_ILR_diff = Inflow_Seg1_MAR_ILR - Inflow_Seg1_Basecase
#   MODFLOW_Seg1_Inflows$MAR_ILR = Inflow_Seg1_MAR_ILR
#   MODFLOW_Seg1_Inflows$MAR_ILR_diff = Inflow_Seg1_MAR_ILR_diff
#   
#   Inflow_Seg32_MAR_ILR = as.numeric(sapply(lapply(strsplit(Inflow_MAR_ILR[grep(x = Inflow_MAR_ILR, pattern = '32  1  0  10  0')],' '),function(x){x[!x ==""]}),"[[",6))
#   Inflow_Seg32_MAR_ILR_diff = Inflow_Seg32_MAR_ILR - Inflow_Seg32_Basecase
#   MODFLOW_Seg32_Inflows$MAR_ILR = Inflow_Seg32_MAR_ILR
#   MODFLOW_Seg32_Inflows$MAR_ILR_diff = Inflow_Seg32_MAR_ILR_diff
#   }



# ########################             MODFLOW INPUT PLOTS             ########################
# 
# MODFLOW_Seg1_Inflows_diff_melt = reshape2::melt(MODFLOW_Seg1_Inflows %>%
#                                                   select('Date','MAR_diff','ILR_diff','MAR_ILR_diff'),id.vars = 'Date')
# 
# (Seg1_Inflow_diff_Plot = ggplot(data = MODFLOW_Seg1_Inflows_diff_melt, aes(x = Date, y = value*0.000408734569, group = variable, color = variable)) +
#     geom_line() +
#     geom_point() +
#     scale_x_date(limits = c(start_date,
#                             as.Date('1995-11-01')),
#                  breaks = seq(start_date, by = "2 years", length.out = 22), expand = c(0,0),
#                  date_labels = ('%b-%y'))
# )
# MODFLOW_Seg32_Inflows_diff_melt = melt(MODFLOW_Seg32_Inflows%>%select('Date','MAR_diff','ILR_diff','MAR_ILR_diff'),id.vars = 'Date')
# (Seg32_Inflow_diff_Plot = ggplot(data = MODFLOW_Seg32_Inflows_diff_melt, aes(x = Date, y = value*0.000408734569, group = variable, color = variable)) +
#     geom_line() +
#     geom_point() +
#     scale_x_date(limits = c(start_date,
#                             end_date),
#                  breaks = seq(start_date, by = "2 years", length.out = 22), expand = c(0,0),
#                  date_labels = ('%b-%y'))
# )
# 
# 
# ########################            DAILY DIFFERENCE PLOTS           ########################
# (MAR_Daily_Diff_Plot = ggplot(Flow_Diff_Daily, aes(x = Date, y = MAR_difference_cfs)) +
#    geom_line() +
#    #geom_line(data =MODFLOW_Seg1_Inflows, aes(x = Date, y = Basecase*0.000408734569/10), color = 'red') +
#    geom_hline(yintercept = 0) +
#    #scale_y_continuous(limits = c(-50,50), breaks = seq(-50,50,by = 10), expand = c(0,0)) +
#    scale_x_date(limits = c(start_date,
#                            end_date),
#                 breaks = seq(start_date, by = "2 years", length.out = 22), expand = c(0,0),
#                 date_labels = ('%b-%y')) +
#    theme(panel.background = element_blank(),
#          panel.border = element_rect(fill=NA, color = 'black'),
#          axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#          axis.text.y = element_text(size = 8),
#          axis.ticks = element_line(size = 0.2),
#          plot.title = element_text(hjust = 0.5, size = 10),
#          axis.title.x = element_blank(),
#          axis.title.y = element_text(size = 8))
# )
# (ILR_Daily_Diff_Plot = ggplot(Flow_Diff_Daily, aes(x = Date, y = ILR_difference_cfs)) +
#     geom_line() +
#     geom_hline(yintercept = 0) +
#     scale_y_continuous(limits = c(-90,90), breaks = seq(-90,90,by = 30), expand = c(0,0)) +
#     scale_x_date(limits = c(start_date,
#                             end_date),
#                  breaks = seq(start_date, by = "2 years", length.out = 22), expand = c(0,0),
#                  date_labels = ('%b-%y')) +
#     theme(panel.background = element_blank(),
#           panel.border = element_rect(fill=NA, color = 'black'),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#           axis.text.y = element_text(size = 8),
#           axis.ticks = element_line(size = 0.2),
#           plot.title = element_text(hjust = 0.5, size = 10),
#           axis.title.x = element_blank(),
#           axis.title.y = element_text(size = 8))
# )
# (MAR_ILR_Daily_Diff_Plot = ggplot(Flow_Diff_Daily, aes(x = Date, y = MAR_ILR_difference_cfs)) +
#     geom_line() +
#     geom_hline(yintercept = 0) +
#     scale_y_continuous(limits = c(-100,100), breaks = seq(-100,100,by = 25), expand = c(0,0)) +
#     scale_x_date(limits = c(start_date,
#                             end_date),
#                  breaks = seq(start_date, by = "2 years", length.out = 22), expand = c(0,0),
#                  date_labels = ('%b-%y')) +
#     theme(panel.background = element_blank(),
#           panel.border = element_rect(fill=NA, color = 'black'),
#           axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
#           axis.text.y = element_text(size = 8),
#           axis.ticks = element_line(size = 0.2),
#           plot.title = element_text(hjust = 0.5, size = 10),
#           axis.title.x = element_blank(),
#           axis.title.y = element_text(size = 8))
# )