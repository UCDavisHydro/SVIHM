# Script for post-processing changes to streamflow resulting from implementing different conjunctive use scenarios
rm(list=ls())

library(ggplot2)
library(lubridate)
library(magrittr)
library(reshape2)
library(dplyr)
library(grid)
library(stringr)
library(tidyr)
#############################################################################################
############################              USER INPUT             ############################
#############################################################################################

# scenario_ids = c("Basecase","MAR")
# scenario_ids = c("Basecase","ILR")
# scenario_ids = c("MAR","ILR","MAR_ILR")
# scenario_ids = c("flowlims","MAR_ILR","mar_ilr_flowlims")
# scenario_ids = c("MAR_ILR","mar_ilr_flowlims", "irrig_0.8", "irrig_0.9")
# scenario_ids = c("Basecase","irrig_0.8","irrig_0.9")
# scenario_ids = c("Basecase","alf_ir_stop_jul10")
scenario_ids = c("MAR","ILR","MAR_ILR","irrig_0.8","irrig_0.9",
               "nvoa0.05","nvgwmoa0.05","nvoa0.05_1.0","nvgwmoa0.05_1.0")


# COMPARE_MAR = TRUE
# COMPARE_ILR = TRUE
# COMPARE_MAR_ILR = TRUE
graphics_type = 'png'    #output type for graphics, currently pdf or png

# Select Flow Change location
  flow_loc = 'Streamflow_FJ_SVIHM'; flow_loc_short = "FJ"
# flow_loc = 'Streamflow_Pred_Loc_2'; flow_loc_short = "PL2"
# flow_loc = 'Streamflow_Pred_Loc_3'; flow_loc_short = "PL3"

results_dir = "C:/Users/Claire/Documents/GitHub/SVIHM/R_Files/Post-Processing/Results"
setwd(results_dir)
start_date = as.Date("1990-10-01")
end_date = as.Date("2018-09-30")
start_wy = 1991
end_wy = 2018
num_stress_periods = length(seq(start_date, end_date, by="month")); nsp = num_stress_periods

dif_lim = c(-80,140)
#.############################################################################################
############################             IMPORT DATA             ############################
#.############################################################################################
DP_Basecase_flow = data.frame(Date = seq(start_date, end_date, "days"),                         # Import Basecase flow data
                              Flow_m3day = read.table(paste0(flow_loc,"_basecase.dat"), skip = 2)[,3],
                              Flow_cfs = read.table(paste0(flow_loc,"_basecase.dat"), skip = 2)[,3]*0.000408734569)
DP_MAR_flow = data.frame(Date = seq(start_date, end_date, "days"),                              # Import MAR flow data
                         Flow_m3day = read.table(paste0(flow_loc,'_MAR.dat'), skip = 2)[,3],
                         Flow_cfs = read.table(paste0(flow_loc,'_MAR.dat'), skip = 2)[,3]*0.000408734569)
DP_ILR_flow = data.frame(Date = seq(start_date, end_date, "days"),                              # Import ILR flow data
                         Flow_m3day = read.table(paste0(flow_loc,'_ILR.dat'), skip = 2)[,3],
                         Flow_cfs = read.table(paste0(flow_loc,'_ILR.dat'), skip = 2)[,3]*0.000408734569)
DP_MAR_ILR_flow = data.frame(Date = seq(start_date, end_date, "days"),                          # Import MAR_ILR flow data
                             Flow_m3day = read.table(paste0(flow_loc,'_MAR_ILR.dat'), skip = 2)[,3],
                             Flow_cfs = read.table(paste0(flow_loc,'_MAR_ILR.dat'), skip = 2)[,3]*0.000408734569)

DP_Basecase_fl_flow = data.frame(Date = seq(start_date, end_date, "days"),                         # Import Basecase flow data
                              Flow_m3day = read.table(paste0(flow_loc,'_flowlims.dat'), skip = 2)[,3],
                              Flow_cfs = read.table(paste0(flow_loc,'_flowlims.dat'), skip = 2)[,3]*0.000408734569)
# DP_MAR_fl_flow = data.frame(Date = seq(start_date, end_date, "days"),                              # Import MAR flow data
#                          Flow_m3day = read.table(paste0(flow_loc,'MAR_flowlims.dat'), skip = 2)[,3],
#                          Flow_cfs = read.table(paste0(flow_loc,'MAR_flowlims.dat'), skip = 2)[,3]*0.000408734569)
# DP_ILR_fl_flow = data.frame(Date = seq(start_date, end_date, "days"),                              # Import ILR flow data
#                          Flow_m3day = read.table(paste0(flow_loc,'ILR_flowlims.dat'), skip = 2)[,3],
#                          Flow_cfs = read.table(paste0(flow_loc,'ILR_flowlims.dat'), skip = 2)[,3]*0.000408734569)
DP_MAR_ILR_fl_flow = data.frame(Date = seq(start_date, end_date, "days"),                          # Import MAR_ILR flow data
                             Flow_m3day = read.table(paste0(flow_loc,'_MAR_ILR_flowlims.dat'), skip = 2)[,3],
                             Flow_cfs = read.table(paste0(flow_loc,'_MAR_ILR_flowlims.dat'), skip = 2)[,3]*0.000408734569)

DP_0.8_flow = data.frame(Date = seq(start_date, end_date, "days"),                              # Import ILR flow data
                            Flow_m3day = read.table(paste0(flow_loc,'_irrig_0.8.dat'), skip = 2)[,3],
                            Flow_cfs = read.table(paste0(flow_loc,'_irrig_0.8.dat'), skip = 2)[,3]*0.000408734569)
DP_0.9_flow = data.frame(Date = seq(start_date, end_date, "days"),                              # Import ILR flow data
                         Flow_m3day = read.table(paste0(flow_loc,'_irrig_0.9.dat'), skip = 2)[,3],
                         Flow_cfs = read.table(paste0(flow_loc,'_irrig_0.9.dat'), skip = 2)[,3]*0.000408734569)

# DP_alf_irr_jul10_flow = data.frame(Date = seq(start_date, end_date, "days"),                              # Import ILR flow data
#                          Flow_m3day = read.table(paste0(flow_loc,'_alf_irr_stop_jul10.dat'), skip = 2)[,3],
#                          Flow_cfs = read.table(paste0(flow_loc,'_alf_irr_stop_jul10.dat'), skip = 2)[,3]*0.000408734569)


DP_natveg_outside_adj_0.05_flow = data.frame(Date = seq(start_date, end_date, "days"),                              # Import ILR flow data
                                Flow_m3day = read.table(paste0(flow_loc,'_natveg_outside_adj_0.05.dat'), skip = 2)[,3],
                                Flow_cfs = read.table(paste0(flow_loc,'_natveg_outside_adj_0.05.dat'), skip = 2)[,3]*0.000408734569)

DP_natveg_gwmixed_outside_adj_0.05_flow = data.frame(Date = seq(start_date, end_date, "days"),                              # Import ILR flow data
                                             Flow_m3day = read.table(paste0(flow_loc,'_natveg_gwmixed_outside_adj_0.05.dat'), skip = 2)[,3],
                                             Flow_cfs = read.table(paste0(flow_loc,'_natveg_gwmixed_outside_adj_0.05.dat'), skip = 2)[,3]*0.000408734569)

DP_natveg_outside_adj_0.05_1.0nvkc_flow = data.frame(Date = seq(start_date, end_date, "days"),                              # Import ILR flow data
                                             Flow_m3day = read.table(paste0(flow_loc,'_natveg_outside_adj_0.05_1.0nvkc.dat'), skip = 2)[,3],
                                             Flow_cfs = read.table(paste0(flow_loc,'_natveg_outside_adj_0.05_1.0nvkc.dat'), skip = 2)[,3]*0.000408734569)

DP_natveg_gwmixed_outside_adj_0.05_1.0nvkc_flow = data.frame(Date = seq(start_date, end_date, "days"),                              # Import ILR flow data
                                                     Flow_m3day = read.table(paste0(flow_loc,'_natveg_gwmixed_outside_adj_0.05_1.0nvkc.dat'), skip = 2)[,3],
                                                     Flow_cfs = read.table(paste0(flow_loc,'_natveg_gwmixed_outside_adj_0.05_1.0nvkc.dat'), skip = 2)[,3]*0.000408734569)

# Make a table for Thomas and his fish reconnection estimate -------------------------------------------------

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
daily_flow_tables = list(DP_Basecase_flow,
                         DP_MAR_flow, DP_ILR_flow, DP_MAR_ILR_flow,
                         DP_Basecase_fl_flow, #DP_MAR_ILR_fl_flow,
                         DP_0.8_flow, DP_0.9_flow, 
                         DP_natveg_outside_adj_0.05_flow,
                         DP_natveg_gwmixed_outside_adj_0.05_flow,
                         DP_natveg_outside_adj_0.05_1.0nvkc_flow,
                         DP_natveg_gwmixed_outside_adj_0.05_1.0nvkc_flow)
scenario_ids = c("basecase",
                 "mar","ilr","mar_ilr",
                 "flowlims",# "mar_ilr_flowlims",
                 "irrig_0.8","irrig_0.9", 
                 "nvoa0.05", "nvgwmoa0.05",
                 "nvoa0.05_1.0","nvgwmoa0.05_1.0")

tabs= flow_record_subset_wide(daily_flow_tables = daily_flow_tables,
                              scenario_ids = scenario_ids)

# for(i in 1:length(tabs)){
#   tab = tabs[[i]]
#  # print(head(tab))
#    write.csv(x=tab, paste(scenario_ids[i], ".csv"))
# }

  
  # make_flow_diff_daily_tab(basecase_flow_table = DP_Basecase_flow,
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


#############################################################################################
##########################             DATA PROCESSING             ##########################
#############################################################################################

make_flow_diff_daily_tab = function(basecase_flow_table = DP_Basecase_flow,
                                    daily_flow_tables, scenario_ids){
  Flow_Diff_Daily = data.frame(Date = seq(start_date, end_date, "days"))                          # Calculate daily difference from basecase condition
  
  for(i in 1:length(daily_flow_tables)){
    scenario_flow_table = daily_flow_tables[[i]]
    scenario_id = scenario_ids[i]
    if(str_to_lower(scenario_id) %in% c("mar","ilr","mar_ilr")){scenario_id = str_to_upper(scenario_id)}
    
    Flow_Diff_Daily$scen_difference_m3day = scenario_flow_table$Flow_m3day - basecase_flow_table$Flow_m3day
    Flow_Diff_Daily$scen_difference_cfs = scenario_flow_table$Flow_cfs - basecase_flow_table$Flow_cfs
    
    unit_strings = c("m3day","cfs")
    colnames(Flow_Diff_Daily)[(2*i):(2*i+1)] = c(paste(scenario_id,"difference", unit_strings, sep = "_"))
  }
  return(Flow_Diff_Daily)
}

make_daily_flow_tab = function(daily_flow_tables, scenario_ids){
  Daily_Flow = data.frame(Date = seq(start_date, end_date, "days"))

  for(i in 1:length(daily_flow_tables)){
    scenario_flow_table = daily_flow_tables[[i]]
    scenario_id = scenario_ids[i]
    if(str_to_lower(scenario_id) %in% c("mar","ilr","mar_ilr")){scenario_id = str_to_upper(scenario_id)}

    Daily_Flow$scen_difference_m3day = scenario_flow_table$Flow_m3day
    Daily_Flow$scen_difference_cfs = scenario_flow_table$Flow_cfs

    unit_strings = c("m3day","cfs")
    colnames(Daily_Flow)[(2*i):(2*i+1)] = c(paste(scenario_id, unit_strings, sep = "_"))
  }

return(Daily_Flow)

}


# FLOW DIFFERENCES

#Specify tables and scenario IDs (for column name differentiation) for daily diff table
Flow_Diff_Daily = make_flow_diff_daily_tab(basecase_flow_table = DP_Basecase_flow,
                                daily_flow_tables = list(DP_MAR_flow, 
                                                         DP_ILR_flow, 
                                                         DP_MAR_ILR_flow,
                                                         DP_Basecase_fl_flow, 
                                                         # DP_MAR_ILR_fl_flow,
                                                         DP_0.8_flow, DP_0.9_flow,
                                                         DP_natveg_outside_adj_0.05_flow,
                                                         DP_natveg_gwmixed_outside_adj_0.05_flow,
                                                         DP_natveg_outside_adj_0.05_1.0nvkc_flow,
                                                         DP_natveg_gwmixed_outside_adj_0.05_1.0nvkc_flow
                                                         ),
                                scenario_ids = c("mar","ilr","mar_ilr", 
                                                 "flowlims", #"mar_ilr_flowlims", 
                                                 "irrig_0.8","irrig_0.9",
                                                 "nvoa0.05","nvgwmoa0.05",
                                                 "nvoa0.05_1.0","nvgwmoa0.05_1.0"
                                                 ))

# Flow_Diff_Daily = make_flow_diff_daily_tab(basecase_flow_table = DP_Basecase_flow,
#                          daily_flow_tables = list(natveg_outside_adj_0.05_flow),
#                          scenario_ids = c("nvoa0.05"))

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

# DAILY FLOW

Daily_Flow = make_daily_flow_tab(
  daily_flow_tables = list(DP_Basecase_flow,
    DP_MAR_flow, 
                           DP_ILR_flow, 
                           DP_MAR_ILR_flow,
                           DP_Basecase_fl_flow, 
                           # DP_MAR_ILR_fl_flow,
                           DP_0.8_flow, DP_0.9_flow,
                           DP_natveg_outside_adj_0.05_flow,
                           DP_natveg_gwmixed_outside_adj_0.05_flow,
                           DP_natveg_outside_adj_0.05_1.0nvkc_flow,
                           DP_natveg_gwmixed_outside_adj_0.05_1.0nvkc_flow
  ),
  scenario_ids = c("basecase","mar","ilr","mar_ilr", 
                   "flowlims", #"mar_ilr_flowlims", 
                   "irrig_0.8","irrig_0.9",
                   "nvoa0.05","nvgwmoa0.05",
                   "nvoa0.05_1.0","nvgwmoa0.05_1.0"
                   ))

# Average flow for each Stress Period
Flow_SP_Avg = Daily_Flow# Copy daily data frame
Flow_SP_Avg$Date = format(Flow_SP_Avg$Date, '%b-%y')
Flow_SP_Avg = aggregate(.~Date,data = Flow_SP_Avg,  FUN = mean)
Flow_SP_Avg$Date = as.Date(paste0(Flow_SP_Avg$Date,'-01'), format = '%b-%y-%d')
Flow_SP_Avg = Flow_SP_Avg[order(Flow_SP_Avg$Date),]


# Average daily flow for each calendar month, over whole model period
Flow_Monthly_Avg = DP_Basecase_flow[seq(-1,-92),]   # Exclude first three months when MAR is not active                                                                            
Flow_Monthly_Avg$Date = format(Flow_Monthly_Avg$Date, '%b')
Flow_Monthly_Avg_2 = Flow_Monthly_Avg                                      # Copy daily data frame
Flow_Monthly_Avg = aggregate(.~Date,data =Flow_Monthly_Avg,  FUN = mean)
Flow_Monthly_SD = aggregate(.~Date, data =Flow_Monthly_Avg_2,  FUN = sd)
names(Flow_Monthly_SD) = c('Date',paste0(names(Flow_Monthly_Avg[-1]),'_SD'))

Flow_Monthly_Avg = merge(x = Flow_Monthly_Avg, y = Flow_Monthly_SD, by.x = "Date", by.y = "Date")
Flow_Monthly_Avg$Date  = factor(Flow_Monthly_Avg$Date , levels = as.character(format(seq(as.Date("1990/1/1"), as.Date("1990/12/31"), "month"), '%b')))
Flow_Monthly_Avg = Flow_Monthly_Avg[order(Flow_Monthly_Avg$Date), ]


MAR_xintercept_cfs = 3 +(-Flow_Diff_Monthly_Avg$MAR_difference_cfs[3] /(Flow_Diff_Monthly_Avg$MAR_difference_cfs[4] - Flow_Diff_Monthly_Avg$MAR_difference_cfs[3]))
MAR_geom_ribbon_data = data.frame(x = c(1,2,3,MAR_xintercept_cfs,4,5,6,7,8,9,10,11,12),
                                  y = c(Flow_Diff_Monthly_Avg$MAR_difference_cfs[1:3], 0, Flow_Diff_Monthly_Avg$MAR_difference_cfs[4:12])      )
ILR_xintercept_1_cfs = 3 +(-Flow_Diff_Monthly_Avg$ILR_difference_cfs[3] /(Flow_Diff_Monthly_Avg$ILR_difference_cfs[4] - Flow_Diff_Monthly_Avg$ILR_difference_cfs[3]))
ILR_xintercept_2_cfs = 6 +(-Flow_Diff_Monthly_Avg$ILR_difference_cfs[6] /(Flow_Diff_Monthly_Avg$ILR_difference_cfs[7] - Flow_Diff_Monthly_Avg$ILR_difference_cfs[6]))
ILR_geom_ribbon_data = data.frame(x = c(1,2,3,ILR_xintercept_1_cfs,4,5,6,ILR_xintercept_2_cfs,7,8,9,10,11,12),
                                  y = c(Flow_Diff_Monthly_Avg$ILR_difference_cfs[1:3], 0, Flow_Diff_Monthly_Avg$ILR_difference_cfs[4:6],0,Flow_Diff_Monthly_Avg$ILR_difference_cfs[7:12]))
MAR_ILR_xintercept_1_cfs = 3 +(-Flow_Diff_Monthly_Avg$MAR_ILR_difference_cfs[3] /(Flow_Diff_Monthly_Avg$MAR_ILR_difference_cfs[4] - Flow_Diff_Monthly_Avg$MAR_ILR_difference_cfs[3]))
MAR_ILR_xintercept_2_cfs = 4 +(-Flow_Diff_Monthly_Avg$MAR_ILR_difference_cfs[4] /(Flow_Diff_Monthly_Avg$MAR_ILR_difference_cfs[5] - Flow_Diff_Monthly_Avg$MAR_ILR_difference_cfs[4]))
MAR_ILR_xintercept_3_cfs = 6 +(-Flow_Diff_Monthly_Avg$MAR_ILR_difference_cfs[6] /(Flow_Diff_Monthly_Avg$MAR_ILR_difference_cfs[7] - Flow_Diff_Monthly_Avg$MAR_ILR_difference_cfs[6]))
MAR_ILR_geom_ribbon_data = data.frame(x = c(1,2,3,MAR_ILR_xintercept_1_cfs,4,MAR_ILR_xintercept_2_cfs,5,6,MAR_ILR_xintercept_3_cfs,7,8,9,10,11,12),
                                  y = c(Flow_Diff_Monthly_Avg$MAR_ILR_difference_cfs[1:3], 0, Flow_Diff_Monthly_Avg$MAR_ILR_difference_cfs[4],0,Flow_Diff_Monthly_Avg$MAR_ILR_difference_cfs[5:6] ,0,Flow_Diff_Monthly_Avg$MAR_ILR_difference_cfs[7:12]))
flowlims_geom_ribbon_data = data.frame(x = 1:12, y = Flow_Diff_Monthly_Avg$flowlims_difference_cfs)
MAR_ILR_flowlims_xintercept_1_cfs = 3 + (-Flow_Diff_Monthly_Avg$mar_ilr_flowlims_difference_cfs[3] /(Flow_Diff_Monthly_Avg$mar_ilr_flowlims_difference_cfs[4] - Flow_Diff_Monthly_Avg$mar_ilr_flowlims_difference_cfs[3]))
MAR_ILR_flowlims_xintercept_2_cfs = 4 +(-Flow_Diff_Monthly_Avg$mar_ilr_flowlims_difference_cfs[4] /(Flow_Diff_Monthly_Avg$mar_ilr_flowlims_difference_cfs[5] - Flow_Diff_Monthly_Avg$mar_ilr_flowlims_difference_cfs[4]))
MAR_ILR_flowlims_xintercept_3_cfs = 5 +(-Flow_Diff_Monthly_Avg$mar_ilr_flowlims_difference_cfs[5] /(Flow_Diff_Monthly_Avg$mar_ilr_flowlims_difference_cfs[6] - Flow_Diff_Monthly_Avg$mar_ilr_flowlims_difference_cfs[5]))
MAR_ILR_Flowlims_geom_ribbon_data = data.frame(x = c(1,2,3,MAR_ILR_flowlims_xintercept_1_cfs, 4, MAR_ILR_flowlims_xintercept_2_cfs,5,MAR_ILR_flowlims_xintercept_3_cfs,6:12),
                                               y = c(Flow_Diff_Monthly_Avg$mar_ilr_flowlims_difference_cfs[1:3], 0, Flow_Diff_Monthly_Avg$MAR_ILR_difference_cfs[4],0,Flow_Diff_Monthly_Avg$mar_ilr_flowlims_difference_cfs[5] ,0,Flow_Diff_Monthly_Avg$mar_ilr_flowlims_difference_cfs[6:12]))
irrig_0.8_geom_ribbon_data = data.frame(x = 1:12, y = Flow_Diff_Monthly_Avg$irrig_0.8_difference_cfs)
irrig_0.9_geom_ribbon_data = data.frame(x = 1:12, y = Flow_Diff_Monthly_Avg$irrig_0.9_difference_cfs)

"nvoa0.05"
"nvgwmoa0.05"

nvoa0.05_xintercept_cfs = 3 +(-Flow_Diff_Monthly_Avg$nvoa0.05_difference_cfs[3] /(Flow_Diff_Monthly_Avg$nvoa0.05_difference_cfs[4] - Flow_Diff_Monthly_Avg$nvoa0.05_difference_cfs[3]))
nvoa0.05_geom_ribbon_data = data.frame(x = c(1,2,3,nvoa0.05_xintercept_cfs,4,5,6,7,8,9,10,11,12),
                                  y = c(Flow_Diff_Monthly_Avg$nvoa0.05_difference_cfs[1:3], 0, Flow_Diff_Monthly_Avg$nvoa0.05_difference_cfs[4:12])      )

nvgwmoa0.05_xintercept_1_cfs = 1 +(-Flow_Diff_Monthly_Avg$nvgwmoa0.05_difference_cfs[1] /(Flow_Diff_Monthly_Avg$nvgwmoa0.05_difference_cfs[2] - Flow_Diff_Monthly_Avg$nvgwmoa0.05_difference_cfs[1]))
nvgwmoa0.05_xintercept_2_cfs = 3 +(-Flow_Diff_Monthly_Avg$nvgwmoa0.05_difference_cfs[3] /(Flow_Diff_Monthly_Avg$nvgwmoa0.05_difference_cfs[4] - Flow_Diff_Monthly_Avg$nvgwmoa0.05_difference_cfs[3]))
nvgwmoa0.05_geom_ribbon_data = data.frame(x = c(1,nvgwmoa0.05_xintercept_1_cfs,2,3,nvgwmoa0.05_xintercept_2_cfs,4,5,6,7,8,9,10,11,12),
                                       y = c(Flow_Diff_Monthly_Avg$nvgwmoa0.05_difference_cfs[1], 0,Flow_Diff_Monthly_Avg$nvgwmoa0.05_difference_cfs[2:3], 
                                             0, Flow_Diff_Monthly_Avg$nvgwmoa0.05_difference_cfs[4:12]))

nvoa0.05_xintercept_cfs = 3 +(-Flow_Diff_Monthly_Avg$nvoa0.05_difference_cfs[3] /(Flow_Diff_Monthly_Avg$nvoa0.05_difference_cfs[4] - Flow_Diff_Monthly_Avg$nvoa0.05_difference_cfs[3]))
nvoa0.05_geom_ribbon_data = data.frame(x = c(1,2,3,nvoa0.05_xintercept_cfs,4,5,6,7,8,9,10,11,12),
                                       y = c(Flow_Diff_Monthly_Avg$nvoa0.05_difference_cfs[1:3], 0, Flow_Diff_Monthly_Avg$nvoa0.05_difference_cfs[4:12])      )

nvoa0.05_1.0_xintercept_cfs =  3 +(-Flow_Diff_Monthly_Avg$nvoa0.05_1.0_difference_cfs[3] /(Flow_Diff_Monthly_Avg$nvoa0.05_1.0_difference_cfs[4] - Flow_Diff_Monthly_Avg$nvoa0.05_1.0_difference_cfs[3]))
nvoa0.05_1.0_geom_ribbon_data  = data.frame(x = c(1,2,3,nvoa0.05_1.0_xintercept_cfs,4,5,6,7,8,9,10,11,12),
                                               y = c(Flow_Diff_Monthly_Avg$nvoa0.05_1.0_difference_cfs[1:3], 0, Flow_Diff_Monthly_Avg$nvoa0.05_1.0_difference_cfs[4:12])      )

nvgwmoa0.05_1.0_xintercept_cfs =  3 +(-Flow_Diff_Monthly_Avg$nvgwmoa0.05_1.0_difference_cfs[3] /(Flow_Diff_Monthly_Avg$nvgwmoa0.05_1.0_difference_cfs[4] - Flow_Diff_Monthly_Avg$nvgwmoa0.05_1.0_difference_cfs[3]))
nvgwmoa0.05_1.0_geom_ribbon_data  = data.frame(x = c(1,2,3,nvgwmoa0.05_1.0_xintercept_cfs,4,5,6,7,8,9,10,11,12),
                                               y = c(Flow_Diff_Monthly_Avg$nvgwmoa0.05_1.0_difference_cfs[1:3], 0, Flow_Diff_Monthly_Avg$nvgwmoa0.05_1.0_difference_cfs[4:12])      )

#.############################################################################################
#.############################################################################################
############################                PLOTS                ############################
#.############################################################################################
#.############################################################################################

#.############################################################################################
####################        STRESS PERIOD AVERAGE DIFFERENCE PLOTS       ####################
#.############################################################################################
(MAR_Monthly_Diff_Plot = ggplot(Flow_Diff_SP_Avg, aes(x = Date, y = MAR_difference_cfs)) +
    geom_hline(yintercept = 0) +
    geom_line() +
    geom_point() +
    scale_y_continuous(limits = dif_lim, 
                       breaks = seq(dif_lim[1], dif_lim[2],by = 10), 
                       expand = c(0,0)) +
   scale_x_date(limits = c(start_date,
                           end_date),
                breaks = seq(start_date, by = "2 years", length.out = 22), expand = c(0,0),
                date_labels = ('%b-%y')) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(fill=NA, color = 'black'),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
          axis.text.y = element_text(size = 8),
          axis.ticks = element_line(size = 0.2),
          plot.title = element_text(hjust = 0.5, size = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 8))
)
(ILR_Monthly_Diff_Plot = ggplot(Flow_Diff_SP_Avg, aes(x = Date, y = ILR_difference_cfs)) +
    geom_hline(yintercept = 0) +
    geom_line() +
    geom_point() +
    scale_y_continuous(limits = dif_lim, 
                       breaks = seq(dif_lim[1], dif_lim[2],by = 10), expand = c(0,0)) +
    scale_x_date(limits = c(start_date,
                            end_date),
                 breaks = seq(start_date, by = "2 years", length.out = 22), expand = c(0,0),
                 date_labels = ('%b-%y')) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(fill=NA, color = 'black'),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
          axis.text.y = element_text(size = 8),
          axis.ticks = element_line(size = 0.2),
          plot.title = element_text(hjust = 0.5, size = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 8))
)
(MAR_ILR_Monthly_Diff_Plot = ggplot(Flow_Diff_SP_Avg, aes(x = Date, y = MAR_ILR_difference_cfs)) +
    geom_hline(yintercept = 0) +
    geom_line() +
    geom_point() +
    scale_y_continuous(limits = dif_lim, 
                       breaks = seq(dif_lim[1], dif_lim[2],by = 10), expand = c(0,0)) +
    scale_x_date(limits = c(start_date,
                            end_date),
                 breaks = seq(start_date, by = "2 years", length.out = 22), expand = c(0,0),
                 date_labels = ('%b-%y')) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(fill=NA, color = 'black'),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
          axis.text.y = element_text(size = 8),
          axis.ticks = element_line(size = 0.2),
          plot.title = element_text(hjust = 0.5, size = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 8))
)

(Flowlim_Monthly_Diff_Plot = ggplot(Flow_Diff_SP_Avg, 
                                    aes(x = Date, y = flowlims_difference_cfs)) +
    geom_hline(yintercept = 0) +
    geom_line() +
    geom_point() +
    scale_y_continuous(limits = dif_lim, 
                       breaks = seq(dif_lim[1], dif_lim[2],by = 10), expand = c(0,0)) +
    scale_x_date(limits = c(start_date,
                            end_date),
                 breaks = seq(start_date, by = "2 years", length.out = 22), expand = c(0,0),
                 date_labels = ('%b-%y')) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(fill=NA, color = 'black'),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
          axis.text.y = element_text(size = 8),
          axis.ticks = element_line(size = 0.2),
          plot.title = element_text(hjust = 0.5, size = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 8))
)

(MAR_ILR_Flowlim_Monthly_Diff_Plot = ggplot(Flow_Diff_SP_Avg, 
                                    aes(x = Date, y = mar_ilr_flowlims_difference_cfs)) +
    geom_hline(yintercept = 0) +
    geom_line() +
    geom_point() +
    scale_y_continuous(limits = dif_lim, 
                       breaks = seq(dif_lim[1], dif_lim[2],by = 10), expand = c(0,0)) +
    scale_x_date(limits = c(start_date,
                            end_date),
                 breaks = seq(start_date, by = "2 years", length.out = 22), expand = c(0,0),
                 date_labels = ('%b-%y')) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(fill=NA, color = 'black'),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
          axis.text.y = element_text(size = 8),
          axis.ticks = element_line(size = 0.2),
          plot.title = element_text(hjust = 0.5, size = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 8))
)
#.############################################################################################
####################           AVERAGE MONTHLY DIFFERENCE PLOTS          ####################
#.############################################################################################
(MAR_Monthly_Avg_Plot = ggplot(data = Flow_Diff_Monthly_Avg) + 
    geom_ribbon(data = MAR_geom_ribbon_data[1:4,], aes(x = x,ymin = y, ymax = 0), fill = 'red', alpha=  0.5) +
    geom_ribbon(data = MAR_geom_ribbon_data[4:13,], aes(x = x,ymin = 0, ymax = y), fill = 'dodgerblue', alpha=  0.5) + 
    geom_hline(yintercept = 0) +
    geom_line(aes(x = seq(1,12), y = MAR_difference_cfs, group = 1)) +
    geom_errorbar(aes(x = seq(1,12), ymin = MAR_difference_cfs-MAR_difference_cfs_SD, 
                      ymax = MAR_difference_cfs+MAR_difference_cfs_SD, group = 1), width = 0.25) +
    geom_point(aes(x = seq(1,12), y = MAR_difference_cfs, group = 1),size = 1.5) +
    scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = Flow_Diff_Monthly_Avg$Date) +
    scale_y_continuous(limits = dif_lim, 
                       breaks = seq(dif_lim[1], dif_lim[2],by = 10), expand = c(0,0)) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(fill=NA, color = 'black'),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
          axis.text.y = element_text(size = 8),
          axis.ticks = element_line(size = 0.2),
          plot.title = element_text(hjust = 0.5, size = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 8))
)
(ILR_Monthly_Avg_Plot = ggplot(data = Flow_Diff_Monthly_Avg) + 
    geom_ribbon(data = ILR_geom_ribbon_data[1:4,], aes(x = x,ymin = 0, ymax = y), fill = 'dodgerblue', alpha=  0.5) +
    geom_ribbon(data = ILR_geom_ribbon_data[4:8,], aes(x = x,ymin = y, ymax = 0), fill = 'red', alpha=  0.5) +
    geom_ribbon(data = ILR_geom_ribbon_data[8:14,], aes(x = x,ymin = 0, ymax = y), fill = 'dodgerblue', alpha=  0.5) +
    geom_hline(yintercept = 0) +
    geom_line(aes(x = seq(1,12), y = ILR_difference_cfs, group = 1)) +
    geom_errorbar(aes(x = seq(1,12), ymin = ILR_difference_cfs-ILR_difference_cfs_SD, 
                      ymax = ILR_difference_cfs+ILR_difference_cfs_SD, group = 1), width = 0.25) +
    geom_point(aes(x = seq(1,12), y = ILR_difference_cfs, group = 1),size = 1.5) +
    scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = Flow_Diff_Monthly_Avg$Date) +
    scale_y_continuous(limits = dif_lim, 
                       breaks = seq(dif_lim[1], dif_lim[2],by = 10), expand = c(0,0)) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(fill=NA, color = 'black'),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
          axis.text.y = element_text(size = 8),
          axis.ticks = element_line(size = 0.2),
          plot.title = element_text(hjust = 0.5, size = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 8))
)
(MAR_ILR_Monthly_Avg_Plot = ggplot(data = Flow_Diff_Monthly_Avg) + 
    geom_ribbon(data = MAR_ILR_geom_ribbon_data[1:4,], aes(x = x,ymin = 0, ymax = y), fill = 'red', alpha=  0.5) +
    geom_ribbon(data = MAR_ILR_geom_ribbon_data[4:6,], aes(x = x,ymin = y, ymax = 0), fill = 'dodgerblue', alpha=  0.5) +
    geom_ribbon(data = MAR_ILR_geom_ribbon_data[6:9,], aes(x = x,ymin = 0, ymax = y), fill = 'red', alpha=  0.5) +
    geom_ribbon(data = MAR_ILR_geom_ribbon_data[9:15,], aes(x = x,ymin = 0, ymax = y), fill = 'dodgerblue', alpha=  0.5) +
    geom_hline(yintercept = 0) +
    geom_line(aes(x = seq(1,12), y = MAR_ILR_difference_cfs, group = 1)) +
    geom_point(aes(x = seq(1,12), y = MAR_ILR_difference_cfs, group = 1),size = 1.5) +
    geom_errorbar(aes(x = seq(1,12), ymin = MAR_ILR_difference_cfs-MAR_ILR_difference_cfs_SD, 
                      ymax = MAR_ILR_difference_cfs+MAR_ILR_difference_cfs_SD, group = 1), width = 0.25) +
    scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = Flow_Diff_Monthly_Avg$Date) +
    scale_y_continuous(limits = dif_lim, 
                       breaks = seq(dif_lim[1], dif_lim[2],by = 10), expand = c(0,0)) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(fill=NA, color = 'black'),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
          axis.text.y = element_text(size = 8),
          axis.ticks = element_line(size = 0.2),
          plot.title = element_text(hjust = 0.5, size = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 8))
)

(Flowlims_Monthly_Avg_Plot = ggplot(data = Flow_Diff_Monthly_Avg) + 
    geom_ribbon(data = flowlims_geom_ribbon_data[1:12,], aes(x = x,ymin = y, ymax = 0), fill = 'dodgerblue', alpha=  0.5) +
    geom_hline(yintercept = 0) +
    geom_line(aes(x = seq(1,12), y = flowlims_difference_cfs, group = 1)) +
    geom_point(aes(x = seq(1,12), y = flowlims_difference_cfs, group = 1),size = 1.5) +
    geom_errorbar(aes(x = seq(1,12), ymin = flowlims_difference_cfs-flowlims_difference_cfs_SD, 
                      ymax = flowlims_difference_cfs+flowlims_difference_cfs_SD, group = 1), width = 0.25) +
    scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = Flow_Diff_Monthly_Avg$Date) +
    scale_y_continuous(limits = dif_lim, 
                       breaks = seq(dif_lim[1], dif_lim[2],by = 10), expand = c(0,0)) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(fill=NA, color = 'black'),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
          axis.text.y = element_text(size = 8),
          axis.ticks = element_line(size = 0.2),
          plot.title = element_text(hjust = 0.5, size = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 8))
)

(MAR_ILR_Flowlims_Monthly_Avg_Plot = ggplot(data = Flow_Diff_Monthly_Avg) + 
    geom_ribbon(data = MAR_ILR_Flowlims_geom_ribbon_data[1:4,], aes(x = x,ymin = 0, ymax = y), fill = 'red', alpha=  0.5) +
    geom_ribbon(data = MAR_ILR_Flowlims_geom_ribbon_data[4:6,], aes(x = x,ymin = y, ymax = 0), fill = 'dodgerblue', alpha=  0.5) +
    geom_ribbon(data = MAR_ILR_Flowlims_geom_ribbon_data[6:8,], aes(x = x,ymin = 0, ymax = y), fill = 'red', alpha=  0.5) +
    geom_ribbon(data = MAR_ILR_Flowlims_geom_ribbon_data[8:15,], aes(x = x,ymin = 0, ymax = y), fill = 'dodgerblue', alpha=  0.5) +
    geom_hline(yintercept = 0) +
    geom_line(aes(x = seq(1,12), y = mar_ilr_flowlims_difference_cfs, group = 1)) +
    geom_point(aes(x = seq(1,12), y = mar_ilr_flowlims_difference_cfs, group = 1),size = 1.5) +
    geom_errorbar(aes(x = seq(1,12), ymin = mar_ilr_flowlims_difference_cfs-mar_ilr_flowlims_difference_cfs_SD, 
                      ymax = mar_ilr_flowlims_difference_cfs+mar_ilr_flowlims_difference_cfs_SD, group = 1), width = 0.25) +
    scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = Flow_Diff_Monthly_Avg$Date) +
    scale_y_continuous(limits = dif_lim, 
                       breaks = seq(dif_lim[1], dif_lim[2],by = 10), expand = c(0,0)) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(fill=NA, color = 'black'),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
          axis.text.y = element_text(size = 8),
          axis.ticks = element_line(size = 0.2),
          plot.title = element_text(hjust = 0.5, size = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 8))
)
(Irrig_0.8_Monthly_Avg_Plot = ggplot(data = Flow_Diff_Monthly_Avg) + 
    geom_ribbon(data = irrig_0.8_geom_ribbon_data[1:12,], aes(x = x,ymin = y, ymax = 0), fill = 'dodgerblue', alpha=  0.5) +
    geom_hline(yintercept = 0) +
    geom_line(aes(x = seq(1,12), y = irrig_0.8_difference_cfs, group = 1)) +
    geom_point(aes(x = seq(1,12), y = irrig_0.8_difference_cfs, group = 1),size = 1.5) +
    geom_errorbar(aes(x = seq(1,12), ymin = irrig_0.8_difference_cfs-irrig_0.8_difference_cfs_SD, 
                      ymax = irrig_0.8_difference_cfs+irrig_0.8_difference_cfs_SD, group = 1), width = 0.25) +
    scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = Flow_Diff_Monthly_Avg$Date) +
    scale_y_continuous(limits = dif_lim, 
                       breaks = seq(dif_lim[1], dif_lim[2],by = 10), expand = c(0,0)) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(fill=NA, color = 'black'),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
          axis.text.y = element_text(size = 8),
          axis.ticks = element_line(size = 0.2),
          plot.title = element_text(hjust = 0.5, size = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 8))
)
(Irrig_0.9_Monthly_Avg_Plot = ggplot(data = Flow_Diff_Monthly_Avg) + 
    geom_ribbon(data = irrig_0.9_geom_ribbon_data[1:12,], aes(x = x,ymin = y, ymax = 0), fill = 'dodgerblue', alpha=  0.5) +
    geom_hline(yintercept = 0) +
    geom_line(aes(x = seq(1,12), y = irrig_0.9_difference_cfs, group = 1)) +
    geom_point(aes(x = seq(1,12), y = irrig_0.9_difference_cfs, group = 1),size = 1.5) +
    geom_errorbar(aes(x = seq(1,12), ymin = irrig_0.9_difference_cfs-irrig_0.9_difference_cfs_SD, 
                      ymax = irrig_0.9_difference_cfs+irrig_0.9_difference_cfs_SD, group = 1), width = 0.25) +
    scale_x_continuous(limits = c(0.5,12.5), breaks = seq(1,12,by = 1), expand = c(0,0), labels = Flow_Diff_Monthly_Avg$Date) +
    scale_y_continuous(limits = dif_lim, 
                       breaks = seq(dif_lim[1], dif_lim[2],by = 10), expand = c(0,0)) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(fill=NA, color = 'black'),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
          axis.text.y = element_text(size = 8),
          axis.ticks = element_line(size = 0.2),
          plot.title = element_text(hjust = 0.5, size = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 8))
)


nvoa0.05_Monthly_Avg_Plot = ggplot(data = Flow_Diff_Monthly_Avg) + 
    geom_ribbon(data = nvoa0.05_geom_ribbon_data[1:4,], aes(x = x,ymin = y, ymax = 0), fill = 'red', alpha=  0.5) +
    geom_ribbon(data = nvoa0.05_geom_ribbon_data[4:13,], aes(x = x,ymin = 0, ymax = y), fill = 'dodgerblue', alpha=  0.5) + 
    geom_hline(yintercept = 0) +
    geom_line(aes(x = seq(1,12), y = nvoa0.05_difference_cfs, group = 1)) +
    geom_point(aes(x = seq(1,12), y = nvoa0.05_difference_cfs, group = 1),size = 1.5) +
    geom_errorbar(aes(x = seq(1,12), ymin = nvoa0.05_difference_cfs-nvoa0.05_difference_cfs_SD, 
                      ymax = nvoa0.05_difference_cfs+nvoa0.05_difference_cfs_SD, group = 1), width = 0.25) +
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


nvgwmoa0.05_Monthly_Avg_Plot = ggplot(data = Flow_Diff_Monthly_Avg) + 
    geom_ribbon(data = nvgwmoa0.05_geom_ribbon_data[1:2,], aes(x = x,ymin = 0, ymax = y), fill = 'dodgerblue', alpha=  0.5) + 
    geom_ribbon(data = nvgwmoa0.05_geom_ribbon_data[2:4,], aes(x = x,ymin = y, ymax = 0), fill = 'red', alpha=  0.5) +
    geom_ribbon(data = nvgwmoa0.05_geom_ribbon_data[4:14,], aes(x = x,ymin = 0, ymax = y), fill = 'dodgerblue', alpha=  0.5) +
    geom_hline(yintercept = 0) +
    geom_line(aes(x = seq(1,12), y = nvgwmoa0.05_difference_cfs, group = 1)) +
    geom_point(aes(x = seq(1,12), y = nvgwmoa0.05_difference_cfs, group = 1),size = 1.5) +
    geom_errorbar(aes(x = seq(1,12), ymin = nvgwmoa0.05_difference_cfs-nvgwmoa0.05_difference_cfs_SD, 
                      ymax = nvgwmoa0.05_difference_cfs+nvgwmoa0.05_difference_cfs_SD, group = 1), width = 0.25) +
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

nvoa0.05_1.0_Monthly_Avg_Plot = ggplot(data = Flow_Diff_Monthly_Avg) + 
  geom_ribbon(data = nvoa0.05_1.0_geom_ribbon_data[1:4,], aes(x = x,ymin = y, ymax = 0), fill = 'red', alpha=  0.5) +
  geom_ribbon(data = nvoa0.05_1.0_geom_ribbon_data[4:13,], aes(x = x,ymin = 0, ymax = y), fill = 'dodgerblue', alpha=  0.5) + 
  geom_hline(yintercept = 0) +
  geom_line(aes(x = seq(1,12), y = nvoa0.05_1.0_difference_cfs, group = 1)) +
  geom_point(aes(x = seq(1,12), y = nvoa0.05_1.0_difference_cfs, group = 1),size = 1.5) +
  geom_errorbar(aes(x = seq(1,12), ymin = nvoa0.05_1.0_difference_cfs-nvoa0.05_1.0_difference_cfs_SD, 
                    ymax = nvoa0.05_1.0_difference_cfs+nvoa0.05_1.0_difference_cfs_SD, group = 1), width = 0.25) +
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

nvgwmoa0.05_1.0_Monthly_Avg_Plot = ggplot(data = Flow_Diff_Monthly_Avg) + 
  geom_ribbon(data = nvgwmoa0.05_1.0_geom_ribbon_data[1:4,], aes(x = x,ymin = y, ymax = 0), fill = 'red', alpha=  0.5) +
  geom_ribbon(data = nvgwmoa0.05_1.0_geom_ribbon_data[4:13,], aes(x = x,ymin = 0, ymax = y), fill = 'dodgerblue', alpha=  0.5) + 
  geom_hline(yintercept = 0) +
  geom_line(aes(x = seq(1,12), y = nvgwmoa0.05_1.0_difference_cfs, group = 1)) +
  geom_point(aes(x = seq(1,12), y = nvgwmoa0.05_1.0_difference_cfs, group = 1),size = 1.5) +
  geom_errorbar(aes(x = seq(1,12), ymin = nvgwmoa0.05_1.0_difference_cfs-nvgwmoa0.05_1.0_difference_cfs_SD, 
                    ymax = nvgwmoa0.05_1.0_difference_cfs+nvgwmoa0.05_1.0_difference_cfs_SD, group = 1), width = 0.25) +
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


#.############################################################################################
###############           DAILY FLOW MONTHLY AVERAGES PLOTS         ###############
#.############################################################################################

# Process so that the error bars show up
Flow_Monthly_Avg$Flow_cfs_SD_down = Flow_Monthly_Avg$Flow_cfs_SD
sd_bigger_than_avg = Flow_Monthly_Avg$Flow_cfs_SD_down>Flow_Monthly_Avg$Flow_cfs
Flow_Monthly_Avg$Flow_cfs_SD_down[sd_bigger_than_avg] = Flow_Monthly_Avg$Flow_cfs[sd_bigger_than_avg] -1

(Monthly_Avg_Flow_Plot = ggplot(data = Flow_Monthly_Avg) + 
   # geom_ribbon(data = nvgwmoa0.05_geom_ribbon_data[1:2,], aes(x = x,ymin = 0, ymax = y), fill = 'dodgerblue', alpha=  0.5) + 
   # geom_ribbon(data = nvgwmoa0.05_geom_ribbon_data[2:4,], aes(x = x,ymin = y, ymax = 0), fill = 'red', alpha=  0.5) +
   # geom_ribbon(data = nvgwmoa0.05_geom_ribbon_data[4:14,], aes(x = x,ymin = 0, ymax = y), fill = 'dodgerblue', alpha=  0.5) +
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
)

#.############################################################################################
###############           DRY, AVERAGE, AND WET YEAR DIFFERENCE PLOTS         ###############
#.############################################################################################
Flow_Diff_SP_Dry_Avg_Wet = subset(Flow_Diff_SP_Avg,#select = c('Date','MAR_difference_cfs', 'ILR_difference_cfs', 'MAR_ILR_difference_cfs'), 
                                  Date %in%c(seq(as.Date("2010/1/1"), by = "month", length.out = 12),
                                             seq(as.Date("2014/1/1"), by = "month", length.out = 12),
                                             seq(as.Date("2017/1/1"), by = "month", length.out = 12)))
Flow_Diff_SP_Dry_Avg_Wet$Year_Type = rep(c('Average (2010)','Dry (2014)','Wet (2017)'), each=12)
Flow_Diff_SP_Dry_Avg_Wet$Year_Type = factor(Flow_Diff_SP_Dry_Avg_Wet$Year_Type, levels = c('Dry (2014)','Average (2010)','Wet (2017)'))
Flow_Diff_SP_Dry_Avg_Wet = Flow_Diff_SP_Dry_Avg_Wet[order(Flow_Diff_SP_Dry_Avg_Wet$Year_Type),]
Flow_Diff_SP_Dry_Avg_Wet$Date = format(Flow_Diff_SP_Dry_Avg_Wet$Date, '%m')

(MAR_Dry_Avg_Wet_Diff_Plot = ggplot(data = Flow_Diff_SP_Dry_Avg_Wet, aes(x = as.numeric(Date), y = MAR_difference_cfs, group = Year_Type, color = Year_Type)) +
   geom_hline(yintercept = 0) +
   geom_line(size = 1) +
   geom_point(size = 1.5) +
   scale_y_continuous(limits = dif_lim, 
                      breaks = seq(dif_lim[1], dif_lim[2],by = 10), expand = c(0,0)) +
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
)
(ILR_Dry_Avg_Wet_Diff_Plot = ggplot(data = Flow_Diff_SP_Dry_Avg_Wet, aes(x = as.numeric(Date), y = ILR_difference_cfs, group = Year_Type, color = Year_Type)) +
    geom_hline(yintercept = 0) +
    geom_line(size = 1) +
    geom_point(size = 1.5) +
    scale_y_continuous(limits = dif_lim, 
                       breaks = seq(dif_lim[1], dif_lim[2],by = 10), expand = c(0,0)) +
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
)
(MAR_ILR_Dry_Avg_Wet_Diff_Plot = ggplot(data = Flow_Diff_SP_Dry_Avg_Wet, aes(x = as.numeric(Date), y = MAR_ILR_difference_cfs, group = Year_Type, color = Year_Type)) +
    geom_hline(yintercept = 0) +
    geom_line(size = 1) +
    geom_point(size = 1.5) +
    scale_y_continuous(limits = dif_lim, 
                       breaks = seq(dif_lim[1], dif_lim[2],by = 10), expand = c(0,0)) +
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
)

(Flowlims_Dry_Avg_Wet_Diff_Plot = ggplot(data = Flow_Diff_SP_Dry_Avg_Wet, aes(x = as.numeric(Date), y = flowlims_difference_cfs, group = Year_Type, color = Year_Type)) +
    geom_hline(yintercept = 0) +
    geom_line(size = 1) +
    geom_point(size = 1.5) +
    scale_y_continuous(limits = dif_lim, 
                       breaks = seq(dif_lim[1], dif_lim[2],by = 10), expand = c(0,0)) +
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
)

(MAR_ILR_Flowlims_Dry_Avg_Wet_Diff_Plot = ggplot(data = Flow_Diff_SP_Dry_Avg_Wet, aes(x = as.numeric(Date), y = mar_ilr_flowlims_difference_cfs, group = Year_Type, color = Year_Type)) +
    geom_hline(yintercept = 0) +
    geom_line(size = 1) +
    geom_point(size = 1.5) +
    scale_y_continuous(limits = dif_lim, 
                       breaks = seq(dif_lim[1], dif_lim[2],by = 10), expand = c(0,0)) +
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
)

(Irrig_0.8_Dry_Avg_Wet_Diff_Plot = ggplot(data = Flow_Diff_SP_Dry_Avg_Wet, aes(x = as.numeric(Date), y = irrig_0.8_difference_cfs, group = Year_Type, color = Year_Type)) +
    geom_hline(yintercept = 0) +
    geom_line(size = 1) +
    geom_point(size = 1.5) +
    scale_y_continuous(limits = dif_lim, 
                       breaks = seq(dif_lim[1], dif_lim[2],by = 10), expand = c(0,0)) +
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
)

(Irrig_0.9_Dry_Avg_Wet_Diff_Plot = ggplot(data = Flow_Diff_SP_Dry_Avg_Wet, 
                                          aes(x = as.numeric(Date), 
                                              y = irrig_0.9_difference_cfs, 
                                              group = Year_Type, color = Year_Type)) +
    geom_hline(yintercept = 0) +
    geom_line(size = 1) +
    geom_point(size = 1.5) +
    scale_y_continuous(limits = dif_lim, 
                       breaks = seq(dif_lim[1], dif_lim[2],by = 10), expand = c(0,0)) +
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
)

(nvoa0.05_Dry_Avg_Wet_Diff_Plot = ggplot(data = Flow_Diff_SP_Dry_Avg_Wet, 
                                          aes(x = as.numeric(Date), 
                                              y = nvoa0.05_difference_cfs, 
                                              group = Year_Type, color = Year_Type)) +
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
)

(nvgwmoa0.05_Dry_Avg_Wet_Diff_Plot = ggplot(data = Flow_Diff_SP_Dry_Avg_Wet, 
                                         aes(x = as.numeric(Date), 
                                             y = nvgwmoa0.05_difference_cfs, 
                                             group = Year_Type, color = Year_Type)) +
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
)

(nvoa0.05_1.0_Dry_Avg_Wet_Diff_Plot = ggplot(data = Flow_Diff_SP_Dry_Avg_Wet, 
                                            aes(x = as.numeric(Date), 
                                                y = nvoa0.05_1.0_difference_cfs, 
                                                group = Year_Type, color = Year_Type)) +
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
)

(nvgwmoa0.05_1.0_Dry_Avg_Wet_Diff_Plot = ggplot(data = Flow_Diff_SP_Dry_Avg_Wet, 
                                             aes(x = as.numeric(Date), 
                                                 y = nvgwmoa0.05_1.0_difference_cfs, 
                                                 group = Year_Type, color = Year_Type)) +
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
)


#.############################################################################################
###############           DRY, AVERAGE, AND WET YEAR FLOW PLOTS         ###############
#.############################################################################################

#.############################################################################################

Flow_SP_Dry_Avg_Wet = subset(Flow_SP_Avg,
                             #select = c('Date','MAR_difference_cfs', 'ILR_difference_cfs', 'MAR_ILR_difference_cfs'), 
                             Date %in%c(seq(as.Date("2010/1/1"), by = "month", length.out = 12),
                                        seq(as.Date("2014/1/1"), by = "month", length.out = 12),
                                        seq(as.Date("2017/1/1"), by = "month", length.out = 12)))
Flow_SP_Dry_Avg_Wet$Year_Type = rep(c('Average (2010)','Dry (2014)','Wet (2017)'), each=12)
Flow_SP_Dry_Avg_Wet$Year_Type = factor(Flow_SP_Dry_Avg_Wet$Year_Type, levels = c('Dry (2014)','Average (2010)','Wet (2017)'))
Flow_SP_Dry_Avg_Wet = Flow_SP_Dry_Avg_Wet[order(Flow_SP_Dry_Avg_Wet$Year_Type),]
Flow_SP_Dry_Avg_Wet$Date = format(Flow_SP_Dry_Avg_Wet$Date, '%m')

(basecase_Dry_Avg_Wet_Flow_Plot = ggplot()+
    # geom_line(data = Flow_SP_Dry_Avg_Wet, aes(x = as.numeric(Date), y = nvgwmoa0.05_cfs, group = Year_Type, color = Year_Type), size = 1.5) +
    geom_line(data = Flow_SP_Dry_Avg_Wet, aes(x = as.numeric(Date), y = basecase_cfs, group = Year_Type, color = Year_Type), linetype = "dashed", size = 0.8) +
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
  
)

(nvoa0.05_Dry_Avg_Wet_Flow_Plot = ggplot()+
    geom_line(data = Flow_SP_Dry_Avg_Wet, 
              aes(x = as.numeric(Date), y = nvoa0.05_cfs, group = Year_Type, color = Year_Type), size = 1.5) +
    geom_line(data = Flow_SP_Dry_Avg_Wet, aes(x = as.numeric(Date), y = basecase_cfs, group = Year_Type, color = Year_Type), linetype = "dashed", size = 0.8) +
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
  
)
  

(nvgwmoa0.05_Dry_Avg_Wet_Flow_Plot = ggplot()+
    geom_line(data = Flow_SP_Dry_Avg_Wet, aes(x = as.numeric(Date), y = nvgwmoa0.05_cfs, group = Year_Type, color = Year_Type), size = 1.5) +
    geom_line(data = Flow_SP_Dry_Avg_Wet, aes(x = as.numeric(Date), y = basecase_cfs, group = Year_Type, color = Year_Type), linetype = "dashed", size = 0.8) +
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
)

(nvoa0.05_1.0_Dry_Avg_Wet_Flow_Plot = ggplot()+
    geom_line(data = Flow_SP_Dry_Avg_Wet, 
              aes(x = as.numeric(Date), y = nvoa0.05_1.0_cfs, group = Year_Type, color = Year_Type), size = 1.5) +
    geom_line(data = Flow_SP_Dry_Avg_Wet, aes(x = as.numeric(Date), y = basecase_cfs, group = Year_Type, color = Year_Type), linetype = "dashed", size = 0.8) +
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
  
)


(nvgwmoa0.05_1.0_Dry_Avg_Wet_Flow_Plot = 
    ggplot()+
    geom_line(data = Flow_SP_Dry_Avg_Wet, aes(x = as.numeric(Date), y = nvgwmoa0.05_1.0_cfs, group = Year_Type, color = Year_Type), size = 1.5) +
    geom_line(data = Flow_SP_Dry_Avg_Wet, aes(x = as.numeric(Date), y = basecase_cfs, group = Year_Type, color = Year_Type), linetype = "dashed", size = 0.8) +
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
)

# Print figures to file ---------------------------------------------------

if (graphics_type == 'pdf'){pdf(paste0(flow_loc_short,'_Avg_Flow_cfs.pdf'), width = 3.5, height = 3)
} else if (graphics_type == 'png'){png(paste0(flow_loc_short,'_Avg_Flow_cfs.png'), width = 3.5, height = 2, units = 'in', res = 600 )}
grid.newpage()
# pushViewport(viewport(layout = grid.layout(1,2)))
# vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(Monthly_Avg_Flow_Plot + 
        ylab('Average Daily Streamflow (cfs)'),
      # vp = vplayout(1,1)
      )
graphics.off()


if (graphics_type == 'pdf'){pdf(paste0(flow_loc_short,'_basecase_Flow_cfs.pdf'), width = 7, height = 3)
} else if (graphics_type == 'png'){png(paste0(flow_loc_short,'_basecase_Flow_cfs.png'), width = 7, height = 3, units = 'in', res = 600 )}
grid.newpage()
pushViewport(viewport(layout = grid.layout(1,2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(Monthly_Avg_Flow_Plot + 
        ylab('Average Daily Streamflow (cfs)'),
      vp = vplayout(1,1))
print(basecase_Dry_Avg_Wet_Flow_Plot +
        ylab(''),
      vp = vplayout(1,2))
graphics.off()



if (graphics_type == 'pdf'){pdf(paste0(flow_loc_short,'_MAR_Flow_Diff_cfs.pdf'), width = 7, height = 3)
} else if (graphics_type == 'png'){png(paste0(flow_loc_short,'_MAR_Flow_Diff_cfs.png'), width = 7, height = 3, units = 'in', res = 600 )}
grid.newpage()
pushViewport(viewport(layout = grid.layout(1,2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(MAR_Monthly_Avg_Plot + 
      ylab('Streamflow Difference (cfs)'),
      vp = vplayout(1,1))
print(MAR_Dry_Avg_Wet_Diff_Plot +
       ylab(''),
      vp = vplayout(1,2))
graphics.off()

if (graphics_type == 'pdf'){pdf('ILR_Flow_Diff_cfs.pdf', width = 7, height = 3)
} else if (graphics_type == 'png'){png(paste0(flow_loc_short,'_ILR_Flow_Diff_cfs.png'), width = 7, height = 3, units = 'in', res = 600 )}
grid.newpage()
pushViewport(viewport(layout = grid.layout(1,2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(ILR_Monthly_Avg_Plot + 
        ylab('Streamflow Difference (cfs)'),
      vp = vplayout(1,1))
print(ILR_Dry_Avg_Wet_Diff_Plot +
        ylab(''),
      vp = vplayout(1,2))
graphics.off()

if (graphics_type == 'pdf'){pdf('MAR_ILR_Flow_Diff_cfs.pdf', width = 7, height = 3)
} else if (graphics_type == 'png'){ png(paste0(flow_loc_short,'_MAR_ILR_Flow_Diff_cfs.png'), width = 7, height = 3, units = 'in', res = 600 )}
grid.newpage()
pushViewport(viewport(layout = grid.layout(1,2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(MAR_ILR_Monthly_Avg_Plot + 
        ylab('Streamflow Difference (cfs)'),
      vp = vplayout(1,1))
print(MAR_ILR_Dry_Avg_Wet_Diff_Plot +
        ylab(''),
      vp = vplayout(1,2))
graphics.off()

if (graphics_type == 'pdf'){pdf('MAR_ILR_Flowlims_Flow_Diff_cfs.pdf', width = 7, height = 3)
} else if (graphics_type == 'png'){ png(paste0(flow_loc_short,'_MAR_ILR_Flowlims_Flow_Diff_cfs.png'), width = 7, height = 3, units = 'in', res = 600 )}
grid.newpage()
pushViewport(viewport(layout = grid.layout(1,2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(MAR_ILR_Flowlims_Monthly_Avg_Plot + 
        ylab('Streamflow Difference (cfs)'),
      vp = vplayout(1,1))
print(MAR_ILR_Flowlims_Dry_Avg_Wet_Diff_Plot +
        ylab(''),
      vp = vplayout(1,2))
graphics.off()

if (graphics_type == 'pdf'){pdf('Flowlims_Flow_Diff_cfs.pdf', width = 7, height = 3)
} else if (graphics_type == 'png'){ png(paste0(flow_loc_short,'_Flowlims_Flow_Diff_cfs.png'), width = 7, height = 3, units = 'in', res = 600 )}
grid.newpage()
pushViewport(viewport(layout = grid.layout(1,2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(Flowlims_Monthly_Avg_Plot + 
        ylab('Streamflow Difference (cfs)'),
      vp = vplayout(1,1))
print(Flowlims_Dry_Avg_Wet_Diff_Plot +
        ylab(''),
      vp = vplayout(1,2))
graphics.off()

if (graphics_type == 'pdf'){pdf('Irrig_0.8_Flow_Diff_cfs.pdf', width = 7, height = 3)
} else if (graphics_type == 'png'){ png(paste0(flow_loc_short,'_Irrig_0.8_Flow_Diff_cfs.png'), width = 7, height = 3, units = 'in', res = 600 )}
grid.newpage()
pushViewport(viewport(layout = grid.layout(1,2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(Irrig_0.8_Monthly_Avg_Plot + 
        ylab('Streamflow Difference (cfs)'),
      vp = vplayout(1,1))
print(Irrig_0.8_Dry_Avg_Wet_Diff_Plot +
        ylab(''),
      vp = vplayout(1,2))
graphics.off()

if (graphics_type == 'pdf'){pdf('Irrig_0.9_Flow_Diff_cfs.pdf', width = 7, height = 3)
} else if (graphics_type == 'png'){ png(paste0(flow_loc_short,'Irrig_0.9_Flow_Diff_cfs.png'), width = 7, height = 3, units = 'in', res = 600 )}
grid.newpage()
pushViewport(viewport(layout = grid.layout(1,2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(Irrig_0.9_Monthly_Avg_Plot + 
        ylab('Streamflow Difference (cfs)'),
      vp = vplayout(1,1))
print(Irrig_0.9_Dry_Avg_Wet_Diff_Plot +
        ylab(''),
      vp = vplayout(1,2))
graphics.off()


if (graphics_type == 'pdf'){pdf('nvoa0.05_Flow_Diff_cfs.pdf', width = 7, height = 3)
} else if (graphics_type == 'png'){ png(paste0(flow_loc_short,'nvoa0.05_Flow_Diff_cfs.png'), width = 7, height = 3, units = 'in', res = 600 )}
grid.newpage()
pushViewport(viewport(layout = grid.layout(1,2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(nvoa0.05_Monthly_Avg_Plot + 
        ylab('Streamflow Difference (cfs)'),
      vp = vplayout(1,1))
print(nvoa0.05_Dry_Avg_Wet_Diff_Plot +
        ylab(''),
      vp = vplayout(1,2))
graphics.off()

if (graphics_type == 'pdf'){pdf('nvgwmoa0.05_Flow_Diff_cfs.pdf', width = 7, height = 3)
} else if (graphics_type == 'png'){ png(paste0(flow_loc_short,'nvgwmoa0.05_Flow_Diff_cfs.png'), width = 7, height = 3, units = 'in', res = 600 )}
grid.newpage()
pushViewport(viewport(layout = grid.layout(1,2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(nvgwmoa0.05_Monthly_Avg_Plot + 
        ylab('Streamflow Difference (cfs)'),
      vp = vplayout(1,1))
print(nvgwmoa0.05_Dry_Avg_Wet_Diff_Plot +
        ylab(''),
      vp = vplayout(1,2))
graphics.off()


if (graphics_type == 'pdf'){pdf('nvoa0.05_1.0_Flow_Diff_cfs.pdf', width = 7, height = 3)
} else if (graphics_type == 'png'){ png(paste0(flow_loc_short,'nvoa0.05__1.0_Flow_Diff_cfs.png'), width = 7, height = 3, units = 'in', res = 600 )}
grid.newpage()
pushViewport(viewport(layout = grid.layout(1,2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(nvoa0.05_1.0_Monthly_Avg_Plot + 
        ylab('Streamflow Difference (cfs)'),
      vp = vplayout(1,1))
print(nvoa0.05_1.0_Dry_Avg_Wet_Diff_Plot +
        ylab(''),
      vp = vplayout(1,2))
graphics.off()

if (graphics_type == 'pdf'){pdf('nvgwmoa0.05_1.0_Flow_Diff_cfs.pdf', width = 7, height = 3)
} else if (graphics_type == 'png'){ png(paste0(flow_loc_short,'nvgwmoa0.05_1.0_Flow_Diff_cfs.png'), width = 7, height = 3, units = 'in', res = 600 )}
grid.newpage()
pushViewport(viewport(layout = grid.layout(1,2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(nvgwmoa0.05_1.0_Monthly_Avg_Plot + 
        ylab('Streamflow Difference (cfs)'),
      vp = vplayout(1,1))
print(nvgwmoa0.05_1.0_Dry_Avg_Wet_Diff_Plot +
        ylab(''),
      vp = vplayout(1,2))
graphics.off()


if (graphics_type == 'pdf'){pdf(paste0(flow_loc_short,'_nvoa0.05_Flow_cfs.pdf'), width = 7, height = 3)
} else if (graphics_type == 'png'){png(paste0(flow_loc_short,'_nvoa0.05_Flow_cfs.png'), width = 7, height = 3, units = 'in', res = 600 )}
grid.newpage()
pushViewport(viewport(layout = grid.layout(1,2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(Monthly_Avg_Flow_Plot + 
        ylab('Average Daily Streamflow (cfs)'),
      vp = vplayout(1,1))
print(nvoa0.05_Dry_Avg_Wet_Flow_Plot +
        ylab(''),
      vp = vplayout(1,2))
graphics.off()

if (graphics_type == 'pdf'){pdf(paste0(flow_loc_short,'_nvgwmoa0.05_Flow_cfs.pdf'), width = 7, height = 3)
} else if (graphics_type == 'png'){png(paste0(flow_loc_short,'_nvgwmoa0.05_Flow_cfs.png'), width = 7, height = 3, units = 'in', res = 600 )}
grid.newpage()
pushViewport(viewport(layout = grid.layout(1,2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(Monthly_Avg_Flow_Plot + 
        ylab('Average Daily Streamflow (cfs)'),
      vp = vplayout(1,1))
print(nvgwmoa0.05_Dry_Avg_Wet_Flow_Plot +
        ylab(''),
      vp = vplayout(1,2))
graphics.off()



if (graphics_type == 'pdf'){pdf(paste0(flow_loc_short,'_nvoa0.05_1.0_Flow_cfs.pdf'), width = 7, height = 3)
} else if (graphics_type == 'png'){png(paste0(flow_loc_short,'_nvoa0.05_1.0_Flow_cfs.png'), width = 7, height = 3, units = 'in', res = 600 )}
grid.newpage()
pushViewport(viewport(layout = grid.layout(1,2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(Monthly_Avg_Flow_Plot + 
        ylab('Average Daily Streamflow (cfs)'),
      vp = vplayout(1,1))
print(nvoa0.05_1.0_Dry_Avg_Wet_Flow_Plot +
        ylab(''),
      vp = vplayout(1,2))
graphics.off()

if (graphics_type == 'pdf'){pdf(paste0(flow_loc_short,'_nvgwmoa0.05_1.0_Flow_cfs.pdf'), width = 7, height = 3)
} else if (graphics_type == 'png'){png(paste0(flow_loc_short,'_nvgwmoa0.05_1.0_Flow_cfs.png'), width = 7, height = 3, units = 'in', res = 600 )}
grid.newpage()
pushViewport(viewport(layout = grid.layout(1,2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(Monthly_Avg_Flow_Plot + 
        ylab('Average Daily Streamflow (cfs)'),
      vp = vplayout(1,1))
print(nvgwmoa0.05_1.0_Dry_Avg_Wet_Flow_Plot +
        ylab(''),
      vp = vplayout(1,2))
graphics.off()

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



# scratchwork -------------------------------------------------------------



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



#############################################################################################
########################             MODFLOW INPUT PLOTS             ########################
#############################################################################################

MODFLOW_Seg1_Inflows_diff_melt = reshape2::melt(MODFLOW_Seg1_Inflows %>%
                                                  select('Date','MAR_diff','ILR_diff','MAR_ILR_diff'),id.vars = 'Date')

(Seg1_Inflow_diff_Plot = ggplot(data = MODFLOW_Seg1_Inflows_diff_melt, aes(x = Date, y = value*0.000408734569, group = variable, color = variable)) +
    geom_line() +
    geom_point() +
    scale_x_date(limits = c(start_date,
                            as.Date('1995-11-01')),
                 breaks = seq(start_date, by = "2 years", length.out = 22), expand = c(0,0),
                 date_labels = ('%b-%y'))
)
MODFLOW_Seg32_Inflows_diff_melt = melt(MODFLOW_Seg32_Inflows%>%select('Date','MAR_diff','ILR_diff','MAR_ILR_diff'),id.vars = 'Date')
(Seg32_Inflow_diff_Plot = ggplot(data = MODFLOW_Seg32_Inflows_diff_melt, aes(x = Date, y = value*0.000408734569, group = variable, color = variable)) +
    geom_line() +
    geom_point() +
    scale_x_date(limits = c(start_date,
                            end_date),
                 breaks = seq(start_date, by = "2 years", length.out = 22), expand = c(0,0),
                 date_labels = ('%b-%y'))
)


#############################################################################################
########################            DAILY DIFFERENCE PLOTS           ########################
#############################################################################################
(MAR_Daily_Diff_Plot = ggplot(Flow_Diff_Daily, aes(x = Date, y = MAR_difference_cfs)) +
   geom_line() +
   #geom_line(data =MODFLOW_Seg1_Inflows, aes(x = Date, y = Basecase*0.000408734569/10), color = 'red') +
   geom_hline(yintercept = 0) +
   #scale_y_continuous(limits = c(-50,50), breaks = seq(-50,50,by = 10), expand = c(0,0)) +
   scale_x_date(limits = c(start_date,
                           end_date),
                breaks = seq(start_date, by = "2 years", length.out = 22), expand = c(0,0),
                date_labels = ('%b-%y')) +
   theme(panel.background = element_blank(),
         panel.border = element_rect(fill=NA, color = 'black'),
         axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
         axis.text.y = element_text(size = 8),
         axis.ticks = element_line(size = 0.2),
         plot.title = element_text(hjust = 0.5, size = 10),
         axis.title.x = element_blank(),
         axis.title.y = element_text(size = 8))
)
(ILR_Daily_Diff_Plot = ggplot(Flow_Diff_Daily, aes(x = Date, y = ILR_difference_cfs)) +
    geom_line() +
    geom_hline(yintercept = 0) +
    scale_y_continuous(limits = c(-90,90), breaks = seq(-90,90,by = 30), expand = c(0,0)) +
    scale_x_date(limits = c(start_date,
                            end_date),
                 breaks = seq(start_date, by = "2 years", length.out = 22), expand = c(0,0),
                 date_labels = ('%b-%y')) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(fill=NA, color = 'black'),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
          axis.text.y = element_text(size = 8),
          axis.ticks = element_line(size = 0.2),
          plot.title = element_text(hjust = 0.5, size = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 8))
)
(MAR_ILR_Daily_Diff_Plot = ggplot(Flow_Diff_Daily, aes(x = Date, y = MAR_ILR_difference_cfs)) +
    geom_line() +
    geom_hline(yintercept = 0) +
    scale_y_continuous(limits = c(-100,100), breaks = seq(-100,100,by = 25), expand = c(0,0)) +
    scale_x_date(limits = c(start_date,
                            end_date),
                 breaks = seq(start_date, by = "2 years", length.out = 22), expand = c(0,0),
                 date_labels = ('%b-%y')) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(fill=NA, color = 'black'),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust= 0.7, size = 8),
          axis.text.y = element_text(size = 8),
          axis.ticks = element_line(size = 0.2),
          plot.title = element_text(hjust = 0.5, size = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 8))
)