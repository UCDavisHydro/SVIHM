# 02_BaseUpdate.R
#
#
library(RSVP)
library(sf)

# ------------------------------------------------------------------------------------------------#
# Settings ----------------------------------------------------------------------------------------
# Dates
start_year <- 1991 # WY 1991; do not change
end_year   <- as.numeric(format(Sys.Date(), "%Y"))  # Assumes current year

# Directory (Created in SVIHM_Input_Files/Updates) - Grabs latest version
update_dir <- latest_dir(data_dir['update_dir','loc'])
update_dir = "C:/Users/ck798/Documents/GitHub/SVIHM/Updates/2024-10-01"

# Scenario selection
current_scenario = "no_pumping" # default is "basecase". Affects a variety of input files.

# Scenario Parameters -----------------------------------------------------

scen_param_tab = data.frame(scen_id = c("basecase", "no_pumping",
                                        "basecase_noMAR", "maxMAR2024",
                                        "no_gw_irr_3a", "no_gw_irr_3b", "no_gw_irr_3c", "no_gw_irr_3d",
                                        "natveg_all_4a", "natveg_all_4b", "natveg_all_4c", "natveg_all_4d"),
                            swbm_natveg_RD_m = c(rep(NA, 4),  #defaults to basecase value in orig. files
                                                 rep(c(1.2, 2.4),4)),
                            natveg_kc = c(rep(NA, 4), # defaults to basecase value in orig. files
                                          rep(c(0.6, 0.6, 1, 1),2)),
                            modflow_ExtD_m = c(rep(NA, 4), # defaults to basecase values in orig. file
                                               rep(3.05, 8)),
                            change_polygons_file = c(F, T, rep(F, 10)),
                            basecase_landuse = c(rep(T, 8), rep(F, 4)),
                            mar_scen = c("basecase","basecase","none","max",rep("none",8)),
                            curtail_scen = c(rep("basecase",4), rep("none",8)))


# ------------------------------------------------------------------------------------------------#

# Temporal discretization -------------------------------------------------------------------------

model_start_date <- get_model_start(start_year)
model_end_date <- as.Date(basename(update_dir))-1

# For June 2023 attempts at forecasting the impact of 2023 curtailment:
# if(current_scenario %in% forecast_2023_curtail){model_end_date <- as.Date('2023-12-31')}
# for running the curtailment writing function and generating modflow inputs.
# For non-curtailment SWBM functions, use standard model end date (i.e. 2023-05-31)
# and hand-stitch on the 2019 data for the remainder of 2023.

num_stress_periods <- calc_num_stress_periods(model_start_date, model_end_date)
num_days <- days_in_month_diff(model_start_date, model_end_date)  # current setup: days = time steps

# ------------------------------------------------------------------------------------------------#


# Create Time-Varying SWBM Inputs -----------------------------------------

# Requires Flow file to have been downloaded to update_dir
fjd <- read.csv(file.path(update_dir, list.files(update_dir, pattern = 'FJ (USGS 11519500)*')),
                          stringsAsFactors = F)
fjd$Date <- as.Date(fjd$Date)

sfr_subws_flow_partitioning <- gen_sfr_flow_partition(model_start_date, model_end_date, update_dir, monthly=F,
                                                              streamflow_records_file="streamflow_records_regressed.txt")
subws_inflow_filename = file.path(update_dir,"daily_streamflow_input.txt")
subws_irr_inflows <- process_sfr_inflows(model_start_date, model_end_date,
                                         stream_inflow_filename = subws_inflow_filename,
                                         avail_for_irr = T,
                                         scenario_id = current_scenario) # Possibly divide flow into avail and unavail for irr based on flow regime
subws_nonirr_inflows <- process_sfr_inflows(model_start_date, model_end_date,
                                            stream_inflow_filename = subws_inflow_filename,
                                            avail_for_irr = F,
                                            scenario_id = current_scenario) # Possibly divide flow into avail and unavail for irr based on flow regime

# Move water to non-irr to enforce SW curtailments (also in curtailment file, but with slightly different dates for GW)

# 2021
subws_nonirr_inflows <- move_inflows(subws_nonirr_inflows, subws_irr_inflows, date_start = '2021-09-10', date_end = '2021-10-25')
subws_irr_inflows <- set_inflows(subws_irr_inflows, date_start = '2021-09-10', date_end = '2021-10-25', value = 0.0)

# 2022
subws_nonirr_inflows <- move_inflows(subws_nonirr_inflows, subws_irr_inflows, date_start = '2022-07-01', date_end = '2022-12-27')
subws_irr_inflows <- set_inflows(subws_irr_inflows, date_start = '2022-07-01', date_end = '2022-12-27', value = 0.0)

# Write SWBM Inputs -------------------------------------------------------

# Main input file
write_SWBM_main_input_file(output_dir = update_dir, num_stress_periods = num_stress_periods)

# Stress Period Days file
# Include stress period # and column names in input file
num_days_tab = data.frame("stress_period" = 1:num_stress_periods, ndays = num_days)
write.table(num_days_tab, file = file.path(update_dir, "stress_period_days.txt"),
            sep = " ", quote = FALSE, col.names = TRUE, row.names = FALSE)

# Drains
write_SWBM_drain_files(num_stress_periods = num_stress_periods, output_dir = update_dir)

# Instream available flow ratio
# write_SWBM_instream_available_file(avail_monthly, output_dir = update_dir)

# Crop coefficients
write_daily_crop_coeff_values_file(model_start_date, model_end_date, update_dir,
                                   scenario_id = current_scenario)

# Surface flow / SFR files
write_SWBM_SFR_inflow_files(sfr_subws_flow_partitioning, update_dir, "SFR_subws_flow_partitioning.txt")
write_SWBM_SFR_inflow_files(subws_irr_inflows, update_dir, "subwatershed_irrigation_inflows.txt")
write_SWBM_SFR_inflow_files(subws_nonirr_inflows, update_dir, "subwatershed_nonirrigation_inflows.txt")
write_SWBM_SFR_diversions_file(output_dir = update_dir, num_divs = 0)  # Using SWBM irr ditch capability instead

# Specified pumping data (if available)
write_ag_pumping_file(start_date = model_start_date, n_stress = num_stress_periods,
                      output_dir = update_dir, ag_pumping_data = NA)
write_muni_pumping_file(start_date = model_start_date, n_stress = num_stress_periods,
                      output_dir = update_dir, muni_pumping_data = NA)

# Land use by field by month. Potentially also edit the master field attributes
# table, the daily kc and ET-from-GW inputs.
write_SWBM_landcover_file(scenario_id = current_scenario, output_dir = update_dir,
                          start_date = model_start_date, end_date = model_end_date)
# Master field attributes table (polygons_table.txt)
copy_or_overwrite_poly_table(scenario_id = current_scenario, output_dir = update_dir)
# Rooting depth in the crop parameters table
if(!is.na(scen_param_tab$swbm_natveg_RD_m[scen_param_tab$scen_id == current_scenario])){
    write_updated_crop_info(scenario_id = current_scenario, output_dir = update_dir,
                        start_date = model_start_date, end_date = model_end_date)
}
# Extinction depth for ET-from-GW
if(!is.na(scen_param_tab$modflow_ExtD_m[scen_param_tab$scen_id == current_scenario])){
  write_updated_ET_inputs(scenario_id = current_scenario, output_dir = update_dir,
                          start_date = model_start_date, end_date = model_end_date)
}

# MAR applications by field by month
write_SWBM_MAR_depth_file(scenario_id = current_scenario, output_dir = update_dir,
                        start_date = model_start_date, end_date = model_end_date)
# Irrigation curtailment fractions (as fraction of calculated demand) by field by month
# Also includes Local Cooperative Solutions (LCSs) that reduce water use (implemented as curtailment)
write_SWBM_curtailment_file(scenario_id = current_scenario, output_dir = update_dir,
                            start_date = model_start_date, end_date = model_end_date)
# ET Correction file
# Includes LCSs that essentially reduce evaporated water losses
write_SWBM_ET_correction_file(output_dir = update_dir,
                              start_date = model_start_date,
                              end_date = model_end_date)

# SFR Network file (requires total time steps)
write_sfr_network_file(nsteps = sum(num_days), output_dir = update_dir, daily = TRUE)

# ------------------------------------------------------------------------------------------------#

# Write MODFLOW Inputs ----------------------------------------------------

# Discretization (DIS)
update_DIS_stress_periods(num_days, num_stress_periods, output_dir = update_dir)

# Drain (DRN)
update_DRNO_stress_periods(num_stress_periods, output_dir = update_dir)

# Head Observations (HOB)
#write_SVIHM_head_obs_file(model_start_date, model_end_date, output_dir = update_dir)
# No longer written each time. Superseded by HOB file in basecase.

# Output Control (OC)
update_OC_stress_periods(num_days, num_stress_periods,
                         output_dir = update_dir, monthly=F,
                         save_budget=F, save_drawdown=F)

# ------------------------------------------------------------------------------------------------#


# Batch File --------------------------------------------------------------

# batch_file_name = paste0("Prepare_",current_scenario,"_Run.bat")
# if(!file.exists(file.path(data_dir["svihm_dir","loc"], batch_file_name))){
#   write_scenario_prep_batchfile(scenario_name = current_scenario)
#   }

# Create new batchfile for assembling the updated model
write_update_prep_batchfile(update_dir = update_dir,
                            scenario_name = current_scenario)

#TODO - Have function that ensures all necessary files are in the updates folder


