# 01_InputDataDownload.R
# Downloads and processes new hydrologic data:
# 1. Precip
# 2. ET
# 3. Streamflow
# 4. Tributary Inflow (calculated from FJ Streamflow)
#
library(RSVP)
library(cimir)
library(readr)

# ------------------------------------------------------------------------------------------------#
# Settings ----------------------------------------------------------------------------------------

# Dates
start_year <- 1991 # WY 1991; do not change
end_year   <- as.numeric(format(Sys.Date(), "%Y"))  # Assumes current year

# Rainfall data requires NOAA CDO token
# (free online: https://www.ncdc.noaa.gov/cdo-web/webservices/v2)
noaa_token <- read_lines(file='_noaa_cdo_token.txt', n_max = 1)
options(noaakey = noaa_token)

# ET data requires CIMIS Token
# (free online: https://cimis.water.ca.gov/)
cimis_key <- read_lines(file='_CIMIS_API_key.txt')
cimir::set_key(cimis_key)
# ------------------------------------------------------------------------------------------------#

# ------------------------------------------------------------------------------------------------#

# Temporal discretization -------------------------------------------------------------------------

model_start_date <- get_model_start(start_year)
model_end_date <- as.Date(floor_date(Sys.Date(), 'month')-1)

num_stress_periods <- calc_num_stress_periods(model_start_date, model_end_date)
num_days <- days_in_month_diff(model_start_date, model_end_date)  # current setup: days = time steps

# Directory (Created in SVIHM_Input_Files/Updates)
update_dir <- create_update_dir(end_date = model_end_date + 1)  # +1 for start of next month, first day not simulated


# Weather Data ------------------------------------------------------------------------------------

# Precip
rnoaa::meteo_clear_cache(force = TRUE)  # Been having issues with rnoaa cache updating...
prcp <- get_daily_precip_table(model_start_date, model_end_date)
write_swbm_precip_input_file(p_record = prcp, output_dir = update_dir, filename = 'precip.txt')

# ET
et <- build_daily_et_df(model_start_date, model_end_date)
# In case of CIMIS rejecting your query (try several times to be sure), download data from
# CIMIS station 225 (daily data, csv file, metric units, 2015-04-19 through present),
# save in update_dir, and use this workaround

if(!exists("et")){
# Requests to CIMIS frequently fail, so the code is setup to try 25 times before giving up.
  for (i in 1:25) {
    message('CIMIS Attempt ',i,"/ 25")
    et <- tryCatch(build_daily_et_df(model_start_date, model_end_date),
                   error=function(cond) {return(NA)})
    if (max(!is.na(et))) { break }
    else if (i==25) { stop('Repeated CIMIS queries failed. Server may be down.')}
  }
}
write_swbm_et_input_file(et_record = et, output_dir = update_dir, filename = 'ref_et.txt')


# River Flows -------------------------------------------------------------------------------------

# Update observed FJ flows
fjd_model <- download_fort_jones_flow(model_start_date,
                                    model_end_date,
                                    output_dir = update_dir,
                                    save_csv = TRUE)
# Temporary fix -----------
# For some reason, 2024-03-01 is missing *daily* flow as of 2024-04-03. Replacing with a value calculated from hourly.
# Revist during next update... may need a more robust solution for NAs
fjd_model <- complete_ts(fjd_model, by='day', keep_col_order = T)
fjd_model[fjd_model$Date==as.Date('2024-03-01'),]$Flow <- 910.0
write.csv(fjd_model, list.files(update_dir, pattern = 'FJ (USGS 11519500)*'), row.names = FALSE)
# End temporary fix -------

tribs <- get_tributary_flows(end_date = model_end_date, fj_update = fjd_model, monthly = F, one_regression = F)

# Combine East and South Fork stream records into one volumetric record (since that is how it's simulated in SVIHM)
tribs <- combine_east_and_south_fork(tribs_list = tribs)

tribs_regressed = write_trib_file_for_partitioning(gauges = tribs, output_dir = update_dir,
                           start_date=model_start_date, end_date=model_end_date, monthly = F)

write_streamflow_by_subws_input_file(tribs_df = tribs_regressed, #gauges = tribs,
                                     output_dir = update_dir,filename = 'daily_streamflow_input.txt',
                                     start_date=model_start_date, end_date=model_end_date, monthly = F)

