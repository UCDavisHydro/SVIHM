# 01_InputDataDownload.R
# Downloads and processes new hydrologic data:
# 1. Precip
# 2. ET
# 3. Streamflow
# 4. Tributary Inflow (calculated from FJ Streamflow)
#
library(readr)
library(RSVP)
library(dataRetrieval)
library(lubridate)
library(cimir)

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
update_dir <- create_update_dir(end_date = model_end_date)

# Weather Data ------------------------------------------------------------------------------------

# Precip
prcp <- get_daily_precip_table(model_start_date, model_end_date)
write_swbm_precip_input_file(p_record = prcp, output_dir = update_dir, filename = 'precip.txt')

# ET
et <- build_daily_et_df(model_start_date, model_end_date)
write_swbm_et_input_file(et_record = et, output_dir = update_dir, filename = 'ref_et.txt')


# River Flows -------------------------------------------------------------------------------------

# Update observed FJ flows
fjd_model <- download_fort_jones_flow(model_start_date,
                                    model_end_date,
                                    output_dir = update_dir,
                                    save_csv = TRUE)

tribs <- get_tributary_flows(end_date = model_end_date, fj_update = fjd_model)
write_tributary_input_file(tribs, output_dir = update_dir, start_date=model_start_date, end_date=model_end_date)

