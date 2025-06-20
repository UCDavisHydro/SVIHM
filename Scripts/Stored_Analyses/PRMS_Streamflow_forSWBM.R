library(RSVP)
library(hydroGOF)

# Write SWBM Streamflow File From PRMS ------------------------------------

#-------------------------------------------------------------------------------------------------#
#-- Settings
model_output_dir <- "C:/Users/lelan/Box/Research/Scott Valley/Models/PRMS/2025-04-29/output/"
update_dir <- latest_dir(data_dir['update_dir','loc'])
out_dir <- file.path(data_dir['input_files_dir','loc'],'PRMS_outputs_for_SWBM')
plot_dir <- file.path(out_dir, 'Plots')

seg_file <- 'seg_outflow.csv'


if (!dir.exists(plot_dir)) { dir.create(plot_dir, recursive = T)}
dir.create(out_dir, recursive = T)

start_year <- 1991 # WY 1991; do not change
end_year   <- as.numeric(format(Sys.Date(), "%Y"))  # Assumes current year

model_start_date <- get_model_start(start_year)
#model_end_date <- as.Date(floor_date(Sys.Date(), 'month')-1)
model_end_date <- as.Date("2023-09-30")

num_stress_periods <- calc_num_stress_periods(model_start_date, model_end_date)
num_days <- days_in_month_diff(model_start_date, model_end_date)  # current setup: days = time steps

use_gauge_data <- FALSE

#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
#-- Read in PRMS segment outflow file (units are CFS)
prms <- read.csv(file.path(model_output_dir,seg_file))

#-- Subset to our streams
prms <- prms[,colnames(prms) %in% c('Date',paste0('X',stream_metadata$PRMS_seg))]

#-- Create a logical vector for renaming
rename_cols <- colnames(prms) != 'Date'

#-- Rename columns (excluding "Date")
colnames(prms) <- c('Date',stream_metadata$name[match(colnames(prms)[rename_cols],
                                                           paste0("X", stream_metadata$PRMS_seg))])
prms$Date <- as.Date(prms$Date)
#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
#-- Read in observed gauge data
daily_all <- read_gauge_daily_data()

#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
#-- Loop through joining and combining each time series
streams <- colnames(prms)[rename_cols]
prms_wobs <- lapply(streams, FUN=function(name){

  message(paste("Working on",name))

  # Collect PRMS data
  prms_subset <- prms[,c('Date',name)]
  colnames(prms_subset) <- c('Date', 'prms_cfs')

  if (use_gauge_data) {
    # Do we have observed data for the stream?
    if (name %in% names(daily_all)) {

      # Collect Gauge data
      gage_subset <- daily_all[[name]][,c('Date','mean_cfs')]
      gage_subset <- gage_subset[!is.na(gage_subset$mean_cfs),]
      colnames(gage_subset) <- c('Date', 'obs_cfs')

      combined <- merge(prms_subset, gage_subset, by="Date", all.x = TRUE)
    } else {
      combined <- prms_subset
      combined$obs_cfs <- NA
    }

    # Make a merged column
    combined$merged_cfs <- ifelse(!is.na(combined$obs_cfs), combined$obs_cfs, combined$prms_cfs)
  } else {
    combined <- prms_subset
    combined$merged_cfs <- combined$prms_cfs
  }

  # Convert to AF
  combined$pred_AF <- combined$merged_cfs * 86400 * 2.2957e-5  # cfs to cfd, to acre-ft
  combined$pred_noobs_AF <- combined$prms_cfs * 86400 * 2.2957e-5  # cfs to cfd, to acre-ft
  if (use_gauge_data){
    combined$obs_AF <- combined$obs_cfs * 86400 * 2.2957e-5  # cfs to cfd, to acre-ft
  }

  # Just to be sure... make sure there are no NAs
  if (nrow(combined[is.na(combined$pred_AF),]) > 0) {stop('NAs present in pred_AF for ',name)}

  return(combined)
})
prms_wobs <- setNames(prms_wobs, streams)

#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
#-- Assemble into a SWBM streamflow file

# Combine East and South Fork stream records into one volumetric record (since that is how it's simulated in SVIHM)
#tribs <- combine_east_and_south_fork(tribs_list = prms_wobs)

# Combine South & East forks (TODO: Revise the function we have to do this)
# Merge the dataframes on the "Date" column
scott_df <- merge(prms_wobs$East_Fork, prms_wobs$South_Fork, by = "Date", all = TRUE, suffixes = c("_East_Fork", "_South_Fork"))

# Sum the corresponding columns (excluding "Date")
for (col in colnames(prms_wobs$East_Fork)[2:length(colnames(prms_wobs$East_Fork))]) {
  scott_df[[col]] <- rowSums(scott_df[, grep(col, colnames(scott_df))], na.rm = FALSE)
}

# Add the merged dataframe to prms_wobs under the name "Scott_River"
prms_wobs$Scott_River <- scott_df

#-- Split French into Miner and French
prms_wobs[['French']]$pred_AF <- prms_wobs[['French']]$pred_AF * 0.5
prms_wobs[['Miners']] <- prms_wobs[['French']]
prms_wobs[['Miners']]$stream_name <- 'Miners'

# Move Scott River to first, take out east and west & FJ
prms_wobs <- prms_wobs[c('Scott_River', names(prms_wobs)[!names(prms_wobs) %in% c('East_Fork','South_Fork','Scott_River', 'FJ')])]

# `tribs_prms` converts from AF/day to m3/day
write_trib_file(prms_wobs, output_dir = out_dir,filename = 'daily_tributary_streamflow_prms.txt',
                start_date=model_start_date, end_date=model_end_date, monthly = F)

#-------------------------------------------------------------------------------------------------#

# Generate various SWBM trib inputs ---------------------------------------

subws_inflow_filename = file.path(out_dir,"daily_tributary_streamflow_prms.txt")
subws_irr_inflows <- process_sfr_inflows(model_start_date, model_end_date,
                                         stream_inflow_filename = subws_inflow_filename,
                                         avail_for_irr = T,
                                         scenario_id = current_scenario)
subws_nonirr_inflows <- process_sfr_inflows(model_start_date, model_end_date,
                                            stream_inflow_filename = subws_inflow_filename,
                                            avail_for_irr = F,
                                            scenario_id = current_scenario)

# Move water to non-irr to enforce SW curtailments (also in curtailment file, but with slightly different dates for GW)

# 2021
subws_nonirr_inflows <- move_inflows(subws_nonirr_inflows, subws_irr_inflows, date_start = '2021-09-10', date_end = '2021-10-25')
subws_irr_inflows <- set_inflows(subws_irr_inflows, date_start = '2021-09-10', date_end = '2021-10-25', value = 0.0)

# 2022
subws_nonirr_inflows <- move_inflows(subws_nonirr_inflows, subws_irr_inflows, date_start = '2022-07-01', date_end = '2022-12-27')
subws_irr_inflows <- set_inflows(subws_irr_inflows, date_start = '2022-07-01', date_end = '2022-12-27', value = 0.0)

# Write it out
write_SWBM_SFR_inflow_files(subws_irr_inflows, out_dir, "subwatershed_irrigation_inflows_prms.txt")
write_SWBM_SFR_inflow_files(subws_nonirr_inflows, out_dir, "subwatershed_nonirrigation_inflows_prms.txt")

#-------------------------------------------------------------------------------------------------#

# Compare Streamflow ------------------------------------------------------

#-------------------------------------------------------------------------------------------------#
#-- Setup
fjd <- read.csv(file.path(update_dir, list.files(update_dir, pattern = 'FJ (USGS 11519500)*')))

# Do tribs regression
tribs_regr_no_obs <- get_tributary_flows(end_date = model_end_date, fj_update = fjd, monthly = F, one_regression = F, use_obs = F)
tribs_regr_no_obs <- lapply(tribs_regr_no_obs, FUN=subset.DateTwoSided, start=model_start_date, end=model_end_date+1)
tribs_regr <- get_tributary_flows(end_date = model_end_date, fj_update = fjd, monthly = F, one_regression = F, use_obs = T)
tribs_regr <- lapply(tribs_regr, FUN=subset.DateTwoSided, start=model_start_date, end=model_end_date+1)
#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
## Evaluate fit of PRMS and Regression vs Observed

# Create a helper function to add Stream and Model columns
add_stream_model_info <- function(stats_df, stream_name, model_name) {
  stats_df$Stream <- stream_name
  stats_df$Model <- model_name
  return(stats_df)
}

# PRMS v Observed
prms_noobs_eval <- lapply(names(prms_wobs), function(stream_name) {
  df <- prms_wobs[[stream_name]]
  stats <- calc_split_sample_stats(df$Date, df$pred_noobs_AF, df$obs_AF, split_date=model_start_date,
                                   list(NSE, rmse, KGE), c('NSE', 'RMSE', 'KGE'))
  stats <- add_stream_model_info(stats, stream_name, "PRMS")
  return(stats)
})

# Regression v Observed
regr_noobs_eval <- lapply(names(tribs_regr_no_obs), function(stream_name) {
  df <- tribs_regr_no_obs[[stream_name]]
  if (all(is.na(df$obs_AF))) {
    stats <- data.frame(Statistic=c('NSE', 'RMSE', 'KGE'), All=NA, Pre=NA, Post=NA)
  } else {
    stats <- calc_split_sample_stats(df$Date, df$pred_AF, df$obs_AF, split_date=model_start_date,
                                     list(NSE, rmse, KGE), c('NSE', 'RMSE', 'KGE'))
  }
  stats <- add_stream_model_info(stats, stream_name, "Regression")
  return(stats)
})

# Combine all results into one data frame
all_eval <- do.call(rbind, c(prms_noobs_eval, regr_noobs_eval))

# Write the combined results to a CSV file
write.csv(all_eval, file.path(out_dir,"model_evaluation_comparison.csv"), row.names = FALSE)

#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
## Compare PRMS to Regression
# Start PDF
pdf(file.path(plot_dir,'Trib_PRMSvRegression.pdf'), width=11, height=8.5)

# Loop over tribs
for (name in intersect(names(prms_wobs), names(tribs_regr))) {
  prms <- prms_wobs[[name]][,c('Date', 'pred_AF')]
  regr <- tribs_regr[[name]][,c('Date', 'pred_AF')]

  prms <- aggregate.Date(prms$Date, prms$pred_AF, interval='m', FUN=sum)
  regr <- aggregate.Date(regr$Date, regr$pred_AF, interval='m', FUN=sum)

  comp <- merge(prms, regr, by = 'Date', suffixes = c('_prms','_regr'))
  comp$Date <- as.Date(paste0(comp$Date,"-15"))

  # Seperate out early time data
  comp$group <- 'Primary'
  #comp[year(comp$Date) <= 1990,'group'] <- 'Warmup'

  # Seperate out by season?
  comp$season <- with(comp, ifelse(months(Date) %in% c("December", "January", "February"), "Winter",
                               ifelse(months(Date) %in% c("March", "April", "May"), "Spring",
                                      ifelse(months(Date) %in% c("June", "July", "August"), "Summer",
                                             ifelse(months(Date) %in% c("September", "October", "November"), "Fall", NA)))))

  # Plot!
  plot.scatterbox(comp$x_regr, comp$x_prms, groups = comp$group,
                  xlab = 'Monthly Sum Regression AF',
                  ylab = 'Monthly Sum PRMS AF',
                  col=c('grey40','orangered'),
                  title=paste('Tributary:',name), pch=16, log=T)
  plot.scatterbox(comp$x_regr, comp$x_prms, groups = comp$season,
                  xlab = 'Monthly Sum Regression AF',
                  ylab = 'Monthly Sum PRMS AF',
                  title=paste('Tributary:',name), pch=16, log=T)

}
dev.off()
#-------------------------------------------------------------------------------------------------#
