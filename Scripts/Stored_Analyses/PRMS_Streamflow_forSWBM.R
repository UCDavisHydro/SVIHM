library(RSVP)
library(hydroGOF)

# Write SWBM Streamflow File From PRMS ------------------------------------

#-------------------------------------------------------------------------------------------------#
#-- Settings
prms_dir <- file.path('C:/Projects/SVIHM/2024_PRMS/TestRun_0802/PRMS')
update_dir <- file.path("../../SVIHM_Input_Files/Updates/PRMS") # for now...
seg_file <- 'segout.txtseg_outflow.csv'

if (!dir.exists(update_dir)) { dir.create(update_dir, recursive = T)}

start_year <- 1991 # WY 1991; do not change
end_year   <- as.numeric(format(Sys.Date(), "%Y"))  # Assumes current year

model_start_date <- get_model_start(start_year)
#model_end_date <- as.Date(floor_date(Sys.Date(), 'month')-1)
model_end_date <- as.Date("2023-09-30")

num_stress_periods <- calc_num_stress_periods(model_start_date, model_end_date)
num_days <- days_in_month_diff(model_start_date, model_end_date)  # current setup: days = time steps

#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
#-- Read in PRMS segment outflow file (units are CFS)
prms <- read.csv(file.path(prms_dir,seg_file))

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

  #message(paste("Working on",name))

  # Collect PRMS data
  prms_subset <- prms[,c('Date',name)]
  colnames(prms_subset) <- c('Date', 'prms_cfs')

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

  # Convert to AF
  # TODO: When we're done with the regression, this should be m3/day for SWBM
  combined$pred_AF <- combined$merged_cfs * 86400 * 2.2957e-5  # cfs to cfd, to acre-ft 2.29569e-5
  combined$pred_noobs_AF <- combined$prms_cfs * 86400 * 2.2957e-5  # cfs to cfd, to acre-ft 2.29569e-5
  combined$obs_AF <- combined$obs_cfs * 86400 * 2.2957e-5  # cfs to cfd, to acre-ft 2.29569e-5

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

tribs_prms = write_trib_file_for_partitioning(gauges = prms_wobs, output_dir = update_dir,monthly = F,
                                                   start_date=model_start_date, end_date=model_end_date)

write_streamflow_by_subws_input_file(tribs_df = tribs_prms, #gauges = tribs,
                                     output_dir = update_dir,filename = 'daily_streamflow_input.txt',
                                     start_date=model_start_date, end_date=model_end_date, monthly = F)

#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#

# Compare Streamflow ------------------------------------------------------

#-------------------------------------------------------------------------------------------------#
#-- Setup
#plot_dir <- file.path('C:/Users/lelan/Documents/CodeProjects/SVIHM/Run/Plots/')
plot_dir <- file.path(prms_dir, '../Plots')
if (!dir.exists(plot_dir)) { dir.create(plot_dir, recursive = T)}

fjd <- read.csv('../../SVIHM_Input_Files/Updates/2024-08-01/FJ (USGS 11519500) Daily Flow, 1990-10-01_2024-07-31.csv')

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
write.csv(all_eval, file.path(prms_dir,"../model_evaluation_comparison.csv"), row.names = FALSE)

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
