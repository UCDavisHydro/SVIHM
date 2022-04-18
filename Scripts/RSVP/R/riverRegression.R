
#-------------------------------------------------------------------------------------------------#
#TODO Refactor so it simply calls two functions, one for pre and one for post, then combines and returns
#TODO remove start/end, only use regression cutoff and (list?) of breaks
#TODO use a list of means and sds (and mapply?) rather than the attributes (have to copy so many times)
# Alternatively, introduce a merge.wAttr() function
get_tributary_flows <- function(start_date=as.Date('1990-10-01'),
                                end_date,
                                fj_update = NULL,
                                max_missing_days = 3,
                                regression_cutoff_date = as.Date('2018-10-01'),
                                verbose = TRUE) {

  #-- Read in data
  daily_all <- read_gauge_daily_data()

  #-- Add in new FJ data
  if (!is.null(fj_update)) {
    daily_all[[1]] <- assimilate_fj_update(daily_all[[1]], fj_update)
  }

  #-- Convert to Monthly Data
  mnthly_all <- lapply(daily_all, FUN=gauge_monthly_prep)

  #-- Prepare Data for regression, pre- and post- WY1973
  preWY1973 <- lapply(mnthly_all, FUN=gauge_regression_prep, end_date = as.Date("1972/10/1"))
  postWY1973 <- lapply(mnthly_all, FUN=gauge_regression_prep, start_date = as.Date("1972/10/1"),
                                                              end_date = regression_cutoff_date)

  #-- Copy over stats for gauges with no postWY1972 data and vice versa
  postWY1973 <- mapply(df.copy_stats, preWY1973, postWY1973, MoreArgs = list(overwrite=F), SIMPLIFY = F)
  preWY1973 <- mapply(df.copy_stats, postWY1973, preWY1973, MoreArgs = list(overwrite=F), SIMPLIFY = F)

  #-- Add in FJ dataset to each to use as dependent variable
  preWY1973_FJ <- add_FJ_to_dflist(preWY1973)
  postWY1973_FJ <- add_FJ_to_dflist(postWY1973)

  #-- Regress
  # Current methodology combines them all into one regression...
  rm.preWY1973  <- lm(normLogAF_trib ~ normLogAF_FJ, data = do.call(rbind, preWY1973_FJ))
  rm.postWY1973 <- lm(normLogAF_trib ~ normLogAF_FJ, data = do.call(rbind, postWY1973_FJ))

  #-- Alternative: Regression individually...
  all.rm.preWY1973 <- lapply(preWY1973_FJ, function(x) {
    if (nrow(x) > 0) {
      rm <- lm(normLogAF_trib ~ normLogAF_FJ, data = x)
    }
  })

  #-- pRepare (Essentially need to add back any cutoff data, fill missing months with NAs)
  preWY1973 <- mapply(gauge_regression_prep, mnthly_all, preWY1973, SIMPLIFY = F)
  preWY1973 <- lapply(preWY1973, complete_monthly, start_date=min(daily_all[[1]]$Date),
                      end_date=as.Date("1972/09/1"))  # inclusive
  preWY1973 <- add_FJ_to_dflist(preWY1973, all.x=T)

  postWY1973 <- mapply(gauge_regression_prep, mnthly_all, postWY1973, SIMPLIFY = F)
  postWY1973 <- lapply(postWY1973, complete_monthly, start_date=as.Date("1972/10/1"), end_date=end_date)
  postWY1973 <- add_FJ_to_dflist(postWY1973, all.x=T)

  #-- pRedict
  preWY1973_pred <- lapply(preWY1973, predict_and_destandardize, lm_object=rm.preWY1973)
  postWY1973_pred <- lapply(postWY1973, predict_and_destandardize, lm_object=rm.postWY1973, lm_nodata=rm.preWY1973)

  #-- Recombine
  preWY1973_pred <- mapply(gauge_reg_obs_merge, preWY1973_pred, mnthly_all[2:length(mnthly_all)], SIMPLIFY = F)
  postWY1973_pred <- mapply(gauge_reg_obs_merge, postWY1973_pred, mnthly_all[2:length(mnthly_all)], SIMPLIFY = F)
  combined <- mapply(rbind, preWY1973_pred, postWY1973_pred, SIMPLIFY = F)

  #-- Add in streams that are based on Patterson
  perc_streams <- list(Johnson = gauge_percent_model(combined[['Patterson']], 0.2, 'Johnson'),
                       Crystal = gauge_percent_model(combined[['Patterson']], 0.15, 'Crystal'))
  combined <- append(combined, perc_streams)

  #-- Return
  return(combined)

}

#-------------------------------------------------------------------------------------------------#

#' Read in River Gauge Daily Means
#'
#' Reads in primary gauge (Fort Jones) and input/tributary gauge daily. Adds Acre-ft (AF) column
#' (calculated from cfs)
#'
#' @param sf_reg_dir streamflow regression directory (optional, default: SVIHM/Streamflow_Regression_Model)
#'
#' @details Input/Tributary Gauges included:
#' * East Fork Scott River
#' * South Fork Scott River
#' * Sugar Creek
#' * Etna Creek
#' * French Creek
#' * Patterson Creek
#' * Kidder Creek
#' * Moffett Creek
#' * Mill Creek
#' * Shackleford Creek
#'
#' No Data for Clark, Johnson, Crystal, or Oro Fino Creeks.
#' @md
#'
#' @return List of daily mean dataframes (FJ followed by order above)
#' @export
#'
#' @examples
read_gauge_daily_data <-  function(sf_reg_dir=data_dir['sf_reg_dir', 'loc']){

  # Loop over streams reading in pre-downloaded time series datasets
  daily_means_all = list()
  for (i in 1:nrow(stream_metadata)) {
    df <- read.table(file.path(sf_reg_dir, stream_metadata[i,'daily_mean_file']),
                                       header = T,
                                       stringsAsFactors = F,
                                       sep = '\t')[,-4]
    df$Date = as.Date(df$Date,'%m/%d/%Y')

    # Convert to AF
    df$AF <- df$mean_cfs * 86400 * 2.2957e-5  # cfs to cfd, to acre-ft 2.29569e-5

    # Put in list
    daily_means_all[[i]] <- df
  }

  names(daily_means_all) <- stream_metadata$name

  return(daily_means_all)
}

#-------------------------------------------------------------------------------------------------#

#' Assimilate Fort Jones Historical & New Updated Data
#'
#' Assumes that fj_update is the more accurate dataset (will drop historical days with overlap)
#'
#' @param historical DataFrame of historical (1941 - ) data
#' @param fj_update DataFrame of more recent data
#'
#' @return DataFrame of single, combined dataset
#' @export
#'
#' @examples
assimilate_fj_update <- function(historical, fj_update) {
  colnames(fj_update) <-  c('Data_Source','site_no','Date','mean_cfs','Qual_Flag')
  fj_update$AF <- fj_update$mean_cfs * 86400 * 2.2957e-5
  merged <- rbind(historical[historical$Date < min(fj_update$Date),],
                  fj_update[,c('Date','mean_cfs','AF','Qual_Flag')])
  return(merged)
}

#-------------------------------------------------------------------------------------------------#

#' Title
#'
#' @param df
#' @param max_missing_days
#' @param value_col
#' @param date_col
#'
#' @return
#' @export
#'
#' @examples
gauge_monthly_prep <- function(df, max_missing_days=3, value_col='AF', date_col='Date') {

  # Drop NAs
  df <- df[!is.na(df[,value_col]),]

  # Get monthly
  mnthly <- resample2monthly(df = df, value_col=value_col, date_col = date_col,
                             FUN=function(x) c(value_col=sum(x), count=length(x)))
  # Missing days is (Total in Month - Count)
  mnthly$missing <- lubridate::days_in_month(mnthly[,date_col]) - mnthly$count

  mnthly <- mnthly[!mnthly$missing > max_missing_days, c(date_col, value_col)]

  return(mnthly)

}

#-------------------------------------------------------------------------------------------------#

#' Title
#'
#' @param gauge
#' @param start_date
#' @param end_date
#' @param value_col
#' @param date_col
#' @param transform
#' @param col_rename
#'
#' @return
#' @export
#'
#' @examples
gauge_regression_prep <- function(gauge, stats_donor=NULL, start_date=NULL, end_date=NULL, value_col='AF',
                                  date_col='Date', transform=log10, col_rename='normLogAF') {

  # Subset
  gauge <- subset.DateTwoSided(gauge, start=start_date, end=end_date, date_col='Date')

  # Optional pre-stat transformation function
  if (!is.null(transform)) {
    gauge[value_col] <- transform(gauge[value_col])
  }
  # Optional Rename
  if (!is.null(col_rename)) {
    names(gauge)[names(gauge) == value_col] <- col_rename
    value_col <- col_rename
  }

  # Standardize (copy over mean & sd if stats_donor provided, else calculate them)
  if (is.null(stats_donor)) {
    gauge <- df.add_stats(gauge, value_col = value_col)
  } else {
    attr(gauge, 'mean') <- attr(stats_donor, 'mean')
    attr(gauge, 'sd') <- attr(stats_donor, 'sd')
  }
  gauge <- df.standardarize(gauge, value_col = value_col)

  return(gauge)
}

#-------------------------------------------------------------------------------------------------#

#' Add FJ (standardized) Streamflow to dataframe to use as regression dependant variable
#'
#' @param gaugelist
#' @param FJ_id
#'
#' @return
#' @export
#'
#' @examples
add_FJ_to_dflist <- function(gaugelist, FJ_id=1, ...) {
  i <- 1  # Stream id counter
  # Loop over streams, skipping FJ (i == 1)
  outlist <- lapply(gaugelist, function(x) {
    # Skip if the stream is FJ itself
    if (i != FJ_id) {
      df <- merge(x, gaugelist[[FJ_id]], by='Date', suffixes = c('_trib','_FJ'), ...)
      if(nrow(df) > 0) {
        df$stream_name <- names(gaugelist)[[i]]
      }
      # Preserve attributes
      attr(df, 'mean') <- attr(x, 'mean')
      attr(df, 'sd') <- attr(x, 'sd')
    }
    # Thank you, next
    i <<- i+1
    return(df)
  })
  # Drop FJ from list
  outlist <- outlist[-FJ_id]
  return(outlist)
}

#-------------------------------------------------------------------------------------------------#

#' Title
#'
#' @param gauge
#' @param lm_object
#' @param lm_nodata
#'
#' @return
#' @export
#'
#' @examples
predict_and_destandardize <- function(gauge, lm_object, lm_nodata=NULL) {
  # By default, use the main lm_object
  lm_use <- lm_object

  # If lm_nodata exists, and there is no observed gauge data, use the alt
  if (!is.null(lm_nodata) & nrow(gauge[!is.na(gauge$normLogAF_trib),]) == 0) {
    lm_use <- lm_nodata
  }

  # Predict, adding "source" column for tracking
  gauge$pred <- predict(lm_use, newdata=gauge)
  gauge$source <- ifelse(is.na(gauge$normLogAF_trib), yes = 'Predicted', no='Observed')

  # Replace NA with predicted
  gauge[is.na(gauge$normLogAF_trib), 'normLogAF_trib'] <- gauge[is.na(gauge$normLogAF_trib), 'pred']

  # Destandardize, de-log
  gauge <- df.destandarize(gauge, 'normLogAF_trib', new_col='pred_AF')
  gauge$pred_AF <- 10^gauge$pred_AF
  return(gauge)
}

#-------------------------------------------------------------------------------------------------#

#' Title
#'
#' @param regression_df
#' @param obs_df
#' @param obs_cols
#'
#' @return
#' @export
#'
#' @examples
gauge_reg_obs_merge <- function(regression_df, obs_df, obs_cols=c('AF')) {
  merge_df <- merge(regression_df, obs_df[,c('Date', obs_cols)], by='Date', all.x = T)
  names(merge_df)[names(merge_df) == obs_cols] <- paste0('obs_', obs_cols)
  return(merge_df)
}

#-------------------------------------------------------------------------------------------------#

gauge_percent_model <- function(source_gauge, percent, stream_name, date_col='Date',
                                name_col='stream_name', value_col='pred_AF', source_col='source') {
  out_df <- source_gauge[,c(date_col, name_col, value_col, source_col)]
  out_df[,name_col] <- stream_name
  out_df[,value_col] <- out_df[,value_col] * percent
  out_df[,source_col] <- 'Predicted'  # Should it be more descriptive?
  return(out_df)
}

#-------------------------------------------------------------------------------------------------#

#' Write Tributary Input File
#'
#' @param gauges List of gauge data from \code{\link{get_tributary_flows}}
#' @param output_dir Directory to write file
#' @param start_date Simulation start date
#' @param end_date Simulation end date
#' @param filename Filename (optional, default: streamflow_input.txt)
#' @param verbose T/F write status info to console (default: TRUE)
#'
#' @return None
#' @export
#'
#' @examples
write_tributary_input_file <- function(gauges,
                                        output_dir,
                                        start_date,
                                        end_date,
                                        filename='streamflow_input.txt',
                                        verbose=TRUE) {

  # Need days in months for moving average from monthly to daily
  days_in_mon = days_in_month_diff(start_date, end_date)

  # Compile into a single dataframe
  i <- 0
  outdf <- lapply(gauges, function(x) {
    i <<- i + 1
    # Subset by date
    out <- subset.DateTwoSided(x, start_date, end_date, include_end=T)
    # Convert to m3/day and rename
    m3_col_name <- paste0(x$stream_name[[1]],'_Avg_Flow_m3day')  # Terrible legacy name
    names(out)[names(out)=='Date'] <- 'Month'
    out[,m3_col_name] <- (out$pred_AF * 1233.48)/days_in_mon # AF/mon to m^3/day
    if (i == 1) {
      # Date only for the first value
      return(out[,c('Month',m3_col_name)])
    } else {
      return(out[,m3_col_name, drop=F])  # drop=F returns a dataframe instead of column
    }
  })
  outdf <- do.call(cbind, outdf)

  # A dumb fix for trying to be too clever
  names(outdf)[1:2] <- c('Month','East_Fork_Avg_Flow_m3day')

  # Arrange like original
  outdf <- outdf[,c("Month","East_Fork_Avg_Flow_m3day", "South_Fork_Avg_Flow_m3day",
                    "Sugar_Avg_Flow_m3day", "French_Avg_Flow_m3day",
                    "Etna_Avg_Flow_m3day", "Johnson_Avg_Flow_m3day",
                    "Crystal_Avg_Flow_m3day","Patterson_Avg_Flow_m3day",
                    "Kidder_Avg_Flow_m3day", "Moffett_Avg_Flow_m3day",
                    "Mill_Avg_Flow_m3day","Shackleford_Avg_Flow_m3day")]

  if (verbose) {message(paste('Writing file: ', filename))}

  write.table(outdf,
              file = file.path(output_dir, filename),
              append = F,
              quote = F,
              row.names = F,
              col.names = T,
              sep = '\t')
}
