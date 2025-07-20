
#-------------------------------------------------------------------------------------------------#
get_tributary_flows <- function(start_date=as.Date('1990-10-01'),
                                end_date,
                                fj_update = NULL,
                                max_missing_days = 3,
                                regression_cutoff_date = as.Date('2018-10-01'),
                                monthly=TRUE,
                                one_regression=TRUE,
                                use_obs=TRUE,
                                verbose = TRUE) {
  # Setup read DF
  streams <- stream_metadata
  streams['FJ','name'] <- 'FJ'
  streams['FJ','daily_file'] <- 'USGS_11519500_WY_1942_2018.txt'
  streams <- streams[c("FJ", setdiff(rownames(streams), "FJ")), ]

  #-- Read in data
  daily_all <- read_gauge_daily_data(streams)

  # Pull out empty gauges - those used in regression-based SVIHM will be added back later
  daily_all <- daily_all[lengths(daily_all) != 0]

  #-- Add in new FJ data
  if (!is.null(fj_update)) {
    daily_all[[1]] <- assimilate_fj_update(daily_all[[1]], fj_update)
  }

  #-- Prepare Data for regression, pre- and post- WY1973
  prepd_all <- lapply(daily_all, FUN=gauge_prep, monthly=monthly)

  preWY1973 <- lapply(prepd_all, FUN=gauge_regression_prep, end_date = as.Date("1972/10/1"))
  postWY1973 <- lapply(prepd_all, FUN=gauge_regression_prep, start_date = as.Date("1972/10/1"),
                                                              end_date = regression_cutoff_date)

  #-- Copy over stats for gauges with no postWY1972 data and vice versa
  postWY1973 <- mapply(df.copy_stats, preWY1973, postWY1973, MoreArgs = list(overwrite=F), SIMPLIFY = F)
  preWY1973 <- mapply(df.copy_stats, postWY1973, preWY1973, MoreArgs = list(overwrite=F), SIMPLIFY = F)

  #-- Add in FJ dataset to each to use as dependent variable
  preWY1973_FJ <- add_FJ_to_dflist(preWY1973)
  postWY1973_FJ <- add_FJ_to_dflist(postWY1973)

  #-- Regress

  if (one_regression) {
    # Current methodology combines them all into one regression...
    rm.preWY1973  <- lm(normLogAF_trib ~ normLogAF_FJ, data = do.call(rbind, preWY1973_FJ))
    rm.postWY1973 <- lm(normLogAF_trib ~ normLogAF_FJ, data = do.call(rbind, postWY1973_FJ))
  } else {
    #-- Alternative: individually regress
    rm.preWY1973 <- lapply(preWY1973_FJ, function(x) {
      if (nrow(x) > 0) {
        rm <- lm(normLogAF_trib ~ normLogAF_FJ, data = x)
      }
    })
    #-- Alternative: Regression individually...
    rm.postWY1973 <- lapply(postWY1973_FJ, function(x) {
      if (nrow(x) > 0) {
        rm <- lm(normLogAF_trib ~ normLogAF_FJ, data = x)
      }
    })
  }

  #-- pRepare (Essentially need to add back any cutoff data, fill missing months with NAs)

  preWY1973 <- mapply(gauge_regression_prep, prepd_all, preWY1973, SIMPLIFY = F)
  preWY1973 <- lapply(preWY1973, complete_ts,
                      by=ifelse(monthly,'month','day'),
                      start_date=min(daily_all[[1]]$Date),
                      end_date=as.Date("1972/09/30"))  # inclusive
  preWY1973 <- add_FJ_to_dflist(preWY1973, all.x=T)

  postWY1973 <- mapply(gauge_regression_prep, prepd_all, postWY1973, SIMPLIFY = F)
  postWY1973 <- lapply(postWY1973, complete_ts,
                       by=ifelse(monthly,'month','day'),
                       start_date=as.Date("1972/10/1"),
                       end_date=end_date)
  postWY1973 <- add_FJ_to_dflist(postWY1973, all.x=T)

  #-- pRedict
  if (one_regression) {
    preWY1973_pred <- lapply(preWY1973, predict_and_destandardize, lm_object=rm.preWY1973, use_obs=use_obs)
    postWY1973_pred <- lapply(postWY1973, predict_and_destandardize, lm_object=rm.postWY1973, lm_nodata=rm.preWY1973, use_obs=use_obs)
  } else {
    preWY1973_pred <- mapply(predict_and_destandardize, preWY1973, rm.preWY1973, rm.postWY1973, use_obs=use_obs, SIMPLIFY = F)
    postWY1973_pred <- mapply(predict_and_destandardize, postWY1973, rm.postWY1973, rm.preWY1973, use_obs=use_obs, SIMPLIFY = F)
  }

  #-- Recombine
  preWY1973_pred <- mapply(gauge_reg_obs_merge, preWY1973_pred, prepd_all[2:length(prepd_all)], SIMPLIFY = F)
  postWY1973_pred <- mapply(gauge_reg_obs_merge, postWY1973_pred, prepd_all[2:length(prepd_all)], SIMPLIFY = F)
  combined <- mapply(rbind, preWY1973_pred, postWY1973_pred, SIMPLIFY = F)

  #-- Add in streams that are based on Patterson
  perc_streams <- list(Johnson = gauge_percent_model(combined[['Patterson']], 0.2, 'Johnson'),
                       Crystal = gauge_percent_model(combined[['Patterson']], 0.15, 'Crystal'))
  combined <- append(combined, perc_streams)

  #-- Split French into Miner and French
  combined[['French']]$pred_AF <- combined[['French']]$pred_AF * 0.5
  combined[['Miners']] <- combined[['French']]
  combined[['Miners']]$stream_name <- 'Miners'
  #frnc_streams <- list()

  # Combine East and South Fork stream records into one volumetric record (since that is how it's simulated in SVIHM)
  combined <- combine_east_and_south_fork(tribs_list = combined)

  #-- Return
  return(combined)

}

#-------------------------------------------------------------------------------------------------#

#' Combine East and South Fork streamflow records
#'
#' Reads in records from two main forks of Scott River and combines them for model input.
#'
#' @param tribs_list List of dataframes containing tributary flow data (gap-filled with FJ flow)
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
#' @return List of daily mean dataframes (With E and S forks replaced by Scott River)
#' @export
#'
#' @examples

combine_east_and_south_fork= function(tribs_list){
  # Combine East Fork and South Fork tributary flows,
  # since they are represented at a single inflow point at the confluence
  pred_or_obs = rep("Observed", nrow(tribs_list$East_Fork))
  pred_or_obs[tribs_list$East_Fork$source == "Predicted" |
                tribs_list$South_Fork$source == "Predicted"] = "Predicted"

  # add South and East Fork flow together to get Scott River inflow
  scott_river = data.frame(Date = tribs_list$East_Fork$Date,
                           normLogAF_trib = NA,
                           normlogAF_FJ = NA,
                           stream_name = "East_and_South_Forks",
                           pred = NA,
                           source = pred_or_obs,
                           pred_AF = tribs_list$East_Fork$pred_AF+tribs_list$South_Fork$pred_AF,
                           obs_AF = tribs_list$East_Fork$obs_AF+tribs_list$South_Fork$obs_AF) # NA for observed if either is NA

  # Remove East and South Fork data tables and add the combined data table
  # As the first stream in the list of tribs
  tribs_list$East_Fork=NULL; tribs_list$South_Fork= NULL
  tribs_list_out = list(Scott_River = scott_river)
  tribs_list_out = append(tribs_list_out, tribs_list)
  return(tribs_list_out)
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
read_gauge_daily_data <-  function(stream_df, sf_reg_dir=data_dir['sf_reg_dir', 'loc']){

  # Loop over streams reading in pre-downloaded time series datasets
  daily_means_all = list()
  for (i in 1:nrow(stream_df)) {
    if (is.na(stream_df[i,'daily_file'])) {
      message(paste('No obs data for stream:', stream_df[i,'name']))
      next
    }
    df <- read.table(file.path(sf_reg_dir, stream_df[i,'daily_file']),
                                       header = T,
                                       stringsAsFactors = F,
                                       sep = '\t')[,-4]
    df$Date = as.Date(df$Date,'%m/%d/%Y')

    # Convert to AF
    df$AF <- df$mean_cfs * 86400 * 2.2957e-5  # cfs to cfd, to acre-ft 2.29569e-5

    # Put in list
    daily_means_all[[ stream_df[i, "name"] ]] <- df
  }

  #names(daily_means_all) <- stream_metadata[!is.na(stream_metadata$daily_file), 'name']

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

# Maybe this should be incorporated into gauge_regression_prep?
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
gauge_prep <- function(df, max_missing_days=3, value_col='AF', date_col='Date', monthly=TRUE) {

  # Drop NAs
  df <- df[!is.na(df[,value_col]),]

  # Replace zeros (or mysterious negative values) with a very small value
  if (nrow(df[df[,value_col]<=0.0,]) > 0) {
    df[df[,value_col]<=0.0,value_col] <- 1e-5
  }

  if (monthly) {
    # Get monthly
    mnthly <- resample2monthly(df = df, value_col=value_col, date_col = date_col,
                               FUN=function(x) c(value_col=sum(x), count=length(x)))
    # Missing days is (Total in Month - Count)
    mnthly$missing <- lubridate::days_in_month(mnthly[,date_col]) - mnthly$count

    df <- mnthly[!mnthly$missing > max_missing_days, c(date_col, value_col)]

  }
  return(df)
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
#' @param use_obs (T/F) overwrite predictions with recorded (observed) data
#'
#' @return
#' @export
#'
#' @examples
predict_and_destandardize <- function(gauge, lm_object, lm_nodata=NULL, use_obs=TRUE) {
  # By default, use the main lm_object
  lm_use <- lm_object

  # If lm_nodata exists, and there is no observed gauge data, use the alt
  if (!is.null(lm_nodata) & nrow(gauge[!is.na(gauge$normLogAF_trib),]) == 0) {
    lm_use <- lm_nodata
  }

  # If we've been directed to not use obs, NA them out
  if (use_obs == F) {gauge$normLogAF_trib = NA}

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

#' Write Tributary Input File - Two Forks Version (old)
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
write_tributary_input_file_2_forks_old <- function(gauges,
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


#-------------------------------------------------------------------------------------------------#

#' Write Tributary Flow Record File
#'
#' @param gauges List of gauge data from \code{\link{get_tributary_flows}}
#' @param output_dir Directory to write file
#' @param start_date Simulation start date
#' @param end_date Simulation end date
#' @param filename Filename (optional, default: daily_tributary_streamflow.txt)
#' @param verbose T/F write status info to console (default: TRUE)
#'
#' @return None
#' @export
#'
#' @examples
write_trib_file <- function(gauges,
                            output_dir,
                            start_date,
                            end_date,
                            old_tribs_df = NA,
                            monthly=F,
                            filename='daily_tributary_streamflow.txt',
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
    current_name <- names(gauges)[i]
    m3_col_name <- paste0(current_name,'_Avg_Flow_m3day')  # Terrible legacy name
    if (monthly) {
      names(out)[names(out)=='Date'] <- 'Month'
      out[,m3_col_name] <- (out$pred_AF * 1233.48)/days_in_mon # AF/mon to m^3/day
      if (i == 1) {
        # Date only for the first value
        return(out[,c('Month',m3_col_name)])
      } else {
        return(out[,m3_col_name, drop=F])  # drop=F returns a dataframe instead of column
      }
    } else {
      # assume daily
      names(out)[names(out)=='Date'] <- 'Day'
      out[,m3_col_name] <- out$pred_AF * 1233.48 # AF/day to m^3/day
      if (i == 1) {
        # Date only for the first value
        return(out[,c('Day',m3_col_name)])
      } else {
        return(out[,m3_col_name, drop=F])  # drop=F returns a dataframe instead of column
      }
    }
  })
  outdf <- do.call(cbind, outdf)

  # A dumb fix for trying to be too clever
  names(outdf)[1:2] <- c(ifelse(monthly,'Month','Day'), 'Scott_River_Avg_Flow_m3day')

  # Arrange like original - instead, order enforced by calling functions
  # out_cols <- c(ifelse(monthly,'Month','Day'),
  #               "Scott_River_Avg_Flow_m3day","Sugar_Avg_Flow_m3day",
  #               "Miners_Avg_Flow_m3day","French_Avg_Flow_m3day",
  #               "Etna_Avg_Flow_m3day", "Johnson_Avg_Flow_m3day",
  #               "Crystal_Avg_Flow_m3day","Patterson_Avg_Flow_m3day",
  #               "Kidder_Avg_Flow_m3day", "Moffett_Avg_Flow_m3day",
  #               "Mill_Avg_Flow_m3day","Shackleford_Avg_Flow_m3day")

  # If old tributary inflow data provided, retain all old trib records
  if(sum(!is.na(old_tribs_df))>1){
    old_tribs_df$Scott_River_Avg_Flow_m3day = rowSums(old_tribs_df[,c("East_Fork_Avg_Flow_m3day",
                                                                      "South_Fork_Avg_Flow_m3day")])
    old_tribs_df$East_Fork_Avg_Flow_m3day = NULL; old_tribs_df$South_Fork_Avg_Flow_m3day = NULL

    # arrange columns to be consistent with output dataframe
    old_tribs_df = old_tribs_df[,colnames(outdf)]
    join_old_to_outdf = match(outdf$Month, old_tribs_df$Month) # find matching indices
    join_old_to_outdf = join_old_to_outdf[!is.na(join_old_to_outdf)] # remove NA indices
    # Assign available old values to output df
    outdf[join_old_to_outdf,] = old_tribs_df
  }

  if (verbose) {message(paste('Writing file: ', filename))}

  write.table(outdf,
              file = file.path(output_dir, filename),
              append = F,
              quote = F,
              row.names = F,
              col.names = T,
              sep = '\t')

  #return(outdf)
}


#-------------------------------------------------------------------------------------------------#

#' Write Surface Water Input File (one combined record per subwatershed)
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
write_streamflow_by_subws_input_file <- function(gauges=NA, tribs_df = NA, # function call provides either gauges (list) or tribs_df (dataframe)
                                                 output_dir,
                                                 start_date,
                                                 end_date,
                                                 monthly=T,
                                                 filename='streamflow_input.txt',
                                                 verbose=TRUE) {

  # If gauges list provided, and no tribs dataframe, build tribs dataframe
  if(sum(is.na(tribs_df))>0 & sum(!is.na(gauges))>0){
    # Need days in months for moving average from monthly to daily
    days_in_mon = days_in_month_diff(start_date, end_date)

    # 1. Compile stream records into a single dataframe
    i <- 0
    all_tribs_df <- lapply(gauges, function(x) {
      i <<- i + 1
      # Subset by date
      out <- subset.DateTwoSided(x, start_date, end_date, include_end=T)
      # Convert to m3/day and rename
      current_name <- names(gauges)[i]
      m3_col_name <- paste0(current_name,'_Avg_Flow_m3day')  # Terrible legacy name
      if (monthly) {
        names(out)[names(out)=='Date'] <- 'Month'
        out[,m3_col_name] <- (out$pred_AF * 1233.48)/days_in_mon # AF/mon to m^3/day
        if (i == 1) {
          # Date only for the first value
          return(out[,c('Month',m3_col_name)])
        } else {
          return(out[,m3_col_name, drop=F])  # drop=F returns a dataframe instead of column
        }
      } else {
        # assume daily
        names(out)[names(out)=='Date'] <- 'Day'
        out[,m3_col_name] <- out$pred_AF * 1233.48 # AF/mon to m^3/day
        if (i == 1) {
          # Date only for the first value
          return(out[,c('Day',m3_col_name)])
        } else {
          return(out[,m3_col_name, drop=F])  # drop=F returns a dataframe instead of column
        }
      }
    })
    all_tribs_df <- do.call(cbind, all_tribs_df)

    # A dumb fix for trying to be too clever
    names(all_tribs_df)[1:2] <- c(ifelse(monthly,'Month','Day'),
                                  'Scott_River_Avg_Flow_m3day')
  } else { # If tribs dataframe provided, rename variable
    all_tribs_df = tribs_df
  }


  #2. Sum stream records by subwatershed
  inflow_segs = read.table(file = file.path( data_dir["time_indep_dir",'loc'], "SFR_inflow_segments.txt"),
                           comment.char = "!", header = T, row.names = NULL, sep = "\t")
  n_subws = length(unique(inflow_segs$subws_ID))# number of subwatersheds
  # associate stream record colname with each inflow segment
  stream_tab_colname = gsub(x = inflow_segs$stream_name, pattern = "Creek", replacement = "")
  stream_tab_colname = trimws(stream_tab_colname)
  stream_tab_colname = gsub(x = stream_tab_colname, pattern = " ", replacement = "_")
  inflow_segs$stream_tab_colname = paste0(stream_tab_colname, "_Avg_Flow_m3day")

  # Make dataframe of just subwatershed ID number and the stream record name for output file
  subws_names_df = inflow_segs[,c("subws_ID","subws_name")]
  subws_names_df$subws_name = gsub(x = subws_names_df$subws_name, pattern = "Scott_Tailings",
                                   replacement = "Scott_River")
  subws_names_df = subws_names_df[!duplicated(subws_names_df),]


  # Initialize output dataframe
  outdf = all_tribs_df[1]
  outdf = cbind(outdf, matrix(data = NA, nrow = nrow(outdf), ncol = n_subws))
  colnames(outdf)[2:ncol(outdf)] = paste0(subws_names_df$subws_name,"_Avg_Flow_m3day")

  for(i in 1:n_subws){ # for each subwatershed

    subws_colname = paste0(subws_names_df$subws_name[subws_names_df$subws_ID==i], "_Avg_Flow_m3day")

    if(inflow_segs$subws_name[inflow_segs$subws_ID==i][1]=="French"){ # For French Creek drainage
      # Use only French flow (as no record for Miners Creek)
      records_in_subws = inflow_segs$stream_tab_colname[inflow_segs$subws_ID==i]
      records_in_subws = records_in_subws[grep(pattern = "French",x = records_in_subws)]
    } else { # identify inflow segment names
      records_in_subws = inflow_segs$stream_tab_colname[inflow_segs$subws_ID==i]
    }

    if(length(records_in_subws)==1){ # if only one inflow segment in the subwatershed
      # Assign subwatershed flow to single stream record
      outdf[,records_in_subws] = all_tribs_df[,records_in_subws]
    } else {
      # Assign subwatershed flow to monthly sum of stream records
      outdf[,subws_colname]= rowSums(all_tribs_df[,records_in_subws])
    }

  }

  # Arrange like original
  outdf <- outdf[,c(ifelse(monthly,"Month","Day"),
                    "Scott_River_Avg_Flow_m3day",
                    "French_Avg_Flow_m3day",
                    "Etna_Avg_Flow_m3day",
                    "Patterson_Avg_Flow_m3day",
                    "Kidder_Avg_Flow_m3day",
                    "Moffett_Avg_Flow_m3day",
                    "Mill_Avg_Flow_m3day",
                    "Shackleford_Avg_Flow_m3day")]

  if (verbose) {message(paste('Writing file: ', filename))}

  write.table(outdf,
              file = file.path(output_dir, filename),
              append = F,
              quote = F,
              row.names = F,
              col.names = T,
              sep = '\t')
}
