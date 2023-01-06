# Various utilties
#
# Forgive me for using multiple naming conventions - LS

# Date Functions ----------------------------------------------------------

water_year_start_date <- function(year, water_years=TRUE) {
  if (water_years) {month <- 10; year <- year-1}
  else {month <- 1}
  return (as.Date(ISOdate(year, month, day=1)))
}

#-------------------------------------------------------------------------------------------------#

water_year_end_date <- function(year, water_years=TRUE) {
  if (water_years) {month <- 9; day=30}
  else {month <- 12; day=31}
  return (as.Date(ISOdate(year, month, day=day)))
}

#-------------------------------------------------------------------------------------------------#

#' Get water year for dates
#'
#' @param dates array of Dates
#' @param wy_start_month Water Year start month (10 (Oct) by default)
#'
#' @return array of water years
#' @export
#'
#' @examples
#' get_water_year(c(as.Date('1991-09-30'), as.Date('1991-10-01')))
get_water_year <- function(dates, wy_start_month=10){
  yr <- as.numeric(format(dates, '%Y'))
  mn <- as.numeric(format(dates, '%m'))
  yr[mn >= wy_start_month] <- yr[mn >= wy_start_month] + 1
  return(yr)
}

#-------------------------------------------------------------------------------------------------#

#' Get Model Start Date
#'
#' @param start_year Integer year
#' @param water_years Boolean
#'
#' @return
#' @export
#'
#' @examples
get_model_start <- function(start_year, water_years=TRUE) {
  return(water_year_start_date(year = start_year, water_years = water_years))
}

#-------------------------------------------------------------------------------------------------#

#' Get Model End Date
#'
#' @param end_year Integer year
#' @param water_years Boolean
#'
#' @return
#' @export
#'
#' @examples
get_model_end <- function(end_year, water_years=TRUE) {
  return(water_year_end_date(year = end_year, water_years = water_years))
}

#-------------------------------------------------------------------------------------------------#

#' Days in each month between two dates
#'
#' @param start
#' @param end
#'
#' @return
#' @export
#'
#' @examples
days_in_month_diff <- function(start, end) {
  # Prep
  end_year <- as.numeric(format(end, '%Y'))
  end_month <- as.numeric(format(end, '%m'))
  end_day <- as.numeric(format(end, '%d'))
  # Get months, get difference between months
  model_months <- seq.Date(from = start, to = end, by = "month")
  model_end_date_plus_one = as.Date(ceiling_date(end+30)) #as.Date(paste(end_year, end_month+1, end_day, sep = "-"))
  model_months_plus_one = seq(from = start, to = model_end_date_plus_one, by = "month")

  return(as.numeric(diff(model_months_plus_one)))
}

#-------------------------------------------------------------------------------------------------#

#' Resample Column to Monthly
#'
#' Resamples a column of a dataset to monthly, given function FUN. Returns montly dataset with
#' date column (of same original name) with the first date of the aggregated month.
#'
#' @param df Dataframe to resample
#' @param value_col Column containing data to resample
#' @param date_col Date column of df (optional, assumes "Date")
#' @param FUN a function to compute the summary statistics
#' @param ... further arguments passed to aggregate
#'
#' @return Dataframe resampled by FUN to monthly interval
#' @author Leland Scantlebury
#' @export
#'
#' @examples
resample2monthly <- function(df, value_col, date_col='Date', FUN, ...) {
  mnthly <- do.call(data.frame, aggregate(df[,value_col], by=list(format(df[,date_col], '%Y-%m')), FUN=FUN, ...))
  # "Fix" column names
  names(mnthly)[1] <- date_col
  names(mnthly)[2:ncol(mnthly)] <- gsub('x.', '', names(mnthly)[2:ncol(mnthly)])
  # Fix occasional issue
  names(mnthly)[2:ncol(mnthly)] <- gsub('value_col', value_col, names(mnthly)[2:ncol(mnthly)])
  # Fix Dates
  mnthly$Date <- as.Date(paste0(mnthly$Date,'-01'),'%Y-%m-%d')
  return(mnthly)
}

#-------------------------------------------------------------------------------------------------#

#' Aggregate value to Daily, Monthly, or Yearly
#'
#' @param dates array of Date values
#' @param values array of values to be aggregated
#' @param interval aggregation interval: 'd' for daily, 'm' for monthly, 'y' for yearly
#' @param date_col Name of output DF column for date (default: 'Date')
#' @param FUN function to aggregate by
#' @param ... Optional values passed to \code{\link{aggregate}}
#'
#' @return Data.frame of aggregated values, with a Date column (only of type 'Date' if daily)
#' @export
#'
#' @examples
#' # Some made up data
#' dates <- as.Date(c('1988-11-30','1988-11-30','1988-11-30',
#'                    '1988-12-01','1988-12-01','1988-12-01'))
#' values <- c(12,13,14,14,15,16)
#' # Use function to average by day
#' avg <- aggregate.Date(dates, values, interval='d', FUN=mean)
aggregate.Date <- function(dates, values, interval, date_col='Date', FUN, ...) {

  # Pick date format code
  if (interval=='m') {
    fmt <- '%Y-%m'
  } else if (interval=='d') {
    fmt <- '%Y-%m-%d'
  } else if (interval=='y') {
    fmt <- '%Y'
  } else {
    stop('Invalid interval (d - Day, m - Month, y - Year)')
  }

  aggie <- do.call(data.frame, aggregate(values, by=list(format(dates, fmt)), FUN=FUN, ...))
  # "Fix" column names
  names(aggie)[1] <- date_col
  names(aggie)[2:ncol(aggie)] <- gsub('x.', '', names(aggie)[2:ncol(aggie)])
  # Make date a date if daily
  if (interval=='d') {
    aggie[,date_col] <- as.Date(aggie[,date_col])
  }

  return(aggie)
}

#-------------------------------------------------------------------------------------------------#

#' Two-Sided date subset
#'
#' @param df Data.frame of time series data
#' @param start Date to start subset (inclusize)
#' @param end Date to end subset (exclusive, unless include_end=T)
#' @param date_col Column with Date-type values
#' @param include_end T/F switch if end date is inclusive (less than or equal to, default=F)
#'
#' @return
#' @export
#'
#' @examples
subset.DateTwoSided <- function(df, start=NULL, end=NULL, date_col='Date', include_end=F) {
  # Subset
  df <- df
  if (!is.null(start)) {
    df <- df[df[,date_col] >= start,]
  }
  if (!is.null(end)) {
    if (include_end) {
      df <- df[df[,date_col] <= end,]
    } else {
      df <- df[df[,date_col] < end,]
    }
  }
  return(df)
}

#-------------------------------------------------------------------------------------------------#

#' Complete monthly time series by filling with NA values
#'
#' @param df
#' @param date_col
#' @param start_date
#' @param end_date
#' @param drop_extra
#'
#' @return
#' @export
#'
#' @examples
complete_monthly <- function(df, date_col='Date', start_date=NULL, end_date=NULL, drop_extra=T) {
  if (drop_extra) {
    df <- subset.DateTwoSided(df, start = start_date, end = end_date,
                              date_col = date_col, include_end=T)
  }
  if (is.null(start_date)) { start_date <- min(df[,date_col])}
  if (is.null(end_date)) { end_date <- max(df[,date_col])}
  all_mnths <- data.frame(Date = seq.Date(start_date, end_date, by="month"))
  # Rename to date_col if necessary
  if (date_col != 'Date') {
    names(all_mnths)[names(all_mnths) == 'Date'] <- date_col
  }
  out <- merge(df, all_mnths, by=date_col, all=T)
  # Not really much of a time saver if you have to copy it over each time, eh?
  attr(out, 'mean') <- attr(df, 'mean')
  attr(out, 'sd') <- attr(df, 'sd')
  return(out)
}

#-------------------------------------------------------------------------------------------------#

#' Find periods of time series with data
#'
#' @param df Data.frame of time series data
#' @param max_missing_days Maximum number of days allowed to be missing before constituting a
#' separate data period
#' @param date_col Column with Date-type values
#' @param value_col Column with data whereupon non-NA values constitute data being present
#'
#' @return Data.frame with period starts and ends as dates
#' @export
#'
#' @examples
#' # Load tributary data
#' daily_all <- read_gauge_daily_data()
#'
#' # Subset to South Fork
#' sf <- df <- daily_all$South_Fork
#'
#' # Get data periods where no more than a month of data is missing
#' sf_periods <- find_data_periods(sf, max_missing_days=30, value_col='mean_cfs')
find_data_periods <- function(df, max_missing_days, date_col='Date', value_col='value') {

  # Remove missing
  df <- df[!is.na(df[value_col]),]

  # Calc day diff
  df[1:nrow(df)-1, 'diff_head'] <- diff(df[,date_col])
  df[2:nrow(df), 'diff_tail'] <- diff(df[,date_col])

  # Find ranges
  starts <- c(df[1,date_col], na.omit(df[df$diff_tail > max_missing_days,date_col]))
  ends   <- c(na.omit(df[df$diff_head > max_missing_days,date_col]), df[nrow(df),date_col])

  have_data <- data.frame('Start' = starts, 'End' = ends)

  return(have_data)
}

#-------------------------------------------------------------------------------------------------#

# Statistics --------------------------------------------------------------

#' Add basic stats (mean, sd) to dataframe attributes
#'
#' @param df Data.frame of data
#' @param value_col Column name to calculate stats on
#' @param na.rm a logical value indicating whether NA values should be
#' stripped before the computation proceeds. (optional)
#'
#' @return Dataframe with added statistic attributes
#' @author Leland Scantlebury
#' @export
#'
#' @examples
df.add_stats <- function(df, value_col, na.rm=FALSE) {
  df.mean <- mean(df[,value_col], na.rm=na.rm)
  df.sd   <- sd(df[,value_col], na.rm=na.rm)
  attr(df, 'mean') <- df.mean
  attr(df, 'sd') <- df.sd
  return(df)
}

#-------------------------------------------------------------------------------------------------#

#' Copy data frame basic stats
#'
#' @param df1 Source of statistics dataframe
#' @param df2 Dataframe to copy stats too
#' @param overwrite T/F, check is stats already exist before copying, don't copy if they exist
#' (default: True)
#'
#' @return dataframe with mean & sd as attributes
#' @author Leland Scantlebury
#' @export
#'
#' @examples
df.copy_stats <- function(df1, df2, overwrite=T) {
  if (!overwrite) {
    # Check if stats exist before copying
    if(!is.na(attr(df2, 'mean'))) {
      # Do nothing, EARLY EXIT
      return(df2)
    }
  }
  attr(df2, 'mean') <- attr(df1, 'mean')
  attr(df2, 'sd') <- attr(df1, 'sd')
  return(df2)
}

#-------------------------------------------------------------------------------------------------#

#' Add basic stats (mean, sd), calculated over a time subset, to dataframe attributes
#'
#' @param df Data.frame of time series data
#' @param value_col Column name to calculate stats on
#' @param date_col Column with Date-type values
#' @param start_date Date at which to start the statistic calculation
#' @param end_date Date at which to end the statistic calculation
#' @param na.rm a logical value indicating whether NA values should be
#' stripped before the computation proceeds.
#'
#' @return Dataframe with added statistic attributes
#' @author Leland Scantlebury
#' @export
#'
#' @examples
df.add_stats.byDate <- function(df, value_col, date_col='Date',
                                start_date=NULL, end_date=NULL, na.rm=F) {
  # Subset
  df_stat <- df
  df_stat <- subset.DateTwoSided(df_stat, start=start_date, end=end_date, date_col=date_col)

  # Do stats, but add to original df
  df.mean <- mean(df_stat[,value_col], na.rm=na.rm)
  df.sd   <- sd(df_stat[,value_col], na.rm=na.rm)
  attr(df, 'mean') <- df.mean
  attr(df, 'sd') <- df.sd
  return(df)
}

#-------------------------------------------------------------------------------------------------#

#' Standardize Data (z-score normalization)
#'
#' Standardizes value_col by subtracting the mean and dividing by the standard deviation
#'
#' @param df Dataframe, with attributes mean and sd related to value_col
#' (see \code{\link{df.add_stats}})
#' @param value_col Column name to normalize
#'
#' @return dataframe with standardized value_col
#' @author Leland Scantlebury
#' @export
#'
#' @examples
df.standardarize <- function(df, value_col) {
  if (!value_col %in% names(df)) {
    stop('Missing value_col from input dataframe.')
  }
  df[,value_col] <- (df[,value_col] - attr(df, 'mean')) / attr(df, 'sd')
  return(df)
}

#-------------------------------------------------------------------------------------------------#

#' De-Standarize Data
#'
#' De-standarizes data by multiplying by the standard deviation and adding the mean. Reverse of
#' df.standarize
#'
#' @param df Dataframe, with attributes mean and sd related to value_col
#' (see \code{\link{df.add_stats}})
#' @param value_col Column name to denormalize
#' @param new_col Column to write destandardized data too (optional, by default will overwrite
#' value_col)
#'
#' @return dataframe with de-standarized data
#' @author Leland Scantlebury
#' @export
#'
#' @examples
df.destandarize <- function(df, value_col, new_col=NULL) {
  new <- df[,value_col] * attr(df, 'sd') + attr(df, 'mean')
  if (is.null(new_col)) {
    df[,value_col] <- new
  } else {
    df[,new_col] <- new
  }
  return(df)
}

#-------------------------------------------------------------------------------------------------#

# MODFLOW -----------------------------------------------------------------

#' Calculate Number of Stress Periods
#'
#' @param model_start Date of model start
#' @param model_end Date of model end
#' @param interval increment of the sequence, see seq.Date argument "by". "month" by default.
#'
#' @return
#' @export
#'
#' @examples
calc_num_stress_periods <- function(model_start, model_end, interval='month') {
  return(length(seq.Date(from = model_start, to = model_end, by = interval)))
}

#-------------------------------------------------------------------------------------------------#

#' MODFLOW Model Time to Date
#'
#' Assumes sp in months, and ts in days! Rounds to nearest day to prevent minor numerical errors
#'
#' @param sp Stress period
#' @param ts Time Step
#' @param origin_date Date at ts=0
#'
#' @return
#' @author Leland Scantlebury
#' @export
#'
#' @examples
mftime2date <- function(sp, ts, origin_date) {
  # Note: I hate sp/ts math - Leland
  return(origin_date %m+% days(1) %m+% months(sp-1) %m+% days(ts-1))
}

#-------------------------------------------------------------------------------------------------#

# File Management  -------------------------------------------------------------

#' Create Update Directory
#'
#' For ModelUpdate.R to create/check if update directory exists. File formula is
#' SVIHM_Input_Files/Updates/<current date>
#'
#' @return new or existing update directory
#' @export
#'
#' @examples
#' create_update_dir()
create_update_dir <- function(end_date) {
  update_dir <- file.path(data_dir['update_dir','loc'], end_date)
  if (!dir.exists(update_dir)) {
    message(paste('Creating Directory:',update_dir))
    dir.create(update_dir, recursive = T)
  } else {
    message(paste('Directory:',update_dir, 'already exists.'))
    message('Warning: Files created by this script may overwrite those in the directory!')
  }
  return(update_dir)
}

#-------------------------------------------------------------------------------------------------#

#' Return Latest (most recent) Directory in a Directory
#'
#' @param dir path to directory to search through
#'
#' @return Latest directory full filepath
#' @author Leland Scantlebury
#' @export
#'
#' @examples
latest_dir <- function(dir) {
  fdf <- file.info(list.files(path=dir, include.dirs = T, full.names = T))
  fdf <- fdf[fdf[['isdir']]==T,]
  return(rownames(fdf)[which.max(fdf$mtime)])
}

#-------------------------------------------------------------------------------------------------#

# Misc. -------------------------------------------------------------------

#' Time Series Observation & Simulation Dataframe CombineR
#'
#' @param obs_df_list list of observation data frames
#' @param sim_df_list list of simulation data frames
#' @param group_names char array of names corresponding to obs-sim dataframe pairs. Intended
#' to allow datasets to be subset easily after the merge
#' @param date_cols char array of date column names for obs and sim dataframes, respectively.
#' Default: c('Date','Date')
#' @param val_cols char array of value column names for obs and sim dataframes, respectively
#' @param col_rename bool, rename value columns obs and sim (downside: user may lose track of units).
#' Default: TRUE
#' @param save_cols char array, names of extra columns to preserve in merge. Default: empty.
#'
#' @note identical val columns between sim/obs datasets likely will fail during the subset step
#' since the second val_col will likely be renamed during the date merge
#'
#' @return dataframe of Date, Obs, Sim, group and save_cols
#' @export
#'
#' @examples
ts_obs_sim_combine <- function(obs_df_list, sim_df_list, group_names, date_cols=c('Date','Date'),
                   val_cols, col_rename=T, save_cols=c()) {
  # Verify
  if (length(obs_df_list) != length(sim_df_list)) {
    stop('Must pass identical length lists of obs and sim dataframes')
  }
  if (length(obs_df_list) != length(group_names)) {
    stop('Must pass one name per dataset in lists')
  }
  #TODO will subetting break if both val_cols are the same??

  # Process
  merged <- mapply(merge,
                   obs_df_list, sim_df_list,
                   MoreArgs = list('by.x'=date_cols[1], 'by.y'=date_cols[2]),
                   SIMPLIFY = F)
  i <- 1
  merged <- lapply(merged, function(x) {
    # Subset
    x <- x[,c(date_cols[1], val_cols, save_cols)]
    if (col_rename) {
      names(x)[2:3] <- c('obs','sim')
    }
    # Add Name
    x$group <- group_names[i]
    i <<- i + 1
    return(x)
  })

  # All together now
  merged <- do.call(rbind, merged)

  # Done!
  return(merged)
}
