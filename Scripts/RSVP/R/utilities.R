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
  model_end_date_plus_one = as.Date(ceiling_date(end+28)) #as.Date(paste(end_year, end_month+1, end_day, sep = "-"))
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

#' Complete time series by filling with NA values
#'
#' @param df
#' @param date_col
#' @param start_date
#' @param end_date
#' @param drop_extra
#' @param keep_col_order
#'
#' @return
#' @export
#'
#' @examples
complete_ts <- function(df, date_col='Date', by="month", start_date=NULL, end_date=NULL, drop_extra=T, keep_col_order=F) {
  col_order <- names(df)
  if (drop_extra) {
    df <- subset.DateTwoSided(df, start = start_date, end = end_date,
                              date_col = date_col, include_end=T)
  }
  if (is.null(start_date)) { start_date <- min(df[,date_col])}
  if (is.null(end_date)) { end_date <- max(df[,date_col])}
  complete <- data.frame(Date = seq.Date(start_date, end_date, by=by))
  # Rename to date_col if necessary
  if (date_col != 'Date') {
    names(complete)[names(complete) == 'Date'] <- date_col
  }
  out <- merge(df, complete, by=date_col, all=T)
  # Not really much of a time saver if you have to copy it over each time, eh?
  attr(out, 'mean') <- attr(df, 'mean')
  attr(out, 'sd') <- attr(df, 'sd')
  if (keep_col_order) {out <- out[,col_order]}
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
#' Converts a stress-period index (in months) and timestep (in days) into an actual
#' Date, relative to a given origin.  Rounds internally to the nearest day.
#'
#' Assumes
#' * \code{sp} counts whole months since \code{origin_date} (1 ??? first month),
#' * \code{ts} counts days *within* that month (1 ??? first day).
#'
#' @param sp Integer or numeric vector of stress-period numbers (months since origin, 1-based).
#' @param ts Integer or numeric vector of timesteps (days within that period, 1-based).
#' @param origin_date Date (or character coercible to Date) corresponding to \code{sp = 1, ts = 1}.
#'
#' @return A \code{Date} vector, same length as \code{sp} and \code{ts}.
#'
#' @importFrom lubridate days months
#' @export
#'
#' @examples
#' # origin = 1990-09-30, sp=1 ts=1 ??? 1990-10-01
#' mftime2date(1, 1, origin_date = "1990-09-30")
#' # two months later, day 15:
#' mftime2date(3, 15, origin_date = "1990-09-30")
mftime2date <- function(sp, ts, origin_date) {
  origin_date <- as.Date(origin_date)
  origin_date %m+%
    lubridate::days(1) %m+%                # move to day 1 of first month
    lubridate::months(sp - 1) %m+%         # add (sp-1) whole months
    lubridate::days(ts - 1)                # then advance (ts-1) days within that month
}

#-------------------------------------------------------------------------------------------------#

# File Management  -------------------------------------------------------------

#' Create Update Directory
#'
#' For 01_InputDataUpdate.R to create/check if update directory exists. File formula is
#' SVIHM_Input_Files/Updates/[current date]
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

#' Create Scenario Directory
#'
#' For 02_ScenarioBuilder to create/check if scenario directory exists.
#'
#' @return new or existing update directory
#' @export
#'
#' @examples
#' create_update_dir()
create_scenario_dir <- function(dir_name) {
  if (!dir.exists(dir_name)) {
    message(paste('Creating Directory:',dir_name))
    dir.create(dir_name, recursive = T)
  } else {
    message(paste('Directory:',dir_name, 'already exists.'))
    message('Warning: Files created by this script may overwrite those in the directory!')
  }
  return(dir_name)
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

#-------------------------------------------------------------------------------------------------#
#' Translate SWBM Code Columns into Text Labels (In-Place)
#'
#' Given a data.frame that may contain SWBM code columns, adds corresponding
#' "_txt" columns with human-readable labels, and inserts each new text column
#' immediately after its source code column.  The function handles these mappings:
#' \itemize{
#'   \item \code{SWBM_LU}      to \code{LU_txt}, using \code{landcover_desc$id} to \code{landcover_desc$Landcover_Name}
#'   \item \code{SWBM_IRR}     to \code{IRR_txt}, using internal \code{swbm_irr_key}
#'   \item \code{WATERSOURC}   to \code{WS_txt}, using internal \code{watersource_key}
#'   \item \code{subws_ID}     to \code{subws_txt}, using \code{tributary_desc$subws_ID} to \code{tributary_desc$subws_name}
#' }
#'
#' @param df A \code{data.frame} that may include SWBM code columns.
#' @param landcover_desc Optional \code{data.frame} with columns \code{id} and \code{Landcover_Name},
#'   required to translate \code{SWBM_LU}.
#' @param tributary_desc Optional \code{data.frame} with columns \code{subws_ID} and \code{subws_name},
#'   required to translate \code{subws_ID}.
#'
#' @return The same \code{df}, but with any of the following new columns added and placed
#'   immediately after their source columns:
#'   \describe{
#'     \item{\code{LU_txt}}{Character labels for \code{SWBM_LU}.}
#'     \item{\code{IRR_txt}}{Character labels for \code{SWBM_IRR}.}
#'     \item{\code{WS_txt}}{Character labels for \code{WATERSOURC}.}
#'     \item{\code{subws_txt}}{Character labels for \code{subws_ID}.}
#'   }
#'   Columns are only added if the corresponding code column is present in \code{df}
#'   and the needed lookup (\code{landcover_desc} or \code{tributary_desc}) is supplied.
#'
#' @examples
#' \dontrun{
#' data(swbm_irr_key); data(watersource_key)
#' landcov <- read.table("landcover_table.txt", header=TRUE)
#' trib    <- read.table("trib_desc.txt", header=TRUE)
#' poly    <- read.table("polygons.txt", header=TRUE)
#' poly2   <- translate_SWBM_codes(poly, landcov, trib)
#' head(poly2[c("SWBM_LU","LU_txt","SWBM_IRR","IRR_txt")])
#' }
#' @export
translate_SWBM_codes <- function(df,
                                 landcover_desc = NULL,
                                 tributary_desc = NULL) {
  # remember original column order
  orig_cols <- names(df)

  # SWBM_LU to LU_txt
  if ("SWBM_LU" %in% orig_cols) {
    if (is.null(landcover_desc)) {
      warning("SWBM_LU present but no landcover_desc provided; skipping LU_txt")
    } else {
      df$LU_txt <- landcover_desc$Landcover_Name[
        match(df$SWBM_LU, landcover_desc$id)
      ]
    }
  }

  # SWBM_IRR to IRR_txt
  if ("SWBM_IRR" %in% orig_cols) {
    df$IRR_txt <- swbm_irr_key$name[
      match(df$SWBM_IRR, swbm_irr_key$code)
    ]
  }

  # WATERSOURC to WS_txt
  if ("WATERSOURC" %in% orig_cols) {
    df$WS_txt <- watersource_key$name[
      match(df$WATERSOURC, watersource_key$code)
    ]
  }

  # subws_ID to subws_txt
  if ("subws_ID" %in% orig_cols) {
    if (is.null(tributary_desc)) {
      warning("subws_ID present but no tributary_desc provided; skipping subws_txt")
    } else {
      df$subws_txt <- tributary_desc$subws_name[
        match(df$subws_ID, tributary_desc$subws_ID)
      ]
    }
  }

  # build new column order: for each original, include it and its *_txt if it exists
  new_order <- c()
  mapping <- list(
    SWBM_LU    = "LU_txt",
    SWBM_IRR   = "IRR_txt",
    WATERSOURC = "WS_txt",
    subws_ID   = "subws_txt"
  )
  for (col in orig_cols) {
    new_order <- c(new_order, col)
    txt_col <- mapping[[col]]
    if (!is.null(txt_col) && txt_col %in% names(df)) {
      new_order <- c(new_order, txt_col)
    }
  }
  # include any other columns added after original set
  extra <- setdiff(names(df), new_order)
  new_order <- c(new_order, extra)

  # reorder and return
  df <- df[new_order]
  return(df)
}
