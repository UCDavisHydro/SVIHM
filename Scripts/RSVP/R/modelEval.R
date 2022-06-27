#-------------------------------------------------------------------------------------------------#

#' Calculate Split Sample Statistics
#'
#' @param dates Array of dates related to sim/obs
#' @param sim Array of simulated values
#' @param obs Array of observed values
#' @param split_date Date to split values on, based on dates array
#' @param FUNs Function(s) to calculate statistics using. Must accept arguments as (sim, obs)
#' @param FUN_names Names of functions in FUNs, used for DF column names
#'
#' @return Dataframe of statistics
#' @seealso \code{\link{calc_split_sample_stats.grouped}},
#' \code{\link{print.split_sample_stats}},
#'
#' @author Leland Scantlebury
#' @export
#'
#' @examples
#' \dontrun{
#' # Read in head observation data
#' #-- HOB data
#' hob_locs <- read.csv('hob_wells.csv', row.names=1, stringsAsFactors = F)
#' hob <- import_HOB(hob_input = 'SVIHM.hob',
#'                   hob_output = 'HobData_SVIHM.dat',
#'                   origin_date = origin_date)
#' hob <- hob[order(hob$row, hob$column),]
#'
#' # Date to split data (i.e. pre-post calibration)
#' cal_split <- as.Date('2012-10-01')
#'
#' hob_compare <- calc_split_sample_stats(dates = hob$date,
#'                                        sim = hob$sim,
#'                                        obs = hob$hobs,
#'                                        split_date = cal_split,
#'                                        FUNs = list(NSE, rmse, KGE),
#'                                        FUN_names = c('NSE', 'RMSE', 'KGE'))
#' }
calc_split_sample_stats <- function(dates, sim, obs, split_date, FUNs, FUN_names) {
  if (length(dates) != length(sim) | length(sim) != length(obs)) {
    stop('Arrays for dates, sim, and obs all must be the same length.')
  }
  if (!typeof(FUNs)=='list') { FUNs <- list(FUNs)}
  i <- 1
  res <- lapply(FUNs, function(f) {
    fname <- FUN_names[i]
    all <- f(sim, obs)
    pre_stat <- f(sim[dates < split_date], obs[dates < split_date])
    pst_stat <- f(sim[dates >= split_date], obs[dates >= split_date])
    i <<- i + 1
    return(data.frame('Statistic' = fname, 'All' = all, 'Pre' = pre_stat, 'Post' = pst_stat))
  })
  res <- do.call(rbind, res)
  return(res)
}

#-------------------------------------------------------------------------------------------------#

#' Calculate Grouped Split Sampled Statistics
#'
#' @param dates Array of dates related to sim/obs
#' @param sim Array of simulated values
#' @param obs Array of observed values
#' @param groups Array of groups that sim, obs values belong to (same length as sim & obs)
#' @param split_date Date to split values on, based on dates array
#' @param FUNs Function(s) to calculate statistics using. Must accept arguments as (sim, obs)
#' @param FUN_names Names of functions in FUNs, used for DF column names
#'
#' @return Dataframe of statistics
#' @seealso \code{\link{calc_split_sample_stats}},
#' \code{\link{print.split_sample_stats}},
#' \code{\link{ts_obs_sim_combine}}
#'
#' @author Leland Scantlebury
#' @export
#'
#' @examples
#' \dontrun{
#' # Incomplete example
#' stream_combined <- ts_obs_sim_combine(streams, streams_sim, stream_names, val_cols = c('Flow','Flow_cfs'))
#' sfrcompare <- calc_split_sample_stats.grouped(stream_combined$Date, stream_combined$sim,
#'                                               stream_combined$obs, stream_combined$group,
#'                                               cal_split,
#'                                               FUNs = list(NSE, rmse, KGE),
#'                                               FUN_names = c('NSE', 'RMSE', 'KGE'))
#' }
calc_split_sample_stats.grouped <- function(dates, sim, obs, groups, split_date, FUNs, FUN_names) {
  res <- list()
  for (grp in unique(groups)) {
    grpd <- dates[groups == grp]
    grps <- sim[groups == grp]
    grpo <- obs[groups == grp]
    grpr <- calc_split_sample_stats(grpd, grps, grpo, split_date, FUNs, FUN_names)
    grpr$Group <- grp
    res <- append(res, list(grpr))
  }
  res <- do.call(rbind, res)
  return(res)
}

#-------------------------------------------------------------------------------------------------#

#' Print Split Sample Statistics as Formatted Line
#'
#' @param sampledf DF of statistics computed by \code{\link{calc_split_sample_stats}} or
#' \code{\link{calc_split_sample_stats.grouped}}
#' @param group Group name (optional, default NA)
#' @param side 'Pre' or 'Post'
#'
#' @return String of statistical results
#' @seealso \code{\link{calc_split_sample_stats}}, \code{\link{calc_split_sample_stats.grouped}}
#' @author Leland Scantlebury
#' @export
#'
#' @examples
#' \dontrun{
#' # Read in head observation data
#' #-- HOB data
#' hob_locs <- read.csv('hob_wells.csv', row.names=1, stringsAsFactors = F)
#' hob <- import_HOB(hob_input = 'SVIHM.hob',
#'                   hob_output = 'HobData_SVIHM.dat',
#'                   origin_date = origin_date)
#' hob <- hob[order(hob$row, hob$column),]
#'
#' # Date to split data (i.e. pre-post calibration)
#' cal_split <- as.Date('2012-10-01')
#'
#' hob_compare <- calc_split_sample_stats(dates = hob$date,
#'                                        sim = hob$sim,
#'                                        obs = hob$hobs,
#'                                        split_date = cal_split,
#'                                        FUNs = list(NSE, rmse, KGE),
#'                                        FUN_names = c('NSE', 'RMSE', 'KGE'))
#' # Finally ready
#' statstr <- print.split_sample_stats(hob_compare, 'Post')
#' print(statstr)
#' }
print.split_sample_stats <- function(sampledf, group=NA, side) {
  # Start String, handle groups
  if (is.na(group)){
    strout <- ''
    df <- sampledf
  } else {
    strout <- paste(group,'-')
    grpdf <- sampledf[sampledf$Group == group,]
  }
  # Add stats
  for (stat in df$Statistic) {
    strout <- paste0(strout, '  ', stat, ' = ', round(df[df$Statistic == stat, side],3))
  }
  return(strout)
}

#-------------------------------------------------------------------------------------------------#
