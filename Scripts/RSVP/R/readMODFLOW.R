# Relies on experimental RMODFLOW Package:
# see: https://github.com/rogiersbart/RMODFLOW

#-------------------------------------------------------------------------------------------------#

#' Import MODFLOW Head Observation (HOB) Files
#'
#' @param hob_input file path to MODFLOW HOB file
#' @param hob_output file path to MODFLOW HOB output
#' @param origin_date date from which to calculate calendar date from HOB file SP/TS offsets
#' (Currently assumes stress periods are months, time steps days)
#'
#' @return dataframe of obs/sim GW levels
#' @author Leland Scantlebury
#' @import lubridate
#' @export
#'
#' @examples
import_HOB <- function(hob_input, hob_output, origin_date) {

  # Read MODFLOW input file for loc/stress period
  hobs <- RMODFLOW::rmf_read_hob(hob_input)

  # Read MODFLOW output file
  hsim = read.table(hob_output,
                     skip = 1,
                     header = F,
                     col.names = c('sim', 'obs', 'obsnam'))

  # Error check
  if (nrow(hsim) != hobs$nh) {
    stop('MODFLOW HOB Input/Ouput data length mismatch.')
  }

  # Merge
  hobs <- merge(hobs$data, hsim[,c('obsnam','sim')], by = 'obsnam', sort=F)

  # Calculate
  hobs$residual <- hobs$hob-hobs$sim

  # Find in time
  hobs$date <- origin_date %m+% (months(hobs$irefsp)-1) %m+% days(hobs$toffset)  #TODO verify

  # Group by row/col to get well name #TODO ensure no duplicates
  # And yes, the regex took me a lot of googling
  hobs <- split(hobs, paste(hobs$row, hobs$column, sep = '_'))
  hobs <- lapply(hobs, function(x) { x$wellnam <- sub("1([^1]*)$", "", x$obsnam[1]); return(x) })
  hobs <- do.call(rbind, hobs)
  row.names(hobs) <- 1:nrow(hobs)

  # Done, return with rearranged columns
  return(hobs[,c('wellnam',names(hobs)[1:11], 'date', 'hobs', 'sim', 'residual')])
}

#-------------------------------------------------------------------------------------------------#

#' Import Streamflow Routing Package Gage Output File
#'
#' @param gag_out file path to gage file
#' @param origin_date date from which to calculate calendar date from SFR time
#'
#' @return dataframe of gage data
#' @author Leland Scantlebury
#' @export
#'
#' @examples
import_sfr_gauge <- function(gag_out, origin_date) {

  # Read first line to get gauge info
  con <- file(gag_out, 'r')
  gag_info <- stringr::str_trim(stringr::str_remove_all(readLines(con, n=1),'\"'))
  close(con)

  # Read table in file
  cols <- c('Time', 'Stage', 'Flow_m3_day', 'Depth', 'Width', 'Midpt_Flow', 'Precip', 'ET',
            'Runoff', 'Conductance', 'HeadDiff', 'Hyd_Grad')
  gag <- read.table(file=gag_out, header = F, skip = 2, col.names = cols)

  # Find in time
  gag$Date <- origin_date + gag$Time

  # Convert to CFS
  gag$Flow_cfs <- gag$Flow_m3_day * 0.000408734569

  # Add attributes
  attr(gag, 'info') <- gag_info

  return(gag)
}
