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

  # Combine (assumes exact same order, should always be true)
  combined <- cbind(hobs$data[c("obsnam","layer","row","column","irefsp","toffset","roff","coff")],
                    hsim[,c('sim','obs')])
  combined$well_id <- unlist(lapply(strsplit(combined$obsnam, ".", fixed=T), '[[', 1))
  # combined$well_id <- sub("_[^_]+$", "", combined$well_id)

  # Handle dry values
  combined$sim[combined$sim==hobs$hobdry] = NA

  # Calculate
  combined$residual <- combined$obs-combined$sim

  # Find in time
  combined$date <- as.Date(mftime2date(sp = combined$irefsp, ts = combined$toffset, origin_date = origin_date))

  # Done, return with rearranged columns

  return(combined[c("well_id","date","obsnam","layer","row","column","irefsp","toffset","roff",
                    "coff","sim","obs","residual")])
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
