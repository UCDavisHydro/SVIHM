#
# SFR network file (read by SWBM) writing function is located in writeSWBMInputs.R
#-------------------------------------------------------------------------------------------------#

#' Process SFR Irrigation Inflows for a Scenario
#'
#' Reads a gap-filled tributary inflow file, subsets it to the model period defined in `scen`,
#' and returns both the original daily inflows (available for irrigation) and a version
#' with all "reserved" flows zeroed out (unavailable for irrigation).
#'
#' @param scen Named list with at least:
#'   \itemize{
#'     \item `start_date` - `Date`; first day of model period.
#'     \item `end_date`   - `Date`; last day of model period.
#'   }
#' @param stream_inflow_filename `character(1)` path to the gap-filled tributary
#'   inflow file (first column must be a Date).
#'
#' @return A `list` with two data frames:
#' \describe{
#'   \item{`irr`}{Original daily inflow volumes within the model period.}
#'   \item{`non_irr`}{Same dates, but all flow columns set to zero.}
#' }
#'
#' @seealso \code{\link{subset.DateTwoSided}}
#'
#' @export
#' @examples
#' \dontrun{
#' scen <- list(
#'   start_date = as.Date("1991-01-01"),
#'   end_date   = as.Date("2020-12-31")
#' )
#' file <- file.path("data", "subwatershed_inflows.txt")
#' inflows <- process_sfr_inflows(scen, file)
#' head(inflows$irr)
#' head(inflows$non_irr)
#' }
process_sfr_inflows <- function(scen, stream_inflow_filename) {
  # 1) read and parse date
  df <- read.table(stream_inflow_filename, header = TRUE)
  date_col <- names(df)[1]
  df[[date_col]] <- as.Date(df[[date_col]])

  # 2) subset to [start, end] inclusive
  df <- subset.DateTwoSided(
    df,
    start      = scen$start_date,
    end        = scen$end_date,
    date_col   = date_col,
    include_end = TRUE
  )

  # 3) build the two outputs
  available <- df

  reserved <- df
  flow_cols <- grepl("flow", names(reserved), ignore.case = TRUE)
  reserved[, flow_cols] <- 0

  # 4) return both in a list
  list(irr = available, non_irr  = reserved
  )
}

#-------------------------------------------------------------------------------------------------#
#' Apply a Daily Streamflow Curtailment to Irrigation Inflows
#'
#' Moves a fixed percentage of each day's flow from the `irr` (available) table to the
#' `non_irr` (reserved) table for a specified inclusive date range.
#'
#' @param flows A list with two data frames:
#'   \describe{
#'     \item{`irr`}{Data frame of irrigation-available inflows; first column must be Date.}
#'     \item{`non_irr`}{Data frame of irrigation-reserved inflows; same structure as `irr`.}
#'   }
#' @param percent Numeric in \[0,1\]; the same fraction of **each day's** flow to move.
#' @param date_start Date or character; first day (inclusive) of curtailment window.
#' @param date_end   Date or character; last day (inclusive) of curtailment window.
#' @param streams Character vector of flow-column names to affect, or `"ALL"` (default)
#'   to use every column except the first (date) column.
#'
#' @return The same `flows` list, but with:
#' \itemize{
#'   \item `flows$irr`  reduced by `percent * original` on each day in the window;
#'   \item `flows$non_irr` increased by that same amount.
#' }
#'
#' @export
#' @examples
#' subws_inflows <- process_sfr_inflows(scen, file)
#' subws_inflows <- streamflow_curtailment(subws_inflows,
#'                                         percent= 1,
#'                                         date_start = "2021-09-10",
#'                                         date_end = "2021-10-25")
streamflow_curtailment <- function(flows,
                                   percent,
                                   date_start,
                                   date_end,
                                   streams = "ALL") {
  # coerce dates
  d1 <- as.Date(date_start)
  d2 <- as.Date(date_end)

  # extract and ensure Date column
  irr     <- flows$irr
  non_irr <- flows$non_irr
  date_col <- names(irr)[1]
  irr[[date_col]]     <- as.Date(irr[[date_col]])
  non_irr[[date_col]] <- as.Date(non_irr[[date_col]])

  # select streams
  if (identical(streams, "ALL")) {
    streams <- names(irr)[-1]
  }

  # find rows in range
  idx <- which(irr[[date_col]] >= d1 & irr[[date_col]] <= d2)
  if (length(idx) == 0) return(flows)  # nothing to do

  # apply constant curtailment
  for (col in streams) {
    orig <- irr[idx, col]
    redn <- orig * percent
    irr[idx, col]     <- orig - redn
    non_irr[idx, col] <- non_irr[idx, col] + redn
  }

  # return updated list
  flows$irr     <- irr
  flows$non_irr <- non_irr
  flows
}

#-------------------------------------------------------------------------------------------------#
#' Write SWBM SFR Inflow Segments file
#'
#' Writes file to establish connection between tributaries, subwatersheds, and MODFLOW SFR package
#'
#' @param output_dir directory to write the files to
#' @param filename input filename, SFR_inflow_segments.txt by default
#' @param only_mf_tribs Only include the tributaries explicitly modeled in the MF file (no PRMS additions)
#' @param verbose Logical. Whether to print status information to the console. Default is TRUE.
#'
#' @returns None. The function writes the input file to the specified directory.
#' @export
#'
#' @examples
#'
#' write_SWBM_SFR_segment_file(output_dir=out_dir)
#'
write_SWBM_SFR_segment_file <- function(output_dir, filename='SFR_inflow_segments.txt', only_mf_tribs=T, verbose=T) {
  tribs <- stream_metadata
  if (only_mf_tribs) {
    prms_stream_metadata <- prms_stream_metadata[prms_stream_metadata$in_modflow,]
  }
  if (verbose) {message(paste('Writing SWBM SFR Segment file: ', filename))}
  write.table(tribs[,c('subws','MF_seg','subws_name','name')],
              file.path(output_dir,filename),
              quote = F,
              row.names = F,
              col.names = c('subws_ID', 'sfr_seg', 'subws_name','trib_name'))
}
