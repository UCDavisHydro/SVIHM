#
# SFR network file (read by SWBM) writing function is located in writeSWBMInputs.R
#-------------------------------------------------------------------------------------------------#

#' Generate Monthly SFR Inflow Partition
#'
#' Inflow to the model domain is often measured downstream of the domain boundary.
#' The upstream inflow may enter the domain on multiple forks/inflow points.
#' This partitioning assigns a fraction of the flow in each subwatershed to
#' different inflow points. Inflow points are assigned to subwatersheds in file
#' SFR_inflow_segments.txt.
#'
#' @param model_start_date Start date of model
#' @param model_end_date End date of model
#' @param unchanging_partition Are the inflow partitioning values unchanging over time? True or false
#'
#' @return
#' @author Claire Kouba, Leland Scantlebury
#' @export
#'
#' @examples
gen_sfr_flow_partition <- function(model_start_date,
                                     model_end_date,
                                     update_dir,
                                     monthly=T,
                                     streamflow_records_file = "streamflow_records_regressed.txt",
                                     unchanging_partition = F) {
  model_dates = seq(from = model_start_date, to = model_end_date, by = ifelse(monthly, "months","days"))

  if(unchanging_partition){
    # Hard-coded SFR partition. Uses average overall values.
    # Built from SVIHM (2018 version) streamflow records (regressed inputs)
    # and hard-coded SWBM (2018 version) choices (in the read_streamflow subroutine,
    # irrigation.f90 file) that divide measured/estimated tributary flow
    # among upstream inflow segments.

    sfr_unchang_part =  data.frame(inflow_seg_name = c('Scott River',
                                                       'Sugar Creek',
                                                       'Miners Creek',
                                                       'French Creek',
                                                       'Etna Creek',
                                                       'Johnson Creek',
                                                       'Crystal Creek',
                                                       'Patterson Creek',
                                                       'Kidder Creek',
                                                       'Moffett',
                                                       'Mill Creek',
                                                       'Shackleford Creek'),
                                   partition = c(0.875, 0.125, 0.5, 0.5, 1, 0.148,
                                                 0.111, 0.741, 1,   1,   1, 1))
                                   # partition = c(1, 1, 0.5, 0.5, 1, 1, # if we made each record a subwatershed
                                                 # 1, 1, 1,   1,   1, 1))
    # generate SFR_subws_flow_partitioning.txt
    sfr_part_tab = data.frame(modelDate = model_dates)
    for(i in 1:nrow(sfr_unchang_part)){
      # Make a new column named for the tributary.
      # Assign the uniform partition values for the full model period
      sfr_part_tab[,sfr_unchang_part$inflow_seg_name[i]] = sfr_unchang_part$partition[i]
    }
  } else { # calculate partitioning based on stream records
    stream_tab = read.table(file = file.path(update_dir,streamflow_records_file), header=T)
    inflow_segs = read.table(file = file.path( data_dir["time_indep_dir",'loc'], "SFR_inflow_segments.txt"),
                             comment.char = "!", header = T, row.names = NULL, sep = "\t")
    n_subws = length(unique(inflow_segs$subws_ID))# number of subwatersheds

    # associate stream record colname with each inflow segment
    stream_tab_colname = gsub(x = inflow_segs$stream_name, pattern = "Creek", replacement = "")
    stream_tab_colname = trimws(stream_tab_colname)
    stream_tab_colname = gsub(x = stream_tab_colname, pattern = " ", replacement = "_")
    inflow_segs$stream_tab_colname = paste0(stream_tab_colname, "_Avg_Flow_m3day")

    #Initialize monthly partition table
    sfr_part_tab = data.frame(modelDate = model_dates)
    sfr_part_matrix = matrix(data = NA, nrow = nrow(sfr_part_tab), ncol = ncol(stream_tab))
    sfr_part_tab = cbind(sfr_part_tab, sfr_part_matrix)
    colnames(sfr_part_tab)[2:ncol(sfr_part_tab)] = inflow_segs$stream_name

    for(i in 1:nrow(sfr_part_tab)){ # for each date
      for(j in 1:n_subws){ # for each subwatershed
        # identify inflow segment names
        tribs_in_subws = inflow_segs$stream_name[inflow_segs$subws_ID==j]

        if(inflow_segs$subws_name[inflow_segs$subws_ID==j][1]=="French"){ # For French Creek drainage
          # Assign 50% flow to each French Creek inflow segment
          flow_partition = 0.5
          sfr_part_tab[i,tribs_in_subws] = flow_partition
        } else {
          # isolate flow for month i in all inflow tribs in subwatershed j
          stream_tab_names = inflow_segs$stream_tab_colname[inflow_segs$subws_ID==j]
          flow_in_tribs = stream_tab[i,stream_tab_names]
          # calculate proportion of flow for each inflow segment
          total_flow = sum(flow_in_tribs)
          flow_partition = flow_in_tribs/total_flow
          # Assign total flow proportion to each inflow segment
          sfr_part_tab[i,tribs_in_subws] = flow_partition#[match(colnames(flow_in_tribs), stream_tab_names)]
        }

      }
    }


  }

  return(sfr_part_tab)
}

#-------------------------------------------------------------------------------------------------#

#' Process SFR Irrigation Inflows
#'
#' Generates table of monthly stream inflow volumes that are either available for irrigation
#' purposes or are unavailable for irrigation (i.e., flows reserved for environmental uses).
#'
#' @param model_start_date Start date of model
#' @param model_end_date End date of model
#' @param stream_inflow_filename Filename of the gap-filled tributary flow records for each
#' subwatershed (see \code{\link{write_tributary_input_file}}).
#' @param avail_for_irr Boolean. Writes the file "subwatershed_irrigation_inflows.txt" if TRUE
#' and the file "subwatershed_nonirrigation_inflows.txt" if FALSE.
#' @param instream_flow_regime Date frame of start- and end-dates and flow values
#' describing an annual instream flow regime. If NA, produces a 0-flow nonirrigation inflows
#' table or passes 100% of inflow to the irrigation inflows table.
#' @param instream_flow_units Handles "cfs", "cms" or "m3day". Defaults to NA.
#'
#' @return
#' @author Claire Kouba, Leland Scantlebury
#' @export
#'
#' @examples
process_sfr_inflows <- function(model_start_date,
                                    model_end_date,
                                    scenario_id,
                                    stream_inflow_filename,
                                    avail_for_irr = T,
                                    instream_flow_regime = NA) {

  # Read tributary streamflow inputs from file
  subws_inflow = read.table(stream_inflow_filename, header = T)
  #colnames(subws_inflow)[colnames(subws_inflow) == "Date"] = "modelDate"
  subws_inflow[,1] <- as.Date(subws_inflow[,1])

  if(avail_for_irr){ # If writing the subwatershed_irrigation_inflows.txt file
    # If no instream flow regime available, make all inflows available for irrigation
    if(is.na(instream_flow_regime)){
      sfr_inflow_tab = subws_inflow
    } else {
      # Process sfr_inflow_tab by subtracting reserved flows from sfr_inflows

    }

  } else if(avail_for_irr == FALSE) { # If writing the subwatershed_nonirrigation_inflows.txt file
    if(is.na(instream_flow_regime)){
      # If no instream flow regime available, reserve no inflows
      sfr_inflow_tab = subws_inflow
      flow_columns = grepl(pattern = "Flow", x = colnames(sfr_inflow_tab), ignore.case = T)
      # Set all monthly reserved flow volumes to 0
      sfr_inflow_tab[,flow_columns] = 0
    } else {
      # process the inflow regime to produce monthly flow volumes for each tributary

    }

  }

  return(sfr_inflow_tab)
}

#-------------------------------------------------------------------------------------------------#

#' Move inflows from one dataframe to another
#'
#' @param inflow_to
#' @param inflow_from
#' @param date_start inclusive
#' @param date_end inclusive
#' @param streams
#'
#' @return
#' @export
#'
#' @examples
move_inflows <- function(inflow_to, inflow_from, date_start, date_end, streams='ALL') {
  # Parse column selection
  if (streams == 'ALL') {
    streams <- colnames(inflow_to)[2:length(colnames(inflow_to))]
  }

  # Move inflows
  inflow_to[inflow_to[,1] >= as.Date(date_start) & inflow_to[,1] <= as.Date(date_end), streams] =
    inflow_from[inflow_from[,1] >= as.Date(date_start) & inflow_from[,1] <= as.Date(date_end), streams]

  return(inflow_to)

}

#-------------------------------------------------------------------------------------------------#

#' Set inflows between dates to specified value
#'
#' @param inflow_df
#' @param date_start inclusive
#' @param date_end inclusive
#' @param value
#' @param streams
#'
#' @return
#' @export
#'
#' @examples
set_inflows <- function(inflow_df, date_start, date_end, value, streams='ALL') {
  # Parse column selection
  if (streams == 'ALL') {
    streams <- colnames(inflow_df)[2:length(colnames(inflow_df))]
  }

  inflow_df[inflow_df[,1] >= as.Date(date_start) & inflow_df[,1] <= as.Date(date_end), streams] = value

  return(inflow_df)
}
