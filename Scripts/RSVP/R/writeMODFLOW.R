#TODO Rework to be more generic

#-------------------------------------------------------------------------------------------------#

#' Write SVIHM MODFLOW Discretization File (DIS) with Updated Stress Periods
#'
#' Uses the reference SVIHM DIS file as a template and write a new version with the updated
#' temporal discretization at the specified directory.
#'
#' @param num_steps Number of time steps (i.e. days) in each stress period
#' @param num_stress_periods Number of stress periods
#' @param output_dir Directory to write DIS file in
#' @param filename character name of file to write (default: SVIHM.dis)
#' @param ref_data_dir Reference data directory containing DIS template file, SVIHM_dis_reference.txt
#' (default: SVIHM/SVIHM_Input_Files/reference_data/)
#' @param verbose T/F write status info to console (default: TRUE)
#'
#' @return
#' @author Claire Kouba
#' @export
#'
#' @examples
update_DIS_stress_periods <- function(num_steps,
                                      num_stress_periods,
                                      output_dir,
                                      filename = 'SVIHM.dis',
                                      ref_data_dir = data_dir['ref_data_dir','loc'],
                                      verbose = TRUE) {
  # 1) Update the number of stress periods in the header
  dis_text = readLines(con = file.path(ref_data_dir, "SVIHM_dis_reference.txt"), n = -1)

  #2) Update/extend the list of time steps in each stress period at end of file (dataset 7)
  start_of_dataset7 = which(dis_text == "  31  31  1  TR")[1] #Locate first line of dataset 7 (specifying Oct 1990)
  dataset_7 = paste0("  ", num_steps, "  ", num_steps, "  1  TR") #Create new dataset 7 using updated model period

  # 3) Put the .dis file text back together and write to file.
  dis_text_updated = c(dis_text[1],  #First line of .dis file
                       paste0(" 2  440  210  ", num_stress_periods,"  4  2"),
                       dis_text[4:start_of_dataset7-1], #really puzzled why a 4-index calls line 3 in this, but this works
                       dataset_7)
  if (verbose) {message(paste('Writing SVIHM DIS file: ', filename))}
  writeLines(dis_text_updated, con = file.path(output_dir, filename))
}

#-------------------------------------------------------------------------------------------------#

#' Write SVIHM MODFLOW Drain File (DRN) with Updated Number of Stress Periods
#'
#' In SVIHM, the drains represent saturation excess overland flow within the discharge zone. The
#' drain activation elevations are set at land surface.
#'
#' @param num_stress_periods Number of stress periods
#' @param output_dir Directory to write DRN file in
#' @param filename character name of file to write (default: SVIHM.drn)
#' @param default_drn_value Numeric value to be written for drain conductance
#' @param ref_data_dir Reference data directory containing DIS template file, SVIHM_dis_reference.txt
#' (default: SVIHM/SVIHM_Input_Files/reference_data/)
#' @param verbose T/F write status info to console (default: TRUE)
#'
#' @return
#' @author Claire Kouba
#' @export
#'
#' @examples
update_DRN_stress_periods <- function(num_stress_periods,
                                      output_dir,
                                      filename='SVIHM.drn',
                                      default_drn_value = 10000,
                                      ref_data_dir = data_dir['ref_data_dir','loc'],
                                      verbose = TRUE) {
  # Assemble output file location
  f <- file.path(output_dir, filename)

  #Assign an elevation, conductance and layer for every cell in the Discharge Zone
  DZ_Cells = read.table(file.path(ref_data_dir,"drn_ET_Cells_Discharge_Zone.txt"), header = T, sep = ",")
  Model_Surface = matrix(t(read.table(file.path(ref_data_dir,'drn_Layer_1_top_z.txt'))),
                         nrow = 440, ncol = 210, byrow = T)
  elevation = matrix(NaN,length(DZ_Cells$row))
  for (i in 1:length(DZ_Cells$row)){
    elevation[i] = Model_Surface[DZ_Cells$row[i],DZ_Cells$column[i]]
  }
  Conductance = matrix(default_drn_value,length(DZ_Cells$row))
  Layer = matrix(1,length(DZ_Cells$row))
  Drains = cbind(Layer, DZ_Cells$row, DZ_Cells$column, round(elevation,2), Conductance)
  rep_drains = matrix(-1, num_stress_periods-1)   # repeat value of -1 for n-1 Stress periods to reuse drains specified in first stress period

  #Write preamble
  if (verbose) {message(paste('Writing SVIHM DRN file: ', f))}
  write('# MODFLOW Drain Package File - Drains applied at land surface within discharge zone',
        file = f, append = F)
  write('PARAMETER    1  2869
          2869        50   AUX IFACE
  DZ   DRN    1  2869',
        file = f, append = T)

  #Define DZ - layer, row, column, elevation, and conductance for each cell with a drain in it
  cat(sprintf("%10i%10i%10i%10.2f%10.3e\n", Drains[,1], Drains[,2],Drains[,3],Drains[,4],Drains[,5]),
      file = f, append = T)

  #Specify DZ for each stress period
  for (i in 1:num_stress_periods){
    write(paste("     0          1          Stress Period", i), file = f, append = T)
    write("     DZ                         ", file = f, append = T)
  }
}

#-------------------------------------------------------------------------------------------------#

update_DRNO_stress_periods <- function(num_stress_periods,
                                       output_dir,
                                       filename='SVIHM.drno',
                                       default_drn_value = 10000.0,
                                       ref_data_dir = data_dir['ref_data_dir','loc'],
                                       verbose = TRUE) {
  #-- Read in DRNO reference - essentially the DRN package matched to nearby SFR reaches
  cells <- read.table(file.path(ref_data_dir,"drno_cells.txt"), header = T)

  #-- Correct conductance with value passed to function
  if (!is.na(default_drn_value)){
    cells$conductance <- default_drn_value
  }

  #-- Assemble output file location
  f <- file.path(output_dir, filename)

  #-- Write DRNO Header
  if (verbose) {message(paste('Writing SVIHM DRNO file: ', f))}
  write('# MODFLOW Drain Overland Flow (DRNO) Package - written by RSVP', file = f, append = F)
  write('          2869        50   NOPRINT', file = f, append = T)

  #Specify DZ for each stress period
  for (i in 1:num_stress_periods){
    if (i==1) {
      # Write SP line
      write(paste('     2869       0          Stress Period', i), file = f, append=T)
      #Define DZ - layer, row, column, elevation, and conductance for each cell with a drain in it
      cat(sprintf("%4i%6i%6i%10.2f%12.3e%6i%6i\n",
                  cells[,1],cells[,2],cells[,3],cells[,4],cells[,5],cells[,6],cells[,7]),
          file = f, append = T)
    } else {
      # Write SP repeat line
      write(paste('     -1         1          Stress Period', i), file = f, append=T)
    }
  }

}

#-------------------------------------------------------------------------------------------------#

#' Write SVIHM Head Observation (HOBS) File
#'
#' TODO: Accept data as arguments? Automate new data download?
#'
#' @param model_start_date Date of model start
#' @param model_end_date Date of model end
#' @param output_dir Directory to write HOBS file in
#' @param filename character name of file to write (default: SVIHM.hob)
#' @param ref_data_dir Reference data directory containing DIS template file, SVIHM_dis_reference.txt
#' (default: SVIHM/SVIHM_Input_Files/reference_data/)
#' @param verbose T/F write status info to console (default: TRUE)
#'
#' @return
#' @author Claire Kouba
#' @export
#'
#' @examples
write_SVIHM_head_obs_file <- function(model_start_date,
                                      model_end_date,
                                      output_dir,
                                      filename='SVIHM.hob',
                                      ref_data_dir = data_dir['ref_data_dir','loc'],
                                      verbose=TRUE) {
  ### 1) Get a water level dataframe
  wl = read.csv(file.path(ref_data_dir,"wl_observations_2022.01.14.csv"))
  stations = read.csv(file.path(ref_data_dir, "wells_2022.01.14.csv"))

  #merge SWN (long well names) onto wl obs table.
  stations_swn = stations[,c('well_code', 'swn')]
  wl$swn = NA
  wl$swn = stations_swn$swn[match(wl$well_code, stations_swn$well_code)]

  # Clean for SVIHM
  #### 1a) Make A4_1 match the name in the water level file (A41). (This avoids confusion on unique WL observation IDs in modflow.)
  wl$well_code = as.character(wl$well_code)
  wl$well_code[wl$well_code == "A41"] = "A4_1"

  #### 1b) Convert longer DWR well names (in the WL file) to the abbreviations in SVIHM hob_info table
  mon_info = read.csv( file.path(ref_data_dir, "Monitoring_Wells_Names.csv"))
  dwr_in_model_short_names = c("DWR_1","DWR_2","DWR_3","DWR_4","DWR_5")
  dwr_in_model_long_names = as.character(mon_info$Well_ID_2[mon_info$Well_ID %in% dwr_in_model_short_names])
  # Select based on State Well Number (SWN), which is listed as the local well number in the VMP data.
  # CASGEM "local well number" is sometimes the SWN but in the cases of DWR_1 and DWR_3 is an unrelated abbreviation.
  replaceables = unique(wl$swn[wl$swn %in% dwr_in_model_long_names])
  replaceables_selector = wl$swn %in% replaceables
  #Replace local well numbers of replaceables with the abbreviations (DWR_1 etc) of replaceable SWNs
  wl$well_code[replaceables_selector] =
    as.character(mon_info$Well_ID[match(wl$swn[replaceables_selector],mon_info$Well_ID_2)])

  ### 2) Read in  .hob info file (well location information)
  hob_info = read.table(file.path(ref_data_dir,"hob_wells.txt"), header = F, skip = 4)
  colnames(hob_info) = c('OBSNAM', 'LAYER', 'ROW', 'COLUMN', 'IREFSP', 'TOFFSET', 'ROFF', 'COFF', 'HOBS', 'STATISTIC', 'STAT-FLAG', 'PLOT-SYMBOL')


  ### 3) Retain just the water level obs (no NAs) from Scott Valley in the model period
  ### TEMPORARY: retain just the ones that have hob_info.
  ### TO DO: Join additional wells to the model grid and add their well loc. info to reference hob_info table.
  # wl = wl[wl$basin == "Scott River Valley" & !is.na(wl$wse_ft) & wl$well_code %in% hob_info$OBSNAM &
  #           wl$date >= model_start_date & wl$date <= model_end_date,]
  wl = wl[!is.na(wl$wse_ft) & wl$well_code %in% hob_info$OBSNAM &
            wl$date >= model_start_date & wl$date <= model_end_date,]

  ### 4) Update total number of well observations and write preamble for .hob file.
  num_wl_obs = dim(wl)[1] # Calculate number of observations for preamble
  preamble = c('# MODFLOW2000 Head Observation File','# Groundwater Vistas also writes drawdown targets here',
               paste0('  ',num_wl_obs,'  0  0 500 -999'), '  1.0  1.0')

  f <- file.path(output_dir, filename)

  if (verbose) {message(paste('Writing SVIHM HOBS file: ', filename))}
  write(preamble, file = f, append = F)


  ### 5) For each observation point, write a) topline of well info and b) details for each observation
  for(i in 1:length(hob_info$OBSNAM)){
    #### 5a) Info for each well location
    ##### 5a1) Calculate number of observations for an individual well loc ("IREFSP" neg values in Dataset 3)
    obs_loc = hob_info$OBSNAM[i]
    IREFSP = sum(wl$well_code == obs_loc, na.rm=T) #calculate number of measurements for this well
    if(IREFSP < 1){next} #skip it if there's no observations for this well name. It's currently happening for A4_1 and the DWR wells.

    #### 5a2) Update IREFSP and write topline (info for each well location)
    topline = paste(as.character(hob_info[i,1]), hob_info[i,2], hob_info[i,3], hob_info[i,4],
                    -1*IREFSP, format(hob_info[i,6],nsmall = 1), format(hob_info[i,7],nsmall = 6),
                    format(hob_info[i,8],nsmall = 6), format(hob_info[i,9],nsmall = 6),
                    format(hob_info[i,10],nsmall = 6), hob_info[i,11], hob_info[i,12], sep = "  ")
    write(topline, file = f, append = T)

    #### 5a3) Write "1" to signify hydraulic heads = obs in Data Set 5
    write("  1", file = f, append = T)

    #### 5b) write up the observations for each well
    ##### 5b1) Create unique observation ids
    wl_subset = wl[wl$well_code == obs_loc & !is.na(wl$well_code),]
    obs_id_num = 1:IREFSP
    obs_id = paste0(obs_loc, obs_id_num)
    ##### 5b2) convert sample date to stress period and time offset
    dates = wl_subset$date
    samp_years = lubridate::year(dates)
    samp_months = lubridate::month(dates)
    stress_periods = (samp_years - lubridate::year(model_start_date))*12 + samp_months - (lubridate::month(model_start_date)-1)
    offset_days = lubridate::day(dates)
    ##### 5b3) Convert wse_ft from feet to meters
    meters_asl = 0.3048 * wl_subset$wse_ft #0.3048006096012 * wl_subset$wse_ft
    #### 5b4) Write the vectors into the file. Attach a bunch of 1s as Modflow flags
    cat(sprintf("%12s%12i%12.6f%12.6f%12.6f%12.6f%8i%8i\n",
                obs_id, stress_periods, offset_days, meters_asl,
                rep(1, IREFSP), rep(1, IREFSP), rep(1, IREFSP), rep(1, IREFSP)), file = f, append = T)
  }
}

#-------------------------------------------------------------------------------------------------#

#' Write SVIHM MODFLOW Output Control File (OC) with Updated Number of Stress Periods
#'
#' @param num_steps Number of time steps (i.e. days) in each stress period
#' @param num_stress_periods Number of stress periods
#' @param output_dir Directory to write OC file in
#' @param filename character name of file to write (default: SVIHM.hob)
#' @param monthly T/F Write print/save statements only at month ends (as opposed to at every time step, F)
#' @param verbose T/F write status info to console (default: TRUE)
#'
#' @return
#' @author Claire Kouba
#' @export
#'
#' @examples
update_OC_stress_periods <- function(num_steps,
                                     num_stress_periods,
                                     output_dir,
                                     filename='SVIHM.oc',
                                     monthly=T,
                                     verbose=TRUE) {

  f <- file.path(output_dir, filename)

  if (verbose) {message(paste('Writing SVIHM OC file: ', filename))}

  preamble = c("# Output Control (OC) package input generated by RSVP",
               "  HEAD PRINT FORMAT 0","  HEAD SAVE UNIT 30",
               "  DRAWDOWN PRINT FORMAT 0","  DRAWDOWN SAVE UNIT 31",
               "  COMPACT BUDGET AUX")
  write(preamble, file = f, append = F)

  for(i in 1:num_stress_periods){
    for (j in 1:as.numeric(num_steps[i])) {

      write(paste0('Period ', i, ' Step ', j), file = f, append = T)
      # Write out print statements if monthly output is NOT on (daily instead)
      # OR if monthly is on and it's the final day in the period (month)
      if ((!monthly) | (monthly & j == as.numeric(num_steps[i]))) {
        stress_end_block = c("     Save Head", "     Save Drawdown", "     Save Budget", "     Print Budget")
        write(stress_end_block, file = f, append = T)
      }
    }
  }
}

