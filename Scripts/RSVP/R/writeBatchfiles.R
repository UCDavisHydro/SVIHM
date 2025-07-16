

#-------------------------------------------------------------------------------------------------#
#' Write Scenario Preparation Batch File
#'
#' Generates a Windows batch script to assemble all necessary input files
#' for a given SVIHM scenario into the \code{Run} folder structure.
#'
#' @param scenario_name Character. Name of the scenario (default \code{"basecase"}).
#'   Used to name the batch file and set the \code{scen} environment variable.
#' @param output_dir Character. Directory where the batch file will be written.
#'   Defaults to \code{data_dir["svihm_dir","loc"]}.
#' @param filename Character or \code{NULL}. Name of the batch file. If \code{NULL},
#'   it defaults to \code{Prepare_<scenario_name>_Run.bat}.
#' @param tabfiles Logical; if \code{TRUE}, copies the \code{SVIHM_tabfiles.nam}
#'   template into the MODFLOW run folder (overwriting \code{SVIHM.nam}).
#' @param true_base Logical; if \code{TRUE}, includes additional "true basecase"
#'   files (polygons, landcover, ET zones, SFR network) in the copy list.
#' @param write_end Logical; if \code{TRUE}, appends echo/pause messages at the end
#'   of the script to report success.
#'
#' @details
#' The generated batch file:
#' \itemize{
#'   \item Sets a local \code{scen} variable to \code{scenario_name}.
#'   \item Creates \code{Run}, \code{Run/SWBM}, and
#'     \code{Run/MODFLOW} directories if they don't already exist.
#'   \item Copies time-independent SWBM and MODFLOW input files from
#'     \code{SVIHM_Input_Files/time_independent_input_files}.
#'   \item Optionally includes extra basecase files or tabfile templates.
#'   \item Optionally writes a final status message and pauses.
#' }
#'
#' @return Invisibly returns \code{NULL}. The side effect is the creation of the
#'   batch file at \code{file.path(output_dir, filename)}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Write a default basecase preparation script:
#' write_scenario_prep_batchfile()
#'
#' # Write a scenario script without tabfiles and no final pause:
#' write_scenario_prep_batchfile(
#'   scenario_name = "test_case",
#'   output_dir    = "C:/SVIHM/RunScripts",
#'   tabfiles      = FALSE,
#'   write_end     = FALSE
#' )
#' }
write_scenario_prep_batchfile <- function(scenario_name='basecase',
                                          output_dir=data_dir['svihm_dir','loc'],
                                          filename=NULL,
                                          tabfiles=TRUE,
                                          true_base=FALSE,
                                          write_end=TRUE) {
  if (is.null(filename)) {
    filename=paste0('Prepare_',scenario_name,'_Run.bat')
  }

  f <- file.path(output_dir, filename)
  write('@setlocal', file = f, append=F)
  write('@echo off', file = f, append=T)
  write(paste0('set scen=',scenario_name), file = f, append=T)
  write('', file = f, append=T)
  write(':: Intended to be run in the main SVIHM folder', file = f, append=T)
  write('', file = f, append=T)
  write(':: Create Run folders (if does not exist)', file = f, append=T)
  write('if not exist "Run" mkdir Run', file = f, append=T)
  write('if not exist "Run\\SWBM" mkdir Run\\SWBM', file = f, append=T)
  write('if not exist "Run\\MODFLOW" mkdir Run\\MODFLOW', file = f, append=T)
  write('', file = f, append=T)
  write(':: Copy scenario independant MODFLOW model files', file = f, append=T)
  write('xcopy SVIHM_Input_Files\\time_independent_input_files\\ET_Cells_Extinction_Depth.txt Run\\SWBM /Y /I', file = f, append=T)
  write('xcopy SVIHM_Input_Files\\time_independent_input_files\\recharge_zones.txt Run\\SWBM /Y /I', file = f, append=T)
  write('xcopy SVIHM_Input_Files\\time_independent_input_files\\SFR_routing.txt Run\\SWBM /Y /I', file = f, append=T)
  write('xcopy SVIHM_Input_Files\\time_independent_input_files\\SFR_inflow_segments.txt Run\\SWBM /Y /I', file = f, append=T)
  write('xcopy SVIHM_Input_Files\\time_independent_input_files\\ag_well_summary.txt Run\\SWBM /Y /I', file = f, append=T)
  write('xcopy SVIHM_Input_Files\\time_independent_input_files\\ag_well_list_by_polygon.txt Run\\SWBM /Y /I', file = f, append=T)
  write('xcopy SVIHM_Input_Files\\time_independent_input_files\\precip_factors.txt Run\\SWBM /Y /I', file = f, append=T)
  write('xcopy SVIHM_Input_Files\\time_independent_input_files\\irr_ditch.txt Run\\SWBM /Y /I', file = f, append=T)
  # MFR Files
  write('xcopy SVIHM_Input_Files\\time_independent_input_files\\modflow_cell_to_catchment.txt Run\\SWBM /Y /I', file = f, append=T)
  # MODFLOW files
  write('xcopy SVIHM_Input_Files\\time_independent_input_files\\SVIHM.* Run\\MODFLOW /Y /I', file = f, append=T)
  if (true_base) {
    # For writing the "real" basecase batchfile file - but all these should be written by RSVP under normal circumstances
    write('xcopy SVIHM_Input_Files\\time_independent_input_files\\polygons_table.txt Run\\SWBM /Y /I', file = f, append=T)
    write('xcopy SVIHM_Input_Files\\time_independent_input_files\\landcover_table.txt Run\\SWBM /Y /I', file = f, append=T)
    write('xcopy SVIHM_Input_Files\\time_independent_input_files\\ET_Zone_Cells.txt Run\\SWBM /Y /I', file = f, append=T)
    write('xcopy SVIHM_Input_Files\\time_independent_input_files\\SFR_network.txt Run\\SWBM /Y /I', file = f, append=T)
  }
  if (tabfiles) {
    # Overwrite NAM file
    write('xcopy SVIHM_Input_Files\\time_independent_input_files\\SVIHM_tabfiles.nam Run\\MODFLOW\\SVIHM.nam /Y /I', file = f, append=T)
  }
  write('xcopy SVIHM_Input_Files\\time_independent_input_files\\Starting_Heads_L*.txt Run\\MODFLOW /Y /I', file = f, append=T)
  write('xcopy SVIHM_Input_Files\\time_independent_input_files\\SVIHM_*_template.txt Run\\SWBM /Y /I', file = f, append=T)
# TODO create plan/infrustructure/folders for handling scenarios
#  write('', file = f, append=T)
#  write(':: Copy files from scenario folder to run folder', file = f, append=T)
# write('xcopy Scenarios\\%scen%\\*.txt Run\\SWBM /Y /I', file = f, append=T)
#  write('xcopy Scenarios\\%scen%\\*.zone Run\\SWBM /Y /I', file = f, append=T)
#  write('xcopy Scenarios\\%scen%\\SVIHM.* Run\\MODFLOW /Y /I ', file = f, append=T)
  write('', file = f, append=T)
  write(':: Copy in generic run batch file', file = f, append=T)
  write('xcopy Scripts\\Batch_Scripts\\Run_SVIHM.bat Run /Y /I', file = f, append=T)
  write('', file = f, append=T)
#  write(':: Copy in Update_Drain_Inflows input file', file = f, append=T)
#  write('xcopy SVIHM_Input_Files\\Update_Drain_Inflows.in Run\\MODFLOW /Y /I', file = f, append=T)
  write('', file = f, append=T)
  write(':: Copy SFR Template File', file = f, append=T)
  write('xcopy Calib_Sens_Files\\UCODE\\UCODE_Template_Files\\SFR_network_jtf.txt Run\\SWBM /Y /I', file = f, append=T)
  write('', file = f, append=T)
  if (write_end) {
    write(':: Report', file = f, append=T)
    write('echo.', file = f, append=T)
    write('echo Run folder populated with %scen%. Model can be run using Run\\Run_SVIHM.bat', file = f, append=T)
    write('pause', file = f, append=T)
  }
}

#-------------------------------------------------------------------------------------------------#
#' Write Scenario SVIHM Batch File
#'
#' Builds on \code{write_scenario_prep_batchfile()} to create a batch script
#' that also copies updated SWBM and MODFLOW files from an \code{scen_dir}.
#'
#' @param update_dir Character. Path to the folder containing updated SWBM/MODFLOW
#'   input files (e.g. a versioned subfolder of \code{Scenarios/}).
#' @param output_dir Character. Directory where the batch file will be written.
#'   Defaults to \code{data_dir["svihm_dir","loc"]}.
#' @param tabfiles Logical; if \code{TRUE}, passes \code{tabfiles = TRUE} to
#'   \code{write_scenario_prep_batchfile()} to include the tabfile template copy.
#' @param scenario_name Character. Name of the scenario to set in the base script
#'   (default \code{"basecase"}).
#'
#' @details
#' This function:
#' \enumerate{
#'   \item Calls \code{write_scenario_prep_batchfile()} with the provided
#'     \code{output_dir}, \code{tabfiles}, and \code{scenario_name}, suppressing
#'     the final echo/pause.
#'   \item Computes a relative path from \code{update_dir} to the project root.
#'   \item Appends \code{xcopy} commands to copy the contents of
#'     \code{update_dir} into \code{Run/SWBM} and
#'     \code{Run/MODFLOW}, renaming/deleting as needed.
#'   \item Adds final echo/pause messages.
#' }
#'
#' @return Invisibly returns \code{NULL}. The side effect is the creation of the
#'   batch file at \code{file.path(output_dir, "Prepare_Update_<name>_Run.bat")}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Suppose you have an update folder at "../Scenarios/v2.1":
#' write_scen_prep_batchfile(
#'   update_dir    = "../Scenarios/v2.1",
#'   output_dir    = "C:/SVIHM/RunScripts",
#'   tabfiles      = TRUE,
#'   scenario_name = "calibration_v2"
#' )
#' }
write_scen_prep_batchfile <- function(scen_dir,
                                        output_dir=data_dir['svihm_dir','loc'],
                                        tabfiles=TRUE,
                                        scenario_name = "basecase") {

  # Get new filename
  update_name <- basename(scen_dir)
  filename <- paste0('Prepare_',update_name,'_Run.bat')
  f <- file.path(output_dir, filename)

  # Get relative location
  relpath <- gsub('../','',scen_dir, fixed = T)
  relpath <- gsub('/', '\\', relpath, fixed=T)

  # Write normal batchfile
  write_scenario_prep_batchfile(output_dir=output_dir,
                                filename = filename,
                                scenario_name = scenario_name,
                                tabfiles = tabfiles,
                                write_end = F)

  # New lines
  write(':: Copy Updated Files to run folders', file = f, append=T)
  write(paste0('xcopy ',relpath,'\\svihm.swbm Run\\SWBM /Y /I'), file = f, append=T)
  write(paste0('xcopy ',relpath,'\\*.txt Run\\SWBM /Y /I'), file = f, append=T)
  write(paste0('xcopy ',relpath,'\\SVIHM.* Run\\MODFLOW /Y /I && del Run\\MODFLOW\\svihm.swbm'), file = f, append=T)

  # Write end
  write('', file = f, append=T)
  write(':: Report', file = f, append=T)
  write('echo.', file = f, append=T)
  write('echo Run folder populated with %scen%. Model can be run using Run\\Run_SVIHM.bat', file = f, append=T)
  write('pause', file = f, append=T)
}
