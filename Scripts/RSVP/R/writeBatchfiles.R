

#-------------------------------------------------------------------------------------------------#

write_scenario_prep_batchfile <- function(scenario_name='basecase',
                                          output_dir=data_dir['svihm_dir','loc'],
                                          filename=NULL,
                                          tabfiles=TRUE,
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
  write('xcopy SVIHM_Input_Files\\time_independent_input_files\\system_commands.txt Run\\SWBM /Y /I', file = f, append=T)
  write('xcopy SVIHM_Input_Files\\time_independent_input_files\\ET_Zone_Cells.txt Run\\SWBM /Y /I', file = f, append=T)
  write('xcopy SVIHM_Input_Files\\time_independent_input_files\\ET_Cells_Extinction_Depth.txt Run\\SWBM /Y /I', file = f, append=T)
  write('xcopy SVIHM_Input_Files\\time_independent_input_files\\recharge_zones.txt Run\\SWBM /Y /I', file = f, append=T)
#  write('xcopy SVIHM_Input_Files\\time_independent_input_files\\SFR_network.txt Run\\SWBM /Y /I', file = f, append=T)
  write('xcopy SVIHM_Input_Files\\time_independent_input_files\\SFR_routing.txt Run\\SWBM /Y /I', file = f, append=T)
  write('xcopy SVIHM_Input_Files\\time_independent_input_files\\SFR_inflow_segments.txt Run\\SWBM /Y /I', file = f, append=T)
  write('xcopy SVIHM_Input_Files\\time_independent_input_files\\ag_well_summary.txt Run\\SWBM /Y /I', file = f, append=T)
  write('xcopy SVIHM_Input_Files\\time_independent_input_files\\ag_well_list_by_polygon.txt Run\\SWBM /Y /I', file = f, append=T)
  write('xcopy SVIHM_Input_Files\\time_independent_input_files\\muni_well_summary.txt Run\\SWBM /Y /I', file = f, append=T)
  write('xcopy SVIHM_Input_Files\\time_independent_input_files\\muni_well_list_by_polygon.txt Run\\SWBM /Y /I', file = f, append=T) #???? Check if needed
  write('xcopy SVIHM_Input_Files\\time_independent_input_files\\print_daily.txt Run\\SWBM /Y /I', file = f, append=T)
  write('xcopy SVIHM_Input_Files\\time_independent_input_files\\polygons_table.txt Run\\SWBM /Y /I', file = f, append=T)
  write('xcopy SVIHM_Input_Files\\time_independent_input_files\\precip_factors.txt Run\\SWBM /Y /I', file = f, append=T)
  write('xcopy SVIHM_Input_Files\\time_independent_input_files\\landcover_table.txt Run\\SWBM /Y /I', file = f, append=T)
  write('xcopy SVIHM_Input_Files\\time_independent_input_files\\irr_ditch.txt Run\\SWBM /Y /I', file = f, append=T) #? need basecase?
  write('xcopy SVIHM_Input_Files\\time_independent_input_files\\SVIHM.* Run\\MODFLOW /Y /I', file = f, append=T)
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

write_update_prep_batchfile <- function(update_dir,
                                        output_dir=data_dir['svihm_dir','loc'],
                                        tabfiles=TRUE,
                                        scenario_name = "basecase") {

  # Get new filename
  update_name <- basename(update_dir)
  filename <- paste0('Prepare_Update_',update_name,'_Run.bat')
  f <- file.path(output_dir, filename)

  # Get relative location
  relpath <- gsub('../','',update_dir, fixed = T)
  relpath <- gsub('/', '\\', relpath, fixed=T)

  # Write normal batchfile
  write_scenario_prep_batchfile(output_dir=output_dir,
                                filename = filename,
                                scenario_name = scenario_name,
                                tabfiles = tabfiles,
                                write_end = F)

  # New lines
  write(':: Copy Updated Files to run folders', file = f, append=T)
  write(paste0('xcopy ',relpath,'\\*.txt Run\\SWBM /Y /I'), file = f, append=T)
  write(paste0('xcopy ',relpath,'\\SVIHM.* Run\\MODFLOW /Y /I'), file = f, append=T)

  # Write end
  write('', file = f, append=T)
  write(':: Report', file = f, append=T)
  write('echo.', file = f, append=T)
  write('echo Run folder populated with %scen%. Model can be run using Run\\Run_SVIHM.bat', file = f, append=T)
  write('pause', file = f, append=T)
}
