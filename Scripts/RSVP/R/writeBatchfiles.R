

#-------------------------------------------------------------------------------------------------#

write_scenario_prep_batchfile <- function(scenario_name='basecase',
                                          output_dir=data_dir['svihm_dir','loc'],
                                          filename='Prepare_Basecase_Run.bat',
                                          write_end=TRUE) {
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
  write('xcopy SVIHM_Input_Files\\time_independent_input_files\\SVIHM.* Run\\MODFLOW /Y /I', file = f, append=T)
  write('xcopy SVIHM_Input_Files\\time_independent_input_files\\Starting_Heads_L*.txt Run\\MODFLOW /Y /I', file = f, append=T)
  write('', file = f, append=T)
  write(':: Copy files from scenario folder to run folder', file = f, append=T)
  write('xcopy Scenarios\\%scen%\\*.txt Run\\SWBM /Y /I', file = f, append=T)
  write('xcopy Scenarios\\%scen%\\*.zone Run\\SWBM /Y /I', file = f, append=T)
  write('xcopy Scenarios\\%scen%\\SVIHM.* Run\\MODFLOW /Y /I ', file = f, append=T)
  write('xcopy SVIHM_Input_Files\\time_independent_input_files\\SVIHM_*_template.txt Run\\SWBM /Y /I', file = f, append=T)
  write('', file = f, append=T)
  write(':: Copy in generic run batch file', file = f, append=T)
  write('xcopy Scripts\\Batch_Scripts\\Run_SVIHM.bat Run /Y /I', file = f, append=T)
  write('', file = f, append=T)
  write(':: Copy in Update_Drain_Inflows input file', file = f, append=T)
  write('xcopy SVIHM_Input_Files\\Update_Drain_Inflows.in Run\\MODFLOW /Y /I', file = f, append=T)
  write('', file = f, append=T)
  write(':: Files of Unknown Necessity', file = f, append=T)
  write('xcopy Calib_Sens_Files\\UCODE\\UCODE_Template_Files\\SVIHM_SFR.jtf Run\\SWBM /Y /I', file = f, append=T)
  write('', file = f, append=T)
  if (write_end) {
    write(':: Report', file = f, append=T)
    write('echo.', file = f, append=T)
    write('echo Run folder populated with %scen%. Model can be run using Run\\Run_SVIHM.bat', file = f, append=T)
    write('pause', file = f, append=T)
  }
}

#-------------------------------------------------------------------------------------------------#

write_update_prep_batchfile <- function(update_dir, output_dir=data_dir['svihm_dir','loc']) {

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
