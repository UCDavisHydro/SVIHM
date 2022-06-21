library(usethis)
library(sf)

#-------------------------------------------------------------------------------------------------#

#-- Create data_dir internal dataset (a directory of directories)

# Assumed path from RSVP package default dir (located in Scripts/RSVP) to SVIHM dir
svihm_dir <- file.path('../../')

data_dir <- read.table(text=
                         'key              rel_loc
                    svihm_dir        .
                    input_files_dir  SVIHM_Input_Files
                    time_indep_dir   SVIHM_Input_Files/time_independent_input_files
                    ref_data_dir     SVIHM_Input_Files/reference_data
                    scenario_dev_dir SVIHM_Input_Files/Scenario_Development
                    update_dir       SVIHM_Input_Files/Updates
                    sf_reg_dir       Streamflow_Regression_Model/
                   ', header=T, row.names=1)

# Add relative path to create "exact" location column
data_dir$loc <- file.path(svihm_dir, data_dir$rel_loc)

# Check (it's important the directories exist)
if (!all(dir.exists(data_dir$loc))) {
  stop(paste('One or more directories do not exist:', data_dir[!dir.exists(data_dir$loc),'loc']))
}

#-------------------------------------------------------------------------------------------------#

#-- Create stream_metadata.R, an internal dataframe full of information about stream/creek data files

streams = c('FJ','East_Fork','South_Fork','Sugar','Etna','French','Patterson','Kidder','Moffett','Mill','Shackleford')

gauge_daily_mean_files <- c('USGS_11519500_WY_1942_2018.txt',  # Can (should) be replaced with latest version
                            'East_Fork_Scott_River_R_input.txt',
                            'South_Fork_Scott_River_R_input.txt',
                            'Sugar_Creek_R_input.txt',
                            'Etna_Creek_R_input.txt',
                            'French_Creek_R_input.txt',
                            'Patterson_Creek_R_input.txt',
                            'Kidder_Creek_R_input.txt',
                            'Moffett_Creek_R_input.txt',
                            'Mill_Creek_R_input.txt',
                            'Shackleford_Creek_R_input.txt')

stream_metadata <- data.frame(name=streams,
                              daily_mean_file=gauge_daily_mean_files)

#-------------------------------------------------------------------------------------------------#

#-- Add to internal data
usethis::use_data(data_dir, stream_metadata, internal = T, overwrite = T)
