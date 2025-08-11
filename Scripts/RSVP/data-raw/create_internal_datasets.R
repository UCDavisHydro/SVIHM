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
                    ref_plot_dir     SVIHM_Input_Files/reference_data_for_plots
                    scenario_dir     Scenarios
                    scenario_dev_dir SVIHM_Input_Files/Scenario_Development
                    update_dir       SVIHM_Input_Files/Updates
                    sf_reg_dir       Streamflow_Regression_Model/
                    key_dir          Scripts/API_keys
                   ', header=T, row.names=1)

# Add relative path to create "exact" location column
data_dir$loc <- file.path(svihm_dir, data_dir$rel_loc)

# Check (it's important the directories exist)
if (!all(dir.exists(data_dir$loc))) {
  stop(paste('One or more directories do not exist:', data_dir[!dir.exists(data_dir$loc),'loc']))
}

#-------------------------------------------------------------------------------------------------#

#-- Create stream_metadata.R, an internal dataframe full of information about stream/creek data files

# streams = c('FJ','East_Fork','South_Fork','Sugar','Etna','French','Patterson','Kidder','Moffett','Mill','Shackleford','Johnson','Crystal')
# swbm_stream_order <-  c("Scott_River","Sugar","Miners","French","Etna","Johnson","Crystal","Patterson","Kidder","Moffett","Mill","Shackleford")
#
#
# prms_seg = c(139, 22, 31, 37, 67, 53, 86, 93, 109, 132, 135, 80, 81)
# gauge_daily_mean_files <- c('USGS_11519500_WY_1942_2018.txt',  # Can (should) be replaced with latest version
#                             'East_Fork_Scott_River_R_input.txt',
#                             'South_Fork_Scott_River_R_input.txt',
#                             'Sugar_Creek_R_input.txt',
#                             'Etna_Creek_R_input.txt',
#                             'French_Creek_R_input.txt',
#                             'Patterson_Creek_R_input.txt',
#                             'Kidder_Creek_R_input.txt',
#                             'Moffett_Creek_R_input.txt',
#                             'Mill_Creek_R_input.txt',
#                             'Shackleford_Creek_R_input.txt',
#                             NA,
#                             NA)
#
# stream_metadata <- data.frame(name=streams,
#                               daily_mean_file=gauge_daily_mean_files,
#                               PRMS_seg=prms_seg)

# Can copy to/from excel
stream_metadata <- read.table(text =
'name	PRMS_seg	MF_seg	subws	subws_name	in_modflow	daily_file
East_Fork	22	1	1	Scott_Tailings	TRUE	East_Fork_Scott_River_R_input.txt
South_Fork	31	1	1	Scott_Tailings	TRUE	South_Fork_Scott_River_R_input.txt
Wildcat	35	1	9	Unnamed_West	FALSE	NA
Sugar	37	2	1	Scott_Tailings	TRUE	Sugar_Creek_R_input.txt
McConaughy_Main	42	5	10	Unnamed_East	FALSE	NA
McConaughy_Branch	43	5	10	Unnamed_East	FALSE	NA
Miners	52	6	2	French	TRUE	NA
French	50	7	2	French	TRUE	French_Creek_R_input.txt
Clark	58	9	9	Unnamed_West	FALSE	NA
Etna	67	11	3	Etna	TRUE	Etna_Creek_R_input.txt
Heartstrand	69	12	10	Unnamed_East	FALSE	NA
Shell	71	12	10	Unnamed_East	FALSE	NA
Hamlin	73	14	10	Unnamed_East	FALSE	NA
Johnson	80	15	4	Patterson	TRUE	NA
Crystal	81	16	4	Patterson	TRUE	NA
Patterson	86	18	4	Patterson	TRUE	Patterson_Creek_R_input.txt
Kidder	93	21	5	Kidder	TRUE	Kidder_Creek_R_input.txt
Moffett	109	24	6	Moffett	TRUE	Moffett_Creek_R_input.txt
McAdams	112	24	11	Unnamed_North	FALSE	NA
Indian	118	25	11	Unnamed_North	FALSE	NA
Rattlesnake	123	26	11	Unnamed_North	FALSE	NA
Mill	132	27	7	Mill	TRUE	Mill_Creek_R_input.txt
Shackleford	134	28	8	Shackleford	TRUE	Shackleford_Creek_R_input.txt
Sniktaw	137	30	11	Unnamed_North	FALSE	NA
Patterson_North	126	30	11	Unnamed_North	FALSE	NA
                                ', header=T)

#-------------------------------------------------------------------------------------------------#

#-- Soil Water Balance Model Tables

# Irrigation method codes
swbm_irr_key <- data.frame(
  code = c(1, 2, 3, 555, 999),
  name = c("Flood",
           "Wheel Line",
           "Center Pivot",
           "No Source",
           "Unknown (Assumed Wheel Line)"),
  stringsAsFactors = FALSE
)

# Water source codes
watersource_key <- data.frame(
  code = c(1, 2, 3, 4, 5, 999),
  name = c("Surface Water",
           "Groundwater",
           "Mixed",
           "Sub-irrigated",
           "Dry Farmed",
           "Unknown (GW Assumed)"),
  stringsAsFactors = FALSE
)

#-- Add to internal data
usethis::use_data(data_dir,
                  stream_metadata,
                  swbm_irr_key,
                  watersource_key,
                  internal = T,
                  overwrite = T)
