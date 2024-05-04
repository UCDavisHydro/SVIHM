#-- Script to install all necessary packages for working with RSVP
#-- Most importantly, it installs RMODLFOW from GitHub
#-- All other packages are available on CRAN

# Setup -------------------------------------------------------------------

# List of CRAN packages to be installed
list.of.packages <- c('RColorBrewer',
                      'zoo',
                      'sf',
                      'sp',
                      'raster',
                      'dataRetrieval',
                      'cimir',
                      'Matrix',
                      'dplyr',
                      'remotes',
                      'devtools')

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]


# CRAN Download/Install ---------------------------------------------------

if(length(new.packages)) install.packages(new.packages)


# GitHub Install ----------------------------------------------------------

#-- May prompt asking to install various packages
#remotes::install_github("rogiersbart/RMODFLOW")
devtools::install_github("ropensci/rnoaa")
devtools::install_github("cneyens/RMODFLOW@develop")
