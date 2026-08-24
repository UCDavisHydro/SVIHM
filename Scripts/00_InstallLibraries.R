#-- Script to install all necessary packages for working with RSVP
#-- Most importantly, it installs RMODLFOW from GitHub
#-- All other packages are available on CRAN

# Note - please install Rtools first:
# https://cran.rstudio.com/bin/windows/Rtools/

# Setup -------------------------------------------------------------------

# List of CRAN packages to be installed
list.of.packages <- c('RColorBrewer',
                      'roxygen2',
                      'pak',
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
library(pak)
#pak("rogiersbart/RMODFLOW")
pak("ropensci/rnoaa")
pak("cneyens/RMODFLOW@develop")

# Assuming that all worked, you should now be able to build the RSVP Package
# Build -> Install Package (Ctrl+Shift+B on Windows)

# Next you should be able to run 01_InputDataUpdate & 02_BaseUpdate
# Or any of the other scripts requiring RSVP
