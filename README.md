# Summary
The Scott Valley Integrated Hydrologic Model (SVIHM) simulates hydrologic conditions in the Scott Valley from October 1, 1990 through September 30th, 2011 (WY1991-WY2011). It is a combination of three models (streamflow regression model, soil-water budget model (SWBM), and MODFLOW model) that are run sequentially. The files contained within this repository allow the user to run the model, post-process results, perform sensitivity analyses, perform parameter estimation, and perform linear and non-linear uncertainty analysis.

The version in this repository contains additional capability through R scripts to update SVIHM through the last month. Input data for precipitation, stream gauges, and evapotranspiration data are all downloaded automatically and incorporated into the model input files.

## A Note on the Instructions Below
The model is setup to be easily run on computers running Microsoft Windows. Other operating systems will be require additional compilation and assembly. If this becomes a persistent request, additional files could be added.

# Running the Model
The first step in running SVIHM is to run a Windows batch file that assembles a `Run` folder with the MODFLOW and SWBM model files. The batch file is in the main SVIHM folder and can be run from the command line using its name:
`
Prepare_Basecase_Run.bat
`
The operations of this batch file (copying a bunch of files!) can be seen by opening the batch file in a text editor like Notepad++. When the batch file finishes running, it will report:

> Run folder populated with basecase. Model can be run using Run\Run_SVIHM.bat

The model can then be run by calling the `Run_SVIHM.bat` batch file in the Windows command prompt, just like was done above. This will run the SWBM, then MODFLOW, then two update utilities, then re-run the SWBM and MODFLOW models. This constitutes one full run of SVIHM.

# R Package - RSVP
Updating and visualizing the model requires [R](https://cran.r-project.org/) and [RStudio](https://www.rstudio.com/). Strictly speaking, R experience is not necessary for running the R scripts but will be valuable if anything goes awry. An R package has been developed specifically for this project, called the R Scott Valley Project (RSVP) package, which contains a large collection of R functions for processing data files, updating the model, and visualizing data & results. Additionally, four scripts exist for working with the model, which are intended to be able to run without modification:

1. `01_InputDataUpdate.R` Downloads the latest precipitation, stream gauge, and evapotranspiration data
2. `02_BaseUpdate.R` uses the newly downloaded data to write SVIHM input files
3. `03_UpdateVisualization.R` visualizes the newly downloaded data *under construction*
4. `04_ModelOutputVisualization.R` creates a large number of graphs showcasing the model fitness and results

While RSVP can be used as an R Package, it is more advisable to open it as a *project* in RStudio. Instructions for working with projects in RStudio are given [here](https://support.posit.co/hc/en-us/articles/200526207-Using-RStudio-Projects).

## Updating the Model
The update script is located in `SVIHM\Scripts\RSVP\01_InputDataUpdate.R`. As it mentioned in the "Settings" section of the script, special access codes from [NOAA](https://www.ncdc.noaa.gov/cdo-web/webservices/v2) and [CIMIS](https://cimis.water.ca.gov/) are required to access their online data. These codes are accessed in the script currently by reading the files named `_noaa_cdo_token.txt` and `_CIMIS_API_key.txt`. They are included in the repository since they are intended to be unique to the user (to prevent overuse). The easiest path for a new user is to simply create these files within the `SVIHM\Scripts\RSVP` directory. The text file should have nothing in it except the token.

To run the script in RStudio, click the 'Source' button in RStudio, or click Code->Source (Ctrl+Shift+S). It should generate a new folder in `SVIHM\SVIHM_Input_Files\Updates\` named by the last day of simulation in the newly generated model (last day of the previous month).

Next the model files need to be updates using this newly downloaded data. This is a separate script: `SVIHM\Scripts\RSVP\02_BaseUpdate.R`. Run it the same way as above. This will create new transient model input files in the same `Update` data folder.

### Running the Updated Model
The second script produces a new Windows batch file in the main SVIHM folder, `Prepare_Update_%Y-%m-%d_Run.bat`, with %Y, %m, %d replaced with the year, month, and day respectively. This script, like the similarly named one discussed above in **Running the Model**, will assemble a Run folder with the newly updated model.

*Note: It is suggested you remove/delete an existing Run folder before running the script.*

Running the new model works exactly the same as above - it uses the same `Run_SVIHM.bat` batch file.

## Visualizing
The visualization R Script is `SVIHM\Scripts\RSVP\04_ModelOutputVisualization.R`. It currently contains some sections that plot water levels and depth to water table that take very long to run, so it may be wise to run the script [in sections or line by line](https://support.rstudio.com/hc/en-us/articles/200484448-Editing-and-Executing-Code-in-the-RStudio-IDE#executing).

Some of the code requires the [RMODFLOW package](https://github.com/rogiersbart/RMODFLOW) which is not available on CRAN. [This page](https://rogiersbart.github.io/RMODFLOW/) provides information on how to install the package along with other great information.

Plots are output to a subfolder in the `Run` folder, named `Plots`.
