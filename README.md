##### Table of Contents  
[Summary](#Summary)  
[Required Software](#Required-Software)  
[Required Hardware](#Required-Hardware)   
<a name="Summary"/>
<a name="Required Software"/>
<a name="Required Hardware"/>

# Summary
The Scott Valley Integrated Hydrologic Model (SVIHM) simulates hydrologic conditions in the Scott Valley from October 1, 1990 through September 30th, 2011 (WY1991-WY2011). It is a combination of three models (streamflow regression model, soil-water budget model, and MODFLOW model) that are run sequentially. The files contained within this repository allow the user to run the model, post-process results, perform sensitivity analyses, perform parameter estimation, and perform linear and non-linear uncertainty analysis.

# Required Software
## Fortran Compiler

Several of the programs used in the model were developed in fortran (specifically fortran 90). It may be necessary to recompile the executables on your own machine using the batch scripts provided. We use gfortran, a free fortran compiler available at <https://gcc.gnu.org/wiki/GFortran>.

## UCODE_2014
UCODE_2014 is a free software suite developed by the USGS that performs automated sensitivity analysis, calibration, and uncertainty analysis. It also has utilities for calculating how non-linear a model is using a modified Beale's measure, influence statistics for observations, and confidence intervals, among others. UCODE_2014 is available at <http://igwmc.mines.edu/freeware/ucode/>. 

## R
R is a free software environment for statistical computing and graphics. Post-processing of model results and updating of model input files during model execution are done using R scripts. R is available at <https://www.r-project.org/>.

# Required Hardware
The SVIHM was developed on a 64-bit Windows machine using an Intel(r) Core(tm) i7-4770 CPU @ 3.40GHz with 16 GB of RAM. Sensitivity and Calibration scripts provided run in parallel using six processors at a time. Input files and batch scripts can be altered to increase or decrease the number of processors used. A single sensitivity analysis or calibration run requires approximately 20 GB of storage space during execution, but this can be reduced to about 3 GB after completion by deleting redundant files necessary for parallization. 

# 

