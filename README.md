# Summary
The Scott Valley Integrated Hydrologic Model (SVIHM) simulates hydrologic conditions in the Scott Valley from October 1, 1990 through September 30th, 2011 (WY1991-WY2011). It is a combination of three models (streamflow regression model, soil-water budget model, and MODFLOW model) that are run sequentially. The files contained within this repository allow the user to run the model, post-process results, perform sensitivity analyses, perform parameter estimation, and perform linear and non-linear uncertainty analysis.

# Required Software
## Fortran Compiler
Several of the programs used in the model were developed in fortran (specifically fortran 90). It may be necessary to recompile the executables on your own machine using the batch scripts provided. We use gfortran, a free fortran compiler available at <https://gcc.gnu.org/wiki/GFortran>