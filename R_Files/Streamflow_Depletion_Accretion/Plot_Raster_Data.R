#Script for plotting rasters generated from 'Interpolate_Depletion_Accretion.py' 

# Script Initialization ---------------------------------------------------
rm(list =ls())
library(raster)
library(rgdal)
library(sp)
library(colorRamps)
library(grid)
#library(ggplot2)
#library(ggspatial)
#library(ggplot2movies)


model_boundary = readOGR('../../Streamflow_Depletion_GIS/Shapefiles/100m_model_boundary_polygon_20180409.shp')

filenames = list.files(path = '../../Streamflow_Depletion_GIS/FJ_Depletion_Rasters/', pattern = 'kgm', full.names = T)
test = raster(filenames[2])

Dry_year_files = filenames[grep(x = filenames, pattern = 'jul01|aug01|sep01|oct01')]

breakpoints <- seq(0,1,by = 0.1)

png('test.png', width = 8, height = 16, units = 'in', res = 1000)
plot(model_boundary, main = 'Streamflow Depletion')
plot(test,breaks=breakpoints, col = matlab.like(10), add = T)
graphics.off()
