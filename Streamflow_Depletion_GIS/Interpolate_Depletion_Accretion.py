# Name: Interpolate_Depletion_Accretion.py
# Description: Interpolate streamflow depletion/accretion data from point shapefiles created by 'Generate_Point_Shapefiles.R' 
# Requirements: Spatial Analyst Extension

# Import system modules
import arcpy
from arcpy import env   
from arcpy.sa import *
import numpy
import os
from datetime import datetime, timedelta
from collections import OrderedDict
import shutil

# Set environment settings
cell_type = 'Accretion'    #Depletion or Accretion
env.workspace = "C:/Users/dtolley/Scott_Valley/Github/SVIHM/Streamflow_Depletion_GIS/Shapefiles/" + cell_type 
arcpy.CheckOutExtension("Spatial") # Check out the ArcGIS Spatial Analyst extension license

filenames = arcpy.ListFeatureClasses(feature_type='point', wild_card = '*May01*')
print(filenames)

#Define functions for creating rasters
def IDW_FJ_dep(filename):
  "Interpolate Normalized Streamflow Depletion at FJ gage"
  filename_prefix = filename.strip('.shp')
  zField = "StrmFlwDep"
  cellSize = 100
  mask_file = "C:/Users/dtolley/Scott_Valley/Github/SVIHM/Streamflow_Depletion_GIS/Shapefiles/100m_model_boundary_polygon_20180409.shp"
  masked_raster = "C:/Users/dtolley/Scott_Valley/Github/SVIHM/Streamflow_Depletion_GIS/FJ_Depletion_Rasters/" + filename_prefix
  IDW_rast = Idw(filename, zField, cellSize)
  IDW_rast_msk = ExtractByMask(IDW_rast, mask_file)   # Execute Extract by Mask
  IDW_rast_msk.save(masked_raster)
  
def IDW_SR_dry_cells(filename):
  "Interpolate length of Scott River segments that go dry (dry is defined as <1cfs (<2446.58 m^3/day)"
  filename_prefix = filename.strip('.shp')
  zField = "drySR_m"
  cellSize = 100
  mask_file = "C:/Users/dtolley/Scott_Valley/Github/SVIHM/Streamflow_Depletion_GIS/Shapefiles/100m_model_boundary_polygon_20180409.shp"
  masked_raster = "C:/Users/dtolley/Scott_Valley/Github/SVIHM/Streamflow_Depletion_GIS/SR_Dry_Length_Rasters/" + filename_prefix
  IDW_rast = Idw(filename, zField, cellSize)
  IDW_rast_msk = ExtractByMask(IDW_rast, mask_file)   # Execute Extract by Mask
  IDW_rast_msk.save(masked_raster)

def IDW_flow_reduction_pct(filename):
  "Interpolate Percentage of Flow Reduction"
  filename_prefix = filename.strip('.shp')
  zField = "FJFlwRdPct"
  cellSize = 100
  mask_file = "C:/Users/dtolley/Scott_Valley/Github/SVIHM/Streamflow_Depletion_GIS/Shapefiles/100m_model_boundary_polygon_20180409.shp"
  masked_raster = "C:/Users/dtolley/Scott_Valley/Github/SVIHM/Streamflow_Depletion_GIS/FJ_Flow_Reduction_Pct_Rasters/" + filename_prefix
  IDW_rast = Idw(filename, zField, cellSize)
  IDW_rast_msk = ExtractByMask(IDW_rast, mask_file)   # Execute Extract by Mask
  IDW_rast_msk.save(masked_raster)

rec = open('C:/Users/dtolley/Scott_Valley/Github/SVIHM/Streamflow_Depletion_GIS/record_file.txt', 'w')
for i in filenames:
	rec.write('Streamflow Depletion Raster' + i + '\n')
	rec.flush()
	IDW_FJ_dep(i)             
	rec.write('Dry Segments Raster ' + i + '\n')
	rec.flush()
	IDW_SR_dry_cells(i)
	rec.write('Flow Reduction Raster ' + i + '\n')
	rec.flush()
	IDW_flow_reduction_pct(i)  		
rec.close()

       
