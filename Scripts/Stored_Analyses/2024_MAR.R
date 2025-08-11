#!/usr/bin/env Rscript
#
# scripts/generate_2024_MAR_depth_tables.R
#
# Reads raw MAR-diversion data and spatial layers,
# computes per-field, per-stress-period MAR depth for both
# the historical (basecase) and maximum-diversion scenarios,
# and writes out two files:
#   data/raw/MAR_depth_basecase.txt
#   data/raw/MAR_depth_maxMAR2024.txt

library(RSVP)    # for swbm_build_field_value_df()
library(sf)
library(dplyr)
library(lubridate)
library(units)

# 1) PARAMETERS --------------------------------------------------------------

start_date <- as.Date("2023-10-01")       # water-year start for MAR
end_date   <- as.Date("2024-09-30")       # water-year end for MAR

# file paths
polygons_file    <- file.path(data_dir["time_indep_dir","loc"], "polygons_table.txt")
recharge_zones   <- file.path(data_dir["time_indep_dir","loc"], "recharge_zones.txt")
fields_shp       <- file.path(data_dir["ref_data_dir","loc"], "Landuse_20190219_updated2023.shp")
mar_fields_shp   <- file.path(data_dir["ref_data_dir","loc"], "MAR_Fields_2024.shp")
mar_csv_base     <- file.path(data_dir["ref_data_dir","loc"], "Scott MAR wy 2024_cfs daily record.csv")
mar_csv_max      <- file.path(data_dir["ref_data_dir","loc"], "Scott MAR wy 2024_cfs daily record_Max Div.csv")

output_dir       <- data_dir["ref_data_dir","loc"]

for (scenario_id in c("basecase", "max")) {

  # pull reference land cover
  poly_tab = read.table(file.path(data_dir["time_indep_dir","loc"],"polygons_table.txt"), header = T)
  num_unique_fields = length(unique(poly_tab$SWBM_id))
  # Check for polygon ID number continuity
  if(nrow(poly_tab) != num_unique_fields){
    print("Caution: polygons (fields) info table contains replicated or missing ID numbers")
  }

  # generate a stress-period-by-field table (wide format)
  field_df = swbm_build_field_value_df(model_start_date = start_date,
                                       model_end_date = end_date,
                                       default_values = 0)

  # Initialize output file
  mar_depth_output = field_df
  # 1) TO DO: add in observed MAR for winter 2023

  # MAR 2024

  # 1) Read in data

  # Read in fields spatial file for spatial relation
  svihm_fields = st_read(dsn = file.path(data_dir["ref_data_dir","loc"]),
                         layer = "Landuse_20190219_updated2023")
  svihm_fields$MAR_distrib_mult = 0     # column for MAR distribution multiplier
  svihm_fields$area_m2 = sf::st_area(svihm_fields)
  # read in spatial file of recharge areas
  mar_fields24 = st_read(dsn = file.path(data_dir["ref_data_dir","loc"]),
                         layer = "MAR_Fields_2024")
  mar_fields24=st_transform(mar_fields24, crs=sf::st_crs(svihm_fields))
  # calculate and name field areas in m2
  mar_fields24_areas = st_area(mar_fields24)
  names(mar_fields24_areas) = substr(x = mar_fields24$Name, start = 7, stop = 7)
  # read in and process MAR diversion volumes
  if(tolower(scenario_id) == "basecase"){
    mar_app = read.csv(file=file.path(data_dir["ref_data_dir","loc"],
                                      "Scott MAR wy 2024_cfs daily record.csv"),
                       colClasses = c("character",rep("numeric",10)))
  } else if(tolower(scenario_id) == "max"){
    mar_app = read.csv(file=file.path(data_dir["ref_data_dir","loc"],
                                      "Scott MAR wy 2024_cfs daily record_Max Div.csv"),
                       colClasses = c("character",rep("numeric",10)))
  }

  mar_app$Date= as.Date(mar_app$Date, format = "%m/%d/%Y") # convert to dates
  # convert NAs to 0s and rename columns to field letter ID
  flow_cols = 2:ncol(mar_app) #identify columns with cfs data
  mar_app[,flow_cols][is.na(mar_app[,flow_cols])]= 0
  colnames(mar_app)[colnames(mar_app)!="Date"] =
    substr(colnames(mar_app)[colnames(mar_app)!="Date"], start=1, stop=1)
  # convert from daily avg cfs to cubic meters per day
  mar_app_m3day = mar_app
  cfs_to_m3day = 1 * 60*60*24 / 35.3147
  AF_to_m3 = 1233.48
  mar_app_m3day[,flow_cols] = round(mar_app[,flow_cols] * cfs_to_m3day)
  # aggregate to monthly for MAR input file
  mar_app_m3month = aggregate(x = mar_app_m3day[,flow_cols],
                              by = list(Date = floor_date(mar_app_m3day$Date, unit="month")),
                              FUN = sum)
  # Add measured ditch leakage to field volumes, proportionally
  total_added_m3 = 585 * AF_to_m3
  mar_app_m3month_tot = sum(apply(X=mar_app_m3month[,flow_cols], MARGIN = 2, FUN = sum))
  added_by_field = total_added_m3 * mar_app_m3month[,flow_cols] / mar_app_m3month_tot
  mar_app_m3month[,flow_cols] = mar_app_m3month[,flow_cols] + added_by_field
  # mar_depth_monthly = mar_app_m3month
  # mar_depth_monthly[,cfs_data] = mapply(`/`,mar_app_m3month[,cfs_data], mar_fields24_areas)

  # make table of stress periods for assigning water volumes to MAR output tab
  months = seq.Date(from = as.Date("1990-10-01"),
                    to = floor_date(Sys.Date(), unit="month"),
                    by="month")
  sp_tab = data.frame(Month = months, stress_period = 1:length(months))
  # Assign stress periods to table of MAR diversions
  mar_app_m3month$stress_period =
    sp_tab$stress_period[match(mar_app_m3month$Date,sp_tab$Month)]

  # 2) Relate MAR fields to SVIHM fields

  # Spatial relations to distribute water diverted for recharge
  # monthly applied volume by field (rchA_i)
  # for each MAR field i, need a column for:
  # recharge area for each field (rchA_i)

  # calculate area and total recharge area
  mar_fields24$area_m2 = sf::st_area(mar_fields24)
  rchA_all = sum(mar_fields24$area_m2)# total area of recharge fields
  # total recharge volume applied to each field i (rchMth_i)
  # 1) calculate:
  # rchMth_i = time series for each field
  # Then, for each intercepted SVIHM field j, need a column for:
  # SVIHM field area (sfA_j)
  # intercept area (intA_j)
  # Total monthly volume applied to each intercepted field (rchMth_j)
  # 2) calculate:
  # rchMth_j = rchMth_i * (intA_j / rchA_i)
  # 3) Convert to MAR depth for full intercepted field
  # rchMth_j_depth = rchMth_j / sfA_j
  # 4)  Populate output table with rchMth_j_depth for each field by month

  for(i in 1:nrow(mar_fields24)){
    mar_field = mar_fields24[i,]
    svihm_intercepts = svihm_fields[mar_field,]
    rchA_i = mar_field$area_m2

    # wigglies = c(714, 1015) # keep out some long snaking ones
    # svihm_intercepts=svihm_intercepts[svihm_intercepts$Poly_nmbr %in% wigglies,]
    # plot(svihm_intercepts$geometry, col = "lightblue")
    # plot(mar_field$geometry, add=T, col=rgb(1,0,0,.5))
    for (j in 1:nrow(svihm_intercepts)){
      sv_field = svihm_intercepts[j,]
      sfA_j = sv_field$area_m2
      # plot(mar_field)
      for(k in 1:nrow(mar_app_m3month)){
        nchar_name = nchar(mar_field$Name)
        field_picker = substr(mar_field$Name, stop = nchar_name, start = nchar_name) == colnames(mar_app_m3month)
        rchMth_i_m3 = mar_app_m3month[k, field_picker]

        if(rchMth_i_m3>0){ # don't run spatial calcs if value of mar is 0
          field_intersec = sf::st_intersection(x=mar_field, y = sv_field)
          intA_j = sf::st_area(field_intersec)

          # SV field recharge volume = MAR field monthly recharge * SV-MAR intercept area / total SV field area
          rchMth_j_m3 = rchMth_i_m3 * (intA_j / rchA_i)
          # Convert to depth in meters
          rchMth_j_depth = round(units::drop_units(rchMth_j_m3 / sfA_j), 3)
          # assign output
          row_picker = mar_depth_output$Stress_Period==mar_app_m3month$Date[k]
          out_fields = gsub(x = colnames(mar_depth_output[,-1]),
                            pattern = "ID_", replacement = "")
          col_picker = which(sv_field$Poly_nmbr == out_fields) + 1
          # add MAR depth to output table
          mar_depth_output[row_picker,col_picker] = rchMth_j_depth + mar_depth_output[row_picker,col_picker]

        }
      }
    }

  }

  write.table(mar_depth_output, file = file.path(output_dir, paste0('MAR_24',scenario_id,'.csv')),
              sep = "  ", quote = FALSE, col.names = TRUE, row.names = FALSE)

}
