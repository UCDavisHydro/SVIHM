library(RSVP)
library(sf)

m2_to_acres = 1/4046.856
apn_overlap_threshold = 0.5
exclude_terrible_and_major_overflow_polygons = F
start_date <- get_model_start(1991)
end_date <- as.Date('2022-12-31')
output_dir <- data_dir["ref_data_dir","loc"]  # reference dir

recognized_scenarios = c("basecase",
                       "curtail_00_pct_all_years",
                       "curtail_50_pct_2022",
                       "curtail_30_pct_2022", "curtail_10_pct_2022",
                       "basecase_2023.06.05_curtail_00_pct_2023",
                       "basecase_2023.06.05_curtail_30_pct_2023")
curtail_scenarios = c("curtail_00_pct_all_years", "curtail_50_pct_2022",
                    "curtail_30_pct_2022", "curtail_10_pct_2022")
curtail_2023_scenarios = c("basecase_2023.06.05_curtail_00_pct_2023",
                         "basecase_2023.06.05_curtail_10_pct_2023",
                         "basecase_2023.06.05_curtail_20_pct_2023",
                         "basecase_2023.06.05_curtail_30_pct_2023",
                         "basecase_2023.06.05_curtail_40_pct_2023",
                         "basecase_2023.06.05_curtail_50_pct_2023",
                         "basecase_2023.06.05_curtail_60_pct_2023") # 2023 projected curtailment scenarios, June 4th 2023
output_filename = "curtailment_fractions.txt"
print(paste("Writing SWBM file:", output_filename))

# pull reference land cover
poly_tab = read.table(file.path(data_dir["time_indep_dir","loc"],"polygons_table.txt"),
                    header = T)
num_unique_fields = length(unique(poly_tab$SWBM_id))
# Check for polygon ID number continuity
if(nrow(poly_tab) != num_unique_fields){
  print("Caution: polygons (fields) info table contains replicated or missing ID numbers")
}
# generate a stress-period-by-field table (wide format)  for saving to output
field_df = swbm_build_field_value_df(nfields = num_unique_fields,
                                   model_start_date = start_date,
                                   model_end_date = end_date)

# Build 0-curtailment fraction table
field_column_selector = grepl(pattern = "ID", x = colnames(field_df))
field_df[,field_column_selector] = 0

## Basecase curtailment scenario -----------------------------------------

## _Process 2022 LCS spatial data -------------------------------------------------------

ref_dir = data_dir["ref_data_dir","loc"]

### Process spatial data associated with curtailment applications from 2022
# Read LCS land cover files (fields from Landuse and APNs from Plans shapefile)
lcs22_fields_all = sf::read_sf(dsn = file.path(ref_dir,
                                               "Landuse_LCS2022 app matching_2023.05.16.shp"))

# plot(lcs_fields$geometry, col = lcs_fields$LCS_APP)

# Process LCS fields with data quality notes
# overflow50_fields = lcs22_fields_all[grepl(pattern = "overflow50",
#                                        lcs22_fields_all$Comments),]
# overflow_major_fields = lcs22_fields_all[grepl(pattern = "majoroverflow",
#                                          lcs22_fields_all$Comments),]
# overflow_terrible_fields = lcs22_fields_all[grepl(pattern = "terribleoverflow",
#                                              lcs22_fields_all$Comments),]
# sliver_fields = lcs22_fields_all[grepl(pattern = "slivercoverage",
#                                                 lcs22_fields_all$Comments),]
# multipol_fields = lcs22_fields_all[grepl(pattern = "multipol",
#                                      lcs22_fields_all$Comments),]
# other_overflow_fields = c(overflow50_fields$Poly_nmbr,
#                           overflow_major_fields$Poly_nmbr,
#                           overflow_terrible_fields$Poly_nmbr,
#                           sliver_fields$Poly_nmbr)
# overflow_fields = lcs22_fields_all[grepl(pattern = "overflow", lcs22_fields_all$Comments) &
#                                    !(lcs22_fields_all$Poly_nmbr %in% other_overflow_fields),]
# # calculate acreage for categories
# (sum(as.numeric(st_area(overflow50_fields))) * m2_to_acres)
# (sum(as.numeric(st_area(overflow_major_fields))) * m2_to_acres)
# (sum(as.numeric(st_area(overflow_terrible_fields))) * m2_to_acres)
# (sum(as.numeric(st_area(overflow_fields))) * m2_to_acres)
# (sum(as.numeric(st_area(sliver_fields))) * m2_to_acres)
# (sum(as.numeric(st_area(multipol_fields))) * m2_to_acres)


# plot(overflow50_fields$geometry)
# plot(overflow_major_fields$geometry)
# plot(overflow_terrible_fields$geometry, col = c("red", "dodgerblue", "green4"), main = "Top 3 unreasonable polygons")
# plot(lcs22_fields_all$geometry, add=T, fill = NA)
# plot(sliver_fields$geometry)

# png(filename = file.path("top 3 unreasonable polygons.png"))
# plot(overflow_terrible_fields$geometry, col = c("red", "dodgerblue", "green4"), main = "Top 3 unreasonable polygons")
# plot(lcs22_fields_all$geometry, add=T, fill = NA)
# dev.off()

# plot(lcs22_fields_all$geometry)
# plot(overflow_terrible_fields$geometry, col = "firebrick", add=t)
# plot(overflow_major_fields$geometry, col = "orangered", add=t)
# plot(overflow50_fields$geometry, col = "goldenrod", add=t)

# Process APNs
# For apps with no visual maps, gotta use the APN data
lcs22_apns_all = sf::read_sf(dsn = file.path(data_dir["ref_data_dir","loc"],
                                             "LCS 2022 Plans_App matching_2023.05.shp"))
lcs22_apns_all = sf::st_transform(x = lcs22_apns_all, crs = sf::st_crs(lcs22_fields_all))
lcs22_apns = lcs22_apns_all[lcs22_apns_all$NoMap!=0,]
nomap_apps = lcs22_apns$NoMap
# remove "2045" and add "20" and "45"
nomap_apps = nomap_apps[nomap_apps!=2045]
nomap_apps = c(nomap_apps, 20, 45)

# Read in the LCS application info tab
lcs22_info = read.table(file.path(data_dir["ref_data_dir","loc"], "2022 LCS Field Number Matching_interim.txt"),
                        header = T, sep = "\t", fill =T, fileEncoding = "latin1")
for(i_app in nomap_apps){ #
  print(paste("Finding field overlap fractions for APNs in LCS app. no.",i_app))
  lcs_acres = lcs22_info$Acres[lcs22_info$Application_Number==i_app]

  if(i_app == 20 | i_app == 45){i_app=2045}

  apn_footprint = lcs22_apns[lcs22_apns$NoMap==i_app,]
  apn_acres = as.numeric(sf::st_area(apn_footprint))*m2_to_acres
  # Find fields overlapping with apn footprint
  lu_poly_overlap = lcs22_fields_all[apn_footprint,]
  lu_poly_overlap$fraction_overlap_apn = NA

  # calculate % of area of each field that overlaps with APN footprint
  for(j in 1:nrow(lu_poly_overlap)){
    field_j = lu_poly_overlap[j,]
    field_apn_intersect = sf::st_intersection(x = field_j, y = apn_footprint)
    field_area_overlap_fraction = as.numeric(sf::st_area(field_apn_intersect) / sf::st_area(field_j))
    lu_poly_overlap$fraction_overlap_apn[j] = field_area_overlap_fraction
  }

  # Assign fields with more than the designated threshold (i.e. 50%) of overlap
  # with the APN footprint to the relevant LCS app number
  assign_these_fields = lu_poly_overlap$Poly_nmbr[lu_poly_overlap$fraction_overlap_apn > apn_overlap_threshold]
  lcs22_fields_all$LCS_APP[lcs22_fields_all$Poly_nmbr %in% assign_these_fields] = i_app
}

# Now that we've assigned all the fields with their LCS app,
# restrict lcs_fields to just ones with an app number
lcs_fields = lcs22_fields_all[!is.na(lcs22_fields_all$LCS_APP),]
# exclude terrible overflow and major overflow
if(exclude_terrible_and_major_overflow_polygons == T){
  lcs_fields_excl_major = lcs_fields[!(grepl(pattern = "majoroverflow",
                                             x = lcs_fields$Comments) |
                                         grepl(pattern = "terribleoverflow",
                                               x = lcs_fields$Comments)),]
  # sum(st_area(lcs_fields_excl_major)) * m2_to_acres
  # sort(unique(lcs_fields_excl_major$LCS_APP))
  # not included (no map): 12, 13, 18, 20, 23, 25, 28, 31, 32, 45
  # not included (major or terrible overflow): 15, 30 (small acreage)
  lcs_fields = lcs_fields_excl_major
  # sum(st_area(lcs_fields)) * m2_to_acres  # 18.7k acres relative to 17k reported. Not so bad
}
# sum(st_area(lcs_fields)) * m2_to_acres  # If include major and terrible overflow, 20.9k acres relative to 17k reported. Not so bad



## _Process 2022 LCS curtailment data -------------------------------------------------------

# Build the curtailment file for 2022 and then 2021.
# build historical curtailments if scenario is Basecase or unrecognized

# read and process 2021 and 2022 curtailment input data

# Compare spatial area of identified corresponding fields (gis acres) to reported acreage
lcs22_info$acres_reported_to_gis_ratio = NA
for(i_app in lcs22_info$Application_Number){
  acres_reported = lcs22_info$Acres[i_app]
  if(i_app == 20 | i_app == 45){i_app_temp = 2045} else { i_app_temp = i_app}
  app_fields = lcs_fields[lcs_fields$LCS_APP==i_app_temp,]
  acres_gis = sum(as.numeric(sf::st_area(app_fields))*m2_to_acres)
  # print(paste("App",i_app, ":", round(acres_reported), "rep. acres;",round(acres_gis), "GIS acres"))
  lcs22_info$acres_reported_to_gis_ratio[i_app] = acres_reported/acres_gis
}

# Table: month, parcel id, fraction curtailed

# Simplified curtailment rules
# For missing % conserved volumes, apply the average of all LCS apps for that month
conserv_cols_index = grepl(pattern = "Spreadsheet_Check", x = colnames(lcs22_info)) &
  grepl(pattern = "conserved", x = colnames(lcs22_info)) &
  ! (grepl(pattern = "Total", x = colnames(lcs22_info)))
lcs22_info[,conserv_cols_index] = as.numeric(as.matrix(lcs22_info[,conserv_cols_index]))
# calculate monthly conservation averages
month_avg_cons =apply(X = lcs22_info[,conserv_cols_index], MARGIN = 2,
                      FUN = mean, na.rm=T)

# add new columns to the end of lcs22_info table for building model input (to avoid altering original columns with NAs)
conserv_preweight = paste0(month.abb[4:10], "_pct_cons_preweight")
# Assign values to model-input columns
lcs22_info[,conserv_preweight] = lcs22_info[,conserv_cols_index]
# fill NAs in each month with monthly average
for(i in 1:sum(conserv_cols_index)){
  month_cons = lcs22_info[,conserv_preweight[i]]
  month_cons[is.na(month_cons)] = month_avg_cons[i]
  # reset curtail_frac model input column for that month
  lcs22_info[,conserv_preweight[i]] = month_cons
}


# OK, now each application has:
## A curtailment conservation % for each month.
## A reported acreage-reported-to-GIS-acreage ratio.
## For each month, we can multiply the conservation % by the reported-to-GIS ratio
# to get a value to apply to each field within the Landuse fields footprint (minus unirrigated fields)

# This should be the same spatial data as lcs22_fields_all but why not bring in the fresh dataset
svihm_fields = sf::read_sf(dsn = file.path(data_dir["ref_data_dir","loc"],"Landuse_20190219.shp"))

# Initialize tab to store area weighted info for each field
summary_tab_22 = data.frame(swbm_id = sort(svihm_fields$Polynmbr))
# Attach LCS curtailment app number to each field ID
summary_tab_22$curtail_app_id = NA
summary_tab_22$curtail_app_id = lcs22_fields_all$LCS_APP[match( summary_tab_22$swbm_id, lcs22_fields_all$Poly_nmbr)]
# Apr-Oct conserv percentages from gap-filled LCS data
monthly_conserv = data.frame(matrix(data = NA, nrow = nrow(summary_tab_22),
                                    ncol = length(conserv_preweight)))
colnames(monthly_conserv) = conserv_preweight
summary_tab_22 = cbind(summary_tab_22, monthly_conserv)
# fill monthly conservation %s in summary_tab_22
for(month_abb in month.abb[4:10]){
  month_cons_colname = paste0(month_abb,"_pct_cons_preweight")
  summary_tab_22[,month_cons_colname] = lcs22_info[
    match(summary_tab_22$curtail_app_id, lcs22_info$Application_Number),
    month_cons_colname]
}

# Attach Landuse fields (gis) to reported acreage ratio
summary_tab_22$acres_reported_to_gis_ratio = NA
summary_tab_22$acres_reported_to_gis_ratio = lcs22_info$acres_reported_to_gis_ratio[
  match(summary_tab_22$curtail_app_id, lcs22_info$Application_Number)]

# Initialize columns for Apr-Oct conserv %ages after final weighting
# final weighting =
## reported curtail app % conserved for a given month, times
## the Landuse fields-area-to-reported-acreage-area ratio

conserv_for_model_lcs = paste0(month.abb[4:10], "_pct_cons_model")
summary_tab_22[,conserv_for_model_lcs] = NA
summary_tab_22[,conserv_for_model_lcs] = summary_tab_22[,conserv_preweight] * summary_tab_22$acres_reported_to_gis_ratio

## _2022 non-LCS fields -------------------------------------------------------

# OK. there are 3 time periods we are dealing with here.
# LCS months, Apr-Oct 2022.
# Non-LCS fields curtailed on July 13 - so July 2022 is 0.5 curtailment for non-LCS fields.
# Non-LCS fields remained curtailed through October 2022. So Aug-Oct 2022 is 1.0 curtailment for non-LCS fields.

conserv_for_model_nonlcs_0.5 = paste0(month.abb[7], "_pct_cons_model")
conserv_for_model_nonlcs_1.0 = paste0(month.abb[8:10], "_pct_cons_model")

non_lcs_fields = lcs22_fields_all[is.na(lcs22_fields_all$LCS_APP),]
# include fields listed as App #0, I think that's a quality control thing
fields_0 = lcs22_fields_all[lcs22_fields_all$LCS_APP==0 & !is.na(lcs22_fields_all$LCS_APP),]
non_lcs_fields = rbind(non_lcs_fields, fields_0)
non_lcs_field_ids = non_lcs_fields$Poly_nmbr
# curtail 100% of all water use on non-lcs fields
summary_tab_22[summary_tab_22$swbm_id %in% non_lcs_field_ids, conserv_for_model_nonlcs_0.5] = (17/31) #0.5
summary_tab_22[summary_tab_22$swbm_id %in% non_lcs_field_ids, conserv_for_model_nonlcs_1.0] = 1.0

# _Produce: month-by-field curtailment fraction tables for historical basecase ---------------------------------------------
# Initialize model input file (curtail_output of this process, input for model run)
curtail_output = field_df
swbm_ids = sort(svihm_fields$Polynmbr)
# match months to stress periods
# Basecase: non-LCS fields get 0.5 curtailment in July, 1.0 Aug, Sept, Oct of 2022
conserv_2022_months_lcs = as.Date(paste0("2022-",4:10,"-01"))
conserv_2022_month_dates_0.5 = as.Date(paste0("2022-",7,"-01"))
conserv_2022_month_dates_1.0 = as.Date(paste0("2022-",8:10,"-01"))

# Make the final input file. Make it run in fortran.
date_matcher_tab_lcs = data.frame(stress_per_date = conserv_2022_months_lcs,
                                  curtail_colname = conserv_for_model_lcs)
date_matcher_tab_0.5 = data.frame(stress_per_date = conserv_2022_month_dates_0.5,
                                  curtail_colname = conserv_for_model_nonlcs_0.5)
date_matcher_tab_1.0 = data.frame(stress_per_date = conserv_2022_month_dates_1.0,
                                  curtail_colname = conserv_for_model_nonlcs_1.0)
# Assign curtailment fractions for LCS fields, Apr-Oct 2022
for(i in 1:nrow(date_matcher_tab_lcs)){
  sp = date_matcher_tab_lcs$stress_per_date[i]
  cname = date_matcher_tab_lcs$curtail_colname[i]
  # assign overlap- and acre-ratio- weighted curtailment fractions to each stress period
  curtail_output[curtail_output$Stress_Period==sp, 2:ncol(curtail_output)] =
    summary_tab_22[match(swbm_ids, summary_tab_22$swbm_id),cname]
}
# Assign curtailments for non-LCS fields, 0.5 Jul 2022
for(i in 1:nrow(date_matcher_tab_0.5)){
  sp = date_matcher_tab_0.5$stress_per_date[i]
  cname = date_matcher_tab_0.5$curtail_colname[i]
  # assign overlap- and acre-ratio- weighted curtailment fractions to each stress period
  curtail_output[curtail_output$Stress_Period==sp, 2:ncol(curtail_output)] =
    summary_tab_22[match(swbm_ids, summary_tab_22$swbm_id),cname]
}
# Assign curtailments for non-LCS fields, 1.0 Aug-Oct 2022
for(i in 1:nrow(date_matcher_tab_1.0)){
  sp = date_matcher_tab_1.0$stress_per_date[i]
  cname = date_matcher_tab_1.0$curtail_colname[i]
  # assign overlap- and acre-ratio- weighted curtailment fractions to each stress period
  curtail_output[curtail_output$Stress_Period==sp, 2:ncol(curtail_output)] =
    summary_tab_22[match(swbm_ids, summary_tab_22$swbm_id),cname]
}

## _2021 data -------------------------------------------------------
# Make new summary tab including only the fields that did curtailments in Aug-Oct 2021
lcs_apps_21 = c(12, 13, 31) # Fawaz, Finley, Menne: 12, 13, 31
conserv_for_model_21 = paste0(month.abb[8:10],"_pct_cons_model")
summary_tab_21 = summary_tab_22[, c("swbm_id", "curtail_app_id", conserv_for_model_21)]
summary_tab_21[,conserv_for_model_21] = 0
summary_tab_21[summary_tab_21$curtail_app_id %in% lcs_apps_21, conserv_for_model_21] = 1

# match months to stress periods
conserv_2021_month_dates = as.Date(paste0("2021-",8:10,"-01"))
date_matcher_tab_21 = data.frame(stress_per_date = conserv_2021_month_dates,
                                 curtail_colname = paste0(month.abb[8:10],"_pct_cons_model"))

# Assign LCS
for(i in 1:nrow(date_matcher_tab_21)){
  sp = date_matcher_tab_21$stress_per_date[i]
  cname = date_matcher_tab_21$curtail_colname[i]
  # assign overlap- and acre-ratio- weighted curtailment fractions to each stress period
  curtail_output[curtail_output$Stress_Period==sp, 2:ncol(curtail_output)] =
    summary_tab_21[match(swbm_ids, summary_tab_21$swbm_id),cname]
}

# 2021 Part 2
# GW curtailed for whole valley from Sept 10 - Oct 25
conserv_for_model_0921 = paste0(month.abb[9], "_pct_cons_model")
conserv_for_model_1021 = paste0(month.abb[10], "_pct_cons_model")
curt_2021_sept = as.Date(paste0("2021-",10,"-01"))
curt_2021_oct = as.Date(paste0("2021-", 9,"-01"))
# Apply to all fields NOT n the lcs_apps_21
summary_tab_21[!(summary_tab_21$curtail_app_id %in% lcs_apps_21), conserv_for_model_0921] = 2/3 # 20 of 30 days
summary_tab_21[!(summary_tab_21$curtail_app_id %in% lcs_apps_21), conserv_for_model_1021] = 25/31

# Date matchers
date_matcher_tab_sept21 = data.frame(stress_per_date = curt_2021_sept,
                                     curtail_colname = conserv_for_model_0921)
date_matcher_tab_oct21 = data.frame(stress_per_date = curt_2021_oct,
                                    curtail_colname = conserv_for_model_1021)

# Assign curtailments for Sept 2021
for(i in 1:nrow(date_matcher_tab_sept21)){
  sp = date_matcher_tab_sept21$stress_per_date[i]
  cname = date_matcher_tab_sept21$curtail_colname[i]
  # assign overlap- and acre-ratio- weighted curtailment fractions to each stress period
  curtail_output[curtail_output$Stress_Period==sp, 2:ncol(curtail_output)] =
    summary_tab_21[match(swbm_ids, summary_tab_21$swbm_id),cname]
}

# Assign curtailments for Oct 2021
for(i in 1:nrow(date_matcher_tab_oct21)){
  sp = date_matcher_tab_oct21$stress_per_date[i]
  cname = date_matcher_tab_oct21$curtail_colname[i]
  # assign overlap- and acre-ratio- weighted curtailment fractions to each stress period
  curtail_output[curtail_output$Stress_Period==sp, 2:ncol(curtail_output)] =
    summary_tab_21[match(swbm_ids, summary_tab_21$swbm_id),cname]
}

# Final table cleanup
# Replace any NAs with 0s
curtail_output[is.na(curtail_output)] = 0
field_cols = grep(pattern = "ID", x=colnames(curtail_output))

# PLACEHOLDER UNTIL BETTER DATA QUALITY
# Identify fields with curtailment fractions > 1. Reset to 1.
# sum(curtail_output[,field_cols] > 1) # 195 of them
# sum(curtail_output[,field_cols] < 1 & curtail_output[,field_cols] > 0) #
curtail_output[,field_cols][curtail_output[,field_cols] > 1] = 1

# Write output file
curtail_output_subset <- subset.DateTwoSided(curtail_output,
                                             as.Date('2021-01-01'), as.Date('2022-12-31'),
                                             date_col = 'Stress_Period', include_end = T)
write.csv(curtail_output_subset, file=file.path(output_dir, 'Curtail_21_22.csv'), row.names = F)
