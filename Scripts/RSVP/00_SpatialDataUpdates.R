# 00_SpatialDataUpdates.R
# Implements updates to some inptu files of time-invariant, spatial SVIHM datasets.
# Incorporates existing spatial data information (i.e., does not re-create SWBM
# spatial files from scratch.
# 1. Soil conductivity for polygons_table.txt update (Feb 2023)
# 2. Diversion point from table for polygons_table.txt update (Feb 2023) (actually, skipping this)
# 3. Ag and Municipal well info
library(RSVP)
library(soilDB)
library(colorspace)
library(sf)
library(httr)


# ------------------------------------------------------------------------------------------------#

#' Calculates and saves spatial precipitation factors.
#'
#' @return Nothing; saves file in SWBM time-independent data folder.
#' @export
#'
#' @examples
#'
#'
#'
#'
#'
#'
calc_and_save_precip_factors=function(){

  # Precip factors

  # Precip factors - spatial variation in rainfall. Field-by-field multiplier of overall rainfall amount.
  precip_factors = calc_swbm_spatial_precip_factors()

  write_swbm_precip_factor_file(output_dir = time_indep_ref, precip_factors_df = precip_factors)

}



# ------------------------------------------------------------------------------------------------#

#' Retrieve SSURGO data and spatially relate the saturated conductivity (max water infiltration
#' rate) to polygons table of SVIHM fields
#'
#' @return Table of SWBM field IDs with associated max water infiltration rate
#' @export
#'
#' @examples
#'
#'
#'
#'
#'
get_polygons_ksat <- function(show_map = F, svihm_fields) {
  #convert micrometers per second (SSURGO units) to meters per day (SVIHM units)
  um_per_sec_to_m_per_day = 1/10^6 * 60*60*24

  # Scott River watershed bounding box in WGS 84 (EPSG 4326)
  scott_huc8_extent = data.frame(xmin = -123.22381, ymin = 41.19597,
                                 xmax = -122.56570, ymax = 41.78506)
  bbox_watershed = sf::st_as_sf(wk::rct(xmin = scott_huc8_extent$xmin,
                                        xmax = scott_huc8_extent$xmax,
                                        ymin = scott_huc8_extent$ymin,
                                        ymax = scott_huc8_extent$ymax,
                                        crs = sf::st_crs(4326)))

  ssurgo.geom <- SDA_spatialQuery(geom = bbox_watershed,
                                  what = 'mupolygon',
                                  db = 'SSURGO',
                                  geomIntersection = TRUE)

  soil_ksat = get_SDA_property(property = "ksat_r",
                               method = c("Weighted Average"),
                               # "Dominant Component (Category)", "Weighted Average", "Min/Max",
                               # "Dominant Component (Numeric)", "Dominant Condition", "None")
                               mukeys = ssurgo.geom$mukey,
                               top_depth = 0,
                               bottom_depth = 200,
  )

  # Attach ksat value to geometry and convert to meters per day
  ssurgo.geom$ksat_um_sec = soil_ksat$ksat_r[match(ssurgo.geom$mukey, soil_ksat$mukey)]
  ssurgo.geom$ksat_m_day = ssurgo.geom$ksat_um_sec * um_per_sec_to_m_per_day

  if(show_map == T){
    # Build order-of-magnitude breaks for plotting
    ksat_range = range(ssurgo.geom$ksat_m_day, na.rm=T)
    breaks_exp = seq(from = floor(log10(ksat_range[1])), to = ceiling(log10(ksat_range[2])))
    ksat_breaks = 10^breaks_exp
    n_classes = length(ksat_breaks)-1
    # Divide the ksat values into categories according to predetermined breaks
    ksat_classifier = cut(x = ssurgo.geom$ksat_m_day, breaks = ksat_breaks, include.lowest = T)
    # Define color palette
    infil_palette = "Purples 3"# "Oslo"
    my_palette = sequential_hcl(n_classes,
                                palette = infil_palette)
    # Plot map of soil properties within the bounding box of the Scott R Watershed
    plot(ssurgo.geom$geom, col = my_palette[ksat_classifier])
    legend_labels = paste(paste(ksat_breaks[1:(n_classes)],
                                ksat_breaks[2:(n_classes+1)], sep = " to "),
                          "m/day")
    legend(x = "bottomleft", legend = legend_labels, fill = my_palette)
  }


  # Spatially relate the ksat values to each field. weighted average.
  # Reproject soils data into EPSG 3310.
  ssurgo_3310 = st_transform(ssurgo.geom, crs = st_crs(x = svihm_fields))
  output_tab = data.frame(field_id = sort(svihm_fields$Polynmbr),
                          ksat_m_day_avg = NA)

  for(i in sort(svihm_fields$Polynmbr)){
    field = svihm_fields[svihm_fields$Polynmbr == i,]
    print(i)
    if(!st_is_valid(field)){
      print(paste("Field ID", i, "is invalid"))
      field = st_buffer(x = field, dist = 0)
      }

    field_ssurgo_intersect = st_intersection(y = field, x = ssurgo_3310)
    # Calculate area-weighted average saturated conductivity for each field
    ksat_m3_per_day = st_area(field_ssurgo_intersect) * field_ssurgo_intersect$ksat_m_day
    remove_these = is.na(ksat_m3_per_day)
    ksat_m3_per_day = ksat_m3_per_day[!remove_these]
    areas = st_area(field_ssurgo_intersect)[!remove_these]
    ksat_m_day_avg = sum(ksat_m3_per_day)/ sum(areas)

    output_tab$ksat_m_day_avg[output_tab$field_id==i] = ksat_m_day_avg

  }

  # Returt dataframe
  return(output_tab)
}



# ------------------------------------------------------------------------------------------------#

#' Assign each SVIHM field polygon a surface water diversion point (SFR segment).
#'
#' @return Table of SWBM field IDs with associated SFR inflow segment.
#' @export
#'
#' @examples
#'
#'
#'
#'
#'
get_polygons_runoff_ISEG <- function(show_map = F, svihm_fields, poly_tab) {

  svihm_fields$subws = poly_tab$subws_ID[match(svihm_fields$Polynmbr, poly_tab$SWBM_id)]

  # Read in SFR routing and tributary inflow segment info
  sfr_routing = read.table(file = file.path(data_dir["time_indep_dir","loc"], "SFR_Routing.txt"),
                           header = T, comment.char = "!")
  sfr_inflows = read.table(file = file.path(data_dir["time_indep_dir","loc"], "SFR_inflow_segments.txt"),
                           header = T, comment.char = "!", sep = "\t")
  # Assign a downstream segment to each inflow segment
  sfr_inflows$outseg = sfr_routing$OUTSEG[match(sfr_inflows$ISEG, sfr_routing$NSEG)]
  # Identify only one downstream segment for each subwatershed (since fields are located within a subws)
  sfr_inflows$outseg[sfr_inflows$subws_name == "Patterson"] = max(sfr_inflows$outseg[sfr_inflows$subws_name == "Patterson"])
  outseg_df = sfr_inflows[,c("subws_ID","outseg")]
  outseg_df = outseg_df[!duplicated(outseg_df),]
  # Assign to svihm_fields
  runoff_iseg_vector = outseg_df$outseg[match(svihm_fields$subws, outseg_df$subws_ID)]
  return(runoff_iseg_vector)

  # Preserving the below code comments for posterity

  # Prior to 4/2023: Currently a placeholder. Not currently worth it to actually assign ISEGs, since
  # diversions get allocated in SWBM based on set fraction of subwatershed demand to each inflow point.

  # Scratch work trying to figure this out:

  # plot(svihm_fields$geometry, col = as.factor(svihm_fields$Trib_2011))

  # Assign subwatershed to old Tributary_2011 field before making modifications
  # svihm_fields$subws = svihm_fields$Trib_2011

  # Difficult to identify the inflow segment for some fields without completely redoing
  # the spatial analysis (and possibly messing with calibration)
  # Also, currently, in medium cases, the surface water diversions get
  # divided among inflow segments on a monthly basis after SW demand is calculated. So
  # the inflow segment assignment happens after the fact, via the subwatershed assignment.


  # Easy cases: Single identified subwatershed, single inflow point
  # easy_subws = c(3, 5, 6, 7, 8) # Etna, Kidder, Moffett, Mill and Shackleford Creeks

  # Medium cases: Single identified subwatershed, multiple inflow points
  # med_subws = c(2, 4) # French (French and Miners inflows) and Patterson (Johnson, Crystal, and Patterson inflows)

  # Hard cases
  # Tailings: subwatershed is "Tailings." Inflow seg is 1 (southernmost point in valley).

  # Scott River: The fields with "Scott River" Trib_2011 info are hard.
  # They get their Sw from the Farmer's Ditch and the SVID Ditch diversions.



  # "Undetermined" subwatershed (in Scott River, French drainages). Keep with no iseg? nah.
  # Special cases: Poly#715 is in French Creek but not labeled as such

  # Scott River and Scott Tailings identified subwatershed
  # 3 diversion points:
  # 1) The confluence of the EF and SF
  # 1b) The confluence of Sugare Creek and Scott River (since these inflows are lumped in old [2018] SWBM)
  # 2) Farmer's ditch diversion (right below Sugar Creek)
  # 3) SVID ditch diversion (at Young's Dam, about 1/3 of the way between French and Etna confluences)
  # Currently, the ditch diversions are modeled *independent* of agricultural demand. We just assume a constant
  # diversion and leakage rate. And any extra water at the end of the ditches... disappears? Right? No, flows back into
  # the Scott River?


  # Motherfucker. What the fuck is in that polygons_table.txt subwatershed field?
  # match poly_tab$subws to spatial polygons
  # color code those fuckers
  # Oh I see. All the "Undetermined" polygons got attached to the East Fork, South Fork, Sugar creek combined inflow.
  # Undetermined = subws 9 in poly_tab.

  # plot(svihm_fields$geometry, col = svihm_fields$subws)



  # So ALL surface water diversions from the entire river corridor are taken from the tailings
  # inflow segment?? AYFKM


  #So, what, in this case we have all fields assigned to a subwatershed. And then the division of SW diversions
  # into inflow point extractions happens after the fact. Yes? Right? So actually specifying iseg for each
  # field would be unnecessary.
  # So, we're not going to do this. screw this exercise.


  # scratchwork
  # poly_tab$subws[poly_tab$Trib_2011=="Undetermined"] = 1 # Assign to Scott Tailings subws
  # poly_tab$subws[poly_tab$X.ID==715] = 2 # All but this Undetermined field; assign this field to French subws (from QGIS visualizing)
  # Currently, several Tailings-area fields are assigned to Scott River:
  # c(1515:1518, 1520,1521,1528,1529)

  # return(iseg_by_field)

}




# ------------------------------------------------------------------------------------------------#

#' Reads in current version of polygons_table_ref.csv and updates for new (2022) SWBM version.
#'
#' @return Nothing; saves file in SWBM time_indep_dir folder.
#' @export
#'
#' @examples
#'
#'
#'
#'
#'

save_updated_polygons_table_txt = function(){
  # Update polygons_table.txt (input file of SVIHM fields info table)

  # Read in reference csv of poly_tab
  poly_tab = read.csv(file.path(data_dir["ref_data_dir","loc"],"polygons_table_ref.csv"),
                      header = T)

  # If this is the old (2018) version of polytab, rename columns to match new (2022) SWBM version
  # and attach ksat soil property
  if(!is.element(el="max_infil_rate", colnames(poly_tab))){
    colnames(poly_tab) = c("SWBM_id","subws_ID","SWBM_LU","SWBM_IRR","MF_Area_m2","WATERSOURC",
                           "AWCcmprcm","Initial_fill_fraction","WL_2_CP_Yr","ILR_Flag","Notes")

    # add information for two extra columns, new as of 2022

    # 1) Assign infiltration rate to polytab
    # Read in fields spatial file for spatial relation
    svihm_fields = read_sf(dsn = file.path(data_dir["ref_data_dir","loc"],"Landuse_20190219.shp"))
    # BEWARE: this takes like 10 minutes to compute intersections of all 2119 polygons. (Is sf slow?)
    infil_tab = get_polygons_ksat(svihm_fields = svihm_fields) # retrieve k_sat, max. infiltration rate

    # Attach infil rates to spatial data
    svihm_fields$max_infil_rate = infil_tab$ksat_m_day[match(svihm_fields$Polynmbr, infil_tab$field_id)]
    # plot(svihm_fields[,"max_infil_rate"])
    # Sort in order
    svihm_fields = svihm_fields[order(svihm_fields$Polynmbr),]
    # Fill in NA values
    # Step 1: try to fill in values with mean of neighboring infiltration rate
    na_field_ids = svihm_fields$Polynmbr[is.na(svihm_fields$max_infil_rate)]
    for (i in na_field_ids){
      field = svihm_fields[i,]
      # plot(field$geometry)
      # plot(svihm_fields, add=T)

      # plot(svihm_fields$geometry)
      # plot(field$geometry, add=T, col = "red", border = "red", lwd = 4)

      neighbor_ids = st_intersects(x = field, y = svihm_fields)[[1]]
      neighbor_ids = neighbor_ids[neighbor_ids != i] # get rid of target field ID

      neighbor_ksats = svihm_fields$max_infil_rate[neighbor_ids]
      # print(paste("Field ID",i))
      # print(paste("Num. neighbors:", length(neighbor_ids)))
      # print(paste("Neighbor ksats:",
      #             paste(svihm_fields$max_infil_rate[neighbor_ids], collapse = ", ")))

      # fill in missing ksat value
      svihm_fields$max_infil_rate[svihm_fields$Polynmbr==i] =
        mean(svihm_fields$max_infil_rate[neighbor_ids], na.rm=T)
    }
    # Step 2: for fields with no neighbors with infiltration rates, use bigger radius
    # sum(is.na(svihm_fields$max_infil_rate)) # how many left?
    na_field_ids = svihm_fields$Polynmbr[is.na(svihm_fields$max_infil_rate)]

    for (i in na_field_ids){
      field = svihm_fields[i,]
      for(j in 100*c(1, 2, 3, 4, 5, 6, 7, 8)){
        field_buffer = st_buffer(x = field, dist = j)
        neighbors = st_intersects(field_buffer, svihm_fields)[[1]]
        neighbor_ids = neighbor_ids[neighbor_ids != i] # get rid of target field ID
        neighbor_ksats = svihm_fields$max_infil_rate[neighbor_ids]

        if(sum(is.na(neighbor_ksats)) != length(neighbor_ksats)){
          # fill in missing ksat value
          svihm_fields$max_infil_rate[svihm_fields$Polynmbr==i] =
            mean(svihm_fields$max_infil_rate[neighbor_ids], na.rm=T)
          break
        } else {
          next
        }
      }
    }

    # plot(svihm_fields[,"max_infil_rate"])

    # attach infil rate to poly_tab
    poly_tab$max_infil_rate = svihm_fields$max_infil_rate[match(poly_tab$SWBM_id, svihm_fields$Polynmbr)]

    # Reorder columns to match new (2022) format
    poly_tab = poly_tab[,c("SWBM_id","subws_ID","SWBM_LU","SWBM_IRR","MF_Area_m2","WATERSOURC",
                           "AWCcmprcm","Initial_fill_fraction","max_infil_rate","runoff_ISEG",
                           "WL_2_CP_Yr","ILR_Flag", "Notes")]

  }

  # Revise the subwatershed IDs to match new SFR setup (Scott River and Scott Tailings now one subws?)
  # Reassign subwatershed 9 (Scott River) to Subwatershed 1 (Scott Tailings), since they draw from the same inflow source
  poly_tab$subws_ID[poly_tab$subws_ID == 9] = 1

  # Assign runoff_ISEG to the segment downstream of each respective subwatershed
  if(!exists("svihm_fields")){svihm_fields = read_sf(dsn = file.path(data_dir["ref_data_dir","loc"],"Landuse_20190219.shp"))}
  poly_tab$runoff_ISEG = get_polygons_runoff_ISEG(svihm_fields=svihm_fields, poly_tab=poly_tab)

  # Convert the ILR Flag 1s and 0s (in the 2018 version) to logicals
  poly_tab$ILR_Flag=as.logical(poly_tab$ILR_Flag)
  # Assign 0-infiltration for polygons with no SSURGO info
  poly_tab$max_infil_rate[is.na(poly_tab$max_infil_rate)] = 0
  # Take out notes column for storage as .txt file
  poly_tab$Notes=NULL

  # Rewrite landuse types based on new landcover_table.txt file. Alfalfa now = 1, grain = 2.
  # set everything that is 25 to alfalfa I guess.
  # still not sure how this is going to work with the fucking rotation though! (as of 2/24/2023)
  landcover_tab = read.table(file.path(data_dir["time_indep_dir","loc"],"landcover_table.txt"),
                               comment.char = "!",
                               sep = "\t", header = T)

  lu_2018 = c(25,25,2,3,4,6) # old categories in 2018 model. Possible update? # lu = c(1,2,3,4,5,6), add Grain as explit new category
  lu_descrip = c("Alfalfa","Grain","Pasture","ET_noIrr","noET_noIrr", "Water")
  lu_color = c("forestgreen","forestgreen","darkolivegreen2","wheat","red","dodgerblue")
  lu_2022 = c(1,2,3,4,5,6)
  lu_df = data.frame(lu_code = lu_2018, lu_descrip = lu_descrip, color = lu_color, lu_code_22 = lu_2022)

  SWBM_LU_new = lu_df$lu_code_22[match(poly_tab$SWBM_LU, lu_df$lu_code)]

  poly_tab$SWBM_LU = SWBM_LU_new

  # revise water holding capacity to be per meter, instead of per 8-feet (2.4384 m)
  poly_tab$AWCcmprcm = poly_tab$AWCcmprcm / (8 / 3.28084)

  # Save polygons_table.txt in the time_indep_dir, excluding notes column.
  write.table(poly_tab, file = file.path(data_dir["time_indep_dir","loc"],"polygons_table.txt"),
              quote=F, col.names = T, sep = "\t", row.names=F)

}


# ------------------------------------------------------------------------------------------------#

#' Reads in 2018 version of well_list_by_polygon.txt (called well_list_by_polygon_ref.txt) and saves as
#' ag_well_list_by_polygon.txt for new (2022) SWBM version.
#'
#' @return Nothing; saves file in SWBM time_indep_dir folder.
#' @export
#'
#' @examples
#'
#'
#'
#'
#'


save_updated_wells_by_poly = function(){
  # Previous version of this file only contained wells info for the 1254 fields
  # in which the land use was alfalfa/grain or pasture (i.e. irrigated crops).
  # Does not allow for expanding or changing groundwater-irrigated acreage
  # is land use scenarios.
  # So, was necessary to re-do the spatial relationship between fields and
  # irrigation wells to list the closest well to each field.

  # Well-polygon matching table (old version)
  wells_by_poly = read.table(file.path(data_dir["ref_data_dir","loc"],"well_list_by_polygon_ref.txt"),
                                header = T, comment.char = "!")
  # Well summary tab, containing location data for all irrigation wells in the model
  well_summary = read.table(file.path(data_dir["time_indep_dir","loc"],"ag_well_summary.txt"),
                                header = T)
  # Read in fields spatial file for spatial relation
  svihm_fields = read_sf(dsn = file.path(data_dir["ref_data_dir","loc"],"Landuse_20190219.shp"))
  # Convert well_summary tab location info into spatial object
  irr_wells = st_as_sf(x = well_summary, coords = c("UTM_E","UTM_N"), crs = sf::st_crs(32610))
  irr_wells = st_transform(x = irr_wells, crs = st_crs(svihm_fields))

  # calculate centroids of SVIHM fields
  fields_centroid = st_centroid(svihm_fields)
  # Find nearest well neighbor to each field centroid
  fields_centroid$nearest_well_id_centroid = irr_wells$well_id[st_nearest_feature(x = fields_centroid,
                                                                      y = irr_wells)]
  # extract two colums for well-field matching table
  wells_by_poly_tab = data.frame(SWBM_id = fields_centroid$Polynmbr,
                                 well_id = fields_centroid$nearest_well_id_centroid)
  # save in order of polygon number
  wells_by_poly_tab = wells_by_poly_tab[order(wells_by_poly_tab$SWBM_id),]
  # Write updated table
  write.table(wells_by_poly_tab,
              file = file.path(data_dir["time_indep_dir","loc"],"ag_well_list_by_polygon.txt"),
              quote=F, col.names = T, sep = "\t", row.names=F)

}


# ------------------------------------------------------------------------------------------------#

#' Reads in 2018 version of well_summary.txt (called well_summary_ref.txt) and saves as
#' ag_well_summary.txt for new (2022) SWBM version.
#'
#' @return Nothing; saves file in SWBM time_indep_dir folder.
#' @export
#'
#' @examples
#'
#'
#'
#'
#'


save_ag_well_summary_tab = function(){
  # well info stored for MODFLOW
  well_summary_old = read.table(file.path(data_dir["ref_data_dir","loc"],"well_summary_ref.txt"),
                            header = T)

  ag_well_summary_tab = data.frame(well_id = well_summary_old$LogNumber,
                                   well_name = NA,
                                   layer = well_summary_old$Layer_2017,
                                   top_scrn_z = NA,
                                   bot_scrn_z = NA,
                                   row_MF = well_summary_old$row,
                                   col_MF = well_summary_old$column,
                                   UTM_E = well_summary_old$UTM_E,
                                   UTM_N = well_summary_old$UTM_N)


  # Find the well location info for the 5 DWR wells. mofo

  # Read in spatial discretization file
  dis_lines = readLines(file.path(data_dir["ref_data_dir","loc"],"SVIHM_dis_reference.txt"))

  #Extract number of rows and columns in the modflow grid
  topline = unlist(strsplit(trimws(dis_lines[2]), "  "))
  n_row = as.numeric(topline)[2]; n_col = as.numeric(topline)[3]

  #Isolate elevation of the bottom of layer 1 and the bottom of layer 2.
  lay1_top_start = grep(pattern = "TOP", x = dis_lines) + 1
  lay1_top_end = grep(pattern = "Layer", x = dis_lines)[1] - 1
  lay1_bot_start = grep(pattern = "Layer", x = dis_lines)[1] + 1
  lay1_bot_end = grep(pattern = "Layer", x = dis_lines)[2] - 1
  lay2_bot_start = grep(pattern = "Layer", x = dis_lines)[2] + 1
  lay2_bot_end = grep(pattern = "TR", x = dis_lines)[1] - 1

  lay1_top_vals = as.numeric(unlist(strsplit(trimws(dis_lines[lay1_top_start:lay1_top_end]), split = "  ")))
  lay1_bot_vals = as.numeric(unlist(strsplit(trimws(dis_lines[lay1_bot_start:lay1_bot_end]), split = "  ")))
  lay2_bot_vals = as.numeric(unlist(strsplit(trimws(dis_lines[lay2_bot_start:lay2_bot_end]), split = "  ")))

  # Convert values to matrix, matching coordinates
  lay1_top = matrix(data = lay1_top_vals, nrow = n_row, ncol = n_col, byrow = T)
  lay1_bot = matrix(data = lay1_bot_vals, nrow = n_row, ncol = n_col, byrow = T)
  lay2_bot = matrix(data = lay2_bot_vals, nrow = n_row, ncol = n_col, byrow = T)
  # Assign top_scrn_z and bot_scrn_z to top and bottom of aquifer layer 2 at designated row,col
  for(i in 1:length(ag_well_summary_tab$well_id)){
    well_i = ag_well_summary_tab$well_id[i]
    row_i = ag_well_summary_tab$row_MF[i]
    col_i = ag_well_summary_tab$col_MF[i]
    layer_i = well_summary_old$Layer_2017[well_summary_old$LogNumber==well_i]
    if(layer_i==1){
      # Top of screen assigned to top of Layer 1
      ag_well_summary_tab$top_scrn_z[i] = lay1_top[row_i, col_i]
      # Bottom of screen assigned to bottom of Layer 1
      ag_well_summary_tab$bot_scrn_z[i] = lay1_bot[row_i, col_i]
    }
    if(layer_i==2){
      # Top of screen assigned to bottom of Layer 1 (top of Layer 2)
      ag_well_summary_tab$top_scrn_z[i] = lay1_bot[row_i, col_i]
      # Bottom of screen assigned to bottom of Layer 2
      ag_well_summary_tab$bot_scrn_z[i] = lay2_bot[row_i, col_i]
    }
  }

  write.table(ag_well_summary_tab,
              file = file.path(data_dir["time_indep_dir","loc"],"ag_well_summary.txt"),
              quote=F, col.names = T, sep = "\t", row.names=F)

}


# ------------------------------------------------------------------------------------------------#

#' Reads in 2018 version of polygons_table.txt (called polygons_table_ref.csv) and saves as
#' polygons_table.txt for new (2022) SWBM version.
#'
#' @return Nothing; saves file in SWBM time_indep_dir folder.
#' @export
#'
#' @examples
#'
#'
#'
#'

save_updated_fields_shapefile = function(){
  # Read in reference csv of poly_tab
  poly_tab = read.csv(file.path(data_dir["ref_data_dir","loc"],"polygons_table_ref.csv"),
                      header = T)
  # Read in fields spatial file for spatial relation
  svihm_fields = read_sf(dsn = file.path(data_dir["ref_data_dir","loc"],"Landuse_20190219.shp"))

  poly2 = svihm_fields
  poly2$ws18code = poly_tab$WATERSOURC[match(poly2$Polynmbr, poly_tab$SWBM_id)]
  poly2$lu18code = poly_tab$SWBM_LU[match(poly2$Polynmbr, poly_tab$SWBM_id)]

  # save landuse categories
  lu = c(25,2,3,4,6) # old categories in 2018 model. Possible update? # lu = c(1,2,3,4,5,6), add Grain as explit new category
  lu_descrip = c("Alfalfa","Pasture","ET_noIrr","noET_noIrr", "Water")
  lu_color = c("forestgreen","darkolivegreen2","wheat","red","dodgerblue")
  lu_df = data.frame(lu_code = lu, lu_descrip = lu_descrip, color = lu_color)

  wat_source = c(1,2,3,4,5,999)
  wat_source_descrip = c("SW","GW","Mixed", "Sub-irrigated","Dry","Unknown")
  wat_source_color = c("dodgerblue","firebrick2","darkorchid1","green","yellow","gray")

  wat_source_df = data.frame(ws_code = wat_source,
                             descrip = wat_source_descrip,
                             color = wat_source_color)

  poly2$lu_18 = lu_df$lu_descrip[match(poly2$lu18code, lu_df$lu_code)]
  poly2$ws_18 = wat_source_df$descrip[match(poly2$ws18code,
                                                       wat_source_df$ws_code)]

  st_write(obj = poly2,
           dsn = file.path(data_dir["ref_data_dir","loc"]),
           layer = "Landuse_20190219_updated2023",
           driver = "ESRI Shapefile", append=F)

  # check to see if they abbreviated the column names
  # test = st_read(dsn = file.path(data_dir["ref_data_dir","loc"],"Landuse_20190219_updated2023.shp"))
}


# ------------------------------------------------------------------------------------------------#

#' Downloads Siskiyou County parcels, clips to Scott River HUC8 watershed, and saves as Scott
#' Valley parcels shapefile in reference data directory.
#'
#' @return Nothing; saves file in ref_data_dir folder.
#' @export
#'
#' @examples
#'
#'
#'
#'

save_scott_parcels_shapefile = function(){

  # Assessors' Parcels
  # Accessed from https://www.co.siskiyou.ca.us/planning/webform/gis-data-download
  # par_url = "https://www.co.siskiyou.ca.us/sites/default/files/fileattachments/community_development/page/2461/siskiyouparcelsnov2017.zip"
  # par_dl = GET(par_url, write_disk(file.path(data_dir["ref_data_dir", "loc"], zipname), overwrite = TRUE))

  # NEW 2023: accessed from https://open-data-siskiyou.hub.arcgis.com/datasets/siskiyou::tax-parcels/explore
  # Download manually and rename files Siskiyou_Parcels_Public.zip

  zipname = "Siskiyou_Parcels_Public.zip" #strsplit(par_url, "/")[[1]][length(strsplit(par_url, "/")[[1]])]
  ref_dir = data_dir["ref_data_dir", "loc"]

  # Unzip file and save in the working directory (defaults to Documents folder)
  unzip(file.path(ref_dir, zipname), exdir = ref_dir) #, list = TRUE) # just lists files, does not unzip
  #Read shapefile into R
  parcels_all = st_read(file.path(ref_dir, "Siskiyou_Parcels_Public.shp"))

  scott_huc8_extent = data.frame(xmin = -123.22381, ymin = 41.19597,
                                 xmax = -122.56570, ymax = 41.78506)

  sf_use_s2(FALSE) # workaround for a stupid bug. rough crop
  parcels_scott = st_crop(x = parcels_all, xmin = scott_huc8_extent$xmin,
                          xmax = scott_huc8_extent$xmax,
                          ymin = scott_huc8_extent$ymin,
                          ymax = scott_huc8_extent$ymax)
  parcels_scott = st_transform(x = parcels_scott, crs = st_crs(3310))

  # plot(parcels_scott$geometry)

  st_write(parcels_scott, dsn = ref_dir, layer = "Scott_HUC8_Parcels_Public.shp",
           driver = "ESRI Shapefile")

  #remove unzipped files from scratch drive
  extension_list = c("cpg", "dbf", "prj", "shx", "shp", "xml", "sbn", "sbx", "shp.xml")
  file.remove(file.path(ref_dir,paste("Siskiyou_Parcels_Public", extension_list, sep = ".")))

}

# ------------------------------------------------------------------------------------------------#

#' Checks for consistency between land use types in polygons_table.txt input file
#' in 2018 and 2022 SWBM versions.
#'
#' @return Nothing
#' @export
#'
#' @examples
#'
#'
#'
#'

check_fields_match = function(){
  m2_to_acres = 1/4046.86

  dir_2023 = file.path(data_dir["svihm_dir","loc"],"Run", "SWBM")
  dir_2018 = "C:/Users/Claire/Documents/GitHub/SVIHM/Scenarios/basecase"
  # Read in reference csv of poly_tab
  poly_tab_23 = read.table(file.path(dir_2023,"polygons_table.txt"),
                      header = T)
  poly_tab_18 = read.table(file.path(dir_2018, "polygons_table.txt"),
                           header = T, comment = "!", fill=T)

  #check that water sources and irrigation are identical. (yep)
  poly_tab_23$wat_src_18 = poly_tab_18$Water_Source[match(poly_tab_23$SWBM_id, poly_tab_18$X.ID)]
  sum(poly_tab_23$WATERSOURC == poly_tab_23$wat_src_18)
  poly_tab_23$irr_18 = poly_tab_18$Irrigation[match(poly_tab_23$SWBM_id, poly_tab_18$X.ID)]
  sum(poly_tab_23$SWBM_IRR == poly_tab_23$irr_18)
  # acreage of different water sources
  sum(poly_tab_23$MF_Area_m2[poly_tab_23$WATERSOURC==1]) * m2_to_acres # 6484 acres under SW
  sum(poly_tab_23$MF_Area_m2[poly_tab_23$WATERSOURC==2]) * m2_to_acres # 16540 acres under GW
  sum(poly_tab_23$MF_Area_m2[poly_tab_23$WATERSOURC==3]) * m2_to_acres # 5084 acres under mixed SW-GW
  sum(poly_tab_23$MF_Area_m2[poly_tab_23$WATERSOURC==4]) * m2_to_acres # 2106 acres subirrigated
  sum(poly_tab_23$MF_Area_m2[poly_tab_23$WATERSOURC==4]) * m2_to_acres # 2106 acres subirrigated



  # Read in fields spatial file for spatial relation
  svihm_fields = read_sf(dsn = file.path(data_dir["ref_data_dir","loc"],"Landuse_20190219.shp"))

  poly2 = svihm_fields

  # Assign poly_tab land use attributes to shapefile
  poly2$lu_2023 = poly_tab_23$SWBM_LU[match(poly2$Polynmbr, poly_tab_23$SWBM_id)]
  poly2$lu_2018 = poly_tab_18$SWBM_LU[match(poly2$Polynmbr, poly_tab_18$SWBM_id)]

  #translate land use codes
  lu_2018 = c(25,25,2,3,4,6) # old categories in 2018 model. Possible update? # lu = c(1,2,3,4,5,6), add Grain as explit new category
  lu_descrip = c("Alfalfa","Grain","Pasture","ET_noIrr","noET_noIrr", "Water")
  lu_color = c("forestgreen","forestgreen","darkolivegreen2","wheat","red","dodgerblue")
  lu_2022 = c(1,2,3,4,5,6)
  lu_df = data.frame(lu_code = lu_2018, lu_descrip = lu_descrip, color = lu_color, lu_code_22 = lu_2022)

  poly2$lu_2018_translated = lu_df$lu_code_22[match(poly2$lu_2018, lu_df$lu_code)]

  sum(poly2$lu_2018_translated==poly2$lu_2023)/nrow(poly2)*100
  # As of 4/19/2023, answer is 100%
  # OK, great. the land uses all fucking match. why is the recharge so different.

  # assign poly_tab water source attributes to shapefile
  poly2$ws_2023 = poly_tab_23$WATERSOURC[match(poly2$Polynmbr, poly_tab_23$SWBM_id)]
  poly2$ws_2018 = poly_tab_18$WATERSOURC[match(poly2$Polynmbr, poly_tab_18$SWBM_id)]
  sum(poly2$ws_2023==poly2$ws_2018)/nrow(poly2)*100
  # and the water sources all seem to match

  # assign poly_tab water source attributes to shapefile
  poly2$irr_2023 = poly_tab_23$SWBM_IRR[match(poly2$Polynmbr, poly_tab_23$SWBM_id)]
  poly2$irr_2018 = poly_tab_18$SWBM_IRR[match(poly2$Polynmbr, poly_tab_18$SWBM_id)]
  sum(poly2$irr_2023==poly2$irr_2018)/nrow(poly2)*100
  # and the irrigation tech all seem to match

}

# ------------------------------------------------------------------------------------------------#

#' Checks for consistency between input files in 2018 and 2022 SWBM versions.
#'
#' @return Nothing
#' @export
#'
#' @examples
#'
#'
#'
#'

check_climate_inputs_match = function(){
  dir_2023 = file.path(data_dir["svihm_dir","loc"],"Run", "SWBM")
  dir_2018 = "C:/Users/Claire/Documents/GitHub/SVIHM/Scenarios/basecase"

  # ET
  et_23 = read.table(file.path(dir_2023,"ref_et.txt"), header = T)
  et_18 = read.table(file.path(dir_2018,"ref_et.txt"), header = F)
  colnames(et_18) = colnames(et_23)
  #truncate et_23
  et_23 = et_23[1:nrow(et_18),]
  summary(et_23$ETo_m - et_18$ETo_m)
  sum(!(et_23$ETo_m - et_18$ETo_m), na.rm=T)

  hist(log(et_23$ETo_m - et_18$ETo_m))
  plot(x = as.Date(et_23$Date, format = "%d/%m/%Y"), y = (et_23$ETo_m - et_18$ETo_m))
  # Precip
  ppt_23 = read.table(file.path(dir_2023,"precip.txt"), header = T)
  ppt_18 = read.table(file.path(dir_2018,"precip.txt"), header = F)
  colnames(ppt_18) = colnames(ppt_23)
  #truncate
  ppt_23 = ppt_23[1:nrow(ppt_18),]
  plot(x = as.Date(ppt_23$Date, format = "%d/%m/%Y"),
       y = (ppt_23$PRCP - ppt_18$PRCP))


  # streams?
  str_23 = read.table(file.path(dir_2023,"streamflow_records_regressed.txt"), header = T)
  str_18 = read.table(file.path(dir_2018,"streamflow_input.txt"), header = T)
  str_23 = str_23[1:nrow(str_18),]


  # # Read in fields spatial file for spatial relation
  # svihm_fields = read_sf(dsn = file.path(data_dir["ref_data_dir","loc"],"Landuse_20190219.shp"))
  #
  # poly2 = svihm_fields
  #
  # # Assign poly_tab land use attributes to shapefile
  # poly2$lu_2023 = poly_tab_23$SWBM_LU[match(poly2$Polynmbr, poly_tab_23$SWBM_id)]
  # poly2$lu_2018 = poly_tab_18$SWBM_LU[match(poly2$Polynmbr, poly_tab_18$SWBM_id)]
  #
  # #translate land use codes
  # lu_2018 = c(25,25,2,3,4,6) # old categories in 2018 model. Possible update? # lu = c(1,2,3,4,5,6), add Grain as explit new category
  # lu_descrip = c("Alfalfa","Grain","Pasture","ET_noIrr","noET_noIrr", "Water")
  # lu_color = c("forestgreen","forestgreen","darkolivegreen2","wheat","red","dodgerblue")
  # lu_2022 = c(1,2,3,4,5,6)
  # lu_df = data.frame(lu_code = lu_2018, lu_descrip = lu_descrip, color = lu_color, lu_code_22 = lu_2022)
  #
  # poly2$lu_2018_translated = lu_df$lu_code_22[match(poly2$lu_2018, lu_df$lu_code)]
  #
  # sum(poly2$lu_2018_translated==poly2$lu_2023)/nrow(poly2)*100
  # # As of 4/19/2023, answer is 100%
  # # OK, great. the land uses all fucking match. why is the recharge so different.
  #
  # # assign poly_tab water source attributes to shapefile
  # poly2$ws_2023 = poly_tab_23$WATERSOURC[match(poly2$Polynmbr, poly_tab_23$SWBM_id)]
  # poly2$ws_2018 = poly_tab_18$WATERSOURC[match(poly2$Polynmbr, poly_tab_18$SWBM_id)]
  # sum(poly2$ws_2023==poly2$ws_2018)/nrow(poly2)*100
  # # and the water sources all seem to match
  #
  # # assign poly_tab water source attributes to shapefile
  # poly2$irr_2023 = poly_tab_23$SWBM_IRR[match(poly2$Polynmbr, poly_tab_23$SWBM_id)]
  # poly2$irr_2018 = poly_tab_18$SWBM_IRR[match(poly2$Polynmbr, poly_tab_18$SWBM_id)]
  # sum(poly2$irr_2023==poly2$irr_2018)/nrow(poly2)*100
  # # and the irrigation tech all seem to match

}

# ------------------------------------------------------------------------------------------------#

#' Checks for consistency between land use types in polygons_table.txt input file
#' in 2018 and 2022 SWBM versions.
#'
#' @return Nothing
#' @export
#'
#' @examples
#'
#'
#'
#'

update_polygons_ksat_diagnostic = function(){

  dir_2023 = file.path(data_dir["svihm_dir","loc"],"Run", "SWBM")
  # Read in reference csv of poly_tab
  poly_tab = read.table(file.path(dir_2023,"polygons_table.txt"),
                           header = T)
  # Set all max infiltrtion rates to be insanely high
  hist(poly_tab$max_infil_rate)
  summary(poly_tab$max_infil_rate)
  # Damn. they already are insanely high. Lots of >1 m per day, I think

  # Read in fields spatial file for spatial relation
  svihm_fields = read_sf(dsn = file.path(data_dir["ref_data_dir","loc"],"Landuse_20190219.shp"))

  poly2 = svihm_fields

  # Assign poly_tab land use attributes to shapefile
  poly2$lu_2023 = poly_tab_23$SWBM_LU[match(poly2$Polynmbr, poly_tab_23$SWBM_id)]
  poly2$lu_2018 = poly_tab_18$SWBM_LU[match(poly2$Polynmbr, poly_tab_18$SWBM_id)]

  #translate land use codes
  lu_2018 = c(25,25,2,3,4,6) # old categories in 2018 model. Possible update? # lu = c(1,2,3,4,5,6), add Grain as explit new category
  lu_descrip = c("Alfalfa","Grain","Pasture","ET_noIrr","noET_noIrr", "Water")
  lu_color = c("forestgreen","forestgreen","darkolivegreen2","wheat","red","dodgerblue")
  lu_2022 = c(1,2,3,4,5,6)
  lu_df = data.frame(lu_code = lu_2018, lu_descrip = lu_descrip, color = lu_color, lu_code_22 = lu_2022)

  poly2$lu_2018_translated = lu_df$lu_code_22[match(poly2$lu_2018, lu_df$lu_code)]

  sum(poly2$lu_2018_translated==poly2$lu_2023)/nrow(poly2)*100
  # As of 4/19/2023, answer is 100%
  # OK, great. the land uses all fucking match. why is the recharge so different.

  # assign poly_tab water source attributes to shapefile
  poly2$ws_2023 = poly_tab_23$WATERSOURC[match(poly2$Polynmbr, poly_tab_23$SWBM_id)]
  poly2$ws_2018 = poly_tab_18$WATERSOURC[match(poly2$Polynmbr, poly_tab_18$SWBM_id)]
  sum(poly2$ws_2023==poly2$ws_2018)/nrow(poly2)*100
  # and the water sources all seem to match

  # assign poly_tab water source attributes to shapefile
  poly2$irr_2023 = poly_tab_23$SWBM_IRR[match(poly2$Polynmbr, poly_tab_23$SWBM_id)]
  poly2$irr_2018 = poly_tab_18$SWBM_IRR[match(poly2$Polynmbr, poly_tab_18$SWBM_id)]
  sum(poly2$irr_2023==poly2$irr_2018)/nrow(poly2)*100
  # and the irrigation tech all seem to match

}




#' # ------------------------------------------------------------------------------------------------#
#'
#' #' Reads in historical curtailments data and translates into an input file for SWBM.
#' #' Saves as curtailment_fractions.txt.
#' #'
#' #' @return Nothing; saves file in SWBM time_indep_ folder.
#' #' @export
#' #'
#' #' @examples
#' #'
#' #'
#' #'
#' #'
#' #'
#'
#'
#' write_historical_curtailments_table = function(){
#'   # Read in curtailments data for growing season 2022
#'   curtail_record_2022 = read.table(file.path(data_dir["ref_data_dir","loc"],
#'                                     "LCS Tracking_2022 growing season_anonymized for SVIHM.txt"),
#'                                 header = T, sep = "\t")
#'
#'
#'   # well info stored for MODFLOW
#'   well_summary_old = read.table(file.path(data_dir["ref_data_dir","loc"],"well_summary_ref.txt"),
#'                                 header = T)
#'
#'   ag_well_summary_tab = data.frame(well_id = well_summary_old$LogNumber,
#'                                    well_name = NA,
#'                                    top_scrn_z = NA,
#'                                    bot_scrn_z = NA,
#'                                    row_MF = well_summary_old$row,
#'                                    col_MF = well_summary_old$column,
#'                                    UTM_E = well_summary_old$UTM_E,
#'                                    UTM_N = well_summary_old$UTM_N)
#'
#'
#'   # Find the well location info for the 5 DWR wells. mofo
#'
#'   # Read in spatial discretization file
#'   dis_lines = readLines(file.path(data_dir["ref_data_dir","loc"],"SVIHM_dis_reference.txt"))
#'
#'   #Extract number of rows and columns in the modflow grid
#'   topline = unlist(strsplit(trimws(dis_lines[2]), "  "))
#'   n_row = as.numeric(topline)[2]; n_col = as.numeric(topline)[3]
#'
#'   #Isolate elevation of the bottom of layer 1 and the bottom of layer 2.
#'   lay1_top_start = grep(pattern = "TOP", x = dis_lines) + 1
#'   lay1_top_end = grep(pattern = "Layer", x = dis_lines)[1] - 1
#'   lay1_bot_start = grep(pattern = "Layer", x = dis_lines)[1] + 1
#'   lay1_bot_end = grep(pattern = "Layer", x = dis_lines)[2] - 1
#'   lay2_bot_start = grep(pattern = "Layer", x = dis_lines)[2] + 1
#'   lay2_bot_end = grep(pattern = "TR", x = dis_lines)[1] - 1
#'
#'   lay1_top_vals = as.numeric(unlist(strsplit(trimws(dis_lines[lay1_top_start:lay1_top_end]), split = "  ")))
#'   lay1_bot_vals = as.numeric(unlist(strsplit(trimws(dis_lines[lay1_bot_start:lay1_bot_end]), split = "  ")))
#'   lay2_bot_vals = as.numeric(unlist(strsplit(trimws(dis_lines[lay2_bot_start:lay2_bot_end]), split = "  ")))
#'
#'   # Convert values to matrix, matching coordinates
#'   lay1_top = matrix(data = lay1_top_vals, nrow = n_row, ncol = n_col, byrow = T)
#'   lay1_bot = matrix(data = lay1_bot_vals, nrow = n_row, ncol = n_col, byrow = T)
#'   lay2_bot = matrix(data = lay2_bot_vals, nrow = n_row, ncol = n_col, byrow = T)
#'   # Assign top_scrn_z and bot_scrn_z to top and bottom of aquifer layer 2 at designated row,col
#'   for(i in 1:length(ag_well_summary_tab$well_id)){
#'     well_i = ag_well_summary_tab$well_id[i]
#'     row_i = ag_well_summary_tab$row_MF[i]
#'     col_i = ag_well_summary_tab$col_MF[i]
#'     layer_i = well_summary_old$Layer_2017[well_summary_old$LogNumber==well_i]
#'     if(layer_i==1){
#'       # Top of screen assigned to top of Layer 1
#'       ag_well_summary_tab$top_scrn_z[i] = lay1_top[row_i, col_i]
#'       # Bottom of screen assigned to bottom of Layer 1
#'       ag_well_summary_tab$bot_scrn_z[i] = lay1_bot[row_i, col_i]
#'     }
#'     if(layer_i==2){
#'       # Top of screen assigned to bottom of Layer 1 (top of Layer 2)
#'       ag_well_summary_tab$top_scrn_z[i] = lay1_bot[row_i, col_i]
#'       # Bottom of screen assigned to bottom of Layer 2
#'       ag_well_summary_tab$bot_scrn_z[i] = lay2_bot[row_i, col_i]
#'     }
#'   }
#'
#'   write.table(ag_well_summary_tab,
#'               file = file.path(data_dir["time_indep_dir","loc"],"ag_well_summary.txt"),
#'               quote=F, col.names = T, sep = "\t", row.names=F)
#'
#' }
#'
#'
