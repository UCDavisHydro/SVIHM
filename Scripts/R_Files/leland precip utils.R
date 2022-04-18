# NOAA Data pull for Leland/Scott Valley 2022.03.02


# USGS Watersheds, waterbodies, and streams -------------------------------
# Accessed at http://prd-tnm.s3-website-us-west-2.amazonaws.com/?prefix=StagedProducts/Hydrography/NHD/HU8/HighResolution/Shape/
# Which was accessed via https://www.usgs.gov/core-science-systems/ngp/national-hydrography/access-national-hydrography-products
HUC8_nums = c(18010208, 18010207, 18010205) # scott, shasta, butte
valley_names = c("scott", "shasta", "butte")
huc_table = data.frame(huc8 = HUC8_nums, basin = valley_names)

#Download the zipped files for each watershed, unzip, load into R, write to database (overwriting for each new basin)
for(i in 1:3){
  huc = huc_table$huc8[i]
  basin = huc_table$basin[i]
  
  wsh_url = paste0("https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/HU8/HighResolution/Shape/NHD_H_",huc,"_HU8_Shape.zip")
  zipname = paste0("NHD_H_",huc,"_HU8_Shape.zip")
  # Download from USGS site and write to the working drive (scratch_dir)
  wsh_dl = GET(wsh_url, write_disk(zipname, overwrite = TRUE))
  # Unzip file and save in the working directory (defaults to Documents folder)
  unzip(file.path(scratch_dir, zipname), exdir = file.path(scratch_dir)) #, list = TRUE) # just lists files, does not unzip
  #Read watershed and named stream shapefiles into R
  wbdhu8 = readOGR(file.path(scratch_dir, "Shape", "WBDHU8.shp"))
  nhdwaterbody = readOGR(file.path(scratch_dir, "Shape", "NHDWaterbody.shp"))
  flowlines = readOGR(file.path(scratch_dir, "Shape", "NHDFlowline.shp"))
  
  #Subset flowlines
  #Note: keep named streams and unnamed ReachCode 18010208002394.
  #This is a channelized ditch connecting Johnson Creek to Crystal Creek
  named_streams = flowlines[(!is.na(flowlines$GNIS_Name)) | flowlines$ReachCode == 18010208002394,]
  #And, name this reach to make it part of Johnson Creek
  named_streams$GNIS_Name[named_streams$ReachCode == 18010208002394] = "Johnson Creek"
  
  #reproject
  wbdhu8 = spTransform(wbdhu8, crs("+init=epsg:3310"))
  nhdwaterbody = spTransform(nhdwaterbody, crs("+init=epsg:3310"))
  named_streams = spTransform(named_streams, crs("+init=epsg:3310"))
  
  # Write shapefiles to Box archive
  writeOGR(obj = wbdhu8, dsn = dms_archive_dir, layer = paste(basin,"watershed_huc8", sep="_"), driver = "ESRI Shapefile")
  writeOGR(obj = nhdwaterbody, dsn = dms_archive_dir, layer = paste(basin,"waterbody_nhd", sep="_"), driver = "ESRI Shapefile")
  writeOGR(obj = named_streams, dsn = dms_archive_dir, layer = paste(basin,"named_streams_nhd", sep="_"), driver = "ESRI Shapefile")
  
  # #Write shapefiles to database
  pgInsert(conn = siskiyou_spatial, name = paste(basin,"watershed_huc8", sep="_"), data.obj = wbdhu8, df.mode = FALSE, overwrite = TRUE)
  pgInsert(conn = siskiyou_spatial, name = paste(basin,"waterbody_nhd", sep="_"), data.obj = nhdwaterbody, df.mode = FALSE, overwrite = TRUE)
  pgInsert(conn = siskiyou_spatial, name = paste(basin,"named_streams_nhd", sep="_"), data.obj = named_streams, df.mode = FALSE, overwrite = TRUE)
}

#Delete zipped files
file.remove(file.path(scratch_dir, "NHD_H_18010208_HU8_Shape.zip"))
file.remove(file.path(scratch_dir, "NHD_H_18010207_HU8_Shape.zip"))
file.remove(file.path(scratch_dir, "NHD_H_18010205_HU8_Shape.zip"))
file.remove(file.path(scratch_dir, "Shape", list.files(file.path(scratch_dir, "Shape")))) #deletes files in unzipped "Shape" folder
unlink(file.path(scratch_dir, "Shape"),recursive = TRUE) #Removes an empty folder



# _NOAA NCDC ---------------------------------------------------------------

#Subfunctions
get_bbox_string = function(poly, return_half = 0){
  poly = spTransform(poly, crs("+init=epsg:4326")) #convert to WGS84)
  north = bbox(poly)[2,2]; west = bbox(poly)[1,1]; south = bbox(poly)[2,1]; east = bbox(poly)[1,2]
  bbox_poly = paste(c(south, west, north, east), collapse = ",")
  if(return_half>0){ ns_midpoint = south + 0.5*(north-south)}
  if(return_half == 1){bbox_poly = paste(c(ns_midpoint, west, north, east), collapse = ",")}
  if(return_half == 2){bbox_poly = paste(c(south, west, ns_midpoint, east), collapse = ",")}
  return(bbox_poly)
}

get_noaa_stations = function(cmk_token = "scKXkYaFjbbLtxmSnNYjWKBKXDvOCoeU" #received from NOAA on 2019-08-09: https://www.ncdc.noaa.gov/cdo-web/token
){
  #It is necessary to get these stations in at least 3 separate queries. Each 
  # query returns a maximum of 25 records and there are 69 total. We use 4
  # queries because it is convenient to use the watersheds as polygons covering
  # the entire area of interest.
  
  #Pull watersheds to calculate bounding box in lat-long
  wsh_scott = get_postgis_query(siskiyou_spatial, "SELECT * FROM scott_watershed_huc8", geom_name = "geom")

  bbox_scott = get_bbox_string(wsh_scott)
  # bbox_butte = get_bbox_string(wsh_butte)
  # bbox_shasta_1 = get_bbox_string(wsh_shasta, return_half=1)
  # bbox_shasta_2 = get_bbox_string(wsh_shasta, return_half=2)
  list_of_bboxes = list(bbox_scott)#, bbox_butte, bbox_shasta_1, bbox_shasta_2)
  
  #Initialize station table
  colnames_station_table = c("results.elevation","results.mindate",
                             "results.maxdate","results.latitude","results.name",
                             "results.datacoverage","results.id",
                             "results.elevationUnit","results.longitude")
  station_table = data.frame(matrix(NA, nrow = 0, ncol = 9))
  colnames(station_table) = colnames_station_table
  
  for(i in 1:length(list_of_bboxes)){
    bbox_string = list_of_bboxes[[i]]
    base_url = "https://www.ncdc.noaa.gov/cdo-web/api/v2/"
    stations_query_url = paste0(base_url, "stations?extent=", bbox_string)
    header_token <- structure(cmk_token , names = "token")
    # header_limit <- structure(200 , names = "limit")
    stations_dl = GET(stations_query_url, httr::add_headers(header_token))#, header_limit))
    
    #Parse download
    stations_dl_unlisted = unlist(content(stations_dl, "parsed" ))
    metadata=stations_dl_unlisted[1:3]
    stations_dl_unlisted = stations_dl_unlisted[-(1:3)] # scrape off 3 metatata arguments
    stations_colnames = names(stations_dl_unlisted[1:9])
    noaa_stations = data.frame(matrix(stations_dl_unlisted, ncol = 9, byrow = T))
    colnames(noaa_stations) = stations_colnames
    
    station_table = rbind(station_table, noaa_stations)
  }
  #Eliminate duplicates
  # sum(duplicated(station_table))
  station_table = station_table[!duplicated(station_table),]
  #Split up results ID into dataset type and station ID
  results_id_split=matrix(unlist(strsplit(as.character(station_table$results.id), split=":")), ncol=2, byrow=T)
  station_table$results.type = results_id_split[,1]
  station_table$station.id = results_id_split[,2]
  
  #Make stations table spatial
  stations_sp = station_table
  stations_sp$results.latitude = as.numeric(as.character(stations_sp$results.latitude))
  stations_sp$results.longitude = as.numeric(as.character(stations_sp$results.longitude))
  coordinates(stations_sp) = ~results.longitude + results.latitude
  proj4string(stations_sp) <- CRS("+init=epsg:4326") #assign WGS84 projection to coordinates
  
  return(list(stations_sp, station_table))
}

get_noaa_data = function(station_list){
  
  base_url = "https://www.ncei.noaa.gov/access/services/data/v1"
  dataset = "daily-summaries"
  # station_list = c("USC00041316","USC00043182","USC00042899", "USC00043614", "USC00049866", "US1CASK0005")
  stations = paste(station_list,collapse = ",") # Callahan, Ft Jones, Etna, Greenview, Yreka, Yreka NW
  start_date = "1800-01-01"
  end_date = Sys.Date()
  data_types = paste(c("TMAX", "TMIN", "PRCP", "SNOW", "SNWD"), collapse=",")
  
  noaa_url = paste0(base_url, "?dataset=", dataset, 
                    "&stations=", stations,
                    "&startDate=", start_date,
                    "&endDate=", end_date,
                    "&dataTypes=", data_types,
                    "&format=csv",
                    "&includeAttributes=0&includeStationName=true&includeStationLocation=true",
                    "&units=metric")
  
  noaa_dl = GET(noaa_url)
  noaa = as.data.frame(content(noaa_dl, "parsed", 
                               col_types = cols("STATION" = col_character(),
                                                "DATE" = col_date(format = ""),
                                                "PRCP" = col_double(),
                                                "SNOW" = col_double(),
                                                "SNWD" = col_double(),
                                                "TMAX" = col_double(),
                                                "TMIN" = col_double()) ))
  return(noaa)
}

combine_cdec_and_noaa_wx_data=function(cdec_data, noaa_data){
  
}

combine_cdec_and_noaa_wx_stations=function(cdec_stn, noaa_stn){
  
}


upload_and_archive_noaa_data = function(noaa_data){
  
  #Update the live "wl_observations" and "wells" tables, and make an archive datestamped copy
  #Archive
  noaa_archive_name = paste0("noaa_daily_data_ghcnd_",format(Sys.Date(), "%Y.%m.%d"),".csv" )
  write.csv(noaa_data, file.path(dms_archive_dir,noaa_archive_name))
  
  #Overwrite copy on server
  copy_to(dest = siskiyou_tables, df = noaa_data, 
          name = "noaa_daily_data", overwrite=TRUE,
          temporary = FALSE, indexes = list("STATION","DATE"))
}


#Pull station list from noaa website
station_info = get_noaa_stations()
station_sp = station_info[[1]]; station_sp = spTransform(station_sp, CRS("+init=epsg:3310"))
station_table = station_info[[2]]
#Only include GHCND stations. These are historical daily records.
ghcnd_stations = station_table$station.id[station_table$results.type == "GHCND"]

noaa_updated_dataset = get_noaa_data(station_list = ghcnd_stations)

upload_and_archive_noaa_data(noaa_data = noaa_updated_dataset)

# Notes: optional extra weather datasets (for scott; could be others in other basins)
# TMAX 	Maximum temperature 
# TMIN 	Minimum temperature 
# TOBS 	Temperature at the time of observation
# DAPR 	Number of days included in the multiday precipitation total (MDPR) 	1949-12-19 	2018-11-29 	
# MDPR 	Multiday precipitation total (use with DAPR and DWPR, if available) 	1949-12-19 	2018-11-29 	
# PRCP 	Precipitation #tenths of a mm
# SNOW 	Snowfall 
# SNWD 	Snow depth
# WT01 	Fog, ice fog, or freezing fog (may include heavy fog)
# WT03 	Thunder
# WT04 	Ice pellets, sleet, snow pellets, or small hail" 
# WT05 	Hail (may include small hail) 
# WT06 	Glaze or rime 
# WT08 	Smoke or haze 
# WT09 	Blowing or drifting snow
# WT11 	High or damaging winds 
# WT14 	Drizzle 
# WT16  Rain (may include freezing rain, drizzle, and freezing drizzle)" 


