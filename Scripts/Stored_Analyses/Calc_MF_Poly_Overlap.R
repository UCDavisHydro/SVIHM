library(RSVP)
library(sf)
library(ggplot2)
library(RMODFLOW)
library(units)
library(dplyr)
library(ggnewscale)

#-------------------------------------------------------------------------------------------------#
# Settings

swbm_fields_file <- file.path(data_dir["ref_data_dir","loc"], "Landuse_20190219.shp")
mf_grid_file <- file.path('C:/Users/lelan/Box/Research/Scott Valley/GIS/MODFLOW/100m_grid_UTM_20180126.shp')
out_file <- file.path(data_dir['time_indep_dir','loc'], 'MF_Polygon_Overlaps.txt')
mf_ref <- file.path('../..//Run_basecase/MODFLOW')

#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
# Read Data
swbm_fields <- read_sf(swbm_fields_file)
mf_grid <- read_sf(mf_grid_file)

# Check data
swbm_fields <- st_make_valid(swbm_fields)
mf_grid <- st_make_valid(mf_grid)

# Set/Convert CRS
st_crs(mf_grid) <- 26910
swbm_fields <- st_transform(swbm_fields, st_crs(mf_grid))

# Fish for BAS
mfnam <- rmf_read_nam(file.path(mf_ref,'SVIHM.nam'))
mfdis <- rmf_read_dis(file.path(mf_ref,'SVIHM.dis'), nam=mfnam)
mfbas <- rmf_read_bas(file.path(mf_ref,'SVIHM.bas'), dis=mfdis, nam=mfnam)
ibound <- mfbas$ibound[,,1]
row_col <- c("row","column")
mf_grid$ibound <- as.integer( ibound[ cbind(mf_grid[[row_col[1]]], mf_grid[[row_col[2]]]) ] )

#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
# Check spatial overlap
ggplot() +
  # draw the MODFLOW grid with thin grey borders
  geom_sf(data = mf_grid, fill = NA, color = "grey60", size = 0.2) +
  # overlay the SWBM fields in semi-transparent color
  geom_sf(data = swbm_fields, fill = "tomato", color = 'grey50', alpha = 0.5) +
  coord_sf() +
  theme_minimal() +
  labs(
    title = "MODFLOW Grid with SWBM Field Polygons",
    subtitle = "Grid in grey, fields overlaid in semi-transparent red"
  )
#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
# Overlap Analysis

# Add unique column for MF
mf_grid <- mf_grid[order(mf_grid$row,mf_grid$column),]
mf_grid$node <- 1:nrow(mf_grid)

# Intersect on 'node' (and grab the cell area)
ov2 <- st_intersection(
  mf_grid[, c("row","column","area", "ibound")],
  swbm_fields[, "Polynmbr"]
)

ov2 <- ov2[order(ov2$Polynmbr),]

# 3) Compute overlap area and weight
ov2$area_overlap <- st_area(ov2)
ov2$w            <- as.numeric(ov2$area_overlap / ov2$area)

#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
# # Refinement
#
# ov2$overlap_type <- NA
#
# # Manual Loop
# for (fid in 1:max(swbm_fields$Polynmbr)) {
#   f_sub <- ov2[ov2$Polynmbr==fid,]
#
#   # Get percent active overlap
#   target_area <- sum(f_sub$area_overlap)
#   pct <- drop_units(sum(f_sub$area_overlap[f_sub$ibound==1]) / target_area)
#
#   # A little check
#   if (drop_units((st_area(swbm_fields[swbm_fields$Polynmbr==fid,]) - sum(f_sub$area_overlap))) > 0.1) {
#     message(paste('Bad Area Overlap with Poly', fid))
#   }
#
#   # Decision Point
#   if (pct <= 0.01) {
#     message(paste('FID:',fid,' - No Overlap'))
#     ov2[ov2$Polynmbr==fid, 'overlap_type'] <- 'No Overlap'
#   } else if (pct < 0.3) {
#     message(paste('FID:',fid,' - Too Small Overlap:',round(pct,3)))
#     ov2[ov2$Polynmbr==fid, 'overlap_type'] <- 'Too Small Overlap'
#   } else if (pct < 1) {
#     message(paste('FID:',fid,' - Renormalizing, Overlap:',round(pct,3)))
#     ov2[ov2$Polynmbr==fid, 'overlap_type'] <- 'Renormalized'
#     ov2[(ov2$Polynmbr==fid & ov2$ibound==0),'w'] <- 0.0
#     ov2[(ov2$Polynmbr==fid & ov2$ibound==0),'area_overlap'] <- as_units(0.0, 'm^2')
#     norm <- sum(st_drop_geometry(ov2[ov2$Polynmbr==fid,'area_overlap']))/target_area
#     ov2[ov2$Polynmbr==fid,'w'] <- st_drop_geometry(ov2[ov2$Polynmbr==fid,'w']) / drop_units(norm)
#   } else {
#     message(paste('FID:',fid,' - FULL Overlap'))
#     ov2[ov2$Polynmbr==fid, 'overlap_type'] <- 'Full Overlap'
#   }
# }
#
# # View Map
# ggplot() +
#   geom_sf(data = ov2, aes(fill = overlap_type), color = 'grey30') +
#   coord_sf() +
#   theme_minimal() +
#   theme(legend.position = "bottom") +
#   labs(title = "Field-cell overlaps by type")
#
# #-------------------------------------------------------------------------------------------------#
# Look at weights by Cell
key_ov <- paste(ov2$row, ov2$column)
w_sum  <- tapply(ov2$w, key_ov, sum, na.rm = TRUE)

key_mf <- paste(mf_grid$row, mf_grid$column)
mf_grid$wtotal <- w_sum[key_mf]
mf_grid$wtotal[is.na(mf_grid$wtotal)] = 0

# classify cell status for the overlay
mf_grid <- mf_grid %>%
  mutate(
    status = case_when(
      ibound == 0 & wtotal > 0 ~ "Inactive (has weight)",
      ibound == 0              ~ "Inactive (no weight)",
      TRUE                     ~ "Active"
    )
  )

ggplot() +
  # 1) Active cells: use the continuous weight fill
  geom_sf(
    data = subset(mf_grid, ibound == 1),
    aes(fill = wtotal),
    color = NA
  ) +
  scale_fill_viridis_c(name = "Weight sum", na.value = "white") +

  # 2) Start a new fill scale for the inactive overlay
  ggnewscale::new_scale_fill() +

  # 3) Inactive cells: categorical colors, drawn on top
  geom_sf(
    data = subset(mf_grid, ibound == 0),
    aes(fill = status),
    color = NA
  ) +
  scale_fill_manual(
    name   = "Inactive cells",
    values = c(
      "Inactive (no weight)"  = "grey85",
      "Inactive (has weight)" = "magenta"
    )
  ) +
  coord_sf() +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Field-cell overlaps",
       subtitle = "Active cells colored by weight; inactive cells overlaid")

#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
# Write

write.table(st_drop_geometry(ov2[,c('Polynmbr','row','column','w')]),
            file = out_file,
            row.names = F,
            col.names = c('Polygon_id', 'Row', 'Col', 'Overlap_Weight'),
            quote = F
)

#-------------------------------------------------------------------------------------------------#
