library(RSVP)
library(sf)
library(dplyr)
library(ggplot2)
library(gridExtra)

#-------------------------------------------------------------------------------------------------#
#-- Setup
start_date <- as.Date('2023-01-01')
end_date <- as.Date('2023-12-31')

lcs_type <- c("percent reduction")
lcs_abbr <- c('pctr')

# LCS reductions
months <- c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")
pctr <- c(0.3, 0.3, 0.3 , 0.3 , 0.3  , 0.3  , 0.3  )

# Combine
reductions <- list(
  "percent reduction" = pctr
)

min_overlap_prct <- 0.01

#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
#-- Read in files
swbm_fields <- read_sf(file.path(data_dir["ref_data_dir","loc"],"Landuse_20190219.shp"))

curt23_fields <- read_sf(file.path(data_dir["ref_data_dir","loc"],"2023_LCS_Fields_Clean.shp"))

# Reproject to a common CRS (NAD83(2011) / California Albers)
curt23_fields <- st_transform(curt23_fields, crs = st_crs(swbm_fields))
#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
#-- For 2023, only percent reduction fields (30%)

lcs_fields <- curt23_fields
i <- 1

# Calculate overlaps
overlaps <- st_intersection(lcs_fields, swbm_fields)

# Calculate overlap fractions
overlaps$overlap_fraction <- as.numeric(st_area(overlaps) /
                                          st_area(swbm_fields[match(overlaps$Polynmbr, swbm_fields$Polynmbr), ]))
overlaps = overlaps[overlaps$overlap_fraction >= min_overlap_prct, ]

# Summarize overlap fractions by SWBM field
overlap_summary <- overlaps %>%
  group_by(Polynmbr) %>%
  summarize(overlap_fraction = sum(overlap_fraction, na.rm = TRUE))

# Add the overlap fractions to the corresponding column in swbm_fields
swbm_fields[lcs_abbr[i]] <- overlap_summary$overlap_fraction[match(swbm_fields$Polynmbr, overlap_summary$Polynmbr)]


#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
#-- Visualize
plot_lcs <- function(data, lcs_col, title) {
  ggplot(data = data) +
    geom_sf(aes_string(fill = lcs_col), color = "black", size = 0.1) +
    scale_fill_viridis_c(name = "Overlap Fraction", option = "C", na.value = "white") +
    theme_minimal() +
    labs(
      title = title,
      subtitle = "Overlap fractions for each field",
    ) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10)
    )
}

# Create individual plots for each LCS type
plot_lcs(swbm_fields, "pctr", "Percent Reduction (PCTR)")

#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
#-- Setup curtailment df, Multiply overlaps by percent reductions

curtailment_df = swbm_build_field_value_df(model_start_date = start_date,
                                           model_end_date = end_date, default_values = 0.0)

# No overlap == zero
for (i in 1:length(lcs_type)) {
  swbm_fields[[lcs_abbr[i]]][is.na(st_drop_geometry(swbm_fields[lcs_abbr[i]]))] <- 0.0
}

# Add columns for each month and LCS combination, and calculate reductions
for (i in 1:length(lcs_abbr)) {
  for (j in seq_along(months)) {
    month_col <- paste0(lcs_abbr[i], "_", months[j])

    # add into swbm_fields for record keeping
    swbm_fields[[month_col]] <- swbm_fields[[lcs_abbr[i]]] * reductions[[lcs_type[i]]][j]

    # Match `Polynmbr` in `swbm_fields` to column names in `curtailment_df`
    matching_cols <- paste0("ID_", swbm_fields$Polynmbr)

    # Assign the reductions to the appropriate stress period in `curtailment_df`
    curtailment_df[curtailment_df$Stress_Period == as.Date(paste0("2023-", j+3, "-01")), matching_cols] <-
      curtailment_df[curtailment_df$Stress_Period == as.Date(paste0("2023-", j+3, "-01")), matching_cols] +
      swbm_fields[[month_col]]

  }
}

# Cap curtailment amounts at 30%
curtailment_df[ , -1] <- pmin(curtailment_df[ , -1], 0.3)

#-------------------------------------------------------------------------------------------------#
#-- Write modified swbm_fields file to shapefile (for validation purposes)
#write_sf(swbm_fields, file.path('C:/Users/lelan/Box/Research/Scott Valley/Analyses/LCS 2024 Plan','swbm_fields_curtail2023.shp'))

#-- Write curtailment as a csv file
write.csv(curtailment_df, file=file.path(data_dir["ref_data_dir","loc"], 'Curtail_23.csv'), row.names = F)

#-------------------------------------------------------------------------------------------------#
