library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(RSVP)
library(sf)

# Settings // Paths
model_output_dir <- "C:/Users/lelan/Box/Research/Scott Valley/Files Recieved/LWA_MFR_2025-04-11/"
ref_dir <- "C:/Users/lelan/Box/Research/Scott Valley/Files Recieved/LWA_MFR_2025-04-11/"
gis_dir <- 'C:/Users/lelan/Box/Research/Scott Valley/GIS'

out_dir <- file.path(data_dir['input_files_dir','loc'],'PRMS_outputs_for_SWBM')
dir.create(out_dir)

# Functions ---------------------------------------------------------------

read_prms_file <- function(file_path, hru_ids_char, skip = 1, sep = ",") {
  # Get number of HRUs from the first line
  header_line <- readLines(file_path, n = 1)
  n_hrus <- length(strsplit(header_line, sep)[[1]]) - 1

  # Build types
  prms_types <- list("")
  prms_types <- append(prms_types, numeric(n_hrus))

  # Read in using scan
  readlist <- scan(file_path, skip = skip, sep = sep, what = prms_types)

  # Name elements in the list
  names(readlist) <- c("Date", paste0("", seq_len(n_hrus)))

  # Convert to data frame
  df <- as.data.frame(readlist, stringsAsFactors = FALSE)
  df$Date <- as.Date(df$Date)

  # Filter to only HRUs of interest
  hru_cols <- intersect(names(df), paste0("X", hru_ids_char))
  if (length(hru_cols) == 0) stop("No matching HRU IDs found.")

  # Reshape and calculate total m3
  df_long <- df[, c("Date", hru_cols)] %>%
    pivot_longer(-Date, names_to = "HRU_ID", values_to = "inches") %>%
    mutate(
      HRU_ID = gsub("^X", "", HRU_ID),            # Strip leading X again
      HRU_ID = as.integer(HRU_ID),
      m3 = inches * 0.0254 * 100 * 100
    ) %>%
    select(Date, HRU_ID, m3)

  return(df_long)
}


# Assemble HRU list -------------------------------------------------------

# Load required files
border_df <- read_csv(file.path(ref_dir, "SVIHM_HRUID_border.csv"), show_col_types = FALSE)
cascade_tab <- read_csv(file.path(ref_dir, "cascade_tab.csv"), show_col_types = FALSE)
touching_df <- read_csv(file.path(ref_dir, "touching_svihm_border.csv"), show_col_types = FALSE)

# Mark SVIHM boundary HRUs (downslope cells)
border_df <- border_df %>%
  mutate(SVIHM_border = !is.na(SubId)) %>%
  select(HRU_ID, SVIHM_border)

# Join to cascade table to find upslope HRUs
cascade_upslope <- cascade_tab %>%
  select(HRU_ID, DS1_Grid_HRUID) %>%
  rename(HRU_ID_upslope = HRU_ID, HRU_ID = DS1_Grid_HRUID) %>%
  left_join(border_df, by = "HRU_ID") %>%
  filter(SVIHM_border)

# Keep only upslope HRUs that touch SVIHM boundary
touching_filtered <- touching_df %>%
  filter(!is.na(SubId_2)) %>%
  rename(HRU_ID_touching = HRU_ID) %>%
  filter(HRU_ID_touching %in% cascade_upslope$HRU_ID_upslope)

# Remove upslope HRUs that flow to streams
stream_flows <- cascade_tab %>%
  select(HRU_ID, DS1_Stream_Cell_segment) %>%
  filter(!is.na(DS1_Stream_Cell_segment))

# Remove HRUs with streams
stream_cells <- cascade_tab %>%
  select(DS1_Grid_HRUID, DS1_Stream_Cell_segment) %>%
  filter(!is.na(DS1_Stream_Cell_segment)) %>%
  distinct()

# Apply stream filters
final_hrus <- touching_filtered %>%
  left_join(stream_flows, by = c("HRU_ID_touching" = "HRU_ID")) %>%
  filter(is.na(DS1_Stream_Cell_segment)) %>%
  select(HRU_ID_touching) %>%
  left_join(stream_cells, by = c("HRU_ID_touching" = "DS1_Grid_HRUID")) %>%
  filter(is.na(DS1_Stream_Cell_segment)) %>%
  pull(HRU_ID_touching)

# Manually remove HRUs
final_hrus <- setdiff(final_hrus, "164510")

touching_hrus <- as.character(final_hrus)

# Read PRMS files, Get Values --------------------------------------------------------------------

# PRMS file processing using original function
gw_mfr    <- read_prms_file(file.path(model_output_dir, "hru_gw_cascadeflow_monthly.csv"), touching_hrus)
hortn_mfr <- read_prms_file(file.path(model_output_dir, "hru_hortn_cascflow_monthly.csv"), touching_hrus)
sz_mfr    <- read_prms_file(file.path(model_output_dir, "hru_sz_cascadeflow_monthly.csv"), touching_hrus)

# Combine into total
mfr_by_hru <- full_join(gw_mfr, hortn_mfr, by = c("Date", "HRU_ID"), suffix = c("_gw", "_hortn")) %>%
  full_join(sz_mfr, by = c("Date", "HRU_ID")) %>%
  rename(m3_sz = m3) %>%
  mutate(
    m3_gw     = coalesce(m3_gw, 0),
    m3_hortn  = coalesce(m3_hortn, 0),
    m3_sz     = coalesce(m3_sz, 0),
    MFR_m3    = m3_gw + m3_hortn + m3_sz
  ) %>%
  select(Date, HRU_ID, MFR_m3)


# Map HRUs to catchments -------------------------------------------------------
prms_catchments <- read_sf(file.path(gis_dir,'PRMS','PRMS_Grid_ScottWatershedOutsideSVIHM_catchments.shp'))
mf_catchments <- read_sf(file.path(gis_dir,'MODFLOW','grid_outmostcells_catchments.shp'))

cell_to_subid <- mf_catchments %>%
  st_drop_geometry() %>%
  select(row, column, SubId) %>%
  arrange(SubId, row, column)

write.table(cell_to_subid, file.path(out_dir, "modflow_cell_to_catchment.txt"), row.names = F, quote = F)

# Map cells to catchments -------------------------------------------------

# Drop geometry and keep HRU_ID + SubID
hru_to_subid <- prms_catchments %>%
  st_drop_geometry() %>%
  select(HRU_ID, SubId)

# Join catchment IDs to full MFR table
mfr_by_catchment <- mfr_by_hru %>%
  left_join(hru_to_subid, by = "HRU_ID") %>%
  filter(!is.na(SubId)) %>%
  group_by(Date, SubId) %>%
  summarise(MFR_m3 = sum(MFR_m3, na.rm = TRUE), .groups = "drop")

# Pivot to wide format (SubID columns, Date rows)
mfr_table_wide <- mfr_by_catchment %>%
  pivot_wider(names_from = SubId, values_from = MFR_m3)

# write.table(mfr_table_wide, file.path(out_dir, "monthly_MFR_by_catchment.txt"), row.names = F, quote = F)

# Format header
header <- c(format("Date", width = 10, justify = "right"),
            format(names(mfr_table_wide)[-1], width = 14, justify = "right"))

# Format each row
formatted_rows <- apply(mfr_table_wide, 1, function(row) {
  date_str <- format(row[1], width = 10, justify = "right")
  value_strs <- formatC(as.numeric(row[-1]),
                        width = 14,
                        digits = 6,
                        format = "E",
                        flag = " ")
  paste(c(date_str, value_strs), collapse = "")
})

# Combine header + data
output_lines <- c(paste(header, collapse = ""), formatted_rows)

# Write to file
writeLines(output_lines, file.path(out_dir, "monthly_MFR_by_catchment.txt"))
