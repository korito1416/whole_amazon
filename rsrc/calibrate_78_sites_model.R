

library(sf)
library(units)
library(terra)
library(readxl)
library(tidyverse)
library(tictoc)
library(conflicted)

conflicts_prefer(dplyr::filter)



# Load raster data (amazon biome share, pixel areas, and land uses)
rsts <- rast(
  list.files(
    "data/calibration/",
    pattern = ".tif",
    full.names = TRUE
  )
)

# Transform shares to areas
rsts$area_amazon_biome <-
  rsts$share_amazon_biome * rsts$pixel_area_ha


rsts$area_forest_2017 <-
  rsts$share_forest_2017 * rsts$pixel_area_ha

rsts$area_agricultural_use_2017 <-
  rsts$share_agricultural_use_2017 * rsts$pixel_area_ha

rsts$area_other_2017 <-
  rsts$share_other_2017 * rsts$pixel_area_ha

# Aggregate into larger sites
rsts <- aggregate(
  rsts,
  fact = 4,
  fun = sum,
  na.rm = TRUE
)

# Extract variables as polygons and project data
calib_df <- as.polygons(rsts, dissolve = FALSE) %>%
  st_as_sf() %>%
  st_transform(5880)

# Remove sites with less than 3% overlap with the amazon biome
calib_df <- calib_df %>%
  filter(area_amazon_biome / pixel_area_ha >= 0.03)

# Add id variable
calib_df$id <- seq_len(nrow(calib_df))

# Change names to z and add zbar
calib_df <- calib_df %>%
  rename(site_area_ha = pixel_area_ha) %>%
  mutate(
    z_2017 = area_agricultural_use_2017,
    zbar_2017 = area_forest_2017 + z_2017,
  )



# Save site boundaries
calib_df %>%
  select(id) %>%
  st_write(
    "data/calibration/grid_78_sites.geojson",
    driver = "GeoJSON",
    delete_dsn = TRUE
  )

# Save calibration data
save(calib_df, file = "data/calibration/calibration_78_sites.Rdata")

# Save calibration data CSV
calib_df %>%
  st_drop_geometry() %>%
  write_csv(file = "data/calibration/calibration_78_sites.csv")


