
library(sf)
library(units)
library(terra)
library(readxl)
library(tidyverse)
library(tictoc)
library(conflicted)

conflicts_prefer(dplyr::filter)



# Load raster data (amazon biome share, pixel areas, and land uses)
raster_variables <- rast(
  list.files(
    "data/calibration/",
    pattern = ".tif",
    full.names = TRUE
  )
)

# Extract variables as polygons and project data
calib_df <- as.polygons(raster_variables, dissolve = FALSE) %>%
  st_as_sf() %>%
  st_transform(5880)

# Remove sites with less than 3% overlap with the amazon biome
calib_df <- calib_df %>%
  filter(share_amazon_biome >= 0.03)

# Add id variable
calib_df$id <- seq_len(nrow(calib_df))

# Transform share variables into area (ha)
calib_df <- calib_df %>%
  rename(site_area_ha = pixel_area_ha) %>%
  mutate(across(
    starts_with("share_"),
    .names = "area_{.col}",
    ~ . * site_area_ha
  )) %>%
  rename_with(
    ~ str_replace(., "share_", ""),
    starts_with("area_")
  )

# Change names to z and add zbar
calib_df <- calib_df %>%
  rename_with(
    ~ str_replace(., "area_agricultural_use", "z"),
    starts_with("area_agricultural_use")
  ) %>%
  mutate(
    zbar_2017 = area_forest_2017 + z_2017,
  )




# Save site boundaries
calib_df %>%
  select(id) %>%
  st_write(
    "data/calibration/grid_1043_sites.geojson",
    driver = "GeoJSON",
    delete_dsn = TRUE
  )

# Save calibration data
save(calib_df, file = "data/calibration/calibration_1043_sites.Rdata")

# Save calibration data CSV
calib_df %>%
  st_drop_geometry() %>%
  write_csv(file = "data/calibration/calibration_1043_sites.csv")

