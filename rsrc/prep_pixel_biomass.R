
library(sf)
library(tictoc)
library(terra)
library(tidyverse)
library(conflicted)
library(sjlabelled)

conflicts_prefer(dplyr::filter())
conflicts_prefer(terra::extract())

# Load pixel data for primary forest
load("data/clean/pixel_primary_forest_2017.Rdata")

# Aboveground biomass rasters
agb_raster <- map(
  list.files(
    "raw/esa/above_ground_biomass/",
    pattern = "_ESACCI-BIOMASS-L4-AGB-MERGED-100m-2017-fv3.0.tif",
    full.names = TRUE
  ),
  rast
)

# Transform to sf
pixel_primary_forest_2017 <- st_as_sf(
  x = pixel_primary_forest_2017,
  coords = c("lon", "lat"),
  crs = st_crs(4326),
  remove = FALSE
)

# Merge AGB data with primary forest sample
pixel_biomass_2017 <-
  map_df(
    .x = seq_along(agb_raster),
    .f = function(.x) {
      # transform to spatVector
      aux_polygons <- vect(pixel_primary_forest_2017)

      # crop spatial points to raster extent
      aux_polygons <- crop(aux_polygons, agb_raster[[.x]])

      # skip raster with no intersection with the sample
      if (is.null(aux_polygons)) {
        next()
      }

      # change raster layer name
      names(agb_raster[[.x]]) <- "agb_2017"

      # extract agb raster data by spatial point
      aux_agb <- extract(agb_raster[[.x]], aux_polygons, xy = TRUE)

      # adjust and select column names
      aux_agb <- aux_agb %>%
        rename(lon = x, lat = y) %>%
        select(-ID)
    }
  )

# Clear environment
rm(agb_raster)

# Transform to sf
pixel_biomass_2017 <-
  st_as_sf(
    x = pixel_biomass_2017,
    coords = c("lon", "lat"),
    crs = st_crs(4326),
    remove = FALSE
  )

# Check if all points were extracted
if (length(pixel_biomass_2017$lon) != length(pixel_primary_forest_2017$lon)) {
  print("Number of spatial points does not match!")
}

# Set labels
set_label(pixel_biomass_2017$agb_2017) <- "aboveground biomass in 2017 (Mg per ha)"

# Save data set
save(pixel_biomass_2017, file = "data/clean/pixel_biomass_2017.Rdata")


