

library(sf)
library(units)
library(tictoc)
library(terra)
library(tidyverse)
library(sjlabelled)
library(conflicted)

conflicts_prefer(terra::extract)



# Load mapbiomas data
clean_mapbiomas <- rast("data/clean/land_use_cover_2000.tif")

# Set random seed to guarantee reproducibility
set.seed(123)

# Extract random sample cells and keep only coordinate info
pixel_sample <- data.frame(spatSample(clean_mapbiomas, 1200000, na.rm = T, xy = T)[, 1:2])

# Read all MapBiomas layers (one for each year)
aux_mapbiomas <- rast(
  list.files("raw/cleaned_mapbiomas/",
    pattern = "land_use_cover_\\d{4}",
    full.names = TRUE
  )
)

# Change raster stack layer names
names(aux_mapbiomas) <- c(1985:2017)

# Use points sample to extract complete panel
pixel_sample <- cbind(pixel_sample, extract(aux_mapbiomas, pixel_sample, df = T)[, -1])

# Reshape
pixel_sample <- pivot_longer(
  pixel_sample,
  cols = matches("\\d{4}"),
  names_to = "year",
  values_to = "mapbiomas_class"
)

# Minor fix year column
pixel_sample$year <- pixel_sample$year %>%
  str_extract("\\d{4}") %>%
  as.numeric()

# Rename columns
pixel_sample <-
  pixel_sample %>%
  rename(lon = x, lat = y)

# Set labels
set_label(pixel_sample$lon) <- "longitude of the pixel centroid (degrees)"
set_label(pixel_sample$lat) <- "latitude of the pixel centroid (degrees)"
set_label(pixel_sample$year) <- "year of reference"
set_label(pixel_sample$mapbiomas_class) <- "mapbiomas land use/cover classification (id)"

# Save data set
save(pixel_sample, file = "data/clean/pixel_sample.Rdata")


