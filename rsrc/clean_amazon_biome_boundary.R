

library(sf)
library(tidyverse)
library(tictoc)
library(sjlabelled)
library(conflicted)

# Resolve conflicts
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::lag)


# Read shapefile
raw_biome <- st_read(
  dsn = "raw/raisg/amazon_biome/",
  layer = "biomas"
)


# Column names
colnames(raw_biome)

# Translate column names
raw_biome <-
  raw_biome %>%
  rename(
    biome_code = id_bioma,
    biome_name = bioma
  )

# Class - no change needed
lapply(raw_biome, class)

# TRANSLATION
# 'grepl' used to avoid encoding trouble with latin characters
raw_biome$biome_name[which(grepl(pattern = "Amazonía", x = raw_biome$biome_name))] <- "Amazon"


# LETTERS CAPITALIZATION
raw_biome <-
  raw_biome %>%
  mutate(biome_name = toupper(biome_name))

# FILTER BIOME OF INTEREST (AMAZON)
raw_biome <-
  raw_biome %>%
  filter(biome_name == "AMAZON")


# Project to CRS 4326 and save
output_path <- "data/calibration/map.geojson"
if (file.exists(output_path)) {
  file.remove(output_path)
}
raw_biome %>%
  st_transform(crs = 4326) %>%
  st_write(output_path, driver = "GeoJSON")

# PROJECTION
# SIRGAS 2000 / Brazil Polyconic (https://epsg.io/5880)
raw_biome <- st_transform(x = raw_biome, crs = 5880)

# GEOMETRY CLEANUP
raw_biome <- st_make_valid(raw_biome)

# LABELS
set_label(raw_biome$biome_code) <- "biome code"
set_label(raw_biome$biome_name) <- "biome name"

# Change object name before saving
amazon_biome <- raw_biome

# Save data set
save(amazon_biome, file = "data/clean/amazon_biome.Rdata")

