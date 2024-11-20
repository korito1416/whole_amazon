

library(sf)
library(tidyverse)
library(tictoc)
library(sjlabelled)
library(conflicted)

# Resolve conflicts
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::lag)

load(here::here("data/clean/amazon_biome.Rdata"))

# Read shapefile
raw_region <- st_read(
  dsn = "raw/geoBoundaries/",
  layer = "geoBoundariesCGAZ_ADM0"
)

amazon_countries_list <- c(
  "Brazil", "Peru", "Colombia", "Venezuela", "Ecuador",
  "Bolivia", "Guyana", "Suriname", "France"
)

raw_region_filtered <- raw_region %>%
  filter(shapeName %in% amazon_countries_list)
    

amazon_countries <- st_intersection(raw_region_filtered, amazon_biome)

save(amazon_countries, file = "data/clean/amazon_countries.Rdata")
