

library(sf)
library(glue)
library(tictoc)
library(terra)
library(conflicted)

conflicts_prefer(terra::extract())



mapbiomas_class <- c("forest", "agricultural_use", "other")
aux_year <- c(2017)

for (class in mapbiomas_class) {
  for (year in aux_year) {
    # Read MapBiomas raster with land use and land cover (baseline)
    in_file <- glue("raw/cleaned_mapbiomas/land_use_cover_{year}.tif")
    raw_raster <- rast(in_file)

    # Change raster values - see "documentation/mapbiomasClass_id_legend.pdf"
    if (class == "forest") {
      # Create dummy for forest class
      raw_raster[raw_raster != 3] <- 0
      raw_raster[raw_raster == 3] <- 1
    } else if (class == "agricultural_use") {
      # Create dummy for agricultural use classes
      raw_raster[!(raw_raster %in% c(15, 20, 39, 41))] <- 0
      raw_raster[raw_raster %in% c(15, 20, 39, 41)] <- 1
    } else if (class == "other") {
      # Create dummy for other classes
      raw_raster[raw_raster %in% c(3, 15, 20, 39, 41)] <- 0
      raw_raster[!(raw_raster %in% c(0, 3, 15, 20, 39, 41))] <- 1
    }

    # Calculating the share of minicells within each category
    # (2250^2) is the total number of minicells
    raw_raster <- aggregate(
      raw_raster,
      fact = 2250,
      fun = sum,
      na.rm = TRUE
    ) / (2250^2)

    # Add category name
    names(raw_raster) <- glue("share_{class}_{year}")

    # Save raster data set
    out_file <- glue("data/calibration/amazon_{class}_{year}_shares_1000_sites.tif")
    writeRaster(raw_raster, out_file, overwrite = TRUE)

    # Clean environment
    rm(raw_raster)
  }
}


