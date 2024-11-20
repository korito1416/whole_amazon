library(terra)
library(purrr)


input_dir <- "raw/mapbiomas/"
output_dir <- "raw/cleaned_mapbiomas/"

# Process all years from 1985 to 2017
years <- 1985:2017

for (year in years) {
  cat("Processing year:", year, "\n")
  

  raster_files <- list.files(
    input_dir,
    pattern = paste0("mapbiomas-amazon-collection-50-amazonia1-", year, ".*\\.tif$"),
    full.names = TRUE
  )
  

  year_rasters <- map(raster_files, rast)
  combined_raster <- do.call(merge, year_rasters)
  

  out_path <- file.path(output_dir, paste0("land_use_cover_", year, ".tif"))
  writeRaster(combined_raster, out_path, overwrite = TRUE)
  
  cat("Saved combined raster for year:", year, "to", out_path, "\n")
}

cat("Processing completed for all years.\n")
