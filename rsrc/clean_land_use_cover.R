library(terra)

raw_raster <- map(
  list.files(
    "raw/mapbiomas/",
    pattern = "mapbiomas-amazon-collection-50-amazonia1-2000.*\\.tif$",
    full.names = TRUE
  ),
  rast
)
combined_raster <- do.call(merge, raw_raster)


# Change 0s to NAs to represent only areas inside the Amazon Biome
combined_raster <- subst(combined_raster, from = 0, to = as.numeric(NA))

# Save reclassified tif
out_path <- "data/clean/land_use_cover_2000.tif"
writeRaster(combined_raster, out_path, overwrite = TRUE)







