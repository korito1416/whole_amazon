library(sf)
library(glue)
library(terra)
library(tidyverse)
library(conflicted)

conflicts_prefer(dplyr::filter())
conflicts_prefer(terra::extract())

load("data/calibration/calibration_1043_sites.Rdata")
calib_df<-calib_df %>%
  st_transform(4326)



# Load clean mapbiomas raster
clean_mapbiomas <- rast("data/clean/land_use_cover_2000.tif")

# Load tree richness raster
trees_rst <- rast("raw/biodiversity/TreeRichness_ha.asc")

# Rename tree richness raster
names(trees_rst) <- "tree_richness_ha"

# Clean trees raster
trees_rst <- trees_rst %>%
  crop(clean_mapbiomas)

# Calculate mean raster value within each polygon
calib_df$tree_richness_ha <- trees_rst %>%
  extract(calib_df, fun = mean, na.rm = TRUE) %>%
  pull(tree_richness_ha)


# Calculate the distance matrix between sites
dist <- st_distance(calib_df)

# For each polygon, find the indices of the 6 nearest neighbors
nns <- apply(dist, 1, function(dist) {
  order(dist)[2:7]
})

imputed_var <- calib_df$tree_richness_ha

# Loop over each site with missing value
for (i in which(is.na(imputed_var))) {
  # Get indices of the 6 nearest neighbors
  nn_indices <- nns[, i]

  # Calculate the mean tree richness for neighbors
  imputed_var[i] <- mean(calib_df$tree_richness_ha[nn_indices], na.rm = TRUE)
}

# Update with imputed values
calib_df$tree_richness_ha <- imputed_var


# Save calibration data
save(calib_df, file = "data/calibration/calibration_1043_with_biodiversity.Rdata")






### plots


load("data/clean/amazon_countries.Rdata")



# Define breaks and labels
breaks <- c(0, 70, 140, 210, 280, 350)
labels <- c("0-70", "70-140", "140-210", "210-280", "280-350")

# Modify your plotting code
plot <- ggplot2::ggplot(data = calib_df) +
  ggplot2::geom_sf(aes(fill = cut(tree_richness_ha, breaks = breaks, labels = labels))) +
  ggplot2::scale_fill_brewer(
    name = expression(paste(gamma^"b", ~"(Number of species / ha)")),
    palette = "YlOrRd",
    breaks = labels
  ) + # Using the labels directly here, since they represent the bins now.
  ggplot2::geom_sf(data = amazon_countries, fill = NA, color = "forestgreen", size = 10) +
  ggplot2::guides(fill = guide_legend(label.position = "bottom", title.position = "top", nrow = 1)) +
  ggplot2::theme(
    panel.grid.major = element_line(colour = "white"),
    panel.grid.minor = element_line(colour = "white"),
    panel.background = element_blank(),
    strip.background = element_rect(fill = NA),
    axis.line = element_blank(), axis.ticks = element_blank(),
    axis.title = element_blank(), axis.text = element_blank(),
    legend.title = element_text(hjust = 0.5, size = 22, face = "bold"),
    legend.position = "bottom", legend.key.width = unit(2, "cm"),
    legend.margin = margin(t = -0.5, r = 0, b = 0.2, l = 0, unit = "cm"),
    legend.text = element_text(size = 20, face = "bold")
  )

ggplot2::ggsave(filename = "plots/calibration/1043SitesModel/map_gamma_b.png", plot = plot, width = 8, height = 6)









fig <- calib_df %>%
  st_transform(4326) %>%
  ggplot() +
  geom_sf(aes(fill = tree_richness_ha)) +
  scale_fill_viridis_c(name = "Number of species / ha")

# Save plot to PDF
ggsave("plots/gamma_b_paper.pdf", fig)

calib_df %>% as_tibble()%>% select(tree_richness_ha) %>% write_csv("data/beta.csv")
