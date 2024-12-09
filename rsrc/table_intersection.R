library(sf)
library(glue)
library(terra)
library(tidyverse)
library(conflicted)

# DATA INPUT


# 1043 SITES MODEL CALIBRATION VARIABLES
load("data/calibration/calibration_1043_sites.Rdata")

# AMAZON BIOME VECTOR DATA
load("data/clean/amazon_biome.Rdata")

load("data/clean/amazon_countries.Rdata")



gdp <- read.csv("data/calibration/imf_gdp.csv")

amazon_countries <- amazon_countries %>%
  left_join(gdp, by = "shapeName") %>%
  mutate(gdp=GDP.Billions.*1e9)

gamma_fit <- read.csv("data/calibration/hmc/gamma_fit_1043.csv")%>%
  mutate(id = row_number())

calib_df <- calib_df %>%
  left_join(gamma_fit, by = "id")

calib_df <- calib_df %>%
  mutate(
    x_2017 = calib_df$gamma_fit * area_forest_2017,
    U_est = calib_df$gamma_fit* z_2017 
  )


load("data/calibration/gamma_b_df.Rdata")
gamma_b_df <- st_drop_geometry(gamma_b_df)

calib_df <- calib_df %>%
  left_join(gamma_b_df, by = "id")




calib_df <- calib_df %>%
  st_transform(4326)

amazon_biome <- amazon_biome %>%
  st_transform(4326)




amazon_countries$country_id <- seq_len(nrow(amazon_countries))

intersections <- st_intersection(calib_df, amazon_countries)

intersections <- intersections %>%
  mutate(overlap_area = as.numeric(st_area(.)))%>%
  group_by(id) %>%  
  mutate(
    total_overlap_area = sum(overlap_area, na.rm = TRUE) 
  ) %>%
  ungroup()   %>%
  mutate(share_area=overlap_area/total_overlap_area)



site_country_counts <- intersections %>%
  st_drop_geometry() %>%  
  group_by(id) %>%  
  summarize(
    num_countries = n_distinct(country_id) 
  )


intersections <- intersections %>%
  left_join(site_country_counts, by = "id") %>%  
  mutate(crosses_countries = ifelse(num_countries > 1, 1, 0)) 



intersections <- st_drop_geometry(intersections)

summary_dataset <- intersections %>%
  group_by(country_id) %>%  # Group by country_id
  summarize(
    sum_area_forest = sum(area_forest_2017*share_area,na.rm = TRUE)/1e6,
    sum_x_2017 = sum(x_2017*share_area, na.rm = TRUE),  # Sum x_2017
    sum_z_2017 = sum(z_2017*share_area, na.rm = TRUE),  # Sum z_2017
    sum_zbar_2017 = sum(zbar_2017*share_area, na.rm = TRUE),  # Sum zbar_2017
    sum_U_est = sum(U_est*share_area,na.rm=TRUE),
    weighted_avg_tree_richness = sum(tree_richness_ha * area_forest_2017*share_area, na.rm = TRUE) / 
      sum(area_forest_2017*share_area, na.rm = TRUE)
  ) %>%
  mutate(
    ratio_z_2017_zbar_2017 = sum_z_2017 / sum_zbar_2017,  # Calculate the ratio
    sum_U_25transfer=sum_U_est*25
  ) %>%
  ungroup()  # Remove grouping after summarization



print(summary_dataset)



