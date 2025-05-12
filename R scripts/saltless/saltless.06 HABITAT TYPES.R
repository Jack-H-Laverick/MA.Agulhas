# Sources
# Domain - Inshore/Offshore domain defined by bathymetry in ./bathymetry/bathymetry.01 DEFINE DOMAIN-BIG.R
#
# Sediment habitat classifications - `Birch et al. (1986): Texture and composition of surficial sediments`
# - this map was originally in a pdf form. The pdf was converted to an image and the image was
#   manually georeferenced in QGIS (using thin-line-splice algorithm). Colours were adjusted in
#   GIMP photo processing software, and polygons were created from the raster in QGIS.
#
# Rock habitat classification - SANBI Marine Ecosystem Map (2018).
# - https://bgis.sanbi.org/SpatialDataset/Detail/2681

#### Set up ####

rm(list = ls())

packages <- c("MiMeMo.tools", "stars") # List packages
lapply(packages, library, character.only = TRUE) # Load packages
source("./R scripts/@_Region file.R")
library(ggpattern)

domains <- readRDS("./Objects/Domains.rds") %>% # Load SF polygons of the MiMeMo model domains
    st_transform(crs = 9822) # Moved to CRS EPSG:9822 as it is equal-area to allow for accurate area calculations

sanbi_ecosystem_map <- st_read("./Data/spatial/SANBI-MarineEcosystemMap2018/MarineEcosystemMap2018_beta.shp")
sanbi_ecosystem_map <- st_transform(sanbi_ecosystem_map, crs = 9822)

sanbi_substratum <- filter(
    sanbi_ecosystem_map,
    str_detect(sanbi_ecosystem_map$Substratum, "Rocky")
)

sediment_polygons <- st_read("./Data/spatial/SA_sediment_features.gpkg") # Import full sediment grid
sediment_polygons <- st_transform(sediment_polygons, crs = 9822)
sediment_labels <- c(
    "sandy_gravel" = 1,
    "sand" = 2,
    "muddy_sand" = 3,
    "sandy_mud" = 4,
    "mud" = 5,
    "gravelly_mud" = 6,
    "gravel_mud-sand-gravel" = 7
)
match_sediment_name <- function(x) names(which(sediment_labels == x))
sediment_polygons$surficial_sediment_class <- sapply(
    sediment_polygons$surficial_sediment,
    match_sediment_name
)

mud_labels <- c("sandy_mud", "mud", "gravelly_mud")
sand_labels <- c("muddy_sand", "sand")
# Note gravel_mud-sand-gravel is a single sediment label as the colours of 'grave' and 'mud-sand-gravel'
# were not distinguishable on the Birch et al. 1986 source map.
gravel_labels <- c("sandy_gravel", "gravel_mud-sand-gravel")

sediment_polygons$coarse_class <- ""
sediment_polygons$coarse_class <- ifelse(sediment_polygons$surficial_sediment_class %in% mud_labels, "mud", sediment_polygons$coarse_class)
sediment_polygons$coarse_class <- ifelse(sediment_polygons$surficial_sediment_class %in% sand_labels, "sand", sediment_polygons$coarse_class)
sediment_polygons$coarse_class <- ifelse(sediment_polygons$surficial_sediment_class %in% gravel_labels, "gravel", sediment_polygons$coarse_class)

st_erase <- function(x, y) st_difference(x, st_union(st_combine(y))) # Helper function that removes all of y from x

# Merge sediment and rock polygons to create a single set (first subset by the domain polygons)
sub_sediment <- st_intersection(sediment_polygons, domains) %>%
    mutate(habitat_class = coarse_class) %>%
    group_by(habitat_class, Shore) %>%
    summarise(geometry = st_union(geom)) %>% # Convert polygons into a multipolygon for each combination
    ungroup() %>%
    st_make_valid()

sub_rocks <- st_intersection(sanbi_substratum, domains) %>% # here we can choose to use rock polygons from the BroadEcosy or Substratum columns.
    mutate(habitat_class = "rock") %>%
    group_by(habitat_class, Shore) %>%
    summarise(geometry = st_union(geometry)) %>% # Convert polygons into a multipolygon for each combination
    ungroup() %>%
    st_make_valid()

# Remove the areas that are rock from the `sub_sediment` polygons
sediment_minus_rock <- st_erase(sub_sediment, sub_rocks)

# Combine the sediment minus rock areas and rock polygons
habitats <- rbind(sediment_minus_rock, sub_rocks) %>%
    rename(Habitat = "habitat_class") %>%
    st_make_valid()
alpha_values <- c("Inshore" = 0.2, "Offshore" = 1.0)
ggplot() +
    geom_sf(data = habitats, aes(fill = Habitat, alpha = Shore)) +
    scale_alpha_manual(values = alpha_values)

#### Calculate proportion of model zones in each habitat - before converting reprojecting ####
proportions <- habitats %>%
    mutate(Cover = as.numeric(st_area(.))) %>% # Measure the area of each habitat type
    st_drop_geometry() %>% # Drop SF formatting
    mutate(Cover = Cover / sum(Cover)) %>% # Calculate the proportion of the model zone in each sediment polygon
    rename(Bottom = Habitat)

habitats <- st_transform(habitats, crs = crs) %>%
    st_make_valid()
saveRDS(habitats, "./Objects/Habitats.rds")
saveRDS(proportions, "./Objects/Sediment area proportions.rds")

ggplot(proportions) +
    geom_col(aes(x = Shore, y = Cover * 100, fill = Bottom), position = "Dodge") +
    theme_minimal() +
    theme(
        panel.grid.major.x = element_blank(),
        legend.position = "top"
    ) +
    viridis::scale_fill_viridis(discrete = TRUE, name = "Sediment class:") +
    labs(y = "Cover (%)", x = NULL, caption = "Percentage of model domain in each habitat class")
ggsave("./Figures/saltless/Habitat types.png", width = 16, height = 8, units = "cm")
