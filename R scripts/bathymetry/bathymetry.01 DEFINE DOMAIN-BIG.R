# Create an object defining the geographic extent of the model domain

#### Set up ####

rm(list = ls())

Packages <- c("tidyverse", "sf", "stars", "rnaturalearth", "raster") # List handy packages
lapply(Packages, library, character.only = TRUE) # Load packages

source("./R scripts/@_Region file.R") # Define project region

world <- ne_countries(scale = "medium", returnclass = "sf") %>% # Get a world map
    st_transform(crs = crs) # Assign polar projection

GEBCO <- raster("../Shared data/GEBCO_2020.nc")
GFW <- raster("../Shared data/distance-from-shore.tif")

crop <- as(extent(22, 37, -39, -26), "SpatialPolygons")
crs(crop) <- crs(GEBCO)

GEBCO <- crop(GEBCO, crop)
GFW <- crop(GFW, crop)

#### Polygons based on depth ####

Depths <- GEBCO
Depths[GEBCO >= 0 | GEBCO < -3000] <- NA

Depths[Depths < -50] <- -3000
Depths[Depths > -50] <- -50

Depths <- st_as_stars(Depths) %>%
    st_as_sf(merge = TRUE) %>%
    st_make_valid() %>%
    group_by(Elevation.relative.to.sea.level) %>%
    summarise(Depth = abs(mean(Elevation.relative.to.sea.level))) %>%
    st_make_valid()

ggplot(Depths) +
    geom_sf(aes(fill = Depth), alpha = 0.2) +
    theme_minimal()

#### Polygons based on distance ####

Distance <- GFW
Distance[GFW == 0 | GFW > 20] <- NA # Distance appears to be in KM not m as stated on the website.

Distance[is.finite(Distance)] <- 20 # Distance appears to be in KM not m as stated on the website.

Distance <- st_as_stars(Distance) %>%
    st_as_sf(merge = TRUE) %>%
    st_make_valid() %>%
    group_by(distance.from.shore) %>%
    summarise(Distance = (mean(distance.from.shore))) %>%
    st_make_valid()

ggplot() +
    geom_sf(data = Distance, fill = "red") +
    geom_sf(data = Depths, aes(fill = Depth), alpha = 0.2) +
    theme_minimal()

#### Expand inshore and cut offshore ####

sf_use_s2(F)

meld <- st_union(Distance, filter(Depths, Depth == 50)) %>%
    st_make_valid()

shrunk <- bind_rows(meld, filter(Depths, Depth == 3000)) %>%
    st_make_valid() %>%
    st_difference()

ggplot(shrunk) +
    geom_sf(aes(fill = Depth), alpha = 0.5)

#### Cut to region mask ####

clipped <- st_intersection(shrunk, st_transform(Region_mask, st_crs(shrunk))) %>%
    st_intersection(st_transform(st_read("./Data/eez/"), st_crs(shrunk))) %>%
    st_cast("POLYGON") %>%
    mutate(area = as.numeric(st_area(.))) %>%
    group_by(Depth) %>%
    slice_max(order_by = area) %>%
    ungroup() %>%
    sfheaders::sf_remove_holes()

ggplot(clipped) +
    geom_sf(aes(fill = Depth), alpha = 0.5)

#### Format to domains object ####

Domains <- transmute(clipped,
    Shore = ifelse(Depth == 50, "Inshore", "Offshore"),
    area = as.numeric(st_area(shrunk)),
    Elevation = exactextractr::exact_extract(GEBCO, shrunk, "mean")
) %>%
    st_transform(crs = crs)

saveRDS(Domains, "./Objects/Domains.rds")

map <- ggplot() +
    geom_sf(data = Domains, aes(fill = Shore), colour = NA) +
    #  geom_sf(data = Region_mask, colour = "red", fill = NA) +
    geom_sf(data = world, size = 0.1, fill = "black") +
    scale_fill_manual(values = c(Inshore = "yellow", Offshore = "yellow3"), name = "Zone") +
    zoom +
    theme_minimal() +
    #  theme(axis.text = element_blank()) +
    labs(caption = "Final model area") +
    NULL
ggsave_map("./Figures/bathymetry/Domains.png", map)

ggplot() +
    geom_stars(data = st_as_stars(GFW) %>% st_transform(crs)) +
    geom_sf(data = world, size = 0.1, fill = "white") +
    zoom +
    theme_minimal() +
    NULL

ggsave_map("./Figures/bathymetry/Distance.png", last_plot())

gfw_monthly <- arrow::read_parquet("../../Spatial Data/fishing_effort_data/Global_fishing_watch/fleet-monthly.parq")
gfw_monthly_total <- gfw_monthly %>%
    group_by(cell_ll_lon, cell_ll_lat) %>%
    summarise(hours = sum(hours))
gfw_monthly_total <- st_as_sf(gfw_monthly_total, coords = c("cell_ll_lon", "cell_ll_lat"), crs = 4326)
gfw_monthly_total <- st_intersection(gfw_monthly_total, Region_mask)
gfw_stars <- st_rasterize(gfw_monthly_total)
gfw_stars[gfw_stars > 150000] <- NA

ggplot() +
    geom_stars(data = log(gfw_stars)) +
    scale_fill_viridis_c() +
    geom_sf(data = Domains, aes(color = Shore), alpha = 0.3)
