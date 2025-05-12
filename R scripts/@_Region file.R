## Set repeated commands specific to the project region
implementation <- "South_Africa"

library(sf)

# EPSG <- rgdal::make_EPSG()
# EPSG2 <- filter(EPSG, str_detect(note, "Cape"))
crs <- 4326 # Specify the map projection for the project

lims <- c(xmin = 21, xmax = 37, ymin = -39, ymax = -25) # Specify limits of plotting window, also used to clip data grids

zoom <- coord_sf(xlim = c(lims[["xmin"]], lims[["xmax"]]), ylim = c(lims[["ymin"]], lims[["ymax"]]), expand = FALSE) # Specify the plotting window for SF maps in this region

ggsave_map <- function(filename, plot) {
    ggsave(filename, plot, scale = 1, width = 12, height = 10, units = "cm", dpi = 500, bg = "white")
} # Set a new default for saving maps in the correct size
pre <- list(scale = 1, width = 12, height = 10, units = "cm", dpi = 500) # The same settings if you need to pass them to a function in MiMeMo.tools

SDepth <- 50 # Shallow deep boundary
DDepth <- 800 # Deep zone maximum depth
ODepth <- 3000 # Depth of the seafloor at the outermost edge of the overhang
Distance <- 20 # Minimum distance from shore buffer for the inshore zone

#### bathymetry.5 MODEL DOMAIN ####

shape <- function(matrix, label = "DUMMY") {
    shape <- matrix %>%
        list() %>%
        st_polygon() %>%
        st_sfc() %>%
        st_sf(Region = label, .)
    st_crs(shape) <- st_crs(4326)
    shape <- st_transform(shape, crs = crs)
    return(shape)
} # Convert a matrix of lat-lons to an sf polygon

# square region mask with the bounding required to match southern benguela system domain
Region_mask <- matrix(
    c(
        22.501, -26.862,
        36.531, -26.862,
        36.531, -38.175,
        22.501, -38.175,
        22.501, -26.862
    ),
    ncol = 2, byrow = T
) %>%
    shape(label = implementation)

ggplot(Region_mask) +
    geom_sf()

#### bounds.2 MAKE TRANSECTS ####

## Polygons to mark which transects are along the open ocean-inshore boundary
# Need to confirm transect placements

# Inshore_Ocean1 <- matrix(c(
#     27.98, 27.98, 28.02, 28.02, 27.98, # Longitudes
#     -38.5, -32, -32, -38.5, -38.5
# ), ncol = 2, byrow = F) %>%
#     shape()

# Inshore_Ocean2 <- matrix(c(
#     14.3, 14.55, 16.3, 16.9, 16.8, 14.3, # Longitudes
#     -30, -30, -28.9, -28.1, -28.1, -30
# ), ncol = 2, byrow = F) %>%
#     shape()

# Inshore_ocean_boundaries <- rbind(Inshore_Ocean1, Inshore_Ocean2)

# rm(Inshore_Ocean1, Inshore_Ocean2)
