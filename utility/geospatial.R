# Utility for geo-spatial analysis

libs <- c(
    "sf",  # read_sf(), st_transform(), ...
    "spdep",  # helpful functions for skater library
    "rmapshaper",  # helpful functions for mapping
    "sfheaders",  # sf_to_df(), sf_remove_holes()
    "ggsn",  # scalbar()
    # "rayshader",  # 3D plot
    "tmap",  # map viewer
    "ggmap",  # map viewer, googlemap (need to register)
    "leaflet",  # map viewer, providers()
    "mapview"  # map viewer
)

# load libraries
invisible(lapply(libs, library, character.only = T))


# Environments ----
crs.base <- "+proj=longlat +datum=WGS84"
register_google(key = readRDS(file = "api/google-api.RDS"))

col.bg <- "#262A33"
col.fg <- "#393F4D"


# Helper Functions ----
# For drawing scale bar, use ggsn::scalebar().
arrow.df <- function(xb, yb, len) {
    s <- len
    arrow.x = c(0, 0.5, 1, 0.5, 0) - 0.5
    arrow.y = c(0, 1.7  , 0, 0.5, 0)
    adata <- data.frame(aX = xb + arrow.x * s, aY = yb + arrow.y * s)
    return(adata)
}

get.country.sf <- function(year = "2016",
                           epsg = "4326",
                           resolution = "10",
                           country = "Japan", 
                           crs = crs.base, ...) {
    
    country_sf <- giscoR::gisco_get_countries(
        year = year,
        epsg = epsg,
        resolution = resolution,
        country = country, 
        ...
    )
    
    country_transformed <- st_transform(country_sf, crs = crs)
    
    return(country_transformed)
}
