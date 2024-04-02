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


# Plot Okinawa to another area
# https://rstudio-pubs-static.s3.amazonaws.com/775867_4cfbe983012a46cab16e53205452d3c8.html
shift_okinawa <- 
    function(data, 
             col_pref = "name", 
             pref_value = "Okinawa", 
             geometry = "geometry", 
             zoom_rate = 1.2, 
             pos = c(15, 3)) {
        row_okinawa <- data[[col_pref]] == pref_value
        geo <- data[[geometry]][row_okinawa]
        cent <- sf::st_centroid(geo)  # compute centroid
        geo2 <- (geo - cent) * zoom_rate + cent + pos
        data[[geometry]][row_okinawa] <- geo2
        return(sf::st_as_sf(data))
    }

layer_autoline_okinawa <- function(
        x = c(136.5, 136.5, 141),
        xend = c(136.5, 141, 145.5),
        y = c(25.5, 29.5, 33),
        yend = c(29.5, 33, 33),
        size = ggplot2::.pt / 15, 
        color = "gray50"
){
    ggplot2::annotate(
        geom = "segment", 
        x = x, 
        xend = xend, 
        y = y, 
        yend = yend, 
        size = .pt / 15, 
        color = color
    )
}


# ne.jpn.sf <- ne_states(country = "Japan", returnclass = "sf") %>%
#     rmapshaper::ms_filter_islands(min_area = 1e8) %>%
#     select(iso_3166_2, name, region, latitude, longitude)
# 
# ne.jpn.sf.shift <- shift_okinawa(ne.jpn.sf)
# ggplot(data = ne.jpn.sf.shift) + 
#     geom_sf() + 
#     layer_autoline_okinawa()
# ggsave(str_glue("tmp.jpg"), dpi = 300, width = 7, height = 7)
