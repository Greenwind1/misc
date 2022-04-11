# https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-A31-v2_2.html#prefecture13

library(sf)
library(leaflet)
library(rmapshaper)
library(mapview)
library(broman)
library(tidyverse)
library(stringr)
library(extrafont)  # "Candara"

theme_set(theme_minimal(base_family = "Candara"))
options(dplyr.summarise.inform = TRUE)

col.os <- "#414a4c"  # Outer Space
col.tb <- "#77dde7"  # Turquoise Blue
col.pb <- "#1ca9c9"  # Pasicif Blue

map.sf <- sf::read_sf(
    "input/Administrative_district_Tokyo_2021/N03-21_13_210101.shp"
    # "input/Administrative_district_Shizuoka_2021/N03-21_22_210101.shp"
) %>% rmapshaper::ms_filter_islands(min_area = 1e8)

flood.sf <- sf::read_sf("input/Flood_Tokyo_2020/A31-12_13.shp")

# p <-  ggplot() + 
#     geom_sf(data = map.sf, 
#             color = col.gr, size = 0.2, alpha = 0.5) + 
#     geom_sf(data = flood.sf[1], 
#             color = col.pb, fill = col.tb, size = 0.1, alpha = 0.5)
# plot(p)


# Setting Coordinate Reference Systems (CRS) in R
# https://www.paulamoraga.com/book-geospatial/sec-spatialdataandCRS.html#setting-coordinate-reference-systems-in-r
m <- leaflet(data = flood.sf %>% 
                 st_transform("+proj=longlat +datum=WGS84")) %>%
    addProviderTiles(
        provider = "OpenStreetMap.HOT", 
        # provider = "CartoDB.PositronNoLabels", 
        options = providerTileOptions(opacity = 0.5)
    ) %>% 
    setView(lng = 139.700, lat = 35.695, zoom = 15) %>%
    addPolygons(
        fillColor = col.tb, fillOpacity = .5, 
        stroke = TRUE, color = col.pb, 
        weight = 1.0, dashArray = "1", opacity = .25 
    )
m

mapshot(
    x = m, file = "fig/Flood-map_Shinjuku-district.png",
    remove_controls = c("zoomControl"),
    vheight = 1300, vwidth = 1300,  # pixel size
)

m <- leaflet(data = flood.sf %>% 
                 st_transform("+proj=longlat +datum=WGS84")) %>%
    addProviderTiles(
        provider = "OpenStreetMap.HOT", 
        # provider = "CartoDB.PositronNoLabels", 
        options = providerTileOptions(opacity = 0.5)
    ) %>% 
    setView(lng = 139.700, lat = 35.695, zoom = 12) %>%
    addPolygons(
        fillColor = col.tb, fillOpacity = .5, 
        stroke = TRUE, color = col.pb, 
        weight = 1.0, dashArray = "1", opacity = .25 
    )
m

mapshot(
    x = m, file = "fig/Flood-map_Central-Tokyo.png",
    remove_controls = c("zoomControl"),
    vheight = 1300, vwidth = 1300,  # pixel size
)
