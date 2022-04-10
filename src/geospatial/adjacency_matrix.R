# IWANAMI DATA SCIENCE Vol.4
# pp.62-63
# Adjacency matrix for CAR model

library(sf)
library(spdep)
library(rmapshaper)
library(tidyverse)
library(stringr)
library(extrafont)  # "Candara"

theme_set(theme_minimal(base_family = "Candara"))
options(dplyr.summarise.inform = TRUE)

map.sf <- sf::read_sf(
    "input/Administrative_district_Tokyo_2021/N03-21_13_210101.shp"
    # "input/Administrative_district_Shizuoka_2021/N03-21_22_210101.shp"
)

map.sf %>% 
    filter(str_detect(N03_004, "区")) %>% 
    # rmapshaper::ms_filter_islands(min_area = 1e8) %>% 
    ggplot() + 
    geom_sf()

# A neighbours list with class nb. See card for details of “nb” objects.
map.sf.nb <- spdep::poly2nb(map.sf)
summary(map.sf.nb)

num <- rep(NA, length(map.sp))

for (i in 1:length(map.sp)) {
    num[i] <- length(map.sp.nb[[i]])
}

adj <- unlist(map.sp.nb)

# cat(num)
# cat(adj)
