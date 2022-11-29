# Take https://gist.github.com/dakvid/3bc77f4d3c2da742113229ba11b5cc8b
# and make it 3d with rayshader

library(glue)
library(stringr)
library(dplyr)
library(sf)
library(rmapshaper)
library(ggplot2)
library(rayshader)
library(av)

# Kontur data -----------------------------------

# boy, am I glad I have 64GB of RAM!
# or that I new about the country slices earlier: https://data.humdata.org/organization/kontur

kontur_nz <-
  readRDS("Day_21_Kontur/kontur_nz.rds")
CRS_KONTUR <- st_crs(kontur_nz)

# # https://data.humdata.org/dataset/kontur-population-dataset
# kontur <- 
#   st_read("kontur_population_20220630.gpkg")
# CRS_KONTUR <- st_crs(kontur)

# https://datafinder.stats.govt.nz
ta <-
  st_read("territorial-authority-2021-clipped-generalised.gpkg")
CRS_NZ <- st_crs(ta)

nz <-
  ta |>
  # Ignore Chathams etc
  filter(! TA2021_V1_00 %in% c("067", "999")) |>
  count() |>
  ms_simplify(keep = 0.02) |>
  st_transform(crs = CRS_KONTUR)
# 
# kontur_nz <- 
#   kontur |> 
#   mutate(is_nz = lengths(st_intersects(geom, nz)) > 0) |> 
#   filter(is_nz)
# saveRDS(kontur_nz, "Day_21_Kontur/kontur_nz.rds")
# rm(kontur); gc()


# Stats NZ grids --------------------------------

# https://datafinder.stats.govt.nz/data/category/grids/
sq500 <- 
  st_read("stats_grid/500m/new-zealand-2018-estimated-resident-population-statistical-g.gpkg")
sq1000 <- 
  st_read("stats_grid/1000m/new-zealand-2018-estimated-resident-population-statistical-g.gpkg")


# Extract Wellington City -----------------------

# And calculate population density, since the shapes aren't the same size

wc_geo <- 
  ta |> 
  filter(TA2021_V1_00_NAME == "Wellington City") |> 
  ms_simplify(keep = 0.05)

wc_500 <- 
  sq500 |> 
  mutate(is_wc = lengths(st_intersects(geom, wc_geo)) > 0) |> 
  filter(is_wc) |> 
  mutate(grid_area = geom |> st_area() |> as.numeric(),
         pop_density = ERP / grid_area * 1000000)

wc_1000 <- 
  sq1000 |> 
  mutate(is_wc = lengths(st_intersects(geom, wc_geo)) > 0) |> 
  filter(is_wc) |> 
  mutate(grid_area = geom |> st_area() |> as.numeric(),
         pop_density = ERP)

wc_kontur <- 
  kontur_nz |> 
  mutate(is_wc = 
           lengths(
             st_intersects(
               geom,
               wc_geo |> st_transform(crs = CRS_KONTUR)
            )) > 0) |> 
  filter(is_wc) |> 
  st_transform(crs = CRS_NZ) |> 
  mutate(grid_area = geom |> st_area() |> as.numeric(),
         pop_density = population / grid_area * 1000000)


# Map ------------------------------------

MAP_FONT <- "Poppins"

SHIFT_LEFT <- c(-25000, 0)
SHIFT_RIGHT <- c(25000, 0)
shift_map <- 
  function(df_sf, shift_xy) {
    df_sf |> 
      mutate(geom = st_geometry(df_sf) + shift_xy) |> 
      st_set_crs(CRS_NZ)
  }

wc_gg <-
  ggplot() +
  # Giving lines a colour gives them height, which it didn't use to do
  # geom_sf(data = wc_geo,
  #         aes(geometry = geom),
  #         linewidth = 2, colour = "black", fill = NA) +
  # geom_sf(data = wc_geo |> shift_map(SHIFT_LEFT),
  #         aes(geometry = geom),
  #         linewidth = 2, colour = "black", fill = NA) +
  # geom_sf(data = wc_geo |> shift_map(SHIFT_RIGHT),
  #         aes(geometry = geom),
  #         linewidth = 2, colour = "black", fill = NA) +
  geom_sf(data = wc_1000,
          aes(geometry = geom, fill = pop_density),
          colour = NA,
          # colour = "black", linewidth = 1,
          alpha = 0.9) +
  geom_sf(data = wc_500 |> shift_map(SHIFT_RIGHT),
          aes(geometry = geom, fill = pop_density),
          colour = NA,
          # colour = "black", linewidth = 1,
          alpha = 0.9) +
  geom_sf(data = wc_kontur |> shift_map(SHIFT_LEFT),
          aes(geometry = geom, fill = pop_density),
          colour = NA,
          # colour = "black", linewidth = 1,
          alpha = 0.9) +
  geom_text(
    aes(x = 1740000, y = 5440000, geometry = NULL),
    label = "Stats NZ\nStatistical Grid\n1km prototype",
    size = 4, colour = "black", hjust = 0.5,
    family = MAP_FONT
  ) +
  geom_text(
    aes(x = 1740000 + SHIFT_RIGHT[1], y = 5440000, geometry = NULL),
    label = "Stats NZ\nStatistical Grid\n500m prototype",
    size = 4, colour = "black", hjust = 0.5,
    family = MAP_FONT
  ) +
  geom_text(
    aes(x = 1740000 + SHIFT_LEFT[1], y = 5440000, geometry = NULL),
    label = "Kontur\nPopulation Density\n400m H3",
    size = 4, colour = "black", hjust = 0.5,
    family = MAP_FONT
  ) +
  scale_fill_viridis_c(guide = guide_colorbar(
    barwidth = 15, barheight = 0.5,
    title = "Population per square km", title.position = "top",
    title.hjust = 0.5
  )) +
  labs(title = "Wellington City Population Density") +
  coord_sf(datum = NULL) +
  theme_void(base_family = MAP_FONT) +
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "white", colour = "white"),
        panel.background = element_rect(fill = "white", colour = "white"),
        plot.title = element_text(hjust = 0.5, family = MAP_FONT,
                                  size = 32))

plot_gg(wc_gg,
        width = 9,
        height = 7,
        scale = 250,
        windowsize = c(1600,866),
        multicore = TRUE,
        phi = 15, theta=0,
        fov = 30, zoom = 0.38,
        background = "#afceff")

for(i in seq(1, 360, by=1)) {
  render_camera(theta = i - 66)
  render_snapshot(filename = glue("Day_28_3d/frames/kontur3d_frame_{str_pad(i, 3, pad = '0')}.png"))
}

av_encode_video(list.files(path = "Day_28_3d/frames", pattern = "png", full.names = TRUE),
                framerate = 30,
                output = "Day_28_3d/Day_28_kontur3d.mp4")
