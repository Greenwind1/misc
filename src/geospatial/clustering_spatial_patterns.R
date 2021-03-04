# CLUSTERING SIMILAR SPATIAL PATTERNS
# https://nowosad.github.io/post/motif-bp5/

library(sf)
library(stars)
library(motif)
library(tmap)
library(dplyr)
library(readr)
library(osfr)

# Download Datasets ----
# osf_retrieve_node("xykzv") %>%
#     osf_ls_files(n_max = Inf) %>%
#     osf_download(path = "input",
#                  conflicts = "overwrite")

# LAND COVER AND LANDFORMS IN AFRICA ----
lc <- read_stars("input/land_cover.tif")
lf <- read_stars("input/landform.tif")

lc_palette_df <- read_csv("input/lc_palette.csv")
lf_palette_df <- read_csv("input/lf_palette.csv")
names(lc_palette_df$color) = lc_palette_df$value
names(lf_palette_df$color) = lf_palette_df$value

# Visualize with tmap ----
tm_lc <- tm_shape(lc) +
    tm_raster(style = "cat",
              palette = lc_palette_df$color,
              labels = lc_palette_df$label,
              title = "Land cover:") +
    tm_layout(legend.position = c("LEFT", "BOTTOM"))
tm_lc

tm_lf <- tm_shape(lf) +
    tm_raster(style = "cat",
              palette = lf_palette_df$color,
              labels = lf_palette_df$label,
              title = "Landform:") +
    tm_layout(legend.outside = TRUE)
tm_lf

# CLUSTERING SPATIAL PATTERNS ----
eco_data <- c(lc, lf)
eco_signature <- lsp_signature(eco_data,
                               type = "incove", 
                               window = 300)

# DISTANCE MATRIX ----
eco_dist <- lsp_to_dist(eco_signature, dist_fun = "jensen-shannon")
class(eco_dist)

# HIERARCHICAL CLUSTERING ----
eco_hclust <- hclust(eco_dist, method = "ward.D2")
plot(eco_hclust)
clusters <- cutree(eco_hclust, k = 8)

# CLUSTERING RESULTS ----
eco_grid_sf <- lsp_add_clusters(eco_signature,
                                clusters)
tm_clu <- tm_shape(eco_grid_sf) +
    tm_polygons("clust", 
                style = "cat",
                palette = "Set2",
                title = "Cluster:") +
    tm_layout(legend.position = c("LEFT", "BOTTOM"))
tm_clu

eco_grid_sf2 <- eco_grid_sf %>%
    dplyr::group_by(clust) %>%
    dplyr::summarize()

tm_shape(eco_data) +
    tm_raster(style = "cat",
              palette = list(lc_palette_df$color, lf_palette_df$color)) +
    tm_facets(ncol = 2) +
    tm_shape(eco_grid_sf2) +
    tm_borders(col = "black") +
    tm_layout(legend.show = FALSE,
              title.position = c("LEFT", "TOP"))
