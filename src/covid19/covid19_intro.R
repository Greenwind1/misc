# install the package
# install.packages("COVID19")

library(COVID19)
library(tidyverse)
library(ggplot2)
library(ggpattern)
library(png)
library(grid)
library(broman)
library(extrafont)
library(directlabels)

theme_set(theme_minimal(base_family = "Candara"))

x <- covid19(verbose = FALSE)

unique(x$administrative_area_level_1)

country.sel <- c(
    "Russia", 
    "Belarus", 
    "Kazakhstan", 
    "Ukraine", 
    "Georgia", 
    "Poland", 
    "Finland", 
    "Norway", 
    "Latvia", 
    "Estonia", 
    "Azerbaijan", 
    "China", 
    "Mongolia"
)

x %>% filter(administrative_area_level_1 == "Japan" | 
                 administrative_area_level_1 == "Diamond Princess") %>% 
    ggplot(data = ., aes(x = date, y = confirmed)) +
    geom_point(aes(color = id), size = 1) +
    geom_line(aes(color = id)) +
    geom_dl(aes(label = administrative_area_level_1), 
            method = list("last.points", cex = .75, hjust = 1, vjust = 1)) +
    scale_y_continuous(trans = "log10") +
    theme(legend.position = "none") + 
    labs(x = NULL, y = "Confirmed COVID-19 cases")
    ggtitle("Confirmed cases (log scale)")

ggsave("fig/covid19_japan-confirmed.png", width = 5, height = 3, dpi = 150)



img <- png::readPNG("fig/covid-19.png")
g0 <- grid::rasterGrob(img, 
                       width = unit(4, "cm"),
                       height = unit(4, "cm"),
                       interpolate = TRUE)
g1 <- grid::rasterGrob(img, 
                       width = unit(1, "cm"),
                       height = unit(1, "cm"),
                       interpolate = TRUE)
g2 <- grid::rasterGrob(img, 
                       width = unit(2, "cm"),
                       height = unit(2, "cm"),
                       interpolate = TRUE)

img2 <- png::readPNG("fig/twitter.png")
g3 <- grid::rasterGrob(img2, 
                       width = unit(0.4, "cm"),
                       height = unit(0.4, "cm"),
                       interpolate = FALSE)

x %>% filter(administrative_area_level_1 %in% country.sel) %>% 
    filter(date == "2022-02-23") %>% 
    mutate(factor(administrative_area_level_1, levels = country.sel)) %>% 
    ggplot() + 
    ggpattern::geom_col_pattern(
        mapping = aes(
            x = reorder(administrative_area_level_1, confirmed), 
            y = confirmed, 
            pattern_fill = administrative_area_level_1,
            # pattern_filename = administrative_area_level_1
        ),
        # pattern = "image", 
        # pattern_type = "tile", 
        pattern = "circle", 
        # pattern_type = "tile", 
        pattern_fill = "#0296CC", 
        pattern_colour = crayons()["Cadet Blue"],
        pattern_density = 0.5, 
        pattern_scale = 20,
        # pattern_spacing = 0.025, 
        fill = crayons()["Cadet Blue"], 
        color = "white"
    ) + 
    # scale_y_continuous(trans = "log10") + 
    # scale_pattern_filename_discrete(choices = img_files) + 
    coord_flip() + 
    annotation_custom(grob = g0, 
                      xmin = 5, xmax = 5, ymin = 1.1e7, ymax = 1.1e7) + 
    annotation_custom(grob = g1, 
                      xmin = 6.5, xmax = 6.5, ymin = 0.95e7, ymax = 0.95e7) + 
    annotation_custom(grob = g2, 
                      xmin = 6.3, xmax = 6.3, ymin = 1.26e7, ymax = 1.26e7) + 
    annotate(geom = "text", x = 1.5, y = 1.5e7, label = "@Maxwell_110",
             size = 3, family = "Candara") + 
    annotation_custom(grob = g3, 
                      xmin = 1.5, xmax = 1.5, ymin = 1.38e7, ymax = 1.38e7) + 
    theme(legend.position = "none") + 
    labs(x = NULL, 
         y = NULL, 
         title = "Confirmed COVID-19 Cases (as of Feb.23.2022)", 
         # caption = "@Maxwell_110", 
         subtitle = "https://covid19datahub.io/")

ggsave("fig/covid19_confirmed.png", width = 7.5, height = 5, dpi = 200, 
       bg = "transparent")
