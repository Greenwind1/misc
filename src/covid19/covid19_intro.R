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
    "Ukraine", 
    "Belarus", 
    "Kazakhstan", 
    "Poland", 
    "Finland", 
    "Norway", 
    "Latvia", 
    "Estonia", 
    "Georgia", 
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
g <- grid::rasterGrob(img, 
                      width = unit(2, "cm"), 
                      height = unit(2, "cm"), 
                      interpolate=TRUE)

img2 <- png::readPNG("fig/twitter.png")
g2 <- grid::rasterGrob(img2, 
                       width = unit(0.4, "cm"),
                       height = unit(0.4, "cm"),
                       interpolate = FALSE)

x %>% filter(administrative_area_level_1 %in% country.sel) %>% 
    filter(date == "2022-02-23") %>% 
    ggplot() + 
    ggpattern::geom_col_pattern(
        mapping = aes(
            x = administrative_area_level_1, 
            y = confirmed, 
            pattern_fill = administrative_area_level_1,
            # pattern_filename = administrative_area_level_1
        ),
        # pattern = "image", 
        # pattern_type = "tile", 
        pattern = "stripe", 
        # pattern_type = "tile", 
        pattern_fill = "#7DA760", 
        pattern_colour = crayons()["Cadet Blue"],
        # pattern_scale = 2, 
        pattern_spacing = 0.025, 
        fill = crayons()["Cadet Blue"], 
        color = "white"
    ) + 
    # scale_y_continuous(trans = "log10") + 
    # scale_pattern_filename_discrete(choices = img_files) + 
    coord_flip() + 
    annotation_custom(grob = g, 
                      xmin = 5, xmax = 5, ymin = 1.1e7, ymax = 1.1e7) + 
    annotate(geom = "text", x = 1.5, y = 1.5e7, label = "@Maxwell_110",
             size = 3, family = "Candara") + 
    annotation_custom(grob = g2, 
                      xmin = 1.5, xmax = 1.5, ymin = 1.38e7, ymax = 1.38e7) + 
    theme(legend.position = "none") + 
    labs(x = NULL, 
         y = NULL, 
         title = "Confirmed COVID-19 Cases (as of Feb.23.2022)", 
         # caption = "@Maxwell_110", 
         subtitle = "https://covid19datahub.io/")

ggsave("fig/covid19_confirmed.png", width = 7.5, height = 5, dpi = 200, 
       bg = "transparent")
