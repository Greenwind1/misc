# install the package
# install.packages("COVID19")

library(COVID19)
library(tidyverse)
library(ggplot2)
library(directlabels)

x <- covid19(verbose = FALSE)

unique(x$administrative_area_level_1)

x %>% filter(administrative_area_level_1 == "Japan" | 
                 administrative_area_level_1 == "Diamond Princess") %>% 
    ggplot(data = ., aes(x = date, y = confirmed)) +
    geom_point(aes(color = id)) +
    geom_line(aes(color = id)) +
    geom_dl(aes(label = administrative_area_level_1), 
            method = list("last.points", cex = .75, hjust = 1, vjust = 1)) +
    scale_y_continuous(trans = "log10") +
    theme_light() +
    theme(legend.position = "none") +
    ggtitle("Confirmed cases (log scale)") +
    ggsave("./codes/covid19/japan-confirmed.png", dpi = 100)
