# Packages ----
# Data manipulation
library(tidyverse)
library(data.table)
library(janitor)  # janitor::row_to_names()
library(DT)
library(stringr)  # in tidyverse
library(lubridate)  # in tidyverse

# Visualisation
library(ggplot2)  # RColorBrewer::display.brewer.all()
library(ggalt)  # geom_encircle
library(ggimage)  # geom_subview
library(patchwork)
library(broman)  # plot_crayons()
library(ggsci)  # Journal palette
library(rcartocolor)  # display_carto_all(); https://bit.ly/3Itq5kB
library(PrettyCols)  # view_all_palettes(colourblind_friendly = TRUE)
library(extrafont)  # fonttable(); "Candara"
library(latex2exp)  # example: latex2exp::TeX("Equation: $\\lambda$")

source(file = "utility/environments.R")
source(file = "utility/helper_functions.R")

fn.prefix <- "playground"

df <- read_csv("input/stock_sentiment/stock_sentiment_toyota_gpt3-5.csv")

caption_text  <- str_glue("{x.glue}: @Maxwell_110")
showtext_auto(enable = TRUE)

col.toyota <- "#d81523"

p <- df %>% 
    mutate(LLM_sentiment = as.factor(LLM_sentiment)) %>% 
    ggplot() + 
    geom_bar(aes(x = LLM_sentiment), width = 0.5, color = col.toyota, fill = col.toyota) + 
    labs(title = "Sentiment analysis for TOYOTA in Jan.2024", 
         subtitle = "using OpenAI and EODHD APIs: https://eodhd.com/", 
         caption = caption_text) + 
    theme(
        text = element_text(family = font.base.showtext, color = col.os), 
        plot.title = element_text(family = "cinzel", 
                                  color = col.toyota, size = 35), 
        plot.subtitle = element_text(family = "cinzel", 
                                     color = col.toyota, size = 30), 
        plot.caption = element_markdown(family = "cinzel", 
                                        color = col.toyota, size = 20), 
        axis.title = element_text(family = "cinzel", 
                                  color = col.toyota, size = 25), 
        axis.text = element_text(family = "cinzel", 
                                 color = col.toyota, size = 15),
        # plot.background = element_rect(fill = col.bg), 
        panel.grid.major = element_line(color = "gray80", linewidth = 0.1), 
        panel.grid.minor = element_blank()
    )

p

ggsave("fig/playground/fig1.jpg", plot = p, dpi = 300)
