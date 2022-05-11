# Packages ----
# Data manipulation
library(tidyverse)
library(data.table)
library(janitor)  # janitor::row_to_names()
library(DT)
library(stringr)
library(lubridate)

# Visualisation
library(ggplot2)
library(ggalt)  # geom_encircle
library(patchwork)
library(broman)  # plot_crayons()
library(rcartocolor)  # display_carto_all(); https://bit.ly/3Itq5kB
library(extrafont)  # "Candara"

# Interactivity
library(plotly)
library(crosstalk)

# Descriptive Statistics
library(mice)
library(ggmice)  # see src/imputation/ggmice_XX.Rmd
library(psych)
library(gtsummary)

# GeoSpatial Analysis
library(sf)  # simple feature
library(spdep)  # helpful functions for mapping
library(rmapshaper)  # helpful functions for mapping
library(tmap)  # map viewer
library(leaflet)  # map viewer
library(mapview)  # map viewer

# ML
# library(tidymodels)
# tidymodels::tidymodels_prefer()

# Multi Level Model
library(lme4)
library(lmerTest)
library(coefplot)
# library(multilevelmod)

# Stan
library(rstan)
library(bayesplot)
library(broom)
library(broom.mixed)

# Parallel
library(purrr)
library(foreach)
library(doParallel)

source(file = "utility/helper_functions.R")


# Environment ----
# rgb(73, 81, 112, maxColorValue = 255)
col.tw <- "#dbd7d2"  # Timberwolf
col.os <- "#414a4c"  # Outer Space
col.rp <- "#7851a9"  # Royal Purple
col.pb <- "#1ca9c9"  # Pacific Blue
col.cb <- "#b0b7c6"  # Cadet Blue
col.el <- "#ceff1d"  # Electric Lime
col.cg <- "#00cc99"  # Caribbean Green
col.mt <- "#ff8243"  # Mango Tango
col.rm <- "#e3256b"  # Razzmatazz
col.sl <- "#fc2847"  # Scarlet
col.plos.yellow <- "#D6E13D"  # PLOS ONE Yellow
col.plos.pink <- "#CF00A3"  # PLOS ONE Pink

font.base <- "Candara"
# font.base <- "Times New Roman"
theme_set(theme_minimal(base_family = font.base))
options(dplyr.summarise.inform = TRUE)


# Load DataSets ----
