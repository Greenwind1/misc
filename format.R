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

# Networks
library(ggraph)
library(tidygraph)
library(BCDAG)  # Bayesian Causal Networks

# ML
library(tidymodels)
tidymodels::tidymodels_prefer()

# Multi Level Model
library(lme4)
library(lmerTest)
library(coefplot)
library(performance)  # Generic R^2, ICC, Over-dispersion, Heteroschedasticity
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


# Environment ----
source(file = "utility/environments.R")
source(file = "utility/helper_functions.R")
# icon.grob <- png_to_grob()

font.base <- "Candara"
# font.base <- "Times New Roman"
theme_set(theme_minimal(base_family = font.base))
options(dplyr.summarise.inform = TRUE)


# DataSet ----
data("penguins")  # modeldata from tidymodels
