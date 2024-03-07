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
library(GGally)  # https://ggobi.github.io/ggally/reference/index.html

# Interactivity
library(plotly)
library(crosstalk)

# Descriptive Statistics
library(mice)
library(ggmice)  # see src/imputation/ggmice_XX.Rmd
library(psych)
library(skimr)  # skim: an alternative to glimpse
library(gtsummary)  # tbl_summary; tbl_regression; https://cran.r-project.org/web/packages/gtsummary/vignettes/tbl_regression.html
library(summarytools)  # dfSummary; stview

# ML
library(tidymodels)  # check src/tidymodels files
library(vip)
library(ale)  # XAI
library(broom.mixed)
library(tictoc)  # timer
tidymodels::tidymodels_prefer()  # override conflicting methods of other pkgs

# Time Series
library(timetk)
library(tsibble)  # https://otexts.com/fpp3/index.html
library(fable)  # https://otexts.com/fpp3/index.html

# GeoSpatial Analysis
source(file = "utility/geospatial.R")

# Networks
library(ggraph)
library(tidygraph)
library(BCDAG)  # Bayesian Causal Networks

# Multi Level Model
library(lme4)
library(lmerTest)
library(coefplot)  # forest plot
library(dotwhisker)  # forest plot
library(performance)  # Generic R^2, ICC, Over-dispersion, Heteroschedasticity
# library(multilevelmod)
library(ggstats)  # ggcoef_model() for forest plot

# Stan
library(rstan)
library(bayesplot)
library(broom)
library(broom.mixed)

# Parallel
library(purrr)
library(foreach)
library(doParallel)

# pacman::p_load()

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
glimpse(penguins)
p <- ggmice::plot_pattern(data = penguins, 
                          vrb = "all", square = TRUE, 
                          rotate = FALSE, cluster = NULL)
p + theme_minimal(base_family = font.base) + 
    theme(panel.grid = element_blank())


# ----
