# Remove library loading if necessary...
# Data manipulation
library(tidyverse)
library(data.table)
library(DT)
library(stringr)
library(lubridate)

# Visualisation
library(ggplot2)
library(patchwork)
library(broman)  # plot_crayons()
library(rcartocolor)  # display_carto_all(); https://bit.ly/3Itq5kB
library(gtsummary)
library(summarytools)
library(leaflet)  # map
library(extrafont)  # "Candara"

# Interactivity
library(plotly)
library(crosstalk)

# Parallel
library(purrr)
library(foreach)
library(doParallel)

# Statiscis Analysis
library(psych)
library(naniar)
library(lme4)
library(lmerTest)
library(coefplot)

library(rstan)
library(bayesplot)

library(broom)
library(broom.mixed)


# Environment ----
# rgb(73, 81, 112, maxColorValue = 255): "#495170"
col.1 <- "#495170"
col.2 <- "#f7be16"
col.3 <- "#fa8072"
scale.col.1 <- c(col.1, col.3)

options(dplyr.summarise.inform = TRUE)


# Load DataSets ----
