# Packages ----
# Data manipulation
library(tidyverse)

# Visualisation
# library(ggplot2)
library(patchwork)
library(broman)  # plot_crayons()
library(rcartocolor)  # display_carto_all(); https://bit.ly/3Itq5kB
library(extrafont)  # "Candara"

# meta analysis
library(grid)
library(meta)
library(ggforestplot)


# Environment ----
source(file = "utility/environments.R")
source(file = "utility/helper_functions.R")


# DataSet ----
# Filter only associations to BMI for the first 30 biomarkers of the example
df <- ggforestplot::df_linear_associations %>% 
    filter(trait == "BMI",
           dplyr::row_number() <= 30)



# Viz Meta Analysis ----
# Draw a forestplot of cross-sectional, linear associations
p <- ggforestplot::forestplot(
    df = df,
    name = name,
    estimate = beta,
    se = se
)
class(p)
plot(p)
