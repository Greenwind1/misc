# https://cran.r-project.org/web/packages/ale/vignettes/ale-x-datatypes.html

library(ale)
library(dplyr)

glimpse(var_cars)


cm <- mgcv::gam(mpg ~ cyl + disp + hp + drat + wt + s(qsec) +
                    vs + am + gear + carb + country,
                data = var_cars)

cars_ale <- ale(var_cars, cm)

names(cars_ale)

# plots
# ale has no trouble modelling any of the datatypes in our sample 
# (logical, factor, ordered, integer, or double). 
# It plots line charts for the numeric predictors and column charts for everything else.
class(cars_ale$plots$cyl)  # => ggplot class object
gridExtra::grid.arrange(grobs = cars_ale$plots, ncol = 2)


cars_ale_ixn <- ale_ixn(var_cars, cm)
cars_ale_ixn$plots %>% 
    purrr::walk(\(.x1) {
        # extract list of x1 ALE outputs
        gridExtra::grid.arrange(grobs = .x1, ncol = 2)  # plot all x1 plots
    })


# As explained in the vignette on modelling with small datasets, 
# a more appropriate modelling workflow would require bootstrapping the entire model, 
# not just the ALE data.

mb <- model_bootstrap(
    var_cars, 
    model_call_string = 'mgcv::gam(mpg ~ cyl + disp + hp + drat + wt + s(qsec) + 
    vs + am + gear + carb + country)',
    boot_it = 10,  # 100 by default but reduced here for a faster demonstration
    silent = TRUE  # progress bars disabled for the vignette
)

gridExtra::grid.arrange(grobs = mb$ale$plots, ncol = 2)

# With such a small dataset, the bootstrap confidence interval always overlap with 
# the middle band, indicating that this dataset cannot support any claims that 
# any of its variables has a meaningful effect on fuel efficiency (mpg). 
# Considering that the average bootstrapped ALE values suggest various intriguing 
# patterns, the problem is no doubt that the dataset is too small–if more data were 
# collected and analyzed, some of the patterns would probably be confirmed.
