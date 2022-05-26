# Packages ----
if (!require(workboots)) {
    install.packages("workboots")
    stopifnot(require(workboots))
}

library(tidymodels)

data("penguins")

penguins <- penguins %>% drop_na()

# split data into testing & training sets
set.seed(123)
penguins_split <- initial_split(penguins)
penguins_test <- testing(penguins_split)
penguins_train <- training(penguins_split)

# create a workflow
penguins_wf <- 
    workflow() %>%
    add_recipe(recipe(body_mass_g ~ ., data = penguins_train) %>% step_dummy(all_nominal())) %>%
    add_model(boost_tree("regression"))
