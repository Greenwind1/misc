library(tidymodels)

# Helper packages
library(readr)       # for importing data
library(vip)         # for variable importance plots

library(tidymodels)
library(readr)

cores <- parallel::detectCores() - 1

hotels <-
    read_csv("https://tidymodels.org/start/case-study/hotels.csv") %>%
    mutate(across(where(is.character), as.factor))

dim(hotels)


set.seed(2023)
splits <- initial_split(hotels, strata = children)

hotel_other <- training(splits)
hotel_test  <- testing(splits)

val_set <- validation_split(hotel_other,
                            strata = children,
                            prop = 0.80)

rf_mod <-
    rand_forest(mtry = tune(),
                min_n = tune(),
                trees = 1000) %>%
    set_engine("ranger", num.threads = cores) %>%
    set_mode("classification")

rf_recipe <-
    recipe(children ~ ., data = hotel_other) %>%
    step_date(arrival_date) %>%
    step_holiday(arrival_date) %>%
    step_rm(arrival_date)

rf_workflow <-
    workflow() %>%
    add_model(rf_mod) %>%
    add_recipe(rf_recipe)


rf_res <-
    rf_workflow %>%
    tune_grid(
        val_set,
        grid = 5,
        control = control_grid(save_pred = TRUE),
        metrics = metric_set(roc_auc)
    )
