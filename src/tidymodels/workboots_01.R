# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   workboots tutorial
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Guthub
# => https://github.com/markjrieke/workboots#usage

# Getting-Started-with-workboots
# => https://bit.ly/38Jnffn

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# 1. Packages ----
if (!require(workboots)) {
    install.packages("workboots")
    stopifnot(require(workboots))
}

if (!require(xgboost)) {
    install.packages("xgboost")
    stopifnot(require(xgboost))
}

library(tictoc)
library(tidyverse)
library(tidymodels)

library(extrafont)
# library(patchwork)


# 2. Environment ----
set.seed(2022)

col.tw <- "#dbd7d2"  # Timberwolf
col.os <- "#414a4c"  # Outer Space
col.rp <- "#7851a9"  # Royal Purple
col.mb <- "#1a4876"  # Midnight Blue
col.pb <- "#1ca9c9"  # Pacific Blue
col.cb <- "#b0b7c6"  # Cadet Blue
col.el <- "#ceff1d"  # Electric Lime
col.cg <- "#00cc99"  # Caribbean Green
col.mt <- "#ff8243"  # Mango Tango
col.rm <- "#e3256b"  # Razzmatazz
col.sl <- "#fc2847"  # Scarlet
col.wb.orange <- rgb(240, 136, 67, maxColorValue = 255)
col.wb.yellow <- rgb(255, 208, 109, maxColorValue = 255)
col.bg <- "#262A33"
col.fg <- "#393F4D"

font.base <- "Candara"
theme_set(theme_minimal(base_family = font.base))
options(dplyr.summarise.inform = TRUE)

icon.arr <- png::readPNG("fig/workboots_logo.png")
icon.grob <- grid::rasterGrob(icon.arr, 
                              width = unit(1.001 * 4, "cm"), 
                              height = unit(1.155 * 4, "cm"), 
                              interpolate = FALSE)


# 3. Dataset ----
data("penguins")
cat(dim(penguins), sep = "\n")
glimpse(penguins)
# View(penguins)


# 4. Preprocessing ----
penguins <- penguins %>% drop_na()
cat(dim(penguins), sep = "\n")  # 11 penguins dorpped...

penguins.split <- initial_split(penguins)
penguins.train <- training(penguins.split)
penguins.test <- testing(penguins.split)
cat(dim(penguins.train), dim(penguins.test), sep = "\n")


# 5. Tune hyper parameters ----
penguins.rec <- 
    recipe(body_mass_g ~ ., data = penguins.train) %>% 
    step_dummy(all_nominal())

penguins.bt <- boost_tree(
    mode = "regression",
    engine = "xgboost",
    mtry = tune(),
    trees = tune()
)

penguins.tune.wf <- workflow() %>% 
    add_recipe(penguins.rec) %>% 
    add_model(penguins.bt)

penguins.folds <- vfold_cv(data = penguins.train, v = 5, repeats = 1)

penguins.tune <-tune_grid(
    object = penguins.tune.wf, 
    resamples = penguins.folds, 
    grid = 10
)

penguins.tune %>% show_best("rmse", n = 10)


# 6. Finalize a workflow ----
penguins.wf <- penguins.tune.wf %>% 
    finalize_workflow(penguins.tune %>% select_best("rmse"))


# 7. Generate predictions from bootstrap models ----
tic()
penguins.pred.int <- penguins.wf %>% 
    predict_boots(
        n = 2000, 
        training_data = penguins.train, 
        new_data = penguins.test
    )
toc()


# 8. Summarise predictions with a 95% CI ----
penguins.summary <- penguins.pred.int %>% summarise_predictions()


# 9. Compare predictions with actual values ----
p1 <- penguins.summary %>% bind_cols(penguins.test) %>%
    ggplot(mapping = aes(
        x = body_mass_g, y = .pred, 
        ymin = .pred_lower, ymax = .pred_upper
    )) + 
    geom_abline(linetype = "dashed", color = col.tw) + 
    geom_point(color = col.wb.orange, size = 2, alpha = 0.75) + 
    geom_errorbar(color = col.wb.yellow, alpha = 0.25, width = 0.0125) + 
    labs(x = "Actual", y = "Predicted", 
         title = "Actual vs Predicted for penguins body mass (g)", 
         subtitle = "Using modeldata::penguins dataset") + 
    theme(
        title = element_text(color = col.wb.yellow, size = 20), 
        axis.text = element_text(color = col.wb.yellow, size = 15), 
        legend.position = c(0.85, 0.15), 
        legend.title = element_text(size = 10), 
        legend.text = element_text(color = col.tw), 
        plot.background = element_rect(fill = col.bg), 
        # plot includes panel
        panel.background = element_rect(fill = col.bg, color = col.bg), 
        panel.grid.major = element_line(color = col.os, size = 0.1), 
        panel.grid.minor = element_blank(), 
    ) + 
    lims(x = c(2500, 6100), y = c(2500, 6100)) + 
    coord_equal() + 
    annotation_custom(grob = icon.grob, 
                      xmin = 3000, xmax = 3000, 
                      ymin = 5500, ymax = 5500) + 
    annotate(geom = "text", 
             x = 5750, y = 2600, 
             label = "@Maxwell_110", size = 5, 
             family = font.base, color = col.wb.yellow, alpha = 0.5)

ggsave("fig/workboots_01-1.jpg", plot = p1, width = 9.5, height = 10)

# 10. Estimating VI
tic()
penguins.vi <- penguins.wf %>% 
    vi_boots(n = 1000, training_data = penguins.train)
toc()  # ~ 10(m)


# 11. Plot VI w/ CIs
p2 <- penguins.vi %>% summarise_importance() %>% 
    mutate(variable = forcats::fct_reorder(variable, .importance)) %>% 
    ggplot(aes(x = variable, y = .importance, 
               ymin = .importance_lower, ymax = .importance_upper)) + 
    geom_errorbar(color = col.wb.orange, width = 0.2) + 
    geom_point(color = col.wb.yellow, size = 3, alpha = 0.75) + 
    labs(x = NULL, y = NULL, title = "Variable Importance w/ CI") + 
    theme(
        title = element_text(color = col.wb.yellow, size = 20), 
        axis.text = element_text(color = col.wb.yellow, size = 15), 
        legend.position = c(0.85, 0.15), 
        legend.title = element_text(size = 10), 
        legend.text = element_text(color = col.tw), 
        plot.background = element_rect(fill = col.bg), 
        # plot includes panel
        panel.background = element_rect(fill = col.bg, color = col.bg), 
        panel.grid.major = element_line(color = col.os, size = 0.1), 
        panel.grid.minor = element_blank(), 
    ) + 
    coord_flip() + 
    annotate(
        geom = "text", x = 2.5, y = 0.5, 
        label = ">   workflow.object %>% vi_boots(n, training_data)", 
        size = 5, family = font.base, color = col.wb.yellow, alpha = 0.9
    )

ggsave("fig/workboots_01-2.jpg", plot = p2, width = 10, height = 7)
