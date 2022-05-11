# Packages ----
# Multilevel and ML
library(tidymodels)
library(multilevelmod)

# Visualisation
library(ggplot2)
library(ggalt)  # geom_encircle
library(patchwork)
library(broman)  # plot_crayons()
library(rcartocolor)  # display_carto_all(); https://bit.ly/3Itq5kB
library(extrafont)  # "Candara"

# Environment ----
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

col.parsnip.g <- "#006838"
col.parsnip.b <- "#BF9F60"
col.parsnip.y <- "#F9F5BB"

font.base <- "Candara"
# font.base <- "Times New Roman"
theme_set(theme_minimal(base_family = font.base))
options(dplyr.summarise.inform = TRUE)

icon.img <- png::readPNG("fig/parsnip_logo.png")
icon.grob <- grid::rasterGrob(icon.img, 
                              width = unit(2.205 * 2.5, "cm"), 
                              height = unit(2.556 * 2.5, "cm"), 
                              interpolate = FALSE)

tidymodels::tidymodels_prefer()

# Dataset ----
data(sleepstudy, package = "lme4")
glimpse(sleepstudy)

sleepstudy %>% 
    ggplot(aes(x = Days, y = Reaction)) + 
    geom_point(color = col.parsnip.g, alpha = 0.5) + 
    geom_line(color = col.parsnip.b) + 
    facet_wrap(facets = . ~ Subject)

lme_spec <- linear_reg() %>% 
    set_engine("lme", random = ~ 1 | Subject)

lme_fit <- lme_spec %>% 
    fit(Reaction ~ Days, data = sleepstudy)

lme_fit

new_subject <- tibble(
    Days = 0:9, 
    Subject = "one"
)

predict(lme_fit, new_data = new_subject) %>% 
    bind_cols(new_subject) %>% 
    ggplot() + 
    geom_point(aes(Days, .pred), color = col.parsnip.g, alpha = 0.5) + 
    geom_line(aes(Days, .pred), color = col.parsnip.b) + 
    annotation_custom(grob = icon.grob, 
                      xmin = 1.5, xmax = 1.5, 
                      ymin = 325, ymax = 325)
ggsave("fig/multilevelmod_get-started_01.jpg", dpi = 150)
