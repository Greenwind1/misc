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
library(forestploter)


# Environment ----
source(file = "utility/environments.R")
source(file = "utility/helper_functions.R")


# DataSet ----
dt.all <- read.csv(system.file("extdata", "example_data.csv",
                               package = "forestploter"))
# Keep needed columns
dt <- dt.all[, 1:6]

# indent the subgroup if there is a number in the placebo column
dt$Subgroup <- ifelse(is.na(dt$Placebo),
                      dt$Subgroup,
                      paste0("   ", dt$Subgroup))

# NA to blank or NA will be transformed to carachter.
dt$Treatment <- ifelse(is.na(dt$Treatment), "", dt$Treatment)
dt$Placebo <- ifelse(is.na(dt$Placebo), "", dt$Placebo)
dt$se <- (log(dt$hi) - log(dt$est)) / 1.96

# Add blank column for the forest plot to display CI.
# Adjust the column width with space.
dt$` ` <- paste(rep(" ", 20), collapse = " ")

# Create confidence interval column to display
dt$`HR (95% CI)` <- ifelse(is.na(dt$se),
                           "",
                           sprintf("%.2f (%.2f to %.2f)",
                                   dt$est, dt$low, dt$hi))
# View(dt)


# Visualization ----
# Basic Forest Plot
p <- forest(
    dt[, c(1:3, 8:9)],
    est = dt$est,
    lower = dt$low,
    upper = dt$hi,
    sizes = dt$se,
    ci_column = 4,
    ref_line = 1,
    arrow_lab = c("Placebo Better", "Treatment Better"),
    xlim = c(0, 4),
    ticks_at = c(0.5, 1, 2, 3),
    footnote = "From vignettes"
)

# Print plot
plot(p)


# Change Theme ----
dt_tmp <- rbind(dt[-1, ], dt[1, ])  # reorder the first row from head to tail
dt_tmp[nrow(dt_tmp), 1] <- "Overall"
# View(dt_tmp)

tm <- forest_theme(base_size = 10, 
                   base_family = font.base, 
                   # Confidence interval point shape, line type/color/width
                   ci_pch = 16,
                   ci_col = col.plos.pink,
                   ci_lty = 1,
                   ci_lwd = 1.5,
                   ci_Theight = 0.4, # Set an T end at the end of CI 
                   # Legend
                   legend_name = "Group", 
                   legend_position = "right", 
                   legend_value = "", 
                   # X-axis
                   xaxis_lwd = 0.6, 
                   xaxis_cex = 1, 
                   # Reference line width/type/color
                   refline_lwd = 1,
                   refline_lty = "dashed",
                   refline_col = col.os,
                   # Vertical line width/type/color
                   vertline_lwd = 1,
                   vertline_lty = "dashed",
                   vertline_col = col.os,
                   # Change summary color for filling and borders
                   summary_fill = col.plos.pink,
                   summary_col = col.plos.pink,
                   # Footnote font size/face/color
                   footnote_cex = 1.0,
                   footnote_fontface = "bold",
                   footnote_col = col.c.b, 
                   title_fontfamily = font.base)


pt <- forest(dt_tmp[, c(1:3, 8:9)], 
             est = dt_tmp$est, 
             lower = dt_tmp$low,  
             upper = dt_tmp$hi, 
             sizes = dt_tmp$se, 
             is_summary = c(rep(FALSE, nrow(dt_tmp)-1), TRUE), 
             ci_column = 4, 
             ref_line = 1, 
             arrow_lab = c("Placebo Better", "Treatment Better"), 
             xlim = c(0, 4), 
             ticks_at = c(0.5, 1, 2, 3), 
             footnote = "@Maxwell_110", 
             theme = tm)

# Print plot
plot(pt)


# Editing Forest Plot ----
# Change text colors
# g <- edit_plot(
#     plot = pt, 
#     row = c(3, 15, 17, 20), 
#     col = NULL, 
#     part = c("body", "header"), 
#     which = c("text", "background"), 
#     gp = gpar(col = col.bmc.pink, fontface = "italic")
# )

# Bold grouping text
g <- edit_plot(pt,
               row = c(c(2, 5, 10, 13, 17, 20) - 1, 22),
               gp = gpar(fontface = "bold"))

# Edit background
g <- edit_plot(g, row = c(3, 15, 17, 20), 
               which = "background",
               gp = gpar(fill = col.sl, alpha = 0.25))

# Insert text at top
g <- insert_text(g,
                 text = "forestploter vignettes",
                 col = 2:3,
                 part = "header",
                 gp = gpar(fontface = "bold"))

# Add underline at the bottom of the header
g <- forestploter::add_border(g, part = "header")
g <- add_border(g, row = 21, part = "body", 
                   gp = gpar(lwd = 1, col = col.os))

plot(g)
ggsave(filename = "fig/forestploter-vignettes_01-1.jpg", 
       plot = plot(g), 
       dpi = 200, width = 4 * 1.75, height = 4 * 1.5)


# Multiple CI columns ----
dt.all$Subgroup <- ifelse(is.na(dt.all$Placebo),
                          dt.all$Subgroup,
                          paste0("   ", dt.all$Subgroup))

# NA to blank or NA will be transformed to carachter.
dt.all$`n` <- ifelse(is.na(dt.all$Treatment), "", dt.all$Treatment)
dt.all$`n ` <- ifelse(is.na(dt.all$Placebo), "", dt.all$Placebo)

# Add two blank column for CI
dt.all$`CVD outcome` <- paste(rep(" ", 20), collapse = " ")
dt.all$`COPD outcome` <- paste(rep(" ", 20), collapse = " ")

# Set-up theme
tm <- forest_theme(base_size = 10, 
                   base_family = font.base, 
                   # Confidence interval point shape, line type/color/width
                   ci_pch = c(15, 18), 
                   ci_col = c(col.plos.pink, col.plos.yellow),
                   ci_lty = 1,
                   ci_lwd = 1.5,
                   ci_Theight = 0.4, # Set an T end at the end of CI 
                   # Legend
                   legend_name = "Group", 
                   legend_position = "right", 
                   legend_value = c("Trt 1", "Trt 2"), 
                   # X-axis
                   xaxis_lwd = 0.6, 
                   xaxis_cex = 1, 
                   # Reference line width/type/color
                   refline_lwd = 1,
                   refline_lty = "dashed",
                   refline_col = col.os,
                   # Vertical line width/type/color
                   vertline_lwd = 1,
                   vertline_lty = "dashed",
                   vertline_col = col.os,
                   # Change summary color for filling and borders
                   summary_fill = col.plos.pink,
                   summary_col = col.plos.pink,
                   # Footnote font size/face/color
                   footnote_cex = 1.0,
                   footnote_fontface = "bold",
                   footnote_col = col.c.b)

p <- forest(dt.all[,c(1, 19, 21, 20, 22)],
            est = list(dt.all$est_gp1,
                       dt.all$est_gp2,
                       dt.all$est_gp3,
                       dt.all$est_gp4),
            lower = list(dt.all$low_gp1,
                         dt.all$low_gp2,
                         dt.all$low_gp3,
                         dt.all$low_gp4), 
            upper = list(dt.all$hi_gp1,
                         dt.all$hi_gp2,
                         dt.all$hi_gp3,
                         dt.all$hi_gp4),
            ci_column = c(3, 5),
            ref_line = 1,
            vert_line = c(0.5, 2),
            nudge_y = 0.2,
            theme = tm)

plot(p)
ggsave(filename = "fig/forestploter-vignettes_01-2.jpg", 
       plot = plot(p), 
       dpi = 200, width = 4 * 1.75, height = 4 * 1.5)
