# =============================================================================
#  ALD Simulation 06 - APCtools Exploratory Data Analysis
#
#  This script performs APC-structured EDA on the simulated dataset generated
#  by ald_sim_06-generate.R, using the APCtools package (Bauer et al. 2022).
#
#  Two layers of analysis are provided:
#    (A) Descriptive (no model): plot_density, plot_variable,
#        plot_densityMatrix, plot_APCheatmap, plot_APChexamap
#    (B) Model-based: GAM with te(age, period) as "viz models" (no jump
#        dummies), used purely for visualization.
#        Note: inference models with jump dummies live in the fitting script.
#
#  Data structure (from generate script):
#    person_id  : individual ID
#    birth_year : cohort identifier (1940, 1945, 1950, 1955, 1960)
#    obs_year   : calendar year (2010-2019); renamed to "period" for APCtools
#    age        : obs_year - birth_year (50-79)
#    visited    : binary, Hurdle Part 1
#    medical_cost: 0 when visited==0; Gamma when visited==1, Hurdle Part 2
#    post_20XX  : cumulative step dummy for each fee revision year
#
#  Reference:
#    Bauer A, Weigert M, Jalal H. 2022. APCtools: Descriptive and
#    Model-based Age-Period-Cohort Analysis. arXiv:2207.03901.
# =============================================================================

library(APCtools)
library(mgcv)
library(tidyverse)
library(ggpubr)
library(patchwork)
library(RColorBrewer)

set.seed(2026)

# ---- Global settings ----
NAME          <- "ald_simulation_06"
font_base     <- "Times New Roman"
OUTPUT_DIR    <- "fig"

dir.create(OUTPUT_DIR, showWarnings = FALSE)

theme_set(
  theme_minimal(base_family = font_base, base_size = 13) +
    theme(
      legend.position  = "bottom",
      strip.background = element_rect(fill = "gray95"),
      panel.grid.minor = element_blank()
    )
)


# 1. Study design parameters ----
# These must match ald_sim_06-generate.R exactly.
COHORT_BIRTH_YEARS   <- c(1940L, 1945L, 1950L, 1955L, 1960L)
REVISION_YEARS       <- c(2012L, 2013L, 2014L, 2015L, 2016L, 2017L, 2018L)
OBS_YEARS            <- 2010:2019

# Age and period groups for plot_densityMatrix
# Age spans 50-79 (5-year bins); period spans 2010-2019 (3 coarse groups)
age_groups <- list(
  c(50, 54), c(55, 59), c(60, 64),
  c(65, 69), c(70, 74), c(75, 79)
)
period_groups <- list(c(2010, 2012), c(2013, 2015), c(2016, 2019))

# Cohort diagonal highlights for plot_densityMatrix
# Each ALD cohort spans exactly one birth year; use a narrow ±1 yr window.
cohort_diags <- list(
  "Born 1940" = c(1939, 1941),
  "Born 1945" = c(1944, 1946),
  "Born 1950" = c(1949, 1951),
  "Born 1955" = c(1954, 1956),
  "Born 1960" = c(1959, 1961)
)

# Data frame for geom_vline overlays (revision years)
REVISION_DF <- data.frame(period = REVISION_YEARS)


# 2. Load and prepare data ----

dat <- read_csv(
  paste0("input/", NAME, "-data.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    # APCtools requires a column named "period" for the calendar year axis.
    # cohort is derived internally as period - age = birth_year.
    period     = as.integer(obs_year),
    birth_year = as.integer(birth_year),
    age        = as.integer(age),
    visited    = as.integer(visited)
  )

# Subset for Part 2 (conditional expenditure; only rows with visits)
dat_visited <- dat %>% filter(visited == 1L)

cat(sprintf(
  "Rows (all): %d | Visit rate: %.1f%% | Part-2 rows: %d\n",
  nrow(dat),
  100 * mean(dat$visited),
  nrow(dat_visited)
))


# 3. Descriptive EDA — Marginal distributions ----
# plot_density() visualizes the marginal distribution of a single variable.
# Metric variables: density plot (default) or boxplot.
# Categorical/factor variables: bar chart.

## 3-1. Medical cost — all observations (includes structural zeros) ----
p_dens_all <- plot_density(
  dat   = dat,
  y_var = "medical_cost"
) + labs(title = "Medical cost: all observations")

## 3-2. Medical cost — visitors only (positive costs, log x-axis) ----
p_dens_pos <- plot_density(
  dat   = dat_visited,
  y_var = "medical_cost"
) +
  scale_x_log10(labels = scales::comma) +
  labs(title = "Medical cost: visitors only (log x-axis)")

## 3-3. Visit rate (binary) ----
# Recode visited as factor so APCtools renders it as a bar chart.
dat_fct <- dat %>%
  mutate(visited_fct = factor(visited,
                              levels = c(0, 1),
                              labels = c("Not visited", "Visited")))

p_dens_visit <- plot_density(
  dat   = dat_fct,
  y_var = "visited_fct"
) + labs(title = "Visit status (marginal proportion)")


p_marginal <- p_dens_all / p_dens_pos / p_dens_visit

ggsave(
  filename = file.path(OUTPUT_DIR, paste0(NAME, "-eda_APCtools_01_marginal.jpg")),
  plot     = p_marginal,
  width    = 10, height = 15, dpi = 300
)


# 4. Descriptive EDA — 1D: variable vs each APC dimension ----
# plot_variable() summarizes a response variable (mean or distribution)
# along one APC dimension (age, period, or cohort).

## 4-1. Medical cost (visitors) by age / period / cohort ----
p_v_cost_age <- plot_variable(
  dat           = dat_visited,
  y_var         = "medical_cost",
  apc_dimension = "age",
  plot_type     = "line"
) + labs(title = "Conditional cost vs Age")

p_v_cost_period <- plot_variable(
  dat           = dat_visited,
  y_var         = "medical_cost",
  apc_dimension = "period",
  plot_type     = "line"
) +
  geom_vline(
    xintercept = REVISION_YEARS,
    linetype   = "dashed",
    color      = "firebrick",
    alpha      = 0.7
  ) +
  scale_x_continuous(breaks = OBS_YEARS) +
  labs(
    title    = "Conditional cost vs Period"
    # subtitle = "Red broken lines: simulated period shock years (COVID-like)"
  )

p_v_cost_cohort <- plot_variable(
  dat           = dat_visited,
  y_var         = "medical_cost",
  apc_dimension = "cohort",
  plot_type     = "line"
) + labs(title = "Conditional cost vs Cohort (birth year)")

## 4-2. Visit rate by age / period / cohort ----
p_v_visit_age <- plot_variable(
  dat           = dat,
  y_var         = "visited",
  apc_dimension = "age",
  plot_type     = "line"
) + labs(title = "Visit rate vs Age")

p_v_visit_period <- plot_variable(
  dat           = dat,
  y_var         = "visited",
  apc_dimension = "period",
  plot_type     = "line"
) +
  geom_vline(
    xintercept = REVISION_YEARS,
    linetype   = "dashed",
    color      = "firebrick",
    alpha      = 0.7
  ) +
  scale_x_continuous(breaks = OBS_YEARS) + 
  labs(
    title    = "Visit rate vs Period",
    # subtitle = "Red broken lines: fee revision years"
  )

p_v_visit_cohort <- plot_variable(
  dat           = dat,
  y_var         = "visited",
  apc_dimension = "cohort",
  plot_type     = "line"
) + labs(title = "Visit rate vs Cohort (birth year)")

# p_1d_cost  <- ggarrange(p_v_cost_age,  p_v_cost_period,  p_v_cost_cohort,  nrow = 1)
p_1d_cost  <- p_v_cost_age + p_v_cost_period + p_v_cost_cohort
# p_1d_visit <- ggarrange(p_v_visit_age, p_v_visit_period, p_v_visit_cohort, nrow = 1)
p_1d_visit <- p_v_visit_age + p_v_visit_period + p_v_visit_cohort

ggsave(
  filename = file.path(OUTPUT_DIR, paste0(NAME, "-eda_APCtools_02_1d_cost.jpg")),
  plot     = p_1d_cost,
  width    = 15, height = 5, dpi = 150
)
ggsave(
  filename = file.path(OUTPUT_DIR, paste0(NAME, "-eda_APCtools_03_1d_visitrate.jpg")),
  plot     = p_1d_visit,
  width    = 15, height = 5, dpi = 150
)
# cat("Saved: eda_02_1d_cost.jpg, eda_03_1d_visitrate.jpg\n")


# 5. Descriptive EDA — Density matrix ----
# plot_densityMatrix() places age on x-axis, period on y-axis,
# and represents cohort on the diagonals.
# Each cell shows the conditional distribution of y_var in that (age, period) cell.

# NOTE: All intervals in age_groups and period_groups must have the same width.
# plot_densityMatrix() enforces this internally and throws an error otherwise.
# With a 10-year observation window (2010-2019), two 5-year period bins are
# the only option that matches the 5-year width used in age_groups.

## 5-1. Medical cost (visitors only) ----
# p_dm_cost <- plot_densityMatrix(
#   dat                 = dat_visited,
#   y_var               = "medical_cost",
#   age_groups          = age_groups,
#   period_groups       = period_groups,
#   log_scale           = TRUE,
#   highlight_diagonals = cohort_diags
# )

# ggsave(
#   filename = file.path(OUTPUT_DIR, paste0(NAME, "-eda_APCtools_04_density_matrix_cost.jpg")),
#   plot     = p_dm_cost,
#   width    = 12, height = 10, dpi = 150
# )

## 5-2. Medical cost — categorical color scale ----
# Visualize expenditure as distance-category bands for cross-APC comparison.
# cost_breaks <- c(0, 5000, 20000, 50000, 100000, Inf)
# cost_labels <- c(
#   "< 5k", "5k-20k", "20k-50k", "50k-100k", "> 100k"
# )

# p_dm_cost_cat <- plot_densityMatrix(
#   dat                 = dat_visited,
#   y_var               = "medical_cost",
#   age_groups          = age_groups,
#   period_groups       = period_groups,
#   log_scale           = TRUE,
#   y_var_cat_breaks    = cost_breaks,
#   y_var_cat_labels    = cost_labels,
#   legend_title        = "Cost band (JPY)",
#   highlight_diagonals = cohort_diags
# )

# ggsave(
#   filename = file.path(OUTPUT_DIR, 
#     paste0(NAME, "-eda_APCtools_05_density_matrix_cost_cat.jpg")),
#   plot     = p_dm_cost_cat,
#   width    = 12, height = 10, dpi = 150
# )
# cat("Saved: eda_04_density_matrix_cost.jpg, eda_05_density_matrix_cost_cat.jpg\n")


# 6. Descriptive EDA — Heatmaps (no model) ----
# plot_APCheatmap() with bin_heatmap = FALSE shows the raw observed mean of
# y_var at each (age, period) cell; cohort diagonals are drawn with markLines_list.

## 6-1. Conditional medical cost (log scale) ----
p_heat_cost_raw <- plot_APCheatmap(
  dat            = dat_visited,
  y_var          = "medical_cost",
  y_var_logScale = TRUE,
  bin_heatmap    = FALSE,
  markLines_list = list(cohort = COHORT_BIRTH_YEARS)
) + labs(
  title    = "Descriptive heatmap: conditional medical cost (log scale)",
  # subtitle = "Diagonal lines = ALD cohort birth years; overlap bands visible"
)

## 6-2. Same but binned into 5-year blocks ----
p_heat_cost_bin <- plot_APCheatmap(
  dat            = dat_visited,
  y_var          = "medical_cost",
  y_var_logScale = TRUE,
  bin_heatmap    = TRUE,
  markLines_list = list(cohort = COHORT_BIRTH_YEARS)
) + labs(
  title    = "Descriptive heatmap (5-yr bins): conditional medical cost"
)

## 6-3. Visit rate ----
p_heat_visit_raw <- plot_APCheatmap(
  dat            = dat,
  y_var          = "visited",
  bin_heatmap    = FALSE,
  markLines_list = list(cohort = COHORT_BIRTH_YEARS)
) + labs(
  title    = "Descriptive heatmap: visit rate",
  # subtitle = "Diagonal lines = ALD cohort birth years"
)

p_heat_desc <- p_heat_cost_raw / p_heat_visit_raw
ggsave(
  filename = file.path(OUTPUT_DIR, paste0(NAME, "-eda_06_heatmap_desc.jpg")),
  plot     = p_heat_desc,
  width    = 7, height = 12, dpi = 150
)
ggsave(
  filename = file.path(OUTPUT_DIR, paste0(NAME, "-eda_06b_heatmap_desc_binned.jpg")),
  plot     = p_heat_cost_bin,
  width    = 7, height = 6, dpi = 150
)
# cat("Saved: eda_06_heatmap_desc.jpg, eda_06b_heatmap_desc_binned.jpg\n")


# 7. Descriptive EDA — Hexamaps (no model) ----
# plot_APChexamap() uses rotated hexagonal axes so that all three APC
# dimensions receive equal visual weight.
# Note: uses base R graphics (not ggplot2); save with png() + dev.off().

png(
  filename = file.path(OUTPUT_DIR, paste0(NAME, "-eda_07_hexamap_cost.jpg")),
  width = 800, height = 700, res = 120
)
plot_APChexamap(
  dat            = dat_visited,
  y_var          = "medical_cost",
  y_var_logScale = TRUE
)
dev.off()

png(
  filename = file.path(OUTPUT_DIR, paste0(NAME, "-eda_07b_hexamap_visit.jpg")),
  width = 800, height = 700, res = 120
)
plot_APChexamap(
  dat   = dat,
  y_var = "visited"
)
dev.off()

cat("Saved: eda_07_hexamap_cost.jpg, eda_07b_hexamap_visit.jpg\n")


# 8. Model-based EDA — Fit "viz" GAM models ----
# Viz models use te(age, period) only — no jump dummies.
# Purpose: clean smooth surface for APCtools visualization functions.
# These are NOT inference models; jump dummies are added only in the
# fitting/inference script.
#
# APCtools automatically exponentiates predictions when the link is
# log (Gamma) or logit (binomial):
#   m1_viz -> odds scale (P(visit) / P(no visit))
#   m2_viz -> cost scale (JPY)

K_AGE    <- 8   # basis dimension for age
K_PERIOD <- 5   # basis dimension for period (only 10 distinct values)

## 8-1. Part 1 (viz): visit probability ----
m1_viz <- gam(
  visited ~ te(age, period, bs = "cr", k = c(K_AGE, K_PERIOD)),
  data   = dat,
  family = binomial(link = "logit"),
  method = "REML"
)
cat("\n=== Part 1 viz model ===\n"); print(summary(m1_viz))

## 8-2. Part 2 (viz): conditional medical cost ----
m2_viz <- gam(
  medical_cost ~ te(age, period, bs = "cr", k = c(K_AGE, K_PERIOD)),
  data   = dat_visited,
  family = Gamma(link = "log"),
  method = "REML"
)
cat("\n=== Part 2 viz model ===\n"); print(summary(m2_viz))

saveRDS(m1_viz, file = file.path(OUTPUT_DIR, paste0(NAME, "-eda_m1_viz.rda")))
saveRDS(m2_viz, file = file.path(OUTPUT_DIR, paste0(NAME, "-eda_m2_viz.rda")))
cat("Saved: model RDS files\n")


# 9. Model-based EDA — Heatmaps ----
# plot_APCheatmap(model = ...) shows the fitted smooth te() surface.
# The diagonal ALD cohort lines make the overlap structure legible.

## 9-1. Part 1 heatmap ----
p_heat_m1 <- plot_APCheatmap(
  dat            = dat,
  model          = m1_viz,
  bin_heatmap    = FALSE,
  plot_CI        = FALSE,
  markLines_list = list(cohort = COHORT_BIRTH_YEARS)
) + labs(
  title    = "Model heatmap: Part 1 — visit probability (odds scale)",
  subtitle = "te(age, period) | Diagonal lines: ALD cohort birth years"
)

## 9-2. Part 2 heatmap ----
p_heat_m2 <- plot_APCheatmap(
  dat            = dat_visited,
  model          = m2_viz,
  bin_heatmap    = FALSE,
  plot_CI        = FALSE,
  markLines_list = list(cohort = COHORT_BIRTH_YEARS)
) + labs(
  title    = "Model heatmap: Part 2 — conditional cost (JPY)",
  subtitle = "te(age, period) | Diagonal lines: ALD cohort birth years"
)

p_heat_model <- ggarrange(p_heat_m1, p_heat_m2, nrow = 1)
ggsave(
  filename = file.path(OUTPUT_DIR, paste0(NAME, "-eda_08_heatmap_model.jpg")),
  plot     = p_heat_model,
  width    = 14, height = 6, dpi = 150
)
cat("Saved: eda_08_heatmap_model.jpg\n")


# 10. Model-based EDA — Hexamaps ----
# Model-based hexamaps visualize the smooth fitted surface with equal
# visual weight for all three APC dimensions.

png(
  filename = file.path(OUTPUT_DIR, paste0(NAME, "-eda_09_hexamap_m1.jpg")),
  width = 800, height = 700, res = 120
)
plot_APChexamap(dat = dat, model = m1_viz)
dev.off()

png(
  filename = file.path(OUTPUT_DIR, paste0(NAME, "-eda_09b_hexamap_m2.jpg")),
  width = 800, height = 700, res = 120
)
plot_APChexamap(dat = dat_visited, model = m2_viz)
dev.off()

cat("Saved: eda_09_hexamap_m1.jpg, eda_09b_hexamap_m2.jpg\n")


# 11. Model-based EDA — Marginal APC effects ----
# Marginal effects are computed by averaging the te() surface along one axis.
# The period marginal absorbs only the smooth trend; discrete fee revision
# jumps are NOT captured here (they live in the inference model's eta_i terms).
#
# vlines_vec: vertical reference lines (revision years / birth years)

## 11-1. Part 1 marginal effects ----
p_marg_m1_age <- plot_marginalAPCeffects(
  model    = m1_viz,
  dat      = dat,
  variable = "age",
  plot_CI  = TRUE
) + labs(title = "Part 1: marginal age effect")

p_marg_m1_period <- plot_marginalAPCeffects(
  model      = m1_viz,
  dat        = dat,
  variable   = "period",
  plot_CI    = TRUE,
  vlines_vec = REVISION_YEARS
) + labs(
  title    = "Part 1: marginal period effect",
  subtitle = "Dotted lines: fee revision years | smooth-only (no jump dummies)"
)

p_marg_m1_cohort <- plot_marginalAPCeffects(
  model      = m1_viz,
  dat        = dat,
  variable   = "cohort",
  plot_CI    = TRUE,
  vlines_vec = COHORT_BIRTH_YEARS
) + labs(
  title    = "Part 1: marginal cohort effect",
  subtitle = "Dotted lines: ALD cohort birth years"
)

## 11-2. Part 2 marginal effects ----
p_marg_m2_age <- plot_marginalAPCeffects(
  model    = m2_viz,
  dat      = dat_visited,
  variable = "age",
  plot_CI  = TRUE
) + labs(title = "Part 2: marginal age effect")

p_marg_m2_period <- plot_marginalAPCeffects(
  model      = m2_viz,
  dat        = dat_visited,
  variable   = "period",
  plot_CI    = TRUE,
  vlines_vec = REVISION_YEARS
) + labs(
  title    = "Part 2: marginal period effect",
  subtitle = "Dotted lines: fee revision years"
)

p_marg_m2_cohort <- plot_marginalAPCeffects(
  model      = m2_viz,
  dat        = dat_visited,
  variable   = "cohort",
  plot_CI    = TRUE,
  vlines_vec = COHORT_BIRTH_YEARS
) + labs(
  title    = "Part 2: marginal cohort effect",
  subtitle = "Dotted lines: ALD cohort birth years"
)

p_marg_m1 <- ggarrange(p_marg_m1_age, p_marg_m1_period, p_marg_m1_cohort, nrow = 1)
p_marg_m2 <- ggarrange(p_marg_m2_age, p_marg_m2_period, p_marg_m2_cohort, nrow = 1)

ggsave(
  filename = file.path(OUTPUT_DIR, paste0(NAME, "-eda_10_marginal_p1.jpg")),
  plot     = p_marg_m1,
  width    = 15, height = 5, dpi = 150
)
ggsave(
  filename = file.path(OUTPUT_DIR, paste0(NAME, "-eda_11_marginal_p2.jpg")),
  plot     = p_marg_m2,
  width    = 15, height = 5, dpi = 150
)
cat("Saved: eda_10_marginal_p1.jpg, eda_11_marginal_p2.jpg\n")


# 12. Model-based EDA — Joint marginal effects ----
# plot_jointMarginalAPCeffects() overlays marginal effects from multiple
# models in a single panel — useful to compare Part 1 vs Part 2 structure.
#
# Caveat: Part 1 (binomial) and Part 2 (Gamma) produce effects on
# incompatible scales, so each model gets its own y-axis / facet.
# The comparison is qualitative (shape of curve), not quantitative.
#
# For the joint plot, dat must cover the union of both models' observation
# space. Since both share the same (age, period) grid, dat is sufficient.

model_list_joint <- list(
  "Part 1 (visit probability)" = m1_viz,
  "Part 2 (conditional cost)"  = m2_viz
)

p_joint_age <- plot_jointMarginalAPCeffects(
  model_list = model_list_joint,
  dat        = dat,
  variable   = "age"
) + labs(
  title    = "Joint marginal age effect: Part 1 vs Part 2",
  subtitle = "Scales differ; shapes are qualitatively comparable"
)

p_joint_period <- plot_jointMarginalAPCeffects(
  model_list  = model_list_joint,
  dat         = dat,
  variable    = "period",
  vlines_list = list(period = REVISION_YEARS)
) + labs(
  title    = "Joint marginal period effect: Part 1 vs Part 2",
  subtitle = "Dotted lines: fee revision years"
)

p_joint_cohort <- plot_jointMarginalAPCeffects(
  model_list  = model_list_joint,
  dat         = dat,
  variable    = "cohort",
  vlines_list = list(cohort = COHORT_BIRTH_YEARS)
) + labs(
  title    = "Joint marginal cohort effect: Part 1 vs Part 2",
  subtitle = "Dotted lines: ALD cohort birth years"
)

p_joint <- ggarrange(p_joint_age, p_joint_period, p_joint_cohort, nrow = 1)
ggsave(
  filename = file.path(OUTPUT_DIR, paste0(NAME, "-eda_12_joint_marginal.jpg")),
  plot     = p_joint,
  width    = 15, height = 5, dpi = 150
)
cat("Saved: eda_12_joint_marginal.jpg\n")


# 13. Model-based EDA — Partial APC plots ----
# plot_partialAPCeffects() visualizes the estimated effect for one APC
# dimension, conditioned on the remaining two.
# The bold blue line is the mean marginal effect; grayscale lines show
# individual partial effects for each age/period/cohort stratum.

## 13-1. Part 1 ----
p_partial_m1_age    <- plot_partialAPCeffects(m1_viz, dat, variable = "age") +
  labs(title = "Part 1: partial age effects")
p_partial_m1_period <- plot_partialAPCeffects(m1_viz, dat, variable = "period") +
  labs(title = "Part 1: partial period effects")
p_partial_m1_cohort <- plot_partialAPCeffects(m1_viz, dat, variable = "cohort") +
  labs(title = "Part 1: partial cohort effects")

## 13-2. Part 2 ----
p_partial_m2_age    <- plot_partialAPCeffects(m2_viz, dat_visited, variable = "age") +
  labs(title = "Part 2: partial age effects")
p_partial_m2_period <- plot_partialAPCeffects(m2_viz, dat_visited, variable = "period") +
  labs(title = "Part 2: partial period effects")
p_partial_m2_cohort <- plot_partialAPCeffects(m2_viz, dat_visited, variable = "cohort") +
  labs(title = "Part 2: partial cohort effects")

for (nm in c("age", "period", "cohort")) {
  idx_p1 <- match(nm, c("age", "period", "cohort"))
  p1 <- list(p_partial_m1_age, p_partial_m1_period, p_partial_m1_cohort)[[idx_p1]]
  p2 <- list(p_partial_m2_age, p_partial_m2_period, p_partial_m2_cohort)[[idx_p1]]
  fn <- file.path(OUTPUT_DIR,
                  sprintf("%s-eda_13_%s_partial.jpg", NAME, nm))
  ggsave(fn, ggarrange(p1, p2, nrow = 1), width = 12, height = 5, dpi = 150)
  cat(sprintf("Saved: eda_13_%s_partial.jpg\n", nm))
}


# 14. Model-based EDA — APC summary table ----
# create_APCsummary() reports the min/max values and overall magnitude of
# each marginal APC effect, giving a compact numerical overview.

apc_summary_m1 <- create_APCsummary(
  model_list = list("Part 1 (visit probability)" = m1_viz),
  dat        = dat
)
apc_summary_m2 <- create_APCsummary(
  model_list = list("Part 2 (conditional cost)" = m2_viz),
  dat        = dat_visited
)

cat("\n=== APC effect summary: Part 1 ===\n");  print(apc_summary_m1)
cat("\n=== APC effect summary: Part 2 ===\n");  print(apc_summary_m2)


# 15. Model-based EDA — Model summary tables ----
# create_modelSummary() produces publication-ready tables:
#   [[1]]: linear (parametric) coefficients
#   [[2]]: smooth (nonparametric) term estimates (edf, p-value)

summary_m1 <- create_modelSummary(list("Part 1" = m1_viz))
summary_m2 <- create_modelSummary(list("Part 2" = m2_viz))

cat("\n=== Model summary — Part 1 (smooth terms) ===\n"); print(summary_m1[[2]])
cat("\n=== Model summary — Part 2 (smooth terms) ===\n"); print(summary_m2[[2]])


cat("\n=== EDA complete. All outputs written to", OUTPUT_DIR, "===\n")
