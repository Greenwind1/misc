# =============================================================================
#  ALD Simulation: Method C — APCtools (Clements et al. 2005 / Bauer et al. 2022)
#
#  Core model:  g(mu_i) = beta0 + f_ap(age_i, period_i) + eta_i
#    f_ap : tensor product smooth te(age, period, bs="cr")  [Clements et al. 2005]
#    eta_i: optional linear predictor (revision jump dummies)
#    cohort = period - age = obs_year - age = birth_year  (diagonal of surface)
#
#  Key difference from Methods A & B:
#    A, B  : s(age) + s(birth_year) — separate marginals, period represented
#             as jump dummies (A) or nonlinear orthogonal basis (B, Carstensen)
#    C     : te(age, period)        — joint 2-D surface, cohort on the diagonal
#             No identification constraint imposed (cf. Carstensen drift=0).
#             The ALD overlap structure implicitly aids identification.
#
#  Dual-model strategy:
#    m*_c_viz  : te(age, period) only   — for APCtools native plots
#    m1_c_infer: te(age, period) + jump dummies — for Part 1 inference
#    m2_c_infer: te(age, period)        — for Part 2 inference
#    (plot_APCheatmap builds its own prediction grid, so extra covariates in
#     the model object cause errors; m*_c_viz avoids this.)
#
#  References:
#    Bauer, A., Weigert, M., & Jalal, H. (2022). APCtools: Descriptive and
#      Model-based Age-Period-Cohort Analysis. JOSS, 7(73), 4056.
#      doi:10.21105/joss.04056
#    Clements, M. S., Armstrong, B. K., & Moolgavkar, S. H. (2005).
#      Lung cancer rate predictions using generalized additive models.
#      Biostatistics, 6(4), 576-589. doi:10.1093/biostatistics/kxi028
#    Weigert, M. et al. (2021). Semiparametric APC analysis of destination
#      choice patterns. Tourism Economics.
#      doi:10.1177/1354816620987198
#
#  Change logs:
#  06: COVID-19 shock on visit frequency only; more realistic cohort effect.
# =============================================================================

library(mgcv)
library(APCtools)  # install.packages("APCtools")
library(tidyverse)
library(patchwork)
library(extrafont)  # fonttable(); "Times New Roman"
source("utility/environments.R")
set.seed(2026)

NAME <- "ald_simulation_06"


# 1. Load data ----
dat <- read_csv(paste0("input/", NAME, "-data.csv"))

# APCtools requires a column named "period".
# cohort = period - age = obs_year - age = birth_year  (verified below).
dat <- dat %>% mutate(period = obs_year)
stopifnot(all(dat$period - dat$age == dat$birth_year))
cat("Cohort identity check (period - age == birth_year): OK\n\n")


# 2. Study design parameters & true DGP functions ----
REVISION_YEARS <- c(2012, 2013, 2014, 2015, 2016, 2017, 2018)
COHORT_BIRTH_YEARS <- c(1940, 1945, 1950, 1955, 1960)
jump_vars <- paste0("post_", REVISION_YEARS)
jump_formula_str <- paste(jump_vars, collapse = " + ")

true_age_logit <- function(age) {
  -2.5 + 0.04 * pmax(age - 50, 0) + 0.06 * pmax(age - 70, 0)
}

true_age_log_amount <- function(age) {
  8.5 + 0.025 * pmax(age - 50, 0) + 0.05 * pmax(age - 70, 0)
}

true_cohort_effect <- function(birth_year) {
  x <- birth_year - 1940
  -0.075 * (x / 10)^2 + 0.2
}

true_theta <- c(
  `2012` = 0,   `2013` = 0,   `2014` = 0,
  `2015` = -0.4, `2016` = +0.2, `2017` = +0.1, `2018` = +0.1
)


# 3. Fitting ----
# Tensor product knot sizes:
#   age    : k = 10  (age range 50-79, 30 unique values)
#   period : k = 5   (period range 2010-2019, 10 unique values)
K_AGE    <- 10
K_PERIOD <- 5

dat_visited <- dat %>% filter(visited == 1)


## 3-1. Visualization models (te only; needed for APCtools native plots) ----

# Part 1 viz: pure te — captures smooth APC structure; jumps not modeled here
formula_c1_viz <- visited ~ te(age, period, bs = "cr", k = c(K_AGE, K_PERIOD))

cat("=== Method C | Part 1 (viz) formula ===\n"); print(formula_c1_viz)
m1_c_viz <- gam(formula_c1_viz, data = dat,
                family = binomial(link = "logit"),
                method = "REML")
cat(sprintf("Method C | Part 1 viz AIC: %.1f\n\n", AIC(m1_c_viz)))

# Part 2 viz: pure te (same as inference model for Part 2)
formula_c2 <- medical_cost ~ te(age, period, bs = "cr", k = c(K_AGE, K_PERIOD))

cat("=== Method C | Part 2 formula ===\n"); print(formula_c2)
m2_c <- gam(formula_c2, data = dat_visited,
            family = Gamma(link = "log"),
            method = "REML")
cat(sprintf("Method C | Part 2 AIC: %.1f\n\n", AIC(m2_c)))


## 3-2. Inference model for Part 1 (te + revision jump dummies as eta_i) ----
# The te(age, period) captures the smooth APC structure.
# Jump dummies (post_r = I(period >= r)) are eta_i in the APCtools framework,
# absorbing discrete discontinuities that the smooth surface cannot represent.
# Per DGP: revisions affect visit probability (Part 1), not expenditure (Part 2).

formula_c1_infer <- as.formula(paste0(
  "visited ~ te(age, period, bs = 'cr', k = c(", K_AGE, ", ", K_PERIOD, ")) + ",
  jump_formula_str
))

cat("=== Method C | Part 1 (infer) formula ===\n"); print(formula_c1_infer)
m1_c_infer <- gam(formula_c1_infer, data = dat,
                  family = binomial(link = "logit"),
                  method = "REML")
cat("\n=== Method C | Part 1 (infer) summary ===\n"); print(summary(m1_c_infer))
cat(sprintf("\nMethod C | Part 1 infer AIC: %.1f\n\n", AIC(m1_c_infer)))
print(summary(m2_c))

saveRDS(m1_c_viz,   file = paste0("output/", NAME, "-fitting_m1_c_viz.rda"))
saveRDS(m1_c_infer, file = paste0("output/", NAME, "-fitting_m1_c_infer.rda"))
saveRDS(m2_c,       file = paste0("output/", NAME, "-fitting_m2_c.rda"))


# 4. APCtools native visualization ----
font.base <- "Times New Roman"
theme_ald <- theme_minimal(base_family = font.base, base_size = 13) +
  theme(
    legend.position  = "bottom",
    strip.background = element_rect(fill = "gray95"),
    panel.grid.minor = element_blank()
  )


## 4-1. Heatmaps of tensor product surface ----
# plot_APCheatmap automatically exponentiates predictions when the model link
# is log or logit (-> odds scale for Part 1, expenditure scale for Part 2).
# Diagonal lines mark the 5 ALD birth cohorts for interpretability.

p_heat_c1 <- plot_APCheatmap(
  dat            = dat,
  model          = m1_c_viz,
  bin_heatmap    = FALSE,
  plot_CI        = FALSE,
  markLines_list = list(cohort = COHORT_BIRTH_YEARS)
) + labs(
  title    = "Part 1 (viz): te(age, period) surface",
  subtitle = "Color: odds of visiting | Diagonal lines: ALD cohort birth years"
)

p_heat_c2 <- plot_APCheatmap(
  dat            = dat_visited,
  model          = m2_c,
  bin_heatmap    = FALSE,
  plot_CI        = FALSE,
  markLines_list = list(cohort = COHORT_BIRTH_YEARS)
) + labs(
  title    = "Part 2: te(age, period) surface",
  subtitle = "Color: conditional expenditure (JPY) | Diagonal: ALD cohorts"
)


## 4-2. Marginal APC effects ----
# Marginal effects are computed by averaging the te() surface over one dimension.
# The period marginal from m1_c_viz captures the smooth period trend embedded in te();
# it does NOT include the discrete jump effects (those live in m1_c_infer's eta_i).

# --- Part 1 ---
p_marg_age_c1 <- plot_marginalAPCeffects(
  model    = m1_c_viz,
  dat      = dat,
  variable = "age",
  plot_CI  = TRUE
) + labs(title = "Marginal age effect: Part 1 (viz model)")

p_marg_per_c1 <- plot_marginalAPCeffects(
  model      = m1_c_viz,
  dat        = dat,
  variable   = "period",
  plot_CI    = TRUE,
  vlines_vec = REVISION_YEARS
) + labs(
  title    = "Marginal period effect: Part 1 (viz model)",
  subtitle = "Dotted: revision years | Smooth-only; jumps in inference model"
)

p_marg_coh_c1 <- plot_marginalAPCeffects(
  model      = m1_c_viz,
  dat        = dat,
  variable   = "cohort",
  plot_CI    = TRUE,
  vlines_vec = COHORT_BIRTH_YEARS
) + labs(
  title    = "Marginal cohort effect: Part 1 (viz model)",
  subtitle = "Dotted: ALD cohort birth years | cohort = period - age"
)

# --- Part 2 ---
p_marg_age_c2 <- plot_marginalAPCeffects(
  model    = m2_c,
  dat      = dat_visited,
  variable = "age",
  plot_CI  = TRUE
) + labs(title = "Marginal age effect: Part 2")

p_marg_per_c2 <- plot_marginalAPCeffects(
  model      = m2_c,
  dat        = dat_visited,
  variable   = "period",
  plot_CI    = TRUE,
  vlines_vec = REVISION_YEARS
) + labs(title = "Marginal period effect: Part 2")

p_marg_coh_c2 <- plot_marginalAPCeffects(
  model      = m2_c,
  dat        = dat_visited,
  variable   = "cohort",
  plot_CI    = TRUE,
  vlines_vec = COHORT_BIRTH_YEARS
) + labs(title = "Marginal cohort effect: Part 2")


# 5. Validation: compare to true DGP ----
# Use m1_c_infer (with jump dummies) for Part 1 validation.
# For reference point: period = 2014 (pre-revision baseline, all dummies = 0).
# At period = 2014, cohort = 2014 - age; we compare to true_cohort_effect(1950).
# Note: te(age, period) conflates age and cohort, so fitted vs true comparison
#   is approximate — the true separation only holds in Methods A and B.

pal_c <- c("True" = "tomato", "Method C (APCtools)" = "purple4")


## 5-1. Age effect (fixed period = 2014, all dummies = 0) ----
age_grid_c <- tibble(
  age       = 50:79,
  period    = 2014,
  post_2012 = 0L, post_2013 = 0L, post_2014 = 0L,
  post_2015 = 0L, post_2016 = 0L, post_2017 = 0L, post_2018 = 0L
)

pred1_c <- predict(m1_c_infer, newdata = age_grid_c, type = "link", se.fit = TRUE)
pred2_c <- predict(m2_c,       newdata = age_grid_c, type = "link", se.fit = TRUE)

age_grid_c <- age_grid_c %>%
  mutate(
    # Part 1
    fit_logit = pred1_c$fit, se_logit = pred1_c$se.fit,
    fit_prob  = plogis(fit_logit),
    lo_prob   = plogis(fit_logit - 1.96 * se_logit),
    hi_prob   = plogis(fit_logit + 1.96 * se_logit),
    # True: DGP age effect + cohort 1950 (period=2014 => cohort=2014-age != 1950 in general,
    # but we fix the comparison at birth_year=1950 as the reference)
    true_prob = plogis(true_age_logit(age) + true_cohort_effect(1950)),
    # Part 2
    fit_log  = pred2_c$fit, se_log = pred2_c$se.fit,
    fit_amt  = exp(fit_log),
    lo_amt   = exp(fit_log - 1.96 * se_log),
    hi_amt   = exp(fit_log + 1.96 * se_log),
    true_amt = exp(true_age_log_amount(age) + true_cohort_effect(1950) * 0.5),
    # E[Y]
    fit_ey   = fit_prob * fit_amt,
    true_ey  = true_prob * true_amt
  )

p_age_prob_c <- ggplot(age_grid_c, aes(x = age)) +
  geom_ribbon(aes(ymin = lo_prob, ymax = hi_prob), fill = "purple4", alpha = 0.2) +
  geom_line(aes(y = fit_prob, color = "Method C (APCtools)"), linewidth = 1.2) +
  geom_line(aes(y = true_prob, color = "True"), linewidth = 1.2, linetype = "dashed") +
  scale_color_manual(values = pal_c) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title    = "Part 1: Frequency",
    subtitle = "Ref: period=2014 (pre-revision), cohort~1950 | Ribbon: 95% CI",
    x = "Age", y = "Visit probability", color = NULL
  ) + theme_ald

p_age_amt_c <- ggplot(age_grid_c, aes(x = age)) +
  geom_ribbon(aes(ymin = lo_amt, ymax = hi_amt), fill = "purple4", alpha = 0.2) +
  geom_line(aes(y = fit_amt, color = "Method C (APCtools)"), linewidth = 1.2) +
  geom_line(aes(y = true_amt, color = "True"), linewidth = 1.2, linetype = "dashed") +
  scale_color_manual(values = pal_c) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "Part 2: Incurred expenditure",
    subtitle = "Ref: period=2014, cohort~1950 | Ribbon: 95% CI",
    x = "Age", y = "Mean incurred expenditure", color = NULL
  ) + theme_ald

p_age_ey_c <- ggplot(age_grid_c, aes(x = age)) +
  geom_line(aes(y = fit_ey,  color = "Method C (APCtools)"), linewidth = 1.2) +
  geom_line(aes(y = true_ey, color = "True"), linewidth = 1.2, linetype = "dashed") +
  scale_color_manual(values = pal_c) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "Frequency * Incurred expenditure",
    subtitle = "Ref: period=2014, cohort~1950",
    x = "Age", y = "Expected expenditure", color = NULL
  ) + theme_ald


## 5-2. Cohort effect ----
# In APCtools, cohort = period - age. To read off the cohort effect at each
# birth year, we evaluate te(age, period) along the diagonal at mid-age for
# each ALD cohort (age = midpoint of observed age range, period = birth_year + age).
# This is the most natural evaluation in the te() framework.
#
# Caveat: the te() surface conflates age and cohort trends. The extracted
# "cohort effect" here is the net diagonal variation at mid-age, not a clean
# separation as in Methods A & B.

cohort_val_df <- tibble(birth_year = COHORT_BIRTH_YEARS) %>%
  mutate(
    age_min   = 2010 - birth_year,
    age_max   = 2019 - birth_year,
    age       = as.integer(round((age_min + age_max) / 2)),
    period    = birth_year + age,
    post_2012 = as.integer(period >= 2012),
    post_2013 = as.integer(period >= 2013),
    post_2014 = as.integer(period >= 2014),
    post_2015 = as.integer(period >= 2015),
    post_2016 = as.integer(period >= 2016),
    post_2017 = as.integer(period >= 2017),
    post_2018 = as.integer(period >= 2018)
  )

pred_coh_c <- predict(m1_c_infer, newdata = cohort_val_df, type = "link", se.fit = TRUE)

cohort_val_df <- cohort_val_df %>%
  mutate(
    fit_logit = pred_coh_c$fit, se_logit = pred_coh_c$se.fit,
    fit_cen   = fit_logit - mean(fit_logit),
    # True: age effect (varies by cohort mid-age) + cohort effect, then centered
    true_raw  = true_age_logit(age) + true_cohort_effect(birth_year),
    true_cen  = true_raw - mean(true_raw),
    lo_cen    = fit_cen - 1.96 * se_logit,
    hi_cen    = fit_cen + 1.96 * se_logit
  )

p_cohort_c <- ggplot(cohort_val_df, aes(x = birth_year)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray60") +
  geom_ribbon(aes(ymin = lo_cen, ymax = hi_cen), fill = "purple4", alpha = 0.2) +
  geom_line(aes(y = fit_cen,  color = "Method C (APCtools)"), linewidth = 1.2) +
  geom_point(aes(y = fit_cen, color = "Method C (APCtools)"), size = 3) +
  geom_line(aes(y = true_cen, color = "True"), linewidth = 1.2, linetype = "dashed") +
  geom_point(aes(y = true_cen, color = "True"), size = 3, shape = 17) +
  scale_color_manual(values = pal_c) +
  scale_x_continuous(breaks = COHORT_BIRTH_YEARS) +
  labs(
    title    = "Cohort Effect (centered, logit scale)",
    subtitle = "Evaluated at each cohort's mid-age on the te() diagonal | 95% CI",
    x = "Birth year", y = "Effect (logit, centered)", color = NULL
  ) + theme_ald


## 5-3. Period effect: revision jump coefficients from inference model ----
coef_m1_infer <- coef(m1_c_infer)
vcov_m1_infer <- vcov(m1_c_infer)
se_m1_infer   <- sqrt(diag(vcov_m1_infer))

jump_df_c <- tibble(
  revision_year = REVISION_YEARS,
  true_val      = as.numeric(true_theta),
  estimate      = coef_m1_infer[jump_vars],
  se            = se_m1_infer[jump_vars]
) %>%
  mutate(
    lo      = estimate - 1.96 * se,
    hi      = estimate + 1.96 * se,
    covered = true_val >= lo & true_val <= hi,
    bias    = estimate - true_val
  )

p_jump_c <- ggplot(jump_df_c, aes(x = factor(revision_year))) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray60") +
  geom_pointrange(
    aes(y = estimate, ymin = lo, ymax = hi, color = "Method C (APCtools)"),
    size = 0.9, linewidth = 1.0
  ) +
  geom_point(aes(y = true_val, color = "True"), size = 4, shape = 17) +
  geom_text(
    aes(y = hi + 0.01, label = ifelse(covered, "covered", "NOT covered")),
    size = 3.5, color = "gray40", vjust = 0
  ) +
  scale_color_manual(values = pal_c) +
  labs(
    title    = "Period Effect: Revision Jump Coefficients (Part 1, logit scale)",
    subtitle = "Eta_i from inference model | Triangle: true | Point+range: estimate +/- 1.96 SE",
    x = "Revision year", y = "Jump coefficient (logit)", color = NULL
  ) + theme_ald


## 5-4. Period smooth from te() (viz model) vs true cumulative jumps ----
# The viz model's te() surface represents the smooth period variation embedded
# in the APC structure. Compare to the true cumulative period effect trajectory.
period_grid_c <- tibble(
  obs_year = 2010:2019,
  period   = 2010:2019,
  age      = 65
)

pred_per_viz <- predict(m1_c_viz, newdata = period_grid_c, type = "link", se.fit = TRUE)

true_period_cumul <- sapply(2010:2019, function(yr) {
  revs <- REVISION_YEARS[REVISION_YEARS <= yr]
  sum(true_theta[as.character(revs)])
})

period_grid_c <- period_grid_c %>%
  mutate(
    fit_log     = pred_per_viz$fit, se_log = pred_per_viz$se.fit,
    fit_log_cen = fit_log - mean(fit_log),
    lo_cen      = fit_log_cen - 1.96 * se_log,
    hi_cen      = fit_log_cen + 1.96 * se_log,
    true_cen    = true_period_cumul - mean(true_period_cumul)
  )

p_period_smooth_c <- ggplot(period_grid_c, aes(x = obs_year)) +
  geom_ribbon(aes(ymin = lo_cen, ymax = hi_cen), fill = "purple4", alpha = 0.2) +
  geom_line(aes(y = fit_log_cen, color = "Method C (APCtools)"), linewidth = 1.2) +
  geom_step(aes(y = true_cen,   color = "True"), linewidth = 1.2, linetype = "dashed") +
  geom_vline(xintercept = REVISION_YEARS, linetype = "dotted", color = "gray60") +
  scale_color_manual(values = pal_c) +
  scale_x_continuous(breaks = 2010:2019) +
  labs(
    title    = "Period Smooth in te() vs True Cumulative Jumps (Part 1, logit)",
    subtitle = "Viz model (no dummies) | Both centered | Fixed age=65 | Dotted: revision years",
    x = "Obs year", y = "Period smooth (logit, centered)", color = NULL
  ) + theme_ald


## 5-5. Residual diagnostics (Part 2) ----
dat_visited_c <- dat_visited %>%
  mutate(
    fitted_log = predict(m2_c, type = "link"),
    resid_dev  = residuals(m2_c, type = "deviance")
  )

p_resid1_c <- ggplot(dat_visited_c, aes(x = fitted_log, y = resid_dev)) +
  geom_point(alpha = 0.15, size = 0.8, color = "purple4") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "tomato") +
  geom_smooth(method = "loess", se = FALSE, color = "tomato", linewidth = 0.8) +
  labs(
    title = "Part 2: Deviance Residuals vs Fitted",
    x = "Fitted (log)", y = "Deviance residual"
  ) + theme_ald

p_resid2_c <- ggplot(dat_visited_c, aes(sample = resid_dev)) +
  stat_qq(alpha = 0.2, size = 0.8, color = "purple4") +
  stat_qq_line(color = "tomato", linewidth = 0.9) +
  labs(
    title = "Part 2: QQ Plot of Deviance Residuals",
    x = "Theoretical quantiles", y = "Sample quantiles"
  ) + theme_ald


# 6. Save figures ----

## 6-1. APCtools native plots ----
# Heatmaps + marginal effects from the APCtools framework
fig_apc_native <- (p_heat_c1 | p_heat_c2) /
  (p_marg_age_c1 | p_marg_per_c1 | p_marg_coh_c1) /
  (p_marg_age_c2 | p_marg_per_c2 | p_marg_coh_c2)

ggsave(
  paste0("fig/", NAME, "-fitting_APCtools_native.jpg"),
  fig_apc_native, width = 18, height = 18, dpi = 300
)


## 6-2. Validation plots (matched to Method A & B layout) ----
fig_c <- (p_age_prob_c | p_age_amt_c) /
  (p_age_ey_c   | p_cohort_c) /
  (p_jump_c     | p_period_smooth_c) /
  (p_resid1_c   | p_resid2_c)

ggsave(
  paste0("fig/", NAME, "-fitting_APCtools.jpg"),
  fig_c, width = 16, height = 22, dpi = 300
)
cat("\nAll figures saved.\n")


# 7. (Optional) 3-way comparison: Methods A, B, C ----
compare_all_methods <- function(m1_a, m2_a, m1_b, m2_b) {
  pal3 <- c(
    "True"                  = "tomato",
    "Method A (jump dummies)" = "steelblue",
    "Method B (Carstensen)" = "darkorange",
    "Method C (APCtools)"   = "purple4"
  )

  ref_a <- tibble(
    age = 50:79, birth_year = 1950,
    post_2012 = 0L, post_2013 = 0L, post_2014 = 0L,
    post_2015 = 0L, post_2016 = 0L, post_2017 = 0L, post_2018 = 0L
  )

  cmp <- tibble(
    age      = 50:79,
    true_prob = plogis(true_age_logit(50:79) + true_cohort_effect(1950)),
    A_prob    = plogis(predict(m1_a, newdata = ref_a, type = "link")),
    B_prob    = plogis(predict(m1_b, newdata = age_grid_c, type = "link")),
    C_prob    = plogis(pred1_c$fit),
    true_amt  = exp(true_age_log_amount(50:79) + true_cohort_effect(1950) * 0.5),
    A_amt     = exp(predict(m2_a, newdata = ref_a, type = "link")),
    B_amt     = exp(predict(m2_b, newdata = age_grid_c, type = "link")),
    C_amt     = exp(pred2_c$fit)
  )

  p_cmp_prob <- cmp %>%
    pivot_longer(c(true_prob, A_prob, B_prob, C_prob),
                 names_to = "src", values_to = "prob") %>%
    mutate(src = recode(src,
      true_prob = "True",
      A_prob    = "Method A (jump dummies)",
      B_prob    = "Method B (Carstensen)",
      C_prob    = "Method C (APCtools)"
    )) %>%
    ggplot(aes(x = age, y = prob, color = src, linetype = src)) +
    geom_line(linewidth = 1.2) +
    scale_color_manual(values = pal3) +
    scale_linetype_manual(values = c(
      "True"                  = "dashed",
      "Method A (jump dummies)" = "solid",
      "Method B (Carstensen)" = "solid",
      "Method C (APCtools)"   = "solid"
    )) +
    scale_y_continuous(labels = scales::percent) +
    labs(
      title    = "Part 1: Visit Probability — Methods A, B, C",
      subtitle = "Ref: birth_year=1950, period=2014 (pre-revision)",
      x = "Age", y = "Visit probability", color = NULL, linetype = NULL
    ) + theme_ald

  p_cmp_amt <- cmp %>%
    pivot_longer(c(true_amt, A_amt, B_amt, C_amt),
                 names_to = "src", values_to = "amt") %>%
    mutate(src = recode(src,
      true_amt = "True",
      A_amt    = "Method A (jump dummies)",
      B_amt    = "Method B (Carstensen)",
      C_amt    = "Method C (APCtools)"
    )) %>%
    ggplot(aes(x = age, y = amt, color = src, linetype = src)) +
    geom_line(linewidth = 1.2) +
    scale_color_manual(values = pal3) +
    scale_linetype_manual(values = c(
      "True"                  = "dashed",
      "Method A (jump dummies)" = "solid",
      "Method B (Carstensen)" = "solid",
      "Method C (APCtools)"   = "solid"
    )) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title    = "Part 2: Conditional Expenditure — Methods A, B, C",
      subtitle = "Ref: birth_year=1950, period=2014 (pre-revision)",
      x = "Age", y = "Mean incurred expenditure", color = NULL, linetype = NULL
    ) + theme_ald

  fig_cmp <- p_cmp_prob | p_cmp_amt
  ggsave(
    paste0("fig/", NAME, "-fitting_compare_ABC.jpg"),
    fig_cmp, width = 18, height = 7, dpi = 300
  )
  cat("3-way comparison figure saved.\n")
  invisible(fig_cmp)
}

# Uncomment to run after sourcing Method A and B scripts:
# compare_all_methods(
#   m1_a = readRDS(paste0("output/", NAME, "-fitting_m1_a.rda")),
#   m2_a = readRDS(paste0("output/", NAME, "-fitting_m2_a.rda")),
#   m1_b = readRDS(paste0("output/", NAME, "-fitting_m1_b.rda")),
#   m2_b = readRDS(paste0("output/", NAME, "-fitting_m2_b.rda"))
# )
