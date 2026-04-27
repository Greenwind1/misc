# =============================================================================
# ALD Simulation - Method B: Carstensen-style APC Hurdle Model
#
# Carstensen (2007) parametrization applied to individual-level data:
#   - Age:    cubic regression spline (full: linear + nonlinear)
#   - Period: cubic regression spline (full: linear + nonlinear)
#   - Cohort: nonlinear part ONLY  (linear slope constrained to zero)
#   - Period jumps: post-revision step dummies (Part 2 only)
#
# Key references:
#   Carstensen, B. (2007). Age-period-cohort models for the Lexis diagram.
#     Statistics in Medicine, 26(15), 3018-3045. doi:10.1002/sim.2764
#   Carstensen, B., Plummer, M., Laara, E., & Hills, M. (2021).
#     Epi: A package for statistical analysis in epidemiology.
#     https://CRAN.R-project.org/package=Epi
#
# Design note:
#   Epi::apc.fit() targets tabulated rate data (cases + person-years).
#   For individual-level Hurdle data we replicate the same identification
#   constraint manually inside gam(): include s(obs_year) as the period
#   smooth and restrict cohort (birth_year) to its nonlinear component only,
#   removing the identifiable linear drift.
# =============================================================================

library(mgcv)
library(splines)   # ns(), poly()
library(tidyverse)
library(patchwork)
set.seed(2026)

NAME <- "ald_simulation_04"

# =============================================================================
# 0. Study design parameters & true DGP functions
# =============================================================================
REVISION_YEARS     <- c(2012, 2014, 2016, 2018)
COHORT_BIRTH_YEARS <- c(1940, 1945, 1950, 1955, 1960)
jump_vars          <- paste0("post_", REVISION_YEARS)

true_age_logit <- function(age) {
  -2.5 + 0.04 * pmax(age - 50, 0) + 0.06 * pmax(age - 70, 0)
}
true_age_log_amount <- function(age) {
  8.5 + 0.025 * pmax(age - 50, 0) + 0.05 * pmax(age - 70, 0)
}
true_cohort_effect <- function(birth_year) {
  x <- birth_year - 1950
  0.3 * (x / 10)^2 - 0.2
}
true_theta <- c(`2012` = -0.08, `2014` = 0.05, `2016` = -0.06, `2018` = 0.04)


# =============================================================================
# 1. Load data and construct Carstensen cohort variable
# =============================================================================
dat <- read_csv(paste0("input/", NAME, "-data.csv"))

# ---------------------------------------------------------------------------
# Carstensen identification constraint:
#   The linear (drift) component of cohort is unidentifiable from A + P and
#   is set to zero.  Only the nonlinear curvature of birth_year is retained.
#
# Implementation A [used here – 5 discrete cohort levels]:
#   poly(birth_year, degree=2, raw=FALSE) produces two orthogonal components:
#     [,1]  linear      → EXCLUDED  (Carstensen constraint)
#     [,2]  quadratic   → INCLUDED  (captures U-shaped cohort effect)
#   The poly() object is saved so that predict() on new data uses the same
#   centering and scaling as the training data.
#
# Implementation B [for continuous cohort, e.g., production NDB data]:
#   make_cohort_nl_basis() builds a natural spline basis for birth_year
#   and projects out the linear component by regression, returning the
#   orthogonalized residual basis.  See helper function below.
# ---------------------------------------------------------------------------

poly_cohort <- poly(dat$birth_year, 2)   # store poly object for later prediction
dat <- dat %>%
  mutate(cohort_nl = poly_cohort[, 2])   # quadratic component only

cat("=== Cohort variable summary ===\n")
cat("birth_year distribution:\n"); print(table(dat$birth_year))
cat(sprintf("cohort_nl range: [%.4f, %.4f]\n\n", min(dat$cohort_nl), max(dat$cohort_nl)))


# ---------------------------------------------------------------------------
# Helper: general-purpose Carstensen cohort basis (continuous cohort)
# ---------------------------------------------------------------------------
# Usage (example):
#   B_nl <- make_cohort_nl_basis(dat$birth_year, df = 4)
#   dat  <- cbind(dat, setNames(as.data.frame(B_nl),
#                               paste0("cnl", seq_len(ncol(B_nl)))))
#   # Then add cnl1 + cnl2 + ... to the formula instead of cohort_nl
# ---------------------------------------------------------------------------
make_cohort_nl_basis <- function(x, df = 3) {
  B     <- ns(x, df = df)                      # natural spline basis (full)
  x_c   <- x - mean(x)                         # center cohort
  coef  <- lm.fit(cbind(1, x_c), B)$coef       # fit linear part per basis col
  B_nl  <- B - cbind(1, x_c) %*% coef          # residualize → remove linear
  keep  <- apply(B_nl, 2, var) > 1e-12         # drop zero-variance columns
  B_nl[, keep, drop = FALSE]
}


# =============================================================================
# 2. Method B – Part 1: Visit probability  (binomial / logit)
# =============================================================================
# Model:
#   logit P(visit_it = 1) = f_A(age_it) + f_P(obs_year_it) + γ · cohort_nl_i
#
# Differences vs Method A:
#   Method A  visited ~ s(age) + s(birth_year)
#   Method B  visited ~ s(age) + s(obs_year) + cohort_nl  (parametric term)
#
# s(obs_year) captures the smooth secular trend in visit probability.
# cohort_nl (quadratic, no linear) captures the U-shaped cohort effect.
# No fee-revision jump terms: revisions affect reimbursed COST, not visit prob.

formula_b1 <- visited ~ s(age, bs = "cr", k = 10) +
                         s(obs_year, bs = "cr", k = 6) +
                         cohort_nl

cat("=== Method B | Part 1 formula ===\n")
print(formula_b1)

m1_b <- gam(
  formula_b1,
  data   = dat,
  family = binomial(link = "logit"),
  method = "REML"
)
cat("\n=== Method B | Part 1 summary ===\n")
print(summary(m1_b))


# =============================================================================
# 3. Method B – Part 2: Conditional expenditure  (Gamma / log)
# =============================================================================
# Model (visitors only):
#   log E[cost_it | visit=1] = f_A(age_it) + f_P(obs_year_it)
#                             + γ · cohort_nl_i
#                             + Σ_r θ_r · D_{r,it}
#
# D_{r,it} = 1(obs_year_it >= r): cumulative post-revision step dummy.
# f_P(obs_year) captures smooth secular cost trends between revisions.
# The two period components are separable: f_P absorbs gradual drift,
# θ_r captures the discrete jump at each fee revision boundary.

formula_b2 <- as.formula(paste0(
  "medical_cost ~ s(age, bs='cr', k=10) + ",
  "s(obs_year, bs='cr', k=6) + ",
  "cohort_nl + ",
  paste(jump_vars, collapse = " + ")
))

cat("\n=== Method B | Part 2 formula ===\n")
print(formula_b2)

m2_b <- gam(
  formula_b2,
  data   = filter(dat, visited == 1),
  family = Gamma(link = "log"),
  method = "REML"
)
cat("\n=== Method B | Part 2 summary ===\n")
print(summary(m2_b))

cat(sprintf("\nMethod B | Part 1 AIC: %.1f\n", AIC(m1_b)))
cat(sprintf("Method B | Part 2 AIC: %.1f\n",  AIC(m2_b)))


# =============================================================================
# 4. Validation plots (Method B only; comparison vs A requires m1, m2 loaded)
# =============================================================================
font.base <- "Times New Roman"
theme_ald <- theme_minimal(base_family = font.base, base_size = 13) +
  theme(
    legend.position  = "bottom",
    strip.background = element_rect(fill = "gray95"),
    panel.grid.minor = element_blank()
  )
pal <- c("True" = "tomato", "Method B (Carstensen)" = "darkorange")

# ---------------------------------------------------------------------------
# Helper: predict cohort_nl for new birth_year value(s)
# ---------------------------------------------------------------------------
# Must use the stored poly_cohort object so that centering/scaling matches
# the training data.  predict.poly() handles this correctly.
pred_cohort_nl <- function(new_by) {
  predict(poly_cohort, newdata = new_by)[, 2]
}


# ---------------------------------------------------------------------------
# 4-1. Age effect
# ---------------------------------------------------------------------------
# Prediction grid: reference cohort birth_year=1950, period mid-window 2014,
# all revision dummies = 0  (isolates pure age trajectory).
#
# NOTE on obs_year choice:
#   Method A does not include obs_year, so "age effect" is independent of
#   calendar time.  Method B has s(obs_year), so we must fix obs_year.
#   We use 2014 (window midpoint) as the period reference.
#   Setting jump dummies to 0 removes the fee-revision component.

age_grid <- tibble(
  age        = 50:79,
  birth_year = 1950,
  obs_year   = 2014,                          # period reference
  cohort_nl  = pred_cohort_nl(1950),          # reference cohort
  post_2012  = 0L, post_2014 = 0L, post_2016 = 0L, post_2018 = 0L
)

# --- Part 1: visit probability ---
pred1_b <- predict(m1_b, newdata = age_grid, type = "link", se.fit = TRUE)
age_grid <- age_grid %>%
  mutate(
    fit_logit = pred1_b$fit,
    se_logit  = pred1_b$se.fit,
    fit_prob  = plogis(fit_logit),
    lo_prob   = plogis(fit_logit - 1.96 * se_logit),
    hi_prob   = plogis(fit_logit + 1.96 * se_logit),
    true_prob = plogis(true_age_logit(age) + true_cohort_effect(1950))
  )

p_age_prob_b <- ggplot(age_grid, aes(x = age)) +
  geom_ribbon(aes(ymin = lo_prob, ymax = hi_prob),
              fill = "darkorange", alpha = 0.2) +
  geom_line(aes(y = fit_prob,  color = "Method B (Carstensen)"), linewidth = 1.2) +
  geom_line(aes(y = true_prob, color = "True"),
            linewidth = 1.2, linetype = "dashed") +
  scale_color_manual(values = pal) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title    = "Part 1: Visit Probability",
    subtitle = "Reference: birth_year=1950, obs_year=2014, jump dummies=0 | 95% CI",
    x = "Age", y = "Visit probability", color = NULL
  ) +
  theme_ald

# --- Part 2: conditional expenditure ---
pred2_b <- predict(m2_b, newdata = age_grid, type = "link", se.fit = TRUE)
age_grid <- age_grid %>%
  mutate(
    fit_log = pred2_b$fit,
    se_log  = pred2_b$se.fit,
    fit_amt = exp(fit_log),
    lo_amt  = exp(fit_log - 1.96 * se_log),
    hi_amt  = exp(fit_log + 1.96 * se_log),
    true_amt = exp(true_age_log_amount(age) + true_cohort_effect(1950) * 0.5)
  )

p_age_amt_b <- ggplot(age_grid, aes(x = age)) +
  geom_ribbon(aes(ymin = lo_amt, ymax = hi_amt),
              fill = "darkorange", alpha = 0.2) +
  geom_line(aes(y = fit_amt,  color = "Method B (Carstensen)"), linewidth = 1.2) +
  geom_line(aes(y = true_amt, color = "True"),
            linewidth = 1.2, linetype = "dashed") +
  scale_color_manual(values = pal) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "Part 2: Conditional Expenditure",
    subtitle = "Reference: birth_year=1950, obs_year=2014, jump dummies=0 | 95% CI",
    x = "Age", y = "Mean incurred expenditure", color = NULL
  ) +
  theme_ald

# --- Combined E[Y] = pi * mu ---
age_grid <- age_grid %>%
  mutate(
    fit_ey  = fit_prob * fit_amt,
    true_ey = true_prob * true_amt
  )

p_age_ey_b <- ggplot(age_grid, aes(x = age)) +
  geom_line(aes(y = fit_ey,  color = "Method B (Carstensen)"), linewidth = 1.2) +
  geom_line(aes(y = true_ey, color = "True"),
            linewidth = 1.2, linetype = "dashed") +
  scale_color_manual(values = pal) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "E[Y] = P(visit) × E[cost | visit]",
    subtitle = "Reference: birth_year=1950, obs_year=2014",
    x = "Age", y = "Expected expenditure", color = NULL
  ) +
  theme_ald


# ---------------------------------------------------------------------------
# 4-2. Cohort effect
# ---------------------------------------------------------------------------
# Compare fitted cohort_nl coefficient vs true cohort shape.
# We extract the marginal cohort effect by predicting at fixed age=65, obs_year=2014
# across all 5 cohort levels, then centering both fitted and true values.

cohort_grid <- tibble(
  birth_year = COHORT_BIRTH_YEARS,
  age        = 65,
  obs_year   = 2014,
  cohort_nl  = pred_cohort_nl(COHORT_BIRTH_YEARS),
  post_2012  = 0L, post_2014 = 0L, post_2016 = 0L, post_2018 = 0L
)

pred_coh_b <- predict(m1_b, newdata = cohort_grid, type = "link", se.fit = TRUE)
cohort_grid <- cohort_grid %>%
  mutate(
    fit_logit = pred_coh_b$fit,
    se_logit  = pred_coh_b$se.fit,
    fit_cen   = fit_logit - mean(fit_logit),
    true_cen  = (true_age_logit(65) + true_cohort_effect(birth_year)) -
                mean(true_age_logit(65) + true_cohort_effect(COHORT_BIRTH_YEARS)),
    lo_cen    = fit_cen - 1.96 * se_logit,
    hi_cen    = fit_cen + 1.96 * se_logit
  )

p_cohort_b <- ggplot(cohort_grid, aes(x = birth_year)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray60") +
  geom_ribbon(aes(ymin = lo_cen, ymax = hi_cen),
              fill = "darkorange", alpha = 0.2) +
  geom_line(aes(y = fit_cen,  color = "Method B (Carstensen)"), linewidth = 1.2) +
  geom_point(aes(y = fit_cen, color = "Method B (Carstensen)"), size = 3) +
  geom_line(aes(y = true_cen, color = "True"),
            linewidth = 1.2, linetype = "dashed") +
  geom_point(aes(y = true_cen, color = "True"), size = 3, shape = 17) +
  scale_color_manual(values = pal) +
  scale_x_continuous(breaks = COHORT_BIRTH_YEARS) +
  labs(
    title    = "Cohort Effect (centered, logit scale)",
    subtitle = "Fixed age=65, obs_year=2014 | Ribbon: 95% CI",
    x = "Birth year", y = "Cohort effect (logit, centered)", color = NULL
  ) +
  theme_ald


# ---------------------------------------------------------------------------
# 4-3. Period effect: revision jump coefficients (Part 2)
# ---------------------------------------------------------------------------
coef_m2b <- coef(m2_b)
vcov_m2b <- vcov(m2_b)
se_m2b   <- sqrt(diag(vcov_m2b))

jump_df_b <- tibble(
  revision_year = REVISION_YEARS,
  true_val      = as.numeric(true_theta),
  estimate      = coef_m2b[jump_vars],
  se            = se_m2b[jump_vars]
) %>%
  mutate(
    lo      = estimate - 1.96 * se,
    hi      = estimate + 1.96 * se,
    covered = true_val >= lo & true_val <= hi,
    bias    = estimate - true_val
  )

cat("\n=== Method B | Period jump coefficients (Part 2) ===\n")
print(jump_df_b %>% select(revision_year, true_val, estimate, se, bias, covered))

p_jump_b <- ggplot(jump_df_b, aes(x = factor(revision_year))) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray60") +
  geom_pointrange(
    aes(y = estimate, ymin = lo, ymax = hi, color = "Method B (Carstensen)"),
    size = 0.9, linewidth = 1.0
  ) +
  geom_point(aes(y = true_val, color = "True"), size = 4, shape = 17) +
  geom_text(
    aes(y = hi + 0.01,
        label = ifelse(covered, "covered", "NOT covered")),
    size = 3.5, color = "gray40", vjust = 0
  ) +
  scale_color_manual(values = pal) +
  labs(
    title    = "Period Effect: Revision Jump Coefficients (Part 2, log scale)",
    subtitle = "Triangle: true value | Point + range: estimate ± 1.96 SE",
    x = "Revision year", y = "Jump coefficient (log)", color = NULL
  ) +
  theme_ald


# ---------------------------------------------------------------------------
# 4-4. Residual diagnostics – Part 2
# ---------------------------------------------------------------------------
dat_visited_b <- dat %>%
  filter(visited == 1) %>%
  mutate(
    cohort_nl  = pred_cohort_nl(birth_year),   # ensure column present
    fitted_log = predict(m2_b, type = "link"),
    resid_dev  = residuals(m2_b, type = "deviance"),
    resid_pear = residuals(m2_b, type = "pearson")
  )

p_resid1_b <- ggplot(dat_visited_b, aes(x = fitted_log, y = resid_dev)) +
  geom_point(alpha = 0.15, size = 0.8, color = "darkorange") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "tomato") +
  geom_smooth(method = "loess", se = FALSE, color = "tomato", linewidth = 0.8) +
  labs(
    title = "Part 2: Deviance Residuals vs Fitted (log scale)",
    x = "Fitted (log)", y = "Deviance residual"
  ) +
  theme_ald

p_resid2_b <- ggplot(dat_visited_b, aes(sample = resid_dev)) +
  stat_qq(alpha = 0.2, size = 0.8, color = "darkorange") +
  stat_qq_line(color = "tomato", linewidth = 0.9) +
  labs(
    title = "Part 2: QQ Plot of Deviance Residuals",
    x = "Theoretical quantiles", y = "Sample quantiles"
  ) +
  theme_ald


# ---------------------------------------------------------------------------
# 4-5. Save figure
# ---------------------------------------------------------------------------
fig_b <- (p_age_prob_b | p_age_amt_b) /
          (p_age_ey_b  | p_cohort_b)  /
          (p_jump_b    | (p_resid1_b | p_resid2_b))

ggsave(paste0("fig/", NAME, "-fitting_Carstensen.jpg"),
       fig_b, width = 16, height = 18, dpi = 300)

cat("\nFigure saved: fig/", NAME, "-fitting_Carstensen.jpg\n")


# =============================================================================
# 5. (Optional) Side-by-side comparison with Method A
# =============================================================================
# Run this block AFTER sourcing ald_simulation_04-fitting.R (which loads m1, m2).
# It superimposes Method A and Method B curves on the same axes.

compare_methods <- function(m1_a, m2_a, m1_b, m2_b) {

  pal3 <- c(
    "True"                  = "tomato",
    "Method A (mgcv GAM)"   = "steelblue",
    "Method B (Carstensen)" = "darkorange"
  )

  # ---- Shared prediction grid ----
  # For Method A: no obs_year in model, birth_year=1950 is sufficient.
  # For Method B: obs_year=2014 is fixed as period reference.
  age_range  <- 50:79
  ref_grid_a <- tibble(
    age       = age_range,
    birth_year = 1950,
    post_2012 = 0L, post_2014 = 0L, post_2016 = 0L, post_2018 = 0L
  )
  ref_grid_b <- tibble(
    age        = age_range,
    birth_year = 1950,
    obs_year   = 2014,
    cohort_nl  = pred_cohort_nl(1950),
    post_2012  = 0L, post_2014 = 0L, post_2016 = 0L, post_2018 = 0L
  )

  # ---- Age effect: Part 1 ----
  p1_a <- predict(m1_a, newdata = ref_grid_a, type = "link", se.fit = TRUE)
  p1_b <- predict(m1_b, newdata = ref_grid_b, type = "link", se.fit = TRUE)

  cmp_age <- tibble(
    age      = age_range,
    true_p   = plogis(true_age_logit(age_range) + true_cohort_effect(1950)),
    fit_a    = plogis(p1_a$fit),
    fit_b    = plogis(p1_b$fit)
  ) %>%
    pivot_longer(c(true_p, fit_a, fit_b),
                 names_to = "source", values_to = "prob") %>%
    mutate(source = recode(source,
      true_p = "True",
      fit_a  = "Method A (mgcv GAM)",
      fit_b  = "Method B (Carstensen)"
    ))

  p_cmp_prob <- ggplot(cmp_age, aes(x = age, y = prob, color = source,
                                     linetype = source)) +
    geom_line(linewidth = 1.2) +
    scale_color_manual(values = pal3) +
    scale_linetype_manual(values = c("True" = "dashed",
                                      "Method A (mgcv GAM)"   = "solid",
                                      "Method B (Carstensen)" = "solid")) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Part 1: Visit Probability — Method A vs B",
         subtitle = "Ref: birth_year=1950, dummies=0 (B: obs_year=2014)",
         x = "Age", y = "Visit probability", color = NULL, linetype = NULL) +
    theme_ald

  # ---- Age effect: Part 2 ----
  p2_a <- predict(m2_a, newdata = ref_grid_a, type = "link", se.fit = TRUE)
  p2_b <- predict(m2_b, newdata = ref_grid_b, type = "link", se.fit = TRUE)

  cmp_amt <- tibble(
    age      = age_range,
    true_a   = exp(true_age_log_amount(age_range) + true_cohort_effect(1950) * 0.5),
    fit_a    = exp(p2_a$fit),
    fit_b    = exp(p2_b$fit)
  ) %>%
    pivot_longer(c(true_a, fit_a, fit_b),
                 names_to = "source", values_to = "amt") %>%
    mutate(source = recode(source,
      true_a = "True",
      fit_a  = "Method A (mgcv GAM)",
      fit_b  = "Method B (Carstensen)"
    ))

  p_cmp_amt <- ggplot(cmp_amt, aes(x = age, y = amt, color = source,
                                    linetype = source)) +
    geom_line(linewidth = 1.2) +
    scale_color_manual(values = pal3) +
    scale_linetype_manual(values = c("True" = "dashed",
                                      "Method A (mgcv GAM)"   = "solid",
                                      "Method B (Carstensen)" = "solid")) +
    scale_y_continuous(labels = scales::comma) +
    labs(title = "Part 2: Conditional Expenditure — Method A vs B",
         subtitle = "Ref: birth_year=1950, dummies=0 (B: obs_year=2014)",
         x = "Age", y = "Mean incurred expenditure", color = NULL, linetype = NULL) +
    theme_ald

  # ---- Period jump coefficients ----
  jump_cmp <- bind_rows(
    jump_df %>% mutate(method = "Method A (mgcv GAM)"),
    jump_df_b %>% mutate(method = "Method B (Carstensen)")
  )
  pal_method <- c("Method A (mgcv GAM)" = "steelblue",
                   "Method B (Carstensen)" = "darkorange")

  p_cmp_jump <- ggplot(jump_cmp,
                        aes(x = factor(revision_year), color = method,
                            group = method)) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "gray60") +
    geom_pointrange(
      aes(y = estimate, ymin = lo, ymax = hi),
      position = position_dodge(width = 0.4),
      size = 0.8, linewidth = 0.9
    ) +
    geom_point(aes(y = true_val), color = "tomato", shape = 17,
               size = 4, inherit.aes = FALSE,
               data = jump_cmp %>% distinct(revision_year, true_val),
               mapping = aes(x = factor(revision_year), y = true_val)) +
    scale_color_manual(values = pal_method) +
    labs(
      title    = "Period Jumps: Method A vs B (Part 2, log scale)",
      subtitle = "Triangle: true value | Point + range: estimate ± 1.96 SE",
      x = "Revision year", y = "Jump coefficient (log)", color = NULL
    ) +
    theme_ald

  fig_cmp <- (p_cmp_prob | p_cmp_amt) / p_cmp_jump
  ggsave(paste0("fig/", NAME, "-fitting_compare_AB.jpg"),
         fig_cmp, width = 16, height = 12, dpi = 300)
  cat("Comparison figure saved: fig/", NAME, "-fitting_compare_AB.jpg\n")
  invisible(fig_cmp)
}

# Uncomment after loading m1, m2 from Method A script:
# source("ald_simulation_04-fitting.R")
# compare_methods(m1, m2, m1_b, m2_b)
