# =============================================================================
# ALD Simulation: Carstensen-style APC Hurdle Model
#
# Identification constraint (Carstensen 2007, Section 6.2, Principle 3):
#   Period linear slope (drift) = 0  -->  drift attributed to Cohort
#
# Period effect representation:
#   A. Default Method: jump dummies only (discrete, no continuous smooth)
#   B. Carstensen-style Method: period_nl_* only (continuous nonlinear smooth, no jumps)
#
# The two methods differ in HOW they represent period effects:
#   Method A assumes period acts only at discrete revision boundaries.
#   Method B assumes period acts as a smooth nonlinear curve.
#
# Model structure:
#   Part 1 and Part 2:
#     Age:    s(age, bs='cr', k=10) full (linear + nonlinear)
#     Cohort: s(birth_year, bs='cr', k=5) full (linear + nonlinear)
#     Period: period_nl_* nonlinear only (linear drift = 0)
#
# References:
#   Carstensen, B. (2007). Age-period-cohort models for the Lexis diagram.
#   Statistics in Medicine, 26(15), 3018-3045. doi:10.1002/sim.2764
# =============================================================================

library(mgcv)
library(splines)
library(tidyverse)
library(extrafont)  # fonttable(); "Candara"
library(patchwork)
source("utility/environments.R")
set.seed(2026)


NAME <- "ald_simulation_03"


# 1. Study design parameters & true DGP functions ----
REVISION_YEARS <- c(2012, 2014, 2016, 2018)
# REVISION_YEARS <- c(2012, 2013, 2014, 2015, 2016, 2017, 2018)
# DOF_PERIOD <- 6  # AIC: 
# DOF_PERIOD <- 5  # AIC: 
# DOF_PERIOD <- 4  # AIC: 
# DOF_PERIOD <- 3  # AIC: 
DOF_PERIOD <- 2  # AIC: 
# knots = c(2015), Boundary.knots = c(2010, 2019)  # AIC: 25033.0
COHORT_BIRTH_YEARS <- c(1940, 1945, 1950, 1955, 1960)

true_age_logit <- function(age) {
  -2.5 +
    0.04 * pmax(age - 50, 0) +
    0.06 * pmax(age - 70, 0)
}
 
# True age effect on expenditure amount (log scale)
true_age_log_amount <- function(age) {
  8.5 +
    0.025 * pmax(age - 50, 0) +
    0.05  * pmax(age - 70, 0)
}
 
# Bathtub-shaped cohort effect (function of birth_year, not age)
# High risk for oldest cohorts, low for middle, very high for youngest
true_cohort_effect <- function(birth_year) {
  x <- birth_year - 1950  # center at 1950
  0.3 * (x / 10)^2 - 0.2  # U-shaped: 1940=->, 1950=min, 1960=->
}

# True period effect: step jumps at each fee revision year
true_theta <- c(
  `2012` = -0.08,
  `2014` =  0.05,
  `2016` = -0.06,
  `2018` =  0.04
)


# 2. Load data ----
dat <- read_csv(paste0("input/", NAME, "-data.csv"))


# 3. Construct period nonlinear basis ----
# Period nonlinear basis:
#   Build ns(obs_year, df=3), then project out the linear component.
#   The residual basis captures only nonlinear curvature in obs_year.
#   The linear drift is constrained to zero (Carstensen default).
mean_year <- mean(dat$obs_year)
x_c_ref <- dat$obs_year - mean_year
ns_period <- ns(dat$obs_year, df = DOF_PERIOD)
# ns_period <- ns(dat$obs_year, knots = c(2015), Boundary.knots = c(2010, 2019))
# ns_period <- ns(dat$obs_year, knots = c(2014, 2016), Boundary.knots = c(2010, 2019))
# ns_period <- ns(dat$obs_year, knots = c(2011, 2013, 2015, 2017), 
#                 Boundary.knots = c(2010, 2019))  # abnormal
print(attr(ns_period, "knots"))
coef_period <- lm.fit(cbind(1, x_c_ref), ns_period)$coef

make_period_nl <- function(x, ns_obj = ns_period) {
  B <- predict(ns_obj, newx = x)
  x_c <- x - mean_year
  B_nl <- B - cbind(1, x_c) %*% coef_period
  keep <- apply(predict(ns_period, newx = dat$obs_year), 2, var) > 1e-12
  B_nl[, keep, drop = FALSE]
}

B_period_nl_full <- make_period_nl(dat$obs_year)
n_pnl     <- ncol(B_period_nl_full)
pnl_names <- paste0("period_nl_", seq_len(n_pnl))

dat <- dat %>%
  bind_cols(setNames(as.data.frame(B_period_nl_full), pnl_names))

cat("=== Period nonlinear basis ===\n")
cat(sprintf("Retained columns: %d (%s)\n\n",
            n_pnl, paste(pnl_names, collapse = ", ")))

period_nl_formula_str <- paste(pnl_names, collapse = " + ")


# 4. Fitting ----
## 4-1. Part 1: Visit probability  (binomial / logit) ----
formula_b1 <- as.formula(paste0(
  "visited ~ s(age, bs='cr', k=10) + ",
  "s(birth_year, bs='cr', k=5)"
  # period_nl_formula_str
))
cat("=== Method B | Part 1 formula ===\n"); print(formula_b1)

m1_b <- gam(formula_b1, data = dat,
            family = binomial(link = "logit"), 
            method = "REML", optimizer = c("outer",  "bfgs"))
cat("\n=== Method B | Part 1 summary ===\n"); print(summary(m1_b))
saveRDS(m1_b, file = paste0("output/", NAME, "-fitting_m1_b.rda"))
cat(sprintf("\nMethod B | Part 1 AIC: %.1f\n", AIC(m1_b)))


## 4-2. Part 2: Conditional expenditure  (Gamma / log) ----
# No jump dummies -- period smooth (period_nl_*) absorbs all period variation,
# including the step-like behavior near revision years.
# The smooth will approximate the cumulative jumps as a piecewise curve.
formula_b2 <- as.formula(paste0(
  "medical_cost ~ s(age, bs='cr', k=10) + ",
  "s(birth_year, bs='cr', k=5) + ", 
  period_nl_formula_str
))
cat("\n=== Method B | Part 2 formula ===\n"); print(formula_b2)

m2_b <- gam(formula_b2, data = filter(dat, visited == 1),
            family = Gamma(link = "log"), method = "REML")
cat("\n=== Method B | Part 2 summary ===\n"); print(summary(m2_b))
saveRDS(m2_b, file = paste0("output/", NAME, "-fitting_m2_b.rda"))
cat(sprintf("Method B | Part 2 AIC: %.1f\n",  AIC(m2_b)))


# 5. Validation plots ----
font.base <- "Times New Roman"
theme_ald <- theme_minimal(base_family = font.base, base_size = 13) +
  theme(legend.position = "bottom",
        strip.background = element_rect(fill = "gray95"),
        panel.grid.minor = element_blank())
pal <- c("True" = "tomato", "Method B (Carstensen)" = "darkorange")

# Period reference point for prediction grids: window midpoint 2014 or window mean
period_nl_2014 <- make_period_nl(2014)
period_nl_mean <- colMeans(B_period_nl_full)

# Helper to attach period_nl at a fixed obs_year to a grid tibble
attach_period_nl <- function(grid_tbl, period_nl_vec, col_names) {
  grid_tbl %>%
    bind_cols(setNames(
      as.data.frame(matrix(
        rep(as.numeric(period_nl_vec), nrow(grid_tbl)),
        nrow = nrow(grid_tbl), ncol = length(col_names), byrow = TRUE
      )),
      col_names
    ))
}


# 5-1. Age effect ----
age_grid <- tibble(age = 50:79, birth_year = 1950) %>%
  attach_period_nl(period_nl_mean, pnl_names)
  # attach_period_nl(period_nl_2014, pnl_names)

pred1_b <- predict(m1_b, newdata = age_grid, type = "link", se.fit = TRUE)
pred2_b <- predict(m2_b, newdata = age_grid, type = "link", se.fit = TRUE)

age_grid <- age_grid %>%
  mutate(
    # Part 1
    fit_logit = pred1_b$fit, se_logit = pred1_b$se.fit,
    fit_prob  = plogis(fit_logit),
    lo_prob   = plogis(fit_logit - 1.96 * se_logit),
    hi_prob   = plogis(fit_logit + 1.96 * se_logit),
    true_prob = plogis(true_age_logit(age) + true_cohort_effect(1950)),
    # Part 2
    fit_log   = pred2_b$fit, se_log = pred2_b$se.fit,
    fit_amt   = exp(fit_log),
    lo_amt    = exp(fit_log - 1.96 * se_log),
    hi_amt    = exp(fit_log + 1.96 * se_log),
    true_amt  = exp(true_age_log_amount(age) + true_cohort_effect(1950) * 0.5),
    # E[Y]
    fit_ey    = fit_prob * fit_amt,
    true_ey   = true_prob * true_amt
  )

p_age_prob_b <- ggplot(age_grid, aes(x = age)) +
  geom_ribbon(aes(ymin = lo_prob, ymax = hi_prob), fill = "darkorange", alpha = 0.2) +
  geom_line(aes(y = fit_prob,  color = "Method B (Carstensen)"), linewidth = 1.2) +
  geom_line(aes(y = true_prob, color = "True"), linewidth = 1.2, linetype = "dashed") +
  scale_color_manual(values = pal) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Frequency",
       subtitle = "Ref: birth_year=1950, obs_year=mean over years, no jump dummies | 95% CI",
       x = "Age", y = "Visit probability", color = NULL) + theme_ald

p_age_amt_b <- ggplot(age_grid, aes(x = age)) +
  geom_ribbon(aes(ymin = lo_amt, ymax = hi_amt), fill = "darkorange", alpha = 0.2) +
  geom_line(aes(y = fit_amt,  color = "Method B (Carstensen)"), linewidth = 1.2) +
  geom_line(aes(y = true_amt, color = "True"), linewidth = 1.2, linetype = "dashed") +
  scale_color_manual(values = pal) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Incurred medical expenditure",
       subtitle = "Ref cohort: birth_year=1950, obs_year=mean over year, no jump dummies | 95% CI",
       x = "Age", y = "Mean incurred expenditure", color = NULL) + theme_ald

p_age_ey_b <- ggplot(age_grid, aes(x = age)) +
  geom_line(aes(y = fit_ey,  color = "Method B (Carstensen)"), linewidth = 1.2) +
  geom_line(aes(y = true_ey, color = "True"), linewidth = 1.2, linetype = "dashed") +
  scale_color_manual(values = pal) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Frequency * Incurred medical expenditure",
       subtitle = "Ref: birth_year=1950, obs_year=2014",
       x = "Age", y = "Expected expenditure", color = NULL) + theme_ald


# 5-2. Cohort effect ----
cohort_grid <- tibble(birth_year = COHORT_BIRTH_YEARS, age = 65) %>%
  attach_period_nl(period_nl_mean, pnl_names)
  # attach_period_nl(period_nl_2014, pnl_names)

pred_coh_b <- predict(m1_b, newdata = cohort_grid, type = "link", se.fit = TRUE)
cohort_grid <- cohort_grid %>%
  mutate(
    fit_logit = pred_coh_b$fit, se_logit = pred_coh_b$se.fit,
    fit_cen   = fit_logit - mean(fit_logit),
    true_cen  = (true_age_logit(65) + true_cohort_effect(birth_year)) -
                mean(true_age_logit(65) + true_cohort_effect(COHORT_BIRTH_YEARS)),
    lo_cen    = fit_cen - 1.96 * se_logit,
    hi_cen    = fit_cen + 1.96 * se_logit
  )

p_cohort_b <- ggplot(cohort_grid, aes(x = birth_year)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray60") +
  geom_ribbon(aes(ymin = lo_cen, ymax = hi_cen), fill = "darkorange", alpha = 0.2) +
  geom_line(aes(y = fit_cen,  color = "Method B (Carstensen)"), linewidth = 1.2) +
  geom_point(aes(y = fit_cen, color = "Method B (Carstensen)"), size = 3) +
  geom_line(aes(y = true_cen, color = "True"), linewidth = 1.2, linetype = "dashed") +
  geom_point(aes(y = true_cen, color = "True"), size = 3, shape = 17) +
  scale_color_manual(values = pal) +
  scale_x_continuous(breaks = COHORT_BIRTH_YEARS) +
  labs(title = "Frequency Cohort Effect (centered, logit scale)",
       subtitle = "Fixed age=65, obs_year=2014 | Cohort fully free | 95% CI",
       x = "Birth year", y = "Cohort effect (logit, centered)", color = NULL) + theme_ald


# 5-3. Period smooth: fitted vs true cumulative jump pattern (Part 2) ----
# Method B has no discrete jump coefficients to compare directly.
# Instead, we visualize the period smooth over the observation window
# alongside the true cumulative period effect (sum of jumps up to each year).

period_grid <- tibble(
  obs_year   = 2010:2019,
  birth_year = 1950,
  age        = 65
) %>%
  bind_cols(setNames(
    as.data.frame(make_period_nl(2010:2019)),
    pnl_names
  ))

pred_period_b2 <- predict(m1_b, newdata = period_grid, type = "link", se.fit = TRUE)

# True cumulative period effect at each year
true_period_cumul <- sapply(2010:2019, function(yr) {
  revs <- REVISION_YEARS[REVISION_YEARS <= yr]
  sum(true_theta[as.character(revs)])
})

period_grid <- period_grid %>%
  mutate(
    fit_log       = pred_period_b2$fit,
    se_log        = pred_period_b2$se.fit,
    fit_log_cen   = fit_log - mean(fit_log),
    lo_cen        = fit_log_cen - 1.96 * se_log,
    hi_cen        = fit_log_cen + 1.96 * se_log,
    true_cen      = true_period_cumul - mean(true_period_cumul)
  )

p_period_b <- ggplot(period_grid, aes(x = obs_year)) +
  geom_ribbon(aes(ymin = lo_cen, ymax = hi_cen), fill = "darkorange", alpha = 0.2) +
  geom_line(aes(y = fit_log_cen, color = "Method B (Carstensen)"), linewidth = 1.2) +
  geom_step(aes(y = true_cen,    color = "True"),
            linewidth = 1.2, linetype = "dashed") +
  geom_vline(xintercept = REVISION_YEARS, linetype = "dotted", color = "gray60") +
  scale_color_manual(values = pal) +
  scale_x_continuous(breaks = 2010:2019) + 
  ylim(-0.2, 0.2) + 
  labs(title = "Frequency Period Effect: Cumulative Revision Jump (logit scale)", 
       subtitle = "Both centered | Vertical dotted: revision years | 95% CI",
       x = "Obs year", y = "Period effect (log, centered)", color = NULL) + theme_ald


# 5-4. Residual diagnostics – Part 2 ----
dat_visited_b <- dat %>%
  filter(visited == 1) %>%
  mutate(
    fitted_log = predict(m2_b, type = "link"),
    resid_dev  = residuals(m2_b, type = "deviance")
  )

p_resid1_b <- ggplot(dat_visited_b, aes(x = fitted_log, y = resid_dev)) +
  geom_point(alpha = 0.15, size = 0.8, color = "darkorange") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "tomato") +
  geom_smooth(method = "loess", se = FALSE, color = "tomato", linewidth = 0.8) +
  labs(title = "Deviance Residuals vs Fitted (log scale)", 
       subtitle = "For incurred medical expenditure", 
       x = "Fitted (log)", y = "Deviance residual") + theme_ald

p_resid2_b <- ggplot(dat_visited_b, aes(sample = resid_dev)) +
  stat_qq(alpha = 0.2, size = 0.8, color = "darkorange") +
  stat_qq_line(color = "tomato", linewidth = 0.9) +
  labs(title = "QQ Plot of Deviance Residuals", 
       subtitle = "For incurred medical expenditure", 
       x = "Theoretical quantiles", y = "Sample quantiles") + theme_ald


# 5-5. Save figure ----
fig_b <- (p_age_prob_b | p_age_amt_b) /
          (p_age_ey_b  | p_cohort_b)  /
          (p_period_b  | (p_resid1_b | p_resid2_b))

ggsave(paste0("fig/", NAME, "-fitting_Carstensen.jpg"),
       fig_b, width = 16, height = 18, dpi = 300)
cat("\nFigure saved.\n")


# 6. (Optional) Comparison with Method A ----
# Run AFTER sourcing ald_simulation_04-fitting.R (loads m1, m2).

compare_methods <- function(m1_a, m2_a, m1_b, m2_b) {
  pal3 <- c("True" = "tomato",
            "Method A (mgcv GAM)"   = "steelblue",
            "Method B (Carstensen)" = "darkorange")

  ref_grid_a <- tibble(
    age = 50:79, birth_year = 1950,
    post_2012 = 0L, post_2014 = 0L, post_2016 = 0L, post_2018 = 0L
  )
  ref_grid_b <- age_grid   # built above

  p1_a <- predict(m1_a, newdata = ref_grid_a, type = "link", se.fit = TRUE)
  p1_b <- predict(m1_b, newdata = ref_grid_b, type = "link", se.fit = TRUE)
  p2_a <- predict(m2_a, newdata = ref_grid_a, type = "link", se.fit = TRUE)
  p2_b <- predict(m2_b, newdata = ref_grid_b, type = "link", se.fit = TRUE)

  cmp <- tibble(
    age       = 50:79,
    true_prob = plogis(true_age_logit(50:79) + true_cohort_effect(1950)),
    A_prob    = plogis(p1_a$fit),
    B_prob    = plogis(p1_b$fit),
    true_amt  = exp(true_age_log_amount(50:79) + true_cohort_effect(1950) * 0.5),
    A_amt     = exp(p2_a$fit),
    B_amt     = exp(p2_b$fit)
  )

  p_cmp_prob <- cmp %>%
    pivot_longer(c(true_prob, A_prob, B_prob),
                 names_to = "src", values_to = "prob") %>%
    mutate(src = recode(src,
      true_prob = "True",
      A_prob    = "Method A (mgcv GAM)",
      B_prob    = "Method B (Carstensen)"
    )) %>%
    ggplot(aes(x = age, y = prob, color = src, linetype = src)) +
    geom_line(linewidth = 1.2) +
    scale_color_manual(values = pal3) +
    scale_linetype_manual(values = c(
      "True" = "dashed",
      "Method A (mgcv GAM)"   = "solid",
      "Method B (Carstensen)" = "solid")) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Part 1: Visit Probability — Method A vs B",
         subtitle = "Ref: birth_year=1950",
         x = "Age", y = "Visit probability", color = NULL, linetype = NULL) +
    theme_ald

  p_cmp_amt <- cmp %>%
    pivot_longer(c(true_amt, A_amt, B_amt),
                 names_to = "src", values_to = "amt") %>%
    mutate(src = recode(src,
      true_amt = "True",
      A_amt    = "Method A (mgcv GAM)",
      B_amt    = "Method B (Carstensen)"
    )) %>%
    ggplot(aes(x = age, y = amt, color = src, linetype = src)) +
    geom_line(linewidth = 1.2) +
    scale_color_manual(values = pal3) +
    scale_linetype_manual(values = c(
      "True" = "dashed",
      "Method A (mgcv GAM)"   = "solid",
      "Method B (Carstensen)" = "solid")) +
    scale_y_continuous(labels = scales::comma) +
    labs(title = "Part 2: Conditional Expenditure — Method A vs B",
         subtitle = "Ref: birth_year=1950",
         x = "Age", y = "Mean incurred expenditure", color = NULL, linetype = NULL) +
    theme_ald

  fig_cmp <- p_cmp_prob | p_cmp_amt
  ggsave(paste0("fig/", NAME, "-fitting_compare_AB.jpg"),
         fig_cmp, width = 16, height = 7, dpi = 300)
  cat("Comparison figure saved.\n")
  invisible(fig_cmp)
}

# compare_methods(
#   m1_a = readRDS(paste0("output/", NAME, "-fitting_m1_a.rda")), 
#   m2_a = readRDS(paste0("output/", NAME, "-fitting_m2_a.rda")), 
#   m1_b = readRDS(paste0("output/", NAME, "-fitting_m1_b.rda")), 
#   m2_b = readRDS(paste0("output/", NAME, "-fitting_m2_b.rda"))
# )
