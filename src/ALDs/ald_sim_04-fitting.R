# =============================================================================
#  ALD Simulation: Hurdle (2-part) model using mgcv package
# 
#  Reference:
#  Galbraith, Sally, Jack Bowden, and Adrian Mander. 2017. 
#  “Accelerated Longitudinal Designs: 
#  An Overview of Modelling, Power, Costs and Handling Missing Data.” 
#  Statistical Methods in Medical Research 26(1): 374–98. 
#  doi:10.1177/0962280214547150.
# 
#  Change logs:
#  02: Shared observation window across all cohorts;
#      cohorts differ in birth year, producing staggered age bands.
#  03: Change the simple cohort effect to the bathtub-shaped cohort effect.
#  04: Increase Noises
# =============================================================================

library(mgcv)
library(tidyverse)
library(patchwork)
library(extrafont)  # fonttable(); "Candara"
source("utility/environments.R") 
set.seed(2026)

NAME <- "ald_simulation_04"


# 1. Load data ----
dat <- read_csv(paste0("input/", NAME, "-data.csv"))


# 2. Study design parameters ----
# Fee revision years falling within the observation window
REVISION_YEARS <- c(2012, 2014, 2016, 2018)
COHORT_BIRTH_YEARS <- c(1940, 1945, 1950, 1955, 1960)
jump_vars <- paste0("post_", REVISION_YEARS)
jump_formula_str <- paste(jump_vars, collapse = " + ")

# True age effect on visit probability (logit scale)
# Gradual increase from age 50, steeper increase from age 70
true_age_logit <- function(age) {
  -2.5 + 0.04 * pmax(age - 50, 0) + 0.06 * pmax(age - 70, 0)
}
 
# True age effect on expenditure amount (log scale)
true_age_log_amount <- function(age) {
  8.5 + 0.025 * pmax(age - 50, 0) + 0.05  * pmax(age - 70, 0)
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


# 3. Fitting ----
## Part 1: Frequency (logistic) ----
formula_part1 <- as.formula(
  "visited ~ s(age, bs='cr', k=10) + s(birth_year, bs='cr', k=5)"
  # "visited ~ s(age, bs='cr', k=10) + s(birth_year, bs='cr', k=10)"  # k knots <= 5
)
print(formula_part1)
cat("\nFitting Part 1 (visit probability)...\n")
m1 <- gam(formula_part1, data = dat,
  family = binomial(link = "logit"), method = "REML")
saveRDS(m1, file = paste0("output/", NAME, "-fitting_m1_a.rda"))

## Part 2: expenditure amount among visitors (Gamma) ----
formula_part2 <- as.formula(paste0(
  "medical_cost ~ s(age, bs='cr', k=10) + s(birth_year, bs='cr', k=5) + ",
  jump_formula_str
))
print(formula_part2)
cat("Fitting Part 2 (expenditure amount)...\n")
m2 <- gam(formula_part2, data = filter(dat, visited == 1),
          family = Gamma(link = "log"), method = "REML")
saveRDS(m1, file = paste0("output/", NAME, "-fitting_m2_a.rda"))
 
## Summary ----
cat(sprintf("\nPart 1 AIC: %.1f\n", AIC(m1)))
cat(sprintf("Part 2 AIC: %.1f\n",  AIC(m2)))
print(summary(m1))
print(summary(m2))


# 4. Check result ----
font.base <- "Times New Roman"
theme_ald <- theme_minimal(base_family = font.base, base_size = 13) + 
  theme(
    legend.position  = "bottom",
    strip.background = element_rect(fill = "gray95"),
    panel.grid.minor = element_blank()
  )

# palette
cohort_colors <- setNames(
  RColorBrewer::brewer.pal(5, "Set2"),
  as.character(sort(unique(dat$birth_year)))
)

# Color palette: true vs fitted
pal <- c("True" = "tomato", "Fitted" = "steelblue")


# 4-1. Age Effect ----
# Prediction grid: fix birth_year at reference cohort (1950), post-all revisions
age_grid <- tibble(
  age        = 50:79,
  birth_year = 1950,
  post_2012  = 0L,
  post_2014  = 0L,
  post_2016  = 0L,
  post_2018  = 0L
)

## 4-1-1: Frequency ----
pred1 <- predict(m1, newdata = age_grid, type = "link", se.fit = TRUE)
age_grid <- age_grid %>% 
  mutate(
    fit_logit = pred1$fit,
    se_logit  = pred1$se.fit,
    fit_prob  = plogis(fit_logit),
    lo_prob   = plogis(fit_logit - 1.96 * se_logit),
    hi_prob   = plogis(fit_logit + 1.96 * se_logit),
    # True value: age effect + cohort effect for birth_year=1950
    true_prob = plogis(true_age_logit(age) + true_cohort_effect(1950))
  )
 
p_age_prob <- ggplot(age_grid, aes(x = age)) +
  geom_ribbon(aes(ymin = lo_prob, ymax = hi_prob),
              fill = "steelblue", alpha = 0.2) +
  geom_line(aes(y = fit_prob,  color = "Fitted"), linewidth = 1.2) +
  geom_line(aes(y = true_prob, color = "True"),   linewidth = 1.2, linetype = "dashed") +
  scale_color_manual(values = pal) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title    = "Part 1: Frequency",
    subtitle = "Reference cohort: birth year 1950 | Ribbon: 95% CI",
    x = "Age", y = "Visit probability", color = NULL
  ) +
  theme_ald
p_age_prob

## 4-1-2: conditional expenditure ----
pred2    <- predict(m2, newdata = age_grid, type = "link", se.fit = TRUE)
age_grid <- age_grid %>%
  mutate(
    fit_log  = pred2$fit,
    se_log   = pred2$se.fit,
    fit_amt  = exp(fit_log),
    lo_amt   = exp(fit_log - 1.96 * se_log),
    hi_amt   = exp(fit_log + 1.96 * se_log),
    true_amt = exp(true_age_log_amount(age) + true_cohort_effect(1950) * 0.5)
  )

p_age_amt <- ggplot(age_grid, aes(x = age)) +
  geom_ribbon(aes(ymin = lo_amt, ymax = hi_amt),
              fill = "steelblue", alpha = 0.2) +
  geom_line(aes(y = fit_amt,  color = "Fitted"), linewidth = 1.2) +
  geom_line(aes(y = true_amt, color = "True"),   linewidth = 1.2, linetype = "dashed") +
  scale_color_manual(values = pal) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "Part 2: Incurred medical expenditure",
    subtitle = "Reference cohort: birth year 1950 | Ribbon: 95% CI",
    x = "Age", y = "Mean inccured medical expenditure", color = NULL
  ) +
  theme_ald
p_age_amt

## 4-1-3. Combined: E[Y] = pi * mu ----
age_grid <- age_grid %>%
  mutate(
    fit_ey  = fit_prob * fit_amt,
    true_ey = true_prob * true_amt
  )
 
p_age_ey <- ggplot(age_grid, aes(x = age)) +
  geom_line(aes(y = fit_ey,  color = "Fitted"), linewidth = 1.2) +
  geom_line(aes(y = true_ey, color = "True"),   linewidth = 1.2, linetype = "dashed") +
  scale_color_manual(values = pal) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "Frequency * Incurred medical expenditure",
    subtitle = "Reference cohort: birth year 1950",
    x = "Age", y = "Expected medical expenditure", color = NULL
  ) +
  theme_ald
p_age_ey


# 4-2. Cohort Effect ----
# Prediction grid: fix age at 65 (mid-range), post-all revisions
cohort_grid <- tibble(
  birth_year = COHORT_BIRTH_YEARS,
  age = 65  # fixed age for cohort comparison
)

pred_coh <- predict(m1, newdata = cohort_grid, type = "link", se.fit = TRUE)
cohort_grid <- cohort_grid %>%
  mutate(
    fit_logit  = pred_coh$fit,
    se_logit   = pred_coh$se.fit,
    # Center both fitted and true for interpretability
    fit_cen    = fit_logit  - mean(fit_logit),
    true_cen   = (true_age_logit(65) + true_cohort_effect(birth_year)) -
                 mean(true_age_logit(65) + true_cohort_effect(COHORT_BIRTH_YEARS)),
    lo_cen     = fit_cen - 1.96 * se_logit,
    hi_cen     = fit_cen + 1.96 * se_logit
  )
 
p_cohort <- ggplot(cohort_grid, aes(x = birth_year)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray60") +
  geom_ribbon(aes(ymin = lo_cen, ymax = hi_cen),
              fill = "steelblue", alpha = 0.2) +
  geom_line(aes(y = fit_cen,  color = "Fitted"), linewidth = 1.2) +
  geom_point(aes(y = fit_cen, color = "Fitted"), size = 3) +
  geom_line(aes(y = true_cen, color = "True"),   linewidth = 1.2, linetype = "dashed") +
  geom_point(aes(y = true_cen, color = "True"),  size = 3, shape = 17) +
  scale_color_manual(values = pal) +
  scale_x_continuous(breaks = COHORT_BIRTH_YEARS) +
  labs(
    title    = "Cohort Effect (centered, logit scale)",
    subtitle = "Fixed age = 65 | Ribbon: 95% CI",
    x = "Birth year", y = "Cohort effect (logit, centered)", color = NULL
  ) +
  theme_ald
p_cohort


# 4-3. Period Effect (Revision Jump Coefficients) ----
# Compare estimated theta_r vs true_theta for Part 2 only
coef_m2 <- coef(m2)
vcov_m2 <- vcov(m2)
se_m2   <- sqrt(diag(vcov_m2))
 
jump_names <- paste0("post_", REVISION_YEARS)
 
jump_df <- tibble(
  revision_year = REVISION_YEARS,
  true_val      = as.numeric(true_theta),
  estimate      = coef_m2[jump_names],
  se            = se_m2[jump_names]
) %>%
  mutate(
    lo       = estimate - 1.96 * se,
    hi       = estimate + 1.96 * se,
    covered  = true_val >= lo & true_val <= hi,   # 95% CI covers true value?
    bias     = estimate - true_val
  )
 
p_jump <- ggplot(jump_df, aes(x = factor(revision_year))) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray60") +
  geom_pointrange(
    aes(y = estimate, ymin = lo, ymax = hi, color = "Fitted"),
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
    subtitle = "Triangle: true value | Point + range: estimate +/- 1.96 SE",
    x = "Revision year", y = "Jump coefficient (log)", color = NULL
  ) + 
  theme_ald
p_jump


# 4-4. Residual Check for Part 2 ----
dat_visited <- dat %>% filter(visited == 1) %>%
  mutate(
    fitted_log = predict(m2, type = "link"),
    resid_dev  = residuals(m2, type = "deviance"),
    resid_pear = residuals(m2, type = "pearson")
  )

# Deviance residuals vs fitted
p_resid1 <- ggplot(dat_visited, aes(x = fitted_log, y = resid_dev)) +
  geom_point(alpha = 0.15, size = 0.8, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "tomato") +
  geom_smooth(method = "loess", se = FALSE, color = "tomato", linewidth = 0.8) +
  labs(
    title = "Part 2: Deviance Residuals vs Fitted (log scale)",
    x = "Fitted (log)", y = "Deviance residual"
  ) +
  theme_ald
 
# QQ plot of deviance residuals
p_resid2 <- ggplot(dat_visited, aes(sample = resid_dev)) +
  stat_qq(alpha = 0.2, size = 0.8, color = "steelblue") +
  stat_qq_line(color = "tomato", linewidth = 0.9) +
  labs(
    title = "Part 2: QQ Plot of Deviance Residuals",
    x = "Theoretical quantiles", y = "Sample quantiles"
  ) +
  theme_ald
p_resid2


# 4-5. Save ----
fig_main <- (p_age_prob | p_age_amt) /
            (p_age_ey  | p_cohort)  /
            (p_jump    | (p_resid1 | p_resid2))

ggsave(paste0("fig/", NAME, "-fitting.jpg"), 
  fig_main, width = 16, height = 18, dpi = 300)
