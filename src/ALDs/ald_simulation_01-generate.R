# =============================================================================
# ALD (Accelerated Longitudinal Design) Simulation
# Hurdle (2-part) model using mgcv package
# Version 01: proto type, data generation based on Hurdle model
# 
# Reference:
# Galbraith, Sally, Jack Bowden, and Adrian Mander. 2017. 
# “Accelerated Longitudinal Designs: 
# An Overview of Modelling, Power, Costs and Handling Missing Data.” 
# Statistical Methods in Medical Research 26(1): 374–98. 
# doi:10.1177/0962280214547150.
# =============================================================================
 
library(mgcv)
library(tidyverse)
library(patchwork)
 
set.seed(2026)

# -----------------------------------------------------------------------------
# 1. Env parameters
# -----------------------------------------------------------------------------
 
N_PERSONS   <- 3000       # Num of subjects
OBS_YEARS   <- 10         # Observation period
COHORT_GAP  <- 5          # Age interval for cohorts
N_COHORTS   <- 5          # Num of cohorts（birth year：1950, 1955, 1960, 1965, 1970）
BASE_YEAR   <- 2010       # Observation start year
 
# Revision year for medical fee
REVISION_YEARS <- c(2012, 2014, 2016, 2018, 2020)


# -----------------------------------------------------------------------------
# 2. True parameters
# -----------------------------------------------------------------------------
 
# --- Age effect: Trapezoid-shaped (convex downward) ---
true_age_effect <- function(age) {
  # Binary frequency (logit-scale)
  logit_pi <- -3.0 +
    0.05  * pmax(age - 40, 0) +          # mild increase after 40yrs
    0.08  * pmax(age - 65, 0) +          # extensive increase after 65yrs
   - 0.002 * pmax(age - 40, 0)^2 * 0.02  # non-linear curve
  logit_pi
}
# plot(20:90, true_age_effect(20:90))

true_age_effect_amount <- function(age) {
  # Incurred expenditure (log-scale)
  log_mu <- 8.0 +
    0.02  * pmax(age - 40, 0) +
    0.06  * pmax(age - 65, 0)
  log_mu
}
# plot(20:90, true_age_effect_amount(20:90))

# --- Cohort effect ---
true_cohort_effect <- function(birth_year) {
  # The younger, the better helath.
  - 0.05 * (birth_year - 1960)
}

# --- Jump effect due to revision of medical fee ---
true_theta <- c(
  `2012` = -0.08, 
  `2014` =  0.05, 
  `2016` = -0.06, 
  `2018` =  0.04, 
  `2020` =  0.03
)

# -----------------------------------------------------------------------------
# 3. Data generation
# -----------------------------------------------------------------------------
 
generate_ald_data <- function() {
 
  # Subject allocation to each cohort
  cohort_birth_years <- 1950 + (0:(N_COHORTS - 1)) * COHORT_GAP
  persons_per_cohort <- N_PERSONS %/% N_COHORTS
 
  df_list <- list()
 
  for (i in seq_along(cohort_birth_years)) {
    # i <- 1
    birth_year <- cohort_birth_years[i]
 
    # Observation period of cohort i
    obs_start <- BASE_YEAR + (i - 1) * COHORT_GAP
    obs_end   <- obs_start + OBS_YEARS - 1
    obs_years <- obs_start:obs_end
 
    # Subject ID
    person_ids <- ((i - 1) * persons_per_cohort + 1):(i * persons_per_cohort)
 
    # Random effect for individual level (frailty)
    re <- rnorm(persons_per_cohort, mean = 0, sd = 0.4)
 
    # Generate panel data for each individual and each year
    for (pid_idx in seq_along(person_ids)) {
      # pid_idx <- 1
      pid <- person_ids[pid_idx]
 
      for (yr in obs_years) {
        # yr <- obs_years[1]
        age <- yr - birth_year
 
        # --- Cumulative jump for period effect ---
        period_jump <- 
          sum(true_theta[as.character(REVISION_YEARS[REVISION_YEARS <= yr])])
 
        # --- Frequency  ---
        logit_pi <- true_age_effect(age) +
                    true_cohort_effect(birth_year) +
                    period_jump * 0.5 +     # slight effect by revision
                    re[pid_idx] * 0.5 +
                    rnorm(1, 0, 0.2)
        pi <- plogis(logit_pi)
        visited <- rbinom(1, 1, pi)
 
        # --- Incurred expenditure ---
        if (visited == 1) {
          log_mu <- true_age_effect_amount(age) +
                    true_cohort_effect(birth_year) * 0.5 +
                    period_jump +
                    re[pid_idx] +
                    rnorm(1, 0, 0.3)
          mu <- exp(log_mu)
          # Gamma distribution (shape = 2)
          medical_cost <- rgamma(1, shape = 2, rate = 2 / mu)
        } else {
          medical_cost <- 0
        }
 
        df_list[[length(df_list) + 1]] <- data.frame(
          person_id   = pid,
          birth_year  = birth_year,
          obs_year    = yr,
          age         = age,
          visited     = visited,
          medical_cost = medical_cost
        )
      }
    }
  }
 
  df <- bind_rows(df_list)
 
  # Dummy variable for revision
  for (ry in REVISION_YEARS) {
    df[[paste0("post_", ry)]] <- as.integer(df$obs_year >= ry)
  }
 
  df
}
 
cat("Generating dataset...\n")
dat <- generate_ald_data()
cat(sprintf("Num of subjects: %d, Zero expenditure ratio: %.1f%%\n",
            nrow(dat), 100 * mean(dat$medical_cost == 0)))

write_csv(dat, file = "input/ald_simulation_01-data.csv")
