# =============================================================================
#  ALD (Accelerated Longitudinal Design) Simulation
#  Hurdle (2-part) model using mgcv package
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
#  04: Increase noises.
#  05: COVID-19 shock that effects on frequency only.
#  06: More realistic COVID-19 shock and cohort effect.
# =============================================================================

library(mgcv)
library(tidyverse)
library(patchwork)
 
set.seed(2026)
NAME <- "ald_simulation_06"

# 1. Study design parameters ----
OBS_START <- 2010        # Start of shared observation window
OBS_END <- 2019        # End of shared observation window
OBS_YEARS <- OBS_START:OBS_END
NOISE_RATIO <- .5
 
# Cohorts defined by birth year.
# The same observation window covers different age bands per cohort:
#   born 1940 -> age 70-79 during 2010-2019
#   born 1945 -> age 65-74 during 2010-2019
#   born 1950 -> age 60-69 during 2010-2019
#   born 1955 -> age 55-64 during 2010-2019
#   born 1960 -> age 50-59 during 2010-2019
COHORT_BIRTH_YEARS   <- c(1940, 1945, 1950, 1955, 1960)
N_PERSONS_PER_COHORT <- 600
 
# Fee revision years falling within the observation window
REVISION_YEARS <- c(2012, 2013, 2014, 2015, 2016, 2017, 2018)
 
cat("=== ALD Study Design ===\n")
cat(sprintf("Observation window: %d - %d (%d years)\n",
            OBS_START, OBS_END, length(OBS_YEARS)))
cat("\nAge range covered by each cohort:\n")
for (by in COHORT_BIRTH_YEARS) {
  cat(sprintf("  Birth year %d: age %d - %d\n", by, OBS_START - by, OBS_END - by))
}


# 2. True parameters ----
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
  x <- birth_year - 1940  # center at 1950 => 1940
  - 0.075 * (x / 10)^2 + 0.2
}
# true_cohort_effect(1940:1960)

# True period effect: step jumps at each fee revision year
true_theta <- c(
  `2012` = 0, 
  `2013` = 0, 
  `2014` = 0, 
  `2015` = -0.4, 
  `2016` = +0.2, 
  `2017` = +0.1, 
  `2018` = +0.1
)


# 3. Data generation ----
generate_ald_data <- function() {
 
  df_list <- list()
  pid     <- 0L
 
  for (birth_year in COHORT_BIRTH_YEARS) {
 
    # Individual-level random effects (frailty)
    re <- rnorm(N_PERSONS_PER_COHORT, mean = 0, sd = 0.35)
 
    for (p_idx in seq_len(N_PERSONS_PER_COHORT)) {
      pid <- pid + 1L
 
      for (yr in OBS_YEARS) {
        # yr <- 2019
        age <- yr - birth_year
 
        # Cumulative period jump up to observation year yr
        revisions_so_far   <- REVISION_YEARS[REVISION_YEARS <= yr]
        period_jump_prob   <- sum(true_theta[as.character(revisions_so_far)])
        # period_jump_amount <- sum(true_theta[as.character(revisions_so_far)])
 
        # --- Hurdle Part 1: visit probability ---
        logit_pi <- true_age_logit(age) +
                    true_cohort_effect(birth_year) +
                    period_jump_prob +
                    re[p_idx] * 0.4 +
                    rnorm(1, 0, 0.15) * NOISE_RATIO 
        pi      <- plogis(logit_pi)
        visited <- rbinom(1, 1, pi)
 
        # --- Hurdle Part 2: expenditure amount (Gamma, visitors only) ---
        medical_cost <- 0
        if (visited == 1L) {
          log_mu <- true_age_log_amount(age) +
                    true_cohort_effect(birth_year) * 0.5 +
                    # period_jump_amount +
                    re[p_idx] +
                    rnorm(1, 0, 0.25) * NOISE_RATIO
          mu <- exp(log_mu)
          medical_cost <- rgamma(1, shape = 2, rate = 2 / mu)
        }
 
        df_list[[length(df_list) + 1L]] <- list(
          person_id    = pid,
          birth_year   = birth_year,
          obs_year     = yr,
          age          = age,
          visited      = visited,
          medical_cost = medical_cost
        )
      }
    }
  }
 
  df <- bind_rows(df_list)
 
  # Add post-revision step dummy variables
  for (ry in REVISION_YEARS) {
    df[[paste0("post_", ry)]] <- as.integer(df$obs_year >= ry)
  }
 
  df
}

cat("Generating dataset...\n")
dat <- generate_ald_data()
cat(sprintf("Num of subjects: %d, Zero expenditure ratio: %.1f%%\n",
            nrow(dat), 100 * mean(dat$medical_cost == 0)))

write_csv(dat, file = paste0("input/", NAME, "-data.csv"))
