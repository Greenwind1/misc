# Bayes Factor: simple demo
# Mar.2021, Maxwell
# ChangeLog:
# 

# 1. Library ----
# Data manipulation
library(tidyverse)
library(Lock5Data)

# Visualisation
library(ggplot2)

# Statiscis Analysis
library(rstan)
library(bayesplot)

# Custom Functions
source("./utility/helper_functions.R")

# 2. Dataset ----
data <- LightatNight4Weeks
view.table(data)

# 3. Naive EDA ----
data %>% ggplot(mapping = aes(x = Light, y = BMGain, fill = Light)) +
    geom_violin(width = 1, color = NA) +
    geom_boxplot(width = 0.1, color = "snow", alpha = 0.2) +
    scale_fill_brewer(palette = "Dark2") +
    theme_minimal() +
    labs(title = "Light vs BMGain") +
    theme(legend.position = "none", plot.title = element_text(size = 15))
ggsave("./fig/bayes_factor_01.png", dpi=300)

# 4. MCMC ----
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7')

data.list <- list(
    N = nrow(data),
    d1 = as.integer(data$Light == "LD"),  # light/dark
    d2 = as.integer(data$Light == "DM"),  # dim light at night
    d3 = as.integer(data$Light == "LL"),  # light/light
    Y = data$BMGain
)

stan.result <- stan(
    file = "./src/stan/model/BF_model1.stan",
    data = data.list,
    seed = 2021,
    chains = 4,
    iter = 2000,
    warmup = 1000,
    thin = 1
)

# 5. Check results ----
print(stan.result, digits_summary = 4)

mcmc_dens(
    stan.result,
    pars = c("mu[1]", "mu[2]", "mu[3]", "sig2", "f1", "f2")
)
ggsave("./fig/bayes_factor_02.png", dpi=300)

mcmc_trace(
    stan.result, 
    pars = c("mu[1]", "mu[2]", "mu[3]", "sig2", "f1", "f2")
)
ggsave("./fig/bayes_factor_03.png", dpi=300)

mcmc_areas(
    stan.result, 
    pars = c("mu[1]", "mu[2]", "mu[3]"),
    prob_outer = 0.95
)

mcmc_acf_bar(
    stan.result, 
    pars = c("mu[1]", "mu[2]", "mu[3]", "f1", "f2")
)

# 6. Compute Bayes Factor ----
mcmc.sample <- rstan::extract(stan.result)
f1 <- mean(mcmc.sample[["f1"]])
f2 <- mean(mcmc.sample[["f2"]])

BF1u <- f1 / (1 / 6)
BF2u <- f2 / (1 / 3)

cat("BF1u:", BF1u, "\n")  # 5.601
cat("BF2u:", BF2u)  # 2.823
