library(rstan)
pacman::p_load(bayesplot, ggmcmc, shinystan, tidybayes, patchwork)

source("utility/environments.R")
pkgbuild::has_rtools(TRUE)


# modeling ----
schools_dat <- list(
  J = 8,
  y = c(28,  8, -3,  7, -1,  1, 18, 12),
  sigma = c(15, 10, 16, 11,  9, 11, 10, 18)
)

fit <- stan(
  file = 'src/stan/model/school.stan', 
  data = schools_dat, 
  iter = 1000, 
  warmup = 1000 / 2, 
  chains = 4
)

fit.array <- as.array(fit)
posterior <- rstan::extract(fit, permuted = FALSE, inc_warmup = TRUE)  # array
fit.matrix <- as.matrix(fit)
print(colnames(fit.matrix))



# rstan basic functions ----
# https://cran.r-project.org/web/packages/rstan/vignettes/rstan.html
plot(fit, pars = c("mu", "tau")) + 
    theme_minimal(base_family = font.base)

traceplot(fit, inc_warmup = TRUE, pars = c("mu", "tau")) + 
    theme_minimal(base_family = font.base) + 
    theme(
        title = element_text(face = font.face, color = col.os, size = 9), 
        plot.subtitle = element_text(face = font.face, color = col.os, size = 7), 
        text = element_text(face = font.face, color = col.os, size = 10), 
        plot.caption = element_text(color = "gray30", size = 12),
        axis.title = element_text(face = font.face, color = col.os), 
        axis.text = element_text(face = font.face, color = col.os), 
        panel.grid.major = element_line(size = 0.25), 
        panel.grid.minor = element_blank(), 
        legend.position = c(0.9, 0.9), 
        legend.text = element_text(size = 6), 
        legend.key.size = unit(0.04, "npc")
    )

pairs(fit, pars = c("mu", "tau"))  # not ggplot class

sampler_params <- get_sampler_params(fit, inc_warmup = TRUE)  # list[matrix, matrix, ...]
do.call(rbind, sampler_params)  # matrix
summary(do.call(rbind, sampler_params), digits = 2)


# ggmcmc ----
# https://cran.r-project.org/web/packages/ggmcmc/vignettes/using_ggmcmc.html
fit.ggs <- ggs(fit)  # tidybayes like function
ggs_histogram(fit.ggs, family = "mu")  # ggplot class object
ggs_density(fit.ggs, family = "mu")
ggs_traceplot(fit.ggs, family = "mu")
ggs_running(fit.ggs, family = "mu")  # useful!
ggs_compare_partial(fit.ggs, family = "mu", partial = 0.1)  # useful!
ggs_autocorrelation(fit.ggs, family = "mu")  # useful!
ggs_crosscorrelation(fit.ggs) + theme(legend.position = "right")
# ggmcmc(fit.ggs, file = "output/stan_3_ggmcmc.pdf")  # not useful...


# bayesplot ----
color_scheme_set("mix-brightblue-pink")
mcmc_trace(posterior,  pars = c("mu", "tau"), n_warmup = 500,
                facet_args = list(nrow = 2, labeller = label_parsed)) + 
    theme_default(base_family = font.base) + 
    facet_text(size = 15) + 
    theme(
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 10), 
        legend.key.size = unit(0.1, "npc")
    )

color_scheme_set("brewer-Purples")
ggplot2::theme_update(legend.position = "right")
mcmc_combo(fit.array, pars = c("mu", "tau"), widths = c(1, 2))  # NOT ggplot class!

mcmc_rhat_hist(rhat(fit))

mcmc_acf(posterior, pars = c("mu", "tau"))
mcmc_acf_bar(posterior, pars = c("mu", "tau"))

color_scheme_set(scheme = "mix-pink-purple")
mcmc_dens(posterior, pars = c("mu", "tau"))
mcmc_dens_overlay(posterior, pars = c("mu", "tau"))
mcmc_areas(fit.matrix, pars = c("mu", "tau"), prob = 0.95) + 
    theme_default(base_family = font.base) + 
    labs(title = "Posterior distributions", 
         subtitle = "with medians and 95% intervals") + 
    theme(
        title = element_text(size = 15), 
        plot.subtitle = element_text(size = 12)
    )


# tidybayes ----
fit.tidy <- fit %>% spread_draws(mu, tau)
