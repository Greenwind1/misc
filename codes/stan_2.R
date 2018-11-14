library(rstan)

model <- stan_demo()


schools_dat <- list(
  J = 8,
  y = c(28,  8, -3,  7, -1,  1, 18, 12),
  sigma = c(15, 10, 16, 11,  9, 11, 10, 18)
)
fit <- stan(
  file = './codes/model/school.stan',
  data = schools_dat,
  iter = 1000,
  chains = 4
)
plot(fit)
traceplot(fit, inc_warmup = TRUE)
