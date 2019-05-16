library(rstan)
library(tweedie)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# E[y] = mu
# V[y] = phi * mu^theta
stanmodel <- stan_model(file = './codes/model/model1.stan')

mu <- 1
power <- 1.3
phi <- 1
lambda.p <- mu ^ (2 - power) / (phi * (2 - power))
cat(lambda.p)
M <- 30
cat(ppois(30, lambda = lambda.p, lower.tail = TRUE))

N1 <- 1000
M1 <- 30
Y1 <- rtweedie(N1, power = 1.3, mu = 1, phi = 1)
data1 <- list(N = N1, M = M1, Y = Y1)
fit1 <- sampling(stanmodel, data = data1)
print(fit1)
traceplot(fit1, inc_warmup = TRUE)

Y2 <- rtweedie(N2, power = 1.01, mu = 3, phi = 1)
data2 <- list(N = N1, M = M1, Y = Y2)
fit2 <- sampling(stanmodel, data = data2)
traceplot(fit2, inc_warmup = TRUE)


stan.model2 <- stan_model(file = './codes/model/model2.stan',
                          model_name = 'agg')
data.agg <- list(N = N1, M = M1, Y = Y1 + Y2)
fit <- sampling(stan.model2, data = data.agg)
print(fit)
traceplot(fit, inc_warmup = TRUE)
