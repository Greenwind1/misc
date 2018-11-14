data {
  int N;
  int M;
  real<lower=0> Y[N];
}

parameters {
  real<lower=0> mu;
  real<lower=0> phi;
  real<lower=1, upper=2> theta;
}

transformed parameters {
  real lambda = 1/phi*mu^(2-theta)/(2-theta);
  real alpha = (2-theta)/(theta-1);
  real beta = 1/phi*mu^(1-theta)/(theta-1);
}

model {
  mu ~ cauchy(0, 5);
  phi ~ cauchy(0, 5);
  
  for (n in 1:N) {
    if (Y[n] == 0) {
      target += -lambda;
    } else {
      vector[M] ps;
      for (m in 1:M)
        ps[m] = poisson_lpmf(m | lambda) + gamma_lpdf(Y[n] | m*alpha, beta);
      target += log_sum_exp(ps);
    }
  }
}
