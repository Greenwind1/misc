data {
  int N;
  int d1[N];
  int d2[N];
  int d3[N];
  real Y[N];
}

parameters {
  real mu[3];
  real invsig2;
}

transformed parameters {
  real mu_n[N];
  for (n in 1:N) {
    mu_n[n] = mu[1] * d1[n] + mu[2] * d2[n] + mu[3] * d3[n];
  }
}

model {
  for (i in 1:3) {
    mu[i] ~ normal(0, 0.001);
  }
  invsig2 ~ inv_gamma(0.01, 0.01);

  for (n in 1:N) {
    Y[n] ~ normal(mu_n[n], invsig2);
  }
}

generated quantities {
  real f1;
  real mu32;
  f1 = int_step(mu[2] - mu[1]) * int_step(mu[3] - mu[2]);
}
