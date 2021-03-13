data {
  int N;
  int d1[N];
  int d2[N];
  int d3[N];
  real Y[N];
}

parameters {
  real mu[3];
  real sig2;
}

transformed parameters {
}

model {
  for (i in 1:3) {
    mu[i] ~ normal(0, 20);
  }
  sig2 ~ lognormal(0, 20);

  for (n in 1:N) {
    Y[n] ~ normal(mu[1] * d1[n] + mu[2] * d2[n] + mu[3] * d3[n], sig2);
  }
}

generated quantities {
  real f1;
  real f2;
  f1 = int_step(mu[2] - mu[1]) * int_step(mu[3] - mu[2]);
  f2 = int_step(mu[2] - mu[1]) * int_step(mu[3] - mu[1]);
}
