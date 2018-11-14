data {
  int N;
  int M;
  real<lower=0> Y[N];
}

parameters {
  real<lower=0> mu1;
  real<lower=0> phi1;
  real<lower=1, upper=2> theta1;
  real<lower=0> mu2;
  real<lower=0> phi2;
  real<lower=1, upper=2> theta2;  
}

transformed parameters {
  real lambda1 = 1 / phi1 * mu1^(2 - theta1)/(2 - theta1);
  real alpha1 = (2 - theta1)/(theta1 - 1);
  real beta1 = 1 / phi1 * mu1^(1 - theta1)/(theta1 - 1);
  real lambda2 = 1 / phi2 * mu2^(2 - theta2)/(2 - theta2);
  real alpha2 = (2 - theta2)/(theta2 - 1);
  real beta2 = 1 / phi2 * mu2^(1 - theta2)/(theta2 - 1);  
}

model {
  mu1 ~ cauchy(0, 5);
  phi1 ~ cauchy(0, 5);
  mu2 ~ cauchy(0, 5);
  phi2 ~ cauchy(0, 5);
  
  for (n in 1:N) {
    if (Y[n] == 0) {
      target += log_sum_exp(-lambda1, -lambda2);
    } else {
      vector[M] ps;
      vector[M] ps1;
      vector[M] ps2;
      for (m in 1:M)
        ps1[m] = poisson_lpmf(m | lambda1) + gamma_lpdf(Y[n] | m * alpha1, beta1);
      for (m in 1:M)
        ps2[m] = poisson_lpmf(m | lambda2) + gamma_lpdf(Y[n] | m * alpha2, beta2);
      for (m in 1:M)
        ps[m] = ps1[m] + ps2[m];        
      target += log_sum_exp(ps);
    }
  }
}
