data {
  int<lower=1> N; 
  int Y[N];
  int<lower = 1> K;
  matrix[N, K] X; 
  int<lower = 1> M;
  matrix[M,1] new_X;
}

transformed data {
 int Kc = K - 1;
 matrix[N, Kc] Xc;
 vector[Kc] means_X;
 for (i in 2:K) {
    means_X[i - 1] = mean(X[, i]);
    Xc[, i - 1] = X[, i] - means_X[i - 1];
  }
 }

parameters {
  vector[Kc] beta;   
  real alpha;   
  real<lower=0> phi;  
}

model {
  // Priors
  target += normal_lpdf(alpha | 5, 100);
  //target += student_t_lpdf(alpha | 3, 5, 10);
  
  target += normal_lpdf(beta | 0, 10);
  
  target += cauchy_lpdf(phi | 0, 2);
  //target += gamma_lpdf(phi | 0.01, 0.01);

  
  // Likelihood
  target += neg_binomial_2_log_glm_lpmf(Y | Xc, alpha, beta, phi);
}



generated quantities {
 real b_alpha = alpha - dot_product(means_X, beta);
 

  int new_y[M];
  
  //int y_rep[N]; 
  //for (n in 1:N) {
  //  y_rep[n] = neg_binomial_2_log_rng(b_alpha + X[n, 2:] * beta, phi);
  //}

  for (n in 1:M) {
    new_y[n] = neg_binomial_2_log_rng(b_alpha + new_X[n, ] * beta, phi);
  }
}

