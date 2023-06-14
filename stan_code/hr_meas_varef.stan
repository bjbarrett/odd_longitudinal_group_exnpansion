data{
  // monkey data
  int N;
  vector[N] kde_shape; //shape of each home range estimated by ctmm
  vector[N] kde_rate; //rate of each home range estimated by ctmm
}

parameters{
  vector<lower=0>[N] hr_area_true; //estimated shape and range
  vector[1] v_mu; // parameter vector
  real<lower=0> k; // scale parameter for big gamma to estimate
}

model{
  vector[N] lambda; 
  vector[N] hr_area_obs; //storage value for posterior of home ranges
  v_mu[1] ~ normal( 1 , 0.8 ); // prior on intercept
  k ~ exponential(1); // prior on scale
  
  for ( i in 1:N ) {
      hr_area_true[i] ~ gamma( kde_shape[i] , kde_rate[i]); // estimate posteriors of home range for each observation
  }
  
  hr_area_obs=hr_area_true; // store posterior as something else to make outcomes
  
    for ( i in 1:N ) {
      lambda[i] = v_mu[1];
      lambda[i] = exp(lambda[i]);
  }
  hr_area_obs ~ gamma( lambda/k , 1/k );
}

