data{
  // mei data
  vector[288] mei;
  array[288] int year_index_mei;
  int N_years;

  // monkey data
  int N;
  vector[N] kde_shape; //shape of each home range estimated by ctmm
  vector[N] kde_rate; //rate of each home range estimated by ctmm
  int N_groups;
  array[N] int year_index;
  array[N] int group_index;
  vector[N] group_size;
}

parameters{
   //mei parameters
  real<lower=0> sigma;
  vector[N_years] am;
  vector[N_years] am_pred;

  //monkey parameters
  matrix[3,N_groups] z_g;
  vector[3] v_mu;
  cholesky_factor_corr[3] L_Rho_g;
  vector<lower=0>[3] sigma_g;
  real<lower=0> k;
  vector<lower=0>[N] hr_area_true; //estimated shape and range
}

transformed parameters{
  matrix[11,3] v;
  v = (diag_pre_multiply(sigma_g, L_Rho_g) * z_g)';
}

model{
   // for mei
  vector[N_years*12] mu;
  am ~ normal( 0 , 2 );
  sigma ~ exponential( 1 );

  for ( i in 1:N_years*12 ) {
    mu[i] = am[year_index_mei[i]];
  }

mei ~ normal( mu , sigma );

for (i in 1:N_years) {
  am_pred[i] ~ normal( am[i] , sigma ) ;
}

  vector[N] lambda; 
  vector[N] hr_area_obs; //storage value for posterior of home ranges
  k ~ exponential(1); // prior on scale
  sigma_g ~ exponential( 1 );
  L_Rho_g ~ lkj_corr_cholesky( 3 );
  v_mu[1] ~ normal( 1 , 0.8 );
  v_mu[2] ~ normal( 0 , 1 );
  v_mu[3] ~ normal( 0 , 1 );
  to_vector( z_g ) ~ normal( 0 , 1 );
  
  for ( i in 1:N ) {
      hr_area_true[i] ~ gamma( kde_shape[i] , kde_rate[i]); // estimate posteriors of home range for each observation from akde shape and scale in ctmm
  }
  
  hr_area_obs=hr_area_true; // store posterior as temp variable to make outcomes
  
    for ( i in 1:N ) {
      lambda[i] = v_mu[1] + v[group_index[i], 1] + (v_mu[2] + v[group_index[i], 2]) * am_pred[year_index[i]] + (v_mu[3] + v[group_index[i], 3]) * group_size[i];
      lambda[i] = exp(lambda[i]);
  }
  hr_area_obs ~ gamma( lambda/k , 1/k );
}

generated quantities{
  matrix[3,3] Rho_g;
  Rho_g = multiply_lower_tri_self_transpose(L_Rho_g);
}
