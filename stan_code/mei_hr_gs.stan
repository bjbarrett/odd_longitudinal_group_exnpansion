data{
  // mei data
  vector[288] mei;
  array[288] int year_index_mei;
  int N_years;

  // monkey data
  int N;
  int N_groups;
  array[N] int year_index;
  vector[N] group_size;
  vector[N] hr_area_mean;
  array[N] int group_index;
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

//for monkeys
vector[N] lambda;
k ~ exponential( 1 );
sigma_g ~ exponential( 1 );
L_Rho_g ~ lkj_corr_cholesky( 2 );
v_mu[1] ~ normal( 1 , 0.8 );
v_mu[2] ~ normal( 0 , 1 );
v_mu[3] ~ normal( 0 , 1 );
to_vector( z_g ) ~ normal( 0 , 1 );
for ( i in 1:N ) {
    lambda[i] = v_mu[1] + v[group_index[i], 1] + (v_mu[2] + v[group_index[i], 2]) * am_pred[year_index[i]] + (v_mu[3] + v[group_index[i], 3]) * group_size[i];
    lambda[i] = exp(lambda[i]);
}

hr_area_mean ~ gamma( lambda/k , 1/k );

}
generated quantities{
  matrix[3,3] Rho_g;
  Rho_g = multiply_lower_tri_self_transpose(L_Rho_g);
}
