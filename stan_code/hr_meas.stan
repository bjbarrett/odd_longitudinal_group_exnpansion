data{
  // monkey data
  int N;
  vector[N] kde_shape;
  vector[N] kde_rate;
}

parameters{
  vector[1] v_mu;
  real<lower=0> k;
}

model{
//for monkeys
vector[N] lambda;
k ~ exponential( 1 );
v_mu[1] ~ normal( 1 , 0.8 );
for ( i in 1:N ) {
    lambda[i] = v_mu[1];
    lambda[i] = exp(lambda[i]);
}

hr_area_mean ~ gamma( lambda/k , 1/k );
}

