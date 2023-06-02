data{
  // mei data
    vector[288] mei;
    array[288] int year_index_mei;
    int N_years ;
  // monkey data
}
parameters{
  //mei parameters
     real<lower=0> sigma;
     vector[N_years] am;
     vector[24] am_pred;
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
}