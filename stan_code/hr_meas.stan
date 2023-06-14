data{
  // monkey data
  int N;
  vector[N] kde_shape;
  vector[N] kde_rate;
}

parameters{
  vector<lower=0>[N] hr_area_true; //restrict lower range most likely
}

model{
//for monkeys
for ( i in 1:N ) {
    hr_area_true[i] ~ gamma( kde_shape[i] , kde_rate[i]);
}

}

