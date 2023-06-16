beta_hr <- 0.002 #effect of homerange on future pop growth rate
beta_gs <- 0.001 #effect of group size on future home range

timesteps <-30 #number of timesteps
GS_init <- 15 #initial group size
HRA_init <- 1.8 # initial home range size
HRA <- GS <- rep(NA,timesteps) #storage vector
HRA[1] <- HRA_init
GS[1] <- GS_init
lambda_mean <- .02 # pop groth rate
lambda_var <- .1 # variance of pop growth rate, make positive to add stochasticity
hist(rnorm(1e5,lambda_mean,lambda_var))
gamma_mean <- 0.02 # mean home range growth
gamma_var <- 0.1 # variance of home range growth,  make positive to add stochasticity
hist(rnorm(1e5,gamma_mean,gamma_var))

for (t in 2:timesteps){
  lambda_i <- rnorm(1 , lambda_mean , lambda_var)
  lambda_i = lambda_i + beta_hr*HRA[t-1]
  gamma_i <- rnorm(1 , gamma_mean , gamma_var )
  gamma_i = gamma_i + beta_gs*GS[t-1]
  GS[t] = lambda_i*GS[t-1] + GS[t-1]
  HRA[t] = gamma_i*HRA[t-1] + HRA[t-1]
}
par(mfrow=c(2,1) , mar=rep(0,4)+1 , oma=c(2,2,2,1))
plot( 1:30 , GS , col="slateblue" , pch=19)
plot( 1:30 , HRA , col="violet" , pch=19)
