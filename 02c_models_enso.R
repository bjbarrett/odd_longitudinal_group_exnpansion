library(rsoi)
library(janitor)
library(RColorBrewer)
library(lubridate)
library(ctmm)
library(tidyverse)

#get enso data
mei <- clean_names(download_mei())
d_mei <- mei[mei$year >= min(d_hr_gs$year),]
d_mei <- d_mei[complete.cases(d_mei),]
plot(d_mei$mei~d_mei$date)
#combie data frames, will do posterior across time series later
str(d_hr_gs)
str(d_mei)
elcol_pal <- rev(brewer.pal(3 , "RdYlBu"))
group_pal <- brewer.pal(11 , "Spectral")

d_mei$phase_index <- as.integer(d_mei$phase)
plot(d_mei$mei~d_mei$date , col=elcol_pal[d_mei$phase_index] , pch="x" , cex=0.5)
mei_spl <- with(d_mei, smooth.spline(date, mei))
lines(mei_spl, col = "grey3")
abline(v=d_mei$date[1:33] , col="grey")
d_hr_gs_2 <- merge(d_hr_gs, d_mei , by="year")
d_hr_gs_2 <- d_hr_gs_2[d_hr_gs_2$month=="JJ",]
min(d_mei$year)
d_mei$year_index_overall <- d_mei$year - 1990


#all groups
plot(d_mei$mei~d_mei$date , col=elcol_pal[d_mei$phase_index] , pch="x" , cex=0.7 , ylim=c(-2.5,2.5))
lines(mei_spl, col = "grey3")
points( d_hr_gs_2$date , standardize(d_hr_gs_2$hr_area_mean) , col=group_pal[d_hr_gs_2$group_index] , pch=19)
abline(v=d_mei$date[1:33] , col="grey")
for(i in c(1:3,5:11)){
  grp_spl <- with(d_hr_gs_2[d_hr_gs_2$group_index==i,], smooth.spline(date, mei ,spar=.5))
  lines(grp_spl, col = group_pal[i])
}

#per group plot
for(i in 1:11){
  plot(d_mei$mei~d_mei$date , col=elcol_pal[d_mei$phase_index] , pch="x" ,
       cex=0.7 , ylim=c(-2.5,2.5) , main=min(d_hr_gs_2$group[d_hr_gs_2$group_index==i] ) )
  lines(mei_spl, col = "grey3")
  points( d_hr_gs_2$date[d_hr_gs_2$group_index==i] , 
          standardize(d_hr_gs_2$hr_area_mean[d_hr_gs_2$group_index==i]) , 
          col=group_pal[i] , pch=19)
  abline(v=d_mei$date[1:33] , col="grey")
}

###mei consolidate
str(d_hr_gs_2)
mean_df <- aggregate(mei ~ year, d_mei, mean)
names(mean_df)[2] <- "mean_annual_mei"
max_df <- aggregate(mei ~ year, d_mei, max)
names(max_df)[2] <- "max_annual_mei"
min_df <- aggregate(mei ~ year, d_mei, min)
names(min_df)[2] <- "min_annual_mei"
sd_df <- aggregate(mei ~ year, d_mei, sd)
names(sd_df)[2] <- "sd_annual_mei"
## get akdes
# get UD telemetry object
UD <- readRDS("/Users/sifaka/Downloads/slp_1990-2019_RSF_AKDEs.rds")

# function to get summary information from AKDEs
summarize_akde <- function(akde){
  
  summary <- summary(akde, units = FALSE) # makes the units fro all UDs the same (m2)

  tibble(id = akde@info$identity, 
         DOF = summary$DOF[1],
         low = (summary$CI[1])/1000000, # convert m2 to km2
         area = (summary$CI[2])/1000000,
         high = (summary$CI[3])/1000000)
}

# wrapper to stack area info into data frame
make_df <- function(id){
  map_dfr(id, summarize_akde) 
}

# apply functions to get data frame and calculate shape and rate
d_akde <- make_df(UD) %>% 
  mutate(scale = area/DOF,
         rate=DOF/area , 
         shape = DOF)
str(d_akde)
##compile bigger data frames

d_hr_gs_3 <- merge(d_hr_gs, mean_df , by="year")
d_hr_gs_3 <- merge(d_hr_gs_3, min_df , by="year")
d_hr_gs_3 <- merge(d_hr_gs_3, max_df , by="year")
d_hr_gs_3 <- merge(d_hr_gs_3, sd_df , by="year")
d_hr_gs_3 <- merge(d_hr_gs_3, d_akde , by="id")

d_hr_gs_3$year_index <- as.integer(as.factor(d_hr_gs_3$year))
d_mei_hr_data <- d_mei[is.element(d_mei$year , d_hr_gs_3$year),]


# d_hr_ov$year <- d_hr_ov$y1
# d_hr_ov_3 <-merge(d_hr_ov, mean_df , by="year")

str(d_hr_gs_3)

list_area <- list(
  hr_area_mean=d_hr_gs_3$hr_area_mean ,
  hr_area_high=d_hr_gs_3$hr_area_high ,
  hr_area_low=d_hr_gs_3$hr_area_low ,
  hr_area_sd=d_hr_gs_3$hr_area_sd ,
  mean_annual_mei=d_hr_gs_3$mean_annual_mei ,
  min_annual_mei=d_hr_gs_3$min_annual_mei ,
  max_annual_mei=d_hr_gs_3$max_annual_mei ,
  sd_annual_mei=d_hr_gs_3$sd_annual_mei ,
  group_index=d_hr_gs_3$group_index ,
  group_size=d_hr_gs_3$group_size_std ,
  year_index=d_hr_gs_3$year_index 
)

list_area_2 <- list(
  hr_area_mean=d_hr_gs_3$hr_area_mean ,
  hr_area_high=d_hr_gs_3$hr_area_high ,
  hr_area_low=d_hr_gs_3$hr_area_low ,
  hr_area_sd=d_hr_gs_3$hr_area_sd ,
  hr_area_rate=d_hr_gs_3$rate ,
  hr_area_shape=d_hr_gs_3$shape ,
  group_index=d_hr_gs_3$group_index ,
  group_size=d_hr_gs_3$group_size_std ,
  year_index=d_hr_gs_3$year_index,
  mei=d_mei_hr_data$mei ,
  year_mei=d_mei_hr_data$year ,
  year_index_mei=as.integer(as.factor(d_mei_hr_data$year)),
  N_years=length(unique(d_mei_hr_data$year)),
  N=nrow(d_hr_gs_3) ,
  N_groups=length(unique(d_hr_gs_3$group_index)) ,
  mean_annual_mei=d_hr_gs_3$mean_annual_mei ,
  min_annual_mei=d_hr_gs_3$min_annual_mei ,
  max_annual_mei=d_hr_gs_3$max_annual_mei ,
  sd_annual_mei=d_hr_gs_3$sd_annual_mei 
)

###visually inspect eate shape
for(i in 10:20){
  plot(density(rgamma(10000,shape=d_akde$shape[[i]], rate=d_akde$rate[[i]] ) , xlim=c(0,10)) , main="blah" )
  lines(density(rgamma(10000,shape=d_akde$shape[[i]], scale=d_akde$scale[[i]] ) ) , lty=2 )
  points( d_akde$area[i] , 0.1 )
  segments(  x0=d_akde$low[i], y0=0.1 , x1=d_akde$high[i] ,y1= 0.1 , col="blue")
}


##stan models
file_name <- 'stan_code/test_mei.stan'
fit= stan( file = file_name,
              data = list_area_2 ,
              iter = 1000,
              chains=4,
              cores=4,
              control=list(adapt_delta=0.9) ,
              refresh=100,
              init=0,
              seed=12
)

precis(fit , depth=2)
post <- extract.samples(fit)
for(i in 1:24){
  dens(post$am[,i] , xlim=c(-3,3))
  dens(post$am_pred[,i], add=TRUE , lty=2)
  points(list_area_2$mei[list_area_2$year_index_mei==i] , rep(0,12), col="red")
}

file_name <- 'stan_code/mei_hr.stan'
fit_hr= stan( file = file_name,
            data = list_area_2 ,
            iter = 4000,
            chains=4,
            cores=4,
            control=list(adapt_delta=0.99) ,
            refresh=250,
            init=0,
            seed=232
)

precis(fit_hr , depth=2)

file_name <- 'stan_code/mei_hr_gs.stan'
fit_hr_gs= stan( file = file_name,
              data = list_area_2 ,
              iter = 4000,
              chains=4,
              cores=4,
              control=list(adapt_delta=0.99) ,
              refresh=250,
              init=0,
              seed=813
)

precis(fit_hr_gs , depth=2)


####old ulam stuff
# m1 <- ulam(
#   alist(
#     hr_area_mean ~ dgamma2( lambda , k) ,
#     log(lambda) <- v_mu[1] + v[group_index,1] + 
#       (v_mu[2] + v[group_index,2])*mean_annual_mei ,
#     
#     transpars> matrix[group_index,2]:v <-
#       compose_noncentered( sigma_g , L_Rho_g , z_g ),
#     matrix[2,group_index]: z_g ~ normal( 0 , 1 ),
#     
#     vector[2]:v_mu[[2]] ~ normal(0,2),
#     vector[2]:v_mu[[1]] ~ normal(2.392861,2) ,
#     cholesky_factor_corr[2]:L_Rho_g ~ lkj_corr_cholesky( 3 ),
#     vector[2]: sigma_g ~ exponential(1),
#     k ~ exponential(1),
#     gq> matrix[2,2]:Rho_g <<- Chol_to_Corr(L_Rho_g)
#     
#   ) , data=list_area , chains=4 , cores=4 , control=list(adapt_delta=0.95) )
# 
# precis(m1, depth=3)
# 
# m1min <- ulam(
#   alist(
#     hr_area_mean ~ dgamma2( lambda , k) ,
#     log(lambda) <- v_mu[1] + v[group_index,1] + 
#       (v_mu[2] + v[group_index,2])*min_annual_mei ,
#     
#     transpars> matrix[group_index,2]:v <-
#       compose_noncentered( sigma_g , L_Rho_g , z_g ),
#     matrix[2,group_index]: z_g ~ normal( 0 , 1 ),
#     
#     vector[2]:v_mu[[2]] ~ normal(0,2),
#     vector[2]:v_mu[[1]] ~ normal(2.392861,2) ,
#     cholesky_factor_corr[2]:L_Rho_g ~ lkj_corr_cholesky( 3 ),
#     vector[2]: sigma_g ~ exponential(1),
#     k ~ exponential(1),
#     gq> matrix[2,2]:Rho_g <<- Chol_to_Corr(L_Rho_g)
#     
#   ) , data=list_area , chains=4 , cores=4 , control=list(adapt_delta=0.95) )
# 
# precis(m1min, depth=3)
# 
# m1max <- ulam(
#   alist(
#     hr_area_mean ~ dgamma2( lambda , k) ,
#     log(lambda) <- v_mu[1] + v[group_index,1] + 
#       (v_mu[2] + v[group_index,2])*max_annual_mei ,
#     
#     transpars> matrix[group_index,2]:v <-
#       compose_noncentered( sigma_g , L_Rho_g , z_g ),
#     matrix[2,group_index]: z_g ~ normal( 0 , 1 ),
#     
#     vector[2]:v_mu[[2]] ~ normal(0,2),
#     vector[2]:v_mu[[1]] ~ normal(2.392861,2) ,
#     cholesky_factor_corr[2]:L_Rho_g ~ lkj_corr_cholesky( 3 ),
#     vector[2]: sigma_g ~ exponential(1),
#     k ~ exponential(1),
#     gq> matrix[2,2]:Rho_g <<- Chol_to_Corr(L_Rho_g)
#     
#   ) , data=list_area , chains=4 , cores=4 , control=list(adapt_delta=0.95) )
# 
# precis(m1max, depth=3)
# ###### model the yearly enso as a posterior
# mm <- ulam(
#   alist(
#     mei ~  dnorm( mu , sigma ),
#     mu <- am[year_index_mei],
#     sigma ~ dexp(1),
#     am[year_index_mei] ~ dnorm(0,2)
#   ) , data=list_area_2 , chains=4 , cores=4 , control=list(adapt_delta=0.95)
# )
# 
# precis(mm, depth=2)
# postmm <- extract.samples(mm)
# 
# #plot predictions. black is posterior mean, blue is marginal posterial predictions conditioned on sigma
# #plot means
# for(i in 1:max(list_area_2$year_index_mei)){
#   dens(postmm$am[,i], xlim=c(-2.5,2.5) , ylim=c(0,3))
#   points (list_area_2$mei[list_area_2$year_index_mei==i] , rep(0,12) , pch=19 , col=col.alpha("slateblue"))
#   for(j in 1:50){
#     curve(dnorm(x, postmm$am[j,i] , postmm$sigma[j]) , col=col.alpha("slateblue") , add=TRUE)
#   }
# }
# 
# 
# #just using postrior of mean
# m0dist <- ulam(
#   alist(
#     hr_area_mean ~ dgamma2( lambda , k) ,
#     log(lambda) <- v_mu[1] + v[group_index,1] + 
#       (v_mu[2] + v[group_index,2])*mei_true[i] ,
#     ##the mei submodel
#     mean_annual_mei ~ dnorm( mei_true , sd_annual_mei ),
#     vector[130]:mei_true ~ dnorm( 0 , 1 ),
#     ## cholesky decomop for vcov matrix
#     transpars> matrix[group_index,2]:v <-
#       compose_noncentered( sigma_g , L_Rho_g , z_g ),
#     matrix[2,group_index]: z_g ~ normal( 0 , 1 ),
#     #other priors
#     vector[2]:v_mu[[2]] ~ normal(0,2),
#     vector[2]:v_mu[[1]] ~ normal(2.392861,2) ,
#     cholesky_factor_corr[2]:L_Rho_g ~ lkj_corr_cholesky( 3 ),
#     vector[2]: sigma_g ~ half_normal(0,1),
#     k ~ exponential(1),
#     # back transform in GQ
#     gq> matrix[2,2]:Rho_g <<- Chol_to_Corr(L_Rho_g)
#   ) , data=list_area , chains=4 , cores=4 , control=list(adapt_delta=0.95) )
# 
# precis(m0dist, depth=2)
# 
# m1dist <- ulam(
#   alist(
#     hr_area_mean ~ dgamma2( lambda , k) ,
#     log(lambda) <- v_mu[1] + v[group_index,1] + 
#       (v_mu[2] + v[group_index,2])*am[year_index] ,
#     ##the mei submodel
#     mei ~  dnorm( mu , sigma ),
#     mu <- am[year_index_mei],
#     sigma ~ dexp(1),
#     am[year_index_mei] ~ dnorm(0,2),
#     ## cholesky decomop for vcov matrix
#     transpars> matrix[group_index,2]:v <-
#       compose_noncentered( sigma_g , L_Rho_g , z_g ),
#     matrix[2,group_index]: z_g ~ normal( 0 , 1 ),
#     #other priors
#     vector[2]:v_mu[[2]] ~ normal(0,2),
#     vector[2]:v_mu[[1]] ~ normal(2.392861,2) ,
#     cholesky_factor_corr[2]:L_Rho_g ~ lkj_corr_cholesky( 3 ),
#     vector[2]: sigma_g ~ half_normal(0,1),
#     k ~ exponential(1),
#     # back transform in GQ
#     gq> matrix[2,2]:Rho_g <<- Chol_to_Corr(L_Rho_g)
#   ) , data=list_area_2 , chains=4 , cores=4 , control=list(adapt_delta=0.95) )
# 
# precis(m1dist , depth=2)
# 
# 
# #model the distribution of enso as a predictor
# m1dist2 <- ulam(
#   alist(
#     hr_area_mean ~ dgamma2( lambda , k) ,
#     log(lambda) <- v_mu[1] + v[group_index,1] + 
#       (v_mu[2] + v[group_index,2])*mei_dist[year_index] ,
#     ## enso submodel
#     mei ~  dnorm( mu , sigma ),
#     mu <- am[year_index_mei],
#     vector[24]:mei_dist ~ normal(am , sigma) ,
#     sigma ~ exponential(1),
#     am[year_index_mei] ~ dnorm(0,2),
#     ## main likelihood loop stuff
#     transpars> matrix[group_index,2]:v <-
#       compose_noncentered( sigma_g , L_Rho_g , z_g ),
#     matrix[2,group_index]: z_g ~ normal( 0 , 1 ),
#     vector[2]:v_mu[[2]] ~ normal(0,2),
#     vector[2]:v_mu[[1]] ~ normal(2.392861,2) ,
#     cholesky_factor_corr[2]:L_Rho_g ~ lkj_corr_cholesky( 3 ),
#     vector[2]: sigma_g ~ exponential(1),
#     k ~ exponential(1),
#     gq> matrix[2,2]:Rho_g <<- Chol_to_Corr(L_Rho_g)
#   ) , data=list_area_2 , chains=4 , cores=4 , control=list(adapt_delta=0.99) )
# 
# 
# plot(precis(m1dist2, depth=2 ))
# post <- extract.samples(m1dist2)
# plot(post$mei_dist~post$am)
# plot(d_hr_gs_3$mean_annual_mei,d_hr_gs_3$hr_area_mean , col=group_pal[d_hr_gs_3$group_index])
# 
# m1dist3 <- ulam(
#   alist(
#     hr_area_mean ~ dgamma2( lambda , k) ,
#     log(lambda) <- v_mu[1] + v[group_index,1] + 
#       (v_mu[2] + v[group_index,2])*mei_dist[year_index] +
#       (v_mu[3] + v[group_index,3])*group_size ,
#     ## enso submodel
#     mei ~  dnorm( mu , sigma ),
#     mu <- am[year_index_mei],
#     vector[24]:mei_dist ~ normal (am , sigma),
#     sigma ~ dexp(1),
#     am[year_index_mei] ~ dnorm(0,2),
#     ## main likelihood loop stuff
#     transpars> matrix[group_index,3]:v <-
#       compose_noncentered( sigma_g , L_Rho_g , z_g ),
#     matrix[3,group_index]: z_g ~ normal( 0 , 1 ),
#     vector[3]:v_mu[[3]] ~ normal(0,2),
#     vector[3]:v_mu[[2]] ~ normal(0,2),
#     vector[3]:v_mu[[1]] ~ normal(2.392861,2) ,
#     cholesky_factor_corr[3]:L_Rho_g ~ lkj_corr_cholesky( 3 ),
#     vector[3]: sigma_g ~ exponential(1),
#     k ~ exponential(1),
#     gq> matrix[3,3]:Rho_g <<- Chol_to_Corr(L_Rho_g)
#   ) , data=list_area_2 , chains=4 , cores=4 , control=list(adapt_delta=0.99) )
# 
# plot(precis(m1dist3 , depth=3))
# 
# ##measurement error on outcome
# area_trk ~ dnorm( area_trk_true  , sd_trk ),
# vector[N]:area_trk_true  ~ dnorm( mu , sigma ),
# 
# m1dist2 <- ulam(
#   alist(
#     hr_area_true ~ dgamma2( lambda , k ) ,
#     vector[N]:hr_area_true ~ dgamma2( hr_shape , hr_area_sd ),
#     log(lambda) <- v_mu[1] + v[group_index,1] + 
#       (v_mu[2] + v[group_index,2])*mei_dist[year_index] ,
#     ## enso submodel
#     mei ~  dnorm( mu , sigma ),
#     mu <- am[year_index_mei],
#     vector[24]:mei_dist ~ normal (am , sigma),
#     sigma ~ dexp(1),
#     am[year_index_mei] ~ dnorm(0,2),
#     ## main likelihood loop stuff
#     transpars> matrix[group_index,2]:v <-
#       compose_noncentered( sigma_g , L_Rho_g , z_g ),
#     matrix[2,group_index]: z_g ~ normal( 0 , 1 ),
#     vector[2]:v_mu[[2]] ~ normal(0,2),
#     vector[2]:v_mu[[1]] ~ normal(2.392861,2) ,
#     cholesky_factor_corr[2]:L_Rho_g ~ lkj_corr_cholesky( 3 ),
#     vector[2]: sigma_g ~ exponential(1),
#     k ~ exponential(1),
#     gq> matrix[2,2]:Rho_g <<- Chol_to_Corr(L_Rho_g)
#   ) , data=list_area_2 , chains=4 , cores=4 , control=list(adapt_delta=0.99) )
# 
# plot(precis(m1dist2, depth=2 ))
# 
# m1dist2 <- ulam(
#   alist(
#     hr_area_true[] ~ gamma( hr_shape , hr_area_sd)
#   ) , data=d_hr_gs_3, chains=4 , cores=4 , control=list(adapt_delta=0.99) 

     