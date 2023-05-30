library(rsoi)
library(janitor)
library(RColorBrewer)
library(lubridate)
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
d_dpl$year <- as.integer(year(d_dpl$date))
d_dpl_gs_2 <- merge(d_dpl, d_mei , by="year")

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

str(d_hr_gs_2)
mean_df <- aggregate(mei ~ year, d_mei, mean)
names(mean_df)[2] <- "mean_annual_mei"

d_mei_hr_data <- d_mei[is.element(d_mei$year , d_hr_gs_3$year),]

d_hr_gs_3 <- merge(d_hr_gs, mean_df , by="year")
d_hr_gs_3$year_index <- as.integer(as.factor(d_hr_gs_3$year))

d_hr_ov$year <- d_hr_ov$y1
d_hr_ov_3 <-merge(d_hr_ov, mean_df , by="year")
#plot raw data
plot(hr_area_mean~mean_annual_mei , data=d_hr_gs_3)
plot(overlap_uds~mean_annual_mei , data=d_hr_ov_3)

####
plot(mean_df[,2]~mean_df[,1])
set.seed(420)
str(d_hr_gs_3)

list_area <- list(
  hr_area_mean=d_hr_gs_3$hr_area_mean ,
  hr_area_high=d_hr_gs_3$hr_area_high ,
  hr_area_low=d_hr_gs_3$hr_area_low ,
  hr_area_sd=d_hr_gs_3$hr_area_sd ,
  mean_annual_mei=d_hr_gs_3$mean_annual_mei ,
  group_index=d_hr_gs_3$group_index ,
  group_size=d_hr_gs_3$group_size_std ,
  year_index=d_hr_gs_3$year_index 
)

list_area_2 <- list(
  hr_area_mean=d_hr_gs_3$hr_area_mean ,
  hr_area_high=d_hr_gs_3$hr_area_high ,
  hr_area_low=d_hr_gs_3$hr_area_low ,
  hr_area_sd=d_hr_gs_3$hr_area_sd ,
  group_index=d_hr_gs_3$group_index ,
  group_size=d_hr_gs_3$group_size_std ,
  year_index=d_hr_gs_3$year_index,
  mei=d_mei_hr_data$mei ,
  year_mei=d_mei_hr_data$year ,
  year_index_mei=as.integer(as.factor(d_mei_hr_data$year)),
  n_years=length(unique(d_hr_gs_3$year_index))
)

## this might need a fixin
# list_ov <- list(
#   mean_annual_mei=d_hr_ov$mean_annual_mei ,
#   overlap_uds=d_hr_ov$overlap_uds ,
#   dyad_index=d_hr_ov$dyad_index,
#   g1_index=d_hr_ov$g1_index,
#   g2_index=d_hr_ov$g2_index,
#   year_index = as.integer(as.factor(d_hr_ov$year)),
#   group_size1_std = d_hr_ov$group_size1_std ,
#   group_size2_std = d_hr_ov$group_size2_std ,
#   hr_area_mean1_std = d_hr_ov$hr_area_mean1_std ,
#   hr_area_mean2_std = d_hr_ov$hr_area_mean2_std,
#   rel_group_size1_std = d_hr_ov$rel_group_size1_std,
#   rel_group_size2_std = d_hr_ov$rel_group_size2_std
# )

# m1 <- ulam(
#   alist(
#     hr_area_mean ~ dgamma2( lambda , k) ,
#     #log(lambda) <- a_g[group_index] + bGS_g[group_index]*group_size_std,
#     log(lambda) <- a_g[group_index] + 
#       bENSO_g[group_index]*mean_annual_mei ,
#     c(a_g,bENSO_g)[group_index] ~ multi_normal(c(a,bENSO),Rho_g,sigma_g),
#     a ~ dnorm(2.392861,2),
#     bENSO ~ dnorm(0,2),
#     Rho_g ~ dlkjcorr(4),
#     c(k,sigma_g) ~ dexp(1)
#   ) , data=list_area , chains=4 , cores=4 ,control=list(adapt_delta=0.999))
# plot(precis(m1 , depth=2))
# precis(m1 , depth=2)

m1 <- ulam(
  alist(
    hr_area_mean ~ dgamma2( lambda , k) ,
    log(lambda) <- v_mu[1] + v[group_index,1] + 
      (v_mu[2] + v[group_index,2])*mean_annual_mei ,
    
    transpars> matrix[group_index,2]:v <-
      compose_noncentered( sigma_g , L_Rho_g , z_g ),
    matrix[2,group_index]: z_g ~ normal( 0 , 1 ),
    
    vector[2]:v_mu[[2]] ~ normal(0,2),
    vector[2]:v_mu[[1]] ~ normal(2.392861,2) ,
    cholesky_factor_corr[2]:L_Rho_g ~ lkj_corr_cholesky( 3 ),
    vector[2]: sigma_g ~ half_normal(0,1),
    k ~ exponential(1),
    gq> matrix[2,2]:Rho_g <<- Chol_to_Corr(L_Rho_g)
    
  ) , data=list_area , chains=4 , cores=4 , control=list(adapt_delta=0.95) )

precis(m1, depth=3)
###### model the yearly enso as a posterior
mm <- ulam(
  alist(
    mei ~  dnorm( mu , sigma ),
    mu <- am[year_index_mei],
    sigma ~ dexp(1),
    am[year_index_mei] ~ dnorm(0,2)
  ) , data=list_area_2 , chains=4 , cores=4 , control=list(adapt_delta=0.95)
)

precis(mm, depth=2)

#plot predictions. black is posterior mean, blue is marginal posterial predictions conditioned on sigma
post <- extract.samples(mm)
str(post)
#plot means
for(i in 1:max(list_area_2$year_index_mei)){
  dens(post$am[,i], xlim=c(-2.5,2.5) , ylim=c(0,3))
  points (list_area_2$mei[list_area_2$year_index_mei==i] , rep(0,12) , pch=19 , col=col.alpha("slateblue"))
  for(j in 1:50){
  curve(dnorm(x,post$am[j,i] , post$sigma[j]) , col=col.alpha("slateblue") , add=TRUE)
  }
}

m1dist <- ulam(
  alist(
    hr_area_mean ~ dgamma2( lambda , k) ,
    log(lambda) <- v_mu[1] + v[group_index,1] + 
      (v_mu[2] + v[group_index,2])*am[year_index] ,
    mei ~  dnorm( mu , sigma ),
    mu <- am[year_index_mei],
    sigma ~ dexp(1),
    am[year_index_mei] ~ dnorm(0,2),
    transpars> matrix[group_index,2]:v <-
      compose_noncentered( sigma_g , L_Rho_g , z_g ),
    matrix[2,group_index]: z_g ~ normal( 0 , 1 ),
    
    vector[2]:v_mu[[2]] ~ normal(0,2),
    vector[2]:v_mu[[1]] ~ normal(2.392861,2) ,
    cholesky_factor_corr[2]:L_Rho_g ~ lkj_corr_cholesky( 3 ),
    vector[2]: sigma_g ~ half_normal(0,1),
    k ~ exponential(1),
    gq> matrix[2,2]:Rho_g <<- Chol_to_Corr(L_Rho_g)
    
  ) , data=list_area_2 , chains=4 , cores=4 , control=list(adapt_delta=0.95) )

#model the distribution of enso as a predictor
m1dist2 <- ulam(
  alist(
    hr_area_mean ~ dgamma2( lambda , k) ,
    log(lambda) <- v_mu[1] + v[group_index,1] + 
      (v_mu[2] + v[group_index,2])*mei_dist[year_index] ,
    mei ~  dnorm( mu , sigma ),
    mu <- am[year_index_mei],
  vector[24]:mei_dist ~ normal (am , sigma),
    sigma ~ dexp(1),
    am[year_index_mei] ~ dnorm(0,2),
    transpars> matrix[group_index,2]:v <-
      compose_noncentered( sigma_g , L_Rho_g , z_g ),
    matrix[2,group_index]: z_g ~ normal( 0 , 1 ),
    
    vector[2]:v_mu[[2]] ~ normal(0,2),
    vector[2]:v_mu[[1]] ~ normal(2.392861,2) ,
    cholesky_factor_corr[2]:L_Rho_g ~ lkj_corr_cholesky( 3 ),
    vector[2]: sigma_g ~ half_normal(0,1),
    k ~ exponential(1),
    gq> matrix[2,2]:Rho_g <<- Chol_to_Corr(L_Rho_g)
    
  ) , data=list_area_2 , chains=4 , cores=4 , control=list(adapt_delta=0.95) )

plot(precis(m1dist2, depth=2))
m1y <- ulam(
  alist(
    hr_area_mean ~ dgamma2( lambda , k) ,
    log(lambda) <- v_mu[1] + v[group_index,1] + 
      (v_mu[2] + v[group_index,2])*mean_annual_mei ,
    transpars> matrix[group_index,2]:v <-
      compose_noncentered( sigma_g , L_Rho_g , z_g ),
    matrix[2,group_index]: z_g ~ normal( 0 , 1 ),
    
    vector[2]:v_mu[[2]] ~ normal(0,2),
    vector[2]:v_mu[[1]] ~ normal(2.392861,2),
    cholesky_factor_corr[2]:L_Rho_g ~ lkj_corr_cholesky( 3 ),
    vector[2]: sigma_g ~ half_normal(0,1),
    k ~ exponential(1),
    gq> matrix[2,2]:Rho_g <<- Chol_to_Corr(L_Rho_g)
    
  ) , data=list_area , chains=4 , cores=4 , control=list(adapt_delta=0.95) )


plot(precis(m1 , depth=3 , pars=c("v_mu" , "sigma_g" , "Rho_g") ) )
precis(m1 , depth=3)


m2 <- ulam(
  alist(
    hr_area_mean ~ dgamma2( lambda , k) ,
    log(lambda) <- a_g[group_index] + a_y[year_index] +
      bENSO_g[group_index]*mean_annual_mei ,
    c(a_g,bENSO_g)[group_index] ~ multi_normal(c(a,bENSO),Rho_g,sigma_g),
    a ~ dnorm(2.392861,2),
    a_y[year_index] ~ dnorm(0,sigma_y),
    bENSO ~ dnorm(0,2),
    Rho_g ~ dlkjcorr(4),
    c(k,sigma_g,sigma_y) ~ dexp(1)
  ) , data=list_area , chains=4 , cores=4 ,control=list(adapt_delta=0.999))
plot(precis(m2 , depth=3))
precis(m2 , depth=2)

m3 <- ulam(
  alist(
    hr_area_mean ~ dgamma2( lambda , k) ,
    log(lambda) <- a_g[group_index] + a_y[year_index] +
      bENSO_g[group_index]*mean_annual_mei + bGS_g[group_index]*group_size ,
    c(a_g,bENSO_g,bGS_g)[group_index] ~ multi_normal(c(a,bENSO,bGS),Rho_g,sigma_g) ,
    a ~ dnorm(2.392861,2),
    a_y[year_index] ~ dnorm(0,sigma_y) ,
    c(bENSO,bGS) ~ dnorm(0,2),
    Rho_g ~ dlkjcorr(3),
    c(k,sigma_g,sigma_y) ~ dexp(1)
  ) , data=list_area , chains=4 , cores=4 ,control=list(adapt_delta=0.999))

plot(precis(m3 , depth=3))
precis(m3 , depth=2)
precis(m2 , depth=2)

m3 <- ulam(
  alist(
    hr_area_mean ~ dgamma2( lambda , k) ,
    log(lambda) <- v_mu[1] + v[group_index,1] + 
      (v_mu[2] + v[group_index,2])*mean_annual_mei +
      (v_mu[3] + v[group_index,3])*group_size,
    
    transpars> matrix[group_index,3]:v <-
      compose_noncentered( sigma_g , L_Rho_g , z_g ),
    matrix[3,group_index]: z_g ~ normal( 0 , 1 ),
    vector[3]:v_mu[[3]] ~ normal(0,1),
    vector[3]:v_mu[[2]] ~ normal(0,1),
    vector[3]:v_mu[[1]] ~ normal(2.392861,2),
    cholesky_factor_corr[3]:L_Rho_g ~ lkj_corr_cholesky( 3 ),
    vector[3]: sigma_g ~ exponential(1),
    k ~ exponential(1),
    gq> matrix[3,3]:Rho_g <<- Chol_to_Corr(L_Rho_g)
    
  ) , data=list_area , chains=4 , cores=4 , control=list(adapt_delta=0.95) )

precis(m3, depth=3)
##overlap
m_ov1 <- ulam(
  alist(
    overlap_uds ~ dbeta2( p , theta) ,
    logit(p) <- d[dyad_index] + g[g1_index] + g[g2_index],
    g[g1_index]  ~ normal(0,sigma_g),
    d[dyad_index]  ~ normal(0,sigma_d),
    c(theta,sigma_g,sigma_d) ~ dexp(1)
  ) , 
  data=list_ov , chains=4 , cores=4 , iter=1000, control=list(adapt_delta=0.99) )
precis(m_ov1 , depth=1 )
plot(precis(m_ov1 , depth=3 ))

m_ov2 <- ulam(
  alist(
    overlap_uds ~ dbeta2( p , theta) ,
    logit(p) <- a + d[dyad_index] + g[g1_index] + g[g2_index]
    + bENSO*mean_annual_mei,
    c(a,bENSO) ~ dnorm(0,1),
    g[g1_index]  ~ normal(0,sigma_g),
    d[dyad_index]  ~ normal(0,sigma_d),
    c(theta,sigma_g,sigma_d) ~ dexp(1),
    Rho ~ lkj_corr(3)
  ) , 
  data=list_ov , chains=4 , cores=4 , iter=1000, control=list(adapt_delta=0.999) )

precis(m_ov2 , depth=2)

m_ov3 <- ulam(
  alist(
    overlap_uds ~ dbeta2( p , theta) ,
    logit(p) <- d[dyad_index] + g[g1_index] + g[g2_index]
    + bENSO_g[g1_index]*mean_annual_mei + bENSO_g[g2_index]*mean_annual_mei,
    c(a,bENSO) ~ dnorm(0,1),
    c(g,bENSO_g)[g1_index]  ~ multi_normal( c(a,bENSO) , Rho , sigma_g ),
    d[dyad_index]  ~ normal(0,sigma_d),
    c(theta,sigma_g,sigma_d) ~ dexp(1),
    Rho ~ lkj_corr(3)
  ) , 
  data=list_ov , chains=4 , cores=4 , iter=1000, control=list(adapt_delta=0.999) )

m_ov4 <- ulam(
  alist(
    overlap_uds ~ dbeta2( p , theta) ,
    logit(p) <- d[dyad_index] + g[g1_index] + g[g2_index]
    + bHR_g[g1_index]*hr_area_mean1_std + bHR_g[g2_index]*hr_area_mean2_std
    + bENSO_g[g1_index]*mean_annual_mei + bENSO_g[g2_index]*mean_annual_mei,
    
    c(a,bHR,bENSO) ~ dnorm(0,1),
    c(g,bHR_g,bENSO_g)[g1_index]  ~ multi_normal( c(a,bHR,bENSO) , Rho , sigma_g ),
    d[dyad_index]  ~ normal(0,sigma_d),
    c(theta,sigma_g,sigma_d) ~ dexp(1),
    Rho ~ lkj_corr(3)
  ) , 
  data=list_ov , chains=4 , cores=4 , iter=1000, control=list(adapt_delta=0.99) )
precis(m_ov4 , depth=1 )
plot(precis(m_ov4 , depth=3 ))

nrow(list_ov)


