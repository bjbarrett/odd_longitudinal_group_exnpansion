library(rsoi)
library(janitor)
#get enso data
mei <- clean_names(download_mei())
d_mei <- mei[mei$year >= min(d_hr_gs$year),]
plot(d_mei$mei~d_mei$date)
#combie data frames, will do posterior across tiem series later
str(d_hr_gs)
str(d_mei)
d_mei$mean_annual_mei <- d_mei
sapply(d_mei$mei , 1 , mean)

mean_df <- aggregate(mei ~ year, d_mei, mean)
names(mean_df)[2] <- "mean_annual_mei"
d_hr_gs <- merge(d_hr_gs, mean_df , by="year")
d_hr_gs$year_index <- as.integer(as.factor(d_hr_gs$year))
d_hr_ov$year <- d_hr_ov$y1
d_hr_ov <-merge(d_hr_ov, mean_df , by="year")
#plot raw data
plot(hr_area_mean~mean_annual_mei , data=d_hr_gs)
plot(overlap_uds~mean_annual_mei , data=d_hr_ov)

####
plot(mean_df[,2]~mean_df[,1])
set.seed(420)

list_area <- list(
  hr_area_mean=d_hr_gs$hr_area_mean ,
  mean_annual_mei=d_hr_gs$mean_annual_mei ,
  group_index=d_hr_gs$group_index ,
  group_size=d_hr_gs$group_size_std ,
  year_index=d_hr_gs$year_index 
)


list_ov <- list(
  mean_annual_mei=d_hr_gs$mean_annual_mei ,
  overlap_uds=d_hr_ov$overlap_uds ,
  dyad_index=d_hr_ov$dyad_index,
  g1_index=d_hr_ov$g1_index,
  g2_index=d_hr_ov$g2_index,
  year_index = as.integer(as.factor(d_hr_ov$y1)),
  group_size1_std = d_hr_ov$group_size1_std ,
  group_size2_std = d_hr_ov$group_size2_std ,
  hr_area_mean1_std = d_hr_ov$hr_area_mean1_std ,
  hr_area_mean2_std = d_hr_ov$hr_area_mean2_std,
  rel_group_size1_std = d_hr_ov$rel_group_size1_std,
  rel_group_size2_std = d_hr_ov$rel_group_size2_std
)

m1 <- ulam(
  alist(
    hr_area_mean ~ dgamma2( lambda , k) ,
    #log(lambda) <- a_g[group_index] + bGS_g[group_index]*group_size_std,
    log(lambda) <- a_g[group_index] + 
      bENSO_g[group_index]*mean_annual_mei ,
    c(a_g,bENSO_g)[group_index] ~ multi_normal(c(a,bENSO),Rho_g,sigma_g),
    a ~ dnorm(2.392861,2),
    bENSO ~ dnorm(0,2),
    Rho_g ~ dlkjcorr(4),
    c(k,sigma_g) ~ dexp(1)
  ) , data=list_area , chains=4 , cores=4 ,control=list(adapt_delta=0.999))
plot(precis(m1 , depth=2))
precis(m1 , depth=2)

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

##overlap
m_ov1 <- ulam(
  alist(
    overlap_uds ~ dbeta2( p , theta) ,
    logit(p) <- d[dyad_index] + g[g1_index] + g[g2_index],
    
    c(a,bHR) ~ dnorm(0,1),
    c(g,bHR_g)[g1_index]  ~ multi_normal( c(a,bHR) , Rho , sigma_g ),
    d[dyad_index]  ~ normal(0,sigma_d),
    c(theta,sigma_g,sigma_d) ~ dexp(1),
    Rho ~ lkj_corr(3)
  ) , 
  data=list_ov , chains=4 , cores=4 , iter=1000, control=list(adapt_delta=0.99) )
precis(m_ov1 , depth=1 )
plot(precis(m_ov_1 , depth=3 ))

m_ov_3 <- ulam(
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
precis(m_ov_3 , depth=1 )
plot(precis(m_ov_3 , depth=3 ))




