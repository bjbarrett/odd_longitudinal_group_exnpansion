library(rethinking)
library(cmdstanr)
library(janitor)
library(dplyr)

# option to make stan run faster
options(mc.cores = parallel::detectCores())

# make list for overlap model
listerine <- list(
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

##simulate to figure out beta regression in rethinking
sims <- rbeta2(1000 , 0.3 , 4)
sim_list <- list( obz=sims)
m_beta_sim <- ulam(
  alist(
    obz ~ dbeta2( p , theta) ,
    logit(p) <- a ,
    a ~ dnorm(0,1),
    theta ~ dexp(1)
  ) , 
  data=sim_list , chains=4 , cores=4 )
precis(m_beta_sim)
precis(m_beta_sim)[1,1]
logistic(precis(m_beta_sim)[1,1]) #close to 0.3
precis(m_beta_sim)[2,1] # close to 4

##########HOME RANGE SIZE##############
#varying intecepts and slopes
set.seed(6)
m_gs_1_gauss <- ulam(
  alist(
    hr_area_mean ~ dnorm( mu , sigma) ,
    mu <- a_g[group_index] + bGS_g[group_index]*group_size_std,
    c(a_g,bGS_g)[group_index] ~ multi_normal(c(a,bGS),Rho_g,sigma_g),
    a ~ dnorm(2.392861,1),
    bGS ~ dnorm(0,1),
    Rho_g ~ dlkjcorr(4),
    c(sigma,sigma_g) ~ dexp(1)
  ) , data=d_hr_gs , chains=4 , cores=4 ,control=list(adapt_delta=0.99))

set.seed(420)
m_gs_1_gam <- ulam(
  alist(
    hr_area_mean ~ dgamma2( lambda , k) ,
    log(lambda) <- a_g[group_index] + bGS_g[group_index]*group_size_std,
    c(a_g,bGS_g)[group_index] ~ multi_normal(c(a,bGS),Rho_g,sigma_g),
    a ~ dnorm(2.392861,1),
    bGS ~ dnorm(0,1),
    Rho_g ~ dlkjcorr(4),
    c(k,sigma_g) ~ dexp(1)
  ) , data=d_hr_gs , chains=4 , cores=4 ,control=list(adapt_delta=0.99))
# trankplot(m_gs_1) #diagnostics of mixing
sort(unique(d_hr_gs$group))
temp <- precis(m_gs_1_gam , depth=3 )
names <- paste(temp@row.names[1:22] , sort(unique(d_hr_gs$group)) )
plot(precis(m_gs_1_gam , depth=3 ))

## 
# Interaction between group size and weighted mean -------------------
##
# weighted mean based on neighboring groups group size * overlap
# weights are very biased at the moment because in some years groups have less neighbors because 
# neighbors were not habituated yet or 
# they are closer to the edge of study site and their neighbors have never been habituated

wt_list<- list(
  hr_area_mean = d_weights$hr_area_mean,
  group_index = d_weights$group_index,
  group_size_std = d_weights$group_size_std,
  neighbors_effect_std = d_weights$weighted_mean_neighbor_comp_std
)


set.seed(67)
m_gs_wt_1_gauss <- ulam(
  alist(
    hr_area_mean ~ dnorm( mu , sigma) ,
    mu <- a_g[group_index] + 
      bGS_g[group_index]*group_size_std + 
      bCA_g[group_index]*neighbors_effect_std + 
      bI_g[group_index]*group_size_std*neighbors_effect_std,
    c(a_g,bGS_g,bCA_g,bI_g)[group_index] ~ multi_normal(c(a,bGS,bCA,bI),Rho_g,sigma_g),
    a ~ dnorm(2.392861,1),
    bGS ~ dnorm(0,1),
    bCA ~ dnorm(0,1),
    bI ~ dnorm(0,1),
    Rho_g ~ dlkjcorr(4),
    c(sigma,sigma_g) ~ dexp(1)
  ) , data=wt_list , chains=4 , cores=4 ,control=list(adapt_delta=0.99))

plot(precis(m_gs_wt_1_gauss))
plot(precis(m_gs_wt_1_gauss, depth = 3))

### BRMS model
library(brms)

# make data frame 
wt_df <- d_weights %>% 
  dplyr::select(hr_area_mean,
                group, 
                group_size_std, 
                weighted_mean_neighbor_comp_std )

# set priors
reg_priors <- c(
  prior(normal(0,1), class = "b"),
  prior(normal(0,1.5), class = "Intercept"),
  prior(exponential(0.67), class = "sd"),
  prior(lkj(3), class = "cor")
)

# model with data
m_wt_brms <- brm(
  bf(hr_area_mean ~ group_size_std*weighted_mean_neighbor_comp_std + (group_size_std|group)),
  data = wt_df,
  save_pars = save_pars(all=TRUE),
  family = Gamma(link = "log"),
  prior = reg_priors,
  init  = "0",
  control = list(adapt_delta = 0.99999,
                 max_treedepth = 13),
  chains = 4, iter = 2000, warmup = 1000,
  cores = 4, seed = 1234, 
  backend = "cmdstanr"
)

# evaluate posterior
pp_check(m_wt_brms, ndraws = 100)
conditional_effects(m_wt_brms)
mcmc_plot(m_wt_brms)

############HOME RANGE OVERLAP##################


####intercepts only model
d_hr_ov[which(d_hr_ov$overlap_uds==0),]
m_ov_0 <- ulam(
  alist(
    overlap_uds ~ dbeta2( p , theta) ,
    logit(p) <- a ,
    a ~ dnorm(0,1),
    theta ~ dexp(1)
  ) , 
  data=listerine , chains=4 , cores=4 )

precis(m_ov_0)


m_ov_1 <- ulam(
  alist(
    overlap_uds ~ dbeta2( p , theta) ,
    logit(p) <- a + d[dyad_index] + g[g1_index] + g[g2_index],
    a ~ dnorm(0,1),
    g[g1_index]  ~ normal(0,sigma_g),
    d[dyad_index]  ~ normal(0,sigma_d),
    c(theta,sigma_g,sigma_d) ~ dexp(1)
  ) , 
  data=listerine , chains=4 , cores=4 , control=list(adapt_delta=0.9))

dens(listerine$overlap_uds)
plot(precis(m_ov_1 , depth=2))
precis(m_ov_1 , depth=2)

m_ov_1a <- ulam(
  alist(
    overlap_uds ~ dbeta2( p , theta) ,
    logit(p) <- a + d[dyad_index] + g[g1_index] ,
    logit(p) <- a + d[dyad_index] + g[g2_index] ,
    a ~ dnorm(0,1),
    g[g1_index]  ~ normal(0,sigma_g),
    d[dyad_index]  ~ normal(0,sigma_d),
    c(theta,sigma_g,sigma_d) ~ dexp(1)
  ) , 
  data=listerine , chains=4 , cores=4 , control=list(adapt_delta=0.9))
plot(precis(m_ov_1a , depth=2))
precis(m_ov_1a , depth=2)

str(d_hr_ov)

set.seed(4)
m_ov_1c <- ulam(
  alist(
    overlap_uds ~ dbeta2( p , theta) ,
    logit(p) <- a + d[dyad_index] + g[g1_index] + g[g2_index] + y[year_index]
    ,
    a ~ dnorm(0,1),
    g[g1_index]  ~ normal(0,sigma_g),
    d[dyad_index]  ~ normal(0,sigma_d),
    y[year_index]  ~ normal(0,sigma_y),
    
    c(theta,sigma_g,sigma_d,sigma_y) ~ dexp(1)
  ) , 
  data=listerine , chains=4 , cores=4 , control=list(adapt_delta=0.95))
precis(m_ov_1c , depth=2)
plot(precis(m_ov_1c , depth=2 , pars='y'))

set.seed(943)
m_ov_2 <- ulam(
  alist(
    overlap_uds ~ dbeta2( p , theta) ,
    logit(p) <- d[dyad_index] + g[g1_index] + g[g2_index]
    + bGS_g[g1_index]*group_size1_std + bGS_g[g2_index]*group_size2_std,
     
    c(a,bGS) ~ dnorm(0,1),
    c(g,bGS_g)[g1_index]  ~ multi_normal( c(a,bGS) , Rho , sigma_g ),
    d[dyad_index]  ~ normal(0,sigma_d),
    c(theta,sigma_g,sigma_d) ~ dexp(1),
    Rho ~ lkj_corr(3)
  ) , 
  data=listerine , chains=4 , cores=4 , iter=1000, control=list(adapt_delta=0.99))
precis(m_ov_2 , depth=1)

precis(m_ov_2 , depth=3 )

set.seed(99)
m_ov_3 <- ulam(
  alist(
    overlap_uds ~ dbeta2( p , theta) ,
    logit(p) <- d[dyad_index] + g[g1_index] + g[g2_index]
    + bHR_g[g1_index]*hr_area_mean1_std + bHR_g[g2_index]*hr_area_mean2_std,
    
    c(a,bHR) ~ dnorm(0,1),
    c(g,bHR_g)[g1_index]  ~ multi_normal( c(a,bHR) , Rho , sigma_g ),
    d[dyad_index]  ~ normal(0,sigma_d),
    c(theta,sigma_g,sigma_d) ~ dexp(1),
    Rho ~ lkj_corr(3)
  ) , 
  data=listerine , chains=4 , cores=4 , iter=1000, control=list(adapt_delta=0.99) )
precis(m_ov_3 , depth=1 )
precis(m_ov_3 , depth=3 )

set.seed(44)
m_ov_4 <- ulam(
  alist(
    overlap_uds ~ dbeta2( p , theta) ,
    logit(p) <- d[dyad_index] + g[g1_index] + g[g2_index]
    + bGS_g[g1_index]*group_size1_std + bGS_g[g2_index]*group_size2_std
    + bHR_g[g1_index]*hr_area_mean1_std + bHR_g[g2_index]*hr_area_mean2_std,
    
    c(a,bGS,bHR) ~ dnorm(0,1),
    c(g,bGS_g,bHR_g)[g1_index]  ~ multi_normal( c(a,bGS,bHR) , Rho , sigma_g ),
    d[dyad_index]  ~ normal(0,sigma_d),
    c(theta,sigma_g,sigma_d) ~ dexp(1),
    Rho ~ lkj_corr(3)
  ) , 
  data=listerine , chains=4 , cores=4 , iter=1000, control=list(adapt_delta=0.99))
precis(m_ov_4 , depth=1 )
precis(m_ov_4 , depth=3 )

##########DAILY PATH LENGTH##############
#varying intercepts and slopes

# gamma
set.seed(13)
m_dpl_1_gam <- ulam(
  alist(
    dpl_mean ~ dgamma2( lambda , k) ,
    log(lambda) <- a_g[group_index] + bGS_g[group_index]*group_size_std,
    c(a_g,bGS_g)[group_index] ~ multi_normal(c(a,bGS),Rho_g,sigma_g),
    a ~ dnorm(2.302197,1),
    bGS ~ dnorm(0,1),
    Rho_g ~ dlkjcorr(4),
    c(k,sigma_g) ~ dexp(1)
  ) , data=d_dpl_gs , chains=4 , cores=4 ,control=list(adapt_delta=0.99))


trankplot(m_dpl_1_gam) #diagnostics of mixing
dev.off()
plot(precis(m_dpl_1_gam , depth=3 ))

