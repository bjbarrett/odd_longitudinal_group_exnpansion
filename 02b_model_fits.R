library(rethinking)
library(cmdstanr)
library(janitor)
library(RColorBrewer)
library(dplyr)

options(mc.cores = parallel::detectCores())
group_pal <- brewer.pal( max(d_hr_gs$group_index), "Spectral")

#varying intecepts and slopes
m_gs_1 <- ulam(
  alist(
    hr_area_mean ~ dnorm( mu , sigma) ,
    mu <- a + a_g[group_index] + (bGS + bGS_g[group_index])*group_size_std,
    c(a_g,bGS_g)[group_index] ~ multi_normal(0,Rho_g,sigma_g),
    a ~ dnorm(2.392861,1),
    bGS ~ dnorm(0,1),
    Rho_g ~ dlkjcorr(4),
    c(sigma,sigma_g) ~ dexp(1)
  ) , data=d_hr_gs , chains=4 , cores=4 )

trankplot(m_gs_1) #diagnostics of mixing
precis(m_gs_1 , depth=3)
plot(precis(m_gs_1 , depth=2))
plot(hr_area_mean ~ group_size_std , data=d_hr_gs , pch=19 , col=group_pal[d_hr_gs$group_index])

#hro

listerine <- list(
 overlap_uds=d_hr_ov$overlap_uds ,
 dyad_index=d_hr_ov$dyad_index,
 g1_index=d_hr_ov$g1_index,
 g2_index=d_hr_ov$g2_index
)

m_ov_1 <- ulam(
  alist(
   overlap_uds ~ dnorm( mu , sigma) ,
    mu <- a + d[dyad_index] + g[g1_index] + g[g2_index],
    a ~ dnorm(0.1671235,1),
    g[g1_index]  ~ normal(0,sigma_g),
    d[dyad_index]  ~ normal(0,sigma_d),
    c(sigma,sigma_g,sigma_d) ~ dexp(1)
  ) , 
  data=listerine , chains=4 , cores=4 )

precis(m_ov_1 , depth=2)
stancode(m_hr_1)





