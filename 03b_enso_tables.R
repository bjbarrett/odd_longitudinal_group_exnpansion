library(xtable)
## fit_gs
xx <- precis(fit_hr , depth=2 , pars=c("v_mu", "sigma_g" , "k" , "sigma"))
xx@row.names
xx@row.names <- c("alpha" , "beta_{MEI}" , "sigma_{alpha}" , 
                  "sigma_{beta_{MEI}}"  , "k" , "sigma_{MEI_obs}")
print(xtable(xx) )
## fit_hr_gs
xx <- precis(fit_hr_gs , depth=2 , pars=c("v_mu", "sigma_g" , "k" , "sigma"))
xx@row.names
xx@row.names <- c("alpha" , "beta_{MEI}" , "beta_{GS}" , "sigma_{alpha}" , 
                  "sigma_{beta_{MEI}}" , "sigma_{beta_{GS}}" , "k" , "sigma_{MEI_obs}")

