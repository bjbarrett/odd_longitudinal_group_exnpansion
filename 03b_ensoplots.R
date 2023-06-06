library(RColorBrewer)
group.pal <- brewer.pal( max(d_hr_gs$group_index), "Spectral")
######################################fit_hr
post <- extract.samples(fit_hr) #different if fit
par(mfrow = c(6, 4))

for(i in 1:24){
  dens(post$am_pred[, i ] )
  dens(post$am[, i ] , lty=2 , add=TRUE)
  points(list_area_2$mei[list_area_2$year_index_mei==i] , rep(0,12), col="red")
}

######plot mean effect across all groups######
par(mfrow = c(3, 4))
par(cex = 0.6)
par(mar = c(3, 3, 0, 0), oma = c(1, 1, 1, 1))

plot(list_area_2$mean_annual_mei,list_area_2$hr_area_mean , ylab="home range area (ha)" ,
     xlab="monthly ENSO index" , col=group.pal[list_area_2$group_index], xlim=c(min(list_area_2$mei),max(list_area_2$mei)) , ylim=c(0,9) )
title("overall posterior mean ", line = -1)
for (obs in 1:list_area_2$N){
  points( post$am_pred[1:200, list_area_2$year_index[obs] ] ,
          rep(list_area_2$hr_area_mean[obs] , 200 ) , col=col.alpha(group.pal[list_area_2$group_index[obs]]), cex=.4)
}

for(i in 1:nrow(d_hr_gs_3)){
  points(list_area_2$mei[list_area_2$year_index[i] == list_area_2$year_index_mei] , 
         rep(list_area_2$hr_area_mean[i] , 12 ) ,
         col=col.alpha(group.pal[list_area_2$group_index[i] ]) )
}

seq.mei <- seq(from=min(list_area_2$mei), to=max(list_area_2$mei) , length=30)
lambda.link <- function(x) exp(post$v_mu[,1] +  post$v_mu[,2] * x)
lambda <- sapply( seq.mei ,lambda.link )
lambda.mean <- apply( lambda , 2 , mean )
lambda.PI <- apply( lambda , 2 , PI , prob=0.89 )
for (i in 1:100) {
  lines(seq.mei , lambda[i,] , col=col.alpha(alpha=0.1 ,"black"))
}
lines(seq.mei , lambda.mean, col=1)
lines(seq.mei , lambda.PI[1,], col=1 , lty=3)
lines(seq.mei , lambda.PI[2,], col=1 , lty=3)

#####group specific effects
for(g in 1:max(list_area_2$group_index)){
  plot(list_area_2$mean_annual_mei[list_area_2$group_index==g],
       list_area_2$hr_area_mean[list_area_2$group_index==g] , 
       ylab="home range area (ha)" , ylim=c(0,9), 
       xlab="monthly ENSO index" , col=group.pal[g], 
       xlim=c(min(list_area_2$mei),max(list_area_2$mei)))
  title(min(d_hr_gs_3$group[d_hr_gs_3$group_index==g]), line = -1)
  
  
  for (obs in which(list_area_2$group_index==g) ){
    points( post$am_pred[1:200, list_area_2$year_index[obs] ] ,
            rep(list_area_2$hr_area_mean[obs] , 200 ) , col=col.alpha(group.pal[g]), cex=.4)
  }
  
  for(i in which(d_hr_gs_3$group_index==g)){
    points(list_area_2$mei[list_area_2$year_index[i] == list_area_2$year_index_mei] ,
           rep(list_area_2$hr_area_mean[i] , 12 ) ,
           col=col.alpha(alpha=0.25 , group.pal[g]) )
  }


  
  seq.mei <- seq(from=min(list_area_2$mei), to=max(list_area_2$mei) , length=30)
  lambda.link <- function(x) exp(post$v_mu[,1] + post$v[,g,1] + (post$v_mu[,2] + post$v[,g,2] )* x )
  lambda <- sapply( seq.mei ,lambda.link )
  lambda.mean <- apply( lambda , 2 , mean )
  lambda.PI <- apply( lambda , 2 , PI , prob=0.89 )
  for (i in 1:100) {
    lines(seq.mei , lambda[i,] , col=col.alpha(group.pal[g]))
  }
  lines(seq.mei , lambda.mean, col=1)
  lines(seq.mei , lambda.PI[1,], col=1 , lty=3)
  lines(seq.mei , lambda.PI[2,], col=1 , lty=3)
}

#########fit_hr_gs#########
post <- extract.samples(fit_hr_gs) #different if fit
par(mfrow = c(6, 4))

for(i in 1:24){
  dens(post$am_pred[, i ] )
  dens(post$am[, i ] , lty=2 , add=TRUE)
  points(list_area_2$mei[list_area_2$year_index_mei==i] , rep(0,12), col="red")
}

######plot mean effect across all groups######
par(mfrow = c(3, 4))
par(cex = 0.6)
par(mar = c(3, 3, 0, 0), oma = c(1, 1, 1, 1))

plot(list_area_2$mean_annual_mei,list_area_2$hr_area_mean , ylab="home range area (ha)" ,
     xlab="monthly ENSO index" , col=group.pal[list_area_2$group_index], xlim=c(min(list_area_2$mei),max(list_area_2$mei)) , ylim=c(0,9) )
title("overall posterior mean ", line = -1)
for (obs in 1:list_area_2$N){
  points( post$am_pred[1:200, list_area_2$year_index[obs] ] ,
          rep(list_area_2$hr_area_mean[obs] , 200 ) , col=col.alpha(group.pal[list_area_2$group_index[obs]]), cex=.4)
}

for(i in 1:nrow(d_hr_gs_3)){
  points(list_area_2$mei[list_area_2$year_index[i] == list_area_2$year_index_mei] , 
         rep(list_area_2$hr_area_mean[i] , 12 ) ,
         col=col.alpha(group.pal[list_area_2$group_index[i] ]) )
}

seq.mei <- seq(from=min(list_area_2$mei), to=max(list_area_2$mei) , length=30)
lambda.link <- function(x) exp(post$v_mu[,1] +  post$v_mu[,2] * x +  post$v_mu[,3] *0)
lambda <- sapply( seq.mei ,lambda.link )
lambda.mean <- apply( lambda , 2 , mean )
lambda.PI <- apply( lambda , 2 , PI , prob=0.89 )
for (i in 1:100) {
  lines(seq.mei , lambda[i,] , col=col.alpha(alpha=0.1 ,"black"))
}
lines(seq.mei , lambda.mean, col=1)
lines(seq.mei , lambda.PI[1,], col=1 , lty=3)
lines(seq.mei , lambda.PI[2,], col=1 , lty=3)

#####group specific effects
for(g in 1:max(list_area_2$group_index)){
  plot(list_area_2$mean_annual_mei[list_area_2$group_index==g],
       list_area_2$hr_area_mean[list_area_2$group_index==g] , 
       ylab="home range area (ha)" , ylim=c(0,9), 
       xlab="monthly ENSO index" , col=group.pal[g], 
       xlim=c(min(list_area_2$mei),max(list_area_2$mei)))
  title(min(d_hr_gs_3$group[d_hr_gs_3$group_index==g]), line = -1)
  
  
  for (obs in which(list_area_2$group_index==g) ){
    points( post$am_pred[1:200, list_area_2$year_index[obs] ] ,
            rep(list_area_2$hr_area_mean[obs] , 200 ) , col=col.alpha(group.pal[g]), cex=.4)
  }
  
  for(i in which(d_hr_gs_3$group_index==g)){
    points(list_area_2$mei[list_area_2$year_index[i] == list_area_2$year_index_mei] ,
           rep(list_area_2$hr_area_mean[i] , 12 ) ,
           col=col.alpha(alpha=0.25 , group.pal[g]) )
  }
  
  
  
  seq.mei <- seq(from=min(list_area_2$mei), to=max(list_area_2$mei) , length=30)
  lambda.link <- function(x) exp(post$v_mu[,1] + post$v[,g,1] + (post$v_mu[,2] + post$v[,g,2] )* x + 0*(post$v_mu[,3] + post$v[,g,3] ))
  lambda <- sapply( seq.mei ,lambda.link )
  lambda.mean <- apply( lambda , 2 , mean )
  lambda.PI <- apply( lambda , 2 , PI , prob=0.89 )
  for (i in 1:100) {
    lines(seq.mei , lambda[i,] , col=col.alpha(group.pal[g]))
  }
  lines(seq.mei , lambda.mean, col=1)
  lines(seq.mei , lambda.PI[1,], col=1 , lty=3)
  lines(seq.mei , lambda.PI[2,], col=1 , lty=3)
}

