library(RColorBrewer)
group_pal <- brewer.pal( max(d_hr_gs$group_index), "Spectral")
post <- extract.samples(fit_hr_gs) #different if fit

for(i in 1:24){
  dens(post$am_pred[, i ] )
  dens(post$am[, i ] , lty=2 , add=TRUE)
  points(list_area_2$mei[list_area_2$year_index_mei==i] , rep(0,12), col="red")
}

plot(list_area_2$mean_annual_mei,list_area_2$hr_area_mean , xlab="home range area (ha)" ,
     ylab="monthly ENSO index" , col=group_pal[list_area_2$group_index], xlim=c(-3,3))

# for (obs in 1:nrow(d_hr_gs_3)){
#   points( post$mei_dist[1:100, list_area_2$year_index[obs] ] , 
#          rep(list_area_2$hr_area_mean[obs] , 100 ) , col=col.alpha(group_pal[list_area_2$group_index[obs]]), cex=.4)
# }

for (obs in 1:list_area_2$N){
  points( post$am_pred[1:200, list_area_2$year_index[obs] ] ,
          rep(list_area_2$hr_area_mean[obs] , 200 ) , col=col.alpha(group_pal[list_area_2$group_index[obs]]), cex=.4)
}

for(i in 1:nrow(d_hr_gs_3)){
  points(list_area_2$mei[list_area_2$year_index[i] == list_area_2$year_index_mei] , 
         rep(list_area_2$hr_area_mean[i] , 12 ) ,
         col=col.alpha(group_pal[list_area_2$group_index[i] ]) )
}




