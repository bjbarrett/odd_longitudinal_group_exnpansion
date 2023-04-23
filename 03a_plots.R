library(RColorBrewer)
group_pal <- brewer.pal( max(d_hr_gs$group_index), "Spectral")

##########hr_area as outcome###############
plot(precis(m_gs_1 , depth=2))

# generate posterior predictions using link

#mean overall effect
plot_seq <- seq(from=min(d_hr_gs$group_size_std) , to=max(d_hr_gs$group_size_std) , length=30)
ag_z <- matrix(0,1000,11) #need to add zeros in VE to plot main effect
dpred <- list(
  group_size_std=plot_seq,
  group_index=rep(1,30)
)
link2 <- link(m_gs_1_gam, data=dpred , replace=list(group_index=ag_z) )
pred_mean <- apply(link2 , 2 , mean)

plot(hr_area_mean ~ group_size_std , data=d_hr_gs , pch=19 , col=group_pal[d_hr_gs$group_index])
lines(pred_mean ~ plot_seq , lw=2, col=1 , lty=1)

for (j in sample( c(1:1000) , 100) ){
  lines( link2[j,] ~ plot_seq , lw=1, col=col.alpha(1, alpha=0.1) , lty=1)
}

#group level effects
par(mfrow = c(3, 4) , mar=c(4,4,2,0) , oma=rep(0,4) + .2 )

for (i in 1:11){
  
  dpred <- list(
    group_size_std=plot_seq,
    group_index=rep(i,30)
  )
  
  link2 <- link(m_gs_1_gam, data=dpred )
  pred_mean <- apply(link2 , 2 , mean)
  
  plot(hr_area_mean ~ group_size_std , data=d_hr_gs[d_hr_gs$group_index==i,] , pch=19 , col=group_pal[i] ,
       xlim=range(d_hr_gs$group_size_std) , ylim=range(d_hr_gs$hr_area_mean) , main = sort(unique(d_hr_gs$group))[i] )
  lines(pred_mean ~ plot_seq , lw=2, col=group_pal[i] , lty=1)
  
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=1, col=col.alpha(group_pal[i], alpha=0.1) , lty=1)
  }
}

## daily path length as outcome -----------------------
dev.off()
plot(precis(m_dpl_1_gam , depth=2))

# generate posterior predictions using link

#mean overall effect
plot_seq <- seq(from=min(d_dpl_gs$group_size_std) , to=max(d_dpl_gs$group_size_std) , length=30)
ag_z <- matrix(0,1000,11) #need to add zeros in VE to plot main effect
dpred <- list(
  group_size_std=plot_seq,
  group_index=rep(1,30)
)
link2 <- link(m_dpl_1_gam, data=dpred , replace=list(group_index=ag_z) )
pred_mean <- apply(link2 , 2 , mean)

plot(dpl_mean ~ group_size_std , data=d_dpl_gs , pch=19 , col=group_pal[d_dpl_gs$group_index])
lines(pred_mean ~ plot_seq , lw=2, col=1 , lty=1)

for (j in sample( c(1:1000) , 100) ){
  lines( link2[j,] ~ plot_seq , lw=1, col=col.alpha(1, alpha=0.1) , lty=1)
}

#group level effects
par(mfrow = c(3, 4) , mar=c(4,4,2,0) , oma=rep(0,4) + .2 )

for (i in 1:11){
  
  dpred <- list(
    group_size_std=plot_seq,
    group_index=rep(i,30)
  )
  
  link2 <- link(m_dpl_1_gam, data=dpred )
  pred_mean <- apply(link2 , 2 , mean)
  
  plot(dpl_mean ~ group_size_std , data=d_dpl_gs[d_dpl_gs$group_index==i,] , pch=19 , col=group_pal[i] ,
       xlim=range(d_dpl_gs$group_size_std) , ylim=range(d_dpl_gs$dpl_mean) , main = sort(unique(d_dpl_gs$group))[i] )
  lines(pred_mean ~ plot_seq , lw=2, col=group_pal[i] , lty=1)
  
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=1, col=col.alpha(group_pal[i], alpha=0.1) , lty=1)
  }
}
