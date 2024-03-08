load("~/simu_cEBMF/sim/cash_sim_1.RData")
res_sum <-  data.frame(  do.call(rbind,lapply( 1:length(res), function(i) as.numeric(res[[i]][[1]][-c(17)] ))))


colnames (res_sum)  <- names(res[[1]][[1]][-c(17)])

res_sum2 <-  data.frame(  do.call(rbind,lapply( 1:length(res), function(i)  (res[[i]][[1]][ c(17)] ))))
colnames (res_sum2)  <- names(res[[1]][[1]][ c(17)])

res_sum <- cbind( res_sum,res_sum2)
 t_res <- res_sum
for ( l in 2:7 ){
  load(paste("~/simu_cEBMF/sim/cash_sim_",l,".RData", sep=""))
  res_sum <-  data.frame(  do.call(rbind,lapply( 1:length(res), function(i) as.numeric(res[[i]][[1]][-c(17)] ))))


  colnames (res_sum)  <- names(res[[1]][[1]][-c(17)])

  res_sum2 <-  data.frame(  do.call(rbind,lapply( 1:length(res), function(i)  (res[[i]][[1]][ c(17)] ))))
  colnames (res_sum2)  <- names(res[[1]][[1]][ c(17)])

  res_sum <- cbind( res_sum,res_sum2)
  t_res <- rbind(t_res,
                 res_sum )
}


library(ggplot2)

P_rmse <-  ggplot( t_res[  -which(t_res$dist=="normal"),], aes(x=rmse_ash, y=rmse_mco))+
   geom_point( alpha=0.2)+
   geom_abline(intercept=0,slope=1)+
  theme_minimal()+
  xlab("RMSE ash")+
  ylab("RMSE cash")+
   facet_wrap(.~as.factor(dist), nrow=1 )+
  theme(strip.background = element_blank(), strip.text = element_blank())
P_rmse
 ggplot( t_res, aes(x=power_ash, y=power_mco))+
   geom_point( alpha=0.2)+
   geom_abline(intercept=0,slope=1)+
   theme_minimal()+
   facet_wrap(.~as.factor(dist), ncol=1 )

 P_T1 <-  ggplot( t_res, aes(x=T1_ash, y=T1_mco))+
   geom_point( alpha=0.2)+
   geom_abline(intercept=0,slope=1)+
   geom_abline(intercept=0.5,slope=0)+
   geom_hline(yintercept = 0.05)+
   theme_minimal()+
   facet_wrap(.~as.factor(dist) , nrow=1 )

 P_T1






 ### logistic susie part



 tt_df <- list()


 h <- 1
 for ( l in 1:7 ){


   load(paste("~/simu_cEBMF/sim/cash_sim_",l,".RData", sep=""))
   dist <- res[[1]][[1]][ c(17)]
   for ( k in 1: length(res)){


     L          <- as.numeric(res[[k]][[1]][18])
     df_bf      <- res[[k]][[2]]
     tt_df[[h]] <- cbind(df_bf,
                         rep(L ,nrow(df_bf)),
                         rep( dist ,nrow(df_bf))
     )

     h <- h+1
   }
 }


 final_df <- do.call( rbind, tt_df)
  final_df <- final_df[which(final_df$purity <0.5),]# remove low purity cs
 colnames(final_df)[c(6,7)] <- c("L","dist")
 final_df <- as.data.frame(final_df)
 final_df$dist
 library(dplyr)
 plot_df <- final_df%>%
   group_by(L, dist)%>%
   summarize(power=mean( n_effect/( L)),
             t1=mean( nfalse_effect/( n_effect+nfalse_effect) ) ,
             n=n(),
             sd_cov =  sqrt( mean( nfalse_effect/( n_effect+nfalse_effect) )* (1-  mean( nfalse_effect/( n_effect+nfalse_effect) ))/n()),
             sd_power =  sqrt(mean( n_effect/( L))*(1- mean( n_effect/( L))) /n()) )


 ggplot(plot_df, aes(x=L, y=power))+
   geom_point()+

   geom_hline(yintercept = 1)+
   geom_errorbar(aes(ymin=power-2*sd_power , ymax=power+2*sd_power), width=.2,
                 position=position_dodge(.9))+
   facet_wrap(.~dist, ncol=4)

 ggplot(plot_df, aes(x=L, y=1- t1))+
   geom_point()+
   ylim(c(0,1.2))+
   geom_hline(yintercept = 0.95)+

   geom_errorbar(aes(ymin=1- t1-2*sd_cov , ymax=1- t1+2*sd_cov), width=.2,
                 position=position_dodge(.9))+
   facet_wrap(.~dist, ncol=4)



 ### Density plot  ----

 samp_simu <- function(dist, N=200000){
   betatrue <- c()
   if( dist=="spiky"){

     samp_fun <- function(){
       id <- sample(size=1, 1:4)
       if(id==1){
         out <-  rnorm( 1, sd=sqrt(2)*0.25)
       }
       if(id==2){
         out <-  rnorm( 1, sd=sqrt(2)*0.5)
       }
       if(id==3){
         out <-  rnorm( 1, sd=sqrt(2)*1)
       }
       if(id==4){
         out <-  rnorm( 1, sd=sqrt(2)*2)
       }
       return( out)
     }
     for ( i in 1:N){

       betatrue <- c( betatrue,  samp_fun())
     }

   }

   if( dist=="near_normal"){
     samp_fun <- function(){
       id <- sample(size=1, 1:2, prob=c(2/3,1/3))
       if(id==1){
         out <-  rnorm( 1, sd=sqrt(2)*1)
       }
       if(id==2){
         out <-  rnorm( 1, sd=sqrt(2)*2)
       }

       return( out)
     }

     for ( i in 1:N){

       betatrue <- c( betatrue,  samp_fun())
     }

   }

   if( dist=="normal"){
     for ( i in 1:N){

       betatrue <- c( betatrue,  rnorm(1,sd=sqrt(2)*1))

     }

   }

   if( dist=="flattop"){

     samp_fun <- function(){
       id <- sample(size=1, 1:7 )
       if(id==1){
         out <-  rnorm( 1,mean=-1.5, sd=5)
       }
       if(id==2){
         out <-  rnorm( 1,mean=-1, sd=5)
       }
       if(id==3){
         out <-  rnorm( 1,  mean=-.5, sd=5)
       }
       if(id==4){
         out <-  rnorm( 1,   sd=5)
       }
       if(id==5){
         out <-  rnorm( 1,  mean= .5, sd=5)
       }
       if(id==6){
         out <-  rnorm( 1, mean= 1, sd=5)
       }
       if(id==7){
         out <-  rnorm( 1,  mean= 1.5, sd=5)
       }
       return( out)
     }

     for ( i in 1:N){

       betatrue <- c( betatrue,  samp_fun())

     }


   }

   if( dist=="skew"){



     samp_fun <- function(){
       id <- sample(size=1, 1:4, prob = c(1/4,1/4,1/3,1/6) )
       if(id==1){
         out <-  rnorm( 1,mean=-2, sd=sqrt(2)*2)
       }
       if(id==2){
         out <-  rnorm( 1,mean=-1, sd=sqrt(2)*1.5)
       }
       if(id==3){
         out <-  rnorm( 1,  mean=0, sd=sqrt(2)*1)
       }
       if(id==4){
         out <-  rnorm( 1,mean=1,   sd=sqrt(2)*1)
       }

       return( out)
     }
     for ( i in 1:N){
       betatrue <- c( betatrue,  samp_fun())
     }


   }

   if( dist=="big-normal"){
     for ( i in 1:N){
       betatrue <- c( betatrue, rnorm(1,sd=4))
     }


   }
   if( dist=="bimodal"){
     samp_fun <- function(){
       id <- sample(size=1 ,1:2   )
       if(id==1){
         out <-  rnorm( 1,mean=-2, sd=1)
       }
       if(id==2){
         out <-  rnorm( 1,mean=2, sd=1)
       }


       return( out)
     }


     for ( i in 1:N){

       betatrue <- c( betatrue, samp_fun())
     }



   }


   return(data.frame(x= betatrue,dist=rep( dist, length(betatrue))))
 }


 tt <- rbind( samp_simu(dist="spiky"),
              samp_simu(dist="near_normal"),
              samp_simu(dist="flattop"),
              samp_simu(dist="skew"),
              samp_simu(dist="big-normal"),
              samp_simu(dist="bimodal")


 )


 tt <-  data.frame(tt)
 head(tt)
 library(ggplot2)
P_dens <-  ggplot( tt, aes(x) )+
   xlim(-10,10)+
   facet_wrap(.~dist , nrow=1)+
   theme_minimal()+
   geom_density(size=1.5
   )

library(gridExtra)
grid.arrange(P_dens, P_rmse  , nrow=2)
