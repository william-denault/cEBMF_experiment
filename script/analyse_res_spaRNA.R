analyse_slice <- function(i){



  library(keras)
  library(tensorflow)
  library(comoR)
  library(flashier)
  library(ggplot2)
  library(fclust)
  library(scatterpie)
  library(gridExtra)


  sample_names=c("151507", "151508", "151509", "151510", "151669", "151670", "151671" ,"151672","151673", "151674" ,"151675" ,"151676")
  # Here we take the 9th sample as example, in total there are 12 samples (numbered as 1-12), the user can test on other samples if needed.
  clusterNum=c(7,7,7,7,5,5,5,5,7,7,7,7) # each sample has different ground truth cluster number



  load( paste0( "/home/wdenault/cEBMF_RCC_experiments/data/DLPFC/LIBD_sample",i,".RData"))
  load(paste0( "/home/wdenault/cEBMF_RCC_experiments/data/res_spatial_PCA/run_spatial_DLPFC",i,".RData"))
  tt =  as.matrix(count_sub)
  truth = KRM_manual_layers_sub$layer_guess_reordered[match(colnames(LIBD@normalized_expr),colnames(count_sub))]
  load(paste0( "/home/wdenault/cEBMF_RCC_experiments/data/res_ebmf/fit_default_",i,".RData"))

  prop <-  fit_default$L_pm/rowSums(fit_default$L_pm)

  res_ebnm <- Fclust.compare(truth, prop)


  my_col= c("#9C9EDE" ,"#5CB85C" ,"#E377C2", "#4DBBD5" ,"#FED439" ,"#FF9896", "#FFDC91")

  X=loc
  #define comoR object





  library(softImpute)
  load(paste0( "/home/wdenault/cEBMF_RCC_experiments/data/res_cebmf/fit_cebmf_",i,".RData"))
  res <-cEBMF.obj



  # colnames(tdf) <- LETTERS[1:ncol( res$loading)]
  prop <- res$loading/rowSums(res$loading)

  res_cebnm <- Fclust.compare(truth, prop)



  d0 <- data.frame(x=loc[,1], y=loc[,2])
  if (clusterNum[i]==7){
    d0$A <- ifelse(truth=="Layer1", 1, 0)
    d0$B <- ifelse(truth=="Layer2", 1, 0)
    d0$C <-  ifelse(truth=="Layer3", 1, 0)
    d0$D <-  ifelse(truth=="Layer4", 1, 0)
    d0$E <-  ifelse(truth=="Layer5", 1, 0)
    d0$F <-  ifelse(truth=="Layer6", 1, 0)
    d0$G <-  ifelse(truth=="WM", 1, 0)
    P0 <- ggplot() + geom_scatterpie(aes(x=x, y=y), data=d0, cols=c("A", "B", "C", "D", "E", "F", "G"),
                                     pie_scale=0.35, color=NA) + coord_fixed()+ theme_minimal()  +
      scale_fill_manual(values =c("#9C9EDE" ,"#5CB85C" ,"#E377C2", "#4DBBD5" ,"#FED439" ,"#FF9896", "#FFDC91"))+
      ggtitle("Ground truth")+theme( axis.text.y=element_blank(),
                                     axis.ticks.y=element_blank(),
                                     axis.text.x=element_blank(),
                                     axis.ticks.x=element_blank(),
                                     legend.position = "none")




    d <- data.frame(x=loc[,1], y=loc[,2])
    tdf =   do.call ( cbind, lapply (1:ncol(res$loading), function (i) {
      res$loading[,i]
    }))
    colnames(tdf) <- LETTERS[1:ncol(res$loading)]
    d <- cbind(d, tdf)
    d <- data.frame(d)

    P1  <- ggplot() + geom_scatterpie(aes(x=x, y=y), data=d, cols=LETTERS[1:ncol(res$loading)] ,
                                      pie_scale=0.35, color=NA) + coord_fixed()+ theme_minimal()  +
      scale_fill_manual(values =my_col[1:ncol(res$loading)])+
      ggtitle(paste0("cEBNMF, ARI score= ", round(res_cebnm[1], digits = 2) ))+theme( axis.text.y=element_blank(),
                                                                                      axis.ticks.y=element_blank(),
                                                                                      axis.text.x=element_blank(),
                                                                                      axis.ticks.x=element_blank(),
                                                                                      legend.position = "none")




    d <- data.frame(x=loc[,1], y=loc[,2])
    tdf =   do.call ( cbind, lapply (1:ncol(fit_default$L_pm), function (i) {
      fit_default$L_pm[,i]
    }))
    colnames(tdf) <- LETTERS[1:ncol(fit_default$L_pm)]
    d <- cbind(d, tdf)
    d <- data.frame(d)

    P2  <- ggplot() + geom_scatterpie(aes(x=x, y=y), data=d, cols=LETTERS[1:ncol(fit_default$L_pm)] ,
                                      pie_scale=0.35, color=NA) + coord_fixed()+ theme_minimal()  +
      scale_fill_manual(values =my_col[1:ncol(fit_default$L_pm)])+
      ggtitle(paste0("EBNMF, ARI score= ", round(res_ebnm[1], digits = 2) ))+theme( axis.text.y=element_blank(),
                                                                                    axis.ticks.y=element_blank(),
                                                                                    axis.text.x=element_blank(),
                                                                                    axis.ticks.x=element_blank(),
                                                                                    legend.position = "none")



  }else{
    d0$A <- ifelse(truth=="Layer1", 1, 0)
    d0$B <- ifelse(truth=="Layer2", 1, 0)
    d0$C <-  ifelse(truth=="Layer3", 1, 0)
    d0$D <-  ifelse(truth=="Layer4", 1, 0)
    d0$E <-  ifelse(truth=="Layer5", 1, 0)
    P0 <- ggplot() + geom_scatterpie(aes(x=x, y=y), data=d0, cols=c("A", "B", "C", "D", "E" ),
                                     pie_scale=0.35, color=NA) + coord_fixed()+ theme_minimal()  +
      scale_fill_manual(values =c("#9C9EDE" ,"#5CB85C" ,"#E377C2", "#4DBBD5" ,"#FED439"  ))+
      ggtitle("Ground truth")+theme( axis.text.y=element_blank(),
                                     axis.ticks.y=element_blank(),
                                     axis.text.x=element_blank(),
                                     axis.ticks.x=element_blank(),
                                     legend.position = "none")






    d <- data.frame(x=loc[,1], y=loc[,2])
    tdf =   do.call ( cbind, lapply (1:ncol(res$loading), function (i) {
      res$loading[,i]
    }))
    colnames(tdf) <- LETTERS[1:ncol(res$loading)]
    d <- cbind(d, tdf)
    d <- data.frame(d)

    P1  <- ggplot() + geom_scatterpie(aes(x=x, y=y), data=d, cols=LETTERS[1:ncol(res$loading)] ,
                                      pie_scale=0.35, color=NA) + coord_fixed()+ theme_minimal()  +
      scale_fill_manual(values =my_col[1:ncol(res$loading)])+
      ggtitle(paste0("cEBNMF, ARI score= ", round(res_cebnm[1], digits = 2) ))+theme( axis.text.y=element_blank(),
                                                                                      axis.ticks.y=element_blank(),
                                                                                      axis.text.x=element_blank(),
                                                                                      axis.ticks.x=element_blank(),
                                                                                      legend.position = "none")




    d <- data.frame(x=loc[,1], y=loc[,2])
    tdf =   do.call ( cbind, lapply (1:ncol(fit_default$L_pm), function (i) {
      fit_default$L_pm[,i]
    }))
    colnames(tdf) <- LETTERS[1:ncol(fit_default$L_pm)]
    d <- cbind(d, tdf)
    d <- data.frame(d)






    P2  <- ggplot() + geom_scatterpie(aes(x=x, y=y), data=d, cols=LETTERS[1:ncol(fit_default$L_pm)] ,
                                      pie_scale=0.35, color=NA) + coord_fixed()+ theme_minimal()  +
      scale_fill_manual(values =my_col[1:ncol(fit_default$L_pm)])+
      ggtitle(paste0("EBNMF, ARI score= ", round(res_ebnm[1], digits = 2) ))+theme( axis.text.y=element_blank(),
                                                                                    axis.ticks.y=element_blank(),
                                                                                    axis.text.x=element_blank(),
                                                                                    axis.ticks.x=element_blank(),
                                                                                    legend.position = "none")



  }




  res <- list(res_cebnm=res_cebnm,res_ebnm =res_ebnm)

  save(res, file=paste0("/home/wdenault/cEBMF_RCC_experiments/res_ARI_score/slice_",i,".RData"))
  P_out <- grid.arrange(P0  ,P1 ,P2, ncol=3)
  ggsave(P_out,file= paste0("/home/wdenault/cEBMF_RCC_experiments/plot/plot_slice_",i,".pdf"),
         width =29.7  ,
         height =  13,
         units = "cm"
  )



}
