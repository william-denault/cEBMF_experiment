i=11
#analyse_slice <- function(i){


for ( i in c(1:2,4,6:7,9:12)){#batch size 1500 1500 for the 2 first one
  #1700 for the third one
  # 3 and 5 are problematic
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



  load( paste0( "C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/data/DLPFC/LIBD_sample",i,".RData"))
  load(paste0( "C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/data/res_spatial_PCA/run_spatial_DLPFC",i,".RData"))
  tt =  as.matrix(count_sub)
  truth = KRM_manual_layers_sub$layer_guess_reordered[match(colnames(LIBD@normalized_expr),colnames(count_sub))]

  loc =  LIBD @location
  tt0 = ( t(as.matrix(LIBD@normalized_expr)) )



  loc =  LIBD @location
  tt0 = ( t(as.matrix(LIBD@normalized_expr)) )



  X=loc
  #define comoR object



  library(softImpute)
  l2_reg=0.1
  Y <- t(t(tt0) - apply(tt0,2,min))
  X_l =X

  X_f =matrix(rnorm(2* ncol(tt0), sd=3), nrow = ncol(tt0))

  param_nnet.x =keras_model_sequential() %>%
    layer_dense(units = 64,
                activation = 'relu',
                input_shape = c(ncol(X_l))) %>%
    layer_dense(units = 64,
                activation = 'relu',
                kernel_regularizer = regularizer_l2(l2_reg)) %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 64,
                activation = 'relu',
                kernel_regularizer = regularizer_l2(l2_reg)) %>%
    layer_dense(units = 10,
                activation = 'softmax')


  param_nnet.y =keras_model_sequential() %>%
    layer_dense(units = 64,
                activation = 'relu',
                input_shape = c(ncol(X_f))) %>%
    layer_dense(units = 10,
                activation = 'softmax')

  mnreg_type="keras"
  K=3
  type_noise='column_wise'
  init_type="flashier_NMF"
  maxit=5
  tol=1e-3

  param_como2 = list()
  param_susie =  list(L=5)
  maxit_como  = 2

  param_como.x  = list(max_class=10,mnreg_type="keras",
                       prior="mix_exp" ,
                       epoch     =150,
                       batch_size= 1500)
  param_como.y  = list(max_class=10,mnreg_type="constant_mnreg",
                       prior="mix_exp"  )
  tt0 = ( t(as.matrix(LIBD@normalized_expr)) )
  cEBMF.obj <- comoR:::init_cEBMF (tt0,
                                   X_l,
                                   X_f,
                                   mnreg_type.x="keras",
                                   mnreg_type.y="constant_mnreg",
                                   K=clusterNum[i],
                                   type_noise    = type_noise,
                                   init_type     = init_type,
                                   param_como.x  =  param_como.x,
                                   param_como.y  =  param_como.y,
                                   maxit_como    = 1,
                                   param_nnet.x  = param_nnet.x,
                                   param_como2   = param_como2,
                                   param_susie   = param_susie )


  for(k in 1:clusterNum[i]){

    cEBMF.obj$factor[,k] <- apply(tt0[which(truth==unique(truth)[k]),],2,mean)
  }

  fit_flash <- flash_init(tt0, var_type = 2, S=0.01)
  fit_flash <- flash_factors_init(fit_flash,
                                  list(abs(cEBMF.obj$loading),abs(cEBMF.obj$factor)),
                                  ebnm_fn = c(ebnm_point_exponential, ebnm_point_exponential))
  fit_flash <- flash_backfit(fit_flash)
  fit_default<-fit_flash

  library(NNLM)
  # NMF
  #
  fit_nmf <- nnmf(tt0,k = clusterNum[i],method = "scd",loss = "mse",verbose = 0,
                  n.threads = 2,rel.tol = 1e-8,max.iter = 100)


  hist(cEBMF.obj$factor)
  for (o in 1:6) {#5 is good
    cEBMF.obj <- comoR:::cEBMF_iter  (cEBMF.obj)
    cEBMF.obj <- comoR:::out_prep.cEBMF(cEBMF.obj)
    # save(cEBMF.obj, file=paste0( "C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/data/res_cebmf/fit_cebmf_",i,".RData"))
  }




  fit_nmf$W<- fit_nmf$W+1e-8

  W <- fit_nmf$W+1e-8
  W <- W / rowSums(W)


  n_truth = as.numeric(as.factor(truth))
  h=1
  for (o in 1:length(unique(n_truth))){
    n_truth[which(n_truth==unique(n_truth)[o])]=h
    h=h+1

  }
  if (length(which(is.na(n_truth)))>0){
    res_NMF <- Fclust.compare( n_truth[-which(is.na(truth))],
                               W[-which(is.na(truth)),])
  }  else{

    res_NMF <- Fclust.compare(n_truth, W)
  }




  library(keras)
  library(tensorflow)
  library(comoR)
  library(flashier)
  library(ggplot2)
  library(fclust)
  library(scatterpie)
  library(gridExtra)
  fit_default$L_pm <- fit_default$L_pm+1e-8
  fit_default$L_pm <- abs(fit_default$L_pm+1e-8)

  W <- fit_default$L_pm+1e-8
  W <- W / rowSums(W)


  prop <- abs(W)

  if (length(which(is.na(n_truth)))>0){
    res_ebnm <- Fclust.compare( n_truth[-which(is.na(truth))],
                                prop[-which(is.na(truth)),])
  }  else{

    res_ebnm <- Fclust.compare(n_truth,   prop)
  }



  my_col= c("#9C9EDE" ,"#5CB85C" ,"#E377C2", "#4DBBD5" ,"#FED439" ,"#FF9896", "#FFDC91")

  X=loc
  #define comoR object





  res <-cEBMF.obj



  # colnames(tdf) <- LETTERS[1:ncol( res$loading)]
  prop <- res$loading/rowSums(res$loading)

  if (length(which(is.na(n_truth)))>0){
    res_cebnm <- Fclust.compare( n_truth[-which(is.na(truth))],
                                 prop[-which(is.na(truth)),])
  }  else{

    res_cebnm <- Fclust.compare(n_truth,   prop)
  }


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

    d <- data.frame(x=loc[,1], y=loc[,2])

    W <- fit_nmf$W
    #W <- W / rowSums(W)

    tdf2 =   do.call ( cbind, lapply (1:ncol(W ), function (i) {
      W[,i]
    }))
    colnames(tdf2) <- LETTERS[1:ncol(W)]
    d <- cbind(d, tdf2)
    d <- data.frame(d)

    P3  <- ggplot() + geom_scatterpie(aes(x=x, y=y), data=d, cols=LETTERS[1:ncol(W)] ,
                                      pie_scale=0.35, color=NA) + coord_fixed()+ theme_minimal()  +
      scale_fill_manual(values =my_col[1:ncol(W)])+
      ggtitle(paste0("NMF, ARI score= ", round(res_NMF[1], digits = 2) ))+theme( axis.text.y=element_blank(),
                                                                                 axis.ticks.y=element_blank(),
                                                                                 axis.text.x=element_blank(),
                                                                                 axis.ticks.x=element_blank(),
                                                                                 legend.position = "none")

  }else{
    d0 <- data.frame(x=loc[,1], y=loc[,2])
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

    d <- data.frame(x=loc[,1], y=loc[,2])

    W <- fit_nmf$W
    #W <- W / rowSums(W)

    tdf2 =   do.call ( cbind, lapply (1:ncol(W ), function (i) {
      W[,i]
    }))
    colnames(tdf2) <- LETTERS[1:ncol(W)]
    d <- cbind(d, tdf2)
    d <- data.frame(d)

    P3  <- ggplot() + geom_scatterpie(aes(x=x, y=y), data=d, cols=LETTERS[1:ncol(W)] ,
                                      pie_scale=0.35, color=NA) + coord_fixed()+ theme_minimal()  +
      scale_fill_manual(values =my_col[1:ncol(W)])+
      ggtitle(paste0("NMF, ARI score= ", round(res_NMF[1], digits = 2) ))+theme( axis.text.y=element_blank(),
                                                                                 axis.ticks.y=element_blank(),
                                                                                 axis.text.x=element_blank(),
                                                                                 axis.ticks.x=element_blank(),
                                                                                 legend.position = "none")


  }


  res <- list(res_cebnm=res_cebnm,res_ebnm =res_ebnm,
              res_NMF=res_NMF)
  P_out <- grid.arrange(P0  ,P1 ,P2,P3, ncol=4)
  ggsave(P_out,file= paste0("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/plot/plot_slice_",i,".pdf"),
         width =29.7  ,
         height =  10,
         units = "cm"
  )
  save(res        , file=paste0( "C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/res_ARI_score/res_",i,".RData"))
  save(cEBMF.obj  , file=paste0( "C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/data/res_cebmf/fit_cebmf_" ,i,".RData"))
  save(fit_default, file=paste0( "C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/data/res_ebmf/fit_default_",i,".RData"))
  save(fit_nmf    , file=paste0( "C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/data/res_nmf/fit_nmf_"     ,i,".RData"))

}

