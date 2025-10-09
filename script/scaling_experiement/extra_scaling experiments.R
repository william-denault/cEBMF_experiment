
library(flashier)
library(mfair)
library(SpatialPCA)
scaling_exp_no_spa <-  function(noise_level=1,seed=1, nrow=1000){

  max_class=10
  max_iter_como=10
  set.seed(seed)
  x <-runif(nrow)
  y <-runif(nrow)
  X = cbind(x,y)
  max_iter_cEBMF=10

  #problem fro set.seed(1)
  f <- matrix(NA, nrow = 3, ncol =200)
  for ( i in 1:ncol (f)){

    t1<- sample (c(0,1), size=1)
    t2<- sample (c(0,1), size=1)

    f[1,i] <- t1*rnorm(n=1)
    f[2,i] <- t2*rnorm(n=1)

    f[3,i] <- t2*rnorm(n=1)

  }
  L <- matrix(NA, ncol=3, nrow=length(x))

  factor  <- c()

  for (i in 1:length(x)){

    if ( (x[i] <.33 & y[i] <.33 )|(x[i] >.33 & y[i] >.33 &  x[i] <.66 & y[i] <.66) | (x[i] >.66 & y[i] >.66 )){
      L[i,] <- c(1,0,0)
      factor=c(factor,1)
    }else{
      if ( (x[i] <.33 & y[i] >.66 )|(x[i] >.33 & y[i] <.33 &  x[i] <.66  ) | (x[i] >.66 & y[i] >.33  & y[i] <.66)){
        L[i,] <- c(0,1,0)
        factor=c(factor,2)
      }else{
        L[i,] <- c(0,0,1)
        factor=c(factor,3)
      }
    }


  }

  df = data.frame(x=x,y=y, Factor=as.factor(factor))



  Z = L%*%f + matrix(rnorm(nrow(L)* ncol(f), sd=noise_level), nrow = nrow(L))

  X_l =X
  X_f = matrix(rnorm(2* ncol(Z)), ncol = 2)



  pt=proc.time()
  fit_default <- flash(Z, greedy_Kmax = 3)
  tflash = proc.time()-pt






  pt=proc.time()
  mfairObject <- createMFAIR(Z, as.data.frame(X), K_max = 5)

  mfairObject <- fitGreedy(mfairObject, sf_para = list(verbose_loop = FALSE))
  mfairObject <- fitBack(mfairObject,
                         verbose_bf_inner = FALSE,
                         sf_para = list(verbose_sf = FALSE, verbose_loop = FALSE)
  )
  tmfai = proc.time()-pt



  return(out =list( tflash = tflash,

                    tmfai = tmfai))

}




library(flashier)
library(mfair)
library(SpatialPCA)
scaling_exp <-  function(noise_level=1,seed=1, nrow=1000){

  max_class=10
  max_iter_como=10
  set.seed(seed)
  x <-runif(nrow)
  y <-runif(nrow)
  X = cbind(x,y)
  max_iter_cEBMF=10

  #problem fro set.seed(1)
  f <- matrix(NA, nrow = 3, ncol =200)
  for ( i in 1:ncol (f)){

    t1<- sample (c(0,1), size=1)
    t2<- sample (c(0,1), size=1)

    f[1,i] <- t1*rnorm(n=1)
    f[2,i] <- t2*rnorm(n=1)

    f[3,i] <- t2*rnorm(n=1)

  }
  L <- matrix(NA, ncol=3, nrow=length(x))

  factor  <- c()

  for (i in 1:length(x)){

    if ( (x[i] <.33 & y[i] <.33 )|(x[i] >.33 & y[i] >.33 &  x[i] <.66 & y[i] <.66) | (x[i] >.66 & y[i] >.66 )){
      L[i,] <- c(1,0,0)
      factor=c(factor,1)
    }else{
      if ( (x[i] <.33 & y[i] >.66 )|(x[i] >.33 & y[i] <.33 &  x[i] <.66  ) | (x[i] >.66 & y[i] >.33  & y[i] <.66)){
        L[i,] <- c(0,1,0)
        factor=c(factor,2)
      }else{
        L[i,] <- c(0,0,1)
        factor=c(factor,3)
      }
    }


  }

  df = data.frame(x=x,y=y, Factor=as.factor(factor))



  Z = L%*%f + matrix(rnorm(nrow(L)* ncol(f), sd=noise_level), nrow = nrow(L))

  X_l =X
  X_f = matrix(rnorm(2* ncol(Z)), ncol = 2)



  pt=proc.time()
  fit_default <- flash(Z, greedy_Kmax = 3)
  tflash = proc.time()-pt






  #load a spatial data
  load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_submission/cEBMF_RCC_experiments/data/res_spatial_PCA/run_spatial_DLPFC1.RData")

  LIBD @normalized_expr =t(Z)
  LIBD @location =X
  LIBD = SpatialPCA_buildKernel(LIBD, kerneltype="gaussian", bandwidthtype="SJ",bandwidth.set.by.user=NULL)
  pt=proc.time()
  LIBD = SpatialPCA_EstimateLoading(LIBD,fast=FALSE,SpatialPCnum=3)
  tspa = proc.time()-pt

  pt=proc.time()
  mfairObject <- createMFAIR(Z, as.data.frame(X), K_max = 5)

  mfairObject <- fitGreedy(mfairObject, sf_para = list(verbose_loop = FALSE))
 # mfairObject <- fitBack(mfairObject,
 #                        verbose_bf_inner = FALSE,
 #                        sf_para = list(verbose_sf = FALSE, verbose_loop = FALSE)
  #)
  tmfai = proc.time()-pt



  return(out =list( tflash = tflash,
                    tspa =  tspa ,
                    tmfai = tmfai))

}




library(flashier)
library(mfair)
library(SpatialPCA)
scaling_exp_flash <-  function(noise_level=1,seed=1, nrow=1000){

  max_class=10
  max_iter_como=10
  set.seed(seed)
  x <-runif(nrow)
  y <-runif(nrow)
  X = cbind(x,y)
  max_iter_cEBMF=10

  #problem fro set.seed(1)
  f <- matrix(NA, nrow = 3, ncol =200)
  for ( i in 1:ncol (f)){

    t1<- sample (c(0,1), size=1)
    t2<- sample (c(0,1), size=1)

    f[1,i] <- t1*rnorm(n=1)
    f[2,i] <- t2*rnorm(n=1)

    f[3,i] <- t2*rnorm(n=1)

  }
  L <- matrix(NA, ncol=3, nrow=length(x))

  factor  <- c()

  for (i in 1:length(x)){

    if ( (x[i] <.33 & y[i] <.33 )|(x[i] >.33 & y[i] >.33 &  x[i] <.66 & y[i] <.66) | (x[i] >.66 & y[i] >.66 )){
      L[i,] <- c(1,0,0)
      factor=c(factor,1)
    }else{
      if ( (x[i] <.33 & y[i] >.66 )|(x[i] >.33 & y[i] <.33 &  x[i] <.66  ) | (x[i] >.66 & y[i] >.33  & y[i] <.66)){
        L[i,] <- c(0,1,0)
        factor=c(factor,2)
      }else{
        L[i,] <- c(0,0,1)
        factor=c(factor,3)
      }
    }


  }

  df = data.frame(x=x,y=y, Factor=as.factor(factor))



  Z = L%*%f + matrix(rnorm(nrow(L)* ncol(f), sd=noise_level), nrow = nrow(L))

  X_l =X
  X_f = matrix(rnorm(2* ncol(Z)), ncol = 2)



  pt=proc.time()
  fit_default <- flash(Z, greedy_Kmax = 3)
  tflash = proc.time()-pt




  return(out =list( tflash = tflash ))

}
