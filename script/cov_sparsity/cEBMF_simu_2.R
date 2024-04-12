library(softImpute)
library( comoR)
library(nnet)
library(ashr)
library(flashier)
library(irlba)
library(PMA)




if(file.exists( "/home/wdenault/cEBMF_RCC_experiments/sim/check_cEBMF_2.RData" )){
  load("/home/wdenault/cEBMF_RCC_experiments/sim/check_cEBMF_2.RData")

}else{
  res <-list()
}
for (o  in (length(res)+1):10000) {

  res[[o]] <-sim_func_cEBMF (noise_level= 0.5,
                             max_iter_cEBMF=50,
                             P1=100 , # number of cov for row /loadings
                             P2=100, # number of cov for col /factors
                             seed=o+1,
                             epoch=100)

  save(res, file="/home/wdenault/cEBMF_RCC_experiments/sim/check_cEBMF_2.RData")
}

