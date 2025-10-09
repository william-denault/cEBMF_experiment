library(softImpute)
library( comoR)
library(nnet)
library(ashr)
library(flashier)
library(irlba)
library(PMA)




if(file.exists("/home/cEBMF_RCC_experiments/sim/check_cEBMF_03.RData"  )){
  load("/home/cEBMF_RCC_experiments/sim/check_cEBMF_03.RData")

}else{
  res <-list()
}
for (o  in (length(res)+1):10000) {

  res[[o]] <-sim_func_cEBMF (noise_level= 2,
                             max_iter_cEBMF=50,
                             P1=10 , # number of cov for row /loadings
                             P2=10, # number of cov for col /factors
                             seed=o+1,
                             epoch=100,
                             beta1=0
  )

  save(res, file="/home/cEBMF_RCC_experiments/sim/check_cEBMF_03.RData")
}
