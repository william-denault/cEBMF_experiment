library(softImpute)
library( comoR)
library(nnet)
library(ashr)
library(flashier)



if(file.exists("/home/wdenault/simu_cEBMF/sim/check_cEBMF_3.RData")){
  load("/home/wdenault/simu_cEBMF/sim/check_cEBMF_3.RData")

}else{
  res <-list()
}
for (o  in (length(res)+1):10000) {

  res[[o]] <-sim_func_cEBMF (noise_level= 1,
                             max_iter_cEBMF=50,
                             P1=200 , # number of cov for row /loadings
                             P2=200, # number of cov for col /factors
                             seed=o)

  save(res, file="/home/wdenault/simu_cEBMF/sim/check_cEBMF_3.RData")
}

