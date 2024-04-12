library(softImpute)
library( comoR)
library(nnet)
library(ashr)
library(flashier)
library(irlba)
library(PMA)



source("/home/wdenault/cEBMF_RCC_experiments/script/tiling_simu_script.R")
if(file.exists("/home/wdenault/cEBMF_RCC_experiments/sim/tiling_3.RData")){
  load("/home/wdenault/cEBMF_RCC_experiments/sim/tiling_3.RData")

}else{
  res <-list()
}
for (o  in (length(res)+1):10000) {

  res[[o]] <- tiling_sim( noise_level= 4, seed=o
  )

  save(res, file="/home/wdenault/cEBMF_RCC_experiments/sim/tiling_3.RData")
}

