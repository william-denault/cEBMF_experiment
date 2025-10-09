library(softImpute)
library( comoR)
library(nnet)
library(ashr)
library(flashier)
library(irlba)
library(PMA)



source("/home/cEBMF_RCC_experiments/script/tiling_simu_script.R")
if(file.exists("/home/cEBMF_RCC_experiments/sim/tiling_2.RData")){
  load("/home/cEBMF_RCC_experiments/sim/tiling_2.RData")

}else{
  res <-list()
}
for (o  in (length(res)+1):10000) {

  res[[o]] <- tiling_sim( noise_level= 2,seed=o
  )

  save(res, file="/home/cEBMF_RCC_experiments/sim/tiling_2.RData")
}

