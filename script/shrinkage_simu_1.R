library( comoR)
library(nnet)
library(ashr)





if(file.exists("/home/wdenault/simu_cEBMF/sim/check_shrinkage_1.RData")){
  load("/home/wdenault/simu_cEBMF/sim/check_shrinkage_1.RData")

}else{
  res <-list()
}
for (o  in (length(res)+1):10000) {

  res[[o]] <-sim_func_single_effect(noise_level = 0.5, max_iter=50)

  save(res, file="/home/wdenault/simu_cEBMF/sim/check_shrinkage_1.RData")
}




