library( comoR)
library(nnet)
library(ashr)


library(nnet)
library(ashr)

if(file.exists("/home/wdenault/simu_cEBMF/sim/cash_sim_1.RData")){
  load("/home/wdenault/simu_cEBMF/sim/cash_sim_1.RData")

}else{
  res <-list()
}
for (o  in (length(res)+1):100) {

  res[[o]] <- comoR::: c_ash_sim ( N=5000,dist= "skew",extended = FALSE ,P=200, max_iter = 10)

  save(res, file="/home/wdenault/simu_cEBMF/sim/cash_sim_1.RData")
}




