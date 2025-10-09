library(softImpute)
library( comoR)
library(nnet)
library(ashr)
library(flashier)
library(irlba)
library(PMA)



source("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/additional_experiment_for_rebutall/tiling_simulation_mfair.R")
if(file.exists("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/additional_experiment_for_rebutall/tiling_results_mfair/tiling_1.RData" )){
  load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/additional_experiment_for_rebutall/tiling_results_mfair/tiling_1.RData.RData" )

}else{
  res <-list()
}
for (o  in (length(res)+1):1000) {

  res[[o]] <- tiling_sim( noise_level= 1, seed=o
  )


  save(res, file="C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/additional_experiment_for_rebutall/tiling_results_mfair/tiling_1.RData.RData")
}

