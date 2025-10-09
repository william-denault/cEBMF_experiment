library(softImpute)
library( comoR)
library(nnet)
library(ashr)
library(flashier)
library(irlba)
library(PMA)



source("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/additional_experiment_for_rebutall/cov_sparsity_sim_mfair.R")
if(file.exists("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/additional_experiment_for_rebutall/cov_sparsity_results_mfair/check_mfair_4.RData" )){
  load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/additional_experiment_for_rebutall/cov_sparsity_results_mfair/check_mfair_4.RData" )

}else{
  res <-list()
}
for (o  in (length(res)+1):1000) {

  res[[o]] <- sim_func_cov_mfair( noise_level= 2, seed=o
  )


  save(res, file="C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/additional_experiment_for_rebutall/cov_sparsity_results_mfair/check_mfair_4.RData")
}

