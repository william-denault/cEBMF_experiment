library(softImpute)
library( comoR)
library(nnet)
library(ashr)
library(flashier)
library(irlba)
library(PMA)



source("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/additional_experiment_for_rebutall/cov_sparsity_sim_CMF.R")
if(file.exists("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/additional_experiment_for_rebutall/cov_sparsity_results_CMF/check_CMF_2.RData" )){
  load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/additional_experiment_for_rebutall/cov_sparsity_results_CMF/check_CMF_2.RData" )

}else{
  res <-list()
}
for (o  in (length(res)+1):1000) {

  res[[o]] <- sim_func_cov_CMF( noise_level= 0.5, seed=o
  )


  save(res, file="C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/additional_experiment_for_rebutall/cov_sparsity_results_CMF/check_CMF_2.RData")
}

