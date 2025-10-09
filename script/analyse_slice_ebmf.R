analyse_slice <- function(i){



  library(keras)
  library(tensorflow)
  library(comoR)
  library(flashier)
  library(ggplot2)
  library(fclust)
  library(scatterpie)
  library(gridExtra)


  sample_names=c("151507", "151508", "151509", "151510", "151669", "151670", "151671" ,"151672","151673", "151674" ,"151675" ,"151676")
  # Here we take the 9th sample as example, in total there are 12 samples (numbered as 1-12), the user can test on other samples if needed.
  clusterNum=c(7,7,7,7,5,5,5,5,7,7,7,7) # each sample has different ground truth cluster number



  load( paste0( "/home/wdenault/cEBMF_RCC_experiments/data/DLPFC/LIBD_sample",i,".RData"))
  load(paste0( "/home/wdenault/cEBMF_RCC_experiments/data/res_spatial_PCA/run_spatial_DLPFC",i,".RData"))
  tt =  as.matrix(count_sub)
  truth = KRM_manual_layers_sub$layer_guess_reordered[match(colnames(LIBD@normalized_expr),colnames(count_sub))]

  loc =  LIBD @location
  tt0 = ( t(as.matrix(LIBD@normalized_expr)) )
  fit_default <-   flash_init(tt0, var_type = 2, S=0.01) %>%

    flash_set_verbose(0) %>%
    flash_greedy(
      Kmax  = clusterNum[i ],
      ebnm_fn = c(ebnm_point_exponential, ebnm_point_exponential)
    )

   save(fit_default,
        file=paste0( "/home/wdenault/cEBMF_RCC_experiments/data/res_ebmf/fit_default_",i,".RData")
        )


}
