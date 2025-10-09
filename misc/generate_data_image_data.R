buff=50



## Load the package
library("spatialLIBD")
library("SpatialExperiment")
library(raster)
library(ggplot2)
library(comoR)
library(flashier)
library(keras)
library(tensorflow)
library(comoR)
## Download the spot-level data
spe <- fetch_data(type = "spe")

sampleid = '151673'#sample_names[i ]

sample_names=c("151507", "151508", "151509", "151510", "151669", "151670", "151671" ,"151672","151673", "151674" ,"151675" ,"151676")
i = which(sample_names == "151673")
# Here we take the 9th sample as example, in total there are 12 samples (numbered as 1-12), the user can test on other samples if needed.
clusterNum=c(7,7,7,7,5,5,5,5,7,7,7,7) # each sample has different ground truth cluster number

load(paste0( "C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/data/res_spatial_PCA/run_spatial_DLPFC",i,".RData"))


spe_sub <- spe[, spe$sample_id == sampleid]
d <- as.data.frame(cbind(colData(spe_sub), SpatialExperiment::spatialCoords(spe_sub)),
                   optional = TRUE)



Z <- t(LIBD@normalized_expr)# RNA seq normalized expression matrix via Spatial PCA protocol

image <- stack("data/image/151673_full_image.tif")
plot(image)



idx=2438
xmin <-  d$pxl_col_in_fullres[idx] -buff
xmax <-  d$pxl_col_in_fullres[idx] +buff
ymin <-  dim(image)[2] -  d$pxl_row_in_fullres[idx] -buff
ymax <-  dim(image)[2] -  d$pxl_row_in_fullres[idx] +buff
crop_extent <- extent(xmin, xmax, ymin, ymax)

# Crop the image
cropped_image <- crop(image, crop_extent)
plot(cropped_image)



i=1


# generate tiles ----
 image_spot <-  list()
 for ( i in 1:nrow(Z)){
  idx <- which(rownames(d) == rownames(Z) [i])

   xmin <-  d$pxl_col_in_fullres[idx] -buff
   xmax <-  d$pxl_col_in_fullres[idx] +buff
   ymin <-  dim(image)[2] -  d$pxl_row_in_fullres[idx] -buff
   ymax <-  dim(image)[2] -  d$pxl_row_in_fullres[idx] +buff

   extent_obj <- extent(xmin, xmax, ymin, ymax)
   image_spot[[i]] <- crop(image,  extent_obj )
   print(i)
}

  plot(image_spot[[1]])
#save(image_spot, file = "data/image/image_spot.RData")
#load("data/image/image_spot.RData")

image_arrays <- lapply(image_spot, function(tile) {
   array <- as.array(tile)
   aperm(array, c(2, 1, 3))  # Adjust dimensions if needed
 })


 x_train <- abind::abind(lapply(image_arrays, function(x) aperm(x, c(2, 1, 3))), along = 4)
 x_train <- aperm(x_train, c(4, 1, 2, 3))  # Rearrange to [samples, height, width, channels]
 rm(image_arrays)
 rm(image_spot)
 rm(spe)
 rm(LIBD)
 rm(spe_sub)
 cat("Corrected shape of input images:", dim(x_train), "\n")
 max_class=10
 param_nnet.x <- keras_model_sequential() %>%
   # First convolutional layer
   layer_conv_2d(filters = 16, kernel_size = c(3,3), padding = "same", activation = 'relu', input_shape = c(2*buff, 2*buff, 3)) %>%
   layer_max_pooling_2d(pool_size = c(2,2)) %>%

   # Second convolutional layer
   layer_conv_2d(filters = 32, kernel_size = c(3,3), padding = "same", activation = 'relu') %>%
   layer_max_pooling_2d(pool_size = c(2,2)) %>%

   # Flattening the output to feed into the dense layer
   layer_flatten() %>%

   # Dense layer for prediction
   layer_dense(units = 64, activation = 'relu') %>%
   layer_dropout(rate = 0.5) %>%

   # Output layer with softmax activation for multi-class classification
   layer_dense(units = max_class, activation = 'softmax')





 Y=Z
 param_como.x  = list(max_class=max_class,mnreg_type="keras",
                      prior="mix_exp" ,
                      epoch     =10,
                      batch_size= 1000)
 param_como.y  = list(max_class=10,mnreg_type="constant_mnreg",
                      prior="mix_exp"  )
 type_noise='column_wise'
 init_type="flashier_NMF"
 maxit=10
 tol=1e-3


 X_l=x_train
 X_f= matrix(rnorm(2*ncol(Z)),   nrow=col(Z))
 mnreg_type.x="keras"
 mnreg_type.y="constant_mnreg"
 K=3
 type_noise    = type_noise
 init_type     = init_type
 param_como.x  =  param_como.x
 param_como.y  =  param_como.y
 maxit_como    = 1
 param_nnet.x  = param_nnet.x
 check_l_prior=TRUE


 cEBMF.obj <- cEBMF (Y=Y,
                     X_l,
                     X_f,
                     mnreg_type.x="keras",
                     mnreg_type.y="constant_mnreg",
                     K=3,
                     maxit=3,
                     type_noise    = type_noise,
                     init_type     = init_type,
                     param_como.x  =  param_como.x,
                     param_como.y  =  param_como.y,
                     maxit_como    = 1,
                     param_nnet.x  = param_nnet.x )
 cEBMF.obj$Y_fit
