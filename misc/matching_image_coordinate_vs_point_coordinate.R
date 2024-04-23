## Load the package
library("spatialLIBD")
library("SpatialExperiment")
library(raster)
library(ggplot2)
## Download the spot-level data
spe <- fetch_data(type = "spe")

## This is a SpatialExperiment object

coords <- spatialCoords(spe)

sample_names=c("151507", "151508", "151509", "151510", "151669", "151670", "151671" ,"151672","151673", "151674" ,"151675" ,"151676")
i = which(sample_names == "151673")
# Here we take the 9th sample as example, in total there are 12 samples (numbered as 1-12), the user can test on other samples if needed.
clusterNum=c(7,7,7,7,5,5,5,5,7,7,7,7) # each sample has different ground truth cluster number
load( paste0( "C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/data/DLPFC/LIBD_sample",i,".RData"))
alpha = NA
na_color = "#CCCCCC40"
clustervar = "spatialLIBD"
image_id ="lowres"
point_size = 2
xy_coords
sampleid = '151673'#sample_names[i ]
img <- SpatialExperiment::imgRaster(spe, sample_id = sampleid,
                                    image_id = image_id)
load(paste0( "C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/data/res_spatial_PCA/run_spatial_DLPFC",i,".RData"))

xy_coords <- LIBD@location
buf=20
idx=1
plot(img [ abs( floor(xy_coords[idx,1]-buf):ceiling(xy_coords[idx,1]+buf)),
           abs(floor(xy_coords[idx,2]-buf):ceiling(xy_coords[idx,2]+buf) )])

spe_sub <- spe[, spe$sample_id == sampleid]
d <- as.data.frame(cbind(colData(spe_sub), SpatialExperiment::spatialCoords(spe_sub)),
                   optional = TRUE)




pxl_row_in_fullres <- pxl_col_in_fullres <- key <- NULL



frame_lims <- frame_limits(spe, sampleid = sampleid,
                           image_id = image_id)
img <- img[frame_lims$y_min:frame_lims$y_max, frame_lims$x_min:frame_lims$x_max]
adjust <- list(x = frame_lims$x_min, y = frame_lims$y_min)


x= pxl_col_in_fullres * SpatialExperiment::scaleFactors(spe,
                                                        sample_id = sampleid, image_id = image_id) - adjust$x
y = pxl_row_in_fullres * SpatialExperiment::scaleFactors(spe,
                                                         sample_id = sampleid, image_id = image_id) - adjust$y

colors = c("#b2df8a", "#e41a1c", "#377eb8", "#4daf4a", "#ff7f00",
           "gold", "#a65628", "#999999", "black", "grey", "white",
           "purple")

idx=2438
p <- ggplot(d[-idx,], aes(x = pxl_col_in_fullres * SpatialExperiment::scaleFactors(spe,
                                                                                   sample_id = sampleid,
                                                                                   image_id = image_id) - adjust$x,
                          y = pxl_row_in_fullres * SpatialExperiment::scaleFactors(spe,
                                                                                   sample_id = sampleid,
                                                                                   image_id = image_id) - adjust$y,
                          fill = factor(!!sym(clustervar)), key = key))

grob <- grid::rasterGrob(img, width = grid::unit(1, "npc"),
                         height = grid::unit(1, "npc"))

p <- p + geom_spatial(data = tibble::tibble(grob = list(grob)),
                      aes(grob = grob), x = 0.5, y = 0.5)


p <-p  + geom_point(shape = 21, size = point_size, stroke = 0,
                    colour = "transparent", alpha = alpha) + coord_fixed(expand = FALSE) +
  scale_fill_manual(values = colors, na.value = na_color) +
  xlim(0, ncol(img)) + ylim(nrow(img), 0)
p

image <- raster("data/image/151673_full_image.tif")
plot(image)
order(d$pxl_row_in_fullres)


idx= 1989
idx=2438
d[idx,]
p1 <- ggplot(d , aes(x = pxl_col_in_fullres * SpatialExperiment::scaleFactors(spe,
                                                                              sample_id = sampleid,
                                                                              image_id = image_id) - adjust$x,
                     y = pxl_row_in_fullres * SpatialExperiment::scaleFactors(spe,
                                                                              sample_id = sampleid,
                                                                              image_id = image_id) - adjust$y,
                     fill = factor(!!sym(clustervar)), key = key))

grob <- grid::rasterGrob(img, width = grid::unit(1, "npc"),
                         height = grid::unit(1, "npc"))

p1 <- p1 + geom_spatial(data = tibble::tibble(grob = list(grob)),
                        aes(grob = grob), x = 0.5, y = 0.5)


p1 <-p1  + geom_point(shape = 21, size = point_size, stroke = 0,
                      colour = "transparent", alpha = alpha) + coord_fixed(expand = FALSE) +
  scale_fill_manual(values = colors, na.value = na_color) +
  xlim(0, ncol(img)) + ylim(nrow(img), 0)

p <- ggplot(d[-idx,], aes(x = pxl_col_in_fullres * SpatialExperiment::scaleFactors(spe,
                                                                                   sample_id = sampleid,
                                                                                   image_id = image_id) - adjust$x,
                          y = pxl_row_in_fullres * SpatialExperiment::scaleFactors(spe,
                                                                                   sample_id = sampleid,
                                                                                   image_id = image_id) - adjust$y,
                          fill = factor(!!sym(clustervar)), key = key))

grob <- grid::rasterGrob(img, width = grid::unit(1, "npc"),
                         height = grid::unit(1, "npc"))

p <- p + geom_spatial(data = tibble::tibble(grob = list(grob)),
                      aes(grob = grob), x = 0.5, y = 0.5)


p <-p  + geom_point(shape = 21, size = point_size, stroke = 0,
                    colour = "transparent", alpha = alpha) + coord_fixed(expand = FALSE) +
  scale_fill_manual(values = colors, na.value = na_color) +
  xlim(0, ncol(img)) + ylim(nrow(img), 0)

library(gridExtra)
grid.arrange(p1, p, ncol=2)



buff=200


dim(image)[2]
xmin <-  d$pxl_col_in_fullres[idx] -buff
xmax <-  d$pxl_col_in_fullres[idx] +buff
ymin <- dim(image)[2] - d$pxl_row_in_fullres[idx] -buff
ymax <-  dim(image)[2] -  d$pxl_row_in_fullres[idx] +buff
crop_extent <- extent(xmin, xmax, ymin, ymax)

# Crop the image
cropped_image <- crop(image, crop_extent)
plot(cropped_image)
