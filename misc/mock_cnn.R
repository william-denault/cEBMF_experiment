library(raster)
library(keras)
library(tensorflow)
library(abind)
# Load the image


#generate a dummy file
#library(tiff)
#library(png)
# Set up the plot parameters
#png("temp_image.png", width = 300, height = 300)  # Using png to draw the image
#plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10))

# Draw some shapes
#rect(1, 1, 3, 3, col="red")
#rect(4, 4, 6, 6, col="blue")
#rect(7, 7, 9, 9, col="green")

# Add some text
#text(5, 5, "Example", cex=1.5)

# Finish the drawing
#dev.off()

# Convert PNG to TIFF
#png_image <- readPNG("temp_image.png")
#writeTIFF(png_image, "example_image.tif")

raster_image <-raster("misc/example_image.tif")

# Function to crop image into smaller tiles
crop_image <- function(r, size) {
  nr <- nrow(r) / size
  nc <- ncol(r) / size
  cropped_images <- list()
  for (i in 1:nr) {
    for (j in 1:nc) {
      extent_obj <- extent(r, (j - 1) * size + 1, j * size, (i - 1) * size + 1, i * size)
      cropped_images[[length(cropped_images) + 1]] <- crop(r, extent_obj)
    }
  }
  return(cropped_images)
}

# Assuming we want 32x32 tiles
tiles <- crop_image(raster_image, 32)
set.seed(123)
labels <- lapply(tiles, function(x) runif(3))  # For example, 3 classes
# Convert rasters to arrays
image_arrays <- lapply(tiles, function(tile) {
  array <- as.array(tile)
  aperm(array, c(2, 1, 3))  # Adjust dimensions if needed
})

# Stack all images into a single array
image_stack <- abind(image_arrays, along = 4)

# Convert labels to a matrix
label_matrix <- do.call(rbind, labels)
model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu', input_shape = c(32, 32, 1)) %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_flatten() %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 3, activation = 'softmax')

# Compile the model
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = 'adam',
  metrics = 'accuracy'
)

# Convert to a keras compatible format
x_train <- array_reshape(image_stack, dim = c(dim(image_stack)[4], 32, 32, 1))
y_train <- to_categorical(label_matrix, num_classes = 3)

# Train the model
model %>% fit(x_train, y_train, epochs = 10, batch_size = 5)
