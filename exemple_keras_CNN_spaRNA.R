# Increase the grid size for finer resolution
library(keras)

set.seed(123)
loc <- matrix(runif(200, min=0, max=100), ncol=2)
count <- matrix(rnorm(2000), nrow=200)
grid_size <- 20  # Define new grid size for higher resolution

# Adjusting the breaks slightly to ensure coverage
x_breaks <- seq(min(loc[,1]), max(loc[,1]) * 1.01, length.out = grid_size + 1)
y_breaks <- seq(min(loc[,2]), max(loc[,2]) * 1.01, length.out = grid_size + 1)

x_bins <- cut(loc[,1], breaks=x_breaks, include.lowest=TRUE, right=TRUE)
y_bins <- cut(loc[,2], breaks=y_breaks, include.lowest=TRUE, right=TRUE)

# Create an empty array to hold the binned data
gene_grid <- array(0, dim = c(grid_size, grid_size, ncol(count)))

# Fill the grid with gene expression data
for (i in 1:nrow(loc)) {
  x_ind <- as.integer(x_bins[i])
  y_ind <- as.integer(y_bins[i])
  gene_grid[x_ind, y_ind, ] <- gene_grid[x_ind, y_ind, ] + count[i, ]
}


num_classes <- 10


# Reshape the grid as needed for a single sample approach
gene_grid <- array_reshape(gene_grid, c(1, grid_size, grid_size, ncol(count)))
# Generate example target data for the new grid resolution
target_grid <- array(runif(grid_size * grid_size * num_classes), dim = c(1, grid_size, grid_size, num_classes))
# Normalize each vector to sum to 1
target_grid <- apply(target_grid, c(1,2,3), function(x) x / sum(x))
target_grid <- array(target_grid, dim = c(1, grid_size, grid_size, num_classes))
# Define and compile the model
model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 16, kernel_size = c(3, 3), padding = 'same', activation = 'relu', input_shape = c(grid_size, grid_size, ncol(count))) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), padding = 'same', activation = 'relu') %>%
  layer_upsampling_2d(size = c(2, 2)) %>%
  layer_conv_2d(filters = num_classes, kernel_size = c(3, 3), padding = 'same', activation = 'linear') %>%
  layer_activation_softmax(axis = 3)  # Apply softmax across the channel dimension

model %>% compile(
  optimizer = 'adam',
  loss = 'categorical_crossentropy',  # Appropriate for multi-class probability prediction
  metrics = c('accuracy')
)

# Fit the model
model %>% fit(gene_grid, target_grid, epochs = 50, batch_size = 1)
# Predicting the probabilities
predicted_probabilities <- model %>% predict(gene_grid)
# Print the first few rows of predictions to check
# Initialize a matrix to hold the predictions for each location
predictions_at_loc <- matrix(nrow = nrow(loc), ncol = num_classes)

# Fill the matrix with predictions
for (i in 1:nrow(loc)) {
  x_ind <- as.integer(x_bins[i])
  y_ind <- as.integer(y_bins[i])
  predictions_at_loc[i, ] <- predicted_probabilities[1, x_ind, y_ind, ]
}

