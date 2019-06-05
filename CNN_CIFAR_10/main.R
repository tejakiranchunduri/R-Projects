rm(list=ls())

### Libraries ###
library(keras)
library(tfruns)
library(cloudml)

### Functions ###
getImageFullPath <- function(name, path, ext = ".png") {
  img_name <- trimws(name, which = "both")
  img_path <- trimws(file.path(path, img_name), which = "both")
  img_path_ext <- paste(img_path, ext, sep="")
}

loadImage <- function(path, width = 32, height = 32) {
  img <- image_load(path)
  img_vector <- image_to_array(img, data_format = "channels_last")
}

rescale <- function(x) (x-min(x))/(max(x) - min(x))

buildX <- function(image_names) {
  image_paths <- lapply(image_names, getImageFullPath, path = train_images_path)
  vector_images <- lapply(image_paths, loadImage, width = img_width, height = img_height)
  images_data_frame <- as.data.frame(do.call(rbind, vector_images))
  images_matrix <- data.matrix(rescale(images_data_frame))
  dim(images_matrix) <- c(nrow(images_matrix), img_width, img_height, 3)
  aperm(images_matrix, c(1,2,3,4))
}

buildY <- function(image_categories) {
  image_labels <- unclass(factor(image_categories))
  to_categorical(sapply(image_labels, function(y) y-1))
}

### Variables ###
img_width <- 32
img_height <- 32
img_channels <- 3

output_size <- 10

val_split <- 0.2
test_split <- 0.2

batch_size <- 128
epochs <- 50

seed <- 42

### Import Data ###
train_images_path <- file.path("train")
data <- read.csv("https://www.utdallas.edu/~sxa172830/trainLabels.csv")

test_sample <- sample(1:nrow(data), nrow(data)*test_split)

X <- buildX(data$id)
Y <- buildY(data$label)

train_X <- X[-test_sample,,,]
train_Y <- Y[-test_sample,]

test_X <- X[test_sample,,,]
test_Y <- Y[test_sample,]

### Model Params ###
params <- tfruns::flags(
  # layer 1
  flag_integer("conv_layer1_filter", 16),
  flag_integer("conv_layer1_kernel_size", 3),
  flag_string("conv_layer1_activation", "relu"),
  # layer 2
  flag_integer("conv_layer2_filter", 32),
  flag_integer("conv_layer2_kernel_size", 2),
  flag_string("conv_layer2_activation", "relu"),
  # max pooling and dropout 1
  flag_integer("max_pool_layer1_size", 2),
  flag_numeric("dropout_layer1", 0.25),
  # layer 3
  flag_integer("conv_layer3_filter", 32),
  flag_integer("conv_layer3_kernel_size", 2),
  flag_string("conv_layer3_activation", "relu"),
  # layer 4
  flag_integer("conv_layer4_filter", 16),
  flag_integer("conv_layer4_kernel_size", 3),
  flag_string("conv_layer4_activation", "relu"),
  # max pooling and dropout 2
  flag_integer("max_pool_layer2_size", 2),
  flag_numeric("dropout_layer2", 0.25),
  # dense layer and drop out
  flag_integer("dense_final", 512),
  flag_string("activation_final", "relu"),
  flag_numeric("dropout_final", 0.50)
)

### Initialize, Build and Compile Model ###
model <- keras_model_sequential()

model %>%
  # layer 1
  layer_conv_2d(filter = params$conv_layer1_filter,
                kernel_size = c(params$conv_layer1_kernel_size, params$conv_layer1_kernel_size),
                padding = "same",
                input_shape = c(img_width, img_height, img_channels),
                activation = params$conv_layer1_activation) %>%
  # layer 2
  layer_conv_2d(filter = params$conv_layer2_filter,
                kernel_size = c(params$conv_layer2_kernel_size, params$conv_layer2_kernel_size),
                padding = "same",
                activation = params$conv_layer2_activation) %>%
  # max pooling and dropout 1
  layer_max_pooling_2d(pool_size = c(params$max_pool_layer1_size, params$max_pool_layer1_size)) %>%
  layer_dropout(params$dropout_layer1) %>%
  # layer 3
  layer_conv_2d(filter = params$conv_layer3_filter,
                kernel_size = c(params$conv_layer3_kernel_size, params$conv_layer3_kernel_size),
                padding = "same",
                activation = params$conv_layer3_activation) %>%
  # layer 4
  layer_conv_2d(filter = params$conv_layer4_filter,
                kernel_size = c(params$conv_layer4_kernel_size, params$conv_layer4_kernel_size),
                padding = "same",
                activation = params$conv_layer4_activation) %>%
  # max pooling and dropout 2
  layer_max_pooling_2d(pool_size = c(params$max_pool_layer2_size, params$max_pool_layer2_size)) %>%
  layer_dropout(params$dropout_layer2) %>%
  # flatten
  layer_flatten() %>%
  # dense layer and drop out
  layer_dense(params$dense_final) %>%
  layer_activation(params$activation_final) %>%
  layer_dropout(params$dropout_final) %>%
  # dense output layer 
  layer_dense(output_size) %>% 
  layer_activation("softmax") %>%
  # compile model
  compile(loss = "categorical_crossentropy",
          optimizer = optimizer_adam(),
          metrics = "accuracy")

summary(model)

history <- model %>% fit(x = train_X, y = train_Y, epochs = epochs, batch_size = batch_size, validation_split = val_split)
plot(history)

### Accuracy ###
train_score <- model %>% evaluate(train_X, train_Y)
print(train_score)

test_score <- model %>% evaluate(test_X, test_Y)
print(test_score)

### Predictions ###
predictions <-  predict_classes(model, test_X)
probabilities <- predict_proba(model, test_X)

### Plot images in 5x5 Grid ###
random <- sample(1:length(test_sample), 25)
actual <- test_Y[random,]
preds <- predictions[random]
probs <- round(probabilities[random,], 2)

par(mfrow = c(5, 5), mar = rep(0, 4))
for(i in 1:length(random)) {
  image(t(apply(test_X[random[i],,,], 2, rev)), col = gray.colors(128), axes = F)
  idx <- which(actual[i,]==1)
  legend("topright",
         legend = preds[i],
         text.col = "blue",
         bty = "n", text.font = 2)
  legend("top",
         legend = idx - 1,
         text.col = "black",
         bty = "n", text.font = 2)
  legend("topleft",
         legend = probs[i,idx],
         text.col = "blue",
         bty = "n", text.font = 2)
}