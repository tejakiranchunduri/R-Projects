library(EBImage)
library(keras)

rm(list=ls())

##################### Utility functions  ##########################
download_data <- function(data_dir, url_path, data_file) {
  if (!dir.exists(data_dir)) {
    download.file(url_path, data_file, mode = "wb")
    untar(data_file, exdir = data_dir)
    unlink(data_file)
  } else {
    cat('Directory already exists')
  }
}

process_to_dataframe <- function(dataPath, label) {
  images <- list.files(dataPath)
  convertedImages <- lapply(images, preprocess_image, path = dataPath)
  vectorizedImages <- lapply(convertedImages, image_to_vector, label = label)
  imageDataFrame <- as.data.frame(do.call(rbind, vectorizedImages))
  names(imageDataFrame) <- c("label", paste("pixel", c(1:imageSize)))
  imageDataFrame
}

preprocess_image <- function(image, path) {
  imagePath <- file.path(path, image)
  img <- readImage(imagePath)
  img_resized <- resize(img, w = width, h = height)
  channel(img_resized, "gray")
}

test_train_split <- function(dataframe, test_split = 0.25) {
  idx <- sample(seq_len(nrow(dataframe)), size = floor(test_split * nrow(dataframe)))
  dataset <- list("train" = dataframe[idx,], "test" = dataframe[-idx,])
}

image_to_vector <- function(image, label) {
  img_matrix <- image@.Data
  img_vector <- as.vector(t(img_matrix))
  c(label, img_vector)
}

process_data <- function(data_x) {
  ds_array <- data_x
  dim(ds_array) <- c(28, 28, ncol(data_x), 1)
  aperm(ds_array, c(3,1,2,4))
}

#### Global variables for Data
width <- 28
height <- 28
imageSize <- width * height

########### Download and unzip data Huskies and Flowers ##################
download_data('HuskiesData', "http://utdallas.edu/~txc163430/n02109961.tar", "n02109961")
download_data('CatsData', "http://utdallas.edu/~txc163430/n02123159.tar", "n02123159")

############# Preprocess and save to csv file ################
huskiesDataFrame <- process_to_dataframe(file.path(getwd(), 'HuskiesData'), 1)
catsDataFrame <- process_to_dataframe(file.path(getwd(), 'CatsData'), 0)

# -------------------------------- TRAIN AND TEST DATA SPLIT ----------------------
combinedDataFrame <- rbind(catsDataFrame, huskiesDataFrame)
dataset <- test_train_split(combinedDataFrame)

# ----------------------------------- MODEL BUILDING ------------------------------
train <- data.matrix(dataset$train)
train_x <- t(train[,-1])
train_y <- train[,1]
train_array <- process_data(train_x)

test <- data.matrix(dataset$test)
test_x <- t(test[,-1])
test_y <- test[,1]
test_array <- process_data(test_x)

model <- keras_model_sequential()  
model %>% 
  layer_conv_2d(kernel_size = c(5, 5), filter = 20, activation = "relu", padding = "same", input_shape = c(28, 28, 1), data_format = "channels_last") %>%
  layer_conv_2d(kernel_size = c(5, 5), filter = 20, activation = "relu", padding = "valid") %>%
  layer_max_pooling_2d(pool_size = 2) %>%
  layer_dropout(rate = 0.25) %>%
  layer_conv_2d(kernel_size = c(5, 5), filter = 50, strides = 2, activation = "relu", padding = "same") %>%
  layer_conv_2d(kernel_size = c(5, 5), filter = 50, activation = "relu", padding = "valid") %>%
  layer_max_pooling_2d(pool_size = 2) %>%
  layer_dropout(rate = 0.25) %>%
  layer_flatten() %>%
  layer_dense(units = 28, activation = "relu") %>% 
  layer_dropout(rate = 0.25) %>%
  layer_dense(units = 1, activation = "sigmoid")

summary(model)

model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = "adam",
  metrics = c('accuracy')
)

history <- model %>% fit(
  x = train_array, y = as.numeric(train_y), 
  epochs = 30, batch_size = 100, 
  validation_split = 0.2
)

train_score <- model %>% evaluate(train_array, train_y)
print(train_score)

test_score <- model %>% evaluate(test_array, test_y)
print(test_score)

plot(history)

predictions <-  predict_classes(model, test_array)
probabilities <- predict_proba(model, test_array)

# Visual inspection of 32 cases
set.seed(100)
random <- sample(1:nrow(test), 25)
actual <- test_y[random]
preds <- predictions[random,]
probs <- as.vector(round(probabilities[random,], 2))

par(mfrow = c(5, 5), mar = rep(0, 4))
for(i in 1:length(random)) {
  image(t(apply(test_array[random[i],,,], 2, rev)), col = gray.colors(128), axes = F)
  legend("topright", legend = ifelse(preds[i] == 0, "Cat", "Dog"), text.col = ifelse(preds[i] == actual[i], "green", "red"), bty = "n", text.font = 2)
  legend("top", legend = ifelse(actual[i] == 0, "Cat", "Dog"), text.col = "black", bty = "n", text.font = 2)
  legend("topleft", legend = probs[i], text.col = ifelse(preds[i] == actual[i], "green", "red"), bty = "n", text.font = 2)
}
