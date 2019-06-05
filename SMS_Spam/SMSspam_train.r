library(tm)
library(SnowballC)
library(keras)

### Function to Preprocess the text data
preprocess <- function(text){
  category <- Corpus(VectorSource(text))
  getTransformations()
  toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})
  category <- tm_map(category, toSpace, "-")
  category <- tm_map(category, toSpace, ":")
  category <- tm_map(category, toSpace, "'")
  category <- tm_map(category, removePunctuation)
  category <- tm_map(category, content_transformer(tolower))
  category <- tm_map(category, removeNumbers)
  category <- tm_map(category, removeWords, stopwords("english"))
  category <- tm_map(category, stripWhitespace)
  category <- tm_map(category, stemDocument)
  category[[1]]$content
}

# Converting the class values directly to 
convertToBinary <- function(x) {
  if (x == "ham")
    0
  else
    1
}

data <- read.table("http://utdallas.edu/~txc163430/SMSSpamCollection.txt", sep="\t")
colnames(data)[1] <- "Class"
colnames(data)[2] <- "Message"

data$Class <- lapply(data$Class, convertToBinary)

messageContent <- lapply(data$Message, as.character)

#Converting to UTF8 format
messageContent <- lapply(messageContent, function(x) iconv(enc2utf8(x), sub = "byte"))

#Preprocess the message text data
data$Message <- lapply(messageContent, preprocess)
data <- data.frame(lapply(data, as.character), stringsAsFactors = FALSE)

vocabSize <- 1000

tokenizerMessage <- text_tokenizer(vocabSize)
fit_text_tokenizer(tokenizerMessage, data$Message)

textSequence <- texts_to_sequences(tokenizerMessage, data$Message)

maxlen <- 100
batch_size <- 50
embedding_dims <- 50
filters <- 64
kernel_size <- 3
hidden_dims <- 100

sequenceData <- textSequence %>%
  pad_sequences(maxlen = maxlen)

labelsData <- data$Class


trainingSize <- floor(0.80 * nrow(data))
set.seed(28)
trainIndices <- sample(seq_len(nrow(data)), size = trainingSize)

x_train <- sequenceData[trainIndices, ]
y_train <- labelsData[trainIndices]
x_test <- sequenceData[-trainIndices,]
y_test <- labelsData[-trainIndices]

model <- keras_model_sequential() %>% 
  layer_embedding(vocabSize, embedding_dims, input_length = maxlen) %>%
  layer_global_max_pooling_1d() %>%
  layer_dense(hidden_dims) %>%
  #layer_dropout(0.3) %>%
  layer_activation("relu") %>%
  layer_dense(1) %>%
  layer_activation("sigmoid") %>% compile(
    loss = "binary_crossentropy",
    optimizer = "adam",
    metrics = "accuracy"
  )

history <- model %>%
  fit(
    x_train,
    y_train,
    batch_size = 100,
    epochs = 10,
    validation_split = 0.2
  )

summary(model)
score <- model %>% evaluate(x_test, y_test)
score

plot(history)
