require(jpeg)
require(RCurl)

url <-"http://utdallas.edu/~txc163430/naruto.jpg"
readImage <- readJPEG(getURLContent(url, binary=TRUE))
graphName <- "Naruto Uzumaki"
dm <- dim(readImage)
rgbImage <- data.frame(
  x=rep(1:dm[2], each=dm[1]),
  y=rep(dm[1]:1, dm[2]),
  r.value=as.vector(readImage[,,1]),
  g.value=as.vector(readImage[,,2]),
  b.value=as.vector(readImage[,,3]))

plot(y ~ x, data=rgbImage, main=graphName,
     col = rgb(rgbImage[c("r.value", "g.value", "b.value")]), 
     asp = 1, pch = ".")

kColors <- c(3, 5, 8, 10, 12, 15, 20, 30, 40, 50, 70)

plotClusteredImages <- function(kvalue) {
  kMeans <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")], 
                   centers = kvalue)
  clusterColour <- rgb(kMeans$centers[kMeans$cluster, ])
  plot(y ~ x, data=rgbImage, main=graphName,
       col = clusterColour, asp = 1, pch = ".",
       axes=FALSE, ylab="", 
       xlab=paste("k-means cluster analysis of ", kvalue, " colours"))
}

lapply(kColors, plotClusteredImages)

wssplot <- function(data, nc=50, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)
  }
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}

wssplot(rgbImage)
