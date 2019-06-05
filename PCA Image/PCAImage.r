library(jpeg)
library(RCurl)

url <-"http://utdallas.edu/~txc163430/naruto.jpg"
naruto <- readJPEG(getURLContent(url, binary=TRUE))
r <- naruto[,,1]
g <- naruto[,,2]
b <- naruto[,,3]

naruto.r.pca <- prcomp(r, center = FALSE)
naruto.g.pca <- prcomp(g, center = FALSE)
naruto.b.pca <- prcomp(b, center = FALSE)

rgb.pca <- list(naruto.r.pca, naruto.g.pca, naruto.b.pca)

compressed_directory <- file.path(getwd(), 'compressed')
if(!dir.exists(compressed_directory)) {
  dir.create(compressed_directory) 
}

for (i in seq.int(3, round(nrow(naruto) - 10), length.out = 10)) {
  pca.img <- sapply(rgb.pca, function(j) {
    compressed.img <- j$x[,1:i] %*% t(j$rotation[,1:i])
  }, simplify = 'array')
  writeJPEG(pca.img, paste('compressed/naruto_', round(i,0), '_components.jpg', sep = ''))
}

if(!file.exists('naruto.jpg')) {
  download.file(url, destfile = 'naruto.jpg') 
}
naruto_original <- file.info('naruto.jpg')$size / 1000
compressed_naruto <- dir('compressed/')

for (i in compressed_naruto) {
  full.path <- paste('compressed/', i, sep='')
  print(paste(i, ' size: ', file.info(full.path)$size / 1000, ' original: ', naruto_original, ' % diff: ', round((file.info(full.path)$size / 1000 - original) / naruto_original, 2) * 100, '%', sep = ''))
}
