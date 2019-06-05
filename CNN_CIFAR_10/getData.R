download_data <- function(url_path, data_file) {
  download.file(url_path, data_file, mode = "wb")
  cat('Unzipping Files .............!!!!!!!!!!!')
  unzip(data_file)
  unlink(data_file)
}
download_data('http://utdallas.edu/~sxa172830/train.zip', "train.zip")
