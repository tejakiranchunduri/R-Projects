library(cloudml)
cloudml_train('main.R', master_type = 'standard_p100', collect = TRUE, config = 'tuning.yml')
