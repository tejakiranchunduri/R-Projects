### Libraries
library(arules)
library(dplyr)
library(stringr)
library(ggplot2)
library(gridExtra)

### Functions
apriori_rules_plot <- function(transactions, support_level, confidence_levels) {
  rule <- sapply(confidence_levels,
                 function(cl) length(apriori(transactions,
                                             parameter=list(sup=support_level,
                                                            conf=cl,
                                                            target="rules"))))
  qplot(confidence_levels, rule, geom=c("point", "line"),
        xlab="Confidence level", ylab="Number of rules found",
        main=paste("Apriori with a support level of", support_level*100, "%")) + theme_bw()
}

read_csv_from_url <- function(url, file) {
  file_url <- paste(url, file, sep="")
  file_data <- read.csv(file_url, sep=",", header=TRUE, stringsAsFactors=FALSE)
  assign(tools::file_path_sans_ext(basename(file)), file_data, envir = .GlobalEnv)
}

### Main code
url <- "http://utdallas.edu/~sxe170530/instacart-market-basket-analysis/"

### Read data
cat("Reading files from url:", url)
files <- c("order_products__prior.csv", "products.csv", "departments.csv")
sapply(files, function(file) read_csv_from_url(url, file))

### Data preprocessing
cat("Cleaning data")
products$product_name <- gsub(",", " ", products$product_name)

### Required variables
support_levels <- c(0.1, 0.05, 0.01, 0.005)
confidence_levels <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)

### Workout for products
merge__prior_products <- merge(order_products__prior, products, by="product_id")
products_transactions_matrix <- select(aggregate(product_name ~ order_id,
                                        data = merge__prior_products,
                                        paste, collapse = ","),
                                       product_name)
cat("Writing products to", paste(getwd(), "products_transactions.csv", sep="/"))
if(file.exists("products_transactions.csv")) file.remove("products_transactions.csv")
write.csv(products_transactions_matrix, "products_transactions.csv", row.names=FALSE, quote=FALSE)

### Q1 - Frequent itemsets for products in orders dataset
products_transactions <- read.transactions("products_transactions.csv",
                                           format="basket", sep = ",", header=TRUE)
summary(products_transactions)
inspect(head(products_transactions, 5))
itemFrequencyPlot(products_transactions, topN=10, type="absolute", col="wheat2",
                  xlab="Product name", ylab="Frequency (absolute)",
                  main="Absolute Product Frequency Plot")
itemFrequencyPlot(products_transactions, topN=10, type="relative", col="lightcyan2",
                  xlab="Product name", ylab="Frequency (relative)",
                  main="Relative Product Frequency Plot")
frequent_products <- eclat (products_transactions, parameter=list(supp=0.04, maxlen=15))
inspect(frequent_products)

### Q2 - Association rules for products in orders dataset
products_plots <- lapply(1:length(support_levels),
                         function(n) apriori_rules_plot(products_transactions,
                                                        support_levels[n],
                                                        confidence_levels))
cat("Saving products' apriori plots at ", paste(getwd(), "products_apriori_plots.pdf", sep="/"))
if(file.exists("products_apriori_plots.pdf")) file.remove("products_apriori_plots.pdf")
ggsave("products_apriori_plots.pdf", marrangeGrob(products_plots, nrow=2, ncol=2))
# Choosing support = 1% and confidence level = 20%
product_association_rules <- apriori(products_transactions,
                                     parameter=list(sup=support_levels[3],
                                                    conf=confidence_levels[8],
                                                    target="rules"))
inspect(product_association_rules)

### Workout for departments
merge__prior_products_departments <- merge(merge__prior_products, departments, by="department_id")
departments_transactions_matrix <- select(aggregate(department ~ order_id,
                                                    data=merge__prior_products_departments,
                                                    paste, collapse=","),
                                          department)
cat(paste("Writing departments to ", paste(getwd(), "departments_transactions.csv", sep="/")))
if(file.exists("departments_transactions.csv")) file.remove("departments_transactions.csv")
write.csv(departments_transactions_matrix, "departments_transactions.csv", row.names=FALSE, quote=FALSE)

### Q3 - Frequent itemsets for departments in orders dataset
departments_transactions <- read.transactions("departments_transactions.csv",
                                              format="basket", sep=",", header=TRUE)
summary(departments_transactions)
inspect(head(departments_transactions, 5))
itemFrequencyPlot(departments_transactions, topN=10, type="absolute", col="wheat2",
                  xlab="Department name", ylab="Frequency (absolute)",
                  main="Absolute Department Frequency Plot")
itemFrequencyPlot(departments_transactions, topN=10, type="relative", col="lightcyan2",
                  xlab="Department name", ylab="Frequency (relative)",
                  main="Relative Department Frequency Plot")
frequent_departments <- eclat (departments_transactions, parameter=list(supp=0.3, maxlen=15))
inspect(frequent_departments)
### Q4 - Association rules for departments in orders dataset
departments_plots <- lapply(1:length(support_levels),
                            function(n) apriori_rules_plot(departments_transactions,
                                                           support_levels[n],
                                                           confidence_levels))
cat("Saving departments' apriori plots at ", paste(getwd(), "departments_apriori_plots.pdf", sep="/"))
if(file.exists("departments_apriori_plots.pdf")) file.remove("departments_apriori_plots.pdf")
ggsave("departments_apriori_plots.pdf", marrangeGrob(departments_plots, nrow=2, ncol=2))
# Choosing support = 10% and confidence level = 80%
department_association_rules <- apriori(departments_transactions,
                                        parameter=list(sup=support_levels[1],
                                                       conf=confidence_levels[2],
                                                       target="rules"))
inspect(department_association_rules)

cat("Execution completed.!")
