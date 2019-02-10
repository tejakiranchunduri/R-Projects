knitr::opts_chunk$set(echo = TRUE)

require(tidyverse)
require(ggplot2)
require(corrplot)

naPercentage <- function(x){
  round(sum(is.na(x))/length(x),2)
}

file_name <- '/Users/shreecharane/Downloads/Semeter_4/CS_6301-R_for_Data_Science/Projects/Linear_Regression/housing.csv'
housing <- read.csv(file_name, sep=",", header=TRUE, stringsAsFactors=FALSE)

### columns in the raw dataset
print(names(housing))

### dimensions of the raw dataset
print(dim(housing))

### summary of the raw dataset
print(summary(housing))

### structure of the raw dataset
print(str(housing))

### NA percentage of the raw dataset
print(apply(housing, 2, naPercentage))

### using median to fill null values
median_bedrooms <- median(housing$total_bedrooms, na.rm=TRUE)
housing$total_bedrooms[is.na(housing$total_bedrooms)] <- median_bedrooms

### Remove latitude, longitude columns
drop_housing <- housing[, -c(1:2)]

### feature normalization
spread_housing <- drop_housing %>% mutate(yesno = 1) %>% spread('ocean_proximity', yesno, fill = 0)
normalized_housing <- spread_housing %>% select(-c(median_house_value))  %>% scale() %>% as_tibble() %>% add_column(median_house_value = spread_housing$median_house_value)
View(normalized_housing)

### correlation matrix - linear regression
corel <- cor(housing)
corrplot(corel, method="circle")
corMat <- as.data.frame(corrplot(corel,method = "number"))
names(corMat) <- names(housing)
row.names(corMat)[abs(corMat$median_house_value) > 0.50]

lm.fit = lm(median_house_value ~ median_income, data=housing)
summary(lm.fit)

### coefficients
coef(lm.fit)

### confidence interval
confint(lm.fit)

predict(lm.fit,data.frame(median_income=(c(5,10,15))), interval="confidence")
predict(lm.fit,data.frame(median_income=(c(5,10,15))), interval="prediction")

plot(housing$median_income, housing$median_house_value, pch="+")
abline(lm.fit,lwd=3,col="red")

### multiple linear regression
lm.fit2 = lm(median_house_value ~ median_income+total_rooms, data=housing)
summary(lm.fit2)

lm.fit2 = lm(median_house_value ~ median_income+housing_median_age, data=housing)
summary(lm.fit2)

lm.fit2 = lm(median_house_value ~ median_income+less1hOcean, data=housing)
summary(lm.fit2)

lm.fit2 = lm(median_house_value ~ median_income+population, data=housing)
summary(lm.fit2)

lm.fit2 = lm(median_house_value ~ ., data=housing)
summary(lm.fit2)

lm.fit2 = lm(median_house_value ~ median_income*population, data=housing)
summary(lm.fit2)

lm.fit2=lm(median_house_value~median_income+I(median_income^2))
summary(lm.fit2)

lm.fit2=lm(median_house_value~poly(median_income,5), data=housing)
summary(lm.fit2)

anova(lm.fit, lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit2)
