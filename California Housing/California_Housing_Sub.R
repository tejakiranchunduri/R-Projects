knitr::opts_chunk$set(echo = TRUE)

require(tidyverse)
require(ggplot2)
require(corrplot)
require(maps)
require(mapview)
require(sf)

naPercentage <- function(x){
  round(sum(is.na(x))/length(x),2)
}

file_name <- 'http://www.utdallas.edu/~txc163430/housing.csv'
housing <- read.csv(file_name, sep=",", header=TRUE, stringsAsFactors=FALSE)

### columns in the raw dataset
names(housing)

### dimensions of the raw dataset
dim(housing)

### summary of the raw dataset
summary(housing)

### structure of the raw dataset
str(housing)

### NA percentage of the raw dataset
apply(housing, 2, naPercentage)

#### PLots - Histograms of data distribution
qplot(housing$median_income,
      geom="histogram",
      main = "Histogram for Ocean Proximity", 
      xlab = "Ocean Proximity",  
      fill=rgb(0.2,0.7,0.1,0.4), 
      col="red")

ggplot(housing, aes(x = ocean_proximity)) + geom_bar()

#### Location Data Analysis - Plotting Map Views for latitude and longitude
california <- st_as_sf(housing, coords = c("longitude", "latitude"), crs= 4326)
mapview(california)
mapview(california, zcol = "median_house_value", legend = TRUE)

### Median House values Ranging from 250K - 500K (in millions)
housing_L1 <- housing[housing$median_house_value >= 250000 & housing$median_house_value < 500001,]
california_L1 <- st_as_sf(housing_L1, coords = c("longitude", "latitude"), crs= 4326)
mapview(california_L1, zcol="median_house_value", legend = TRUE)

### Median House values Ranging from 50K - 100K (in millions)
housing_L3 <- housing[housing$median_house_value >= 50000 & housing$median_house_value < 100000,]
california_L3 <- st_as_sf(housing_L3, coords = c("longitude", "latitude"), crs= 4326)
mapview(california_L3, zcol="median_house_value", legend = TRUE)

### Median House values Ranging from 450K - 500K (in millions)
housing_L4 <- housing[housing$median_house_value >= 450000 & housing$median_house_value <= 500001,]
california_L4 <- st_as_sf(housing_L4, coords = c("longitude", "latitude"), crs= 4326)
mapview(california_L4, zcol="median_house_value", legend = TRUE)


### using median to fill null values
median_bedrooms <- median(housing$total_bedrooms, na.rm=TRUE)
housing$total_bedrooms[is.na(housing$total_bedrooms)] <- median_bedrooms


### feature normalization
spread_housing <- housing %>% mutate(yesno = 1) %>% spread('ocean_proximity', yesno, fill = 0)

spread_housing$median_house_value <- spread_housing$median_house_value / 1000000

normalized_housing <- spread_housing %>% 
                      select(-c(longitude, latitude, median_house_value))  %>% 
                      scale() %>% as_tibble() %>% 
                      add_column(median_house_value = spread_housing$median_house_value, longitude = spread_housing$longitude, latitude = spread_housing$latitude)

names(normalized_housing)<-str_replace_all(names(normalized_housing), c(" " = "_" , "<1" = "lessThan" ))
### correlation matrix - linear regression
corel <- cor(normalized_housing)
corrplot(corel, method="circle")
corMat <- as.data.frame(corrplot(corel,method = "number"))
names(corMat) <- names(normalized_housing)
row.names(corMat)[abs(corMat$median_house_value) > 0.50]

lm.fit = lm(median_house_value ~ median_income, data=normalized_housing)
summary(lm.fit)

### coefficients
coef(lm.fit)

### confidence interval
confint(lm.fit)

predict(lm.fit,data.frame(median_income=(c(-1,0,1,2,3,4,5))), interval="confidence")
predict(lm.fit,data.frame(median_income=(c(-1,0,1,2,3,4,5))), interval="prediction")

plot(housing$median_income, housing$median_house_value, pch="+")
abline(lm.fit,lwd=3,col="red")

### multiple linear regression
lm.fit2 = lm(median_house_value ~ median_income+total_rooms, data=normalized_housing)
summary(lm.fit2)

lm.fit2 = lm(median_house_value ~ median_income+housing_median_age, data=normalized_housing)
summary(lm.fit2)

lm.fit2 = lm(median_house_value ~ median_income+lessThanH_OCEAN, data=normalized_housing)
summary(lm.fit2)

lm.fit2 = lm(median_house_value ~ median_income+population, data=housing)
summary(lm.fit2)

lm.fit2 = lm(median_house_value ~ ., data=normalized_housing)
summary(lm.fit2)

lm.fit2 = lm(median_house_value ~ median_income*population, data=normalized_housing)
summary(lm.fit2)

lm.fit2=lm(median_house_value ~ median_income+I(median_income^2), data = normalized_housing)
summary(lm.fit2)

lm.fit2=lm(median_house_value~poly(median_income,5), data=normalized_housing)
summary(lm.fit2)

anova(lm.fit, lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit2)
