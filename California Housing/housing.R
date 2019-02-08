######## load data

housing <- read.csv("housing.csv", sep=",", header=T, stringsAsFactors= F)
names(housing)
dim(housing)
summary(housing)
str(housing)

for(i in 1:ncol(housing)) {
  colName <- colnames(housing[i])
  pctNull <- sum(is.na(housing[,i]))/length(housing[,i])
  print(paste("Column ", colName, " has ", round(pctNull*100, 3), "% of nulls"))
}


####### Method 1 : using median to fill null values

median_bedrooms <- median(housing$total_bedrooms, na.rm=TRUE)
housing$total_bedrooms[is.na(housing$total_bedrooms)] <- median_bedrooms


###### Method 2 : Build model to predict and then fill nulls 

# TODO

###### Remove latitude, longitude columns

housing <- housing[, -c(1:2)] # ignore latitude and longitude


######## categorical to numerical for ocean proximity

housing$less1hOcean <- 0
housing$inland <- 0
housing$island <- 0
housing$nearbay <- 0
housing$nearocean <- 0


for(i in 1:nrow(housing)) {
  if(housing[i,'ocean_proximity'] == "NEAR BAY") {
    housing[i,'nearbay'] <- 1
  }
  else if(housing[i,'ocean_proximity'] == "NEAR OCEAN") {
    housing[i, 'nearocean'] <- 1
  }
  else if (housing[i,'ocean_proximity'] == "ISLAND") {
    housing[i,'island'] <- 1
  }
  else if(housing[i,'ocean_proximity'] == "INLAND") {
    housing[i,'inland'] <- 1
  }
  else if(housing[i,'ocean_proximity'] == "<1H OCEAN") {
    housing[i,'less1hOcean'] <- 1
  }
}

housing <- housing[, -c(8)] #remove ocean proximity column


### feature normalization

for(i in 1:ncol(housing)) {
  housing[,i] <- (housing[,i] - mean(housing[,i]))/ sd(housing[,i])
}
View(housing)


####### correlation matrix - linear regression

library(corrplot)
require(corrplot)
H <- cor(housing)
corrplot(H, method="circle")
corMat <- as.data.frame(corrplot(H,method = "number"))
names(corMat) <- names(housing)
row.names(corMat)[abs(corMat$median_house_value) > 0.50]

lm.fit = lm(median_house_value ~ median_income, data=housing)
summary(lm.fit)
coef(lm.fit)
confint(lm.fit)

predict(lm.fit,data.frame(median_income=(c(5,10,15))), interval="confidence")
predict(lm.fit,data.frame(median_income=(c(5,10,15))), interval="prediction")

plot(housing$median_income, housing$median_house_value, pch="+")
abline(lm.fit,lwd=3,col="red")


###### multiple linear regression

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
anova(lm.fit, lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit2)

lm.fit2=lm(median_house_value~poly(median_income,5), data=housing)
summary(lm.fit2)
