library(tidyverse)
library(caret)
library(plotly)
library(data.table)
library(GGally)
library(tidymodels)
library(car)
library(scales)
library(lmtest)
library(ggplot2)
library(psych)
library(MASS)
library(car)
library(corrplot)

dataQ1 <- read.csv("/Users/shagunkrishna/Downloads/Book4.csv")
View(dataQ1)

dataQ2 <- dataQ1
str(dataQ2)
summary(dataQ1$price)
boxplot(dataQ1$price)

#removing outliers
bench <- 16503+1.5*IQR(dataQ1$price)
dataQ2 <- dataQ2[dataQ2$price<bench,]
summary(dataQ2$price)
boxplot(dataQ2$price)
#checking for collinear variables
dataQ2.rcorr = cor(as.matrix(dataQ2[-c(2,3,4,5,6,7,13,14,16)]))
dataQ2.rcorr
ggcorr(dataQ2, label = TRUE, label_size = 2.9, hjust = 1, layout.exp = 2)
View(dataQ2)

#removed 7th column i.e enginelocation because after removing outliers, 
#only front level remains which also means that only very expensive cars have engine at the back
#Removing other highly correlated variables
dataQ3 <- dataQ2[-c(7,8,9,10,12,15,22)]

#converting categorical variables
dataQ3$symboling <- as.numeric(dataQ3$symboling)
dataQ3$fueltype <- as.factor(dataQ3$fueltype)
dataQ3$aspiration <- as.factor(dataQ3$aspiration)
dataQ3$doornumber <- as.factor(dataQ3$doornumber)
dataQ3$carbody <- as.factor(dataQ3$carbody)
dataQ3$drivewheel <- as.factor(dataQ3$drivewheel)
#dataQ3$enginelocation <- as.factor(dataQ3$enginelocation)
dataQ3$enginetype <- as.factor(dataQ3$enginetype)
dataQ3$cylindernumber <- as.factor(dataQ3$cylindernumber)
dataQ3$fuelsystem <- as.factor(dataQ3$fuelsystem)
str(dataQ3)

#checking for high correlation between variables
ggcorr(dataQ3, label = TRUE, label_size = 2.9, hjust = 1, layout.exp = 2)

#running regression models
model1 <- lm(price ~., dataQ3)
summary(model1)
model2 <- lm(price ~ .-peakrpm, dataQ3)
summary(model2)
model3 <- lm(price ~ .-peakrpm-fuelsystem, dataQ3)
summary(model3)
model4 <- lm(price ~ .-peakrpm-fuelsystem-symboling, dataQ3)
summary(model4)
model5 <- lm(price ~ .-peakrpm-fuelsystem-symboling-drivewheel, dataQ3)
summary(model5)
model6 <- lm(price ~ .-peakrpm-fuelsystem-symboling-drivewheel-carheight, dataQ3)
summary(model6)
model7 <- lm(price ~ .-peakrpm-fuelsystem-symboling-drivewheel-carheight-doornumber, dataQ3)
summary(model7)
model8 <- lm(price ~ .-peakrpm-fuelsystem-symboling-drivewheel-carheight-doornumber-aspiration, dataQ3)
summary(model8)
model9 <- lm(price ~ .-peakrpm-fuelsystem-symboling-drivewheel-carheight-doornumber-aspiration-fueltype-cylindernumber, dataQ3)
summary(model9)
model10 <- lm(price ~ .-peakrpm-fuelsystem-symboling-drivewheel-carheight-doornumber-aspiration-fueltype-cylindernumber-boreratio, dataQ3)
summary(model10)

#check for multicollinearity
vif(model10)
#check for homoskedasticity
lmtest::bptest(model10)
#check for normality
shapiro.test(model10$residuals)


#data transformation
# dataQ3$carlength <- 1/sqrt(dataQ3$carlength)
# #dataQ3$wheelbase <- 1/sqrt(dataQ3$wheelbase)
# #dataQ3$carwidth <- 1/sqrt(dataQ3$carwidth)
# #dataQ3$carheight <- 1/sqrt(dataQ3$carheight)
# #dataQ3$curbweight <- 1/sqrt(dataQ3$curbweight)
# #dataQ3$enginesize <- 1/sqrt(dataQ3$enginesize)
# dataQ3$boreratio <- 1/sqrt(dataQ3$boreratio)
# dataQ3$stroke <- 1/sqrt(dataQ3$stroke)
# dataQ3$compressionratio <- 1/sqrt(dataQ3$compressionratio)
# dataQ3$horsepower <- 1/sqrt(dataQ3$horsepower)
# dataQ3$peakrpm <- 1/sqrt(dataQ3$peakrpm)
# #dataQ3$citympg <- 1/sqrt(dataQ3$citympg)
# dataQ3$highwaympg <- 1/sqrt(dataQ3$highwaympg)
# dataQ3$price<-1/sqrt(dataQ3$price)

dataQ3$carlength <- log10(dataQ3$carlength)
#dataQ3$carwidth <- log10(dataQ3$carwidth)
#dataQ3$carheight <- log10(dataQ3$carheight)
#dataQ3$curbweight <- log10(dataQ3$curbweight)
#dataQ3$enginesize <- log10(dataQ3$enginesize)
dataQ3$boreratio <- log10(dataQ3$boreratio)
dataQ3$stroke <- log10(dataQ3$stroke)
dataQ3$compressionratio <- log10(dataQ3$compressionratio)
dataQ3$horsepower <- log10(dataQ3$horsepower)
dataQ3$peakrpm <- log10(dataQ3$peakrpm)
# dataQ3$citympg <- log10(dataQ3$citympg)
dataQ3$highwaympg <- log10(dataQ3$highwaympg)
dataQ3$price<-log10(dataQ3$price)

model11 <- lm(price ~ .-peakrpm-fuelsystem-symboling-drivewheel-carheight-doornumber-aspiration-fueltype-cylindernumber-boreratio, dataQ3)
summary(model11)
plot(model11)

#check for multicollinearity
vif(model11)
#check for homoskedasticity
lmtest::bptest(model11)
#check for normality
shapiro.test(model11$residuals)

#All tests passed and the model is parsimonious


# *******************************************************************************************
library(tidyverse)
library(caret)
library(plotly)
library(data.table)
library(GGally)
library(tidymodels)
library(car)
library(scales)
library(lmtest)
library(ggplot2)
library(psych)
library(MASS)
library(car)

dataQ1 <- read.csv("/Users/shagunkrishna/Downloads/Book4.csv")
View(dataQ1)

dataQ2 <- dataQ1
str(dataQ2)
summary(dataQ1$price)

#removing outliers
bench <- 16503+1.5*IQR(dataQ1$price)
dataQ2 <- dataQ2[dataQ2$price<bench,]

#checking for collinear variables
ggcorr(dataQ2, label = TRUE, label_size = 2.9, hjust = 1, layout.exp = 2)
View(dataQ2)
#removed 7th column i.e enginelocation because after removing outliers, 
#only front level remains which also means that only very expensive cars have engine at the back
dataQ3 <- dataQ2[-c(7,8,9,10,12,15,22)]

#converting categorical variables
dataQ3$symboling <- as.numeric(dataQ3$symboling)
dataQ3$fueltype <- as.factor(dataQ3$fueltype)
dataQ3$aspiration <- as.factor(dataQ3$aspiration)
dataQ3$doornumber <- as.factor(dataQ3$doornumber)
dataQ3$carbody <- as.factor(dataQ3$carbody)
dataQ3$drivewheel <- as.factor(dataQ3$drivewheel)
dataQ3$enginelocation <- as.factor(dataQ3$enginelocation)
dataQ3$enginetype <- as.factor(dataQ3$enginetype)
dataQ3$cylindernumber <- as.factor(dataQ3$cylindernumber)
dataQ3$fuelsystem <- as.factor(dataQ3$fuelsystem)
str(dataQ3)

ggcorr(dataQ3, label = TRUE, label_size = 2.9, hjust = 1, layout.exp = 2)

#running regression models
model1 <- lm(price ~., dataQ3)
summary(model1)
model2 <- lm(price ~ .-peakrpm, dataQ3)
summary(model2)
model3 <- lm(price ~ .-peakrpm-fuelsystem, dataQ3)
summary(model3)
model4 <- lm(price ~ .-peakrpm-fuelsystem-symboling, dataQ3)
summary(model4)
model5 <- lm(price ~ .-peakrpm-fuelsystem-symboling-drivewheel, dataQ3)
summary(model5)
model6 <- lm(price ~ .-peakrpm-fuelsystem-symboling-drivewheel-carheight, dataQ3)
summary(model6)
model7 <- lm(price ~ .-peakrpm-fuelsystem-symboling-drivewheel-carheight-doornumber, dataQ3)
summary(model7)
model8 <- lm(price ~ .-peakrpm-fuelsystem-symboling-drivewheel-carheight-doornumber-aspiration, dataQ3)
summary(model8)
model9 <- lm(price ~ .-peakrpm-fuelsystem-symboling-drivewheel-carheight-doornumber-aspiration-fueltype-cylindernumber, dataQ3)
summary(model9)

#check for multicollinearity
vif(model9)
#check for homoskedasticity
lmtest::bptest(model9)
#check for normality
shapiro.test(model9$residuals)


# data transformation
dataQ3$carlength <- 1/sqrt(dataQ3$carlength)
dataQ3$wheelbase <- 1/sqrt(dataQ3$wheelbase)
dataQ3$carwidth <- 1/sqrt(dataQ3$carwidth)
dataQ3$carheight <- 1/sqrt(dataQ3$carheight)
dataQ3$curbweight <- 1/sqrt(dataQ3$curbweight)
dataQ3$enginesize <- 1/sqrt(dataQ3$enginesize)
dataQ3$boreratio <- 1/sqrt(dataQ3$boreratio)
dataQ3$stroke <- 1/sqrt(dataQ3$stroke)
dataQ3$compressionratio <- 1/sqrt(dataQ3$compressionratio)
dataQ3$horsepower <- 1/sqrt(dataQ3$horsepower)
dataQ3$peakrpm <- 1/sqrt(dataQ3$peakrpm)
dataQ3$citympg <- 1/sqrt(dataQ3$citympg)
dataQ3$highwaympg <- 1/sqrt(dataQ3$highwaympg)
dataQ3$price<-1/sqrt(dataQ3$price)

dataQ3$carlength <- log10(dataQ3$carlength)
dataQ3$carwidth <- log10(dataQ3$carwidth)
dataQ3$carheight <- log10(dataQ3$carheight)
dataQ3$curbweight <- log10(dataQ3$curbweight)
dataQ3$enginesize <- log10(dataQ3$enginesize)
dataQ3$boreratio <- log10(dataQ3$boreratio)
dataQ3$stroke <- log10(dataQ3$stroke)
dataQ3$compressionratio <- log10(dataQ3$compressionratio)
dataQ3$horsepower <- log10(dataQ3$horsepower)
dataQ3$peakrpm <- log10(dataQ3$peakrpm)
dataQ3$citympg <- log10(dataQ3$citympg)
dataQ3$highwaympg <- log10(dataQ3$highwaympg)
dataQ3$price<-log10(dataQ3$price)

model10 <- lm(price ~ .-peakrpm-fuelsystem-symboling-drivewheel-carheight-doornumber-aspiration-fueltype-cylindernumber, dataQ3)
summary(model10)
plot(model10)

#check for multicollinearity
vif(model10)
#check for homoskedasticity
lmtest::bptest(model10)
#check for normality
shapiro.test(model10$residuals)
