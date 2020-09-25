# Importing the Dataset
Employee<-read.csv("C:\\AMTA\\Turnover.csv", header = TRUE, sep = ',')
View(Employee)
dim(Employee)
data.frame(colSums(is.na(Employee)))
Employee <- na.omit(Employee)

# Dataset Exploration
summary(Employee)
dim(Employee)
str(Employee)

# Checking Dependent variable for class imbalance
table(Employee$LeaverStatus)

# Converting the categorical variables into factor 
Employee$BossGender <- as.factor(Employee$BossGender)
Employee$Gender <- as.factor(Employee$Gender)
Employee$Country <- as.factor(Employee$Country)
Employee$LeaverStatus <- as.factor(Employee$LeaverStatus)
Employee$Age<- as.numeric(Employee$Age)
Employee$Lengthof.Service<-as.numeric(Employee$Lengthof.Service)

# Create Training Data
input_ones <- Employee[which(Employee$LeaverStatus == 1), ]  # all 1's
input_zeros <- Employee[which(Employee$LeaverStatus == 0), ]  # all 0's

set.seed(100)  # for repeatability of samples
input_ones_training_rows <- sample(1:nrow(input_ones), 0.85*nrow(input_ones))  
input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.85*nrow(input_zeros))

# Create Train Data
training_ones <- input_ones[input_ones_training_rows, ]  
training_zeros <- input_zeros[input_zeros_training_rows, ]
trainingData <- rbind(training_ones, training_zeros)  # row bind the 1's and 0's 


# Create Test Data
test_ones <- input_ones[-input_ones_training_rows, ]
test_zeros <- input_zeros[-input_zeros_training_rows, ]
testData <- rbind(test_ones, test_zeros)  # row bind the 1's and 0's 



# 1st Iteration
LogReg1<- glm(LeaverStatus ~ ., family = 'binomial', data = trainingData)
summary(LogReg1)  # All variables are significant


# 2nd Iteration
LogReg2<- glm(LeaverStatus ~ .- Country, family = 'binomial', data = trainingData)
summary(LogReg2)  # All variables are significant




# 3rd Iteration
LogReg3<- glm(LeaverStatus ~ .- Country - Age, family = 'binomial', data = trainingData)
summary(LogReg3)  # All variables are significant


# 4th Iteration
LogReg4<- glm(LeaverStatus ~ .- Age - Country - BossGender, family = 'binomial', data = trainingData)
summary(LogReg4)  # All variables are significant
predicted <- predict(LogReg4, newdata = testData, type="response")

library(InformationValue)

plotROC(testData$LeaverStatus, predicted) 


CM_1 <- confusionMatrix(testData$LeaverStatus, predicted, threshold = .1)
CM_2 <- confusionMatrix(testData$LeaverStatus, predicted, threshold = .15)
CM_3 <- confusionMatrix(testData$LeaverStatus, predicted, threshold = .2)
CM_4 <- confusionMatrix(testData$LeaverStatus, predicted, threshold = .25)

optCutOff_final<- optimalCutoff(testData$LeaverStatus, predicted)
CM_final <-confusionMatrix(actuals=testData$LeaverStatus, predictedScores=predicted, threshold = optCutOff_final)



sens_1 <- sensitivity(testData$LeaverStatus, predicted, threshold = .1)
spec_1 <- specificity(testData$LeaverStatus, predicted, threshold = .1)

sens_2 <- sensitivity(testData$LeaverStatus, predicted, threshold = .15)
spec_2 <- specificity(testData$LeaverStatus, predicted, threshold = .15)

sens_3 <- sensitivity(testData$LeaverStatus, predicted, threshold = .2)
spec_3 <- specificity(testData$LeaverStatus, predicted, threshold = .2)

sens_4 <- sensitivity(testData$LeaverStatus, predicted, threshold = .25)
spec_4 <- specificity(testData$LeaverStatus, predicted, threshold = .25)

sens_final <- sensitivity(testData$LeaverStatus, predicted, threshold = optCutOff_final)
spec_final <- specificity(testData$LeaverStatus, predicted, threshold = optCutOff_final)


CM_1
CM_2
CM_3
CM_4
CM_final




