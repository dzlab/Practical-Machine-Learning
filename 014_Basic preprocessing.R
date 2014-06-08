# processing predictor variables (useful for model-based approaches than ?)
# warning preprocessing is useful for continuous variables and does not work well for factor variables

library(caret)
library(kernlab)

data(spam)       # load the spam dataset

inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]

# the variable how many capital we see in a row (letter of an email)
hist(training$capitalAve, main="", xlab="ave. capital run length")
# most of the emails has few capital letter
mean(training$capitalAve)
sd(training$capitalAve) # highly variable!!

# Need for preprocess the dataset so that the prediction algorithm don't get wrong by the high variability nature of the variables

# Standardizing
trainCapAve <- training$capitalAve
trainCapAveS <- (trainCapAve - mean(trainCapAve)) / sd(trainCapAve)
# After reducint the variability, the mean and standard deriviation become
mean(trainCapAveS)
sd(trainCapAveS)

# Standardizing - test set
# keep in mind when applying the model on the test set, we should use mean and sd of the test set
testCapAve <- testing$capitalAve
testCapAveS <- (testCapAve - mean(testCapAve)) / sd(testCapAve)
mean(testCapAveS)
sd(testCapAveS)

# Can use preProcess function to do the pre-processing for us by 
# Here we pass all variables except the last one (i.e. 58) which is the outcome, and ask for centring and scalling them all
preObj <- preProcess(training[,-58], method=c("center", "scale"))

trainCapAveS <- predict(preObj, training[,-58])$capitalAve
mean(trainCapAveS)
sd(trainCapAveS)

# then apply pre-process to the testing set with the same object
testCapAveS <- predict(preObj, testing[, -58])$capitalAve
mean(testCapAveS)
sd(testCapAveS)

# can also pass the pre-processing options directly to the train function
set.seed(32343)
modelFit <- train(type ~., data=training, preProcess=c("center", "scale"), method="glm")
modelFit

# Others transformation techniques can be used to solve problems that centering and scaling cannot solve
# Standardizing - Box-Cox transforms continous data and make them look like normal data by maximum likelihood
preObj <- preProcess(training[,-58], method=c("BoxCox"))
trainCapAveS <- predict(preObj, training[, -58])$capitalAve
par(mfrow=c(1,2))
hist(trainCapAveS)
qqnorm(trainCapAveS) # in the Q-Q plot we can see the problem with some points near to -2 and 2 and the line is not perfect

# Standardizing - Imputing data using KNN-Impute
# Missing values in the dataset are not well handled by prediction models
set.seed(13343)

# Make some values NA using a randomly selected indices
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1], size=1, prob=0.05)==1
training$capAve[selectNA] <- NA

# Impute and standardize
# KNN-Impute looks to the K (e.g. 10) nearest values to a missing value and average the values then impute them at that position
install.packages("RANN")
library("RANN")
preObj <- preProcess(training[, -58], method="knnImpute")
capAve <- predict(preObj, training[, -58])$capAve

# Standardize true values
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth - mean(capAveTruth)) / sd(capAveTruth)

# We can compare between the actual values (before removing them) and the imputted values, the imputation works well if they are close
# An overall comparison
quantile(capAve - capAveTruth)

# compare only concerned values (i.e. the one selected for imputation)
quantile((capAve - capAveTruth)[selectNA])

# compare all but the selected values for imputation
# values from both distributions are even closer (i.e. smaller difference)
quantile((capAve - capAveTruth)[!selectNA])

# Further information
# Preprocessing with Caret:
#   - http://caret.r-forge.r-project.org/preprocess.html
