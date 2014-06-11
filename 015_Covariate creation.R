# Notes:
# Covariates or predictors/features are the variables used in the model to combine and predict outcomes.
# level 1 covariates creation from raw data (e.g. email body) gerate variables to describe this data (e.g. frequency of a word).
# level 2 transforming tidy covariates into more usefulones (e.g. square of captial average) for prediction (regression).

library(ISLR)
library(caret)
data(Wage)

inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain, ]
testing <- Wage[-inTrain, ]

# 6. Common covariates to add, dummy (quantitative) variables (check regression models class)
table(training$jobclass)
dummies <- dummyVars(wage ~ jobclass, data=training)
head(predict(dummies, newdata=training))

# 7. removing zero covariates (i.e. have no variabilities), e.g. variables which are always true for a the dataset
nsv <- nearZeroVar(training, saveMetrics=TRUE)
# sex is not variables as the dataset concern male, same thing for region
nsv

# 8. spline basis for linear regression 
library(splines)
# calcuting the square and cubic of x=age
bsBasis <- bs(training$age, df=3)
# adding those variables (i.e. x² and x³ in addition to x¹) allows for a curvy model fitting
bsBasis

# 9. Fitting curves with splines
lm1 <- lm(wage ~ bsBasis, data=training) # fitting a linear model
# plot the variable 'age' and outcome 'wage'
plot(training$age, training$wage, pch=19, cex=0.5)
# add to the plot the curvy model on red
points(training$age, predict(lm1, newdata=training), col="red", pch=19, cex=0.5)

# 10. Splines on the test set
# predict from the created variable a new set of variables to plug to the testing model (as done with the training model)
predict(bsBasis, age=testing$age)

# Further information
# Level 1 feature creation: google "feature extraction for [data type]"
#   - in some applications automated feature creation is possible:
#     http://www.cs.nyu.edu/~yann/talks/lecun-ranzato-icml2013.pdf
# Level 2 feature creation
#   - Caret's preProcess function can handle some preprocessing
#     caret.r-forge.r-project.org/preprocess.html
#   - Basically any method can be used to create variables, use exploratory analysis to create them.
#   - Carpet's gam function can be used to smooth the multiple created variables
