library(caret)
library(kernlab)

data(spam)

# Data splitting with createDataPartition()
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)

training <- spam[inTrain, ]
testing <- spam[-inTrain, ]

modelFit <- train(type ~., data=training, method="glm")

# train options
args(train.default)

# train control
args(trainControl)

# Setting the seed
set.seed(1235)
modelFit2 <- train(type ~., data=training, method="glm")
modelFit2

# Further information
# Caret tutorials:
#   - http://www.edii.uclm.es/~useR-2013/Tutorials/kuhn/user_caret_2up.pdf
# Model training and tuning
#   - http://caret.r-forge.r-project.org/training.html
