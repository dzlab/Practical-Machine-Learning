install.packages("caret")
install.packages("e1071")

library(caret)
library(kernlab)

data(spam)

# Data splitting with createDataPartition()
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)

training <- spam[inTrain, ]
testing <- spam[-inTrain, ]
dim(training)

# Fit a model with train() from 'caret'
set.seed(32343)
modelFit <- train(type ~., data=training, method="glm") # generalized linear model
modelFit

# final model
modelFit$finalModel

# prediction
predictions <- predict(modelFit, newdata=testing)
predictions

# confusion Matrix
confusionMatrix(predictions, testing$type)

# Further information
# Caret tutorials:
#   - http://www.edii.uclm.es/~useR-2013/Tutorials/kuhn/user_caret_2up.pdf
#   - http://cran.r-project.org/web/packages/caret/vignettes/caret.pdf
# A paper introducing the caret package
#   - http://www.jstatsoft.org/v28/i05/paper
