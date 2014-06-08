library(caret)
library(kernlab)

data(spam)

# Data splitting with createDataPartition()
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)

training <- spam[inTrain, ]
testing <- spam[-inTrain, ]
dim(training)

# Paritioning the dataset with 'K-fold'
set.seed(32343)
folds <- createFolds(y=spam$type, k=10, list=TRUE, returnTrain=TRUE)
sapply(folds, length)
folds[[1]][1:10]

# Paritioning the dataset with 'K-fold' and return test
set.seed(32323)
folds <- createFolds(y=spam$type, k=10, list=TRUE, returnTrain=FALSE)
sapply(folds, length)
folds[[1]][1:10]

# Paritioning the dataset with 'resampling'
set.seed(32323)
folds <- createResample(y=spam$type, times=10, list=TRUE)
sapply(folds, length)

# Paritioning a time serie dataset with Time Slices
set.seed(32323)
tme <- 1:1000
folds <- createTimeSlices(y=tme, initialWindow=20, horizon=10)
names(folds)
folds$train[[1]]
folds$test[[1]]

# Further information
# Caret tutorials:
#   - http://www.edii.uclm.es/~useR-2013/Tutorials/kuhn/user_caret_2up.pdf
#   - http://cran.r-project.org/web/packages/caret/vignettes/caret.pdf
# A paper introducing the caret package
#   - http://www.jstatsoft.org/v28/i05/paper
