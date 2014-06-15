# When having many variables that may be correlated with each others it may not be necessary to include them all in the model, instead include a summary that catch most of their characteristics.

# 2. Correlated predictors
library(caret)
library(kernlab)
data(spam)

inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]

# Looking for highly related or similar predictors by calculating the correclation between the predictor variables (except the outcome) and taking the absolute value
M <- abs(cor(training[,-58]))

# each variable has a correlation of 1 with each self, so remove diagonal
diag(M) <- 0

# which variables have high correlation between them
which(M > 0.8, arr.ind=T)

# what is the name of the resulting correlated variables
names(spam)[c(34, 32)]

# plot both variables
plot(spam[, 34], spam[, 32])

# PCA helps in case we don't need every predictor by replacing the redundant ones with a weighted combination that capture most of the information while reducing noise.

# 5. a combination we can do is rotating the plot
X <- 0.71 * training$num415 + 0.71 * training$num857
Y <- 0.71 * training$num415 - 0.71 * training$num857
plot(X, Y)

# 6. Genrally, this is about data compression
# 7. SVD: X = UDV^t and PCA
# 8. Principal components in R - prcomp
smallSpam <- spam[, c(34, 32)]
prComp <- prcomp(smallSpam)
plot(prComp$x[, 1], prComp$x[, 2])
prComp$rotation

# 10. Trying PCA on SPAM data
typeColor <- ((spam$type=="spam") * 1 + 1)
prComp <- prcomp(log10(spam[,-58]+1))
# first component capture more information on data than the second one
plot(prComp$x[,1], prComp$x[,2], col=typeColor, xlab="PC1", ylab="PC2")

# 11. PCA with caret, give method 'pca' and number of components to the pre processing function
preProc <- preProcess(log10(spam[, -58]+1), method="pca", pcaComp=2)
spamPC <- predict(preProc, log10(spam[, -58]+1))
plot(spamPC[, 1], spamPC[, 2], col=typeColor)

# 12. Preprocessing with PCA
preProc <- preProcess(log10(training[, -58]+1), method="pca", pcaComp=2)
# create training prediction 
trainPC <- predict(preProc, log10(training[, -58]+1))
# relate the trainng variable 'training$type' to the principal components 'trainPC'
# the ~ separates the outcome from the predictors
modelFit <- train(training$type ~ ., method="glm", data=trainPC)

# 13. Preprocessing with PCA
# Use the same components for the test set by passing the 'preProc' variable
testPC <- predict(preProc, log10(testing[, -58]+1))
# use the confusion matrix to get the accuracy
confusionMatrix(testing$type, predict(modelFit, testPC))

# 15. Fianl thoughts on PCs
# PCA are useful for linear-type models, they makes it harder to interpret predictors. To avoid outliers, first transform (with log/box cox) and plot predictors to identify problems.

