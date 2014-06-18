# Predicting with trees by continuously sublitting the outcome to different groups until the group is homogenous, each time using a variable. E.g. Obama vs Clinton.

# 4. Algorithm:
#    1. Start with all variables in one group
#    2. Find best variable that splits the outcome
#    3. Divide the data into groups 'leaves' on that split 'node'
#    4. Within each split, find the best variable/split that separates outcome
#    5. Continue until groups are too small or sufficiently pure

# 5. Measure of impurity: with metrics like Gini index and Deviance/information gain

# 6. An example of good split with 15 to 1 against a bad one of 8 to o

# 7. Example: Iris Data
install.packages("iris")
library(iris)
library(ggplot2)

names(iris)

table(iris$Species)

# 9. Create training and test sets
inTrain <- createDataPartition(y=iris$Species, p=0.7, list=FALSE)
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]

dim(training); dim(testing)

# 10. Iris petal widths/sepal width
# Plot the variables against each other
# Notice for the color we provide 'Species' a discrete variable, you will notice it is a clustering problem rather than linear models
qqplot(Petal.Width, Sepal.Width, colour=Species, data=training)

# 11. Iris petal widths/sepal width
library(caret)
# use caret for building the model, with method 'rpart' which is R package for doing classification and regression trees
modFit <- train(Species ~ ., method="rpart", data=training)
# it outputs the nodes and different model splits to understand what the tree is doing
print(modFit$finalModel)

# 12. Plot tree: we can also visualize the tree in a dandergram
plot(modFit$finalModel, uniform=TRUE, main="Classification Tree")
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=.8)

# 13. Prettier plots: display a colored and more readable dandergram
library(rattle)
fancyRpartPlot(modFit$finalModel)

# 14. Predicting new values: here predicting a class/category for a given input
predict(modFit, newdata=testing)

# 15. Notes and Futher resources:
# Transaformations (e.g. normalizing) may not be useful unless you modify the order of the variables. Trees can also be used for regression problems (continuous outcome).
# Books
#  - Introduction to statistial learning http://www-bcf.usc.edu/~gareth/ISL
#  - Elements of Statistical learning http://statweb.stanford.edu/~tibs/ElemStatLearn/
#  - Classification and Regression Trees - Leo Breiman (Amazon)
