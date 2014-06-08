library(kernlab)
data(spam)
set.seed(333)

# take a subset of data to build a predictor
smallSpam <- spam[sample(dim(spam)[1], size=10),]
spamLabel <- (smallSpam$type == "spam") * 1 + 1
plot(smallSpam$capitalAve, col=spamLabel)

# define a first predictor with high accuracy but overfit dataset samples
rule1 <- function(x) {
   prediction <- rep(NA, length(x))
   prediction[x > 2.7] <- "spam"
   prediction[x < 2.4] <- "nonspam"
   # add a rule for a specific spam that cannot be captured with previous conditions
   prediction[(x >= 2.40 & x <= 2.45)] <- "spam"
   prediction[(x > 2.45 & x <= 2.70)] <- "nonspam"
   return(prediction)
}
# show the accuracy of this predictor
table(rule1(smallSpam$capitalAve), smallSpam$type)

# define a second predictor that omit the rule specific to a point
rule2 <- function(x) {
   prediction <- rep(NA, length(x))
   prediction[x > 2.8] <- "spam"
   prediction[x <= 2.8] <- "nonspam"
   return(prediction)
}
table(rule2(smallSpam$capitalAve), smallSpam$type)

# apply both predictors to the whole dataset and display accuracy table
table(rule1(spam$capitalAve), spam$type)
table(rule2(spam$capitalAve), spam$type)

mean(rule1(spam$capitalAve)==spam$type)

# show the number of times the predictor gave an errorneous value
sum(rule1(spam$capitalAve)==spam$type)
sum(rule2(spam$capitalAve)==spam$type)

