# install the kernlab package containing the spam dataset
install.packages("kernlab")
library(kernlab)
data(spam)

# display the frequency/density of some words of the data set
head(spam)

# display the density of the different times word 'your' appeared in a spam and non spam mails
plot(density(spam$your[spam$type=="nonspam"]), col="blue", main="", xlab="Frequency of 'your'")
lines(density(spam$your[spam$type=="spam"]),col="red")

# display a horizontal line showing dividing the frequency of 'your' in a spam and non spam mails
abline(v=0.5,col="black")

# a sample prediction algorithm
prediction <- ifelse(spam$your > 0.5, "spam", "nonspam")

# display the percentage of spams and non spams classified correctly and not
table(prediction, spam$type)/length(spam$type)

# accuracy = 0.459 + 0.292 = 0.751
