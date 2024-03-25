##PCB 6468 Homework 1

##Question 1
#copy data
q1 <- read.table(file = "clipboard", sep = "\t", header = TRUE)
#1 sample 2-tailed t-test
q1x <- c(2, 1.5, 1, 0.5, 1, 0.5, 0, 0, 0, 0, 1, 0.5)
t.test(q1x, y = NULL, alternative = "two.sided", mu = 0.5)


##Question 2
#copy data
q2 <- read.table(file = "clipboard", sep = "\t", header = TRUE)
#2 sample t-test
#x is standard, y is test
q2x <- c(1, 9, 4, 10)
q2y <- c(2, 9, 5, 6)
t.test(q2x, q2y, var.equal = TRUE)


##Question 3
##Part 1
#install e1071
install.packages(e1071)
library(e1071)
#copy data
q3 <- read.table(file = "clipboard", sep = "\t", header = TRUE)
#x is standard, y is test
q3x <- read.table(file = "clipboard", sep = "\t", header = FALSE)
q3y <- read.table(file = "clipboard", sep = "\t", header = FALSE)
#central tendency standard
mean(q3x$V2)
median(q3x$V2)
skewness(q3x$V2)
kurtosis(q3x$V2)
var(q3x$V2)
sd(q3x$V2)
#central tendency test
mean(q3y$V2)
median(q3y$V2)
skewness(q3y$V2)
kurtosis(q3y$V2)
var(q3y$V2)
sd(q3y$V2)

#Part 2
skewtest <- -0.0987
skewstand <- -0.1767
kurttest <- -0.9550
kurtstand <- -0.5738
#Mann-WhitneyWilcox Test for skewness/kurtosis
wilcox.test(skewstand, skewtest)
wilcox.test(kurtstand, kurttest)


