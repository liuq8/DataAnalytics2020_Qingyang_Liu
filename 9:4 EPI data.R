install.packages("MASS")
library(MASS)
attach(Boston)
?Boston
help("Boston")
dim(Boston)
names(Boston)
str(Boston)
nrow(Boston)
ncol(Boston)
summary(Boston)
summary


install.packages("ISLR")

library(ISLR)
data("Auto")
head(Auto)
help("Auto")
head(Auto,10)
names(Auto)
summary(Auto)
fivenum(Auto$mpg)
boxplot(Auto$weight)
mean(Auto$weight)
median(Auto$weight)


help("read.csv")
data1 <- read.csv(file.choose(),header=TRUE)
data1
help(data1)
head(data1)
names(data1)
summary(data1$EPI)
fivenum(data1$EPI)
boxplot(data1$EPI)
mean(data1$EPI)
median(data1$EPI)
hist(data1$EPI)
