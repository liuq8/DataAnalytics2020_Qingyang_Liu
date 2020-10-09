library(rpart)
library(rpart.plot)
library(ggplot2)
data('msleep')
str(msleep)
help('msleep')
mSleepDF1 <- msleep[,c(3,6,10,11)] #3=vore, 6=sleep_total, 10=brainwt, 11=bodywt
str(mSleepDF1)
head(mSleepDF1)
sleepModel_1 <- rpart(sleep_total~., data=mSleepDF1, method='anova')
sleepModel_1
rpart.plot(sleepModel_1,type=3,fallen.leaves = TRUE)
rpart.plot(sleepModel_1,type=3,digits=3,fallen.leaves = TRUE)
rpart.plot(sleepModel_1,type=3,digits=4, fallen.leaves = TRUE)

install.packages('C50')
data('iris')
head(iris)
table(iris$Species)
set.seed(9850)
grn<- runif(nrow(iris))
irisrand<- iris[order(grn),]
str(irisrand)
classificationmodel1<- C50::C5.0(irisrand[1:100,-5],irisrand[1:100,5])
classificationmodel1
summary(classificationmodel1)
prediction1 <-predict(classificationmodel1,irisrand[101:150,])
prediction1
table(iris[101:150,5],prediction1)
plot(classificationmodel1)
library('e1071')
classifier <- naiveBayes(iris[,1:4],iris[,5])
table(predict(classifier,iris[,-5]),iris[,5],dnn = list('predicted','actual'))
classifier$apriori
classifier$tables$Sepal.Length
plot(function(x)dnorm(x,1.462,0.1736640),0,8,col='red',main='Petal length distribution for the 3 different species')
curve(dnorm(x,4.260,0.4699110),add=TRUE,col='blue')
curve(dnorm(x,5.552,0.5518947),add=TRUE,col='green')



require(rpart)
Swiss_rpart <- rpart(Fertility ~ Agriculture + Education + Catholic, data = swiss)
rpart.plot(Swiss_rpart,type=3,fallen.leaves = TRUE) 
rpart.plot(Swiss_rpart,type=3,digit=3,fallen.leaves = TRUE) 
text(Swiss_rpart)

require(party)

treeSwiss<-ctree(Species ~ ., data=iris)
plot(treeSwiss)

cforest(Species ~ ., data=iris, controls=cforest_control(mtry=2, mincriterion=0))

treeFert<-ctree(Fertility ~ Agriculture + Education + Catholic, data = swiss)

cforest(Fertility ~ Agriculture + Education + Catholic, data = swiss, controls=cforest_control(mtry=2, mincriterion=0))


library(tree)
tr <- tree(Species ~ ., data=iris)
tr
tr$frame
plot(tr)
text(tr)

fit2M <- ctree(Mileage~Price + Country + Reliability + Type, data=na.omit(cu.summary))
summary(fit2M)
plot(fit2M, uniform=TRUE, main="CI Tree Tree for Mileage ")

fitK <- ctree(Kyphosis ~ Age + Number + Start, data=kyphosis)
plot(fitK, main="Conditional Inference Tree for Kyphosis")
plot(fitK, main="Conditional Inference Tree for Kyphosis",type="simple")




