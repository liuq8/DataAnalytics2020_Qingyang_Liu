library(e1071)
set.seed (1)
x=matrix(rnorm(20*2), ncol=2)
y=c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,] + 1
x
y
plot(x, col=(3-y))
dat <- data.frame(x = x,y  = as.factor(y))
svmfit <- svm(y ~., data=dat, kernel="linear", cost=10,scale=FALSE)
plot(svmfit , dat)
svmfit$index
summary(svmfit)
svmfit <- svm(y ~., data=dat, kernel="linear", cost = 0.1, scale=FALSE)
plot(svmfit , dat)
svmfit$index

set.seed (1)
tune.out <- tune(svm, y ~.,data=dat,kernel="linear", ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out)
bestmod=tune.out$best.model 
summary(bestmod)
xtest=matrix(rnorm(20*2), ncol=2)
ytest=sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,] + 1
testdat=data.frame(x=xtest, y=as.factor(ytest))
ypred <-predict(bestmod ,testdat)
table(predict=ypred, truth=testdat$y)
svmfit <- svm(y~., data=dat, kernel="linear", cost=.01, scale=FALSE)
ypred=predict(svmfit ,testdat)
table(predict=ypred, truth=testdat$y)

x[y==1,]=x[y==1,]+0.5
plot(x, col=(y+5)/2, pch=19)
dat=data.frame(x=x,y=as.factor(y))
svmfit <-svm(y~., data=dat, kernel="linear", cost=1e5)
summary(svmfit)
plot(svmfit,dat)

svmfit <- svm(y~., data=dat, kernel="linear", cost=1)
summary(svmfit)
plot(svmfit ,dat)

# We now examine the Khan data set, which consists of a number of tissue samples 
# corresponding to four distinct types of small round blue cell tumors
# For each tissue sample, gene expression measurements are available. 
#The data set consists of training data, xtrain and ytrain, and testing data, xtest and ytest.
library(e1071)
library(ISLR)
names(Khan)
# Let's examine the dimension of the data:
# This data set consists of expression measurements for 2,308 genes.
# The training and test sets consist of 63 and 20 observations respectively
dim(Khan$xtrain )
dim(Khan$xtest )


length(Khan$ytrain )
length(Khan$ytest )
table(Khan$ytrain )
table(Khan$ytest )

# We will use a support vector approach to predict cancer subtype using gene expression measurements.
# In this data set, there are a very large number of features relative to the number of observations.
# This suggests that we should use a linear kernel, because the additional flexibility that will 
# result from using a polynomial or radial kernel is unnecessary.
dat <- data.frame(x=Khan$xtrain , y = as.factor(Khan$ytrain ))
out <- svm(y ~., data=dat, kernel="linear",cost=10)
summary(out)

# We see that there are no training errors. In fact, this is not surprising, because the large number 
# of variables relative to the number of observations implies that it is easy to find hyperplanes that 
# fully separate the classes. 
# We are most interested not in the support vector classifier’s performance on the training observations, 
# but rather its performance on the test observations.

dat.te=data.frame(x=Khan$xtest , y = as.factor(Khan$ytest ))
pred.te=predict(out, newdata=dat.te)
table(pred.te, dat.te$y)
# We see that using cost=10 yields two test set errors on this data.

data(reuters)
is(reuters)
tsv <- ksvm(reuters,rlabels,kernel="stringdot",
            kpar=list(length=5),cross=3,C=10)
tsv





