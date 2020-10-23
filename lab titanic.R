titanicdf<- read.csv('/Users/claireliu/Box/Documents/MSBA/6962 Data Analytics/dataset/Titanic_train.csv')
View(titanicdf)
titanicdf <- na.omit(titanicdf)
str(titanicdf)
titanicdf$Pclass<- factor(titanicdf$Pclass,order=TRUE, levels=c(3,2,1))
library(rpart)
titanic_rpart <- rpart(Survived~ Sex+Pclass+Age,method = 'class',data=titanicdf)
library(rpart.plot)
rpart.plot(titanic_rpart)


 
install.packages("stats")

titanicdf1<- read.csv('/Users/claireliu/Box/Documents/MSBA/6962 Data Analytics/dataset/Titanic_train.csv')
str(titanicdf1)
titanicd1 <-na.omit(titanicdf1)

require(party)
titanic_ctree <- ctree(Survived~ Pclass+Age+Fare,data = titanicdf1)
plot(titanic_ctree)



titanic_hclust<- data.frame(titanicdf1$Survived,titanicdf1$Pclass,titanicdf1$Age)
str(titanic_hclust)
View(titanic_hclust)
titanic_hclust<-na.omit(titanic_hclust)
titanic_hclust<-scale(titanic_hclust)
a<-dist(titanic_hclust,method = 'euclidean')
titanic_hclustmodel<- hclust(a, method='complete')
plot(titanic_hclustmodel)
