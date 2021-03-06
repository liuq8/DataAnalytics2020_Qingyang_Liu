library(readcsv)
EPI_data <- read.csv("Box/Documents/MSBA/6962 Data Analytics/EPI_data.csv")
plot(ecdf(EPI_data$EPI),do.points=FALSE,verticals = TRUE) 
plot(ecdf(EPI_data$EPI),do.points=TRUE,verticals = TRUE)
par(pty='s')
qqnorm(EPI_data$EPI)
qqline(EPI_data$EPI)
x<-seq(30,95,1)
x
x2<-seq(30,95,2)
x3<-seq(30,96,3)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q plot")
qqline(x)

plot(ecdf(EPI_data$DALY),do.points=FALSE,verticals = TRUE) 
plot(ecdf(EPI_data$DALY),do.points=TRUE,verticals = TRUE)
par(pty='s')
qqnorm(EPI_data$DALY)
qqline(EPI_data$DALY)
qqplot(qt(ppoints(250),df=5),EPI_data$DALY,xlab="Q-Q plot DALY")

plot(ecdf(EPI_data$WATER_H),do.points=FALSE,verticals = TRUE) 
plot(ecdf(EPI_data$WATER_H),do.points=TRUE,verticals = TRUE)
par(pty='s')
qqnorm(EPI_data$WATER_H)
qqline(EPI_data$WATER_H)

boxplot(EPI_data$EPI,EPI_data$DALY)
boxplot(EPI_data$EPI,EPI_data$ENVHEALTH)
boxplot(EPI_data$DALY,EPI_data$ECOSYSTEM)
boxplot(EPI_data$AIR_H,EPI_data$WATER_H)
boxplot(EPI_data$AIR_E,EPI_data$WATER_E)

multivariate <- read.csv("Box/Documents/MSBA/6962 Data Analytics/multivariate.csv")
View(multivariate)
attach(multivariate)
mm<-lm(multivariate$Homeowners~multivariate$Immigrant)
mm
plot(multivariate$Homeowners~multivariate$Immigrant)
abline(mm)
abline(mm,col=2,lwd=3)
attributes(mm)
mm$coefficients


#in-class work
plot(mtcars$wt,mtcars$mpg)
library(ggplot2)
qplot(mtcars$wt,mtcars$mpg)
qplot(wt,mpg,data=mtcars)
ggplot(mtcars,aes(x=wt,y=mpg))+geom_point()
plot(pressure$temperature,pressure$pressure,type='l')
points(pressure$temperature,pressure$pressure)

lines(pressure$temperature,pressure$pressure/2, col="red")
points(pressure$temperature,pressure$pressure/2, col="blue")
library(ggplot2)
qplot(pressure$temperature,pressure$pressure,geom="line")
qplot(temperature,pressure,data=pressure,geom='line')
ggplot(pressure,aes(x=temperature,y=pressure))+geom_line()+geom_point()

barplot(BOD$demand,names.arg = BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl))
qplot(mtcars$cyl)
qplot(factor(mtcars$cyl))
qplot(factor(cyl),data=mtcars)
ggplot(mtcars,aes(x=factor(cyl)))+geom_bar()

hist(mtcars$mpg)
hist(mtcars$mpg,breaks=10)
hist(mtcars$mpg,breaks=5)
hist(mtcars$mpg,breaks=12)
qplot(mpg,data=mtcars,binwidth=4)
ggplot(mtcars,aes(x=mpg))+geom_histogram(binwidth=4)
ggplot(mtcars,aes(x=mpg))+geom_histogram(binwidth = 5)

plot(ToothGrowth$supp,ToothGrowth$len)
boxplot(len~supp, data=ToothGrowth)
boxplot(len~supp+dose, data=ToothGrowth)
library(ggplot2)
qplot(ToothGrowth$supp,ToothGrowth$len,geom='boxplot')
qplot(supp,len,data=ToothGrowth,geom='boxplot')
ggplot(ToothGrowth,aes(x=supp,y=len))+geom_boxplot()
qplot(interaction(ToothGrowth$supp,ToothGrowth$dose),ToothGrowth$len,geom='boxplot')
qplot(interaction(supp,dose),len,data=ToothGrowth,geom='boxplot')
ggplot(ToothGrowth,aes(x=interaction(supp,dose),y=len))+geom_boxplot()
