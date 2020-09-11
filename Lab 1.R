days <- c('Mon','Tue','Wed','Thu','Fri','Sat','Sun')
temp <- c(28,30.5,32,31.2,29.3,27.9,26.4)
snowed <- c('T','T','F','F','T','T','F')
help("data.frame")
RPI_weather_week <- data.frame(days,temp,snowed)

RPI_weather_week
head(RPI_weather_week)

str(RPI_weather_week)
summary(RPI_weather_week)

RPI_weather_week[1,]
RPI_weather_week[,1]

RPI_weather_week[,'snowed']
RPI_weather_week[,'days']
RPI_weather_week[,'temp']
RPI_weather_week[1:5,c('days','temp')]
RPI_weather_week$temp
subset(RPI_weather_week,subset=snowed=='T')

sorted.snowed <- order(RPI_weather_week['snowed'])
sorted.snowed
RPI_weather_week[sorted.snowed,]

dec.snow <- order(-RPI_weather_week$temp)
dec.snow
RPI_weather_week[dec.snow,]

empty.DataFrame <- data.frame()
v1 <- 1:10
v1
letters
v2 <- letters[1:10]
df <- data.frame(col.name.1=v1, col.name.2=v2)
write.csv(df,file='saved_df1.csv')
df2 <- read.csv('saved_df1.csv')
df2

GPW3=read.csv('/Users/claireliu/Documents/MSBA/6962 Data Analytics/GPW3_GRUMP_SummaryInformation_2010.csv')

GPW3

hist(GPW3$Resolution)
install.packages("readxl")
library("readxl")
EPI2010=read_excel('/Users/claireliu/Documents/MSBA/6962 Data Analytics/2010EPI_data.xls')

EPI_data <- read.csv('/Users/claireliu/Documents/MSBA/6962 Data Analytics/EPI_data.csv')
attach(EPI_data)
fix(EPI_data)
View(EPI_data)
EPI=EPI_data$EPI
tf <- is.na(EPI)
E <- EPI[!tf]
EPI

summary(EPI)
fivenum(EPI)
stem(EPI)
hist(EPI)
hist(EPI,seq(30.,95.,1.0),prob=TRUE)
lines(density(EPI,na.rm=TRUE,bw=1))
rug(EPI)

#Excercise 1
plot(ecdf(EPI),do.points=FALSE,verticals = TRUE)
par(pty='s')
qqnorm(EPI); qqline(EPI)
x <- seq(30,95,1)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q plot for tdsn")
qqline(x)

plot(ecdf(DALY),do.points=FALSE,verticals = TRUE)
par(pty='s')
qqnorm(DALY); qqline(DALY)
x <- seq(30,95,1)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q plot for tdsn")
qqline(x)

plot(ecdf(WATER_H),do.points=FALSE,verticals = TRUE)
par(pty='s')
qqnorm(WATER_H); qqline(WATER_H)
x <- seq(30,95,1)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q plot for tdsn")
qqline(x)

boxplot(EPI,DALY)
qqplot(EPI,DALY)
boxplot(WATER_H,AIR_H)
qqplot(WATER_H,AIR_H)

#Exercise 2
EPILand <- EPI[!Landlock]
Eland <- EPI[!is.na(EPILand)]
hist(Eland)
hist(Eland,seq(30.,95.,1.0),prob=TRUE)

plot(ecdf(Eland),do.points=FALSE,verticals = TRUE)
par(pty='s')
qqnorm(Eland); qqline(Eland)
x <- seq(30,95,1)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q plot for tdsn")
qqline(x)

EPI_South_Asia <- EPI[EPI_regions=="South Asia"]
EPI_South_Asia


View(GPW3)
Diff95 <- GPW3$Diff95
plot(ecdf(Diff95),do.points=FALSE,verticals = TRUE)
par(pty='s')
qqnorm(EPI); qqline(EPI)
x <- seq(30,95,1)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q plot for tdsn")
qqline(x)

AsiaDiff95 <- Diff59[GPW3$ContinentName=="Asia"]
hist(AsiaDiff95)
boxplot(AsiaDiff95)


water_treatment<- read.csv('/Users/claireliu/Documents/MSBA/6962 Data Analytics/water-treatment.csv')
View(water_treatment)
PH.E <- water_treatment$PH.E
DQO.E <- water_treatment$DQO.E

plot(ecdf(PH.E),do.points=FALSE,verticals = TRUE)
par(pty='s')
qqnorm(EPI); qqline(EPI)
x <- seq(30,95,1)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q plot for tdsn")
qqline(x)

DatePH7.5 <- water_treatment$DATE[PH.E>=7.5]
boxplot(PH.E)
