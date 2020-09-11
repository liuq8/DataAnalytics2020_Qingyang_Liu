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
EPI2010=read_excel('/Users/claireliu/Documents/MSBA/6962 Data Analytics/2010EPI_data.xls')
install.packages("readxl")
library("readxl")

EPI_data <- read.csv()
attach(EPI2010)
fix(EPI2010)
View(EPI2010)
EPI=EPI2010$`2010 Environmental Performance Index (EPI)`
tf <- is.na(EPI)
E <- EPI[!tf]
EPI
