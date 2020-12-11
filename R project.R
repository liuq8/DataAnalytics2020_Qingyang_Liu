Original_data=read.csv('/Users/claireliu/Box/Documents/MSBA/6962 Data Analytics/R project dataset/Sugar_Futures.csv',stringsAsFactors = FALSE)
View(Original_data)
#split this into train test sets. test is 0.3 of the total set, which give us until 2008 data 
test=Original_data[196:276,]
train=Original_data[1:195,]
View(train)
View(test)
write.csv(train,paste(path_out,'train.csv',sep = ''))
write.csv(test,paste(path_out,'test.csv',sep = ''))

#convert Year_Month to date format
train$Year_Month<- as.Date(paste(train$Year_Month,"/01",sep=""),formate="%Y/%m/%d")
test$Year_Month<- as.Date(paste(test$Year_Month,"/01",sep=""),formate="%Y/%m/%d")
class(train$Year_Month)

str(train)

#train test split 
train_y=train$Sugar_Futures
train_X=train[,-11]
test_X=test[,-11]
test_y=test$Sugar_Futures
#save train test data
path_out = '/Users/claireliu/Box/Documents/MSBA/6962 Data Analytics/R project dataset/'
write.csv(train_X,paste(path_out,'train_X.csv',sep = ''))
write.csv(train_y,paste(path_out,'train_y.csv',sep = ''))
write.csv(test_X,paste(path_out,'test_X.csv',sep = ''))
write.csv(test_y,paste(path_out,'test_y.csv',sep = ''))
View(train_X)
median(train_y)
mean(train_y)
print(class(train_y))

#generate line plots to look at trends and general relationships 
library(ggplot2)
ggplot(train,aes(x=Year_Month))+ geom_line(aes(y=Sugar_Futures))+ggtitle("Sugar Futures By Month")
ggplot(train,aes(x=Year_Month))+ geom_line(aes(y=Soybean_Futures))+ggtitle("Soybean Futures By Month")
ggplot(train,aes(x=Year_Month))+ geom_line(aes(y=Sales))+ggtitle("Soybean Sales By Month")
ggplot(train,aes(x=Year_Month))+ geom_line(aes(y=Domestic_Production))+ggtitle("Domestic Production Sales By Month")
train$Net_Trade <- train$Import-train$Export
ggplot(train,aes(x=Year_Month))+ geom_line(aes(y=Net_Trade))+ggtitle("Net Trade By Month")
ggplot(train,aes(x=Year_Month))+ geom_line(aes(y=Import),color='green')+ggtitle("Import&Export&Net Trade By Month")+ geom_line(aes(y=Export),color='red')+geom_line(aes(y=Net_Trade))
ggplot(train,aes(x=Year_Month))+ geom_line(aes(y=Crude_Oil_Price))+ggtitle("Crude Oil Price By Month")
ggplot(train,aes(x=Year_Month))+ geom_line(aes(y=Sugar_Import_Price))+ggtitle("Sugar Import Sales Price By Month")

# basic stats about the dependent variable 
max(train$Sugar_Futures)
min(train$Sugar_Futures)
mean(train$Sugar_Futures)
boxplot(train$Sugar_Futures,main="Sugar Futures")

#naive model-univariate 
install.packages("forecast")
library(forecast)
nrow(test) #determine h value
naive_model <- naive(train_y,h=81)
summary(naive_model)

forecast_benchmark <- data.frame(test_y) #create a benchmark table to evaulate accuracy of each model
forecast_benchmark$naive = 10.82 #nive model result
naive_MAE <- mean(abs(test_y-forecast_benchmark$naive)/test_y)
naive_MAE #0.39967, quite high
install.packages("Metrics")
library(Metrics)
naive_MSE <- mse(test_y,forecast_benchmark$naive)
naive_MSE #108.0434

#Linear Model without lags
linear_model1 <- lm(train_y~train_X$Import+train_X$Export+train_X$Domestic_Production
                   +train_X$Sales+train_X$Sugar_Import_Price+train_X$Soybean_Futures+train_X$Crude_Oil_Price)
summary(linear_model1)
plot(resid(linear_model1),main = "Reisudal Plot: Linear_Model1")

train_X$Net_Trade <- train_X$Import-train_X$Export
linear_model2 <- lm(train_y~train_X$Net_Trade+train_X$Domestic_Production
                    +train_X$Sales+train_X$Sugar_Import_Price+train_X$Soybean_Futures+train_X$Crude_Oil_Price)
summary(linear_model2)
plot(resid(linear_model2),main = "Reisudal Plot: Linear_Model2")

#Log Model without lag
log_model1 <- lm(log(train_y)~log(train_X$Import)+log(train_X$Export)+log(train_X$Domestic_Production)
                    +log(train_X$Sales)+log(train_X$Sugar_Import_Price)+log(train_X$Soybean_Futures)+log(train_X$Crude_Oil_Price))
summary(log_model1)
plot(resid(log_model1),main = 'Residual plot: Log_Model1')

log_model2 <- lm(log(train_y)~log(train_X$Net_Trade)+log(train_X$Domestic_Production)
                    +log(train_X$Sales)+log(train_X$Sugar_Import_Price)+log(train_X$Soybean_Futures)+log(train_X$Crude_Oil_Price))
summary(log_model2)
plot(resid(log_model2),main = 'Residual plot: Log_Model2')


#look at autocorrelation and particial arutocorrelation
acf_train_y <- acf(train$Sugar_Futures)
pacf_train_y <- pacf(train$Sugar_Futures)# pac with 1 lag has a significant impact

# creating lagged variables
install.packages("DataCombine")
library(DataCombine)
train_lag <- slide(train,"Sugar_Futures",NewVar = "Sugar_Futures_Lag",slideBy = -1)
train_lag <- slide(train_lag,"Sugar_Import_Price",NewVar = "Sugar_Import_Price_Lag",slideBy = -1)
train_lag <- slide(train_lag,"Import",NewVar = "Import_Lag",slideBy = -1)
train_lag <- slide(train_lag,"Export",NewVar = "Export_Lag",slideBy = -1)
train_lag <- slide(train_lag,"Domestic_Production",NewVar = "Domestic_Production_Lag",slideBy = -1)
train_lag <- slide(train_lag,"Sales",NewVar = "Sales_Lag",slideBy = -1)
train_lag <- slide(train_lag,"Soybean_Futures",NewVar = "Soybean_Futures_Lag",slideBy = -1)
train_lag <- slide(train_lag,"Crude_Oil_Price",NewVar = "Crude_Oil_Price_Lag",slideBy = -1)
View(train_lag)
head(train_lag)
train_lag <-train_lag[-1,]
#linear model with 1 month lag
#sustitued above three variable with 1 month lag, plus sugar futures with 1 month lag
linear_model1_lag <- lm(train_lag$Sugar_Futures~train_lag$Import_Lag+train_lag$Export_Lag+train_lag$Domestic_Production_Lag
                          +train_lag$Sales_Lag+train_lag$Sugar_Import_Price_Lag+train_lag$Soybean_Futures_Lag
                          +train_lag$Crude_Oil_Price_Lag+train_lag$Sugar_Futures_Lag)
summary(linear_model1_lag)
plot(resid(linear_model1_lag),main = 'Residual plot: Linear_Model1_lag')

linear_model2_lag <- lm(train_lag$Sugar_Futures~(train_lag$Import_Lag-train_lag$Export_Lag)+train_lag$Domestic_Production_Lag
                        +train_lag$Sales_Lag+train_lag$Sugar_Import_Price_Lag+train_lag$Soybean_Futures_Lag
                        +train_lag$Crude_Oil_Price_Lag+train_lag$Sugar_Futures_Lag)
summary(linear_model2_lag)
plot(resid(linear_model1_lag),main = 'Residual plot: Linear_Model2_lag')
#log models with lag
log_model1_lag <- lm(log(train_lag$Sugar_Futures)~log(train_lag$Import_Lag)+log(train_lag$Export_Lag)+log(train_lag$Domestic_Production_Lag)
                 +log(train_lag$Sales_Lag)+log(train_lag$Sugar_Import_Price_Lag)+log(train_lag$Soybean_Futures_Lag)+log(train_lag$Crude_Oil_Price_Lag)+log(train_lag$Sugar_Futures_Lag))
summary(log_model1_lag)
plot(resid(log_model1_lag),main = 'Residual plot: Log_Model1_lag')

log_model2 <- lm(log(train_lag$Sugar_Futures)~log(train_lag$Import_Lag-train_lag$Export_Lag)+log(train_lag$Domestic_Production_Lag)
                 +log(train_lag$Sales_Lag)+log(train_lag$Sugar_Import_Price_Lag)+log(train_lag$Soybean_Futures_Lag)+log(train_lag$Crude_Oil_Price_Lag)+log(train_lag$Sugar_Futures_Lag))
summary(log_model2)
plot(resid(log_model2),main = 'Residual plot: Log_Model2_lag')
# is 2 lags with average necessary?
acf_train_y <- acf(train$Soybean_Futures)
pacf_train_y <- pacf(train$Soybean_Futures)# pac with 1 lag has a significant impact
#with 2 lags
train_lag2 <- slide(train,"Sugar_Futures",NewVar = "Sugar_Futures_Lag",slideBy = -1)
train_lag2<- slide(train_lag2,"Sugar_Import_Price",NewVar = "Sugar_Import_Price_Lag",slideBy = -1)
train_lag2 <- slide(train_lag2,"Import",NewVar = "Import_Lag",slideBy = -1)
train_lag2 <- slide(train_lag2,"Export",NewVar = "Export_Lag",slideBy = -1)
train_lag2 <- slide(train_lag2,"Domestic_Production",NewVar = "Domestic_Production_Lag",slideBy = -1)
train_lag2 <- slide(train_lag2,"Sales",NewVar = "Sales_Lag",slideBy = -1)
train_lag2 <- slide(train_lag2,"Soybean_Futures",NewVar = "Soybean_Futures_Lag",slideBy = -1)
train_lag2 <- slide(train_lag2,"Crude_Oil_Price",NewVar = "Crude_Oil_Price_Lag",slideBy = -1)

train_lag2 <- slide(train_lag2,"Sugar_Futures",NewVar = "Sugar_Futures_Lag2",slideBy = -2)
train_lag2 <- slide(train_lag2,"Sugar_Import_Price",NewVar = "Sugar_Import_Price_Lag2",slideBy = -2)
train_lag2 <- slide(train_lag2,"Import",NewVar = "Import_Lag2",slideBy = -2)
train_lag2 <- slide(train_lag2,"Export",NewVar = "Export_Lag2",slideBy = -2)
train_lag2 <- slide(train_lag2,"Domestic_Production",NewVar = "Domestic_Production_Lag2",slideBy = -2)
train_lag2 <- slide(train_lag2,"Sales",NewVar = "Sales_Lag2",slideBy = -2)
train_lag2 <- slide(train_lag2,"Soybean_Futures",NewVar = "Soybean_Futures_Lag2",slideBy = -2)
train_lag2 <- slide(train_lag2,"Crude_Oil_Price",NewVar = "Crude_Oil_Price_Lag2",slideBy = -2)
View(train_lag2)
head(train_lag2)
train_lag2<-train_lag2[-c(1:2),]

View(train_y_lag2)
train_y_lag2<- data.frame(train_y)
train_y_lag2<- train_y[1:193]

train_lag2$mean_Import<- (train_lag2$Import_Lag+train_lag2$Import_Lag2)/2 
train_lag2$mean_Export<-(train_lag2$Export_Lag+train_lag2$Export_Lag2)/2
train_lag2$mean_Domestic_Production<- (train_lag2$Domestic_Production_Lag+train_lag2$Domestic_Production_Lag2)/2
train_lag2$mean_Sales<-(train_lag2$Sales_Lag+train_lag2$Sales_Lag2)/2
train_lag2$mean_Sugar_Import_Price<-(train_lag2$Sugar_Import_Price_Lag+train_lag2$Sugar_Import_Price_Lag2)/2
train_lag2$mean_Soybean_Futures<-(train_lag2$Soybean_Futures_Lag+train_lag2$Soybean_Futures_Lag2)/2
train_lag2$mean_Crude_Oil_Price<-(train_lag2$Crude_Oil_Price_Lag+train_lag2$Crude_Oil_Price_Lag2)/2
train_lag2$mean_Sugar_Futures<- (train_lag2$Sugar_Futures_Lag+train_lag2$Sugar_Futures_Lag2)/2

linear_model1_lag2 <- lm(train_y_lag2~mean_Import+mean_Export
                         +mean_Domestic_Production+mean_Sales
                         +mean_Sugar_Import_Price+mean_Soybean_Futures
                         +mean_Crude_Oil_Price+mean_Sugar_Futures,data=train_lag2)
summary(linear_model1_lag2)
plot(resid(linear_model1_lag2))
head(test)
#prediction and test data manipulation
test <-Original_data[194:276,]
test_X <- slide(test,"Sugar_Futures",NewVar = "Sugar_Futures_Lag",slideBy = -1)
test_X<- slide(test_X,"Sugar_Import_Price",NewVar = "Sugar_Import_Price_Lag",slideBy = -1)
test_X <- slide(test_X,"Import",NewVar = "Import_Lag",slideBy = -1)
test_X <- slide(test_X,"Export",NewVar = "Export_Lag",slideBy = -1)
test_X <- slide(test_X,"Domestic_Production",NewVar = "Domestic_Production_Lag",slideBy = -1)
test_X <- slide(test_X,"Sales",NewVar = "Sales_Lag",slideBy = -1)
test_X <- slide(test_X,"Crude_Oil_Price",NewVar = "Crude_Oil_Price_Lag",slideBy = -1)
test_X <- slide(test_X,"Soybean_Futures",NewVar = "Soybean_Futures_Lag",slideBy = -1)

test_X <- slide(test_X,"Sugar_Futures",NewVar = "Sugar_Futures_Lag2",slideBy = -2)
test_X <- slide(test_X,"Sugar_Import_Price",NewVar = "Sugar_Import_Price_Lag2",slideBy = -2)
test_X <- slide(test_X,"Import",NewVar = "Import_Lag2",slideBy = -2)
test_X <- slide(test_X,"Export",NewVar = "Export_Lag2",slideBy = -2)
test_X <- slide(test_X,"Domestic_Production",NewVar = "Domestic_Production_Lag2",slideBy = -2)
test_X <- slide(test_X,"Sales",NewVar = "Sales_Lag2",slideBy = -2)
test_X <- slide(test_X,"Crude_Oil_Price",NewVar = "Crude_Oil_Price_Lag2",slideBy = -2)
test_X <- slide(test_X,"Soybean_Futures",NewVar = "Soybean_Futures_Lag2",slideBy = -2)

test_X$mean_Import<- (test_X$Import_Lag+test_X$Import_Lag2)/2 
test_X$mean_Export<-(test_X$Export_Lag+test_X$Export_Lag2)/2
test_X$mean_Domestic_Production<- (test_X$Domestic_Production_Lag+test_X$Domestic_Production_Lag2)/2
test_X$mean_Sales<-(test_X$Sales_Lag+test_X$Sales_Lag2)/2
test_X$mean_Sugar_Import_Price<-(test_X$Sugar_Import_Price_Lag+test_X$Sugar_Import_Price_Lag2)/2
test_X$mean_Soybean_Futures<-(test_X$Soybean_Futures_Lag+test_X$Soybean_Futures_Lag2)/2
test_X$mean_Crude_Oil_Price<-(test_X$Crude_Oil_Price_Lag+test_X$Crude_Oil_Price_Lag2)/2
test_X$mean_Sugar_Futures<- (test_X$Sugar_Futures_Lag+test_X$Sugar_Futures_Lag2)/2
head(test_X)
test_X<-test_X[-c(1:2),]
View(test_X)

test_X_fit<-test_X[,28:35]
head(test_X_fit)


pred_y = predict(linear_model1_lag2,newdata=test_X_fit)
View(pred_y)
View(test_y)
library(Metrics)
mse(test_y,pred_y)
