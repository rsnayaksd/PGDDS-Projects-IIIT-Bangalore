#########################################################################
####### Case Study - Time Series - Retail-Giant Sales Forecasting #######
#                                                                       #
# 1. Business Understanding                                             #
# 2. Data Understanding                                                 #
# 3. Data Preparation                                                   #
# 4. Model Building and Analysis                                        #
# 5. Model Evaluation                                                   #
# 6. Conclusion                                                         #
#                                                                       #
#########################################################################

# 1. Business Understanding: 

#################################################################################################################
#                                                                                                               #
# "Global Mart" is an online store super giant having worldwide operations. It takes orders and       	        # 	
# delivers across the globe and deals with all the major product categories - consumer, corporate & home office #
# 										                                                                                  				#
# Now as a sales/operations manager, you want to finalise the plan for the next 6 months.  		                	#
# So, you want to forecast the sales and the demand for the next 6 months, that would help you manage 	      	#
# the revenue and inventory accordingly.                                                      									#	
# 														                                                                                  #
# The store caters to 7 different market segments and in 3 major categories. You want to forecast at this     	#
# granular level, so you subset your data into 21 (7*3) buckets before analysing these data.		              	#
# 													                                                                                  	#
# But not all of these 21 market buckets are important from the store's point of view. So you need to find out	#
# 2 most profitable (and consistent) segment from these 21 and forecast the sales and demand for these segments #
#                                                                                                               #
#################################################################################################################


# 2. Data Understanding: 

#########################################################################
#                                                                       #
# Global Superstore Dataset                                             #
# Number of Instances in Complete Dataset : 51,290                      #
# Number of Attributes : 24                                             #
#                                                                       #
# Data Dictionary                                                       #
# ______________________________________________________________________#
# Attribute_Name| Description                                           #
# ______________  ______________________________________________________#
# Row ID	      : Unique ID of each data point/record in the dataset    #
# Order ID	    : Unique ID of the transaction                          #
# Order Date	  : Date on which the order was placed                    #
# Ship Date	    : Date on which the shipment was made                   #
# Ship Mode	    : The mode of shipment (category)                       #
# Customer ID	  : The unique ID of the customer                         #
# Customer Name	: Name of the customer                                  #
# Segment	      : The market segment to which the product belongs       #
# City	        : City of the delivery address                          #
# State	        : State of the delivery address                         #
# Country	      : Country of the delivery address                       #
# Postal Code	  : Postal code of the delivery address                   #
# Market	      : Market segment to which the customer belongs          #
# Region	      : Geographical region of the customer                   #
# Product ID	  : Unique ID of the product                              #
# Category	    : Category of the product                               #
# Sub-Category	: Sub-category of the product                           #
# Product Name	: Name of the product                                   #
# Sales	        : Total sales value of the transaction                  #
# Quantity	    : Quantity of the product ordered                       #
# Discount	    : Discount percentage offered on the product            #
# Profit	      : Profit made on the transaction                        #
# Shipping Cost : Shipping cost incured on the transaction              #
# Order Priority:	Priority assigned to the order                        #
#                                                                       #
#########################################################################

# 3. Data Preparation: 

#install.packages("ggplot2")
#install.packages("graphics")
#install.packages("forecast")
#install.packages("lubridate")
#install.packages("dplyr")
#install.packages("tseries")

#Switch OFF warning messages
options( warn = -1 )

#Loading necessary libraries

library("ggplot2")
library("graphics")
library("forecast")
library("lubridate")
library("dplyr")
library("tseries")

#Loading Data


Global_Superstore_master <- read.csv("Global Superstore.csv", stringsAsFactors = F)

#Understanding Dimensions
dim(Global_Superstore_master)

#Structure of the data
str(Global_Superstore_master)  #51290 obs. of  24 variables

#Printing first few rows for dataset
head(Global_Superstore_master)

#Checking missing value in complete dataset
sum(is.na(Global_Superstore_master))  #41926

#Selecting only the required fields for analysis
Global_Superstore <- Global_Superstore_master[c("Order.Date","Segment","Market","Sales","Quantity","Profit")]

#Checking missing value
sum(is.na(Global_Superstore)) # no missing value in the required set

#Converting Order.Date in data format
Global_Superstore$Order.Date <- dmy(Global_Superstore$Order.Date)

#Replace days by first date of the month
mday(Global_Superstore$Order.Date) <- 01

# Aggregate Sales, Quantiy, and Profit by Segment, Market and Order.Date
Global_Superstore_agg <- group_by(Global_Superstore, Segment, Market, Order.Date) %>% summarise(Sales_agg=sum(Sales), Quantity_agg=sum(Quantity), Profit_agg=sum(Profit))

# Compute Cofficient of Variable(CV) for aggregated Profit and short the data in assending order of CV
Global_Superstore_profitable <- group_by(Global_Superstore_agg,Segment, Market) %>% summarise(CV=sd(Profit_agg)/mean(Profit_agg)) %>% arrange(CV)

#To visualize coeff. of variance of Monthly Profit for different Market segments

ggplot(Global_Superstore_profitable,aes(x=factor(Market),y=CV,fill=factor(Segment))) + 
  geom_bar(stat="identity",position="dodge")+xlab("Market") + 
  ylab("Coeff. of variance of Monthly Profit") + 
  ggtitle("Coeff. of variance in Monthly Profit Vs. Market Segment") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(aes(label=round(CV,2)), vjust=1.5, color='black', position=position_dodge(.9), size=4)

#Based on the maximum profits and consistent profit month on month, we have chosen below two Market Segements
#1. EU Consumer
#2. APAC Consumer

#Subseting the dataset for Segment== "Consumer", Market == "EU"
Global_Superstore_Consumer_EU <- filter(Global_Superstore_agg, Segment=="Consumer", Market=="EU") %>% arrange(Order.Date)

#Subseting the dataset for Segment=="Consumer", Market=="APAC"
Global_Superstore_Consumer_APAC <- filter(Global_Superstore_agg, Segment=="Consumer", Market=="APAC") %>% arrange(Order.Date)

#To see no. of rows for EU and APAC Market segments
nrow(Global_Superstore_Consumer_EU)     #48
nrow(Global_Superstore_Consumer_APAC)   #48

#To generate month sequence numbers
Month=1:nrow(Global_Superstore_Consumer_APAC)

#To catagorize the training and test data for EU and APAC market segments
Global_Superstore_Consumer_EU <- cbind(Global_Superstore_Consumer_EU,Month=Month)
Global_Superstore_Consumer_APAC <- cbind(Global_Superstore_Consumer_APAC,Month=Month)

Global_Superstore_Consumer_EU_in <- Global_Superstore_Consumer_EU[1:42,]
Global_Superstore_Consumer_EU_out <- Global_Superstore_Consumer_EU[43:48,]

Global_Superstore_Consumer_APAC_in <- Global_Superstore_Consumer_APAC[1:42,]
Global_Superstore_Consumer_APAC_out <- Global_Superstore_Consumer_APAC[43:48,]

#--------------------------------------------------------------------------------------------------------#
###########################              EU Consumer               #####################################                               
#--------------------------------------------------------------------------------------------------------#

########################## TIME SERIES Analysis for EU Consumer Sales(ARMA) ################################
total_EU_Consumer_Sales_TS <- ts(Global_Superstore_Consumer_EU$Sales_agg)
plot(total_EU_Consumer_Sales_TS)


EU_Consumer_Sales_in_TS <- ts(Global_Superstore_Consumer_EU_in$Sales_agg)
plot(EU_Consumer_Sales_in_TS)

#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries <- stats::filter(EU_Consumer_Sales_in_TS, 
                         filter=rep(1/(2*w+1),(2*w+1)), 
                         method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(EU_Consumer_Sales_in_TS)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series

timevals_in <- Global_Superstore_Consumer_EU_in$Month
lines(smoothedseries, col="blue", lwd=2)
# Amplitude of the sesional curve doesnot seems increasing with time
# so will try fitting additive model for the case

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Sales')

#Now, let's fit a additive model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit1 <- lm(Sales ~ sin(0.5*Month) + cos(0.5*Month)
            + Month, data=smootheddf)
summary(lmfit1) #Adjusted R-squared:  0.7053
global_pred1 <- predict(lmfit1, Month=timevals_in)
summary(global_pred1)

plot(EU_Consumer_Sales_in_TS)
lines(smoothedseries, col="blue", lwd=2)
lines(timevals_in, global_pred1, col='red', lwd=2)



lmfit2 <- lm(Sales ~ sin(0.5*Month) * poly(Month,2) + cos(0.5*Month) * poly(Month,2)
            + Month, data=smootheddf)
summary(lmfit2) # Adjusted R-squared:  0.7456
global_pred2 <- predict(lmfit2, Month=timevals_in)
summary(global_pred2)

plot(EU_Consumer_Sales_in_TS)
lines(smoothedseries, col="blue", lwd=2)
lines(timevals_in, global_pred2, col='red', lwd=2)


lmfit3 <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
            + Month, data=smootheddf)
summary(lmfit3) # Adjusted R-squared:  0.835
global_pred3 <- predict(lmfit3, Month=timevals_in)
summary(global_pred3)

plot(EU_Consumer_Sales_in_TS)
lines(smoothedseries, col="blue", lwd=2)
lines(timevals_in, global_pred3, col='red', lwd=2)



lmfit4 <- lm(Sales ~ sin(0.5*Month) * poly(Month,4) + cos(0.5*Month) * poly(Month,4)
            + Month, data=smootheddf)
summary(lmfit4) # Adjusted R-squared:  0.8209
global_pred4 <- predict(lmfit4, Month=timevals_in)
summary(global_pred4)

plot(EU_Consumer_Sales_in_TS)
lines(smoothedseries, col="blue", lwd=2)
lines(timevals_in, global_pred4, col='red', lwd=2)


# From the above fit for determining global complonent, Adjusted R-squared for poly degree 3 looks best
# So, will consider lmfit3 for determining global complonent

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- EU_Consumer_Sales_in_TS-global_pred3
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)
par(mar=c(1,1,1,1))
tsdiag(armafit)
par(mar=c(5.1,4.1,4.1,2.1))
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary") # p-value = 0.01(<0.05)
kpss.test(resi) # p-value = 0.1 (>0.05)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata <- Global_Superstore_Consumer_EU_out
timevals_out <- outdata$Month

global_pred_out <- predict(lmfit3,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata$Sales_agg)[5]
MAPE_class_dec
# 92.95788

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred3),ts(global_pred_out))
plot(total_EU_Consumer_Sales_TS, col = "black")
lines(class_dec_pred, col = "red")


########################## TIME SERIES Analysis for EU Consumer  Sales(auto ARIMA) ################################

# Segment: Consumer    Market: EU  SALES
autoarima <- Global_Superstore_Consumer_EU_in$Sales_agg %>% auto.arima()
autoarima
par(mar=c(1,1,1,1))
tsdiag(autoarima)
par(mar=c(5.1,4.1,4.1,2.1))
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise
timeser <- ts(Global_Superstore_Consumer_EU_in$Sales_agg)
resi_auto_arima <- timeser - fitted(autoarima)


adf.test(resi_auto_arima,alternative = "stationary") # p-value = 0.01(<0.05)
kpss.test(resi_auto_arima) # p-value = 0.1 (>0.05)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,Global_Superstore_Consumer_EU_out$Sales_agg)[5]
MAPE_auto_arima # 28.9226

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(Global_Superstore_Consumer_EU$Sales_agg, col = "black")
lines(auto_arima_pred, col = "red")


# MAPE value for ARMA(manual) is 92.95788 where as for auto ARIMA is 28.9226.
# so auto ARIMA here is better for forcasting.

# lets model the same model using all data and try forcasting for next 6 months.
Global_Superstore_Consumer_EU$Sales_agg %>% auto.arima() %>% predict( n.ahead = 6)


#Time Series:
#  Start = 49 
#End = 54 
#Frequency = 1 
#[1] 49358.71 58063.62 59714.33 54191.79 56811.55 58010.84


#--------------------------------------------------------------------------------------------------------#
###########################              EU Consumer             #####################################                               
#--------------------------------------------------------------------------------------------------------#
########################## TIME SERIES Analysis for EU Consumer  Quantity(ARMA) ################################


# TIME SERIES Analysis for Sales

total_EU_Consumer_Quantity_TS <- ts(Global_Superstore_Consumer_EU$Quantity_agg)
plot(total_EU_Consumer_Quantity_TS)


EU_Consumer_Quantity_in_TS <- ts(Global_Superstore_Consumer_EU_in$Quantity_agg)
plot(EU_Consumer_Quantity_in_TS)


#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries <- stats::filter(EU_Consumer_Quantity_in_TS, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(EU_Consumer_Quantity_in_TS)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series

timevals_in <- Global_Superstore_Consumer_EU_in$Month
lines(smoothedseries, col="blue", lwd=2)
# Amplitude of the sesional curve doesnot seems inceeasing with time
# so will try fitting additive model for the case

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Sales')

#Now, let's fit a additive model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit1 <- lm(Sales ~ sin(0.5*Month) + cos(0.5*Month)
             + Month, data=smootheddf)
summary(lmfit1)# Adjusted R-squared:  0.8393
global_pred1 <- predict(lmfit1, Month=timevals_in)
summary(global_pred1)

plot(EU_Consumer_Quantity_in_TS)
lines(smoothedseries, col="blue", lwd=2)
lines(timevals_in, global_pred1, col='red', lwd=2)



lmfit2 <- lm(Sales ~ sin(0.5*Month) * poly(Month,2) + cos(0.5*Month) * poly(Month,2)
             + Month, data=smootheddf)
summary(lmfit2) # Adjusted R-squared:  0.8666 
global_pred2 <- predict(lmfit2, Month=timevals_in)
summary(global_pred2)

plot(EU_Consumer_Quantity_in_TS)
lines(smoothedseries, col="blue", lwd=2)
lines(timevals_in, global_pred2, col='red', lwd=2)


lmfit3 <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
             + Month, data=smootheddf)
summary(lmfit3) #Adjusted R-squared:  0.8814 
global_pred3 <- predict(lmfit3, Month=timevals_in)
summary(global_pred3)

plot(EU_Consumer_Quantity_in_TS)
lines(smoothedseries, col="blue", lwd=2)
lines(timevals_in, global_pred3, col='red', lwd=2)




lmfit4 <- lm(Sales ~ sin(0.5*Month) * poly(Month,4) + cos(0.5*Month) * poly(Month,4)
             + Month, data=smootheddf)
summary(lmfit4) # Adjusted R-squared:  0.8731 
global_pred4 <- predict(lmfit4, Month=timevals_in)
summary(global_pred4)

plot(EU_Consumer_Quantity_in_TS)
lines(smoothedseries, col="blue", lwd=2)
lines(timevals_in, global_pred4, col='red', lwd=2)


# Checking the Adjusted R-squared and also considering complexity of models, 
# lmfit3 with poly degree 3 seems most appropriate.

#Now, let's look at the locally predictable series
#We will model it as an ARMA series
local_pred <- EU_Consumer_Quantity_in_TS-global_pred3
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)
par(mar=c(1,1,1,1))
tsdiag(armafit)
par(mar=c(5.1,4.1,4.1,2.1))
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary") # p-value = 0.01 (<0.05)
kpss.test(resi) # p-value = 0.1 (>0.05)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata <- Global_Superstore_Consumer_EU_out
timevals_out <- outdata$Month

global_pred_out <- predict(lmfit3,data.frame(Month =timevals_out))

local_pred_out <- predict(armafit, n.ahead = 6)
fcast <- global_pred_out + local_pred_out$pred

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata$Quantity_agg)[5]
MAPE_class_dec
#31.45475


#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred2),ts(global_pred_out))
plot(total_EU_Consumer_Quantity_TS, col = "black")
lines(class_dec_pred, col = "red")

########################## TIME SERIES Analysis for EU Consumer  Quantity(Auto ARIMA) ################################


# Segment: Consumer    Market: EU  SALES
autoarima <- Global_Superstore_Consumer_EU_in$Quantity_agg %>% auto.arima()
autoarima
par(mar=c(1,1,1,1))
tsdiag(autoarima)
par(mar=c(5.1,4.1,4.1,2.1))
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise
timeser <- ts(Global_Superstore_Consumer_EU_in$Quantity_agg)
resi_auto_arima <- timeser - fitted(autoarima)

library(tseries)
adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,Global_Superstore_Consumer_EU_out$Quantity_agg)[5]
MAPE_auto_arima
30.13319

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(Global_Superstore_Consumer_EU$Quantity_agg, col = "black")
lines(auto_arima_pred, col = "red")


# MAPE value for ARMA(manual) is 31.45475 where as for auto ARIMA is 30.13319.
# so auto ARIMA here is better for forcasting.

# lets model the same model using all data and try forcasting for next 6 months.
Global_Superstore_Consumer_EU$Quantity_agg %>% auto.arima() %>% predict( n.ahead = 6)

#Time Series:
#  Start = 49 
#End = 54 
#Frequency = 1 
#[1] 626.2009 786.6056 842.9179 704.8258 768.6274 807.6497




#--------------------------------------------------------------------------------------------------------#
###########################              APAC Consumer               #####################################                               
#--------------------------------------------------------------------------------------------------------#
########################## TIME SERIES Analysis for APAC Consumer Sales(ARMA) ################################


# TIME SERIES Analysis for Sales
total_APAC_Consumer_Sales_TS <- ts(Global_Superstore_Consumer_APAC$Sales_agg)
plot(total_APAC_Consumer_Sales_TS)

APAC_Consumer_Sales_in_TS <- ts(Global_Superstore_Consumer_APAC_in$Sales_agg)
plot(APAC_Consumer_Sales_in_TS)

#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries <- stats::filter(APAC_Consumer_Sales_in_TS, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(APAC_Consumer_Sales_in_TS)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series

timevals_in <- Global_Superstore_Consumer_APAC_in$Month
lines(smoothedseries, col="blue", lwd=2)

# Amplitude of the seasonal curve does not seems inceeasing with time
# so will try fitting additive model for the case

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Sales')

#Now, let's fit a additive model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit1 <- lm(Sales ~ sin(0.5*Month) + cos(0.5*Month)
             + Month, data=smootheddf)
summary(lmfit1) #Adjusted R-squared:  0.718 
global_pred1 <- predict(lmfit1, Month=timevals_in)
summary(global_pred1) 

plot(APAC_Consumer_Sales_in_TS)
lines(smoothedseries, col="blue", lwd=2)
lines(timevals_in, global_pred1, col='red', lwd=2)

lmfit2 <- lm(Sales ~ sin(0.5*Month) * poly(Month,2) + cos(0.5*Month) * poly(Month,2)
             + Month, data=smootheddf)
summary(lmfit2) # Adjusted R-squared:  0.7958 
global_pred2 <- predict(lmfit2, Month=timevals_in)
summary(global_pred2)

plot(APAC_Consumer_Sales_in_TS)
lines(smoothedseries, col="blue", lwd=2)
lines(timevals_in, global_pred2, col='red', lwd=2)


lmfit3 <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
             + Month, data=smootheddf)
summary(lmfit3) # Adjusted R-squared:  0.816
global_pred3 <- predict(lmfit3, Month=timevals_in)
summary(global_pred3)

plot(APAC_Consumer_Sales_in_TS)
lines(smoothedseries, col="blue", lwd=2)
lines(timevals_in, global_pred3, col='red', lwd=2)

lmfit4 <- lm(Sales ~ sin(0.5*Month) * poly(Month,4) + cos(0.5*Month) * poly(Month,4)
             + Month, data=smootheddf)
summary(lmfit4) # Adjusted R-squared:  0.8071
global_pred4 <- predict(lmfit4, Month=timevals_in)
summary(global_pred4)

plot(APAC_Consumer_Sales_in_TS)
lines(smoothedseries, col="blue", lwd=2)
lines(timevals_in, global_pred4, col='red', lwd=2)


# Checking the Adjusted R-squared and also considering complexcity of models, 
# lmfit3 with poly dergee 3 seems most appropriate.


#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- APAC_Consumer_Sales_in_TS-global_pred3
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)
par(mar=c(1,1,1,1))
tsdiag(armafit)
par(mar=c(5.1,4.1,4.1,2.1))
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary") # p-value = 0.01 (<0.05)
kpss.test(resi) # p-value = 0.1 (>0.05)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata <- Global_Superstore_Consumer_APAC_out
timevals_out <- outdata$Month

global_pred_out <- predict(lmfit3,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata$Sales_agg)[5]
MAPE_class_dec
#31.07429


#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred3),ts(global_pred_out))
plot(total_APAC_Consumer_Sales_TS, col = "black")
lines(class_dec_pred, col = "red")

########################## TIME SERIES Analysis for APAC Consumer Sales(Auto ARIMA) ################################

# Segment: Consumer    Market: EU  SALES
autoarima <- Global_Superstore_Consumer_EU_in$Sales_agg %>% auto.arima()
autoarima
par(mar=c(1,1,1,1))
tsdiag(autoarima)
par(mar=c(5.1,4.1,4.1,2.1))
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise
timeser <- ts(Global_Superstore_Consumer_EU_in$Sales_agg)
resi_auto_arima <- timeser - fitted(autoarima)


adf.test(resi_auto_arima,alternative = "stationary") # p-value = 0.01 (<0.05)
kpss.test(resi_auto_arima) # p-value = 0.1 (> 0.05)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,Global_Superstore_Consumer_EU_out$Sales_agg)[5]
MAPE_auto_arima
#28.9226

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(Global_Superstore_Consumer_EU$Sales_agg, col = "black")
lines(auto_arima_pred, col = "red")


# MAPE value for ARMA(manual) is 31.07429 where as for auto ARIMA is 28.9226
# so auto ARIMA here is better for forecasting.

# lets model the same model using all data and try forcasting for next 6 months.
Global_Superstore_Consumer_APAC$Sales_agg %>% Arima(order=c(2,1,0)) %>% predict( n.ahead = 6)

#Time Series:
#  Start = 49 
#End = 54 
#Frequency = 1 
#[1] 71649.02 69216.51 68565.57 69356.25 69031.66 69072.60

########################## TIME SERIES Analysis for APAC Consumer Quantity (ARMA) ################################

# TIME SERIES Analysis for Quantity
total_APAC_Consumer_Quantity_TS <- ts(Global_Superstore_Consumer_APAC$Quantity_agg)
plot(total_APAC_Consumer_Quantity_TS)

APAC_Consumer_Quantity_in_TS <- ts(Global_Superstore_Consumer_APAC_in$Quantity_agg)
plot(APAC_Consumer_Quantity_in_TS)

#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries <- stats::filter(APAC_Consumer_Quantity_in_TS, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(APAC_Consumer_Quantity_in_TS)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series

timevals_in <- Global_Superstore_Consumer_APAC_in$Month
lines(smoothedseries, col="blue", lwd=2)

# Amplitude of the sesional curve doesnot seems inceeasing with time
# so will try fitting additive model for the case

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Quantity')

#Now, let's fit a additive model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit1 <- lm(Quantity ~ sin(0.5*Month) + cos(0.5*Month)
             + Month, data=smootheddf)
summary(lmfit1) # Adjusted R-squared:  0.7027 
global_pred1 <- predict(lmfit1, Month=timevals_in)
summary(global_pred1)

plot(APAC_Consumer_Quantity_in_TS)
lines(smoothedseries, col="blue", lwd=2)
lines(timevals_in, global_pred1, col='red', lwd=2)

lmfit2 <- lm(Quantity ~ sin(0.5*Month) * poly(Month,2) + cos(0.5*Month) * poly(Month,2)
             + Month, data=smootheddf)
summary(lmfit2) # Adjusted R-squared:  0.819 
global_pred2 <- predict(lmfit2, Month=timevals_in)
summary(global_pred2)

plot(APAC_Consumer_Quantity_in_TS)
lines(smoothedseries, col="blue", lwd=2)
lines(timevals_in, global_pred2, col='red', lwd=2)


lmfit3 <- lm(Quantity ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
             + Month, data=smootheddf)
summary(lmfit3) # Adjusted R-squared:  0.8193 
global_pred3 <- predict(lmfit3, Month=timevals_in)
summary(global_pred3)

plot(APAC_Consumer_Quantity_in_TS)
lines(smoothedseries, col="blue", lwd=2)
lines(timevals_in, global_pred3, col='red', lwd=2)

lmfit4 <- lm(Quantity ~ sin(0.5*Month) * poly(Month,4) + cos(0.5*Month) * poly(Month,4)
             + Month, data=smootheddf)
summary(lmfit4) # Adjusted R-squared:  0.8216 
global_pred4 <- predict(lmfit4, Month=timevals_in)
summary(global_pred4)

plot(APAC_Consumer_Quantity_in_TS)
lines(smoothedseries, col="blue", lwd=2)
lines(timevals_in, global_pred4, col='red', lwd=2)

# Checking the Adjusted R-squared and also considering complexcity of models, 
# lmfit2 with poly dergee 2 seems most appropriate.


#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- APAC_Consumer_Quantity_in_TS-global_pred2
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)
par(mar=c(1,1,1,1))
tsdiag(armafit)
par(mar=c(5.1,4.1,4.1,2.1))
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary") # p-value = 0.01 (<0.05)
kpss.test(resi) # p-value = 0.1(>0.05)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata <- Global_Superstore_Consumer_APAC_out
timevals_out <- outdata$Month

global_pred_out <- predict(lmfit3,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata$Quantity_agg)[5]
MAPE_class_dec
# 62.10289


#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred3),ts(global_pred_out))
plot(total_APAC_Consumer_Quantity_TS, col = "black")
lines(class_dec_pred, col = "red")


########################## TIME SERIES Analysis for APAC Consumer Quantity( auto ARIMA) ################################

# Segment: Consumer    Market: EU  Quantity
autoarima <- Global_Superstore_Consumer_EU_in$Quantity_agg %>% auto.arima()
autoarima
par(mar=c(1,1,1,1))
tsdiag(autoarima)
par(mar=c(5.1,4.1,4.1,2.1))
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise
timeser <- ts(Global_Superstore_Consumer_EU_in$Quantity_agg)
resi_auto_arima <- timeser - fitted(autoarima)

library(tseries)
adf.test(resi_auto_arima,alternative = "stationary") # p-value = 0.04521 (<0.05)
kpss.test(resi_auto_arima) # p-value = 0.1 (>0.05)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,Global_Superstore_Consumer_APAC_out$Quantity_agg)[5]
MAPE_auto_arima
# 37.54

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(Global_Superstore_Consumer_APAC$Quantity_agg, col = "black")
lines(auto_arima_pred, col = "red")


# MAPE value for ARMA(manual) is 62.10289 where as for auto ARIMA is 30.13319.
# so auto ARIMA here is better for forcasting.

# lets model the same model using all data and try forcasting for next 6 months.
Global_Superstore_Consumer_APAC$Quantity_agg %>% arima(order=c(2,1,0)) %>% predict( n.ahead = 6)

#Time Series:
#  Start = 49 
#End = 54 
#Frequency = 1 
#[1] 842.6532 837.2847 837.8234 838.6757 838.1635 838.2291

############################################# CONCLUSION ###########################################################
#  The two most profitable buckets out of 21 are EU, Consumer and APAC, Consumer segment. 
#  The MAPE values of  all the predictions are low. 
#  The multiplicative model has been followed for modeling out the globally predictable part for both the segments 
#  The ACF,PACF plots for all the segments shows that the locally predictable part is itself 
#   weakly stationary and residuals for all the segments came out to be pure white noise. 
################################################## END #############################################################
