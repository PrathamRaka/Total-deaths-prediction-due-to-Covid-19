library(tidyverse)
library(ggplot2)
library(zoo)
library(aTSA)
library(tseries)
library(forecast)
library(lubridate)

# Load the csv file first.
dat <- read.csv("covid-data.csv")

class(dat$date)

# Here we change the date column into a date type.
dat$date <- as.Date(dat$date)
class(dat$date)

# We pick the US rows only.
dat_US <- dat[dat$location == "United States",c(3,4,7,8)]


#Plotting the number of deaths in the US
p <- ggplot(dat_US, aes(x=date, y=new_deaths)) +
  geom_line() + scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  xlab("Months") + ylab("New Deaths")+ggtitle("Number of deaths in the US")+
  geom_area(fill="lightblue", color="black")
p

# Let plot the mean to check the first condition if our data is stationary or not.
#The graph above shows the number of deaths from January — August 1st due to Covid19. The red line displays the mean and as we can see above the mean is constantly increasing.
p + geom_smooth(method = lm, col = "red", se = FALSE)


#Trend Line 
p1 <- ggplot(dat_US, aes(x=date, y=total_deaths)) +
  geom_line() + scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  xlab("Months") + ylab("Aggregate Number of Deaths")+ggtitle("Trend Line")+
  geom_area(fill="red", color="black")
p1

# Here we can see the aggregate numbers of death.
dat_US$total_deaths

# Change the data to zoo type
dat_US_Analysis <- dat_US[,c(2,4)]
dat_demo <- zoo(dat_US_Analysis$new_deaths, seq(from = as.Date("2019-12-31"), 
                                            to = as.Date("2020-08-01"), by = 1))

class(dat_demo)

summary(dat_demo)

plot((dat_demo))

# Make the data stationary, by differencing the data.
stationary_data <- diff(dat_demo)
plot(stationary_data)

# To check if it's stationary we conduct a quantitative test. We use the Augmented Dickey-Fuller Test.
# H_0 = The null hypothesis for this test is that there is a unit root.
# H_A = The alternative hypothesis is that the time series is stationary (or trend-stationary).
adf.test(as.matrix(stationary_data)) 

# We select a significance level of 0.05 and since our p-value is 0.01 and smaller then 0.05, we come to the conclusion to reject the null hypothesis. In other words our data is stationary. 
# We use the Auto Correlation Graph

# First we have a look at our acf graph when our data isn't stationary.
acf(dat_demo)

# Here we see that our values are all exceeding the blue line. The goal is to have the values under the blue line and they should be inverted as well.
# To select the p and q values we select the number before the first inverted line.
acf(stationary_data)
pacf(stationary_data) 

plot(stationary_data)


##The auto.arima function selected p=3, d=0 and q=2
# arima has a auto.arima function which gives us the ideal arima model based on our data.
arima_funct <- auto.arima(stationary_data)
arima_funct

# lets use the auto.arima function to forecast 3 weeks

forecast1 <- forecast(arima_funct, h=21)
additional_deaths <- round(sum(forecast1$upper[,2]),0)
print(additional_deaths)


total_number_of_deaths <- round(sum(dat_US_Analysis$new_deaths)+
                                  additional_deaths,0)
print(total_number_of_deaths)

# let us use the auto.arima function to forecast 3 months
forecast2 <- forecast(object = arima_funct, h = 90)
additional_deaths2 <- round(sum(forecast2$upper[,2]),0)
print(additional_deaths2)

total_number_of_deaths2 <- round(sum(dat_US_Analysis$new_deaths)+
                                   additional_deaths2,0)
print(total_number_of_deaths2)

plot(forecast1)

#Below we are just adding the dates and make changes to the x axis for the projected graphs.
#The auto.arima function selected p=3, d=0 and q=2
#Projection graph 2020/08/01–2020/08/21:
delta <- (forecast1$lower[,2]+forecast1$upper[,2])/2

predicted_21 <- data.frame(date=seq(from=18476,by=1,length=21),new_deaths=delta)
original <- data.frame(date=as.numeric(dat_US_Analysis$date),new_deaths=c(0,stationary_data))
z <- rbind(original,predicted_21)
z$date <- as.Date(z$date)


forecast1_data <- as.data.frame(forecast1)
forecast1_data <- data.frame(date=seq(from=18476,by=1,length=21),forecast1_data, y = delta)
forecast1_data$date <- as.Date(forecast1_data$date)

graph <- ggplot(data=z,aes(x=date,y=new_deaths),show.legend = FALSE)+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  geom_line(data=original,aes(x=as.Date(date),y=new_deaths))+
  geom_ribbon(data=forecast1_data,aes(x = date,ymin =Lo.95, ymax =Hi.95), inherit.aes = FALSE,fill = "lightsteelblue2")+
  geom_ribbon(data=forecast1_data,aes(x = date,ymin =Lo.80, ymax =Hi.80), inherit.aes = FALSE,fill = "lightsteelblue3")+
  geom_line(data=forecast1_data,aes(x=date,y=y),size=1,color='purple')+
  ggtitle("Forecasts from ARIMA(3,0,2) with zero mean")+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")+
  labs(x="Date",y="New Deaths")
graph 

#Projected deaths due to Covid-19 in US from 2020/08/01 to 2020/08/21 is 18,589


#Projection graph 2020/08/01–2020/10/31:
delta2 <- (forecast2$lower[,2]+forecast2$upper[,2])/2

predicted_22 <- data.frame(date=seq(from=18476,by=1,length=nrow(forecast2$lower)),new_deaths=delta2) # prediction from ARIMA
original <- data.frame(date=as.numeric(dat_US_Analysis$date),new_deaths=c(0,stationary_data)) # Previous Observations 
z2 <- rbind(original,predicted_22) 
z2$date <- as.Date(z2$date) # convert numeric column to date (creating the base layer of ggplot)

forecast2_data <- as.data.frame(forecast2) # convert forecast data into data.frame
forecast2_data <- data.frame(date=seq(from=18476,by=1,length=90),forecast2_data, y = delta2) #add a date and delta of Lo.95 & Hi.95
forecast2_data$date <- as.Date(forecast2_data$date) # covert to date 

graph <- ggplot(data=z2,aes(x=date,y=new_deaths),show.legend = FALSE)+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  geom_line(data=original,aes(x=as.Date(date),y=new_deaths))+
  geom_ribbon(data=forecast2_data,aes(x = date,ymin =Lo.95, ymax =Hi.95), inherit.aes = FALSE,fill = "lightsteelblue2")+
  geom_ribbon(data=forecast2_data,aes(x = date,ymin =Lo.80, ymax =Hi.80), inherit.aes = FALSE,fill = "lightsteelblue3")+
  geom_line(data=forecast2_data,aes(x=date,y=y),size=1,color='purple')+
  ggtitle("Forecasts from ARIMA(3,0,2) with zero mean")+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")+
  labs(x="Date",y="New Deaths")
graph 
#Projected deaths due to Covid-19 in US from 2020/08/01 to 2020/10/31 is 82,653

#Based on our ARIMA model and our forecast, we can see a slight difference but close enough result when comparing it to CNN’s forecast of that specified time frame.
#The difference for the 21-day forecast can be explained by CNN rounding their numbers since it’s a news headline and make it easier to read for the reader. 
#There is a difference of only 19,000–18,589 = 411 people, which is very close. 
#When it comes to the forecast for the next 90 days, we can see a bigger difference 235,967- 231,000 = 4,967. 
#The difference of 4,967, is reasonable.