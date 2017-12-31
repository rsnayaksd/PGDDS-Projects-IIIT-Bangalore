#load the required packages 
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)

#1. Data Cleaning and Preparation

# To load the uber request data file into R.  
uber_dataset <- read.csv("Uber Request Data.csv",stringsAsFactors = FALSE,na.strings = TRUE)

# To make the time & date separator consistent
uber_dataset$request_dt <- str_replace_all(uber_dataset$Request.timestamp, "[/]", "-")
uber_dataset$drop_dt <- str_replace_all(uber_dataset$Drop.timestamp, "[/]", "-")

# Checking the type of request_dt
class(uber_dataset$request_dt)

# Converting time columns to datetime
uber_dataset$request_dt <- as.POSIXct(uber_dataset$request_dt, format = "%d-%m-%Y %H:%M")

#uber_dataset$request_dt <- strptime(uber_dataset$request_dt, format = "%d-%m-%Y %H:%M")

uber_dataset$drop_dt <- as.POSIXct(uber_dataset$drop_dt, format = "%d-%m-%Y %H:%M")

#uber_dataset$drop_dt <- strptime(uber_dataset$drop_dt, format = "%d-%m-%Y %H:%M")

# Checking the type of request_dt
class(uber_dataset$request_dt)


# Creating a separate hour and day column
uber_dataset$request_hour <- format(uber_dataset$request_dt, "%H")
uber_dataset$day <- format(uber_dataset$request_dt, "%d")

# Converting request_hour into numeric for performing analysis and ploting graphs
uber_dataset$request_hour <- as.numeric(uber_dataset$request_hour)
# Checking the type of request_hour
class(uber_dataset$request_hour)

# Making separate Time slots column in uber_dataset using conditional statements
# ifelse(test_expression,x,y)
uber_dataset$time_slot = ifelse(uber_dataset$request_hour < 5, "1 Pre_Morning", 
                                ifelse(uber_dataset$request_hour < 10,"2 Morning_Rush",
                                       ifelse(uber_dataset$request_hour < 17,"3 Day_Time",
                                              ifelse(uber_dataset$request_hour < 22,"4 Evening_Rush","5 Late_Night"))))

# Checking all unique values of time_slot
unique(uber_dataset$time_slot)

# Results Expected
# Visually identify the most pressing problems for Uber. 
# Hint: Create plots to visualise the frequency of requests that get cancelled or show 'no cars available'; identify the most problematic types of requests (city to airport / airport to city etc.) and the time slots (early mornings, late evenings etc.) using plots
# Plot the number of cabs requested in a particular hour for all 05 days

# Analysis 1.1 : Hourly status for Uber Cabs

hourwise_status_count1 <- ggplot(uber_dataset %>% filter(Status != "Trip Completed"),aes(x=factor(request_hour),fill=Status))
plot01 <- hourwise_status_count1 + geom_bar(stat='count', position = "dodge") +
  ggtitle("Hourly Status for Uber Cabs")+
  labs(x="Time in Hours", y="Status wise number of Cabs")+
  labs(fill="Pickup Point")

#view the plot
plot01


# Analysis 1.2 : Hourly status for Uber Cabs [City to Airport]
hourwise_status_count2 <- ggplot(uber_dataset %>% filter(Status != "Trip Completed", Pickup.point == "City"),aes(x=factor(request_hour),fill=Status))
plot02 <- hourwise_status_count2 + geom_bar(stat='count', position = "dodge") +
  ggtitle("Hourly Status for Uber Cabs [City to Airport]")+
  labs(x="Time in Hours", y="Status wise number of Cabs")+
  labs(fill="Pickup Point")

#view the plot
plot02

# Analysis 1.3 : Hourly status for Uber Cabs [Airport to City]
hourwise_status_count3 <- ggplot(uber_dataset %>% filter(Status != "Trip Completed", Pickup.point == "Airport"),aes(x=factor(request_hour),fill=Status))
plot03 <- hourwise_status_count3 + geom_bar(stat='count', position = "dodge") +
  ggtitle("Hourly Status for Uber Cabs [Airport to City]")+
  labs(x="Time in Hours", y="Status wise number of Cabs")+
  labs(fill="Pickup Point")

#view the plot
plot03

# Analysis 1.04
hourwise_status_count4 <- ggplot(uber_dataset %>% filter(Status != "Trip Completed"),aes(x=factor(time_slot),fill=Pickup.point))
plot04 <- hourwise_status_count4 + geom_bar(stat='count', position = "dodge") +
  ggtitle("Hourly Status for Uber Cabs [Airport to City]")+
  labs(x="Time in Hours", y="Status wise number of Cabs")+
  labs(fill="Pickup Point")

#view the plot
plot04


# Problem 2 : Find out the gap between supply and demand and show the same using plots.
# 2.1 Find the time slots when the highest gap exists

# Creating a seperate column in uber dataset for checking trip completd or not
uber_dataset$isCompleted  <- ifelse(uber_dataset$Status=="Trip Completed","Trip Completed","Trip Not Completed")

hourwise_status_count5 <- ggplot(uber_dataset,aes(x=factor(time_slot),fill=isCompleted))
plot05 <- hourwise_status_count5 + geom_bar(stat='count', position = "dodge") +
  ggtitle("City  Demand supply Gap")+
  labs(x="Time in Hours", y="Status wise number of Cabs")+
  labs(fill="Trip Status")

#view the plot
plot05


# 2.2 Find the types of requests (city-airport or airport-city) for which the gap is the most severe in the identified time slots

# City Supply Demand Gap
hourwise_status_count6 <- ggplot(uber_dataset %>% filter(Pickup.point == "City"),aes(x=factor(time_slot),fill=isCompleted))
plot06 <- hourwise_status_count6 + geom_bar(stat='count', position = "dodge") +
  ggtitle("City Demand supply Gap")+
  labs(x="Time in Hours", y="Status wise number of Cabs")+
  labs(fill="Trip Status")

#view the plot
plot06

# Airport Supply Demand Gap
hourwise_status_count7 <- ggplot(uber_dataset %>% filter(Pickup.point == "Airport"),aes(x=factor(time_slot),fill=isCompleted))
plot07 <- hourwise_status_count + geom_bar(stat='count', position = "dodge") +
  ggtitle("Airport Demand supply Gap")+
  labs(x="Time in Hours", y="Status wise number of Cabs")+
  labs(fill="Trip Status")

#view the plot
plot07
