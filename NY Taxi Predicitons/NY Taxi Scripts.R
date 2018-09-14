
library(twitteR)
library(wordcloud)
library(tm)
library(SnowballC)
library(readr)
library(dplyr)
library(ggplot2)  
library(twitteR)
library(wordcloud)
library(tm)
library(SnowballC)
library(readr)
library(rtweet)
library(dplyr)
library(ggplot2)  
library(reshape2)




NY.20   <-read.csv(file="Desktop/Projects/Kaggle/New York City Taxi Prediction/train.csv",nrows=3500000)


#write.csv(NY.20, file =  "NY.50.csv")

summary(fit1)

str(NY.20)
########
NY.20 <- NY.20[complete.cases(NY.20), ]




##Extract Date
NY.20$Date <- sub(" .*", "\\", NY.20$pickup_datetime)

### Extract Time
NY.20$Time <- sub(".*? (.+)", "\\1", NY.20$pickup_datetime)
#remove utc
NY.20$Time <- substr(NY.20$Time,1,nchar(NY.20$Time)-4)

#### Identify weekday
NY.20$WeekDay <-  weekdays(as.Date(NY.20$Date))
# Identify Month
NY.20$Month <-  months(as.Date(NY.20$Date))
#Identify Year
NY.20$Year <- strftime(NY.20$Date, "%Y")
#Identify Month Date
NY.20$MDay <- strftime(NY.20$Date, "%d")
#identify Hour
NY.20$Hour <-  sub(":.*", "\\", NY.20$Time)
#Identify minute
NY.20$Minute <-  sub(".*:\\s*|:.*", "\\", NY.20$Time)


#### Are there any NA's?
library(geosphere)
##
which(is.na(NY.20$dropoff_longitude))

#### There should be no longitude value smaller than -360

#NY.49 <- filter(NY.20, NY.20$pickup_longitude < -359 | NY.20$dropoff_longitude < -359)

#NY.49 <- filter(NY.20, NY.20$pickup_longitude < - 500)
NY.49 <- subset(NY.20, NY.20$pickup_longitude < -500)


#NY.47 <- filter(NY.20, NY.20$pickup_latitude < -90 | NY.20$dropoff_latitude < -90)

#NY.48 <- filter(NY.20, NY.20$pickup_longitude > -359 & NY.20$dropoff_longitude > -359)
#NY.48 <- filter(NY.20, NY.20$pickup_longitude > -359 & NY.20$dropoff_longitude > -359)


NY.48 <- subset(NY.20, NY.20$pickup_longitude > -360 & NY.20$dropoff_longitude > -360)


#remove latitude < -90


#NY.46 <- filter(NY.48,NY.48$pickup_latitude > -90 & NY.48$dropoff_latitude > -90)
NY.46 <- subset(NY.48,NY.48$pickup_latitude > -90 & NY.48$dropoff_latitude > -90)

##3 remove latitude >90
#NY.45 <- filter(NY.46,  NY.46$pickup_latitude < 90 & NY.46$dropoff_latitude < 90)
NY.45 <- subset(NY.46,  NY.46$pickup_latitude < 90 & NY.46$dropoff_latitude < 90)

# remove longitude > 360

NY.45 <- subset (NY.45,NY.45$pickup_longitude < 360 & NY.45$dropoff_longitude < 360)


NY.45$distance <- distHaversine(NY.45[,c('pickup_longitude','pickup_latitude')], NY.45[,c('dropoff_longitude','dropoff_latitude')])


## turn required columns into factors

cols <- c("WeekDay", "Month", "Year", "MDay","Hour")

NY.45[cols] <- lapply(NY.45[cols], factor)


### Run M MLR
fit1 <- lm(formula= fare_amount ~ pickup_longitude + pickup_latitude + dropoff_longitude 
           + passenger_count + dropoff_latitude + WeekDay + Month + Year + MDay + Hour + distance, data = NY.45)





##### predict new data

NY.test.2   <-read.csv(file="Desktop/Projects/Kaggle/New York City Taxi Prediction/test.csv")
NY.test   <-read.csv(file="Desktop/Projects/Kaggle/New York City Taxi Prediction/test.csv")

########################################


NY.test <- NY.test[complete.cases(NY.test), ]




##Extract Date
NY.test$Date <- sub(" .*", "\\", NY.test$pickup_datetime)

### Extract Time
NY.test$Time <- sub(".*? (.+)", "\\1", NY.test$pickup_datetime)
#remove utc
NY.test$Time <- substr(NY.test$Time,1,nchar(NY.test$Time)-4)

#### Identify weekday
NY.test$WeekDay <-  weekdays(as.Date(NY.test$Date))
# Identify Month
NY.test$Month <-  months(as.Date(NY.test$Date))
#Identify Year
NY.test$Year <- strftime(NY.test$Date, "%Y")
#Identify Month Date
NY.test$MDay <- strftime(NY.test$Date, "%d")
#identify Hour
NY.test$Hour <-  sub(":.*", "\\", NY.test$Time)




#### There should be no longitude value smaller than -360



NY.test <- subset(NY.test, NY.test$pickup_longitude > -359 & NY.test$dropoff_longitude > -359)


#remove latitude < -90


NY.test <- subset(NY.test,NY.test$pickup_latitude > -90 & NY.test$dropoff_latitude > -90)

##3 remove latitude >90
NY.test <- subset(NY.test,  NY.test$pickup_latitude < 90 & NY.test$dropoff_latitude < 90)



## calculate distance

NY.test$distance <- distHaversine(NY.test[,c('pickup_longitude','pickup_latitude')], NY.test[,c('dropoff_longitude','dropoff_latitude')])


## turn required columns into factors

cols <- c("WeekDay", "Month", "Year", "MDay","Hour")

NY.test[cols] <- lapply(NY.test[cols], factor)

#

cols2 <- c("pickup_longitude","pickup_latitude","dropoff_longitude","dropoff_latitude","passenger_count","WeekDay", "Month", "Year", "MDay","Hour","distance")

NY.test <- NY.test[cols2]

yhat.lm = predict(fit1,newdata=NY.test)

NY.test.2$fare_amount<- yhat.lm


Kaggle.NY.submit <- NY.test.2 %>% select("key", "fare_amount")

write.csv(Kaggle.NY.submit, file = "Desktop/Projects/Kaggle/New York City Taxi Prediction/kaggle.NY.5.sub.csv", row.names=FALSE)


## random forest

library(randomForest)

rf.house=randomForest(fare_amount~pickup_longitude + pickup_latitude + dropoff_longitude 
                      + passenger_count + dropoff_latitude + WeekDay + Month + Year + MDay + Hour + distance,data=NY.45,mtry=4,importance=TRUE)




