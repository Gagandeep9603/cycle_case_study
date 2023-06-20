#importing the important libraries 
library(tidyverse)
library(lubridate)
library(ggplot2)
library(readr)
library(dplyr)

# reading the merged data.
df <- read_csv("cycle_data.csv")

# changing ride_id and rideable_type to character
df <- mutate(df, ride_id = as.character(ride_id), rideable_type = as.character((rideable_type)))


# reviewing the counts of members and casual users 
table(df$member_casual)

# adding date,day,month,year and day_of_week 
df$date <- as.Date(df$started_at)
df$month <- format(as.Date(df$date),"%m")
df$day <- format(as.Date(df$date),"d")
df$year <- format(as.Date(df$date),"y")
df$day_of_week <- format(as.Date(df$date),"%A")

# calculating the ride_length
df$ride_length <- difftime(df$ended_at,df$started_at)

# reviewing the data 
str(df)

#converting ride_length to numeric 
df$ride_length <- as.numeric(as.character(df$ride_length))
is.numeric(df$ride_length)


# removing the data where the start_station_name is HQ QR and the ride_lenght is negative 
# removing the bad data and creating df_v2

df_v2 <- df[!(df$start_station_name == "HQ QR" | df$ride_length<0),]

# finding the mean,median,mode and max,min for ride_length
summary(df_v2$ride_length)

aggregate(df_v2$ride_length ~ df_v2$member_casual + df_v2$day_of_week, FUN = mean)

# number_of_rides graph
df_v2 %>%
  mutate(weekday = wday(started_at,label=TRUE)) %>%
  group_by(member_casual,weekday) %>%
  summarise(number_of_rides = n(), avg_duration = mean(ride_length))%>%
  arrange(member_casual,weekday)%>%
  ggplot(aes(x=weekday,y=number_of_rides,fill=member_casual))+
  geom_col(position = "dodge")

# avd_duration graph
df_v2 %>%
  mutate(weekday = wday(started_at,labe=TRUE))%>%
  group_by(member_casual,weekday)%>%
  summarise(number_of_rides = n(),avg_duration = mean(ride_length))%>%
  arrange(member_casual,weekday)%>%
  ggplot(aes(x=weekday,y=avg_duration,fill=member_casual))+
  geom_col(position = "dodge")
  
# making a new data frame for rides having duration more than 24 hrs 
df_v3 <-df[!(df$ride_length<1440),]
# no._of_rides_greater_than_24hrs graph
df_v3 %>%
  mutate(weekday = wday(started_at,label=TRUE)) %>%
  group_by(member_casual,weekday) %>%
  summarise(no._of_rides_more_than_24hrs= n(), avg_duration = mean(ride_length))%>%
  arrange(member_casual,weekday)%>%
  ggplot(aes(x=weekday,y=no._of_rides_more_than_24hrs,fill=member_casual))+
  geom_col(position = "dodge")

# making a data frame having rides less than 24hrs 
df_v4 <-df[!(df$ride_length>1440),]
# no._of_rides_less_than_24hrs graph 
df_v4 %>%
  mutate(weekday = wday(started_at,label=TRUE)) %>%
  group_by(member_casual,weekday) %>%
  summarise(no._of_rides_less_than_24hrs= n(), avg_duration = mean(ride_length))%>%
  arrange(member_casual,weekday)%>%
  ggplot(aes(x=weekday,y=no._of_rides_less_than_24hrs,fill=member_casual))+
  geom_col(position = "dodge")

# making a csv file , for further analysis if needed 
counts <- aggregate(df_v2$ride_length ~ df_v2$member_casual + df_v2$day_of_week,FUN = mean)
write.csv(counts, file='cycle_data_cleaned_v2.csv')