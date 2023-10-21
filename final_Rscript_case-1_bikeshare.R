#First, loading the required packages and importing and cleaning data

library(tidyverse) #for data import, cleaning, and manipulation
library(lubridate) #for date functions
library(ggplot2) #for visualizations
getwd()
setwd("C:/Users/Haneen/OneDrive - BUC/Documents/Data Analytics/Course 8 (Capstone)/Case_1_Bike-share_company/Case-1_data/2021-2023_cvs") #setting working directory for clear and simple data import
m1_2022 <- read_csv("202201-divvy-tripdata.csv") #importing datasets
m2_2022 <- read_csv("202202-divvy-tripdata.csv")
m3_2022 <- read_csv("202203-divvy-tripdata.csv")
m4_2022 <- read_csv("202204-divvy-tripdata.csv")
m5_2022 <- read_csv("202205-divvy-tripdata.csv")
m6_2022 <- read_csv("202206-divvy-tripdata.csv")
m7_2022 <- read_csv("202207-divvy-tripdata.csv")
m8_2022 <- read_csv("202208-divvy-tripdata.csv")
m9_2022 <- read_csv("202209-divvy-tripdata.csv")
m10_2022 <- read_csv("202210-divvy-tripdata.csv")
m11_2022 <- read_csv("202211-divvy-tripdata.csv")
m12_2022 <- read_csv("202212-divvy-tripdata.csv")
m1_2023 <- read_csv("202301-divvy-tripdata.csv")
m2_2023 <- read_csv("202302-divvy-tripdata.csv")
m3_2023 <- read_csv("202303-divvy-tripdata.csv")
m4_2023 <- read_csv("202304-divvy-tripdata.csv")
m5_2023 <- read_csv("202305-divvy-tripdata.csv")
m6_2023 <- read_csv("202306-divvy-tripdata.csv")
colnames(m1_2022) #comparing column names in each file to ensure they are identical for joining into a single dataframe
colnames(m2_2022)
colnames(m3_2022)
colnames(m4_2022)
colnames(m5_2022)
colnames(m6_2022)
colnames(m7_2022)
colnames(m8_2022)
colnames(m9_2022)
colnames(m10_2022)
colnames(m11_2022)
colnames(m12_2022)
colnames(m1_2023)
colnames(m2_2023)
colnames(m3_2023)
colnames(m4_2023)
colnames(m5_2023)
colnames(m6_2023)
str(m1_2022) #inspecting the dataframes for incongruences
str(m2_2022)
str(m3_2022)
str(m4_2022)
str(m5_2022)
str(m6_2022)
str(m7_2022)
str(m8_2022)
str(m9_2022)
str(m10_2022)
str(m11_2022)
str(m12_2022)
str(m1_2023)
str(m2_2023)
str(m3_2023)
str(m4_2023)
str(m5_2023)
str(m6_2023)

#Combining monthly data into a single dataframe
all_trips <- bind_rows(m1_2022, m2_2022, m3_2022, m4_2022, m5_2022, m6_2022, m7_2022
                       , m8_2022, m9_2022, m10_2022, m11_2022, m12_2022, m1_2023
                       , m2_2023, m3_2023, m4_2023, m5_2023, m6_2023)
glimpse(all_trips) #inspecting new dataframe

#Dropping data that is irrelevant in this analysis like latitudes and longitudes
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng))

#Inspecting different aspects of the new dataframe
colnames(all_trips)
nrow(all_trips)
dim(all_trips)
str(all_trips)
head(all_trips)
summary(all_trips)
table(all_trips$member_casual) #in previous years, Divvy used different labels for members and casual riders, so
#running this code to check and ensure that there are only two distinct values in the member_casual column
table(all_trips$rideable_type) #checking the rideable_type column for distinct values for accuracy

#At this point, the data could only be aggregated at the ride level, so to provide more opportunities for aggregation,
#columns of day, month, and year need to be added
all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

#Adding a calculated field for the length of a ride
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at, units = "mins")
str(all_trips) #inspecting the structure of the columns for accuracy
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length)) #converting ride_length to numeric to run calculations on the data
is.numeric(all_trips$ride_length) #checking for accuracy that ride_length is now in numeric form

#Some of the trips in the data have a negative ride_length when bikes were undocked by Divvy to run quality checks, which needed to be removed
#Creating a new version of the dataframe since data is being removed
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

#Moving on to data manipulation and analysis

#Running descriptive analysis on ride_length
summary(all_trips_v2$ride_length)
#Aggregating data to compare members and casual riders
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
#Reordering days of week for clarity
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean) #rerunning average ride length by day of week and usertype
#Creating a tibble to analyze ridership data by usertype and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%  
  summarise(number_of_rides = n()							 
            ,average_duration = mean(ride_length)) %>% 	
  arrange(member_casual, weekday)

#Finally, visualizing and exporting the data

#Creating a visualization for the number of rides by rider type
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  scale_fill_manual(values=c("#9933FF",
                             "darkblue")) +
  geom_col(position = "dodge") +
  labs(title = "Average Number of Rides by Day of Week", subtitle = "Members vs Casual Riders",
       caption = "January 2022 to June 2023", fill = "Rider Type") +
  ylab("Number of Rides") +
  xlab("Day of Week") +
  annotate("text", x="Wed", y=750000 , label = "Members: more rides on weekdays", color = "darkblue", size = 2.5, fontface = "bold") +
  annotate("text", x="Wed", y=875000 , label = "Casual: more rides on weekends", color = "#9933FF", size = 2.5, fontface = "bold")

#Creating a visualization for the average ride length by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  scale_fill_manual(values=c("#9933FF",
                             "darkblue")) +
  geom_col(position = "dodge") +
  labs(title = "Average Ride Length by Day of Week", subtitle = "Members vs Casual Riders",
       caption = "January 2022 to June 2023", fill = "Rider Type") +
  ylab("Average Ride Length") +
  xlab("Day of Week") +
  annotate("text", x="Wed", y=35 , label = "Casual: longer rides on weekends", color = "#9933FF", size = 2.5, fontface = "bold")

#Exporting a summary file for further analysis and visualizations in Excel and Tableau
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week + all_trips_v2$month, FUN = mean)
write.csv(counts, file = '...trips_analysis_summary.csv')
