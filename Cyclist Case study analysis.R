library(tidyverse)
library(ggplot2)
library(lubridate)
library(dplyr)
library(readr)
library(janitor)
library(data.table)
library(tidyr)

###load data 

df1 <- read.csv("C:/Users/LENOVO/Desktop/Projects/Google/2023/202304-divvy-tripdata/202304-divvy-tripdata.csv")
df2 <- read.csv("C:/Users/LENOVO/Desktop/Projects/Google/2023/202303-divvy-tripdata/202303-divvy-tripdata.csv")
df3 <- read.csv("C:/Users/LENOVO/Desktop/Projects/Google/2023/202302-divvy-tripdata/202302-divvy-tripdata.csv")
df4 <- read.csv("C:/Users/LENOVO/Desktop/Projects/Google/2023/202301-divvy-tripdata/202301-divvy-tripdata.csv")
df5 <- read.csv("C:/Users/LENOVO/Desktop/Projects/Google/2022/202212-divvy-tripdata/202212-divvy-tripdata.csv")
df6 <- read.csv("C:/Users/LENOVO/Desktop/Projects/Google/2022/202211-divvy-tripdata/202211-divvy-tripdata.csv")
df7 <- read.csv("C:/Users/LENOVO/Desktop/Projects/Google/2022/202210-divvy-tripdata/202210-divvy-tripdata.csv")
df8 <- read.csv("C:/Users/LENOVO/Desktop/Projects/Google/2022/202209-divvy-tripdata/202209-divvy-publictripdata.csv")
df9 <- read.csv("C:/Users/LENOVO/Desktop/Projects/Google/2022/202208-divvy-tripdata/202208-divvy-tripdata.csv")
df10 <- read.csv("C:/Users/LENOVO/Desktop/Projects/Google/2022/202207-divvy-tripdata/202207-divvy-tripdata.csv")
df11 <- read.csv("C:/Users/LENOVO/Desktop/Projects/Google/2022/202206-divvy-tripdata/202206-divvy-tripdata.csv")
df12 <- read.csv("C:/Users/LENOVO/Desktop/Projects/Google/2022/202205-divvy-tripdata/202205-divvy-tripdata.csv")


##merge data into one file 

df <- bind_rows(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12)

str(df)

##data cleaning

df[['started_at']] <- ymd_hms(df[['started_at']])
df[['ended_at']] <- ymd_hms(df[['ended_at']])

# droping not required columns for analysis

df <- df %>%
  select(-c(start_lat:end_lng))

glimpse(df)


#### Rename columns for better readability

df <- df %>%
  rename(df = rideable_type, 
         start_time = started_at,
         end_time = ended_at,
         customer_type = member_casual)

glimpse(df)


# column for day of the week the trip started
df$day_of_the_week <- format(as.Date(df$start_time),'%a')



# column for month when the trip started
df$month <- format(as.Date(df$start_time),'%b_%y')



# The time is then converted back to POSIXct with today’s date – the date is of no interest to us,only the hours-minutes-seconds are.
df$time <- format(df$start_time, format = "%H:%M")
df$time <- as.POSIXct(df$time, format = "%H:%M")


# column for trip duration in min
df$trip_duration <- (as.double(difftime(df$end_time, df$start_time)))/60



# check the dataframe
glimpse(df)


##  Let's check to see if the trip_duration column has any negative values, as this may cause problem while creating visualizations. Also, we do not want to include the trips that were part of quality tests by the company. These trips are usually identified by string 'test' in the start_station_name column.



# checking for trip lengths less than 0
nrow(subset(df,trip_duration < 0))



#checking for testrides that were made by company for quality checks
nrow(subset(df, start_station_name %like% "TEST"))
nrow(subset(df, start_station_name %like% "test"))
nrow(subset(df, start_station_name %like% "Test"))



#As there are 103 rows with trip_duration less than 0 mins and 0 trips that were test rides, we will remove these observations from our dataframe as they contribute to only about 0.03% of the total rows. We will create a new dataframe deviod of these obseravtions without making any changes to the existing dataframe.  



# remove negative trip durations 
all_trips_v2 <- df[!(df$trip_duration < 0),]


#check dataframe
glimpse(all_trips_v2)



#It is important to make sure that customer_type column has only two distinct values. Let's confirm the same.


## checking count of distinct values

table(all_trips_v2$customer_type)

#aggregating total trip duration by customer type

setNames(aggregate(trip_duration ~ customer_type, all_trips_v2, sum), c("customer_type", "total_trip_duration(mins)"))


  
  ### 4&5. Analyze and Share Data
  
#The dataframe is now ready for descriptive analysis that will help us uncover some insights on how the casual riders and members use Cyclistic rideshare differently.

#First, let's try to get some simple statistics on trip_duration for all customers, and do the same by customer_type.


# statictical summary of trip_duration for all trips
summary(all_trips_v2$trip_duration)


#statistical summary of trip_duration by customer_type
all_trips_v2 %>%
  group_by(customer_type) %>%
  summarise(min_trip_duration = min(trip_duration),max_trip_duration = max(trip_duration),
            median_trip_duration = median(trip_duration), mean_trip_duration = mean(trip_duration))

```

The mean trip duration of member riders is lower than the mean trip duration of all trips, while it is exactly the opposite for casual riders, whose mean trip duration is higher than the the mean trip duration of all trips. This tells us that casual riders usually take the bikes out for a longer duration compared to members.

<br/>

#### Total number of trips by customer type and day of the week
```{r}

# fix the order for the day_of_the_week and month variable so that they show up in the same sequence in output tables and visualizations
all_trips_v2$day_of_the_week <- ordered(all_trips_v2$day_of_the_week, levels=c("Pzt", "Sal", "Çar", "Per", "Cum", "Cmt", "Paz"))

all_trips_v2$month <- ordered(all_trips_v2$month, levels=c("Kas_20","Ara_20","Oca_21","Şub_21","Mar_21","Nis_21", "May_21","Haz_21","Tem_21", "Ağu_21","Eyl_21","Eki_21"))

#Total number of trips by customer type and day of the week

all_trips_v2 %>% 
  group_by(customer_type, day_of_the_week) %>%  
  summarise(number_of_rides = n(),average_duration_mins = mean(trip_duration)) %>% 
  arrange(customer_type, desc(number_of_rides))


```


#### Visualization
```{r}

#Total trips by customer type Vs. Day_of_Week


all_trips_v2 %>%  
  group_by(customer_type, day_of_the_week) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(customer_type, day_of_the_week)  %>% 
  ggplot(aes(x = day_of_the_week, y = number_of_rides, fill = customer_type)) +
  labs(title ="Total trips by customer type Vs. Day of the week") +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))




#From the table and graph above, casual customers are most busy on Sundays followed by Saturdays, while members are most busy on later half of the week extending into the weekend. Interesting pattern to note though is the consistent trip numbers among members with less spread over entire week as compared to casual riders who don't seem to use the bikeshare services much during weekdays.


  
# Average number of trips by customer type and month


all_trips_v2 %>% 
  group_by(customer_type, month) %>%  
  summarise(number_of_rides = n(),`average_duration_(mins)` = mean(trip_duration)) %>% 
  arrange(customer_type,desc(number_of_rides))


#### Visualization

#Total trips by customer type Vs. Month
all_trips_v2 %>%  
  group_by(customer_type, month) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(customer_type, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = customer_type)) +
  labs(title ="Total trips by customer type Vs. Month") +
  theme(axis.text.x = element_text(angle = 30)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))


#The data shows that the months of July, August, September and October are the most busy time of the year among both members and casual riders. This could be attributed to an external factor (eg. cold weather, major quality issue) that might have hindered with customer needs. 2021 is a tough year when Covid comes. People care more about their health. The charts shows that the no.of rides in 2021 is higher than 2020 in general. However, the number of trips made by members is always higher than the casual riders across all months of the year.


  
  #### Visualizaton of average trip duration by customer type on each day of the week
  
  
all_trips_v2 %>%  
  group_by(customer_type, day_of_the_week) %>% 
  summarise(average_trip_duration = mean(trip_duration)) %>%
  ggplot(aes(x = day_of_the_week, y = average_trip_duration, fill = customer_type)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Average trip duration by customer type Vs. Day of the week")

```

The average trip duration of a casual rider is more than twice that of a member. Note that this necessarily does not mean that casual riders travel farther distance. It is also interesting to note that weekends not only contribute to more number of trips but also longer trips on average when compared to weekdays.


<br/>
  
  #### Visualizaton of average trip duration by customer type Vs. month
  ```{r}

all_trips_v2 %>%  
  group_by(customer_type, month) %>% 
  summarise(average_trip_duration = mean(trip_duration)) %>%
  ggplot(aes(x = month, y = average_trip_duration, fill = customer_type)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Average trip duration by customer type Vs. Month") +
  theme(axis.text.x = element_text(angle = 30))
```

Average trip duration of member riders is anywhere between 10-30 minutes throughout the year, exception being February when it goes slightly over 20 minutes. However, there seems to be a distinct pattern when it comes to casual riders, whose average trip duration swings wildly from as low as ~25 minutes to more than an hour depending on time of the year. It is worth noting unusually long trip durations by casual riders in the month of February.

<br/>
  
  #### Visualizaton of bike demand over 24 hr period (a day)
  ```{r}
all_trips_v2 %>%  
  group_by(customer_type, time) %>% 
  summarise(number_of_trips = n()) %>%
  ggplot(aes(x = time, y = number_of_trips, color = customer_type, group = customer_type)) +
  geom_line() +
  scale_x_datetime(date_breaks = "1 hour", minor_breaks = NULL,
                   date_labels = "%H:%M", expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title ="Demand over 24 hours of a day", x = "Time of the day")


```

For the members, there seems to be two distict peak demand hours: 7-9 AM and 5-7 PM, the latter one coinciding with the peak demand hours of casual riders as well. One could probably hypothesize that office-goers make up majority of the members profile due to demand in both morning and evening hours, but we need more data to substabtiate this assumption.

<br/>
  
  #### Visualizaton of ride type Vs. number of trips by customer type
  ```{r}


all_trips_v2 %>%
  group_by(ride_type, customer_type) %>%
  summarise(number_of_trips = n()) %>%  
  ggplot(aes(x= ride_type, y=number_of_trips, fill= customer_type))+
  geom_bar(stat='identity') +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(title ="Ride type Vs. Number of trips")

```

Classic bikes are predominantly used by members. Docked bikes are almost never used compared to others. Electric bikes are equally used by both members as well as casual riders. If docked bikes cost the highest among all 3 types, it would be a financially sound move to increase their fleet while reducing docked bikes, as they are already preferred by members who make up for the majority of the trips.

<br/>
  
  ### 6. Act
  
  #### Important Findings
  - Casual riders made 41% of total trips contributing to 66% of total trip duration between May'22 - April'23. Member riders make up 59% of total trips contributing to 34% of total trip duration between May'22 - April'23

- Usage (based on trip duration) of bikes by casual riders is almost twice that of member riders.

- Casual customers use bikeshare services more during weekends, while members use them consistently over the entire week.

- Average trip duration of casual riders is more than twice that of member rider over any given day of the week cumulatively.

- Casual riders ride longer during first half of the year compared to the second half, while members clock relatively similar average trip duration month over month.

- Casual riders prefer electric bikes the most while classic bikes are popular among members.

<br/>
  
  ### Recommendations
  
  - Provide attractive promotions for casual riders on weekdays so that casual members use the bikeshare services or uniformly across the entire week.

- Offer discounted membership fee for renewals after the first year. It might nudge casual riders to take up membership.

- Offer discounted pricing during non-busy hours so that casual riders might choose to use bikes more often and level out demand over the day.

### Additonal data that could expand scope of analysis

- Age and gender profile - Again, this data could be used to study the category of riders who can be targeted for attracting new members.

- Address/ neighborhood details of members to investigate if there are any location specific parameters that encourage membership.

- Pricing details for members and casual riders - Based on this data, we might be to optimize cost structure for casual riders or provide discounts without affecting the profit margin.





