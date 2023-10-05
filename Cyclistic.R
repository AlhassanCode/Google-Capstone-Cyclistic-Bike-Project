# Installing and loading necessary packages for cleaning and working on the bike dataset

# Set CRAN mirror
options(repos = c(CRAN = "https://www.stats.bris.ac.uk/R/"))


install.packages("tidyverse")
library(tidyverse)


# Loading the bike dataset for year 2019 ( Q1-Q4)

bike_data_2019_Q1 <- read_csv("Divvy_Trips_2019_Q1.csv")
bike_data_2019_Q2 <- read_csv("Divvy_Trips_2019_Q2.csv")
bike_data_2019_Q3 <- read_csv("Divvy_Trips_2019_Q3.csv")
bike_data_2019_Q4 <- read_csv("Divvy_Trips_2019_Q4.csv")

# Checking for consistency in all 4 datasets

glimpse(bike_data_2019_Q1)
glimpse(bike_data_2019_Q2)
glimpse(bike_data_2019_Q3)
glimpse(bike_data_2019_Q4)

# Closer look at the columns in Quarter 2 of 2019
colnames(bike_data_2019_Q2)

# Rename columns in data for Quarter 2 to match all other columns in the other quarters

colnames(bike_data_2019_Q2) <- c("trip_id","start_time","end_time","bikeid","tripduration","from_station_id",
                                 "from_station_name","to_station_id","to_station_name","usertype",
                                 "gender","birthyear")

# view columns in the new 2019 quarter2 table

glimpse(bike_data_2019_Q2)
colnames(bike_data_2019_Q2)

# check column types
str(bike_data_2019_Q1)
str(bike_data_2019_Q2)
str(bike_data_2019_Q3)
str(bike_data_2019_Q4)

# converting trip_id and bikeid types to character

bike_data_2019_Q1 <-  mutate(bike_data_2019_Q1, trip_id = as.character(trip_id)
                   ,bikeid = as.character(bikeid))

bike_data_2019_Q2 <-  mutate(bike_data_2019_Q1, trip_id = as.character(trip_id)
                             ,bikeid = as.character(bikeid))

bike_data_2019_Q3 <-  mutate(bike_data_2019_Q1, trip_id = as.character(trip_id)
                             ,bikeid = as.character(bikeid))

bike_data_2019_Q4 <-  mutate(bike_data_2019_Q1, trip_id = as.character(trip_id)
                             ,bikeid = as.character(bikeid))

# Combining all the four quarters into one data frame/table

all_bike_trips_2019 <- bind_rows(bike_data_2019_Q1,bike_data_2019_Q2,
                                 bike_data_2019_Q3,bike_data_2019_Q4)


# Inspect new table

head(all_bike_trips_2019)
glimpse(all_bike_trips_2019)
colnames(all_bike_trips_2019)
nrow(all_bike_trips_2019)
dim(all_bike_trips_2019)
str(all_bike_trips_2019)
summary(all_bike_trips_2019)

# DATA CLEANING

# for consistence with the terms used by Company, we need to change the user type variables

all_bike_trips_2019 <- all_bike_trips_2019 %>%
  mutate(usertype = case_when(
    usertype == "Subscriber" ~ "Member",
    usertype == "Customer" ~ "Casual",
    TRUE ~ usertype
  ))

# Since membership type is level or category based, we would want to change usertype to a factor variable

all_bike_trips_2019$usertype <- as.factor(all_bike_trips_2019$usertype)

# Inspect change

glimpse(all_bike_trips_2019)
unique(all_bike_trips_2019$usertype)

table(all_bike_trips_2019$usertype)

# Transform the data to enable better aggregation by creating columns with year, month and day

all_bike_trips_2019$date <- as.Date(all_bike_trips_2019$start_time)
all_bike_trips_2019$month <- format(as.Date(all_bike_trips_2019$date), "%m")
all_bike_trips_2019$day <- format(as.Date(all_bike_trips_2019$date), "%d")
all_bike_trips_2019$year <- format(as.Date(all_bike_trips_2019$date), "%Y")
all_bike_trips_2019$day_of_week <- format(as.Date(all_bike_trips_2019$date), "%A")

# find time difference in seconds between trips
all_bike_trips_2019$ride_length <- difftime(all_bike_trips_2019$end_time,
                                            all_bike_trips_2019$start_time)
# Inspect columns in new bike trip data
str(all_bike_trips_2019)
glimpse(all_bike_trips_2019)

# convert ride length to numeric for easy calculations

all_bike_trips_2019$ride_length <- as.numeric(all_bike_trips_2019$ride_length)

unique(all_bike_trips_2019$from_station_name)

# Removing bad data from our table

all_trips_2019 <- all_bike_trips_2019[!(all_bike_trips_2019$ride_length<0),]

dim(all_trips_2019)

# Beginning Analysis

summary(all_trips_2019$ride_length)

# Make comparison between Casual and Annual members

aggregate(all_trips_2019$ride_length ~ all_trips_2019$usertype, FUN = mean)
aggregate(all_trips_2019$ride_length ~ all_trips_2019$usertype, FUN = median)
aggregate(all_trips_2019$ride_length ~ all_trips_2019$usertype, FUN = max)
aggregate(all_trips_2019$ride_length ~ all_trips_2019$usertype, FUN = min)

# See average ride time per day for the Casual and Annual members

aggregate(all_trips_2019$ride_length ~ all_trips_2019$usertype +
            all_trips_2019$day_of_week, FUN = mean)

# Re order days of the week as results appear mixed up

all_trips_2019$day_of_week <- ordered(all_trips_2019$day_of_week,
                                    levels=c("Sunday", "Monday", "Tuesday",
                                             "Wednesday", "Thursday", "Friday",
                                             "Saturday"))

# Re run to see average time per day for Casual and Annual members


aggregate(all_trips_2019$ride_length ~ all_trips_2019$usertype +
            all_trips_2019$day_of_week, FUN = mean)


# Analyse ridership data by user type and weekday

  all_trips_2019_V2 <- all_trips_2019 %>%
  mutate(weekday = wday(start_time,label = TRUE)) %>%
  group_by(usertype, weekday) %>%
  summarise(number_of_rides =n(), average_duration = mean(ride_length)) %>%
  arrange(usertype,weekday)



# Visualizing avg trip duration by week as per usertype

plot1 <- all_trips_2019_V2 %>% ggplot(aes(x = weekday, y = average_duration, fill = usertype)) +
  scale_fill_manual(values = c("lightblue","purple"))+
  geom_col(position = "dodge") + ggtitle("Average trip duration based on usertypes per week")

# Visualize average number of rides based on usertypes

plot2 <- all_trips_2019_V2 %>% ggplot(aes(x = weekday, y = number_of_rides, fill = usertype)) +
  scale_fill_manual(values = c("lightblue","purple"))+
  geom_col(position = "dodge") + ggtitle("Average number of rides based on usertypes per week")

plot2_modified <- plot2 +
  scale_y_continuous(labels = scales::number_format(scale = 1))

# Export final `all_trips_2019_V2` file to excel

install.packages("writexl")
library(writexl)

write.csv(all_trips_2019, "all_trips_2019.csv", row.names = FALSE)
write_xlsx(all_trips_2019_V2, path = "all_trips_2019_V2.xlsx")

# Saving the plot as a high-quality image (e.g., PNG)
ggsave("plot1.png", plot = plot1, width = 6, height = 4, dpi = 300)
ggsave("plot2.png", plot = plot2, width = 6, height = 4, dpi = 300)
ggsave("plot2_modified.png", plot = plot2_modified, width = 6, height = 4, dpi = 300)
