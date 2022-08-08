#=====================
# STEP 1: INSTALL AND LOAD REQUIRED PACKAGES
#=====================
install.packages("tidyverse")
install.packages("lubridate")
install.packages("geosphere")
library(tidyverse) 
library(lubridate)
library(geosphere)
library(readr)
library(tidyr)
library(dplyr)

#=====================
# STEP 2: COLLECT DATA
#=====================
# Upload Divvy datasets (csv files) here
july_2021 <- read_csv("202107-divvy-tripdata.csv")
aug_2021 <- read_csv("202108-divvy-tripdata.csv")
sep_2021 <- read_csv("202109-divvy-tripdata.csv")
oct_2021 <- read_csv("202110-divvy-tripdata.csv")
nov_2021 <- read_csv("202111-divvy-tripdata.csv")
dec_2021 <- read_csv("202112-divvy-tripdata.csv")
jan_2022 <- read_csv("202201-divvy-tripdata.csv")
feb_2022 <- read_csv("202202-divvy-tripdata.csv")
mar_2022 <- read_csv("202203-divvy-tripdata.csv")
apr_2022 <- read_csv("202204-divvy-tripdata.csv")
may_2022 <- read_csv("202205-divvy-tripdata.csv")
jun_2022 <- read_csv("202206-divvy-tripdata.csv")

#====================================================
# STEP 3: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================
# Compare column names each of the files
# While the names don't have to be in the same order, they DO need to match perfectly before we can use a command to join them into one file
colnames(july_2021)
colnames(aug_2021)
colnames(sep_2021)
colnames(oct_2021)
colnames(nov_2021)
colnames(dec_2021)
colnames(jan_2022)
colnames(feb_2022)
colnames(mar_2022)
colnames(apr_2022)
colnames(may_2022)
colnames(jun_2022)
#column names match perfectly, therefore no need to rename any columns. 

# Inspect the dataframes and look for incongruencies
str(may_2021)
str(june_2021)
str(july_2021)
str(aug_2021)
str(sep_2021)
str(oct_2021)
str(nov_2021)
str(dec_2021)
str(jan_2022)
str(feb_2022)
str(mar_2022)
str(apr_2022)

# Stack individual month's data frames into one big data frame
all_trips <- bind_rows(july_2021, aug_2021, sep_2021, oct_2021, nov_2021, dec_2021, jan_2022, feb_2022, mar_2022, apr_2022, may_2022, jun_2022)

#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Inspect the new table that has been created
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics

# Add columns that list the date, hour, month, day, and year of each ride.
# We will also add two calculated columns that shows trip duration and distance travelled for each trip.
# This will allow us to aggregate ride data for each month, day, or year etc. Before completing these operations we could only aggregate at the ride level

all_trips_v2 <- all_trips %>% 
  mutate(ride_length = as.numeric(difftime(ended_at, started_at, unit="mins"))) %>% 
  mutate(ride_distance = distHaversine(cbind(start_lng, start_lat), cbind(end_lng, end_lat))) %>% # returns distance in meters
  mutate(year = year(started_at)) %>% 
  mutate(month = month(started_at, label = TRUE)) %>% 
  mutate(day = wday(started_at)) %>% 
  mutate(hour = hour(started_at))

# Inspect the structure of the columns
str(all_trips)
summary(all_trips)
head(all_trips_v2)

# Remove "Bad" data
# The dataframe includes several entries where trip duration and distance was less than 0. We will remove those entries.
# We will also remove entries where start station or end station names are missing.
filtered_data <- all_trips_v2 %>% 
  filter(!is.na(start_station_name)) %>% 
  filter(!is.na(end_station_name)) %>% 
  filter(ride_length > 0)
  filter(ride_distance > 0)

# We will also remove columns for start station ID and end station ID as they are redundant in this analysis.
filtered_data <- filtered_data %>%  
  select(-c(start_station_id, end_station_id))

# Export the filtered dataframe for further analysis and visualization
final_data_v2 <- filtered_data %>% 
  group_by(member_casual, rideable_type, year, month, day, hour, start_lat, start_lng) %>% 
  summarise(number_of_rides =n(), avg_ride_length =  mean(ride_length), avg_ride_distance = mean(ride_distance))
write_csv(final_data, file = 'C:\\Users\\swara\\OneDrive\\Desktop\\DA Course\\Case Study - Track 1\\final_data.csv')
