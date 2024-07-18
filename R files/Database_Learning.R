# Clear the workspace
rm(list = ls())

library(tidyverse)
library(janitor)
# Detach and reload the libraries to ensure correct function priority
# detach("package:plyr")
# detach("package:dplyr")
library(plyr) # Load plyr first
library(dplyr) # Load dplyr second to avoid conflicts
library(readr)
library(lubridate)
library(ggcorrplot)
library(RColorBrewer)
library(sqldf)
library(scales)
library(ggpubr)
library(ggcorrplot)

local_path <- "C:/Users/hu437/OneDrive/Desktop/CEE_Project/Flight_delay.csv"
flights_df <- read_csv(local_path)

# Renaming columns
flights_df <- flights_df %>%
  dplyr::rename(
    weekday = DayOfWeek,
    dep_time = DepTime,
    arr_time = ArrTime,
    scheduled_arr_time = CRSArrTime,
    uniq_carrier_code = UniqueCarrier,
    flight_num = FlightNum,
    tail_num = TailNum,
    actual_flight_time_min = ActualElapsedTime,
    estimate_flight_time_min = CRSElapsedTime,
    air_time_min = AirTime,
    arr_delay = ArrDelay,
    dep_delay = DepDelay,
    dep_airport_code = Origin,
    dest_airport_code = Dest,
    distance_miles = Distance,
    landing_to_gate_min = TaxiIn,
    gate_to_takeoff_min = TaxiOut,
    cancellation_cause_code = CancellationCode,
    carrier_delay = CarrierDelay,
    weather_delay = WeatherDelay,
    nas_delay = NASDelay,
    security_delay = SecurityDelay,
    late_aircraft_delay = LateAircraftDelay
  )

# Update names to lowercase
names(flights_df) <- tolower(names(flights_df))

# Identify columns with only zero values
vector <- c()
for (i in names(flights_df)) {
  if (is.double(flights_df[[i]][2]) == TRUE) {
    if (sum(flights_df[[i]], na.rm = TRUE) == 0 ) {
      vector <- append(vector, i)
    }
  }
}
cat("The vector contains columns:", vector, sep="\n-")
flights_df <- select(flights_df, -all_of(vector), -dep_delay)

# Calculate total delay
flights_df <- mutate(flights_df,
                     total_delay = carrier_delay + weather_delay + nas_delay + 
                       security_delay + late_aircraft_delay)

# Extract month from date
flights_df <- mutate(flights_df, month = month(dmy(date)))

# Classify delays
flights_df <- mutate(flights_df, 
                     delay_degree = case_when(
                       total_delay <= 15 ~ "No delay",
                       total_delay > 15 & total_delay < 45 ~ "Medium delay",
                       total_delay >= 45 ~ "Large delay"
                     ))


