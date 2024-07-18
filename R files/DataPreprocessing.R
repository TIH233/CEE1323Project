# Clear the workspace
rm(list = ls())
   
library(tidyverse)
library(janitor)
detach("package:plyr") # detaching both libraries ...
detach("package:dplyr")
library(plyr) # ... and loading them again to make sure 
library(dplyr) # they are loaded in the right order
library(readr)
library(lubridate)
library(ggcorrplot)
library(RColorBrewer)
library(sqldf)
library(scales)
library(ggpubr) 
library(ggcorrplot)
# need to run the code with detach commented once (or the object cannot be found later), 
# then run without comment (or detach will have error)

# download data from
# < http://127.0.0.1:56595/%22https://www.kaggle.com/datasets/undersc0re/flight-delay-
# and-causes/metadata?select=Flight_delay.csv%22 >
# and assign the path as a string to the local_path variable
local_path <- "C:/Users/hu437/OneDrive/Desktop/CEE_Project/Flight_delay.csv"

flights_df <- read_csv(local_path)

# display first few rows of data frame
head(flights_df)

# change the name of columns
names(flights_df) <- tolower(names(flights_df %>% 
                                     dplyr::rename(weekday = DayOfWeek,
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
                                                   dep_airport = Org_Airport,
                                                   dest_airport_code = Dest,
                                                   dest_airport = Dest_Airport,
                                                   distance_miles = Distance, 
                                                   landing_to_gate_min = TaxiIn,
                                                   gate_to_takeoff_min =TaxiOut,
                                                   cancellation_cause_code = CancellationCode,
                                                   carrier_delay = CarrierDelay,
                                                   weather_delay = WeatherDelay,
                                                   nas_delay = NASDelay,
                                                   security_delay = SecurityDelay,
                                                   late_aircraft_delay = LateAircraftDelay)))

colnames(flights_df)

# find columns that give no information (cancelled and diverted all 0)
vector <- c()
for (i in names(flights_df)) {
  if (is_double(flights_df[[i]][2]) == TRUE) {
    if (sum(flights_df[i]) == 0 ) {
      vector <- append(vector, i)
    }
  }
}

cat("the vector contains columns:", vector, sep="\n-")

#method 1: selecting all except from elements of vector to delete columns
flights_df <- select(flights_df, -all_of(vector))

flights_df <- mutate(flights_df,
                     total_delay = (carrier_delay + weather_delay + nas_delay + 
                                      security_delay + late_aircraft_delay))

library(lubridate)

# create a column with month of each flight
flights_df <- flights_df %>% mutate(month = month(dmy(date)))
# create a column with date of the month
flights_df <- flights_df %>%
  mutate(day_of_month = day(dmy(date))) %>%
  select(-date)


colnames(flights_df)

# categorize flight delay rank

# flights_df <- flights_df %>% 
#   mutate(degree_delay =
#            ifelse(total_delay <= 15, "no delay", 
#                   ifelse(total_delay >= 45, "large delay", "medium delay")))

# with vectorized loop (rather than for and simply value variables 
# --> replicate the whole data set in each iteration of i)

# vec <- c()
# for (t in flights_df$total_delay) {
#   if (t <= 15) {
#     vec <- append(vec, "No delay")
#   } else if (t >= 45) {
#     vec <- append(vec, "Large delay")
#   } else {
#     vec <- append(vec, "Medium delay")
#   }
# }
# 
# flights_df["delay_degree"] <- vec

library(dplyr)

flights_df <- flights_df %>% 
  mutate(delay_degree = case_when(
    total_delay <= 15 ~ "No delay",
    total_delay >= 45 ~ "Large delay",
    TRUE ~ "Medium delay"  # 'TRUE' acts as the 'else' condition
  ))
# even better

# Create class column for each delay (classify whether is moderate delay)
flights_df <- flights_df %>%
  mutate(delay_class = ifelse(delay_degree == "Large delay", 1, 0))


# Remove the "cancellation_cause_code" column
flights_df <- select(flights_df, -cancellation_cause_code)

# View the first few rows of the updated data frame
colnames(flights_df)

# get rid of redundant columns
flights_df <- select(flights_df, -airline, -dep_airport, -dest_airport, -delay_degree, -dep_airport, -dest_airport)

# count class data points
flights_df %>%
  count(delay_class)
# ======================================================================================================

library(dplyr)
library(tidyr)

# find top 5 airports
# Consolidate Flights Data
flights_activity <- flights_df %>%
  select(dep_airport_code, dest_airport_code) %>%
  pivot_longer(everything(), names_to = "airport_role", values_to = "airport_code") %>%
  select(-airport_role)

# Count Flights per Airport
airport_counts <- flights_activity %>%
  group_by(airport_code) %>%
  summarise(flights_count = n()) %>%
  arrange(desc(flights_count))

# Filter Top 5 Airports
top_5_airports <- head(airport_counts, 5)

# Display the result
print(top_5_airports)

# Filter Data Points
flights_df <- flights_df %>%
  filter(dep_airport_code %in% top_5_airports$airport_code | 
           dest_airport_code %in% top_5_airports$airport_code)

# save memory
rm(flights_activity)

# Display the first few rows of the updated data frame to verify
head(flights_df)

# count class data points
flights_df %>%
  count(delay_class)
# ======================================================================================================

# visualize carrier and one-hot encoding
# One-hot encoding for 'uniq_carrier_code'
flights_df$uniq_carrier_code <- factor(flights_df$uniq_carrier_code)
oneHotMatrix <- model.matrix(~ uniq_carrier_code - 1, data = flights_df)
oneHotMatrix <- as.data.frame(oneHotMatrix)

# Correct naming for one-hot encoded columns
newCarrierNames <- gsub("uniq_carrier_code", "", colnames(oneHotMatrix))
newCarrierNames <- gsub("^\\s+|\\s+$", "", newCarrierNames) # Trim whitespace
newCarrierNames <- paste0(newCarrierNames, "_airline") # Add suffix "_airline"

# Combine the original DataFrame with the one-hot encoding matrix
flights_df <- select(flights_df, -uniq_carrier_code)
names(oneHotMatrix) <- newCarrierNames
flights_df <- cbind(flights_df, oneHotMatrix)
# ======================================================================================================

# rearrange the Data Frame layout
# Reorder the dataframe to ensure logical column order
initialColumns <- c("flight_num", "weekday", "month","day_of_month", "scheduled_arr_time", "arr_time", "dep_time")
carrierCodes <- newCarrierNames # Already suffixed and ordered
remainingColumns <- setdiff(names(flights_df), c(initialColumns, "delay_class", carrierCodes))
finalColumnOrder <- c(initialColumns, carrierCodes, remainingColumns, "delay_class")
flights_df <- flights_df[, finalColumnOrder]

# ======================================================================================================

# after the calculation, for later usage, delete delay components and tail #
flights_df <- flights_df %>%
  select(
    -carrier_delay, 
    -weather_delay, 
    -nas_delay, 
    -security_delay, 
    -late_aircraft_delay, 
    -tail_num
  )
# ======================================================================================================

# encoding categorical values (one-hot: airports, cyclic: month(12), date(30), weekday(7), transformation: taxiin,out)

# Convert weekday, month, and day_of_month to cyclic features
flights_df <- flights_df %>%
  mutate(
    sin_weekday = sin(2 * pi * weekday / 7),
    cos_weekday = cos(2 * pi * weekday / 7),
    sin_month = sin(2 * pi * month / 12),
    cos_month = cos(2 * pi * month / 12),
    sin_day = sin(2 * pi * day_of_month / 31),
    cos_day = cos(2 * pi * day_of_month / 31)
  ) %>%
  select(-weekday, -month, -day_of_month)  # Remove the original columns

# One-hot encode the departure and destination airport codes
# although all the airports still remain in the dataframe, only top5 are of interests so only put dummy variables for them

# Create binary features for each of the top 5 airports for departure and arrival
for (airport in top_5_airports$airport_code) {
  flights_df <- flights_df %>%
    mutate(
      !!paste0("dep_", airport) := as.integer(dep_airport_code == airport),
      !!paste0("arr_", airport) := as.integer(dest_airport_code == airport)
    )
}
# Now you have additional binary columns in your dataframe
# indicating if the flight is related to one of the top 5 airports

# since only one hot matrix columns are of interests, remove categorical terms
if ("dep_airport_code" %in% names(flights_df) & "dest_airport_code" %in% names(flights_df)) {
  flights_df <- select(flights_df, -dep_airport_code, -dest_airport_code)
} else {
  message("Columns dep_airport_code and/or dest_airport_code do not exist in the dataframe.")
}

# Log-transform taxiin and taxiout -- normalization
flights_df <- flights_df %>%
  mutate(
    log_taxiin = ifelse(landing_to_gate_min > 0, log(landing_to_gate_min), NA),
    log_taxiout = ifelse(gate_to_takeoff_min > 0, log(gate_to_takeoff_min), NA)
  ) %>%
  select(-landing_to_gate_min, -gate_to_takeoff_min)  # Remove the original columns

# ======================================================================================================
library(dplyr)

# Function to categorize time into day periods
categorize_time <- function(time) {
  time <- sprintf("%04d", time)  # Ensure time has four digits
  hours <- as.integer(substr(time, 1, 2))  # Extract hour part
  
  # Assign time periods
  if (hours >= 5 && hours < 12) {
    'Morning'
  } else if (hours >= 12 && hours < 17) {
    'Afternoon'
  } else if (hours >= 17 && hours < 21) {
    'Evening'
  } else {
    'Night'
  }
}

# Apply the categorize_time function
flights_df <- flights_df %>%
  mutate(
    arr_period = sapply(arr_time, categorize_time),
    dep_period = sapply(dep_time, categorize_time)
  ) %>%
  select(-arr_time, -dep_time)  # Remove the original time columns

# Add a unique prefix to the time of day encoded columns for clarity 
# -- if not, it will be confused with airport encoded columns with the same name pattern "arr_|dep_" in the rellocating
# One-hot encoding the period columns
arr_period_encoded <- model.matrix(~ arr_period - 1, data = flights_df)
dep_period_encoded <- model.matrix(~ dep_period - 1, data = flights_df)

# Convert matrices to data frames
arr_period_encoded_df <- as.data.frame(arr_period_encoded)
dep_period_encoded_df <- as.data.frame(dep_period_encoded)

# Rename columns to ensure they are distinguishable
names(arr_period_encoded_df) <- paste("timearr", names(arr_period_encoded_df), sep = "_")
names(dep_period_encoded_df) <- paste("timedep", names(dep_period_encoded_df), sep = "_")

# Combine the original dataframe with the encoded dataframes
flights_df <- cbind(flights_df, arr_period_encoded_df, dep_period_encoded_df)

# Now, identify the one-hot encoded columns for the time periods
time_period_cols <- c(names(arr_period_encoded_df), names(dep_period_encoded_df))

# Move the one-hot encoded time period columns right after 'scheduled_arr_time'
# and ensure 'scheduled_arr_time' exists and is spelled correctly
flights_df <- flights_df %>%
  relocate(all_of(time_period_cols), .after = "scheduled_arr_time")

# normalize distance with min-max method (no harm doing this)
flights_df$distance_miles_normalized <- (flights_df$distance_miles - min(flights_df$distance_miles)) / 
  (max(flights_df$distance_miles) - min(flights_df$distance_miles))

# Optionally, remove the original categorical time period columns if they are no longer needed
flights_df <- select(flights_df, -arr_period, -dep_period, -distance_miles)

rm(arr_period_encoded, arr_period_encoded_df, dep_period_encoded, dep_period_encoded_df)
# ======================================================================================================

# After the Random Forest Inference on importance of features
# 9 features are selected according to the ranking of importance and multicolinearity 
# Another round of rearrangement

flights_df <- flights_df %>%
  select(
    timedep_dep_periodEvening,
    timedep_dep_periodNight,
    WN_airline,
    dep_DFW,
    arr_ORD,
    arr_delay,
    log_taxiin,
    log_taxiout,
    distance_miles_normalized,
    delay_class  
  )

# Remove NA in the data frame last to ensure the text files have no such things (MATLAB load() cannot handle NAs)

flighs_df <- na.omit(flights_df)

# # Ensure that 'delay_class' is a factor if this is a classification problem
# flights_df$delay_class <- as.factor(flights_df$delay_class)
# cannot turn delay_class into a factor (MATLAB cannot read text type as a class)

# Although in EDA,it is spotted that Airline Alaska has abnormal value in June. 
# since both the airlines and month is not included, not in concern.
# ======================================================================================================

# Data set separation --> Training, Testing, Validation

library(caret)

# Set seed for reproducibility
set.seed(123)

# Decide on the size of each dataset
train_size <- 0.7
validation_size <- 0.15
test_size <- 0.15  # This should add up to 1 with the train_size

# Create indices for the training set
train_index <- createDataPartition(flights_df$delay_class, p = train_size, list = FALSE)

# Subset the training data
training_set <- flights_df[train_index, ]

# Create a temporary dataset excluding the training set
temp_data <- flights_df[-train_index, ]

# Calculate the proportion of validation set relative to the temp_data
validation_prop <- validation_size / (validation_size + test_size)

# Create indices for the validation set based on the temp_data
validation_index <- createDataPartition(temp_data$delay_class, p = validation_prop, list = FALSE)

# Subset the validation and test data
validation_set <- temp_data[validation_index, ]
testing_set <- temp_data[-validation_index, ]

# Remove rows with NA values
training_set <- na.omit(training_set)
validation_set <- na.omit(validation_set)
testing_set <- na.omit(testing_set)

# Check the number of rows in each set
nrow(training_set)
nrow(validation_set)
nrow(testing_set)

rm(temp_data, oneHotMatrix, train_index, validation_index)
# ======================================================================================================

# Output the three data sets to .txt file (read by MATLAB and other software)
# in the format with no column headers

# Write the training data to a .txt file
write.table(training_set, "training_data.txt", sep = "\t", row.names = FALSE, col.names = FALSE)

# Write the validation data to a .txt file
write.table(validation_set, "validation_data.txt", sep = "\t", row.names = FALSE, col.names = FALSE)

# Write the testing data to a .txt file
write.table(testing_set, "testing_data.txt", sep = "\t", row.names = FALSE, col.names = FALSE)

# ======================================================================================================

# With all the efforts, this is perfectly done! :)