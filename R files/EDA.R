# Aim to conduct EDA on the data to draw inference that help machine learning model development
# First few steps of data preprocessing is the same (cleaning, renaming, categorizing, etc.)
# mostly draw inference in context of overall population (rather than filtered data based on research interest's)
# but it is assumed that underlying pattern/ characteristics shown in the whole population is similar in the subset of interests

# Clear the workspace
rm(list = ls())

library(tidyverse)
library(janitor)
# detach("package:plyr") # detaching both libraries ...
# detach("package:dplyr")
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

local_path <- "C:/Users/hu437/OneDrive/Desktop/CEE_Project/Flight_delay.csv"
flights_df <- read_csv(local_path)

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


vector <- c()
for (i in names(flights_df)) {
  if (is_double(flights_df[[i]][2]) == TRUE) {
    if (sum(flights_df[i]) == 0 ) {
      vector <- append(vector, i)
    }
  }
}
cat("the vector contains columns:", vector, sep="\n-")
flights_df <- select(flights_df, -all_of(vector))

flights_df <- mutate(flights_df,
                     total_delay = (carrier_delay + weather_delay + nas_delay + 
                                      security_delay + late_aircraft_delay))

library(lubridate)
flights_df <- flights_df %>% mutate(month = month(dmy(date)))


library(dplyr)

flights_df <- flights_df %>% 
  mutate(delay_degree = case_when(
    total_delay <= 15 ~ "No delay",
    total_delay >= 45 ~ "Large delay",
    TRUE ~ "Medium delay"  # 'TRUE' acts as the 'else' condition
  ))

# check the data out :)
glimpse(flights_df)
 









# EDA
# ======================================================================================================
# firstly count delay classes number (since it can be seen that there are little no delay cases)
delay_counts <- table(flights_df$delay_degree)
print(delay_counts)

# no delay is considerably less than the two other classes

barplot(delay_counts, main = "Counts of Each Delay Degree", xlab = "Delay Degree", ylab = "Count", col = "blue")

# bar plot with large vs. no+medium
ggplot(flights_df, aes(x = factor(ifelse(delay_degree %in% c("No delay", "Medium delay"), "no_medium", delay_degree)))) +
  geom_bar(aes(fill = delay_degree)) +
  labs(title = "Sum of No and Medium Delays vs. Large Delays", x = "Delay Category", y = "Count") +
  scale_fill_manual(values = c("no_medium" = "blue", "Large delay" = "red")) +
  theme_minimal()
# can be seen as balanced dataaset (no need for undersampling)
# ======================================================================================================

# try to find whether Arr_Delay = Carrier_Delay + Late_Aircraft_Delay (in the first few observations)
library(ggplot2)

# use ggplot to compare the sum and the Arr_Delay
ggplot(flights_df, aes(x = carrier_delay + late_aircraft_delay, y = arr_delay)) +
  geom_point(alpha = 0.3) +  # Using semi-transparent points for dense scatter plots
  geom_smooth(method = "lm") +  # Adds a linear regression fit line
  labs(title = "Comparison of arr_delay vs. Sum of carrier and late aircraft delays",
       x = "Sum of Carrier and Late Aircraft Delays",
       y = "Arrival Delay") +
  theme_minimal()

library(dplyr)
# 
# # Attempting to use n() after explicitly grouping by a constant
# summary_df <- flights_df %>%
#   group_by(Group = 1) %>%  # Create a dummy group to avoid 
#   summarise(
#     Count = n(),  # Total number of rows
#     Matches = sum(arr_delay == (carrier_delay + late_aircraft_delay)),
#     Percentage = mean(arr_delay == (carrier_delay + late_aircraft_delay)) * 100,
#     .groups = 'drop'  # Drop the grouping after summarising
#   )
# # always has error with use of n(), 'Must only be used inside data-masking verbs like `mutate()`, `filter()`, and `group_by()`.

# Calculate the total number of observations
total_count <- nrow(flights_df)
# Calculate the number of matches where 'arr_delay' equals the sum of 'carrier_delay' and 'late_aircraft_delay'
matches_count <- sum(flights_df$arr_delay == (flights_df$carrier_delay + flights_df$late_aircraft_delay))
# Calculate the percentage of matches
matches_percentage <- (matches_count / total_count) * 100
# Print results
cat("Total Count: ", total_count, "\n")
cat("Matches: ", matches_count, "\n")
cat("Percentage: ", matches_percentage, "%\n")
# 45% matches, the relationship seems to be significant but there must be other factors
# ======================================================================================================

# further explore the relationship between delay components(5 of them) and arr,dep_delay

# correlation matrix
delay_data <- flights_df[, c("arr_delay", "dep_delay", "carrier_delay", "weather_delay", "nas_delay", "security_delay", "late_aircraft_delay")]
correlation_matrix <- cor(delay_data, use = "complete.obs")  # Handle missing values if necessary
# Display the correlation matrix
print(correlation_matrix)

# visualize correlation matrix in a heatmap
library(ggplot2)
library(reshape2)

correlation_matrix <- cor(flights_df[, c("arr_delay", "dep_delay", "carrier_delay", "weather_delay", "nas_delay", "security_delay", "late_aircraft_delay")], use = "pairwise.complete.obs")

# Melt the correlation matrix for visualization
melted_corr <- melt(correlation_matrix)

# Create a heatmap
ggplot(melted_corr, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +  # Draw the tiles for heatmap
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), name = "Correlation") +
  theme_minimal() +
  labs(title = "Heatmap of Correlation Between Delays", x = "Variables", y = "Variables") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# arrival and departure delay has 0.94 correlation while other components have no more than 0.506 correlation
# ======================================================================================================

library(tidyr)
# create histograms for some attributes to see distribution
selected_columns <- c("landing_to_gate_min", "gate_to_takeoff_min", "arr_delay", "distance_miles")
flights_df_long <- pivot_longer(flights_df, cols = selected_columns, names_to = "metric", values_to = "value")

library(ggplot2)

ggplot(flights_df_long, aes(x = value)) + 
  geom_histogram(bins = 30, fill = 'blue', alpha = 0.7) + 
  facet_wrap(~metric, scales = 'free') +
  theme_minimal() + 
  labs(x = 'Value', y = 'Count', title = 'Histograms of Flight Metrics') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# ======================================================================================================

# use Z-score method to identify and handle outliers (in arr, dep delay and distance) -- further insights
# Load necessary libraries
library(ggplot2)
library(dplyr)
# Create a temporary dataframe for Z-score calculations
flights_df_temp <- flights_df %>%
  select(landing_to_gate_min, gate_to_takeoff_min, arr_delay, distance_miles) %>%
  mutate(across(everything(), ~(. - mean(. , na.rm = TRUE)) / sd(. , na.rm = TRUE), .names = "z_{.col}")) %>%
  pivot_longer(cols = starts_with("z_"), names_to = "variable", values_to = "z_score")
# Plotting
ggplot(flights_df_temp, aes(x = variable, y = z_score)) +
  geom_boxplot() +
  geom_hline(yintercept = c(-3, 3), linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Z-score distribution for taxin, taxout arr_delay, and distance",
       y = "Z-score",
       x = "") +
  coord_flip()
# Clean up (if needed, but since it's a temporary dataframe, it might not be necessary)
rm(flights_df_temp)
# This turns out to be shit-- outliers (outside red dash) are more than normal values -- highly extreme and not normal(skewness)
# very tailed distribution
# normalization, regularization, feature engineering (done and examined in data preprocessing file)
# ======================================================================================================

# what airline has the most delay time in the given time frame
flights_df %>% 
  dplyr::group_by(airline) %>% 
  drop_na() %>% 
  summarize(accumulated_delay = sum(total_delay)) %>% 
  arrange(-accumulated_delay)
# Soutwest seems to be the worst, but it has the highest number of flights

# # of flights for each airline
as.data.frame(table(flights_df$airline)) %>% arrange(-Freq)

# bar chart with number of flights for each airline
ggplot(flights_df) +
  geom_bar(aes(x = airline), fill = "#00CC99", color = "#009933", alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) + 
  labs(title = "Number of Flights US Airlines",
       x = "Airline", y = "Number of Flights")

# mean total delay time --> unbiased measure
# flights_df %>% 
#   group_by(airline) %>% 
#   drop_na() %>% 
#   summarize(delay = mean(total_delay)) %>% 
#   arrange(-delay)
#  only one output (not desired)

library(dplyr)

# Correctly calculate the mean total delay per airline
# Here we use dplyr::summarize (specify library) to avoid any namespace clashes 
# -- function from dplyr and plyr have the same name but function differently 
avg_delay_by_airline <- flights_df %>%
  dplyr::group_by(airline) %>%
  dplyr::summarize(delay = mean(total_delay, na.rm = TRUE), .groups = 'drop')

# Display the resulting tibble
print(avg_delay_by_airline)

# it can be seen that JetBlue Airways has highest avg delay time
# ======================================================================================================

# then analyze the biggest driver of the delay (may serve for validation in machine learning model, pareto)
# whether sum and mean has the same ranking?
df1 <- flights_df %>%  summarize(carrier = sum(carrier_delay),
                                 weather = sum(weather_delay),
                                 nas = sum(nas_delay),
                                 security = sum(security_delay),
                                 late_aircraft = sum(late_aircraft_delay)) %>% 
  pivot_longer(cols=1:5, names_to = 'Delay_Type', values_to = 'Accumulated_Delay') %>% 
  arrange(-Accumulated_Delay)

df2 <- flights_df %>% summarize(carrier = mean(carrier_delay),
                                weather = mean(weather_delay),
                                nas = mean(nas_delay),
                                security = mean(security_delay),
                                late_aircraft = mean(late_aircraft_delay)) %>% 
  pivot_longer(cols=1:5, names_to = 'Delay_Type', values_to = 'Average_Delay') %>% 
  arrange(-Average_Delay)
# turns out to be the same
# merge two data using same column(key), just like SQL
# in R, unless explicitly assigned to a variable, the data/ values will just print and lost

# draw the bar plot of avg, sum delay of different delay type
merge(df1, df2) %>% 
  arrange(-Average_Delay) %>%  
  pivot_longer(cols = c("Accumulated_Delay", "Average_Delay"), 
               names_to ="Method", values_to = "Value") %>% 
  ggplot() + 
  geom_bar(aes(x = reorder(Delay_Type, -Value), y = Value, fill = Delay_Type), 
           color = "dark grey", alpha = 0.9, stat="identity", position = "dodge") + 
  facet_wrap(~Method, scale = "free") + 
  scale_y_continuous(labels = format_format(big.mark = ",", scientific = FALSE)) +
  labs(x = "Delay Type", y = "Delay (min)", fill = "") +
  theme(legend.position="top", axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  scale_fill_brewer(palette = 14)

# 1. avg, sum has similar difference and rank
# 2. late_aircraft > carrier > nas > security > weather
# ======================================================================================================

# display avg delay of airlines per month
startdate <-  min(flights_df$date)
enddate <-  max(flights_df$date)

# Calculate average delay per airline and month
ag <- flights_df %>%
  dplyr::group_by(airline, month) %>%
  dplyr::summarize(delay = mean(total_delay, na.rm = TRUE), .groups = 'drop')

ggplot(data=ag) +
  geom_bar(aes(x = reorder(airline, -delay), y = delay, fill = airline), 
           stat = "identity", width = 0.6) +
  labs(title = "Average Delay per Airline", subtitle = paste("From", startdate, "to", enddate),
       caption = "by Markus Köfler", x = "Airlines", y = "Average Delay (min)") +
  theme(axis.text.x = element_blank()) + 
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
  facet_wrap(~month) #  create a separate plot for each month value

# fluctuate month to month, indicating month may contribute to the delay (weather, vacation season)
# it can be seen that there is no delay for Alaska Airline in June, is it because it can achieve 0 delay during the whole month
# Or it is due to data set defects?
nrow(filter(flights_df, airline=="Alaska Airlines Inc." & month==6))
# 0 flight by the airline in June --> further research required
# ======================================================================================================

# the relationship between flight delay and flight distance
# scatter plot
# jitter: avoid overlapping with a lot of data (over plotting) by adding little random variation to each point
# Adds a smoothed conditional mean line to the plot, fitting a linear model (method = "lm") to the data. 
# This line is meant to show the trend or relationship between distance and delay. 
# It's colored red to stand out against the jittered points.

ggplot2::ggplot(flights_df) +
  ggplot2::geom_jitter(ggplot2::aes(distance_miles, total_delay), alpha = 0.1, shape = 1, color = "navy") +
  ggplot2::geom_smooth(ggplot2::aes(distance_miles, total_delay), color = "red", method = "lm") +
  ggplot2::facet_wrap(~airline, scales = "free", shrink = FALSE) +
  ggpubr::stat_cor(ggplot2::aes(distance_miles, total_delay), 
                   color = "red", geom = "label", fill = "transparent") +
  ggplot2::labs(title = "Correlation between Flight Distance and Total Delay", 
                subtitle = "Individual Airlines",
                x = "Distance (miles)", y = "Delay (min)") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1))

# fit a linear model for each airline with calculated coefficients, see relationship, distribution, trend of delay
# Conclusion: Weak correlation between distance and delay in each airline (even some have p value less than 0.05)
# no consistent trend (some positive, some negative)
# overall, may not be the feature. However, interaction effect can be investigated (since only linear relationship is weak)
# since EC, SVM algorithms can identify complex relationships, distance remains (also due to other studies selecting this)
# ======================================================================================================

# the correlation between distance, scheduled, actual elapsed time, airtime
# Calculate the correlation matrix
correlation_matrix <- cor(flights_df[, c("actual_flight_time_min", "estimate_flight_time_min", "distance_miles","air_time_min")], use = "complete.obs")

library(ggplot2)
library(reshape2)

# Melt the correlation matrix
melted_corr <- melt(correlation_matrix)
# Create a heatmap
ggplot(melted_corr, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +  # Use white lines to separate the tiles
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, name = "Correlation") +
  geom_text(aes(label = sprintf("%.2f", value)), color = "black", size = 4) +  # Add text labels
  theme_minimal() +
  labs(title = "Heatmap of Correlation Matrix", x = "", y = "")
# high correlation between these three
# random forest needed to identify importance to decide which to remove
# ======================================================================================================

# Random Forest
# 1. Mean Decrease Impurity (MDI): node impurity (variance in regression, Gini impurity for classification)
# weighted by the probability of reaching that node across all the trees
# 2. Mean Decrease Accuracy (MDA): observing the mix-up of the values of a particular feature, 
# measuring decreasing in accuracy


# ======================================================================================================


