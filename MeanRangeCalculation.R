# Load necessary libraries
library(readr)

# Replace 'path_to_your_data.txt' with the path to your actual data file.
# If your txt file is separated by tabs, you can use read_tsv() instead.
# If it's space-separated, use read_delim() with a space delimiter.
data <- read_delim('C:/Users/hu437/OneDrive/Desktop/CEE_Project/Data/training_data.txt', delim = "\t") # Adjust delimiter as needed

# Function to calculate max, min, and mean
calculate_stats <- function(data, column_name) {
  max_value <- max(data[[column_name]], na.rm = TRUE)
  min_value <- min(data[[column_name]], na.rm = TRUE)
  mean_value <- mean(data[[column_name]], na.rm = TRUE)
  
  return(list(max = max_value, min = min_value, mean = mean_value))
}

# Calculate stats for d1 to d9 and print the results
for(i in 1:9) {
  column_name <- paste("d", i, sep="")
  stats <- calculate_stats(data, column_name)
  cat("Statistics for", column_name, ": Max =", stats$max, "Min =", stats$min, "Mean =", stats$mean, "\n")
}
