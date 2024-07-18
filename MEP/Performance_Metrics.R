# Clear the workspace
rm(list = ls())

# Set the working directory
setwd("C:/Users/hu437/OneDrive/Desktop/CEE_Project/GEP")

# Load necessary libraries
library(caret)
library(pROC)
library(gridExtra)
library(knitr)  # For kable, which makes nice tables
library(gridGraphics)
library(knitr)

# Function to load data
load_data <- function(filename) {
  if (!file.exists(filename)) {
    stop("File does not exist: ", filename)
  }
  data <- read.table(filename, header = FALSE)
  if (ncol(data) < 9) {
    stop("Data does not contain enough columns.")
  }
  features <- data[, -ncol(data)]
  class <- data[, ncol(data)]
  list(features = features, class = class)
}

# Model function - replicating MATLAB's gepModel function
gepModel <- function(d) {
  if (length(d) < 9) {
    stop("Not enough data points in the vector.")
  }
    # just to ensure the program will return text rather than error and crash
  ROUNDING_THRESHOLD <- 1.65696345809307E+16
  G4C3 <- 9.81078524124882
  y <- exp(d[1]) + d[4]
  y <- y + exp(d[3]) * ((d[2] - d[8]) + (d[5] - d[9]) - ((d[5] - d[1]) * (d[6] - d[2])))
  y <- y + d[5]
  y <- y + ((d[2] + d[8] - d[8]) - (d[4] - (d[6] ^ G4C3)))
  
  if (y >= ROUNDING_THRESHOLD) {
    return(1)
  } else {
    return(0)
  }
}

# Function to predict using the model -- applies gepModel to each row in data
model_function <- function(data) {
  # Applying gepModel to each row of features
  predictions <- apply(data$features, 1, gepModel)
  return(predictions)
}
# ===========================================================================================================

# Function for additional validation metrics
metrics2 <- function(actual, predicted_scores) {
  k <- sum(actual * predicted_scores) / sum(actual^2)
  k_prime <- sum(actual * predicted_scores) / sum(predicted_scores^2)
  R <- cor(actual, predicted_scores)
  m <- (R^2 - k^2) / R^2
  n <- (R^2 - k_prime^2) / R^2
  Rm <- R^2 * (1 - sqrt(abs(R^2 - k^2) / (1 - R^2)))
  Ro2 <- 1 - sum((actual - k * predicted_scores)^2) / sum((actual - mean(actual))^2)
  Ro2_prime <- 1 - sum((actual - k_prime * predicted_scores)^2) / sum((actual - mean(actual))^2)
  data.frame(k, k_prime, R, m, n, Rm, Ro2, Ro2_prime)
}

# Function to evaluate model, plot ROC curve, confusion matrix, and display metrics
evaluate_model_and_plot_ROC <- function(data, dataset_name) {
  actual <- as.numeric(data$class)  # Convert factors to numeric
  predicted_scores <- model_function(data)
  
  conf_matrix <- confusionMatrix(as.factor(predicted_scores), as.factor(actual))
  ROC_curve <- roc(actual, predicted_scores)
  AUC_info <- auc(ROC_curve)
  
  # Calculate and plot standard metrics
  MSE <- mean((actual - predicted_scores)^2)
  MAE <- mean(abs(actual - predicted_scores))
  MAPE <- mean(abs((actual - predicted_scores) / actual)) * 100
  R2 <- 1 - sum((actual - predicted_scores)^2) / sum((actual - mean(actual))^2)
  metrics_table <- data.frame(Metric = c("MSE", "MAE", "MAPE", "R2"), Value = c(MSE, MAE, MAPE, R2))
  
  roc_plot <- ggroc(ROC_curve) + ggtitle(paste(dataset_name, "ROC Curve with AUC:", round(AUC_info, 3)))
  confusion_plot <- plot(conf_matrix$table, main = paste(dataset_name, "Confusion Matrix"), col = c("red", "green"))
  grid.echo()
  confusion_grob <- grid.grab()
  metrics_plot <- tableGrob(kable(metrics_table, caption = paste(dataset_name, "Performance Metrics")))
  
  # Display standard metrics and plots
  grid.arrange(roc_plot, confusion_grob, metrics_plot, ncol = 3)
  
  # Check if it's validation data to display additional metrics
  if (dataset_name == "Validation") {
    additional_metrics <- metrics2(actual, predicted_scores)
    additional_metrics_plot <- tableGrob(kable(additional_metrics, caption = "Additional Validation Metrics"))
    grid.newpage()
    grid.draw(additional_metrics_plot)
  }
}

# ===========================================================================================================
# Load data
train <- load_data("training_data.txt")
test <- load_data("testing_data.txt")
valid <- load_data("validation_data.txt")

# Evaluate and visualize
train_metrics <- evaluate_model_and_plot_ROC(train, "Training")
test_metrics <- evaluate_model_and_plot_ROC(test, "Testing")
valid_metrics <- evaluate_model_and_plot_ROC(valid, "Validation")
