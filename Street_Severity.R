# Load necessary libraries
library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)

# Load the dataset (assuming your dataset is stored in a variable called 'accidents_data')
# Replace 'your_dataset.csv' with the actual file path or dataset name
accidents_data <- read.csv('US_accidents.csv')

# Identify streets with the highest severity proportions
highest_severity_streets <- accidents_data %>%
  arrange(desc(SeverityProportion)) %>%
  slice(1)

# Identify streets with the lowest severity proportions
lowest_severity_streets <- accidents_data %>%
  arrange(SeverityProportion) %>%
  slice(1)

# Display the results
cat("Streets with the Highest Severity Proportions:\n")
print(highest_severity_streets[, c("City", "Street", "SeverityProportion")])

cat("\nStreets with the Lowest Severity Proportions:\n")
print(lowest_severity_streets[, c("City", "Street", "SeverityProportion")])