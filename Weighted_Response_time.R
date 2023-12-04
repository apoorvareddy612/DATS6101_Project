# Load necessary libraries
library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)

# Load the dataset (assuming your dataset is stored in a variable called 'accidents_data')
# Replace 'your_dataset.csv' with the actual file path or dataset name
accidents_data <- read.csv('US_accidents.csv')

accidents_data$Weighted_Response_Time <- accidents_data$Response.time * accidents_data$Weights



# Alternatively, plot a boxplot to visualize the distribution
ggplot(accidents_data, aes(x = 1, y = Weighted_Response_Time)) +
  geom_boxplot(fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Boxplot of Weighted Response Times",
       x = "",
       y = "Weighted Response Time") +
  coord_cartesian(ylim = c(0, 0.001))
