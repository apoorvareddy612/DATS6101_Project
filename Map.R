library(tidyverse)
library(scales)
library(lubridate)
library(plotly)
library(gridExtra)
library(tidytext)
library(modelr)
library(caret)
library(ROSE)
library(glmnet)
library(rpart)
library(rpart.plot)
library(randomForest)
options(warn = -1)

accidents <- read_csv("US_Accidents_EDA.csv")

fig <- function(width, heigth){
  options(repr.plot.width = width, repr.plot.height = heigth)
}

fig(13, 8)
ggplot(df, aes(Wind_Direction, ..prop.., group = Severity)) +
  geom_bar(aes(fill = Severity), position = "dodge") +
  scale_y_continuous(labels = percent) +
  labs(x = "Wind Direction",
       y = "Proportion",
       title = "Wind direction does not have a great impact on severity") +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.6))

