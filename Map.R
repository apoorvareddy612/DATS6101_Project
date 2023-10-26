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

address <- c("Country", "City", "County", "Street", "Zipcode")
df_add <- accidents %>% select(-all_of(address))

df_add <- df_add %>% 
  mutate(Severity = as.character(Severity)) %>% 
  mutate_if(is.logical, as.character)

df_add <- df_add %>% 
  type_convert() %>%
  mutate(Severity = factor(Severity)) %>%
  mutate_if(is.logical, factor) %>%
  mutate_if(is.character, factor)

states <- map_data("state") %>% as_tibble() %>% select(long, lat, group, region)
states_abb <- read_csv("data.csv") %>%
  mutate(State = tolower(State)) %>%
  select(State, Code) %>%
  rename("State_full" = State)
accident_count <- df_add %>%
  count(State) %>%
  left_join(states_abb, by = c("State" = "Code"))

states <- states %>%
  left_join(accident_count, by = c("region" = "State_full"))
# top 10 states
top_10 <- accident_count %>%
  arrange(desc(n)) %>%
  head(10)
top_10 <- top_10$State %>% unlist()

top_10_map <- states %>%
  filter(State %in% top_10)
top_10_label <- top_10_map %>%
  group_by(region, State) %>%
  summarise(long = mean(long), lat = mean(lat))

ggplot(states, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = n), color = "#636363", size = 0.1) +
  geom_polygon(data = top_10_map, color = "red", fill = NA, size = 0.8) +
  scale_fill_gradient(low = "#fee5d9", high = "#de2d26",
                      name = "Accident Count", labels = unit_format(unit = "K", scale = 1e-03)) +
  ggrepel::geom_label_repel(mapping = aes(label = State, group = 1), data = top_10_label) +
  theme_minimal() +
  coord_quickmap() +
  labs(title = "Accident distribution in the U.S.",
       x = "Longitude",
       y = "Latitude")