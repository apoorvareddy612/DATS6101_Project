library(tidyverse)
library(lubridate)
library(readr)
library(patchwork) #install.packages("patchwork",repos = "https://cloud.r-project.org")

accidents <- read_csv('US_accidents_EDA.csv')
accidents_new <- accidents %>% mutate(startHr=hour(Start_Time))

accidents_severity <- accidents_new %>% group_by(startHr) %>% summarise(mean(Severity))
accident_summary <- merge(accidents_count, accidents_severity)
accident_summary <- accident_summary %>% rename(sev_avg = "mean(Severity)")
#1
ggplot(data = accident_summary) + 
  geom_col(mapping=aes(x=startHr, y=n, fill=sev_avg)) +
  scale_fill_distiller(palette="Reds", trans= "reverse") +
  labs(
    title = "Amount of car accidents by hour",
    x = "Hour",
    y = "Number of accidents",
    caption = "A Countrywide Traffic Accident Dataset, 2016-2023.",
    fill = "Average Severity") +
  scale_y_continuous(expand = expansion(mult = c(0, .1)))

#2
accidents %>% group_by(Weather_Condition) %>% summarize(percentage=((n()/nrow(accidents)) * 100)) %>% arrange(-percentage) %>% filter(percentage > 1) -> weather_conditions_per

options(repr.plot.width = 22, repr.plot.height = 10)
weather_conditions_per %>% ggplot() +
  geom_col(mapping = aes(x=reorder(Weather_Condition, -percentage), y = percentage, fill = Weather_Condition)) + 
  labs(x = "Weather condition", y="%", title ="Percentage of accidents by weather condition") +
  theme(text= element_text(size=18))

#3
accidents %>% 
  group_by(Astronomical_Twilight) %>% 
  summarise(percentage= ((n()/nrow(accidents))*100) ) -> astronomical_twilight_per
astronomical_twilight_per

options(repr.plot.width = 20, repr.plot.height = 8)
astronomical_twilight_per %>% ggplot() +
  geom_col(mapping = aes(x=Astronomical_Twilight, y = percentage, fill=Astronomical_Twilight)) +
  labs(x = "Time of the day (astronomical twilight)", y="%", title ="Accidents by time of the day (astronomical twilight)") +
  theme(text = element_text(size=18))

#4
accidents %>%
  group_by(Severity) %>%
  summarise(percentage = n() / nrow(accidents) *100) -> severity_per
severity_per

options(repr.plot.width = 20, repr.plot.height = 8)
severity_per %>%
  ggplot() + 
  geom_col(mapping = aes(x=Severity, y=percentage, fill=Severity)) +
  labs(x = "Severity", y="%", title ="Accidents by severity") +
  theme(text = element_text(size=18))

#5
accidents %>%
  group_by(Weather_Condition) %>%
  filter(Severity==4) %>%
  summarise(count = n()/nrow(.)*100) %>%
  filter(count>1)%>%
  arrange(-count) -> severity4_weather
severity4_weather

options(repr.plot.width = 20, repr.plot.height = 8)
severity4_weather %>%
  ggplot() +
  geom_col(mapping=aes(x=reorder(Weather_Condition, -count), y=count, fill=Weather_Condition))+ 
  theme(text = element_text(size=18)) +
  labs(x = "Weather condition", y="%", title ="Severity 4 acccidents by weather condition")-> severity4_weather_plot

accidents %>%
  group_by(Weather_Condition) %>%
  filter(Severity==3) %>%
  summarise(count = n()/nrow(.)*100) %>%
  filter(count>1)%>%
  arrange(-count) -> severity3_weather
severity3_weather

options(repr.plot.width = 20, repr.plot.height = 8)
severity3_weather %>%
  ggplot() +
  geom_col(mapping=aes(x=reorder(Weather_Condition, -count), y=count, fill=Weather_Condition))+ 
  theme(text = element_text(size=18)) +
  labs(x = "Weather condition", y="%", title ="Severity 3 acccidents by weather condition") -> severity3_weather_plot

accidents %>%
  group_by(Weather_Condition) %>%
  filter(Severity==2) %>%
  summarise(count = n()/nrow(.)*100) %>%
  filter(count>1)%>%
  arrange(-count) -> severity2_weather
severity2_weather

options(repr.plot.width = 20, repr.plot.height = 8)
severity2_weather %>%
  ggplot() +
  geom_col(mapping=aes(x=reorder(Weather_Condition, -count), y=count, fill=Weather_Condition))+ 
  theme(text = element_text(size=18)) +
  labs(x = "Weather condition", y="%", title ="Severity 2 acccidents by weather condition") -> severity2_weather_plot

accidents %>%
  group_by(Weather_Condition) %>%
  filter(Severity==1) %>%
  summarise(count = n()/nrow(.)*100) %>%
  filter(count>1)%>%
  arrange(-count) -> severity1_weather
severity1_weather

severity1_weather %>%
  ggplot() +
  geom_col(mapping=aes(x=reorder(Weather_Condition, -count), y=count, fill=Weather_Condition))+ 
  theme(text = element_text(size=18))+
  labs(x = "Weather condition", y="%", title ="Severity 1 acccidents by weather condition")-> severity1_weather_plot

options(repr.plot.width = 30, repr.plot.height = 16)
severity4_weather_plot + severity3_weather_plot +severity2_weather_plot +severity1_weather_plot