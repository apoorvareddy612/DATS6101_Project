library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(viridis)
library(ggcorrplot)
library(VIM)
library(stringr)
options(warn=-1)
library(scales)

accidents <- read_csv("US_accidents_EDA.csv")

LATE_NIGHT <- 'Late Night'
NIGHT <- 'Night'
MORNING <- 'Morning'
AFTERNOON <- 'Afternoon'
EVENING <- 'Evening'

INTERVAL_5_6 <- '5 to 6'
INTERVAL_6_7 <- '6 to 7'
INTERVAL_7_8 <- '7 to 8'
INTERVAL_8_9 <- '8 to 9'
INTERVAL_9_10 <- '9 to 10'
INTERVAL_10_11 <- '10 to 11'
INTERVAL_11_12 <- '11 to 12'

accidents$Hour <- strftime(accidents$Start_Time, format="%H")

get_time_of_day <- function(hour) {
  if (hour >= 5 && hour < 12) {
    return(MORNING)
  } else if (hour >= 12 && hour < 16) {
    return(AFTERNOON)
  } else if (hour >= 16 && hour < 20) {
    return(EVENING)
  } else {
    return(NIGHT)
  }
}

get_interval_of_day <- function(hour, time_of_the_day) {
  if(time_of_the_day == MORNING){
    if(hour == 5){
      return(INTERVAL_5_6)  
    } else if (hour == 6) {
      return(INTERVAL_6_7)
    } else if(hour == 7){
      return(INTERVAL_7_8)
    } else if(hour == 8){
      return(INTERVAL_8_9)
    } else if(hour == 9){
      return(INTERVAL_9_10)
    } else if(hour == 10){
      return(INTERVAL_10_11)
    } else if(hour == 11){
      return(INTERVAL_11_12)
    } else{
      return('Not Morning')
    }
  } else {
    return('Not Morning')
  }    
}

get_acc_val_day <- function(Acc_Bool,ID) {
  if(Acc_Bool == "True"){
    return(ID)
  } else {
    return(nrow(accidents)-ID)
  }
}

accidents$Hour <- as.numeric(accidents$Hour)
accidents$time_of_the_day <- mapply(get_time_of_day, accidents$Hour)
accidents$Interval_Morning <- mapply(get_interval_of_day, accidents$Hour, accidents$time_of_the_day)

Time_of_the_day <- accidents$time_of_the_day
ID <- accidents$ID
Severity <- accidents$Severity
Interval_Morning <- accidents$Interval_Morning

#1
bb <- aggregate(ID ~ Time_of_the_day, accidents, FUN=length)
bb$total = nrow(accidents)
bb <- xtabs(bb$ID / bb$total ~ bb$Time_of_the_day, bb)



barplot(t(bb)
        ,main="Comparison of proportion of accidents \noccuring at different times of the day"
        ,ylab="Proportion"
        ,xlab="Time of the day"
        ,col=c("mistyrose")
        ,las=1        
        ,args.legend=list(x="topleft")
        ,axes=F
        ,cex.axis=1.2
        ,cex.names=1.2
        ,cex.lab=1
        ,cex.main=1.2)
axis(side=2,at=c(0,0.1,0.2,0.3,0.4),labels=c("0","0.1","0.2","0.3","0.4"),las=2)

#2

a.Time_of_the_day.Severity <- aggregate(ID ~ Time_of_the_day + Severity, accidents, FUN=length)
a.Time_of_the_day.Size <- aggregate(ID ~ Time_of_the_day, FUN=length)
a <- merge(a.Time_of_the_day.Severity, a.Time_of_the_day.Size, by = "Time_of_the_day")
colnames(a) <- c("Time_of_the_Day", "Severity", "Severity.Count", "Total.Count")
xtab <- xtabs(Severity.Count / Total.Count ~ Time_of_the_Day + Severity, a)

par(mar=c(7,7,4.1,2.1))

barplot(t(xtab)
        ,main="main"
        ,ylab="ylab"
        ,xlab="xlab"
        ,col=c("lightblue","mistyrose","lightgreen","red")
        ,las=1 
        ,legend.text=c("Severity1","Severity2","Severity3","Severity4")
        ,args.legend=list(x="topleft")
        ,axes=F
        ,beside=FALSE)
axis(side=2,at=c(0,0.5,1),labels=c("0%","50%","100%"))
mtext("Time of the day",side=1,line=6)

#3
accidents$hr <- lubridate::hour(accidents$Start_Time)
accidents$day <- lubridate::day(accidents$Start_Time)
accidents$week <- lubridate::week(accidents$Start_Time)
accidents$month <- lubridate::month(accidents$Start_Time)
accidents$Weekday <- lubridate::wday(accidents$Start_Time, label=T, abbr=F)
accidents$Weekdaya <- lubridate::wday(accidents$Start_Time, label=T, abbr=T)
accidents$Year <- lubridate::year(accidents$Start_Time)


accidents %>% group_by(hr, Weekday) %>%
  summarise(n = n()) %>%
  ggplot(aes(hr, n, fill = Weekday)) + 
  geom_bar(stat = "identity", show.legend = F) +
  facet_grid(~Weekday) +   labs(x = "24 hours of the day", y = "Count of Accidents", 
                                title = "Accidents by Day & Hour") + theme_light(base_size = 18) +
  scale_fill_brewer(palette=("Oranges")) + 
  theme(strip.background = element_rect(fill="grey90")) +
  theme(strip.text = element_text(size = 16, color = 'black')) +
  scale_y_continuous(labels = scales::comma)

#4
states_abb <- accidents %>% mutate(State = tolower(State)) %>% select(State) %>% rename("State_full" = State)
accident_count <- accidents %>% count(State)

# top 10 states
top_10 <- accident_count %>%
  arrange(desc(n)) %>%
  head(10)
top_10 <- top_10$State %>% unlist()

accidents %>% 
  filter(State %in% top_10) %>%
  count(State) %>%
  ggplot(aes(reorder(State, n), n)) +
  geom_col() +
  geom_label(aes(label = n), nudge_y = -30000) +
  labs(x = NULL, y = "Number of accidents",
       title = "Top 10 States with the most accidents") +
  scale_x_discrete(labels = rev(c("California", "Florida","Texas", "South Carolina","New York",
                                  "North Carolina", "Pennsylvania","Virginia",
                                  "Minnesota", "Oregon"))) +
  scale_y_continuous(breaks = seq(0, 700000, 100000), labels = scales::unit_format(unit = "K", scale = 1e-03)) +
  coord_flip()




