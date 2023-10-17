library(lubridate)
accidents <- read_csv("US_Accidents_EDA.csv")
accidents$month <- lubridate::month(accidents$Start_Time)
accidents$accident_count <- 1

accidents$month = factor(accidents$month, order=T)
anova_result <- aov(accident_count ~ month, data = accidents)
summary(anova_result)

xkabledply(anova_result, title = "ANOVA result summary")
#Fail to reject the Null hypothesis


accidents$day_type <- ifelse(weekdays(accidents$Start_Time) %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

contingency_table <- table(accidents$day_type)
print(contingency_table)
chi_square_result <- chisq.test(contingency_table, correct = FALSE)
print(chi_square_result)
#Reject the null hypothsis 
