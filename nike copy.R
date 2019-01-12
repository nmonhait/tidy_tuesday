library(readr)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(viridis)
library(ggalt)


biketown <- read_csv("~/Desktop/blogdown/monhait/viz/data/week10_biketown.csv")

biketown <- biketown %>% 
  select(PaymentPlan, StartTime, EndTime, StartDate) %>% 
  filter(!is.na(PaymentPlan),
         !is.na(StartTime),
         !is.na(EndTime),
         !is.na(StartDate)) %>% 
  mutate(total_time = (EndTime - StartTime) / 60,
         date = mdy(StartDate)) %>% 
  select(-StartDate) %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  select(-date) %>% 
  unite(month_year, month, year, sep = "-") %>% 
  mutate(start_hour = hour(StartTime),
         start_min = minute(StartTime),
         end_hour = hour(EndTime),
         end_min = minute(EndTime),
         hour_diff = abs(end_hour - start_hour),
         min_diff = abs(end_min - start_min)) %>% 
  select(-total_time, -StartTime, -EndTime) %>% 
  mutate(hour_diff = hour_diff * 60,
         ride_length = hour_diff + min_diff) %>% 
  select(PaymentPlan, month_year, ride_length)

# differences by month
length_by_month <- biketown %>% 
  separate(month_year,c("month", "year"), sep = "-") %>% 
  group_by(month, PaymentPlan) %>% 
  summarize(avg_ride = mean(ride_length))

ggplot(data = length_by_month) +
  geom_col(aes(x = month, y = avg_ride, fill = PaymentPlan),
           position = "dodge")

length_by_year <- biketown %>% 
  separate(month_year,c("month", "year"), sep = "-") %>% 
  group_by(year, PaymentPlan) %>% 
  summarize(avg_ride = mean(ride_length))

ggplot(data = length_by_year) +
  geom_col(aes(x = year, y = avg_ride, fill = PaymentPlan),
           position = "dodge")

counts_month <- biketown %>% 
  separate(month_year,c("month", "year"), sep = "-") %>% 
  group_by(month, PaymentPlan) %>% 
  count()

  
  
ggplot(counts_month, aes(x=month, y=n, color = PaymentPlan)) + 
  geom_point(size= counts_month$n / 8000) +   
  geom_segment(aes(x=month, 
                   xend=month, 
                   y=min(n), 
                   yend=max(n)), 
               linetype= 3, 
               size=0.1) +   
  labs(title="Usage of Nike BIKETOWN System", 
       subtitle="by Month and User Type") +  
  coord_flip() +
  scale_x_discrete(limits = c(1,2,3,4,5,6,7,8,9,10,11,12)) +
  theme_tufte(base_family = "Arial Black") +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        axis.text = element_text(size = 10),
        aspect.ratio = 1/3,
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_color_viridis(option = "inferno", discrete = TRUE, begin = 0.5, end = 0.1) +
  labs(x = "Month", y = "Usage Counts", color = "Payment Type")
  
