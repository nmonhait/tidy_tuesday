library(readr)
library(tidyverse)
library(ggplot2)
library(gghighlight)
library(ggthemes)
library(viridis)

url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-08/IMDb_Economist_tv_ratings.csv"
tv <- read_csv(url)

parenthood <- tv %>% 
  filter(title == "Parenthood" & date > "2010-01-01")

com_dram_ratings <- tv %>% 
  filter(genres == "Comedy,Drama" & date > "2010-01-01") %>% 
  group_by(title) %>% 
  summarize(rating = mean(av_rating)) %>% 
  filter(title != "Parenthood" & rating > 8) %>% 
  mutate(hit = rating > 8.53)

color_pal = c("grey80", "darkorange1")

parenthood_plot_base <- ggplot(com_dram_ratings, aes(x=reorder(title, rating), y=rating,
                             color = hit, label=rating))  +
  geom_point(stat='identity', size=6) +
  scale_color_manual(values = c("lightpink", "brown1")) +
  geom_segment(aes(y = 8.53, 
                   x = title, 
                   yend = rating, 
                   xend = title,
                   color = hit), 
               size = 2) +
  geom_text(color="black", size=0) +
  labs(title="What to Watch Next if you Loved Parenthood", 
       subtitle="Comedy/Drama Series Ratings Compared to Parenthood Average Rating") + 
  ylim(8, 9.5) +
  coord_flip() +
  geom_hline(yintercept = 8.53, size = 2, color = "dimgray",
             linetype = 3) +
  theme_bw() +
  theme_tufte(base_family = "Tahoma") +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.text = element_text(size = 15),
        plot.subtitle = element_text(size = 15, hjust = 0.5)) +
  labs(x = NULL, y = "Average Rating") +
  theme(legend.position="none")




