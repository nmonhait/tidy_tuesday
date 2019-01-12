library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(jpeg)
library(ggthemes)
library(gghighlight)
library(extrafont)
library(grid)

url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-08-28/nfl_2010-2017.csv"
nfl <- read_csv(url)

#filter to superbowl only teams and corresponding year
stl_2000 <- nfl %>% 
  filter(game_year == 2000 & team == "STL") %>% 
  select(team, game_year, rush_avg, rush_tds, rec_avg, rec_tds, position) %>% 
  group_by(position, game_year, team) %>% 
  summarize(rush_points = sum(rush_tds, na.rm = TRUE) * 6,
            rec_points = sum(rec_tds, na.rm = TRUE) * 6,
            total_points = rush_points + rec_points)
bal_2001 <- nfl %>% 
  filter(game_year == 2001 & team == "BAL") %>% 
  select(team, game_year, rush_avg, rush_tds, rec_avg, rec_tds, position) %>% 
  group_by(position, game_year, team) %>% 
  summarize(rush_points = sum(rush_tds, na.rm = TRUE) * 6,
            rec_points = sum(rec_tds, na.rm = TRUE) * 6,
            total_points = rush_points + rec_points)
ne_2002 <- nfl %>% 
  filter(game_year == 2002 & team == "NE") %>% 
  select(team, game_year, rush_avg, rush_tds, rec_avg, rec_tds, position) %>% 
  group_by(position, game_year, team) %>% 
  summarize(rush_points = sum(rush_tds, na.rm = TRUE) * 6,
            rec_points = sum(rec_tds, na.rm = TRUE) * 6,
            total_points = rush_points + rec_points)
tb_2003 <- nfl %>% 
  filter(game_year == 2003 & team == "TB") %>% 
  select(team, game_year, rush_avg, rush_tds, rec_avg, rec_tds, position) %>% 
  group_by(position, game_year, team) %>% 
  summarize(rush_points = sum(rush_tds, na.rm = TRUE) * 6,
            rec_points = sum(rec_tds, na.rm = TRUE) * 6,
            total_points = rush_points + rec_points)
ne_2004 <- nfl %>% 
  filter(game_year == 2004 & team == "NE") %>% 
  select(team, game_year, rush_avg, rush_tds, rec_avg, rec_tds, position) %>% 
  group_by(position, game_year, team) %>% 
  summarize(rush_points = sum(rush_tds, na.rm = TRUE) * 6,
            rec_points = sum(rec_tds, na.rm = TRUE) * 6,
            total_points = rush_points + rec_points)
ne_2005 <- nfl %>% 
  filter(game_year == 2005 & team == "NE") %>% 
  select(team, game_year, rush_avg, rush_tds, rec_avg, rec_tds, position) %>% 
  group_by(position, game_year, team) %>% 
  summarize(rush_points = sum(rush_tds, na.rm = TRUE) * 6,
            rec_points = sum(rec_tds, na.rm = TRUE) * 6,
            total_points = rush_points + rec_points)
pit_2006 <- nfl %>% 
  filter(game_year == 2006 & team == "PIT") %>% 
  select(team, game_year, rush_avg, rush_tds, rec_avg, rec_tds, position) %>% 
  group_by(position, game_year, team) %>% 
  summarize(rush_points = sum(rush_tds, na.rm = TRUE) * 6,
            rec_points = sum(rec_tds, na.rm = TRUE) * 6,
            total_points = rush_points + rec_points)
ind_2007 <- nfl %>% 
  filter(game_year == 2007 & team == "IND") %>% 
  select(team, game_year, rush_avg, rush_tds, rec_avg, rec_tds, position) %>% 
  group_by(position, game_year, team) %>% 
  summarize(rush_points = sum(rush_tds, na.rm = TRUE) * 6,
            rec_points = sum(rec_tds, na.rm = TRUE) * 6,
            total_points = rush_points + rec_points)
nyg_2008 <- nfl %>% 
  filter(game_year == 2008 & team == "NYG") %>% 
  select(team, game_year, rush_avg, rush_tds, rec_avg, rec_tds, position) %>% 
  group_by(position, game_year, team) %>% 
  summarize(rush_points = sum(rush_tds, na.rm = TRUE) * 6,
            rec_points = sum(rec_tds, na.rm = TRUE) * 6,
            total_points = rush_points + rec_points)
pit_2009 <- nfl %>% 
  filter(game_year == 2009 & team == "PIT") %>% 
  select(team, game_year, rush_avg, rush_tds, rec_avg, rec_tds, position) %>% 
  group_by(position, game_year, team) %>% 
  summarize(rush_points = sum(rush_tds, na.rm = TRUE) * 6,
            rec_points = sum(rec_tds, na.rm = TRUE) * 6,
            total_points = rush_points + rec_points)
no_2010 <- nfl %>% 
  filter(game_year == 2010 & team == "NO") %>% 
  select(team, game_year, rush_avg, rush_tds, rec_avg, rec_tds, position) %>% 
  group_by(position, game_year, team) %>% 
  summarize(rush_points = sum(rush_tds, na.rm = TRUE) * 6,
            rec_points = sum(rec_tds, na.rm = TRUE) * 6,
            total_points = rush_points + rec_points)
gb_2011 <- nfl %>% 
  filter(game_year == 2011 & team == "GB") %>% 
  select(team, game_year, rush_avg, rush_tds, rec_avg, rec_tds, position) %>% 
  group_by(position, game_year, team) %>% 
  summarize(rush_points = sum(rush_tds, na.rm = TRUE) * 6,
            rec_points = sum(rec_tds, na.rm = TRUE) * 6,
            total_points = rush_points + rec_points)
nyg_2012 <- nfl %>% 
  filter(game_year == 2012 & team == "NYG") %>% 
  select(team, game_year, rush_avg, rush_tds, rec_avg, rec_tds, position) %>% 
  group_by(position, game_year, team) %>% 
  summarize(rush_points = sum(rush_tds, na.rm = TRUE) * 6,
            rec_points = sum(rec_tds, na.rm = TRUE) * 6,
            total_points = rush_points + rec_points)
bal_2013 <- nfl %>% 
  filter(game_year == 2013 & team == "BAL") %>% 
  select(team, game_year, rush_avg, rush_tds, rec_avg, rec_tds, position) %>% 
  group_by(position, game_year, team) %>% 
  summarize(rush_points = sum(rush_tds, na.rm = TRUE) * 6,
            rec_points = sum(rec_tds, na.rm = TRUE) * 6,
            total_points = rush_points + rec_points)
sea_2014 <- nfl %>% 
  filter(game_year == 2014 & team == "SEA") %>% 
  select(team, game_year, rush_avg, rush_tds, rec_avg, rec_tds, position) %>% 
  group_by(position, game_year, team) %>% 
  summarize(rush_points = sum(rush_tds, na.rm = TRUE) * 6,
            rec_points = sum(rec_tds, na.rm = TRUE) * 6,
            total_points = rush_points + rec_points)
ne_2015 <- nfl %>% 
  filter(game_year == 2015 & team == "NE") %>% 
  select(team, game_year, rush_avg, rush_tds, rec_avg, rec_tds, position) %>% 
  group_by(position, game_year, team) %>% 
  summarize(rush_points = sum(rush_tds, na.rm = TRUE) * 6,
            rec_points = sum(rec_tds, na.rm = TRUE) * 6,
            total_points = rush_points + rec_points)
den_2016 <- nfl %>% 
  filter(game_year == 2016 & team == "DEN") %>% 
  select(team, game_year, rush_avg, rush_tds, rec_avg, rec_tds, position) %>% 
  group_by(position, game_year, team) %>% 
  summarize(rush_points = sum(rush_tds, na.rm = TRUE) * 6,
            rec_points = sum(rec_tds, na.rm = TRUE) * 6,
            total_points = rush_points + rec_points)
ne_2017 <- nfl %>% 
  filter(game_year == 2017 & team == "NE") %>% 
  select(team, game_year, rush_avg, rush_tds, rec_avg, rec_tds, position) %>% 
  group_by(position, game_year, team) %>% 
  summarize(rush_points = sum(rush_tds, na.rm = TRUE) * 6,
            rec_points = sum(rec_tds, na.rm = TRUE) * 6,
            total_points = rush_points + rec_points)

superbowl_all <- rbind(bal_2001, ne_2002, tb_2003, ne_2004, ne_2005,
          pit_2006, ind_2007, nyg_2008, pit_2009, no_2010,
          gb_2011, nyg_2012, bal_2013, sea_2014, ne_2015,
          den_2016, ne_2017)

#background field for plot
field <- readJPEG("~/Desktop/blogdown/monhait/n/static/img/field.jpg")
color_pal <- c("black", "darkorange", "red")
team_label <- superbowl_all %>% 
  filter(total_points >= 115) 


superbowl_plot <- ggplot(data = superbowl_all, aes(x = game_year, y = total_points, color = position)) +
  annotation_custom(rasterGrob(field, width=unit(1,"npc"), height=unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf) +
  geom_point(size = 3) +
  geom_line(size = 2) +
  scale_color_manual(values = color_pal) +
  theme_tufte() +
  labs(x = "Superbowl Year", y = "Total Points Scored (all season)") +
  ggtitle("Total Season Points Scored by Superbowl Champion Teams",
          subtitle = " Compared to Season Average by Position 2000 - 2017") +
  theme_tufte(base_family = "Arial-Black") +
  theme(plot.title = element_text(size = 20),
        axis.text = element_text(size = 15),
        plot.subtitle = element_text(size = 15)) +
  scale_x_continuous(breaks = c(2000, 2005, 2010, 2015)) +
  geom_label(aes(label = ifelse(position != "WR/TE", NA, team),
                       vjust = -0.4))

  
#find total_points by position averages per year for non-superbowl champs
non_2000 <- nfl %>% 
  filter(game_year == 2000 & team != "STL") %>% 
  select(team, game_year, rush_avg, rush_tds, rec_avg, rec_tds, position) %>% 
  group_by(position, game_year) %>% 
  summarize(rush_points = (sum(rush_tds, na.rm = TRUE) * 6) /31,
            rec_points = (sum(rec_tds, na.rm = TRUE) * 6) / 31,
            total_points = rush_points + rec_points)
non_2001 <- nfl %>% 
  filter(game_year == 2001 & team != "BAL") %>% 
  select(team, game_year, rush_avg, rush_tds, rec_avg, rec_tds, position) %>% 
  group_by(position, game_year) %>% 
  summarize(rush_points = (sum(rush_tds, na.rm = TRUE) * 6) /31,
            rec_points = (sum(rec_tds, na.rm = TRUE) * 6) / 31,
            total_points = rush_points + rec_points)
non_2002 <- nfl %>% 
  filter(game_year == 2002 & team != "NE") %>% 
  select(team, game_year, rush_avg, rush_tds, rec_avg, rec_tds, position) %>% 
  group_by(position, game_year) %>% 
  summarize(rush_points = (sum(rush_tds, na.rm = TRUE) * 6) /31,
            rec_points = (sum(rec_tds, na.rm = TRUE) * 6) / 31,
            total_points = rush_points + rec_points)
non_2003 <- nfl %>% 
  filter(game_year == 2003 & team != "TB") %>% 
  select(team, game_year, rush_avg, rush_tds, rec_avg, rec_tds, position) %>% 
  group_by(position, game_year) %>% 
  summarize(rush_points = (sum(rush_tds, na.rm = TRUE) * 6) /31,
            rec_points = (sum(rec_tds, na.rm = TRUE) * 6) / 31,
            total_points = rush_points + rec_points)
non_2004 <- nfl %>% 
  filter(game_year == 2004 & team != "NE") %>% 
  select(team, game_year, rush_avg, rush_tds, rec_avg, rec_tds, position) %>% 
  group_by(position, game_year) %>% 
  summarize(rush_points = (sum(rush_tds, na.rm = TRUE) * 6) /31,
            rec_points = (sum(rec_tds, na.rm = TRUE) * 6) / 31,
            total_points = rush_points + rec_points)
non_2005 <- nfl %>% 
  filter(game_year == 2005 & team != "NE") %>% 
  select(team, game_year, rush_avg, rush_tds, rec_avg, rec_tds, position) %>% 
  group_by(position, game_year) %>% 
  summarize(rush_points = (sum(rush_tds, na.rm = TRUE) * 6) /31,
            rec_points = (sum(rec_tds, na.rm = TRUE) * 6) / 31,
            total_points = rush_points + rec_points)
non_2006 <- nfl %>% 
  filter(game_year == 2006 & team != "PIT") %>% 
  select(team, game_year, rush_avg, rush_tds, rec_avg, rec_tds, position) %>% 
  group_by(position, game_year) %>% 
  summarize(rush_points = (sum(rush_tds, na.rm = TRUE) * 6) /31,
            rec_points = (sum(rec_tds, na.rm = TRUE) * 6) / 31,
            total_points = rush_points + rec_points)
non_2007 <- nfl %>% 
  filter(game_year == 2007 & team != "IND") %>% 
  select(team, game_year, rush_avg, rush_tds, rec_avg, rec_tds, position) %>% 
  group_by(position, game_year) %>% 
  summarize(rush_points = (sum(rush_tds, na.rm = TRUE) * 6) /31,
            rec_points = (sum(rec_tds, na.rm = TRUE) * 6) / 31,
            total_points = rush_points + rec_points)
non_2008 <- nfl %>% 
  filter(game_year == 2008 & team != "NYG") %>% 
  select(team, game_year, rush_avg, rush_tds, rec_avg, rec_tds, position) %>% 
  group_by(position, game_year) %>% 
  summarize(rush_points = (sum(rush_tds, na.rm = TRUE) * 6) /31,
            rec_points = (sum(rec_tds, na.rm = TRUE) * 6) / 31,
            total_points = rush_points + rec_points)
non_2009 <- nfl %>% 
  filter(game_year == 2009 & team != "PIT") %>% 
  select(team, game_year, rush_avg, rush_tds, rec_avg, rec_tds, position) %>% 
  group_by(position, game_year) %>% 
  summarize(rush_points = (sum(rush_tds, na.rm = TRUE) * 6) /31,
            rec_points = (sum(rec_tds, na.rm = TRUE) * 6) / 31,
            total_points = rush_points + rec_points)
non_2010 <- nfl %>% 
  filter(game_year == 2010 & team != "NO") %>% 
  select(team, game_year, rush_avg, rush_tds, rec_avg, rec_tds, position) %>% 
  group_by(position, game_year) %>% 
  summarize(rush_points = (sum(rush_tds, na.rm = TRUE) * 6) /31,
            rec_points = (sum(rec_tds, na.rm = TRUE) * 6) / 31,
            total_points = rush_points + rec_points)
non_2011 <- nfl %>% 
  filter(game_year == 2011 & team != "GB") %>% 
  select(team, game_year, rush_avg, rush_tds, rec_avg, rec_tds, position) %>% 
  group_by(position, game_year) %>% 
  summarize(rush_points = (sum(rush_tds, na.rm = TRUE) * 6) /31,
            rec_points = (sum(rec_tds, na.rm = TRUE) * 6) / 31,
            total_points = rush_points + rec_points)
non_2012 <- nfl %>% 
  filter(game_year == 2012 & team != "NYG") %>% 
  select(team, game_year, rush_avg, rush_tds, rec_avg, rec_tds, position) %>% 
  group_by(position, game_year) %>% 
  summarize(rush_points = (sum(rush_tds, na.rm = TRUE) * 6) /31,
            rec_points = (sum(rec_tds, na.rm = TRUE) * 6) / 31,
            total_points = rush_points + rec_points)
non_2013 <- nfl %>% 
  filter(game_year == 2013 & team != "BAL") %>% 
  select(team, game_year, rush_avg, rush_tds, rec_avg, rec_tds, position) %>% 
  group_by(position, game_year) %>% 
  summarize(rush_points = (sum(rush_tds, na.rm = TRUE) * 6) /31,
            rec_points = (sum(rec_tds, na.rm = TRUE) * 6) / 31,
            total_points = rush_points + rec_points)
non_2014 <- nfl %>% 
  filter(game_year == 2014 & team != "SEA") %>% 
  select(team, game_year, rush_avg, rush_tds, rec_avg, rec_tds, position) %>% 
  group_by(position, game_year) %>% 
  summarize(rush_points = (sum(rush_tds, na.rm = TRUE) * 6) /31,
            rec_points = (sum(rec_tds, na.rm = TRUE) * 6) / 31,
            total_points = rush_points + rec_points)
non_2015 <- nfl %>% 
  filter(game_year == 2015 & team != "NE") %>% 
  select(team, game_year, rush_avg, rush_tds, rec_avg, rec_tds, position) %>% 
  group_by(position, game_year) %>% 
  summarize(rush_points = (sum(rush_tds, na.rm = TRUE) * 6) /31,
            rec_points = (sum(rec_tds, na.rm = TRUE) * 6) / 31,
            total_points = rush_points + rec_points)
non_2016 <- nfl %>% 
  filter(game_year == 2016 & team != "DEN") %>% 
  select(team, game_year, rush_avg, rush_tds, rec_avg, rec_tds, position) %>% 
  group_by(position, game_year) %>% 
  summarize(rush_points = (sum(rush_tds, na.rm = TRUE) * 6) /31,
            rec_points = (sum(rec_tds, na.rm = TRUE) * 6) / 31,
            total_points = rush_points + rec_points)
non_2017 <- nfl %>% 
  filter(game_year == 2017 & team != "NE") %>% 
  select(team, game_year, rush_avg, rush_tds, rec_avg, rec_tds, position) %>% 
  group_by(position, game_year) %>% 
  summarize(rush_points = (sum(rush_tds, na.rm = TRUE) * 6) /31,
            rec_points = (sum(rec_tds, na.rm = TRUE) * 6) / 31,
            total_points = rush_points + rec_points)

non_all <- rbind(non_2001, non_2002, non_2003, non_2004, non_2005,
                       non_2006, non_2007, non_2008, non_2009, non_2010,
                       non_2011, non_2012, non_2013, non_2014, non_2015,
                       non_2016, non_2017)

#add average lines to superbowl_plot

nfl_plot <- superbowl_plot +
  geom_point(data = non_all, aes(x = game_year, y = total_points),
             size = 2, alpha = 0.3) +
  geom_line(data = non_all, aes(x = game_year, y = total_points),
            size = 2, alpha = 0.3) +
  scale_color_manual(values = color_pal)

nfl_plot
