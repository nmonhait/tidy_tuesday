library(readr)
library(tidyverse)
library(ggthemes)
library(viridis)
library(sf)
library(tigris)
library(gridExtra)
library(ggplot2)

url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-22/prison_population.csv"
incarceration_trends <- read_csv(url)

#Rate of incarceration in CO counties in 2015 top 10
CO_overall <- incarceration_trends %>% 
  filter(pop_category == "Total" & year == 2015 & state == "CO") %>% 
  mutate(prop_in_prison = prison_population / population * 100000) %>% 
  na.omit() %>% 
  select(-urbanicity, -region, -division, -state) %>% 
  filter(prop_in_prison > 600) 


CO_bar <- ggplot(data = CO_overall) +
  geom_col(aes(x = county_name, y = prop_in_prison), fill = "darkslategray") +
  scale_x_discrete(limits = c("Lincoln County", "Mesa County","Moffat County",
                              "Alamosa County",
                              "Pueblo County", "Denver County",
                              "Gilpin County","El Paso County",
                               "Fremont County", "Logan County")) +
  coord_flip() +
  labs(x = NULL, y = "Rate per 100,000") +
  theme_minimal(base_family = "Arial Black") +
  theme(plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 10),
        axis.text.x = element_text(size = 5)) 

#for map
co_counties <- counties(state = "CO", cb = TRUE, class = "sf")
counties_on_plot <- co_counties %>% 
  filter(NAME %in% c("Logan", "Fremont", "El Paso", "Gilpin",
                     "Denver", "Pueblo", "Alamosa", "Moffat",
                     "Mesa", "Lincoln"))

CO_plot <- ggplot() + 
  geom_sf(data = co_counties, fill = "lightgray") +
  geom_sf(data = counties_on_plot, fill = "darkslategray") +
  geom_sf_text(data = counties_on_plot, aes(label = NAME) , 
               color = "white", size = 2) +
  labs(x = NULL, y = NULL) +
  ggtitle("CO Counties with the Highest Rate of Incarcertaion",
          subtitle = "2015") +
  theme_map(base_family = "Arial Black") +
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5))

grid.arrange(CO_plot, CO_bar, heights = 2:1)
