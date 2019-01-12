library(readr)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(forcats)
library(viridis)
library(patchwork)

url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-05-29/week9_comic_characters.csv"
comics <- read_csv(url)

#genderless characters

comics <- comics %>% 
  select(sex, hair, eye, align) %>% 
  mutate(no_gender = sex %in% c("Agender Characters", "Genderfluid Characters",
                                "Genderless Characters", "Transgender Characters"),
         male = sex %in% c("Male Characters",
         female = sex %in% "Female Characters") %>% 
  filter(!is.na(align)) %>% 
  filter(!is.na(eye)) %>% 
  filter(!is.na(hair)) %>% 
  filter(!is.na(sex)) 
  
comics <- comics %>% 
  filter(align != "Reformed Criminals")

#for align plot
align_prop <- comics %>% 
  group_by(sex, align) %>% 
  count() 

align_f <- align_prop %>% 
  filter(sex == "Female Characters") %>% 
  mutate(n = n / 2039)

align_m <- align_prop %>% 
  filter(sex == "Male Characters") %>% 
  mutate(n = n / 4544)

align_non <- align_prop %>% 
  filter(sex == "Agender Characters") %>% 
  mutate(n = n / 19)

align_all <- rbind(align_f, align_m, align_non)


align_plot <- ggplot(data = align_all) +
  geom_col(aes(x = sex, y = n, fill = align),
           width = 0.5) +
  labs(x = NULL, y = NULL, fill = NULL) +
  scale_fill_viridis(discrete = TRUE) +
  theme_tufte(base_family = "Arial Black") +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.text = element_text(size = 10),
        aspect.ratio = 1/3,
        plot.subtitle = element_text(hjust = 0.5),
        plot.margin = unit(c(0,0,0,0), "cm")) +
  coord_flip() +
  ggtitle("Comic Book Character Differences by Gender Identity",
          subtitle = "Character Type") +
  scale_y_continuous(breaks = NULL)


#hair color 
eye_prop <- comics %>% 
  group_by(sex, eye) %>% 
  count() 

eye_f <- eye_prop %>% 
  filter(sex == "Female Characters") %>% 
  mutate(n = n / 2039)

eye_m <- eye_prop %>% 
  filter(sex == "Male Characters") %>% 
  mutate(n = n / 4544)

eye_non <- eye_prop %>% 
  filter(sex == "Agender Characters") %>% 
  mutate(n = n / 19)

eye_all <- rbind(eye_f, eye_m, eye_non)

eyes <- eye_all %>% 
  filter(eye %in% c("Black Eyes", "Blue Eyes", "Brown Eyes",
                     "Green Eyes", "Red Eyes", "White Eyes"))

eye_plot <- ggplot(data = eyes) +
  geom_col(aes(x = sex, y = n, fill = eye),
           width = 0.5) +
  labs(x = NULL, y = NULL, fill = NULL) +
  scale_fill_viridis(option = "viridis", discrete = TRUE) +
  theme_tufte(base_family = "Arial Black") +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.text = element_text(size = 10),
        aspect.ratio = 1/3,
        plot.subtitle = element_text(hjust = 0.5),
        plot.margin = unit(c(0,0,0,0), "cm")) +
  coord_flip() +
  ggtitle(NULL, subtitle = "Eye Color")

#eye color
hair_prop <- comics %>% 
  group_by(sex, hair) %>% 
  count() 

hair_f <- hair_prop %>% 
  filter(sex == "Female Characters") %>% 
  mutate(n = n / 2039)

hair_m <- hair_prop %>% 
  filter(sex == "Male Characters") %>% 
  mutate(n = n / 4544)

hair_non <- hair_prop %>% 
  filter(sex == "Agender Characters") %>% 
  mutate(n = n / 19)

hair_all <- rbind(hair_f, hair_m, hair_non)

test <- hair_all %>% 
  filter(hair %in% c("Black Hair", "Brown Hair", "Blond Hair",
                     "Green Hair", "No Hair"))

hair_plot <- ggplot(data = test) +
  geom_col(aes(x = sex, y = n, fill = hair),
           width = 0.5) +
  labs(x = NULL, y = NULL, fill = NULL) +
  scale_fill_viridis(option = "viridis", discrete = TRUE) +
  theme_tufte(base_family = "Arial Black") +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.text = element_text(size = 10),
        aspect.ratio = 1/3,
        plot.subtitle = element_text(hjust = 0.5),
        plot.margin = unit(c(0,0,0,0), "cm")) +
  coord_flip() +
  ggtitle(NULL, subtitle = "Hair Color") +
  scale_y_continuous(breaks = NULL) 

align_plot + hair_plot +
  eye_plot + 
  plot_layout(ncol = 1)
