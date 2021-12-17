#Loading packages
library(tidyverse)
library(showtext)
library(ggpubr)


#Adding Google fonts
showtext_auto()
font_add_google("Snowburst One", "Snow")


#Importing data
#Data source https://www.kaggle.com/lydia70/chicago-weather-dreaming-of-a-white-christmas?select=ChicagoWeatherChristmas.csv


#Plotting ribbon plot of snowfall and min temp
p <- dat %>%
  select(Year, Celcius.Min.Temp, Celcius.High.Temp) %>%
  filter(!is.na(Celcius.Min.Temp), !is.na(Celcius.High.Temp), 
         Year > 1919) %>%  #Removing NAs, filtering to 1920 onwards
  ggplot(aes(x = Year)) +
  geom_ribbon(aes(ymin = Celcius.Min.Temp, ymax = Celcius.High.Temp), fill = "#5a6e9f", alpha = 0.5) +
  geom_line(aes(y = (Celcius.Min.Temp + Celcius.High.Temp)/2), colour = "#5c5b5c") +
  theme_classic((base_size = 30)) +  #Removing grid and grey panel, increasing base size
  theme(plot.margin = margin(1, 2, 0.5, 1, "cm"),  #Customising margins
        text = element_text(family = "Snow", size = 80, colour = "white"),  #Customising fonts
        plot.title = element_text(hjust=0.5,  size = 92),
        axis.text = element_text(colour = "white"),
        plot.caption = element_text(size = 50),
        axis.line = element_line(colour = "#777a87"), #Changing axis line and tick colours
        axis.ticks = element_line(colour = "#777a87"),
        panel.background = element_rect(fill = '#181c2f'),  #Changing fill colour to navy
        plot.background = element_rect(fill = '#181c2f')) +
  scale_x_continuous(limits = c(1920, 2020), breaks = seq(1920, 2020, 20), expand = c(0, 0)) +  #Customising axes
  scale_y_continuous(limits = c(-30, 20), breaks = seq(-30, 20, 5)) +
  xlab("") +  #Adding axis labels and caption
  ylab("Temperature Range (°C)") +
  labs(title = "Chicago Christmas Weather",
       caption = "Data source: Kaggle")

p


#Saving plot
ggsave("Weather.png", height = 8, width = 10, units = "in", dpi = 300)


#Making polar chart art
p2 <- dat %>%
  select(Year, Celcius.Min.Temp, Celcius.High.Temp) %>%
  filter(!is.na(Celcius.Min.Temp), !is.na(Celcius.High.Temp)) %>%  #Removing NAs, filtering to 1920 onwards
  ggplot(aes(x = Year)) +
  geom_ribbon(aes(ymin = Celcius.Min.Temp, ymax = Celcius.High.Temp), fill = "#ababb4", alpha = 0.8) +
  theme_void((base_size = 30)) +  #Removing grid and grey panel, increasing base size
  theme(plot.margin = margin(-1, -1, -1, -1, "cm"),  #Customising margins
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill = '#181c2f', colour = NA),  #Changing fill colour to navy, removing borders
        plot.background = element_rect(fill = '#181c2f', colour = NA)) +
  scale_x_continuous(limits = c(1920, 2020), breaks = seq(1920, 2020, 20), expand = c(0, 0)) +  #Customising axes
  scale_y_continuous(limits = c(-30, 20), breaks = seq(-30, 20, 5)) +
  coord_polar()

p2


#Snowflake 2
p3 <- dat %>%
  select(Year, Celcius.Min.Temp) %>%
  filter(!is.na(Celcius.Min.Temp)) %>%  #Removing NAs
  mutate(Celcius.Min.Temp = Celcius.Min.Temp - 8) %>%
  ggplot(aes(x = Year, y = Celcius.Min.Temp)) +
  geom_segment(aes(x = Year, xend = Year, y = 0, yend = Celcius.Min.Temp), size = 0.5, colour = "white") +
  geom_point(size = 2.5, colour = "white", fill = alpha("white"), shape = 16) +  #Adding points, customising appearance
  theme_void((base_size = 30)) +  #Removing grid and grey panel, increasing base size
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),  #Customising margins
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill = '#181c2f', colour = NA),  #Changing fill colour to navy, removing borders
        plot.background = element_rect(fill = '#181c2f', colour = NA)) +
  coord_polar()

p3


#Snowflake 3
p4 <- dat %>%
  select(Year, Celcius.Min.Temp) %>%
  filter(!is.na(Celcius.Min.Temp)) %>%  #Removing NAs
  mutate(Celcius.Min.Temp = Celcius.Min.Temp + 30) %>%
  ggplot(aes(x = Year, y = Celcius.Min.Temp)) +
  geom_segment(aes(x = Year, xend = Year, y = 0, yend = Celcius.Min.Temp), size = 0.5, colour = "white") +
  geom_point(size = 2.5, colour = "white", fill = alpha("white"), shape = 8) +  #Adding points, customising appearance
  theme_void((base_size = 30)) +  #Removing grid and grey panel, increasing base size
  theme(plot.margin = margin(-1, -1, -1, -1, "cm"),  #Customising margins
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill = '#181c2f', colour = NA),  #Changing fill colour to navy, removing borders
        plot.background = element_rect(fill = '#181c2f', colour = NA)) +
  coord_polar()

p4


#Snowflake 4
p5 <- dat %>%
  select(Year, Celcius.Min.Temp, Celcius.High.Temp) %>%
  filter(!is.na(Celcius.Min.Temp), !is.na(Celcius.High.Temp)) %>%  #Removing NAs
  ggplot(aes(x = Year)) +
  geom_line(aes(y = Celcius.Min.Temp), colour = "#ababb4", size = 1) +
  geom_line(aes(y = Celcius.High.Temp), colour = "#747682", size = 1.2) +
  theme_void((base_size = 30)) +  #Removing grid and grey panel, increasing base size
  theme(plot.margin = margin(-1, -1, -1, -1, "cm"),  #Customising margins
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill = '#181c2f', colour = NA),  #Changing fill colour to navy, removing borders
        plot.background = element_rect(fill = '#181c2f', colour = NA)) +
  coord_polar()

p5

#Arranging snowflakes
ggarrange(p2, p3, p4, p5, ncol = 2, nrow = 2)


#Saving plot
ggsave("Snowflakes.png", height = 8, width = 8, units = "in", dpi = 300)
