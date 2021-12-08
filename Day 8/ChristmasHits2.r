#Loading packages
library(tidyverse)
library(tidytuesdayR)
library(showtext)


#Adding Google fonts
showtext_auto()
font_add_google("Snowburst One", "Snow")
font_add_google("Roboto Slab", "Roboto")



#Loading data
tuesdata <- tidytuesdayR::tt_load(2019, week = 52)

christmas_songs <- tuesdata$christmas_songs


#Plotting scatter of peak chart position against weeks on chart
p <- christmas_songs %>%
  select(year, peak_position, weeks_on_chart) %>%
  filter(year > 1979) %>% #Filtering to 80s onwards
  mutate(era = case_when(year > 1979 & year < 1990 ~ "80s",
                         year > 1989 & year < 2000 ~ "90s",
                         year > 1999 & year < 2010 ~ "2000s",
                         year > 2009 & year < 2020 ~ "2010s")) %>%  #Adding categories for custom colour scheme
  ggplot(aes(x = weeks_on_chart, y = peak_position, shape = era)) +
  geom_point(stat = "identity", size = 4, colour = "white", alpha = 0.8) +
  theme_classic(base_size = 20) +  #Increasing base size, removing grid lines
  theme(plot.margin = margin(1, 1, 1, 1, "cm"),
        text = element_text(family = "Snow", size = 72, colour = "white"),  #Using custom Google fonts
        plot.title = element_text(hjust = 0.5,  size = 100),
        legend.text = element_text(size = 44, family = "Roboto"),
        axis.text = element_text(colour = "white", family = "Roboto"),
        axis.line = element_line(colour = "white"), #Changing axis line and tick colours
        axis.ticks = element_line(colour = "white"),
        panel.background = element_rect(fill = '#8f0101'),  #Changing fill colour to red
        plot.background = element_rect(fill = '#8f0101'),
        legend.position = "top",
        legend.title = element_blank(),
        legend.key = element_rect(fill = "#8f0101", colour = "#8f0101"), #Changing legend fill
        legend.background = element_rect(fill = "#8f0101")) +
  scale_y_reverse(limits = c(100, 1), breaks = c(1, 10, 20, 40, 30, 50, 60, 70, 80, 90, 100)) + #Reversing y-axis and customising breaks
  scale_x_continuous(limits = c(0,21), breaks = seq(0, 21, 2), expand = c(0, 0)) +  #Customising x-axis
  scale_shape_manual(values = c(19, 5, 8, 1),  #Customising point shapes
                     limits = c("80s", "90s", "2000s", "2010s")) +  #Reordering legend
  xlab("Weeks in the Charts") +   #Customising axis labels
  ylab("Chart Position") +
  labs(title = "Christmas Hits") +   #Adding plot title
  labs(caption = "Data source: Kaggle")   #Adding caption

p


#Saving plot
ggsave("Christmas Hits Scatter.png", height = 10, width = 10, units = "in", dpi = 300)
