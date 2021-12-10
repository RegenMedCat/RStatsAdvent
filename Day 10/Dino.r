#Loading packages
library(tidyverse)
library(tidytuesdayR)
library(showtext)


#Adding Google fonts
showtext_auto()
font_add_google("BenchNine", "Bench")


#Getting data
tuesdata <- tidytuesdayR::tt_load(2020, week = 42)

datasaurus <- tuesdata$datasaurus


#Checking data
glimpse(datasaurus)


#Dino scatter plot
p <- datasaurus %>%
  filter(dataset == "dino") %>%  #Filtering to dino
  ggplot(aes(x = x, y = y)) +
  geom_point(size = 5, colour = "#0b5e08") +  #Changing dino points to green
  geom_point(aes(x = 4, y = 65), colour = "white", shape = 8, size = 25) +  #Adding snowflakes
  geom_point(aes(x = -8, y = 8), colour = "white", shape = 8, size = 25) +
  geom_point(aes(x = -15, y = 105), colour = "white", shape = 8, size = 25) +
  geom_point(aes(x = 95, y = 95), colour = "white", shape = 8, size = 25) +
  geom_point(aes(x = 55, y = 99), colour = "red", shape = 17, size = 30) +   #Adding santa hat
  geom_point(aes(x = 55, y = 110), colour = "white", shape = 19, size = 10) +
  geom_point(aes(x = 63, y = 93), colour = "white", shape = 19, size = 13) +
  geom_point(aes(x = 60, y = 93), colour = "white", shape = 19, size = 13) +
  geom_point(aes(x = 57, y = 93), colour = "white", shape = 19, size = 13) +
  geom_point(aes(x = 54, y = 93), colour = "white", shape = 19, size = 13) +
  geom_point(aes(x = 51, y = 93), colour = "white", shape = 19, size = 13) +
  geom_point(aes(x = 48, y = 93), colour = "white", shape = 19, size = 13) +
  theme_void() + #Removing grid and axes
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),  #Adjusting plot margins
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#1f2124"), #Changing background colour
        plot.caption = element_text(family = "Bench", size = 64,  face = "italic", color="white", hjust = 0)) +  #Customising caption font
  scale_x_reverse(limits = c(100 , -20)) +  #Flipping dino to face the other way
  scale_y_continuous(limits = c(0 , 110)) +
  ylab("") +    #Removing axis labels
  xlab("") +
  labs(caption = "Data source: Datasaurus Dozen") + #Adding caption
  annotate(geom = "text", x = 4, y = 43, label="ROARING",  #Adding text
           color = "red", family = "Bench", size = 72) +
  annotate(geom = "text", x = 4, y = 31, label="XMAS!",
           color = "red", family = "Bench", size = 72)

p


#Saving plot
ggsave("Dino.png", height = 8, width = 10, units = "in", dpi = 300)
