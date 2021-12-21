#Loading packages
library(tidyverse)
library(waffle)
library(showtext)


#Adding Google fonts
showtext_auto()
font_add_google("Bangers", "Bangers")


#Importing data
dat <- read.csv("https://raw.githubusercontent.com/RegenMedCat/RStatsAdvent/main/Day%2021/No1s.csv")


#Checking data
glimpse(dat)


#Counting number of solos, duets and group number 1s
my_sum <- dat %>%
  count(Type)

print(my_sum)


#Plotting waffle chart of number 1s
p <- waffle(c("Soloist" = 14, "Duet" = 7, "Group" = 19), rows = 4, colors = c("#14342B", "#FDE74C", "#DD6031")) +  #Removed 1980 to have 40
  theme(plot.margin = margin(-1, 0.5, -0.5, 0.5, "cm"),
        text = element_text(family = "Bangers"),   #Customising fonts
        legend.text = element_text(size = 80, margin = margin(r = 50, unit = "pt")),  #Customising legend margins and text size
        plot.title = element_text(hjust = 0.5, size = 100),
        plot.caption = element_text(hjust = 1, vjust = -50, size = 40), #Customising legend and caption positions
        legend.position  = c(0.56, -0.1),  
        legend.direction = "horizontal",
        legend.key.width = unit(0.75, 'cm')) +
  labs(title = "UK Christmas number ones 1981 - 2020") +  #Adding title and caption
  labs(caption = "@RegenMedCat")

p

#Saving plot
ggsave("No1s.png", height = 5.5, width = 10, units = "in", dpi = 300)
