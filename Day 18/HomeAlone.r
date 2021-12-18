#Loading packages
library(tidyverse)
library(showtext)

#Adding Google fonts
showtext_auto()
font_add_google("Merriweather", "Merriweather")


#Building data frame
Movie <- c("Home Alone", "Home Alone 2", "Home Alone 3", "Home Alone 5")
Gross <- c(476.7, 359, 79.1, 0.895)

dat <- data.frame(Movie, Gross)

print (dat)


#Plotting scatter with houses as points
p <- dat %>%
  ggplot(aes(x = Movie, y = Gross)) + 
  geom_point(size = 28, shape = 15, colour = "#f51d2c") +   #Customising points
  geom_point(aes(x = Movie, y = Gross+58), size = 26, shape = 17, colour = "#f51d2c") +  #Adding triangles as roof
  geom_point(aes(x = Movie, y = Gross+98), size = 24, shape = 15, colour = "#183e6d") +  #Covering up points of triangles
  theme_classic(base_size = 40) +  #Increasing base size, removing grid lines
  theme(plot.margin = margin(1, 1, 1, 1, "cm"),  #Customising margins
        text = element_text(family = "Merriweather", size = 80, colour = "#f0dd13", face = "bold"),  #Using custom Google fonts
        axis.text = element_text(colour = "#f0dd13"),
        plot.title = element_text(hjust = 0.5,  size = 120),
        plot.subtitle = element_text(hjust = 0.5),
        axis.line = element_line(colour = "#f0dd13"), #Changing axis line and tick colours
        axis.ticks = element_line(colour = "#f0dd13"),
        panel.background = element_rect(fill = '#183e6d'),  #Changing fill colour to blue
        plot.background = element_rect(fill = '#183e6d'),
        plot.tag.position = c(0.722, 0.975),  #Customising position and angle of e on end of Home Alone
        plot.tag = element_text(angle = 30, size = 120)) +
  scale_y_continuous(limits = c(0, 580), breaks = seq(0, 580, 100)) +  #Customising y-axis to avoid houses getting cut off
  xlab("") +   #Customising axis labels
  ylab("Gross Earnings ($ millions)") +
  labs(title = "HOME ALON") +   #Adding plot title
  labs(tag = "e") +  #Adding e as tag
  labs(subtitle = "Lost in Remakes") +
  labs(caption = "@RegenMedCat")   #Adding caption

p

#Saving plot
ggsave("HomeAlone.png", height = 10, width = 12, units = "in", dpi = 300)
