#Loading packages
library(tidyverse)
library(showtext)
library(ggforce)


#Adding Google fonts
showtext_auto()
font_add_google("Cookie", "Cookie")


#Importing data
dat <- read.csv("https://raw.githubusercontent.com/RegenMedCat/RStatsAdvent/main/Day%209/12Days.csv")

#Plotting number of gifts against day
p <- dat %>%
  ggplot(aes(x = reorder(Gift, -Day), y = Quantity)) +  #Reordering by day number, descending
  geom_point(shape = 12, colour = "#e5849c", size = 8, fill = "white") +   #Changing points to look like presents
  geom_ellipse(aes(x0 = Day + 0.65, y0 = Quantity - 0.21,   #Adding ellipses for bows on top of each "present"
                   b = 0.2, a = 0.25, angle = -0.7), 
               colour = "#ed395d") +
  geom_ellipse(aes(x0 = Day + 0.65, y0 = Quantity + 0.21, 
                   b = 0.2, a = 0.25, angle = 0.7), 
               colour = "#ed395d") +
  geom_curve(mapping = aes(x = Day + 0.5, xend = Day,   #Adding curves for ribbon
                           y = Quantity, yend = Quantity  + 1), size = 0.6,
             curvature = 0.4, colour = "#ed395d") +
  geom_curve(mapping = aes(x = Day, xend = Day  + 0.5,
                           y = Quantity - 1, yend = Quantity), size = 0.6,
             curvature = 0.4, colour = "#ed395d") +
  theme_classic(base_size = 20) +
  theme(panel.background = element_blank(),
        plot.background = element_rect(fill = "#2b5c3b"), #Changing background colour
        text = element_text(family = "Cookie", size = 60, colour = "white"),  #Customising font
        plot.title = element_text(size= 80, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(face = "italic"),
        axis.text = element_text(colour = "white"),
        axis.line = element_line(colour = "#b4d4c1"), #Changing axis line and tick colours
        axis.ticks = element_line(colour = "#b4d4c1")) +
  scale_y_continuous(limits = c(10, 43), breaks = seq(10, 43, 2), expand = c(0, 0)) + #Customising axes
  scale_x_discrete(expand = c(0.1, 0)) +
  coord_flip() +  #Flipping axes
  xlab("") +    #Adding labels and titles
  ylab("Total Number of Gifts") +
  labs(title = "The 12 Days of Christmas",
       subtitle = "(expensive and wasteful)",
       caption = "@RegenMedCat")

p


#Saving plot
ggsave("12 Days.png", height = 6, width = 10, units = "in", dpi = 300)
