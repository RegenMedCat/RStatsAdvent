#Loading packages
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(showtext)


#Adding Google fonts
showtext_auto()
font_add_google("Snowburst One", "Snow")

#Followed https://r-spatial.org/r/2018/10/25/ggplot2-sf.html
world <- ne_countries(scale = "medium", returnclass = "sf")


#Plotting world map
ggplot(data = world) +
  geom_sf(colour = "#12481a", fill = "#1e792c", lwd = 0) +
  theme_void() +
  theme(plot.title = element_text(family = "Snow", hjust = 0.5, size = 100),  #Customising fonts
        panel.background = element_rect(fill = '#d2e4d4', colour = NA),  #Changing fill colour to light green, removing border
        plot.background = element_rect(fill = '#d2e4d4', colour = NA)) +
  geom_curve(aes(x = 0, y = 90, xend = -3.7839, yend = 56.0019), curvature = -0.2, linetype = 2,  #Adding arrow between North Pole and Central Scotland
             arrow = arrow(length = unit(0.3,"cm"))) +
  geom_point(aes(x = -0.48, y = 56.5), shape = 15, colour = "#ff4538", size = 10, stroke = 5) +  #Adding present
  geom_point(aes(x = -0.48, y = 56.5), shape = 3, colour = "#d2e4d4", size = 7, stroke = 3) +
  geom_label(aes(x = 1, y = 64), label = "3780 km from North Pole", family = "Snow", size = 18) +  #Adding label
  coord_sf(xlim = c(-15, 40), ylim = c(35, 70)) +
  labs(title = "Santa Stop Here")  #Adding title


#Saving plot
ggsave("Map.png", height = 10, width = 10, units = "in", dpi = 300)
