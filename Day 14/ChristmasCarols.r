#Loading packages
library(tidyverse)
library(showtext)
library(ggradar)


#Adding Google fonts
showtext_auto()
font_add_google("Libre Baskerville", "Libre")


#Importing data
dat <- read.csv("https://raw.githubusercontent.com/RegenMedCat/RStatsAdvent/main/Day%2014/ChristmasCarols.csv")


#Plotting radar chart
#Roughly followed https://www.r-graph-gallery.com/web-radar-chart-with-R.html
p <- dat %>%
  mutate(IMDb = IMDb/10,  #Converting all ratings to a percentage (decimal)
         Prime = Prime/5,
         Tomatometer = Tomatometer/100,
         Rotten.Tomatoes.Audience = Rotten.Tomatoes.Audience/100,
         Metacritic = Metacritic/100) %>%
  rename("Rotten Tomatoes (Audience)" = Rotten.Tomatoes.Audience) %>%
  ggradar(font.radar = "Libre", 
    grid.label.size = 20,
    axis.label.size = 12,
    group.point.size = 5,
    group.colours = c("#ff9430", "#f72e00", "#f7a900"),  #Customising colours
    background.circle.colour = "#fffdc4",
    gridline.mid.colour = "grey") +
  theme(plot.margin = margin(-5, -5, -5, -5),  #Adjusting margins
        text = element_text(family = "Libre", face = "bold"),   #Customising fonts
        plot.title = element_text(size = 70, hjust = 0.5),
        legend.text = element_text(size = 30),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = c(0.5, 0),  #Customising legend position
        legend.direction = "horizontal", 
        legend.title = element_blank()) +
  labs(title = "What's the best version of A Christmas Carol?")

p


#Saving plot
ggsave("ChristmasCarols.png", height = 8, width = 8, units = "in", dpi = 300)
