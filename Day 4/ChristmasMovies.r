#Loading packages
library(tidyverse)
library(showtext)


#Adding Google fonts
showtext_auto()
font_add_google("Snowburst One", "Snow")
font_add_google("Bigelow Rules", "Bigelow")


#Importing data
dat <- read.csv("https://raw.githubusercontent.com/RegenMedCat/RStatsAdvent/main/Day%204/Movies.csv")


#Checking data
glimpse(dat)


#Plotting upside down lollipop chart (to look like hanging baubles)
p <- dat %>%
  mutate(Gross = (Gross/1000000), Gross = round(Gross, 1),   #Converting to millions, rounding to 1 decimal place
         Title = replace(Title, Title == "The Nutcracker and the Four Realms", "The Nutcracker (2018)")) %>%  #Title too long so renaming
  ggplot(aes(x = Title, y = Gross)) +
  geom_segment(aes(x = Title, xend = Title, y=0, yend=Gross), size = 0.5) +
  geom_point(size = 20, colour = "#ffa229", fill = alpha("#ffc629"), shape = 21, stroke = 1.5) +  #Adding points, customising appearance
  geom_text(aes(label = Gross), size = 35, family = "Bigelow") +  #Adding text to lollipops
  geom_text(aes(label = Title), size = 40, family = "Bigelow", #Adding titles
            nudge_x = -0.3, nudge_y = 130, angle = 90) +   #Customising position
  scale_y_reverse() +   #Flipping y axis so looks like hanging baubles
  theme_void() +  #Removing grid and axes
  theme(axis.text.x = element_blank(),  #Removing x-axis labels
        plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.title = element_text(family = "Bigelow", hjust = 0.5,  size = 150),
        plot.caption = element_text(family = "Bigelow", size=100,  face = "italic")) +
  ggtitle("Top 10 Highest Grossing Christmas Movies ($ millions)") +  #Adding title
  labs(caption = "Data source: Wikipedia")   #Adding caption

p

#Saving plot
ggsave("Movies.png", height = 15, width = 10, units = "in", dpi = 300)
