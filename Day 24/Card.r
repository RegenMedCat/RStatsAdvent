#Loading packages
library(tidyverse)
library(showtext)


#Adding Google fonts
showtext_auto()
font_add_google("Bigelow Rules", "Bigelow")
font_add_google("Oswald", "Oswald")


#Building a dataframe
df <- data.frame(
  letter = c("C", "h", "r", "i", "s", "t", "m", "a", "s"),
  x = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
  y = c(1, 1.2, 1, 1.2, 1, 1.2, 1, 1.2, 1))


#Checking data
glimpse(df)


#Plotting upside down lollipop chart (to look like hanging baubles)
p <- df %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(aes(x = x, y = y - 0.15), size = 3, colour = "#ffa229", shape = 1, stroke = 1.5) +  #Adding rings to baubles
  geom_segment(aes(x = x, xend = x, y = 0, yend = y - 0.179), size = 0.5) +
  geom_point(aes(x = x, y = y - 0.12), size = 5, colour = "#ffa229", shape = 15, stroke = 1.5) +  #Adding squares to tops
  geom_point(size = 21, colour = "#ffa229", fill = alpha("#ffc629"), shape = 21, stroke = 1.5) +  #Adding points, customising appearance
  geom_text(aes(x = x, y = y - 0.01, label = letter), size = 52, family = "Bigelow") +  #Adding text to lollipops
  scale_y_reverse(limits = c(1.5, 0)) +   #Flipping y axis so looks like hanging baubles
  theme_void() +  #Removing grid and axes
  theme(axis.text.x = element_blank(),  #Removing x-axis labels
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        plot.title = element_text(family = "Bigelow", hjust = 0.5,  size = 170),
        plot.caption = element_text(family = "Bigelow", size = 170,  face = "italic", hjust = 0.5)) +
  ggtitle("Merry") +  #Adding title
  labs(caption = "and best wishes for 2022!") +   #Adding caption
  annotate(geom = "text", x = 8.87, y = 0.23, label = "@RegenMedCat", angle = 90, size = 15, family = "Oswald")

p

#Saving plot
ggsave("Card.png", height = 6, width = 10, units = "in", dpi = 300)
