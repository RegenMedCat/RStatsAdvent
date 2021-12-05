#Loading packages
library(tidyverse)
library(gganimate)


#Making animated shooting star following https://goodekat.github.io/presentations/2019-isugg-gganimate-spooky/slides.html#1
#Loading data
link <- "https://raw.githubusercontent.com/goodekat/presentations/master/2019-isugg-gganimate-spooky/bat-data/bats-subset.csv"

bats <- read.csv(link) %>% 
  mutate(id = factor(id))


#Making plot
a <- bats %>%
  filter(id == 3) %>%
  ggplot(aes(x = longitude, 
             y = latitude)) +
  geom_path(colour = "#ffcf5e", linetype = "dotted") +  #Adding for trail behind star
  geom_point(colour = "#ffb300", shape = 8, size = 6) +
  theme_void() + #Removing grid and axes
  theme(panel.background = element_blank(),
        plot.background = element_rect(fill = "#020014")) +
  scale_y_reverse() +  #Flipping y-axis
  transition_reveal(along = time) +  #Adding animation
  view_follow()

a

animate(a, fps=10, height=550, width=550, res=100)


#saving GIF
anim_save("star.gif")
