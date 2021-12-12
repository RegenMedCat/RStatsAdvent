#Loading packages
library(tidyverse)
library(showtext)


#Adding Google fonts
showtext_auto()
font_add_google("Cookie", "Cookie")


#Importing data
#Downloaded from https://data.world/makeovermonday/2017-w-52-christmas-trees-sold-in-the-us


#Plotting bar plot of tree sales by year
p <- dat %>%
  mutate(Number.of.trees.sold = Number.of.trees.sold/10^6) %>%  #Converting to millions
  ggplot(aes(x = Year, y = Number.of.trees.sold, fill = Type.of.tree)) + 
  geom_bar(position = "stack", stat = "identity") +   #Making bars stacked
  geom_text(aes(label = Number.of.trees.sold), vjust = -1, family = "Cookie", colour = "white", size = 15) +  #Adding labels to bars
  theme_classic(base_size = 20) +  #Increasing base size, removing grid lines
  theme(text = element_text(family = "Cookie", size = 80, colour = "white"),  #Using custom Google fonts
        axis.text = element_text(colour = "white"),
        plot.title = element_text(hjust = 0.5,  size = 100),
        axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.6),   #Rotating x-axis labels
        axis.line = element_line(colour = "white"), #Changing axis line and tick colours
        axis.ticks = element_line(colour = "white"),
        panel.background = element_rect(fill = '#131413'),  #Changing fill colour to dark grey
        plot.background = element_rect(fill = '#131413'),
        legend.position = "top",  #Moving legend to top
        legend.title = element_blank(),
        legend.key = element_rect(fill = "#131413", colour = "#131413"), #Changing legend fill
        legend.background = element_rect(fill = "#131413")) +
  scale_fill_manual(values = c("#62a862", "#244a24")) +  #Adding custom fill colours
  scale_y_continuous(limits = c(0 , 50), breaks = seq(0, 50, 5), expand = c(0, 0)) +  #Customising axes
  scale_x_continuous(breaks = seq(2004, 2016, 1)) +
  xlab("Year") +   #Customising axis labels
  ylab("Trees Sold (Millions)") +
  labs(title = "US Christmas Tree Sales") +   #Adding plot title
  labs(caption = "Data source: data.world")   #Adding caption
  
  p

  
#Saving plot
ggsave("Trees.png", height = 8, width = 10, units = "in", dpi = 300)
