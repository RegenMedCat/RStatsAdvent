#Loading packages
library(tidyverse)
library(showtext)


#Adding Google fonts
showtext_auto()
font_add_google("Cookie", "Cookie")


#Importing data
dat <- read.csv("https://raw.githubusercontent.com/RegenMedCat/RStatsAdvent/main/Day%213/MorePies.csv")


#Creating new data frame to store angles for labels
label_data <- dat %>%
              group_by(Pie) %>%
              summarise(max = max(Rating)) %>%  #Did this so there's only one row per pie brand for plotting labels later
              mutate(Angle = case_when(Pie == "Sainsbury's" ~ "72",
                                       Pie == "Tesco" ~ "18",
                                       Pie == "Coop" ~ "-18",
                                       Pie == "Heston" ~ "-72",
                                       Pie == "Holly Lane (Aldi)" ~ "72",
                                       Pie == "Iceland" ~ "18",
                                       Pie == "M&S" ~ "-18",
                                       Pie == "Mr Kipling" ~ "-72"))


#Plotting circular bar plot
#Roughly followed https://www.r-graph-gallery.com/296-add-labels-to-circular-barplot.html
p <- ggplot(dat, aes(x = as.factor(Pie), y = Rating,  fill = Category)) +       
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(y = Rating-0.6, label = Rating),   #Adding labels to bars
            position = position_dodge(0.9),
            vjust = 0,
            colour = "white", family = "Cookie", size = 14) + 
  geom_text(data = label_data,   #Adding brand labels
            aes(x = Pie, y = max+0.7, label = Pie), 
            hjust = 0.5, angle = label_data$Angle, inherit.aes = FALSE,
            family = "Cookie", size = 32) +
  theme_void() +
  theme(plot.margin = margin(-120, -120, -120, -120),  #I know these are ridiculous but it worked out ok
        plot.title = element_text(family = "Cookie", size = 80, hjust = 0.5),  #Customising fonts
        plot.caption = element_text(family = "Cookie", size = 40),
        legend.text = element_text(family = "Cookie", size = 60),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = '#dbf2d8'),  #Changing fill colour to light green
        plot.background = element_rect(fill = '#dbf2d8'),
        legend.position = c(0.5, 0.82),  #Customising legend position
        legend.direction = "horizontal", 
        legend.title = element_blank(),
        legend.key = element_rect(fill = "#dbf2d8", colour = "#dbf2d8"), #Changing legend fill
        legend.background = element_rect(fill = "#dbf2d8", colour = "#dbf2d8")) +
  ylim(-3, 7) +  #-3 changes size of centre hole
  coord_polar(start = 0) +  #Making plot circular
  scale_fill_manual(values = c("#053207", "#09540c", "#528754", "#84a985"))  #Adding custom colours

p


#Saving plot
ggsave("MorePies.png", height = 8, width = 8, units = "in", dpi = 300)
