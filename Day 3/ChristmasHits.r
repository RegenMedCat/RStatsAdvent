#Loading packages
library(tidyverse)
library(tidytuesdayR)
library(showtext)


#Adding Google fonts
showtext_auto()
font_add_google("Snowburst One", "Snow")
font_add_google("Quicksand", "Quicksand")


#Loading data
tuesdata <- tidytuesdayR::tt_load(2019, week = 52)

christmas_songs <- tuesdata$christmas_songs


#Line graph of peak chart position over time
p <- christmas_songs %>%
      select(year, peak_position) %>%
      group_by(year) %>%
      filter(peak_position == min(peak_position)) %>%   #Filtering to highest chart position
      distinct(year, peak_position, .keep_all= TRUE) %>%   #Removing duplicates
      ggplot(aes(x=year, y=peak_position)) +
      geom_line(size=1, colour="white", linetype = "dotted") +  #Changing point and line appearance
      geom_point(size=5, colour="white", shape=8) +
      theme_classic(base_size = 20) +  #Increasing base size, removing grid lines
      theme(plot.margin = margin(1, 1, 1, 1, "cm"),   #Changing plot margins
            text = element_text(family="Snow", size= 72, colour="white"),  #Using custom Google fonts
            plot.title = element_text(hjust=0.5,  size=92),
            legend.text = element_text(size=44, family="Quicksand"),
            axis.text = element_text(colour="white", family="Quicksand"),
            axis.line = element_line(colour = "white"), #Changing axis line and tick colours
            axis.ticks = element_line(colour = "white"),
            panel.background = element_rect(fill = '#040c26'),  #Changing background colour to navy
            plot.background = element_rect(fill = '#040c26')) +
      scale_y_reverse(limits = c(100, 1), breaks = c(1, 10, 20, 40, 30, 50, 60, 70, 80, 90, 100)) + #Reversing y-axis and customising breaks
      scale_x_continuous(limits = c(1955,2018), breaks = seq(1955, 2018, 5), expand = c(0, 0)) +  #Customising x-axis
      xlab("Year") +   #Customising axis labels
      ylab("Peak Chart Position") +
      labs(title="Christmas Hits") +   #Adding plot title
      labs(caption = "Data source: Kaggle")   #Adding caption

p


#Saving plot
ggsave("Christmas Hits.png", height = 8, width = 12, units = "in", dpi = 300)
