#Loading packages
library(tidyverse)
library(showtext)
library(ggrepel)


#Adding Google fonts
showtext_auto()
font_add_google("Mountains of Christmas", "Chris")
font_add_google("PT Sans Narrow", "PT")


#Loading data
episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/episodes.csv')


specials <- episodes %>%
  select(uk_viewers, story_number, episode_title, type, first_aired)  %>%
  filter(type == "special",
         story_number %in% c("167", "178", "188", "199", "202a", "213", 
                             "225", "231", "241", "253", "263", "276")) %>%  #Filtering to Christmas specials only
  mutate(Doctor = case_when(story_number == "167" ~ "Tenth",
                            story_number == "178" ~ "Tenth",
                            story_number == "188" ~ "Tenth",
                            story_number == "199" ~ "Tenth",
                            story_number == "202a" ~ "Tenth",
                            story_number == "213" ~ "Eleventh",
                            story_number == "225" ~ "Eleventh",
                            story_number == "231" ~ "Eleventh",
                            story_number == "241" ~ "Eleventh",
                            story_number == "253" ~ "Twelfth",
                            story_number == "263" ~ "Twelfth",
                            story_number == "276" ~ "Twelfth"))  #Adding column with doctor number


#Plotting scatter of UK viewer numbers vs. air date
p <- specials %>%
  mutate(Doctor = factor(Doctor, 
                               levels = c("Tenth", "Eleventh", "Twelfth"))) %>%
  ggplot(aes(x=first_aired, y=uk_viewers, label=episode_title, shape=Doctor)) + 
  geom_point(stat='identity', size=4, colour="white")  +  #Adding points, customising colour and size
  geom_text_repel(color="white", size=12, family="PT",
                  nudge_y = 0.5, hjust = 0.5, min.segment.length = Inf) +  #Adding labels, customising font
  theme_classic(base_size = 20) +  #Increasing base size, removing grid lines
  theme(text = element_text(family="Chris", size= 72, colour="white"),  #Using custom Google fonts
        plot.title = element_text(hjust=0.5,  size=92),
        legend.text = element_text(size=44, family="PT"),
        axis.text = element_text(colour="white", family="PT"),
        axis.line = element_line(colour = "white"), #Changing axis line and tick colours
        axis.ticks = element_line(colour = "white"),
        panel.background = element_rect(fill = '#102372'),  #Changing fill colour to tardis blue
        plot.background = element_rect(fill = '#102372'),
        legend.position = "top",
        legend.title = element_blank(),
        legend.key = element_rect(fill = "#102372", colour = "#102372"), #Changing legend fill
        legend.background = element_rect(fill = "#102372")) +
  scale_y_continuous(limits = c(7, 14), breaks = seq(7, 14, 2)) + #Customising axes
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_shape_manual(values = c(19, 8, 5)) +  #Customising point shapes
  #Customising limits, removing annoying axis gap
  xlab("First Aired") +   #Customising axis labels
  ylab("UK Viewers (Millions") +
  labs(title="Doctor Who Christmas Day Specials") +   #Adding plot title
  labs(caption = "Data source: datardis")   #Adding caption

p


#Saving the plot
ggsave("DrWho.png", height = 8, width = 10, units = "in", dpi = 300)
