#Day 21
#Loading packages
library(tidyverse)
library(showtext)


#Importing data
dat <- read.csv("https://raw.githubusercontent.com/RegenMedCat/RStatsAdvent/main/Day%2021/No1s.csv")


#Adding Google fonts
showtext_auto()
font_add_google("Dancing Script", "Dance")


#Filtering to 1990 onwards
dat2 <- dat %>%
  filter(Year > 1989) %>%
  mutate(Note = case_when(Weeks.at.No.1 < 7 ~ "low",
                          Weeks.at.No.1 > 6 ~ "high"))


#Plotting scatter of weeks at number 1 over time
p <-  ggplot(dat2, aes(x = Year, y = Weeks.at.No.1)) + 
  geom_point(size = 10) +   #Customising points
  geom_segment(data = filter(dat2, Note == "low"), 
               mapping = aes(x = Year + 0.24, xend = Year + 0.24, y = Weeks.at.No.1, yend = Weeks.at.No.1 + 5), 
               size = 1.5) +  #Adding stems going up
  geom_segment(data = filter(dat2, Note == "high"), 
               mapping = aes(x = Year - 0.24, xend = Year - 0.24, y = Weeks.at.No.1, yend = Weeks.at.No.1 - 5), 
               size = 1.5) +  #Adding stems going down to higher notes
    theme_classic(base_size = 30) +  #Increasing base size, removing grid lines
    theme(text = element_text(family = "Dance", size = 100),  #Using custom Google fonts
          axis.text = element_text(colour = "black"),
          plot.title = element_text(hjust = 0.5,  size = 140),
          axis.ticks = element_line(colour = "black"),
          legend.position = "none",
          panel.background = element_rect(fill = '#fcffe6'),  #Changing fill colour to cream
          plot.background = element_rect(fill = '#fcffe6'),
          panel.grid.major.y = element_line(color = "black", size = 1)) +  #Adding grid lines for ledger lines
  scale_y_continuous(limits = c(-1, 12), breaks = seq(2, 10, 2), expand = c(0, 0)) +  #Customising where grid lines will appear
  scale_x_continuous(limits = c(1990, 2021), breaks = seq(1990, 2021, 5)) +  #Customising where grid lines will appear
  xlab("") +   #Customising axis labels
  ylab("Weeks at number 1") +
  labs(title = "UK Christmas Number 1s") +   #Adding plot title
  labs(caption = "@RegenMedCat")   #Adding caption

p


#Saving plot
ggsave("No1s Weeks.png", height = 5, width = 20, units = "in", dpi = 300)
