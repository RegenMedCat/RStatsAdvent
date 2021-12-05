#Loading packages
library(tidyverse)
library(ggpubr)
library(showtext)


#Adding Google fonts
showtext_auto()
font_add_google("Lobster", "Lobster")


#Custom palette
pal <- c("#f5a045", "#f2f545", "#f28715", "#b16009", "#f7f98e", "#fdfeea")


#Importing data
dat <- read.csv("https://raw.githubusercontent.com/RegenMedCat/RStatsAdvent/main/Day%205/Foods.csv")

#Checking data
view(dat)


#Plotting donut chart of donut nutritional content
d <- dat %>%
  filter(Food == "Donuts") %>%
  ggdonutchart("Percentage", label = "Component", fill="Component") +
  theme_void() +  #Removing labels
  theme(plot.title = element_text(family = "Lobster", hjust = 0.5, size = 60),
        legend.text = element_text(family = "Lobster", hjust = 0.5, size = 25),
        legend.title = element_blank(),
        legend.direction = "horizontal") +
  scale_fill_manual(values = pal) +
  ggtitle("Donuts")

d


#Hula Hoops
d2 <- dat %>%
  filter(Food == "Hula.Hoops") %>%
  ggdonutchart("Percentage", label = "Component", fill="Component") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(family = "Lobster", hjust = 0.5, size = 60)) +
  scale_fill_manual(values = pal) +
  ggtitle("Hula Hoops")

d2


#Cheerios
d3 <- dat %>%
  filter(Food == "Cheerios") %>%
  ggdonutchart("Percentage", label = "Component", fill="Component") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(family = "Lobster", hjust = 0.5, size = 60)) +
  scale_fill_manual(values = pal) +
  ggtitle("Cheerios")

d3


#Onion rings
d4 <- dat %>%
  filter(Food == "Onion.Rings") %>%
  ggdonutchart("Percentage", label = "Component", fill="Component") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(family = "Lobster", hjust = 0.5, size = 60)) +
  scale_fill_manual(values = pal) +
  ggtitle("Onion Rings")

d4


#Party Rings
d5 <- dat %>%
  filter(Food == "Party.Rings") %>%
  ggdonutchart("Percentage", label = "Component", fill = "Component") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(family = "Lobster", hjust = 0.5, size = 60)) +
  scale_fill_manual(values = pal) +
  ggtitle("Party Rings")

d5


#Arranging plots
#https://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots
prow <- plot_grid( d + theme(legend.position = "none"), d2, d3, d4, d5, nrow = 1)

prow


#Getting common legend
legend <- get_legend(d + theme(legend.position = c(0.5, 0)))

p <- plot_grid(legend, prow, ncol = 1, rel_heights = c(0.2, 1))
p


#Adding annotations
annotate_figure(p,
                top = text_grob("Five Beige Rings!",
                                x = 0.5, y = 0, family="Lobster", size = 100))


#Saving plot
ggsave("Rings.png", height = 5, width = 10, units = "in", dpi = 300)
