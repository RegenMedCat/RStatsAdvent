#Loading packages
library(tidyverse)
library(showtext)
library(palmerpenguins)


#Adding Google fonts
showtext_auto()
font_add_google("Patrick Hand", "PH")


#Checking data
view(penguins)


#Scatter plot of flipper length vs. body mass
p <- penguins %>%
  filter(!is.na(flipper_length_mm), !is.na(body_mass_g)) %>%  #Removings NAs
  ggplot(aes(x = flipper_length_mm, y = body_mass_g, colour = species, fill = species, shape = species)) +
  geom_point(size = 3, alpha = 0.8) +  #Increasing point size and making slightly transparent
  theme_classic((base_size = 30)) +  #Removing grid and grey panel, increasing base size
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),  #Customising margins
        text = element_text(family = "PH", size = 80),  #Customising fonts
        axis.text = element_text(colour = "black"),
        plot.caption = element_text(size = 60),
        panel.background = element_rect(fill = '#d4feff'),  #Changing fill colour to light blue
        plot.background = element_rect(fill = '#d4feff'),
        legend.position = "top",  #Customising legend position
        legend.direction = "horizontal", 
        legend.title = element_blank(),
        axis.ticks = element_line(colour = "black"),
        legend.key = element_rect(fill = "#d4feff", colour = "#d4feff"), #Changing legend fill
        legend.background = element_rect(fill = "#d4feff", colour = "#d4feff")) +
  scale_colour_manual(values = c("#ff1988", "#007bff", "#ff9019")) +  #Customising fill colours and shapes
  scale_fill_manual(values = c("#ff1988", "#007bff", "#ff9019")) +
  scale_shape_manual(values = c(21, 22, 25)) +
  scale_y_continuous(limits = c(2700, 6500), breaks = seq(2700, 6500, 500)) +  #Customising axes
  scale_x_continuous(limits = c(170, 235), breaks = seq(170, 235, 10)) +
  ylab("Body Mass (g)") +  #Adding axis labels and caption
  xlab("Flipper Length (mm)") +
  labs(caption = "Data source: palmerpenguins")

p


#Saving plot
ggsave("Penguins.png", height = 8, width = 10, units = "in", dpi = 300)
