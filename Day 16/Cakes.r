#Loading packages
library(tidyverse)
library(showtext)


#Adding Google fonts
showtext_auto()
font_add_google("Berkshire Swash", "BS")


#Importing data
dat <- read.csv("https://raw.githubusercontent.com/RegenMedCat/RStatsAdvent/main/Day%2016/Cakes.csv")


dat2 <- dat %>%
  mutate(Per.kg = Cost/Mass)  #Calculating cost per kg


#Calculating linear regression
#https://stackoverflow.com/questions/7549694/add-regression-line-equation-and-r2-on-graph
lm_eqn <- function(dat2){
  m <- lm(Per.kg ~ Score, dat2);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}


#Plotting scatter plot
p <- dat2 %>%
  ggplot(aes(x = Score, y = Per.kg)) +
  geom_point(colour = "#417c6f", size = 5) +
  geom_smooth(method='lm',formula = y ~ x, colour = "#2b675b", fill = "grey", alpha = 0.1) +  #Adding regression line
  theme_classic((base_size = 30)) +  #Removing grid and grey panel, increasing base size
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),  #Customising margins
        text = element_text(family = "BS", size = 80, colour = "#ecdeb9"),  #Customising fonts
        plot.title = element_text(hjust=0.5,  size = 92),
        axis.text = element_text(colour = "#ecdeb9"),
        plot.caption = element_text(size = 50),
        axis.line = element_line(colour = "#b3985c"), #Changing axis line and tick colours
        axis.ticks = element_line(colour = "#b3985c"),
        panel.background = element_rect(fill = '#8a1d23'),  #Changing fill colour to red
        plot.background = element_rect(fill = '#8a1d23')) +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) +  #Customising y-axis
  xlab("Score (out of 100)") +  #Adding axis labels and caption
  ylab("Price per kg (£)") +
  labs(title = "Christmas cakes: do you get what you pay for?",
       caption = "Data source: Good Housekeeping")


p + geom_text(x = 86, y = 26.5, label = lm_eqn(dat2), parse = TRUE, 
              family = "BS", size = 15, colour = "#ecdeb9")  #Adding regression equation


#Saving plot
ggsave("Cakes.png", height = 8, width = 10, units = "in", dpi = 300)
