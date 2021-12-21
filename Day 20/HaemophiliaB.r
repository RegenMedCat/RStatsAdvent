#Day 20
#Loading packages
library(tidyverse)
library(showtext)
library(waffle)
library(ggpubr)
library(ggtext)
library(patchwork)
library(echarts4r)
library(htmltools)
library(png)


#Adding fonts
showtext_auto()
font_add_google("Cabin", "Cabin")


#Waffle chart of haemophilia A vs. B
#Followed https://r-charts.com/part-whole/waffle-chart-ggplot2/
p <- waffle(c('Haemophilia A' = 8, 'Haemophilia B' = 2), rows = 2, colors = c("#dc2c2c", "#8a1717")) +
  theme(plot.margin = margin(0.5, 2, 0.5, 0.5, "cm"),
        text = element_text(family = "Cabin"),
        legend.text = element_text(size = 50, margin = margin(r = 16, unit = "pt")),
        plot.title = element_text(hjust = 0.5, size = 60),
        plot.subtitle = element_text(hjust = 0.5, size = 60),
        legend.position  = c(0.5, -0.1),
        legend.direction = "horizontal",
        legend.key.width = unit(0.75, 'cm')) +
  labs(title = "Christmas disease, also known as haemophilia B, is a rare bleeding disorder caused by a lack") +
  labs(subtitle ="of a blood clotting protein called factor IX. The prevalence is around 1 in 30,000 live births.")
        
p


#Creating df for pie chart
df <- data.frame(
  group = c("Mild", "Moderate", "Severe"),
  value = c(20, 30, 50))


#Computing the position of labels
#https://www.r-graph-gallery.com/piechart-ggplot2.html
df <- df %>% 
  arrange(desc(group)) %>%
  mutate(prop = value / sum(df$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop)


#Pie chart of disease severity
p2 <- ggplot(df, aes(x = "", y = prop, fill = group)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  theme(plot.margin = margin(-2, 0.5, 0, -2, "cm"),
        legend.position = "none",
        plot.caption = element_text(hjust = 0.5, size = 60, family = "Cabin")) +
  geom_text(aes(y = ypos, label = group), color = "white", size = 22, family = "Cabin") +
  scale_fill_manual(values = c("#ad5c5c", "#dc2c2c", "#8a1717")) +
  labs(caption = "The median age of diagosis is 36 months for mild disease and 1 month for severe disease")
  
p2


#Bullet points of symptoms
#There's probably a better way to do this instead of hacking a ggplot
df2 <- data.frame(
  label = c("Bruising", "Prolonged bleeding", "Nosebleeds", "Bleeding into joints"),
  x = c(1, 1, 1, 1),
  y = c(10, 8, 6, 4))


p3 <- df2 %>%
  ggplot(aes(x = x, y = y)) + 
  geom_point(size = 10, shape = 19, colour = "#8a1717") +   #Customising points
  geom_point(aes(x = x, y = y + 0.6), size = 5, shape = 17, colour = "#8a1717") +   #Adding triangles to tops of dots to look like blood drops
  geom_text(aes(x = x + 1.1, y = y + 0.2, label = label), family = "Cabin", size = 20, hjust = 0) +
  scale_x_continuous(limits = c(0, 10)) +  #Adjusting axis to spread out bullet points
  scale_y_continuous(limits = c(0, 15)) +
  theme_void() +  #Removing axes, grid and background colour
  theme(plot.margin = margin(-8, 0, 0.5, -8, "cm"),
        plot.tag.position = c(0.5, 0.82),
        plot.tag = element_text(hjust = 0.5, size = 60, family = "Cabin", face = "bold")) +
  labs(tag = "Common symptoms")

p3


#Plotting filled icon plot for inheritance
#Followed https://www.listendata.com/2019/06/create-infographics-with-r.html
sex = data.frame(sex = c("sons affected", "daughters carriers"), value=c(50, 50),
          path = c("path://M20.822 18.096c-3.439-.794-6.64-1.49-5.09-4.418 4.72-8.912 1.251-13.678-3.732-13.678-5.082 0-8.464 4.949-3.732 13.678 1.597 2.945-1.725 3.641-5.09 4.418-3.073.71-3.188 2.236-3.178 4.904l.004 1h23.99l.004-.969c.012-2.688-.092-4.222-3.176-4.935z",
                   "path://M20.822 19.307c-2.967-.681-6.578-2.437-5.514-4.723.684 1.126 2.801 1.777 4.45.804-4.747-1.204 2.334-9.471-3.871-14.105-1.135-.853-2.526-1.283-3.912-1.283-1.378 0-2.751.425-3.862 1.283-6.206 4.634.876 12.901-3.872 14.105 1.649.974 3.77.293 4.451-.804 1.064 2.286-2.551 4.043-5.514 4.723-2.978.682-3.178 2.466-3.178 4.004l.005.689h23.99l.005-.691c0-1.537-.2-3.32-3.178-4.002z"))


sex %>% 
  e_charts(sex) %>% 
  e_x_axis(splitLine=list(show = FALSE), 
           axisTick=list(show = FALSE),
           axisLine=list(show = FALSE),
           axisLabel= list(show = FALSE)) %>%
  e_y_axis(max=100, 
           splitLine=list(show = FALSE),
           axisTick=list(show = FALSE),
           axisLine=list(show = FALSE),
           axisLabel=list(show = FALSE)) %>%
  e_color(color = c('#8a1717','#e6b2ae')) %>%
  e_pictorial(value, symbol = path, z = 10, name= 'realValue', 
              symbolBoundingData = 100, symbolClip= TRUE) %>% 
  e_pictorial(value, symbol = path, name = 'background', 
              symbolBoundingData = 100) %>% 
  e_labels(position = "bottom", offset= c(0, 10), 
           textStyle =list(fontSize = 40, 
                           colour = "black", fontWeight ='bold'),
           formatter="{@[1]}% {@[0]}") %>%
  e_legend(show = FALSE) %>%
  e_theme("westeros")


#I exported the e-chart as a image then re-imported it
#Probably a much better way to do this!
image <- readPNG("img.png", native = TRUE)


#Creating dummy data frame
df3 <- data.frame(
  x = c(1),
  y = c(1))


#Adding image to a blank plot
p4 <- df3 %>%
  ggplot(aes(x = x, y = y)) +
  theme_void() +
  inset_element(image,
                left = 0,
                bottom = 0,
                right = 1,
                top = 1) +
  theme(plot.margin = margin(0, 0, 0.5, 0, "cm"),
        plot.subtitle = element_text(hjust = 0.5, size = 60, family = "Cabin"),
        plot.tag.position = c(0.5, 0.9),
        plot.tag = element_text(size = 60, family = "Cabin"),
        panel.background = element_blank(),
        plot.background = element_blank()) +
  labs(subtitle = "Inheritance is X-linked recessive") + 
  labs(tag = "If a woman is a carrier:")

p4


#Arranging plots
def_plot <- p / p2 / p3 / p4

def_plot +
  plot_annotation(title = "What is Christmas disease?",
                  subtitle = "",
                  theme = theme(plot.title = element_text(family = "Cabin", hjust = 0.5, face = "bold", size = 120),
                                plot.subtitle = element_text(size = 40)))  #Adding title to infographic


#Saving plot
ggsave("Haemophilia.png", height = 400, width = 300, units = "mm", dpi = 300)
