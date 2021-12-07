#Loading packages
library(tidyverse)
library(showtext)
library(png)
library(ggimage)


#Adding Google fonts
showtext_auto()
font_add_google("Chewy", "Chewy")
font_add_google("Ubuntu", "Ubuntu")


#Importing data
#Original data source: https://public.tableau.com/app/profile/nela7296/viz/2018MincePieRating/AverageRatingDash
#Blog post: https://blog.cloudflare.com/internet-mince-pie-database/
dat <- read.csv("https://raw.githubusercontent.com/RegenMedCat/RStatsAdvent/main/Day%207/MincePies.csv")


#Loading pie image to use as points
#Borrowed code from https://buzzrbeeline.blog/2018/06/13/fun-and-easy-r-graphs-with-images/
#and https://themockup.blog/posts/2020-10-11-embedding-images-in-ggplot/
image <- readPNG("pie.png")
dat$image <- "pie.png"  #adding image to dataframe


#Defining aspect ratio
asp_ratio <- 1


#Plotting box plot of mince pie ratings
p <- dat %>%
     mutate(Seller = fct_relevel(Seller, "Supermarkets", "Other")) %>%
      ggplot(aes(x=Seller, y=Rating)) +
      geom_boxplot(fill='#f5a61d', color="black") +  #Adding custom fill colour
      geom_image(aes(x=Seller, image=image, asp = asp_ratio), size=0.06) +  #Adding pie images as points
      geom_text(data=subset(dat, Rating > 8.2),    #Adding labels for top and bottom rated
            aes(x=Seller, y=Rating, label=Brand),
            family = "Chewy", size = 15, nudge_x = 0.22) +  #Customising font and position
      geom_text(data=subset(dat, Rating < 2.5),
            aes(x=Seller, y=Rating, label=Brand),
            family = "Chewy", size = 15, nudge_x = 0.32) +
      theme_classic(base_size = 30) +  #Removing gridlines and background grey colour, increasing base size
      theme(plot.margin = margin(1, 1, 1, 1, "cm"),  #Changing plot margins
            text = element_text(family = "Chewy", size= 72),  #Using custom Google fonts
            plot.title = element_text(hjust = 0.5,  size=92),
            axis.text = element_text(family = "Ubuntu", colour = "black"),
            axis.ticks = element_line(colour = "black"),
            panel.background = element_rect(fill = '#ffe9c2'),  #Changing background colour
            plot.background = element_rect(fill = '#ffe9c2')) +
      scale_y_continuous(limits = c(0,9), breaks = seq(0, 9, 0.5), expand = c(0, 0)) +  #Customising y-axis
      ggtitle("Who makes the best mince pies?") +  #Adding title
      xlab("") +  #Removing x-axis label
      labs(caption = "Data source: 2018 Mince Pie Ratings by Nela")   #Adding caption

p


#Saving plot
ggsave("Pies.png", dpi = 300, height = 8, width = 10 * asp_ratio)
