#Loading packages
library(tidyverse)
library(tidytuesdayR)
library(tidytext)
library(ggwordcloud)
library(gutenbergr)
library(showtext)


#Adding Google fonts
showtext_auto()
font_add_google("Cookie", "Cookie")


#Loading data
dickens <- gutenberg_download(46)


#Used for tidying Dickens data https://github.com/susanli2016/Data-Analysis-with-R/blob/master/Charles_Dickens.Rmd 
#Tidying and tokenising
tidy_dickens <- dickens %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) #Removing stop words


tokens <- tidy_dickens %>% 
  unnest_tokens(word, word) %>% 
  dplyr::count(word, sort = TRUE) %>% 
  ungroup() %>% 
  top_n(50) %>% #using top 50 words only
  mutate(c = case_when(n < 21 ~ 'vlow',
                       n > 20 & n < 40 ~ 'low',
                       n > 39 & n < 100 ~ 'mid',
                       n > 100 ~ 'high')) #Adding categories for custom colour scheme


#Plotting word cloud
ggplot(data=tokens, aes(colour=c)) + 
geom_text_wordcloud_area(aes(label = word, size = n), shape = "star", family="Cookie") +
scale_size_area(max_size = 150) +
theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),  #Adjusting plot margins
      panel.background = element_rect(fill = '#F8F8F8'),  #Changing background colour
      plot.background = element_rect(fill = '#F8F8F8')) +
scale_colour_manual(values = c("#21495D", "#5D212B", "#333637", "#215d35")) #Adding custom colours


#Saving plot
ggsave("Dickens.png", height = 8, width = 10, units = "in", dpi = 300)
