#Loading packages
library(tidyverse)
library(showtext)
library(tidytext)
library(ggwordcloud)


#Adding Google fonts
showtext_auto()
font_add_google("Dancing Script", "Dance")


#Importing data
dat <- read.delim("https://gist.githubusercontent.com/DeastinY/899d532069febdb969d50eb68b7be583/raw/d4c2b7d6cd58639274fa2f061db6905c58853947/input.txt")


#Tidying and tokenising
tokens <- dat %>%
  mutate(word = 1) %>%
  unnest_tokens(word, O.Come..All.Ye.Faithful) %>%
  anti_join(stop_words) %>% #removing stop words
  dplyr::count(word, sort = TRUE) %>%
  top_n(20)  %>%
mutate(c = case_when(n < 201 ~ 'vlow',
                     n >200 & n < 300 ~ 'low',
                     n > 299 & n < 400 ~ 'mid',
                     n > 399 ~ 'high')) #Adding categories for custom colour scheme


#Plotting word cloud
p <- ggplot(data = tokens, aes(colour = c)) + 
  geom_text_wordcloud_area(aes(label = word, size = n), family = "Dance", rm_outside = TRUE) +
  scale_size_area(max_size = 30) +
  theme(panel.background = element_rect(fill = "#FFFDF5"),  #Changing background colour
        plot.background = element_rect(fill = "#FFFDF5")) +
  scale_colour_manual(values = c("#283026", "#540B0E", "#9E2A2B", "#8f6527"))

p
