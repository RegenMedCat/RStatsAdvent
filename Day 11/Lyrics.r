#Loading packages
library(tidyverse)
library(showtext)
library(tidytuesdayR)
library(tidytext)
library(ggwordcloud)


#Adding Google fonts
showtext_auto()
font_add_google("Mountains of Christmas", "Chris")

#Loading data
christmas_lyrics <- readr::read_tsv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-24/christmas_lyrics.tsv")


#Tidying and tokenising
tidy_lyrics <- christmas_lyrics  %>%
  select(title, lyric) %>%
  unnest_tokens(word, lyric) %>%
  anti_join(stop_words) #removing stop words

tokens <- tidy_lyrics %>% 
  unnest_tokens(word, word) %>% 
  dplyr::count(word, sort = TRUE) %>% 
  ungroup() %>% 
  top_n(75) %>% #using top 75 words only
  mutate(cat = case_when(n < 100 ~ 'low',    #Adding categories for custom colour scheme
                       n > 99 & n < 200 ~ 'mid',
                       n > 199 & n < 699 ~ 'high',
                       n > 700 ~ "top")) %>% 
mutate(angle = 45 * sample(-2:2, n(), replace = TRUE, prob = c(1, 1, 4, 1, 1)))  #For adding rotation to some words


#Plotting word cloud
ggplot(tokens, aes(colour = cat)) + 
  geom_text_wordcloud_area(aes(label = word, size = n, angle = angle), shape = "star", family = "Chris", rm_outside = TRUE) +
  scale_size_area(max_size = 50) +
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),  #Adjusting plot margins
        panel.background = element_rect(fill = '#F8F8F8'),  #Changing background colour
        plot.background = element_rect(fill = '#F8F8F8')) +
  scale_colour_manual(values = c("#243a29", "#3b7840","#d5a84c", "#e09200")) #Adding custom colours
