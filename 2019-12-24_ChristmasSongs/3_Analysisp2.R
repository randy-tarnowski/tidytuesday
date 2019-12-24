#Word analysis of 

#Inspired by https://github.com/ewenme/geniusr


library(purrr)
library(ggplot2)
library(syuzhet)
library(tidytext)
library(genius)
library(textdata)
# scrape album tracklist

# set lexicon
bing <- get_sentiments("bing")

lyrics<-read.csv("christmaslyrics_agg.csv")%>%
  select(-1:-4)%>%
  rename(totalsent=sentiment)

lyrics$lyrics<-as.character(lyrics$lyrics)

# counting negative / positive words
sentiment <-lyrics%>%
  unnest_tokens(word, lyrics) %>% 
  # remove stop words
  anti_join(stop_words) %>%
  # join afinn score
  inner_join(bing) %>%
  # count negative / positive words
  count(word, sentiment, sort = TRUE) %>%
  ungroup() %>%
  # lots of "bums" thanks to glee. also, censor "shit" and "bitch"
  filter(word!="bum")%>%
  mutate(word=ifelse(word=="bitch", "b****h", word),
         word=ifelse(word=="shit", "s**t", word))
  
sentiment$word<-stringr::str_to_sentence(sentiment$word)

library(bbplot)
# plotting top contributors
sentiment %>%
  group_by(sentiment) %>%
  top_n(15) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(alpha = 0.8, show.legend = FALSE,  colour="white", stat="identity") +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Source: Billboard Top 100 (1958-2017, A Dash of Data) \n Randy Tarnowski | @randytarnowski",
       x = NULL,
       title="Naughty & Nice: Most Commonly Occuring Positive/Negative Words in Top 100 Christmas Songs",
       subtitle = "From the Bing/Minqing lexicon") +
  coord_flip() +
  theme_ipsum_rc() + 
  scale_fill_manual(values = c("red", "seagreen")) + 
  theme(axis.text.y=element_text(size=rel(1.25))) +
  theme(plot.subtitle = element_text(face = "italic")) 


