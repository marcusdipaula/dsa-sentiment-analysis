# Web Scraping Twitter's content
# 04-DataStoryTelling
# This is a sequence of script 03-DataWrangling

# Installing the necessary packages
# install.packages("tidyverse")
# install.packages("twitteR")
# install.packages("tidytext")
# install.packages("gridExtra")
# install.packages("stopwords")

# Loading the necessary packages
# library(tidyverse)
# library(twitteR)
# library(tidytext)
# library(gridExtra)


# Ploting the first 10 words
# For more information, see the help of these functions.
clean_tweet_words %>% 
  count(word, sort=T) %>% 
  slice(1:10) %>% 
  ggplot(aes(x = reorder(word, n, function(n) -n), y = n, fill = n)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1)) +
  xlab("First 10 words") +
  ylab("Frequency")


# Getting the words that match positive and negative sentiments
# For more information, see the help of these functions.
sentiments <- tidytext::get_sentiments("bing")

tweet_sentiment <- clean_tweet_words %>% 
  left_join(sentiments) %>% 
  filter(sentiment != "NA") # or drop_na()

table_resume <- tweet_sentiment %>% 
  group_by(sentiment) %>% 
  summarise(total = n()) %>% 
  mutate(percent = total/sum(total) )


# Other plots
# For more information, see the help of these functions.

tweet_sentiment %>% 
  count(word, sort=T) %>% 
  left_join(sentiments, by = c("word" = "word")) %>% 
  slice(1:10) %>% 
  ggplot(aes(x = reorder(word, n, function(n) -n), y = n, fill = sentiment)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1)) +
  xlab("First 10 sentiments") +
  ylab("Frequency")


ggplot(tweet_sentiment, aes(x=sentiment, y=)) +
  geom_bar(aes(fill = sentiment)) +
  guides(fill=FALSE) +
  geom_line(data = table_resume, aes(x=sentiment, y=total, group=1), linetype="dashed", size=1.5) + 
  geom_point(data = table_resume, aes(x=sentiment, y=total, group=1), size=4, shape=19) +
  geom_text(data = table_resume, aes(x=sentiment, y=total, label=total),hjust=-0.2, vjust=-0.5) + 
  xlab("Sentiments") +
  ylab("Frequency")


ggplot(data=table_resume, aes(x=sentiment, y=percent, group=1)) + 
  geom_line(colour="red",linetype="dashed", size=1.5) + 
  geom_point(colour="red", size=4, shape=19) + 
  expand_limits(y=0) +
  geom_text(aes(label = round(percent, digits = 2)), hjust=-0.2, vjust=-0.5) + 
  xlab("Sentiments") +
  ylab("Percentage of total")


clean_tweet_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  reshape2::acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  wordcloud::comparison.cloud(colors = c("#F8766D", "#00BFC4"), max.words = 70)


