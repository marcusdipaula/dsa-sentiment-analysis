# Web Scraping Twitter's content
# 03-DataWrangling
# This is a sequence of script 02-DataCollect.R

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

#### Extracting hashtags for exploratory analysis #####

# Making sure you have a data frame as a tibble
# For more information, see the help of this function.
twitters_search_df <- as_tibble(twitters_search_df)

# Adding NA to the cells that don't have any hashtags
# For more information, see the help of these functions.
hashtag_tweets <- apply(twitters_search_df[,1], 
                        MARGIN = 2, 
                        FUN = function(x) {
                              ifelse(
                                # test (if it detects a hashtag):
                                stringr::str_detect(x, "#\\w+")==T,
                                # if true, then a vector containing the matching elements themselves 
                                # (all values in each cell) is returned:
                                grep(pattern = "#\\w+", x = x, value = TRUE), 
                                # if false, then return NA:
                                NA ) 
                              } )


# Extracting hashtags 
# For more information, see the help of these functions.
hashtag_tweets <- hashtag_tweets %>% 
  as_tibble() %>% 
  str_extract_all("#\\w+", simplify = T) %>% 
  t() %>% 
  as_tibble()

# Is there any NA?
# any(is.na(hashtag_tweets))

# Barplot of hashtags
# For more information, see the help of these functions.
hashtag_tweets %>% 
  count(V1, sort = T) %>% 
  top_n(10) %>% 
  ggplot(aes(x = reorder(V1, n, function(n) -n), y = n, fill = n)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1)) +
  xlab("First 10 hashtags") +
  ylab("Frequency")


#### Wrangling on tweets for exploratory analysis #####

# Cleaning tweets
# For more information, see the help of these functions.
clean_tweets <- twitters_search_df %>% 
  select(text) %>% 
  apply(MARGIN = 2, 
        FUN =  function(x) {
          x = gsub("http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", " ", x)
          x = gsub("http\\w+", "", x)
          x = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", x)
          x = gsub("#\\w+", " ", x)
          x = gsub("@\\w+", " ", x)
          x = str_replace_all(x, "(\\<.*?\\>)", "")
          x = str_replace_all(x, "[[:punct:]]", " ")
          x = gsub("[[:digit:]]", " ", x)
          x = stringi::stri_trans_tolower(x)
        }) %>%  
  as_tibble()


# Looking for NA's (works fine in a data.frame or tibble object)
# sapply(clean_tweets, function(x) { sum(is.na(x))})

# Putting each word into a row with unnest_tokens of {tidytext}
# For more information, see the help of these functions.
tweet_words <- clean_tweets %>%
                  tidytext::unnest_tokens(word, text)

# creating a list of stopwords
# For more information, see the help of these functions.
list_stopwords_iso <- stopwords::stopwords(language = "en", 
                                           source = "stopwords-iso") %>% 
                                    enframe(name = NULL) %>% 
                                    add_row(value = c("trump", "president"))

# This is an alternative list:
# list_snowball <- stopwords::stopwords(language = "en", source = "snowball") %>% 
#                     enframe(name = NULL)


# Removing stopwords of the data frame
# For more information, see the help of these functions.
clean_tweet_words <- tweet_words %>% 
                        anti_join(list_stopwords_iso, 
                                  by = c("word" = "value"))

