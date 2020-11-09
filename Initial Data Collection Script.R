# Jonathan Schlosser
# Data Collection and Initial Analysis
# Data Incubator Challenge
# November 8, 2020

# Idea: 
  # Create a app that can be used to monitor trends that can then inform business decisions. 
  # App will allow for trends to be rcognized and trends to be searched. 
  # Will show prevalence (Freq, Proportion), TF-IDF, and Topic Model. 
  # Will also conduct sentiment analyses, geographic analyses, and word netwok analyses. 
  # Will periodically pull from various APIs (Twitter, Facebook, Instagram, TikTok, and YouTube)
    # For this initial analysis, only Twitter was aaccessed. 
  # Additional inclusion may be News media. 
    # Explored this here with the NewsAPI
  # For the ability to search by a terms, to set terms relating to one's business, and to identify related 
    # terms and topics. 
  # The final app will esentially be a predictor of trends to engage with, and a digital brand awareness managemet platform. 

# Notes:
  # Collecting initial data and exploring concept in R. 
  # Actual app will be built in Python. 

#### Bringing in libraries and setting up APIs ####
require(tidyverse)
require(tidytext)
require(quanteda)
require(plyr)
require(topicmodels)

# For News API
#install.packages("newsanchor")
require(newsanchor)


# Newsanchor APT Credentials 
# API Key should be tied to the R file. But, if not, please register for a key 
# at https://newsapi.org and enter it in the dialog box. 

#set_api_key(path = "~/.Renviron")
#api_key = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"

# For Twitter API
install.packages("rtweet")
library(rtweet)

#RTweet Twitter API Credentials and Token Creation
appname <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
key <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
secret <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
access_token <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
access_secret <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"

twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = access_token,
  access_secret = access_secret)

# Creating a final list of sources to include in the search
full_source_list = c('abc-news',
                     'associated-press',
                     'bbc-news',
                     'bloomberg',
                     'breitbart-news',
                     'business-insider',
                     'buzzfeed',
                     'cbs-news',
                     'fox-news',
                     'independent',
                     'nbc-news',
                     'newsweek',
                     'politico',
                     'reuters',
                     'the-hill',
                     'the-huffington-post',
                     'the-new-york-times',
                     'the-wall-street-journal',
                     'the-washington-post',
                     'the-washington-times',
                     'time',
                     'usa-today')

# Calling API and getting search results
news_data = data.frame()

for (i in 1:23) {
  tryCatch({
    results <- get_headlines_all(sources = full_source_list[i])
    variablename = paste(full_source_list[i], "-temp")
    news_data = rbind(news_data, results$results_df)
  }, error=function(e){})
}

write_csv(news_data, 'news_data.csv')
# Collecting Tweet Data... only streaming about 1 minutes worth... 
# This can be customized to run continuously and to be based on a search term. 

tweets = stream_tweets(lookup_coords('usa'),
                       timeout = 60)

write_csv(tweets, 'tweets.csv')

# Extracting a minimized dataframe with only a limited selection of variables. 
  # Will need to be merciless with this in the final devvelopment. 
tweets <- data.frame(tweets$created_at,
                           tweets$screen_name,
                           tweets$text,
                           tweets$is_quote,
                           tweets$is_retweet,
                           tweets$favorite_count,
                           tweets$retweet_count,
                           tweets$quote_count,
                           tweets$reply_count,
                           tweets$lang,
                           tweets$location, 
                           tweets$description,
                           tweets$followers_count,
                           tweets$friends_count,
                           tweets$statuses_count,
                           tweets$account_created_at)

colnames(tweets) <- c("created_at",
                            "screen_name",
                            "text",
                            "is_quote",
                            "is_retweet",
                            "favorite_count",
                            "retweet_count",
                            "quote_count",
                            "reply_count",
                            "lang",
                            "location", 
                            "description",
                            "followers_count",
                            "friends_count",
                            "statuses_count",
                            "account_created_at")

# Merged Text File
news_text = data_frame(news_data$published_at, news_data$description)
colnames(news_text) = c('time', 'text')

tweet_text = data_frame(tweets$created_at, tweets$text)
colnames(tweet_text) = c('time', 'text')

texts = rbind(news_text, tweet_text)

# Making corpus and DFM

text_corpus = corpus(texts, text_field = "text")

text_dfm = dfm(text_corpus, 
               stem = T, 
               remove = stopwords('english'), 
               remove_punct = T) 

# Identifying the top 100 news terms
top_text = topfeatures(text_dfm, 100)
top_text = as.data.frame(top_text)
top_text = top_text %>% rownames_to_column("words")

# Plotting the top 30 news terms 
top_text = top_text %>% arrange(desc(top_text))
top_text = top_text[1:30,]

ggplot(top_text, aes(x=reorder(words, -top_text), y=top_text)) +
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Number of Times Word Appears in Texts")+
  xlab("")+
  guides(fill=FALSE)

# Analysis by Proportion

# TF-IDF

# Topic Model

# Label Documnets by Topic

# Topics Over Time

# Identify Peaking Topics






