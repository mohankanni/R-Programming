
###################################################################
##############      Twitter Sentiment Analysis        #############
##############        Author: Mohan Kanni             #############
##############  Created on: Wed May 21 10:10:10 2019  #############
##############     Last Modified on: Apr 10 2019      #############
##############  Last Modified by: Mohan Kanni         #############
###################################################################


## Importing Required Libraries 

library(plyr)
library(stringr)
library(RCurl)
library(purrr)
library(httr)
library(dplyr)

# Lib to Write Excel File Output
library(xlsx)

#Libraries to Pull Tweets and to Perform Sentiment Analysis
library(twitteR)
library(sentimentr)


#Twitter Ouath Credientials
setup_twitter_oauth(
  consumer_key = "",
  consumer_secret = "",
  access_token = "-",
  access_secret = ""
)

#List of KeyWords to Retrieve Tweets 
keyword <- c('Singapore SmartCity','Singapore Health','Singapore Saftey','Singapore Security',
             'Singapore Education','Singapore Tourism')

#TweetSearch <- userTimeline("SmartTirupati", n = 2000)


#Start Of Loop for KeyWords
for (m in keyword ) {
print(m)
TweetSearch <- searchTwitter(m, n = 3200, lang = "en")
TweetSearch_df <- tbl_df(map_df(TweetSearch, as.data.frame))

# Condition to Skip Keyword with Zero Tweets
if (length(TweetSearch_df)>0) {
  
  print(m)
  
  #Filtering Tweets to Clean
  TweetSearch_df <- unique(TweetSearch_df)
  
  tweets <- TweetSearch_df$text
  
  cc <- iconv(tweets, "latin1", "ASCII", sub = "")
  
  x <- gsub("^ ", "", cc)
  
  a <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", x)
  
  b <- gsub("@\\w+", "", a)
  
  c <- gsub("#\\w+", "", b)
  
  d <- gsub("[[:punct:]]", "", c)
  
  e <- gsub("[[:digit:]]", "", d)
  
  f <- gsub("http\\w+", "", e)
  
  h <- gsub("[ \t]{2,}", "", f)
  
  t <- gsub("^\\s+|\\s+$", "", h)
  
  #Try Catch to Convert Lower Case After Cleaning
  try.error <- function(x) {
    y <- NA
    # tryCatch error
    try_error <- tryCatch(tolower(x), error = function(e) e)
    # if not an error
    if (!inherits(try_error, "error")) {
      y <- tolower(x)
    }
    # result
    return(y)
  }
  
  # lower case using try.error with sapply
  u <- sapply(t, try.error)
  
  # remove NAs in some_txt
  Text <- u[!is.na(u)]
  names(Text) <- NULL
  
  #Converting Cleaned Text into a DataFrame
  cleaned_text <- as.data.frame(Text)
  cleaned_text <- as.character(cleaned_text$Text)
  
  #Sentiment Score for Cleaned Tweets
  feedout <- sentiment(cleaned_text,
                       polarity_dt = lexicon::hash_sentiment_jockers,
                       valence_shifters_dt = lexicon::hash_valence_shifters, hyphen = "",
                       amplifier.weight = 0.8, n.before = 5, n.after = 2,
                       question.weight = 1, adversative.weight = 0.85, missing_value = 0
  )
  
  #Converting Sentiment Score to Sentiment Type
  score <- feedout$sentiment
  for (i in score) {
    wd <- data.frame(ifelse(score > 0, "Positive", ifelse(score < 0, "Negative", "Neutral")))
  }
  names(wd) <- c("Sentiment")
  
  OUTPUT <- cbind.data.frame(m, score, wd, TweetSearch_df$created, TweetSearch_df$text, cleaned_text, TweetSearch_df$favoriteCount, TweetSearch_df$retweetCount)
  
  path <- paste0(str_replace_all(m, " ", ""), ".xlsx", sep = "")
  
  write.xlsx(OUTPUT, path)
} else {
  print("No Records")
} # End of Condition

} #End of Loop

