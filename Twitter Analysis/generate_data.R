#Libraries to Pull Tweets and to Perform Sentiment Analysis
library(twitteR)
library(xlsx)
library(httr)

options(java.parameters = "- Xmx2048m")


#Twitter Ouath Credientials
setup_twitter_oauth(
  consumer_key = "",
  consumer_secret = "",
  access_token = "",
  access_secret = ""
)

#List of Countries
countries <- read_excel("countries.xlsx")

# #Top Countries with Most Twitter Users and Per Capita
# filtered<-c('United States','Japan','United Kingdom','Saudi Arabia','Turkey','Brazil','India'
#             ,'Mexico','Indonesia','Spain','France','Canada','Philippines','Thailand','South Korea',
#             'Argentina','Germany','Malaysia','Colombia','Australia','Singapore','Kuwait','Brunei','Sweden','Netherlands')
# 
# countries<-countries[countries$name %in% filtered, ]

for (i in 1:nrow(countries)){
  print(paste0(i,countries$country))
  countries$text[i] <- paste0(countries$latitude[i],",",countries$longitude[i],",",'1000mi')
}

#Creating Empty Data Frame for Final Data

finaldf <- data.frame()

trackingdf <- data.frame()

# Start of Country Based Loop
for (i in 1:nrow(countries)){
  print(paste0(i,"  ",countries$country[i]))
  if(i %in% c(1:100*10)){
    print(paste0(i," - Sleeping for 2 Minutes"))
    Sys.sleep(120)
  } 
#List of tweets to Retrieve Tweets
TweetSearch <- searchTwitter('accident+fire', n = 3200, lang = "en",
                             geocode=countries$text[i],
                             retryOnRateLimit=2)

# Condition to Skip Keyword with Zero Tweets
if (length(TweetSearch)>0) {
  print(paste0("Total No of Tweets: ",length(TweetSearch)))
  total <- length(TweetSearch)
  striped_Tweets <- strip_retweets(TweetSearch, strip_manual = TRUE, strip_mt = TRUE)
  tweetsDF <- twListToDF(striped_Tweets)
  newdf<-tweetsDF[,c("text","favoriteCount","created","truncated","id",
                  "statusSource","screenName","retweetCount","longitude","latitude")]
  newdf$country <- countries$country[i]
  newdf$label <- 'accident+fire'
  stripped <- nrow(newdf)
  finaldf <- rbind(finaldf, newdf)
  metricsdf <- cbind.data.frame(total,stripped,'accident+fire',countries$country[i])
  trackingdf <- rbind(trackingdf, metricsdf)
  print(paste0("Total No of Tweets After Removing Retweets: ",nrow(newdf)))
  }else{
    print("No Records")
  }
}


##Exporting Data

# write.xlsx(finaldf,"./Twitter/rawdata/tweets.xlsx",
#            row.names = FALSE)
write.table(finaldf,"tweets1.txt",sep = ",",row.names = FALSE)
# write.csv(finaldf,"tweets.txt",
#            row.names = FALSE)
# saveRDS(finaldf,"./Twitter/rawdata/tweets.rds")
# saveRDS(trackingdf,"./Twitter/rawdata/trackingdf.rds")

##Read , Bind and Save Again
# finalread<- readRDS(file = "tweets.rds")
# finaldf <- rbind(finaldf, finalread)
saveRDS(finaldf,"tweets1.rds")

# trackread<- readRDS(file = "./Twitter/rawdata/trackingdf.rds")
# trackingdf <- rbind(trackingdf, trackread)
saveRDS(trackingdf,"trackingdf1.rds")

count(finaldf, "id")
library(dplyr)
finaldf %>%
  group_by(id) %>%
  summarise(n_distinct(id))
