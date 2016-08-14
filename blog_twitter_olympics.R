# author - Anupama Rajaram
# Date - August 8, 2016
# Description - Twitter analysis


# =================================================================== #
# Setting up workspace and required libraries.
# =================================================================== #
# call source file which will load all standard packages.
source("workspace_prep.R")
library(wordcloud)
library(qdap)
library(tm)
library(stringr)


# =================================================================== #
# Connect to Twitter API
# =================================================================== #
# set variables for twitter API call authorization 
# real tokens are not provided here, due to privacy reasons.
# please request your tokens from https://apps.twitter.com/
consumer_key = "ckey"
consumer_secret = "csecret"
access_token = "atoken"
access_secret = "asecret"
options(httr_oauth_cache=T) 

# Below command will enable the use of a local file to cache OAuth 
# access credentials between R sessions.
setup_twitter_oauth(consumer_key, consumer_secret,
                    access_token, access_secret)

					
# =================================================================== #
# Search the Twitter API 
# =================================================================== #					
# Search Twitter for all tweets with the tag "#TeamUSA"
# note - previously 10000 tweets were requested but the API can only return 2469
# Hence n value limited to 2000.
# "n" => number of tweets to return, "since" => start date constraint
# "geocode" => adding co-ordinates for Philadelphia & accepting tweets from 50-mile radius.
tw_search = searchTwitter('#TeamUSA', n=2000, since='2016-08-01', 
                          geocode='39.9526,-75.1652,50mi')

# store the tweets in an RDS file. The command below creates a file, if it 
# does not already exist.
saveRDS(tw_search, 'USteam_olympics.rds')

# read the RDS file - if you already have tweets stored in a file, 
# you can skip the previous step.
tw_rx = readRDS('USteam_olympics.rds')

# convert the file to a usable dataframe.
tweet_doc = twListToDF(tw_rx)

# remove special characters and emojis from tweets
tweet_doc$text <- sapply(tweet_doc$text,function(row) iconv(row, "latin1", "ASCII", sub=""))


# adding a time variable that converts tweets to Brazil timezone. 
# Rio de Janeiro follows Chicago timezone, i.e 1 hour behind Philadelphia/NYC.
tweet_doc$Riotime = with_tz(tweet_doc$created, 'America/Chicago')
tweet_doc$strptime = as.POSIXct(strptime(tweet_doc$Riotime, "%Y-%m-%d %H:%M:%S"))

# stripping Dates variable for date time manipulation
# we only concern ourselves with day and hour since our tweets only start from 1st August 2016.
tweet_doc$day = as.numeric(format(tweet_doc$strptime, "%d"))
tweet_doc$hour = as.numeric(format(tweet_doc$strptime, "%H"))

# Create new variable to determine digital device used for these Tweets.
# we extract this from the link given under variable StatusSource
par(mar = c(3, 3, 3, 2))
tweet_doc$statusSource_new = substr(tweet_doc$statusSource, 
                                    regexpr('>', tweet_doc$statusSource) + 1, 
                                    regexpr('</a>', tweet_doc$statusSource) - 1)


# =================================================================== #
# Graphical Analysis
# =================================================================== #
# number of tweets by hour - counts increase during evening and peak around 9pm CDT.
gptime <- ggplot(tweet_doc, aes(hour)) + geom_bar(aes(fill = isRetweet)) + xlab('Tweets by hour')
ggplotly(gptime)

# graph - tweet source device by tweet count
gp <- ggplot(tweet_doc, aes(x= statusSource , fill = isRetweet)) + geom_bar( )
ggplotly(gp)
# iphones dominate tweets.

# Emotional sentiment of tweets
# Split into retweets and original tweets
sp = split(tweet_doc, tweet_doc$isRetweet)
orig = sp[['FALSE']]

# Extract the retweets and pull the original author's screenname
rt = mutate(sp[['TRUE']], sender = substr(text, 5, regexpr(':', text) - 1))

polfn = lapply(orig$text, function(txt) {
        # strip sentence enders so each tweet is analyzed as a sentence,
        # and +'s which muck up regex
        gsub('(\\.|!|\\?)\\s+|(\\++)', ' ', txt) %>%
          # strip URLs
          gsub(' http[^[:blank:]]+', '', .) %>%
          # calculate polarity
          polarity()
  })
orig$emotionalValence = sapply(polfn, function(x) x$all$polarity)

# As reality check, what are the most and least positive tweets
orig$text[which.max(orig$emotionalValence)]
##[1] "That looked like a very easy win for #TeamUSA  #beachvolleyball #Rio2016"
orig$text[which.min(orig$emotionalValence)]
## [1] "I think it's a very odd sport but damn those guys are fit #Rio2016 #waterpolo #TeamUSA"


# How does emotionalValence change over the day?
filter(orig, mday(created) == 06) %>%
  ggplot(aes(created, emotionalValence)) +
  geom_point() + 
  geom_smooth(span = .5)


# Who’s retweeting whom?
# Adjust retweets to create an edgelist for network
el = as.data.frame(cbind(sender = tolower(rt$sender), 
                         receiver = tolower(rt$screenName)))
el = count(el, sender, receiver) 
rtnet = network(el, matrix.type = 'edgelist', directed = TRUE, 
                ignore.eval = FALSE, names.eval = 'num')

# Get names of only those who were retweeted to keep labeling reasonable
vlabs = rtnet %v% 'vertex.names'
vlabs[degree(rtnet, cmode = 'outdegree') == 0] = NA

col3 = RColorBrewer::brewer.pal(3, 'Paired') # Define some pretty colors, mostly for later

par(mar = c(0, 0, 3, 0))
plot(rtnet, label = vlabs, label.pos = 5, label.cex = .8, 
     vertex.cex = log(degree(rtnet)) + .5, vertex.col = col3[1],
     edge.lwd = 'num', edge.col = 'gray70', main = '#TeamUSA Retweet Network')



library(syuzhet)
mySentiment <- get_nrc_sentiment(tweet_doc$text)


# link to blogpost and analysis
# http://juliasilge.com/blog/Joy-to-the-World/

# create corpus of words from tweet texts and then process content to remove punctuations, 
# special characters, etc.
wordCorpus <- Corpus(VectorSource(tweet_doc$text))
wordCorpus <- tm_map(wordCorpus, removePunctuation)
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english"))
wordCorpus <- tm_map(wordCorpus, stripWhitespace)


# stem the word corpus
wordCorpus <- tm_map(wordCorpus, stemDocument)

# create a beautiful word cloud
pal <- brewer.pal(9,"Dark2")
pal <- pal[-(1:4)]
set.seed(38)
wordcloud(words = wordCorpus, max.words=500, random.order=FALSE, 
          rot.per=0.35, use.r.layout=FALSE, colors=pal)



# create TDM from the word corpus:
tdm <- TermDocumentMatrix(wordCorpus)
tdm

# inspect the term document matrix:
inspect(tdm[1934:1960, 70:80])

# who have we been tweeting about? extract and discplay twitter names
friends <- str_extract_all(tweet_doc$text, "@\\w+")
namesCorpus <- Corpus(VectorSource(friends))

set.seed(146)
wordcloud(words = namesCorpus, max.words=400, random.order=FALSE, 
          rot.per=0.10, use.r.layout=FALSE, colors=pal)





# load some more libraries solely for the sentiment analysis
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr )


mySentiment <- get_nrc_sentiment(tweet_doc$text)
# this dataframe assigns emotional value to each of the 2000 tweets we extracted.

# test the sentiment analysis:
tweet_doc[2,"text"]   # print the tweet
get_nrc_sentiment(tweet_doc[2,"text"]) # print emotions assigned

# example 2
tweet_doc[93,"text"]  # tweet about michael phelps
get_nrc_sentiment(tweet_doc[93,"text"])

# attach the sentiment values to tweet_doc
tweet_doc <- cbind(tweet_doc, mySentiment)


# aggregate sentiments into a new object
sentimentTotals <- data.frame(colSums(tweet_doc[,c(22:31)]))

# give column names
names(sentimentTotals) <- "count"
sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
rownames(sentimentTotals) <- NULL

# plot overal sentiment scores
gpsenti <- ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for All Tweets")
ggplotly(gpsenti)


# twitter sentiment over time
# ============================
posnegtime <- tweet_doc %>% 
  group_by(Riotime = cut(Riotime, breaks="2 hours")) %>%
  summarise(negative = mean(negative),
            positive = mean(positive)) %>% melt
names(posnegtime) <- c("timestamp", "sentiment", "meanvalue")
posnegtime$sentiment = factor(posnegtime$sentiment,levels(posnegtime$sentiment)[c(2,1)])
posnegtime$strptime = as.POSIXct(strptime(posnegtime$timestamp, "%Y-%m-%d %H:%M:%S"))

# stripping Dates variable for date time manipulation
posnegtime$day = as.numeric(format(posnegtime$strptime, "%d"))
posnegtime$hour = as.numeric(format(posnegtime$strptime, "%H"))


# define the color palette
cbPalette <- c( "#E69F00" ,    # gold
                "#56B4E9",     # blue
                "#000000",     # black
                "#009E73",     # teal green
                "#F0E442",     # yellow
                "#0072B2",     # indigo
                "#D55E00",     # red
                "#CC79A7"      # rani-pink
                )

# plot twitter sentiment by hour
emotimeseris <- ggplot(data = posnegtime, aes(x = strptime, y = meanvalue, group = sentiment)) +
  geom_line(size = 2.5, alpha = 0.7, aes(color = sentiment)) +  geom_point(size = 0.5) 
e1graph <- emotimeseris + scale_colour_manual(values=cbPalette)
ecolgraph <- e1graph + ggtitle("Sentiment Over Time") + ylab("Average sentiment score") +
  xlab("Tweets by day and hour")
ggplotly(ecolgraph) 

