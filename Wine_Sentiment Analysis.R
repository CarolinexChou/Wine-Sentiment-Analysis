#
#
#
#    Customer and Social Analytics
#
# Team 9A
#------------------------------------------------------------------
  
### Goal: What are common traits on Old World and New World wines ###


#
#
# Load Dataset and Clean dataset
#
#

library(dplyr)
library(scales)
library(syuzhet)
library(tidyverse)
library(tidytext)
library(tm)
library(wordcloud2)
dat <- read.csv('winemag-data_first150k.csv')
str(dat)
hist(dat$price)
# Filter only wine with price less than $100
dat <- dat[dat$price < 100,]
dat <- dat[dat$points > 85,]
# Split wine to old world and new world
old_world <- c('Austria','Bulgaria','Croatia','Czech Republic',
               'England','France','Geogia','Germany','Greece','Hungary',
               'Italy','Portugal','Romania','Spain','Turkey','Switzerland')
new_world <- c('US','New Zealand','Chile','Canada','South Africa')
WineOld <- dat[dat$country %in% old_world,]
WineNew <- dat[dat$country %in% new_world,]
rm(old_world,new_world)

#
#
# New World and Old World Wine Text Cleaning
#
#

# Creating Corpus for New World Wine
corpusNew <- Corpus(VectorSource(WineNew$description))
inspect(corpusNew[1:5])
# Clean Text
corpusNew <- tm_map(corpusNew, tolower)
corpusNew <- tm_map(corpusNew, removePunctuation)
corpusNew <- tm_map(corpusNew, removeNumbers)
corpusNew <- tm_map(corpusNew, removeWords, stopwords('english'))
corpusNew <- tm_map(corpusNew, removeWords, c("–","-",
                                        "doesnt","yet","despite","now",
                                        "many","without","cut","adds","add"))
corpusNew <- tm_map(corpusNew, removeWords, c("drink","wine","wines","drinking","bottle",
                                        "side","one","little","rather",
                                        "although","body","around","almost",
                                        "will","time","best","end","plump",
                                        "weight","shows","nose","also","great",
                                        "can","well","quite","though","made",
                                        "years","year","hint","youll","opens","bottling",
                                        "lively","comes","like","somewhat","get",
                                        "give","lots","beef","bacon","sample",
                                        "offers","hints","just","still","structure","flavors"))
corpusNew <- tm_map(corpusNew, gsub,pattern = 'oaky', replacement ='oak')
corpusNew <- tm_map(corpusNew, gsub,pattern = c('fruitiness','fruity'), replacement ='fruit')
corpusNew <- tm_map(corpusNew, gsub,pattern = 'aromas', replacement ='aroma')
corpusNew <- tm_map(corpusNew, gsub,pattern = 'flavored', replacement ='flavors')
corpusNew <- tm_map(corpusNew, gsub,pattern = 'raspberries', replacement ='raspberry')
corpusNew <- tm_map(corpusNew, gsub,pattern = c('minerally','minerali'), replacement ='mineral')
corpusNew <- tm_map(corpusNew, gsub,pattern = "rosé", replacement ='rose')
corpusNew <- tm_map(corpusNew, gsub,pattern = "smoked", replacement ='smoke')
corpusNew <- tm_map(corpusNew, gsub,pattern = "sweetness", replacement ='sweet')
corpusNew <- tm_map(corpusNew, gsub,pattern = c("spices","spiced","spice"), replacement ='spicy')
corpusNew <- tm_map(corpusNew, gsub,pattern = c("acids","acid","acidic"), replacement ='acidity')
corpusNew <- tm_map(corpusNew, gsub,pattern = "peaches", replacement ='peach')
corpusNew <- tm_map(corpusNew, gsub,pattern = "plums", replacement ='plum')
corpusNew <- tm_map(corpusNew, stripWhitespace)
inspect(corpusNew[1:5])

# Creating Corpus for Old World Wine
corpusOld <- Corpus(VectorSource(WineOld$description))
inspect(corpusOld[1:5])
# Clean Text
corpusOld <- tm_map(corpusOld, tolower)
corpusOld <- tm_map(corpusOld, removePunctuation)
corpusOld <- tm_map(corpusOld, removeNumbers)
corpusOld <- tm_map(corpusOld, removeWords, stopwords('english'))
corpusOld <- tm_map(corpusOld, removeWords, c("–","-",
                                              "doesnt","yet","despite","now",
                                              "many","without","cut","adds","add"))
corpusOld <- tm_map(corpusOld, removeWords, c("drink","wine","wines","drinking","bottle",
                                              "side","one","little","rather",
                                              "although","body","around","almost",
                                              "will","time","best","end","plump",
                                              "weight","shows","nose","also","great",
                                              "can","well","quite","though","made",
                                              "years","year","hint","youll","opens","bottling",
                                              "lively","comes","like","somewhat","get",
                                              "give","lots","beef","bacon","sample",
                                              "offers","hints","just","still","structure","flavors"))
corpusOld <- tm_map(corpusOld, gsub,pattern = 'oaky', replacement ='oak')
corpusOld <- tm_map(corpusOld, gsub,pattern = c('fruitiness','fruity'), replacement ='fruit')
corpusOld <- tm_map(corpusOld, gsub,pattern = 'aromas', replacement ='aroma')
corpusOld <- tm_map(corpusOld, gsub,pattern = 'flavored', replacement ='flavors')
corpusOld <- tm_map(corpusOld, gsub,pattern = 'raspberries', replacement ='raspberry')
corpusOld <- tm_map(corpusOld, gsub,pattern = c('minerally','minerali'), replacement ='mineral')
corpusOld <- tm_map(corpusOld, gsub,pattern = "rosé", replacement ='rose')
corpusOld <- tm_map(corpusOld, gsub,pattern = "smoked", replacement ='smoke')
corpusOld <- tm_map(corpusOld, gsub,pattern = "sweetness", replacement ='sweet')
corpusOld <- tm_map(corpusOld, gsub,pattern = c("spices","spiced","spice"), replacement ='spicy')
corpusOld <- tm_map(corpusOld, gsub,pattern = c("acids","acid","acidic"), replacement ='acidity')
corpusOld <- tm_map(corpusOld, gsub,pattern = "peaches", replacement ='peach')
corpusOld <- tm_map(corpusOld, gsub,pattern = "plums", replacement ='plum')
corpusOld <- tm_map(corpusOld, stripWhitespace)
inspect(corpusOld[1:5])

#
#
#
# WordCloud on New World and Old world Wine
#
#
#

# New World Wine
tdm <- TermDocumentMatrix(corpusNew)
tdm <- as.matrix(tdm)
tdm[1:20,1:20]

N <- rowSums(tdm)
N <- subset(N, N>1000)
N
N <- data.frame(names(N),N)
colnames(N) <- c('word','freq')
wordcloud2(N,size=0.3)

# Old World Wine
tdmold <- TermDocumentMatrix(corpusOld)
tdmold <- as.matrix(tdmold)
tdmold[1:20,1:20]

O <- rowSums(tdmold)
O <- subset(O, O>1000)
O
O <- data.frame(names(O),O)
colnames(O) <- c('word','freq')
wordcloud2(O,size=0.3)

#
#
#
# Sentimental Analysis
#
#
#

# Old World Wine
# Find the overall nrc sentiments
WineOldClean <- tibble(text = str_to_lower(WineOld$description))
emotionOld <- get_nrc_sentiment(WineOldClean[1:10000,]$text)
barplot(colSums(emotionOld),las = 2,col=rainbow(10))

# Find the word inside the nrc
Old_word_counts <- WineOldClean[1:10000,] %>% 
  unnest_tokens(output = word, input = text) %>% 
  inner_join(get_sentiments("nrc")) %>% 
  count(word,sentiment,sort = TRUE)

Old_top_20 <- Old_word_counts %>% 
  group_by(sentiment) %>% 
  slice_max(order_by = n, n =10) %>% 
  ungroup() %>% 
  mutate(word =reorder(word,n))

Old_top_20 %>% 
  ggplot(aes(word,n, fill = sentiment)) +
  geom_col(show.legend = FALSE)+
  facet_wrap(~sentiment, scales = 'free_y')+
  labs(y='Old World Word Sentiment',x=NULL)+
  coord_flip()

# Calculate the polarity score
polarity_scores <- unlist(lapply(WineOldClean[1:10000,]$text, get_sentiment, method = "nrc"))
sum(polarity_scores[polarity_scores > 0]) - sum(polarity_scores[polarity_scores < 0])

# New World Wine
# Find the overall nrc sentiments
WineNewClean <- tibble(text = str_to_lower(WineNew$description))
emotionNew <- get_nrc_sentiment(WineNewClean[1:10000,]$text)
barplot(colSums(emotionNew),las = 2,col=rainbow(10))

# Find the word inside the nrc
New_word_counts <- WineNewClean[1:10000,] %>% 
  unnest_tokens(output = word, input = text) %>% 
  inner_join(get_sentiments("nrc")) %>% 
  count(word,sentiment,sort = TRUE)

New_top_20 <- New_word_counts %>% 
  group_by(sentiment) %>% 
  slice_max(order_by = n, n =10) %>% 
  ungroup() %>% 
  mutate(word =reorder(word,n))

New_top_20 %>% 
  ggplot(aes(word,n, fill = sentiment)) +
  geom_col(show.legend = FALSE)+
  facet_wrap(~sentiment, scales = 'free_y')+
  labs(y='New World Word Sentiment',x=NULL)+
  coord_flip()

# Calculate the polarity score
polarity_scores2 <- unlist(lapply(WineNewClean[1:10000,]$text, get_sentiment, method = "nrc"))
sum(polarity_scores2[polarity_scores2 > 0]) - sum(polarity_scores2[polarity_scores2 < 0])


#
#
#  Summary
#
# New world wordcloud shows more diverse flavors, like cherry, spicy, blackberry
# whilst old world wine shows obvious traditional wine traits like aroma and 
# balance of all flavors as significant traits (as we notice almost equally
# distributed number of word counts especially flavors.
# Main common traits of those two are fruity, tannins, acidity, finish. 
# So, no matter where the wine is from, main baisc wine traits are requisite.
# 
# 