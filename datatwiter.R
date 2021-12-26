library(twitteR)
library(ROAuth)
library(tm)
library(rtweet)
library(wordcloud2)
library(wordcloud)
setup_twitter_oauth('Ku2qFpZO8NpMpo32oEo2b2QE3','zuZw35SGd03a6RCZUuEo8HSwz2Oe8ZUvAQFjMGXSrRM1aTsYnh','1183706196785561600-T3I2TbJm6DQvvhzdKVd9O3zj1a58ad','1Eq3ziKOUgHQSw4z9EmdFgzlsofbGBxSWtfUoTqImNKcK')

#ambil data dari twitter
yogyakarta<-searchTwitter("prambanan", n=1500, retryOnRateLimit = 10e3)

#save data mentah
saveRDS(yogyakarta,file = 'inidata_mentah.rds')

#load data set
yogyakarta<-readRDS('inidata_mentah.rds')
jogja=twListToDF(yogyakarta)
View(jogja)

##visualisasi time series 
ts_plot(jogja, "3 hour") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frekuensi tweet tentang prambanan selama 9 hari kebelakang",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

#lihat data mentah
View(jogja)

#cleaning data
#hanya ambil tweet saja
komen<-jogja$text
komenc<-Corpus(VectorSource(komen))

#hapus tanda baca, link url, huruf aneh, dan emoji
removeURL <-function(x) gsub("http[^[:space:]]*", "", x)
twitclean <-tm_map(komenc,removeURL)

removeRT<-function(y) gsub("RT", "", y)
twitclean<-tm_map(twitclean,removeRT)

removeUN<-function(z) gsub("@\\w+", "", z)
twitclean<-tm_map(twitclean,removeUN)

remove.all <- function(xy) gsub("[^[:alpha:][:space:]]*", "", xy)
twitclean <- tm_map(twitclean,remove.all)

twitclean<-tm_map(twitclean, removePunctuation)
twitclean<-tm_map(twitclean, tolower)

#membuat nilai untuk masing-masing kata
{
  dtm<-TermDocumentMatrix(twitclean)
  m<-as.matrix(dtm)
  v<-sort(rowSums(m),decreasing = TRUE)
  jogja<-data.frame(word=names(v),freq=v)
}
head(jogja,n=10)

wordcloud2(jogja,shape = "cloud",
           backgroundColor = "black",
           color = 'random-light',
           size = 0.5)

## save data
dataframe<-data.frame(text=unlist(sapply(twitclean,'[')),stringsAsFactors = F)
View(dataframe)

write.csv(dataframe , "inidata_bersih.csv")

