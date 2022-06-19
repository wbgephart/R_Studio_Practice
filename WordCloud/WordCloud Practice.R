df1 <- read.csv ("SA603_M6_sample.csv")

library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(syuzhet)

myCorpus <- Corpus(VectorSource(df$Abstract))
myCorpus <- Corpus(VectorSource(iconv(df$Abstract, "WINDOWS-1252", "UTF-8")))
myCorpus
myCorpus[[1]]$content #Lets you read the content from the first line

#TO LOWER CASES, content_transformer transforms the content, tolower transforms it to lower case
myCorpus <- tm_map(myCorpus, content_transformer(tolower))

#TO REMOVE STOPWORDS such as at or as or the
myCorpus <- tm_map(myCorpus, removeWords, stopwords('english'))

#Remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)

#Remove anything other than English letters or space the gsub function removes all punctuation
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))

#remove extra whitespace
myCorpus <- tm_map(myCorpus, stripWhitespace)

#Define stopwords
myStopwords <- c(stopwords('english'), "purpose", "methods", "results", "conclusions",
                 "however", "conclusions", "group", "groups", "significantly", "significant", "sd")
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

#remove whitespace again
myCorpus <- tm_map(myCorpus, stripWhitespace)

#text stemming - which reduces words to their root form
myCorpus <- tm_map(myCorpus, stemDocument)

#generate term-document matrix
tdm <- TermDocumentMatrix(myCorpus,
                          control = list(wordLengths = c(2, Inf))) #as list format
dtm_m = as.matrix(tdm) #as matrix format
dtm_v <- sort(rowSums(dtm_m),decreasing = TRUE) #sorts the matrix by summation of rows
dtm_d <- data.frame(word = names(dtm_v), freq=dtm_v)

#display the top 5 most frequent words
head(dtm_d, 5)

#inspect frequent words - shows words which show up more than X# of times
freq.terms <- findFreqTerms (tdm, lowfreq = 20)
freq.terms

dtm_m = as.matrix(tdm)
dtm <- as.DocumentTermMatrix(tdm)

#Plot the most frequent words, las 
barplot(dtm_d[1:5,]$freq, las = 2, names.arg = dtm_d[1:5,]$word,
        col="lightgreen", main = "Top 5 most frequent words", ylab = "Word Frequencies")

#generate word cloud
set.seed(1234)
par(mar=c(0,0,0,0))
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
          max.words = 100, random.order=FALSE, rot.per=.4,
          colors=brewer.pal(8, "Dark2"))
         
#find associations
findAssocs(tdm, terms = c("studi", "perform", "use"), corlimit = .35)

#Sentiment
text <- iconv(df$Abstract, "WINDOWS-1252", "UTF-8")

#BING - snowball library

bing_vector <- get_sentiment(text, method="bing")
head(bing_vector)
summary(bing_vector)

#affin
afinn_vector <- get_sentiment(text, method="afinn")
head(afinn_vector)
summary(afinn_vector)

#NRC Emotions
library(syuzhet)
emotions <- get_nrc_sentiment(text)
emotions

#okagumi
install.packages("devtools")
library(devtools)
devtools::install_github('okugami79/sentiment140')
library(sentiment)
sentiments <- sentiment(text)
table(sentiments$polarity)




         
