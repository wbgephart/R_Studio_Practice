install.packages("readxl")
library(readxl)
df <- read_excel("assignment_data_M6.xlsx")

library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(syuzhet)
library(sentiment)

myCorpus <- Corpus(VectorSource(iconv(df$text, "WINDOWS-1252", "UTF-8")))
myCorpus <- tm_map(myCorpus, removeWords, stopwords('english'))
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
myCorpus <- tm_map(myCorpus, removeNumbers)
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
myCorpus <- tm_map(myCorpus, stripWhitespace)
myCorpus[[1]]$content
myCorpus <- tm_map(myCorpus, stemDocument)
tdm <- TermDocumentMatrix(myCorpus,
                          control = list(wordlengths = c(2, Inf)))
dtm_m = as.matrix(tdm)
dtm_v <- sort(rowSums(dtm_m), decreasing = TRUE)
dtm_d <- data.frame(word = names(dtm_v), freq=dtm_v)

set.seed(1234)
par(mar=c(0,0,0,0))
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
          max.words = 100, random.order = FALSE, rot.per=.4,
          colors = brewer.pal(8, "Dark2"))

findAssocs(tdm, terms = c("olemiss", "hottytoddi", "oxford", "spring", "ole", "miss"),
           corlimit = .35)

head(dtm_d, 10)

text <- iconv(df$text, "WINDOWS-1252", "UTF-8")
sentiments <- sentiment(text)
table(sentiments$polarity)
