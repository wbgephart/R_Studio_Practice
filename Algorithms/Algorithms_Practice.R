install.packages("tm")
install.packages("cluster")
install.packages("factoextra")
install.packages("topicmodels")

library(tm)
library(cluster)
library(factoextra)
library(topicmodels)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(syuzhet)

df <- data.frame(SA603_A8_newdataset_1_)

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

dtm <- as.DocumentTermMatrix(tdm)
m2 <- as.matrix(dtm)
tdm2 <- weightTfIdf(tdm)
dtm2 <- as.DocumentTermMatrix(tdm2)
m3 <- as.matrix(dtm2)

#hierarchical Clustering
m2_2 <- na.omit(m2)
h1 <- agnes(m2_2, method = "complete") 
h1$height
pltree(h1, main = "Dendrogram of agnes")

#k-means clustering
k1 <- kmeans(m2, centers = 3)
k1$cluster
k2 <- kmeans(m3, centers = 3)
k2$cluster

#requires "factoextra" package
fviz_cluster(k1, data = m2)
fviz_cluster(k2, data = m3)

#silhouette coefficient
silh <- silhouette(k2$cluster, dist(m3))
silh

#find the "K"
#defined an average silhouette function
avg_sil <- function(k) {
  km.res <- kmeans(m3, centers = k, )
  ss <- silhouette(km.res$cluster, dist(m3))
  mean(ss[, 3])
}

#compute for K = 2 to K =15
k.values <- 2:15

#extract avg silhouette for 2-15 clusters
ave.silh <- as.numeric()
  for (i in k.values){
    ave.silh[i-1]<-avg_sil(i)
  }
  plot(k.values, ave.silh[],
  type = "b", pch = 19, frame = FALSE,
  xlab = "Number of clusters K",
  ylba = "Average Silhouettes")
}

# easier way
fviz_nbclust(m3, kmeans, method = "silhouette")

#final model K = 2
kresult <- kmeans(m3, centers = 2)

#k-means topic analysis (find 10 most frequent terms)
centroids <- as.data.frame(t(kresult$centers))
names(centroids) <- c("group1", "group1")
term1 <- centroids[order(-centroids$group1),]
term2 <- centroids[order(-centroids$group2),]
terms <- rbind(row.names(term1[1:10,]),
               row.names(term2[1:10,]))

#LDA
#Basic Syntax
l1 <- LDA(dtm, k = 2)

#log-likelihood
logLik(l1)

#LDA topic analysis (find 10 most frequent terms)
term <- terms(l1, 10)
term

#find K (log-likelihood method)
ldav=as.numeric()
for (i in 2:30){
  ldatemp <- LDA(dtm, k = i)
  ldav[i] <- logLik(ldatemp)
}
ldav
plot(ldav)

l2 <- LDA(dtm, k = 29)
term <- terms(l2, 10)
term





