install.packages("rvest")
library(rvest)
library(dplyr)


#tests
urls <- "https://www.tandfonline.com/toc/urqe20/92/4?nav=tocList"
sample <- read_html(urls)
titles <- html_nodes(sample, css = '.hlFld-Title')
titles
titles2 <- html_text(titles)
titles3 <- sample %>% html_nodes('.hlFld-Title') %>% html_text()
titles3

#find the pattern of URLs
urls <- NULL
for (j in 1:2) {
  for (i in 1:4) {
    urls[(j-1)*4+i] <- paste0("https://www.tandfonline.com/toc/urqe20/",j,"/",i,"?nav=tocList")
  }
}
urls

#test the scraper with multiple URLs
data <- data.frame()
temp <- data.frame()
titles <- NULL
for (k in urls) {
  html <- read_html(k)
  titles <- c(titles, html%>% html_nodes('.hlFld-Title') %>% html_text())
  temp <- data.frame(titles)
  data <- rbind(data,temp)
}

#scrape the titles of papers in RQES volume 1 to 5
data <- data.frame()
temp <- data.frame()
for (j in 1:5) {
  for (i in 1:4) {
    urls <- paste0("https://www.tandfonline.com/toc/urqe20/",j,"/",i,"?nav=tocList")
    html <- read_html(urls)
    titles <- html%>% html_nodes ('.hlFld-Title') %>% html_text()
    temp <- data.frame(titles)
    temp$issue <- i
    temp$volume <- j
    data <- rbind(data,temp)
  
    }
}

install.packages("twitteR")
install.packages("base64enc")
install.packages("httr")
install.packages("openssl")
install.packages("httpuv")
 
library(twitteR)
library(base64enc)
library(httr)
library(openssl)
library(httpuv)

api_key = "qgaBg3H4KXbd64ms5BPePaEau"
api_secret = "V7KKavMm2XTE8Xjj1mbeb7k58ZSlTc4YgxNAeGceuHaOWHad9m"
acc_token = "1297627822966673408-HaGIyvu8YwFHdrFBKTvWZW1Z0BUTF7"
acc_token_sec = "jTuvAg64Krjdu0fQacjTNS30NZkvoC51kcfUINwdGZaDf"

setup_twitter_oauth(api_key, api_secret, acc_token, acc_token_sec)

tweets <- userTimeline("@BoilerBall", n = 101)
tweets[[1]]$text
tweets2 <- searchTwitter("data analytics", n = 1000, lang = 'en')
tweets2[[1]]$text

data_frame <- twListToDF(tweets)
data_frame_2 <- twListToDF(tweets2)
write.csv(data_frame, "twitter_1.csv")
write.csv(data_frame_2, "twitter_2.csv")





