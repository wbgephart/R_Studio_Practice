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
