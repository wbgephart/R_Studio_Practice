df <- read.csv("mlb_sample.csv")
summary(df)
mean(df$rev)
sd(df$rev)
table(df$NAME)
table(df$NAME, df$ps)
table(df$NAME, df$ps, df$AL) //NATIONAL LEAGUE TEAMS = 0, AL TEAMS = 1
quantile(df$rev)
quantile(df$rev, .3)
quantile(df$rev, c(.2, .3, .4, .5, .6, .7, .8))

hist(df$rev)
hist(df$rev, breaks = 20)
hist(df$rev, breaks = 100)
hist(df$rev, breaks = 10)
hist(df$rev, breaks = 10, main = 'Sample Histogram')
hist(df$rev, breaks = 10, main = 'Sample Histogram', xlab = 'Revenue', ylab = 'Frequency')
hist(df$rev, breaks = 10, main = 'Sample Histogram', xlab = 'Revenue', ylab = 'Frequency', col = 'red', border = 'green')

df_bos <- df[which(df$NAME == 'BOS'),]
df_nyy <- df[which(df$NAME == 'NYY'),]
plot(df_bos$YEAR, df_bos$rev)
plot(df_bos$YEAR, df_bos$rev, type = "l")
plot(df_bos$YEAR, df_bos$rev, type = "b")
plot(df_bos$YEAR, df_bos$rev, type = "o")
plot(df_bos$YEAR, df_bos$rev, type = "l", col = "red", main = "revenue of Redsox")
lines(df_nyy$YEAR, df_nyy$rev, type = "l", col = "blue")
plot(df_bos$YEAR, df_bos$rev, type = "l", col = "red", main = "revenue of Redsox", ylim = c(150, 500))

boxplot(df_bos$rev)
boxplot(df_bos$rev, df_nyy$rev)
boxplot(df_bos$rev, df_nyy$rev, main = "Revenue Box Plots", xlab = "team", ylab = "revenue", col = "yellow", border = "blue")
legend ("topleft", c("RedSox", "Yankees"))

cor(df_bos$rev, df_nyy$rev)
cor.test(df_bos$rev, df_nyy$rev)

t.test(df_bos$rev)
t.test(df_bos$rev, mu=250)
t.test(df_bos$rev, df_nyy$rev)
t.test(df_bos$rev, df_nyy$rev, paired = TRUE)

#H1: The team go to the post season have greater revenue than the teams that fail to go to the post season
t.test(df$rev ~ df$ps)
boxplot(df$rev ~ df$ps)

aov(df$rev ~ df$ID)
result <- aov(df$rev ~ df$ID)
summary(result)

#H1 REvenue of AL teams is greater than revenue of NL teams
result <- aov(df$rev ~ df$AL)
summary(result)

result2 <- aov(df$rev ~ df$ps)
summary(result2)

result2 <- aov(df$rev ~ as.factor(df$ps))
summary(result2)

TukeyHSD(result2)

result <- aov(df$rev ~ as.factor(df$ID))
summary(result)
TukeyHSD(result)

result3 <- aov(rev~as.factor(ps)*as.factor(AL), data = df)
summary(result3)
TukeyHSD(result3)

interaction.plot(x.factor = df$ps, trace.factor = df$AL, response = df$rev, type = "b")


