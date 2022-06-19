library(ggplot2)
library(readxl)

Baseball_Stats <- read_excel("Baseball_Stats.xlsx")
View(Baseball_Stats) 

Baseball_Averages <- read_excel("Baseball_Averages.xlsx")
View(Baseball_Averages) 

df <- data.frame(Baseball_Stats)
df2 <- data.frame(Baseball_Averages)

rm(Baseball_Stats, Baseball_Averages)

#Bar Graph of Home Runs Per Year 
ggplot(df2, aes(Year, Average.Home.Runs, fill = ..y..)) +
  geom_bar(stat = "identity") +
  coord_cartesian(ylim=c(125,225)) +
  theme(legend.position = "none") +
  labs(title = "Average HR per Year",
       subtitle = NULL,
       tag = NULL,
       x = "Year",
       y = "Home Runs",
       color = NULL)

#Bar Graph of Runs Per Year
ggplot(df2, aes(Year, Average.Runs, fill = ..y..)) +
  geom_bar(stat = "identity") +
  coord_cartesian(ylim=c(600,800)) +
  theme(legend.position = "none") +
  labs(title = "Average Runs per Year",
       subtitle = NULL,
       tag = NULL,
       x = "Year",
       y = "Runs",
       color = NULL)

#Plot Graph of Runs Scored on Average in MLB in comparison to Home Runs hit on Average
df2 %>%
  ggplot(aes(x = Average.Runs, y = Average.Home.Runs, label = Year)) +
  geom_abline(slope = .2353, intercept = 0) +
  geom_point() +
  geom_label_repel(min.segment.length = 0, 
                   max.overlaps = Inf,
                   label.size = 0,
                   label.padding = 0.1,
                   label.r = 0,
                   size = 3) +
  coord_fixed(xlim=c(600, 800), ylim=c(100,300)) +
  labs(title = "Home Runs vs Runs Scored",
       subtitle = NULL,
       tag = NULL,
       x = "Average Runs Scored Across MLB",
       y = "Average Home Runs Hit Across MLB",
       color = NULL) 

#Plot Graph of Home Run Percentage By Year
df2 %>%
  ggplot(aes(x = Year, y = Home.Run.Percentage)) +
  geom_abline(slope = .2374, intercept = -465.54) +
  geom_point() +
  coord_fixed(xlim=c(2004, 2020), ylim=c(10,17.5)) +
  labs(title = "HR/BIP By Year",
       subtitle = NULL,
       tag = NULL,
       x = "Year",
       y = "Home Runs Hit per Batted Ball",
       color = NULL) 

