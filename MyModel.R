library(dplyr)
library(plyr) #ddply
library(caret)

ft = read.csv("features.csv")

# only neaded if some values are N/A
# ft[is.na(ft)]=0

# base linear model -> no train / test set
m2 <- lm(overalGradeDiff ~ CountofSubmissions + countOfVideoandForumEvents + totalTimeSpent, data=ft)

set.seed(1234)
tr.index= sample(nrow(ft), nrow(ft)*0.6)
ft.train= ft[tr.index,]
ft.test = ft[-tr.index,]
