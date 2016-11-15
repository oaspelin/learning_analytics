library(dplyr)
library(plyr) #ddply
library(caret)

ft = read.csv("features.csv")

# only neaded if some values are N/A
# ft[is.na(ft)]=0

# base linear model -> no train / test set
m1 <- lm(totalGradeDelta ~ countOfSubmissions + countOfVideoAndForumEvents + totalTimeSpent, data=ft)

set.seed(1234)
tr.index= sample(nrow(ft), nrow(ft)*0.6)
ft.train= ft[tr.index,]
ft.test = ft[-tr.index,]

m1_train = train(totalGradeDelta ~ countOfSubmissions + countOfVideoAndForumEvents + totalTimeSpent, data=ft.train, method="lm")

summary(m1_train)$r.squared
