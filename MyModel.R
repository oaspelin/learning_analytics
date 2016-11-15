library(dplyr)
library(plyr) #ddply
library(caret)
library(corrplot)

ft = read.csv("features.csv")
View(ft)
# only neaded if some values are N/A
# ft[is.na(ft)]=0

#see if any features correlate with eachother, these can be removed (skip 3 first columns)
correlation_matrix<- cor(ft[-(1:3)])
corrplot(correlation_matrix, method = "number")

# base linear model -> no train / test set
m1 <- lm(bestGrade ~ ., data=ft[-(1:3)])

#split model
set.seed(1234)
tr.index= sample(nrow(ft), nrow(ft)*0.6)
ft.train= ft[tr.index,]
ft.test = ft[-tr.index,]

#Control function
set.seed(123)
ctrl <- trainControl(method = "repeatedcv", repeats =3)

#train model
m1_train = train(bestGrade ~ .,data=ft.train[-(1:3)], method="lm",trControl=ctrl)
summary(m1_train)
summary(m1_train)$r.squared
