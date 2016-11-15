library(dplyr)
library(plyr) #ddply
#------ read data frame
db=read.csv('OutputTable.csv')
#------ sort submissions
db=db[order(db$UserID,db$ProblemID,db$SubmissionNumber),]
dim(db)
View(db)
#------- aggregate by UserID and ProblemID ---------
  length(unique(db$UserID))
  agg.features = ddply(db,
                       .(UserID,ProblemID),
                       summarise,
                       overalGradeDiff = Grade[length(Grade)]-Grade[1],
                       CountofSubmissions = length(SubmissionNumber),
                       countOfVideoandForumEvents = (sum(NVideoEvents, na.rm = T) + sum(NForumEvents,na.rm = T)),
                       totalTimeSpent = TimeStamp[length(TimeStamp)] - TimeStamp[1])
#------ remove cases with only one attempt
  agg.features=filter(agg.features,CountofSubmissions>1); dim(agg.features)
#------ save feature file
  write.csv(agg.features, file='features.csv')
 