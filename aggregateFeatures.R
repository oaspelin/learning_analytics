library(dplyr)
library(plyr) #ddply
#------ read data frame
db=read.csv('../OutputTable2.csv')
#------ sort submissions
db=db[order(db$UserID,db$ProblemID,db$SubmissionNumber),]
dim(db)
View(db)
#------- aggregate by UserID and ProblemID ---------
  length(unique(db$UserID))
  # agg.features = ddply(db,
  #                      .(ProblemID,UserID),
  #                      summarise,
  #                      totalGradeDelta = Grade[length(Grade)]-Grade[1],
  #                      bestGrade = max(Grade),
  #                      countOfSubmissions = length(SubmissionNumber),
  #                      countOfVideoAndForumEvents = (sum(NVideoEvents, na.rm = T) + sum(NForumEvents,na.rm = T)),
  #                      totalTimeSpent = TimeStamp[length(TimeStamp)] - TimeStamp[1],
  #                      avgTimeSpentPrSubmission = totalTimeSpent/countOfSubmissions,
  #                      increasePerSubmission= totalGradeDelta/countOfSubmissions)
  

#------ remove cases with only one attempt
  #agg.features=filter(agg.features,CountofSubmissions>1); dim(agg.features)
#------ save feature file
  # only neaded if some values are N/A
  db[is.na(db)]=0
  db$Improved<-factor(ifelse(db$GradeDiff>=0,"yes","no"))
  View(db)
  write.csv(db, file='features.csv')
 