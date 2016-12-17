library(plyr) #ddply
library(dplyr)
library(caret)
library(corrplot)
library(doMC)
#======================================================================== 
#         step 1: train classifier
#======================================================================== 

#------ read features extracted from train set, using your python script
db=read.csv('OutputTable.csv', stringsAsFactors = F)

#------ sort submissions
db=db[order(db$UserID,db$ProblemID,db$SubmissionNumber),]

#----- remove first submissions
db= filter(db,SubmissionNumber>0)

#---- remove cases when there is no video or forum activity between two submissions
db= filter(db,NVideoAndForumEvents>0)  
db[is.na(db)]=0

#----- make a catgorical variable, indicating if grade improved
db$improved = factor(ifelse(db$GradeDiff>0 ,"Yes", "No"))
table(db$improved)

#----- custom R features
db["VideoSinceLast"]<-NA
db$VideoSinceLast<-(db$NVideoEvents/db$TimeSinceLast)

db["ForumSinceLast"]<-NA
db$ForumSinceLast<-(db$NForumEvents/db$TimeSinceLast)

db["VideoPerSubmission"]<-NA
db$VideoPerSubmission<-(db$NVideoEvents/db$SubmissionNumber)

db["ForumPerSubmission"]<-NA
db$ForumPerSubmission<-(db$NForumEvents/db$SubmissionNumber)

set.seed(1234)

fs=c(
  'SubmissionNumber',
  'TimeSinceLast',
  'ProblemID',
  # 'NVideoEvents',
  # 'NForumEvents',
  # 'NVideoAndForumEvents',
  'DurationOfVideoActivity',
  'AverageVideoTimeDiffs', 
  # 'NumberOfVideoUnique', 
  'VideoPlayScore', 
  'VideoSeekScore', 
  'VideoDownloadScore', 
  'VideoEventCountScore', 
  # 'NumberOfVideoPlay', 
  # 'NumberOfVideoSeek', 
  # 'NumberOfVideoDownload', 
  # 'VideoScore', 
  #   'VideoUniquePerTotalVideoEvent',
  'AverageForumTimeDiffs', 
  'ThreadViewScore', 
  'ThreadSubscribeScore', 
  'ThreadLaunchScore', 
  'ThreadPostOnScore', 
  'PostCommentOnScore', 
  'ForumVoteScore', 
  # 'NumberOfThreadView', 
  # 'NumberOfThreadSubscribe', 
  # 'NumberOfThreadLaunch', 
  # 'NumberOfThreadPostOn', 
  # 'NumberOfPostCommentOn', 
  # 'NumberOfForumVote', 
  #   'ForumEventCountScore',
  'VideoSinceLast',
  'ForumSinceLast',
  'VideoPerSubmission',
  'ForumPerSubmission'
  # 'ForumScore'
)

registerDoMC(8)

#============================================
#================ CORRELATION ===============
#============================================

correlation_matrix <- cor(db[,fs])
corrplot(correlation_matrix, method = "color")

highlyCorrelated <- findCorrelation(correlation_matrix, cutoff=0.6)

fs[highlyCorrelated]

#============================================
#============== TRAIN CONTROL =============== 
#============================================
ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 10,
                     classProbs = TRUE,
                     # search = "random",
                     summaryFunction = twoClassSummary
)

#============================================
#========== RFE FEATURE SELECTION ===========
#============================================
filterCtrl <- rfeControl(functions=caretFuncs, method="cv", number=2)

rfWithFilter <- rfe(x=db[,fs],
                    y=db$improved,
                    sizes=c(1:length(fs)),
                    method="rf",
                    rfeControl=filterCtrl)

# DOESN'T SEEM TO PROVIDE ANY USEFUL INSIGHT

#============================================
#=============== RANDOM FOREST ==============
#============================================
paramGrid <- expand.grid(mtry = c(2:(max(ceiling(0.3*length(fs)),floor(sqrt(length(fs))))+1)))
model<-train(x=db[,rfWithFilter$optVariables],
             y=db$improved,
             method = "rf",
             metric="ROC",
             trControl = ctrl,
             tuneGrid = paramGrid,
             preProc = c("center", "scale"))
plot(model); model

#============================================
#=============== NEURAL NETWORKS ============
#============================================
#find tunegrid
paramGrid <- expand.grid(.decay = c(0.5, 0.4, 0.3, 0.2, 0.1, 0.001), .size = c(4:14))

model<-train(x=db[,fs],
             y=db$improved,
             method="nnet",
             metric ="ROC",
             linout=FALSE, 
             trace=FALSE,
             preProcess = c("center","scale"),
             trControl = ctrl,
             tuneGrid = paramGrid
)
plot(model);model

#============================================
#=============== SVM RBF ====================
#============================================
paramGrid = data.frame(.C = c(.25, .5, 1),.sigma = .05)

model <- train(x=db[,fs],
               y=db$improved,
               method = "svmRadial",
               tuneGrid = paramGrid, 
               trControl = ctrl,
               preProc = c("center", "scale"))

#======================================================================== 
#         step 2.1: Use classifier to predict progress for test data
#======================================================================== 

testDb=read.csv('OutputTable_test.csv', stringsAsFactors = F)
testDb$Grade=NULL; testDb$GradeDiff=NULL;

#----- custom R features
testDb["VideoSinceLast"]<-NA
testDb$VideoSinceLast<-(testDb$NVideoEvents/testDb$TimeSinceLast)

testDb["ForumSinceLast"]<-NA
testDb$ForumSinceLast<-(testDb$NForumEvents/testDb$TimeSinceLast)

testDb["VideoPerSubmission"]<-NA
testDb$VideoPerSubmission<-(testDb$NVideoEvents/testDb$SubmissionNumber)

testDb["ForumPerSubmission"]<-NA
testDb$ForumPerSubmission<-(testDb$NForumEvents/testDb$SubmissionNumber)

testDb[is.na(testDb)]=0

#---- use trained model to predict progress for test data
preds= predict(model$finalModel, newdata=testDb);

#======================================================================== 
#         step 2.1: prepare submission file for kaggle
#======================================================================== 

cl.Results=testDb[,c('ProblemID', 'UserID', 'SubmissionNumber')]
cl.Results$improved=preds
levels(cl.Results$improved)=c(0,1) # 
cl.Results$uniqRowID= paste0(cl.Results$UserID,'_', cl.Results$ProblemID,'_', cl.Results$SubmissionNumber)
cl.Results=cl.Results[,c('uniqRowID','improved')]
table(cl.Results$improved)

#----- keep only rows which are listed in classifier_template.csv file
#----- this excludes first submissions and cases with no forum and video event in between two submissions
classifier_template= read.csv('classifier_template.csv', stringsAsFactors = F)
kaggleSubmission=merge(classifier_template,cl.Results )
write.csv(kaggleSubmission,file='classifier_results.csv', row.names = F)


#------- submit the resulting file (classifier_results.csv) to kaggle 
#------- report AUC in private score in your report


