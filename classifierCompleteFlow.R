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
db$VideoSinceLast<-(db$DurationOfVideoActivity/db$TimeSinceLast)

# db["ForumSinceLast"]<-NA
# db$ForumSinceLast<-(db$NForumEvents/db$TimeSinceLast)
# 
# db["VideoPerSubmission"]<-NA
# db$VideoPerSubmission<-(db$NVideoEvents/db$SubmissionNumber)
# 
# db["ForumPerSubmission"]<-NA
# db$ForumPerSubmission<-(db$NForumEvents/db$SubmissionNumber)

set.seed(1234)

fs=c(
  'SubmissionNumber',
  'TimeSinceLast',
  'ProblemID',
  # 'NVideoEvents',
  # 'NForumEvents',
  # 'NVideoAndForumEvents',
  # 'DurationOfVideoActivity',
  'AverageVideoTimeDiffs',
  'AverageForumTimeDiffs',
  
  'VideoSinceLast',
  
  # 'NumberOfVideoUnique',
  # 'VideoEventCountScore',
  # 'NumberOfVideoPlay',
  # 'NumberOfVideoSeek',
  # 'NumberOfVideoDownload',
  # 'VideoUniquePerTotalNumberVideoEvent',

  # 'NumberOfForumLoad',
  # 'NumberOfThreadView',
  # 'NumberOfThreadSubscribe',
  # 'NumberOfThreadLaunch',
  # 'NumberOfThreadPostOn',
  # 'NumberOfPostCommentOn',
  # 'NumberOfForumVote',
  # 'ForumEventCountScore',
  

  'ThreadViewScore',
  'ThreadSubscribeScore',
  'ThreadLaunchScore',
  'ThreadPostOnScore',
  'PostCommentOnScore',
  'ForumVoteScore',
  'ForumLoadScore',
  
  'VideoUniqueScore',
  'VideoPlayScore', 
  'VideoSeekScore', 
  'VideoDownloadScore' 
)

# registerDoMC(8)

#============================================
#================ DATA SPLIT ================
#============================================

set.seed(1234)
tr.index= sample(nrow(db), nrow(db)*0.7)
db.train= db[tr.index,]
db.test = db[-tr.index,]

#============================================
#================ CORRELATION ===============
#============================================

correlation_matrix <- cor(db.train[,fs])
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

rfWithFilter <- rfe(x=db.train[,fs],
                    y=db.train$improved,
                    sizes=c(1:length(fs)),
                    method="rf",
                    rfeControl=filterCtrl)

# DOESN'T SEEM TO PROVIDE ANY USEFUL INSIGHT

#============================================
#=============== RANDOM FOREST ==============
#============================================
# (max(ceiling(0.3*length(fs)),floor(sqrt(length(fs))))+1))
paramGrid <- expand.grid(mtry = c(6:12))
model<-train(x=db.train[,rfWithFilter$optVariables],
             y=db.train$improved,
             method = "rf",
             metric="ROC",
             trControl = ctrl,
             tuneGrid = paramGrid,
             preProc = c("center", "scale"))
plot(model); model

#============================================
#=============== NEURAL NETWORKS ============
#============================================
paramGrid <- expand.grid(.decay = c(0.5, 0.3, 0.1, 0.001), .size = c(6:14))

model<-train(x=db.train[,fs],
             y=db.train$improved,
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

model <- train(x=db.train[,fs],
               y=db.train$improved,
               method = "svmRadial",
               tuneLength = 13,
               trControl = ctrl,
               metric="ROC",
               preProc = c("center", "scale"))

paramGrid <- expand.grid(sigma = c(.01, .015, 0.2, 2.5, 3),
                    C = c(0.75, 0.9, 1, 1.1, 1.25)
)

#Train and Tune the SVM
model <- train(x=db.train[,fs],
                  y= db.train$improved,
                  method = "svmRadial",
                  preProc = c("center","scale"),
                  metric="ROC",
                  tuneGrid = paramGrid,
                  trControl=ctrl)

#============================================
#============== PREDICTABILITY ==============
#============================================
library(AUC)

preds= predict(model, newdata=db.test[, fs]);
table(preds)
ROC_curve= roc(preds, db.test$improved);  auc(ROC_curve)


#========================================================================
#         step 2.1: Use classifier to predict progress for test data
#========================================================================

testDb=read.csv('OutputTable_test.csv', stringsAsFactors = F)
testDb$Grade=NULL; testDb$GradeDiff=NULL;

#----- custom R features
testDb["VideoSinceLast"]<-NA
testDb$VideoSinceLast<-(testDb$DurationOfVideoActivity/testDb$TimeSinceLast)
# 
# testDb["ForumSinceLast"]<-NA
# testDb$ForumSinceLast<-(testDb$NForumEvents/testDb$TimeSinceLast)
# 
# testDb["VideoPerSubmission"]<-NA
# testDb$VideoPerSubmission<-(testDb$NVideoEvents/testDb$SubmissionNumber)
# 
# testDb["ForumPerSubmission"]<-NA
# testDb$ForumPerSubmission<-(testDb$NForumEvents/testDb$SubmissionNumber)

testDb[is.na(testDb)]=0

#---- use trained model to predict progress for test data
preds= predict(model, newdata=testDb[,fs]);

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
