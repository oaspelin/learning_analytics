setwd("~/Documents/EPFL/Digital Education and Learning Analytics/Dataset and Scripts-20161108/learning_analytics")
library(plyr) #ddply
library(dplyr)
library(caret)
library(corrplot)
#======================================================================== 
#         step 1: train classifier
#======================================================================== 

#------ read features extracted from train set, using your python script
db=read.csv('../OutputTable.csv', stringsAsFactors = F)

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

set.seed(1234)

fs=c(
  'SubmissionNumber',
  'TimeSinceLast',
  'ProblemID',
  # 'AverageForumTimeDiffs',
  # 'NForumEvents',
  # 'NVideoEvents',
  'NVideoAndForumEvents',
  # 'NumberOfThreadViews',
  # 'NumberOfThreadSubscribe',
  # 'NumberOfThreadLaunch',
  # 'NumberOfThreadPostOn',
  # 'NumberOfPostCommentOn',
  # 'NumberOfForumVotes',
  # 'ForumScore',
  'DurationOfVideoActivity',
  'AverageVideoTimeDiffs'
  # 'NumberOfVideoPlay',
  # 'NumberOfVideoSeek',
  # 'NumberOfVideoDownload',
  # 'NumberOfVideoUnique',
  # 'VideoUniquePerTotalVideoEvent'
  # 'VideoScore'
)

#============================================
#============== TRAIN CONTROL =============== 
#============================================
ctrl <- trainControl(method = "cv",
                     # number = 10,
                     # repeats = 5,
                     classProbs = TRUE,
                     # search = "random",
                     summaryFunction = twoClassSummary
                     )

#============================================
#======== LDA SBD FEATURE SELECTION =========
#============================================
filterCtrl <- sbfControl(functions = rfSBF, method = "repeatedcv", repeats = 5)

rfWithFilter <- sbf(x=db[,fs],
                    y=db$improved,
                    sbfControl = filterCtrl,
                    preProc = c("center", "scale"))

# DOESN'T SEEM TO PROVIDE ANY USEFUL INSIGHT

#============================================
#=============== RANDOM FOREST ==============
#============================================
paramGrid <- expand.grid(mtry = c(1:(max(ceiling(0.3*length(fs)),floor(sqrt(length(fs))))+1)))

model<-train(x=db[,fs],
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
paramGrid <- expand.grid(.decay = c(0.5, 0.1), .size = c(5, 6, 7))

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
#=============== glnet ======================
#============================================
#find tunegrid
model <- train(x=db[,fs],
               y=db$improved,
               method='glmnet',
               metric = "ROC",
               trControl=ctrl)
model
grid = expand.grid(.alpha=c(0.54,0.056),.lambda=seq(0.0001,0.014,by=0.00001))
model <- train(x=db[,fs],
               y=db$improved,
               method='glmnet',
               metric = "ROC",
               tuneGrid = grid,
               trControl=ctrl)
model
plot(model, metric='ROC')

#============================================
#================ CORRELATION ===============
#============================================

correlation_matrix<- cor(db[,fs])
corrplot(correlation_matrix, method = "color")

#----- check generalizability of your model on new data
preds= predict(model, newdata=db.test);
table(preds)
# install.packages('AUC')
library(AUC)
ROC_curve= roc(preds, db.test$improved);  auc(ROC_curve)
confusionMatrix(preds, db.test$improved)

#======================================================================== 
#         step 2.1: Use classifier to predict progress for test data
#======================================================================== 

testDb=read.csv('../OutputTable_test.csv', stringsAsFactors = F)
testDb$Grade=NULL; testDb$GradeDiff=NULL;
testDb[is.na(testDb)]=0

#---- use trained model to predict progress for test data
preds= predict(model, newdata=testDb[, fs]);

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
classifier_template= read.csv('../classifier_template.csv', stringsAsFactors = F)
kaggleSubmission=merge(classifier_template,cl.Results )
write.csv(kaggleSubmission,file='classifier_results.csv', row.names = F)


#------- submit the resulting file (classifier_results.csv) to kaggle 
#------- report AUC in private score in your report


