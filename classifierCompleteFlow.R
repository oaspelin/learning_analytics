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
#db$NVideoAndForum= db$NVideoEvents+db$NForumEvents
db= filter(db,NVideoAndForumEvents>0)  
# db= filter(db,NVideoEvents>0)  
# db= filter(db,NForumEvents>0) 

#--- replace NA values with 0

# test to remove columns with low entry frequency
# useful <- apply(!is.na(db),2,sum) > (dim(db)[1] * 0.65)
# db = db[useful]

db[is.na(db)]=0

#----- make a catgorical variable, indicating if grade improved
db$improved = factor(ifelse(db$GradeDiff>0 ,"Yes", "No"))
table(db$improved)


# ----- (Optional) split your training data into train and test set. Use train set to build your classifier and try it on test data to check generalizability. 
set.seed(1234)
tr.index= sample(nrow(db), nrow(db)*0.9)
db.train= db[tr.index,]
db.test = db[-tr.index,]
dim(db.train)
dim(db.test)

#Control function for linear model
#set.seed(123)
#ctrl <- trainControl(method = "repeatedcv", repeats =3)

#train model
# m1_train = train(improved ~ .,data=db.train, method="lm",trControl=ctrl)
# summary(m1_train)
# summary(m1_train)$r.squared

#----- train classifier to predict 'improved' status 
#----- Try different methods, model parameters, feature sets and find the best classifier 
#----- Use AUC as model evaluation metric

# fs=c('SubmissionNumber',
#      'TimeSinceLast',
#      'ForumScore',
#      'AverageForumTimeDiffs',
#      'NForumEvents',
#      'DurationOfVideoActivity',
#      'ProblemID',
#      'VideoScore',
#      'AverageVideoTimeDiffs')


#-------------For tuneGrid-----------------#
tune<-max(ceiling(0.3*length(fs)),floor(sqrt(length(fs))))
#ifelse(tune==1, range<-c((tune):(tune+1)), range<-c((tune-1):(tune+1)))
range<-c(1:(tune+1))

paramGrid <- expand.grid(mtry = range)

ctrl= trainControl(method = 'cv', summaryFunction=twoClassSummary ,classProbs = TRUE)

model<-train(x=db.train[,fs],
             y=db.train$improved,
             method = "rf",
             metric="ROC",
             trControl = ctrl,
             tuneGrid = paramGrid,
             preProc = c("center", "scale"))
plot(model); model

#decent tutorial https://www.r-bloggers.com/the-5th-tribe-support-vector-machines-and-caret/
#svmLinear
set.seed(1492)
svmFit <- train(x=db.train[,fs],
                y=db.train$improved,
                method= "svmLinear",
                metric ="ROC",
                trControl=ctrl,
                preProc= c("center", "scale"))
svmFit$finalModel
#radial kernel
set.seed(1492)
radialFit<-train(x=db.train[,fs],
                 y=db.train$improved,
                 method= "svmRadial",
                 metric ="ROC",
                 tuneLength=9,
                 trControl=ctrl,
                 preProc= c("center", "scale"))
plot(radialFit); radialFit

set.seed(1492)
# Use the expand.grid to specify the search space	
grid <- expand.grid(sigma = c(1.36, 1.039, 1.042),
                    C = c(62, 63, 64, 65, 66)
)
radialFit<-train(x=db.train[,fs],
                 y=db.train$improved,
                 method= "svmRadial",
                 metric ="ROC",
                 tuneGrid = grid,
                 trControl=ctrl,
                 preProc= c("center", "scale"))
plot(radialFit); radialFit

fs=c('SubmissionNumber',
     'TimeSinceLast')
     # 'AverageForumTimeDiffs',
     # 'NumberOfThreadViews',
     # 'NumberOfThreadSubscribe',
     # 'NumberOfThreadLaunch',
     # 'NumberOfThreadPostOn',
     # 'NumberOfPostCommentOn',
     # 'NumberOfForumVotes',
     # 'DurationOfVideoActivity',
     # 'NumberOfVideoPlay',
     # 'NumberOfVideoSeek',
     # 'NumberOfVideoDownload',
     # 'NumberOfVideoUnique',
     # 'ProblemID',
     # 'AverageVideoTimeDiffs')

##check correlation
correlation_matrix<- cor(db.train[,fs])
corrplot(correlation_matrix, method = "color")

filterCtrl <- sbfControl(functions = ldaSBF, method = "repeatedcv", repeats = 10)
# set.seed(10)
rfWithFilter <- sbf(x=db.train[,fs],
                    y=db.train$improved,
                    sbfControl = filterCtrl)

# sControl<-safsControl(functions=rfSA,
#                       method="cv",
#                       #repeats = 5,
#                       improve=50)
# 
# knnFeatures<-safs(x=db.train[,fs],
#                   y=db.train$improved,
#                   iters=100,
#                   safsControl = sControl)

#----kNN-------#
knnFit <- train(x=db.train[,fs],
                y=db.train$improved,
                method = "knn",
                metric ="ROC",
                trControl = ctrl, 
                preProcess = c("center","scale"), 
                tuneGrid = expand.grid(.k=77:100)) #up to  40 nearest
plot(knnFit); knnFit

#-----------LDA------------#
set.seed(1234)
ldaFit<-train(x=db.train[,fs],
              y=db.train$improved,
              method = "lda",
              metric ="ROC",
              preProcess = c("center","scale"),
              trControl=ctrl)

#-----------stepLDA------------#
maxvar     <-(length(fs)) 
direction <-"forward"
tune_1     <-data.frame(maxvar,direction)

set.seed(1234)
stepldaFit<-train(x=db.train[,fs],
              y=db.train$improved,
              method = "stepLDA",
              metric ="ROC",
              preProcess = c("center","scale"),
              trControl=ctrl,
              tuneGrid = tune_1)

#----- check generalizability of your model on new data
preds= predict(model, newdata=db.test);
predsSVM= predict(svmFit, newdata=db.test);
table(preds)
table(predsSVM)
# install.packages('AUC')
library(AUC)
ROC_curve= roc(preds, db.test$improved);  auc(ROC_curve)
confusionMatrix(preds, db.test$improved)

#======================================================================== 
#         step 2.1: Use classifier to predict progress for test data
#======================================================================== 

testDb=read.csv('OutputTable_test.csv', stringsAsFactors = F)
testDb$Grade=NULL; testDb$GradeDiff=NULL;
testDb[is.na(testDb)]=0

#---- use trained model to predict progress for test data
preds= predict(model, newdata=testDb);

#======================================================================== 
#         step 2.1: prepare submission file for kaggle
#======================================================================== 

cl.Results=testDb[,c('ProblemID', 'UserID', 'SubmissionNumber')]
cl.Results$improved=preds
levels(cl.Results$improved)=c(0,1) # 
cl.Results$uniqRowID= paste0(cl.Results$UserID,'_', cl.Results$ProblemID,'_', cl.Results$SubmissionNumber)
cl.Results=cl.Results[,c('uniqRowID','improved')]
table(cl.Results$improved)

#----- keep only rows which are listed in classifier_templtae.csv file
#----- this excludes first submissions and cases with no forum and video event in between two submissions
classifier_templtaete= read.csv('classifier_templtae.csv', stringsAsFactors = F)
kaggleSubmission=merge(classifier_templtaete,cl.Results )
write.csv(kaggleSubmission,file='classifier_results.csv', row.names = F)


#------- submit the resulting file (classifier_results.csv) to kaggle 
#------- report AUC in private score in your report


