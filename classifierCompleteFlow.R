library(dplyr)
library(plyr) #ddply
#======================================================================== 
#         step 1: train classifier
#======================================================================== 

  #------ read features extracted from train set, using your python script
  db=read.csv('../OutputTable2.csv', stringsAsFactors = F)
  
  #------ sort submissions
  db=db[order(db$UserID,db$ProblemID,db$SubmissionNumber),]
  
  #--- replace NA values with 0
  db[is.na(db)]=0
  
  #----- remove first submissions
  db= filter(db,SubmissionNumber>0)
  
  #---- remove cases when there is no video or forum activity between two submissions
  db$NVideoAndForum= db$NVideoEvents+db$NForumEvents
  db= filter(db,NVideoAndForum>0)  
  
  #----- make a catgorical vribale, indicating if grade improved
  db$improved = factor(ifelse(db$GradeDiff>0 ,'Yes', 'No' ))
  table(db$improved)
  
  # ----- (Optional) split your training data into train and test set. Use train set to build your classifier and try it on test data to check generalizability. 
  set.seed(1234)
  tr.index= sample(nrow(db), nrow(db)*0.9)
  db.train= db[tr.index,]
  db.test = db[-tr.index,]
  dim(db.train)
  dim(db.test)
  
  #Control function for linear model
  set.seed(123)
  ctrl <- trainControl(method = "repeatedcv", repeats =3)
  
  #train model
  m1_train = train(improved ~ .,data=db.train, method="lm",trControl=ctrl)
  summary(m1_train)
  summary(m1_train)$r.squared
  
  #----- train classifier to predict 'improved' status 
  #----- Try different methods, model parameters, feature sets and find the best classifier 
  #----- Use AUC as model evaluation metric
  library(caret)
  fs=c('TimeSinceLast','SubmissionNumber','NumberOfVideoPlays', 'NumberOfThreadViews', 'DurationOfVideoActivity', 'AverageVideoTimeDiffs')
  
  #-------------For tuneGrid-----------------#
  tune<-max(ceiling(0.3*length(fs)),floor(sqrt(length(fs))))
  interval<-1
  range<-c((tune-interval):(tune+interval))
  range
  paramGrid <- expand.grid(mtry = range)
  
  ctrl= trainControl(method = 'cv', summaryFunction=twoClassSummary ,classProbs = TRUE)
  model=train(x=db.train[,fs],
               y=db.train$improved,
               method = "rf",
               metric="ROC",
               trControl = ctrl,
               #tuneGrid = paramGrid,
               preProc = c("center", "scale"))
  print(model);   plot(model)  
  
  #svmLinear
  svmFit <- train(x=db.train[,fs],
                  y=db.train$improved,
                  method= "svmLinear",
                  metric ="ROC",
                  trControl=ctrl,
                  tuneLength = 15,
                  preProc= c("center", "scale"))
  print(svmFit);   plot(svmFit) 
  
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
  
  
  