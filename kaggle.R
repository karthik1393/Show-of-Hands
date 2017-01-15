library(caret)
library(randomForest)
library(rpart)
library(missForest)
library(rpart.plot)
library(tm)
library(mice)
library(gbm)
library(Boruta)
library(corrplot)
library(xgboost)
library(ROCR)
library(caTools)
library(party)
library(dummies)
library(e1071)
train<-read.csv("train2016.csv")
test<-read.csv("test2016.csv")
train[train==""]<-NA
test[test==""]<-NA
str(train)
str(test)
summary(test)
summary(train)
kaggle_glm<-glm(Party~.,data=train,family=binomial)
kaggle_pre<-predict(kaggle_glm,newdata=test,type="response")
kaggle_pre
table(kaggle_pre>0.5)
##data exploration using table commands
table(train$EducationLevel)
table(train$Party)
table(train$Income)
table(train$Gender)
table(train$HouseholdStatus)
###DE using plot and boxplot
plot(train$Party)
boxplot(as.integer(train$Income))
train_new<-train[,!(names(train) %in% c("USER_ID","Party"))]
str(train_new)
test_new<-test[,!(names(test) %in% "USER_ID")]
str(test_new)
##Imputation using miss forest
train_1<-missForest(train_new,maxiter = 3,ntree=100)
test_1<-missForest(test_new,maxiter = 3,ntree=100)
str(train_1)
str(test_1)
train_1$ximp<-as.data.frame(train_1$ximp)
train_imp<-train_1$ximp
table(is.na(train_imp))
train_imp[5560,]

train_imp$USER_ID<-train$USER_ID
train_imp$Party<-train$Party
str(train_imp)
summary(train_imp)
test_1$ximp<-as.data.frame(test_1$ximp)
test_imp<-test_1$ximp
str(test_imp)
test_imp$USER_ID<-test$USER_ID
train_imp$age<-as.numeric(train_imp$YOB >=1955 & train_imp$YOB <=1997)
train_imp$age<-as.factor(train_imp$age)
train_imp<-subset(train_imp,age==1)
str(train_imp)
table(train_imp$age)
str(train_imp$YOB)
write.csv(train_imp,"train_imp.csv")
##Feature selection using boruta
train_bor<-Boruta(Party~.-USER_ID,data=train_imp,doTrace=2)
print(train_bor)
train_final_b<-TentativeRoughFix(train_bor)
print(train_final_b)
getSelectedAttributes(train_final_b)
##train with glm
train_lm<-glm(Party~Q109244+Q115611+Q98197+HouseholdStatus+EducationLevel+age,data=train_imp,family=binomial)
summary(train_lm)
train_pre<-predict(train_lm,type="response")
table(train_imp$Party,  train_pre>0.5)
train_predi<-prediction(train_pre,train_imp$Party)
train_roc<-performance(train_predi,'tpr','fpr')
plot(train_roc,colorize = TRUE, text.adj = c(-0.2,1.9))
as.numeric(performance(train_predi,"auc")@y.values)
(1679+1700)/nrow(train_imp)

##another model of glm
train_lm1<-glm(Party~Q109244+Q101163+Q116881+Q124122+Q114961,data=train_imp,family=binomial)
summary(train_lm1)
train_pre1<-predict(train_lm1,newdata=train_imp,type="response")
table(train_imp$Party,  train_pre1>0.5)
(1241+1696)/nrow(train_imp)

# cv
set.seed(1000)
cont<-trainControl(method = "cv",number = 10)
gr<-expand.grid(.cp=seq(0,1,0.001))
train(Party~Q109244+Q115611+Q98197+YOB+Q101163+Q116881+Q114961,data=train_imp,method="rpart",trControl=cont,tuneGrid=gr)
##rpart
?rpart
train_cart<-rpart(Party~Q109244+Q115611+Q98197+Q101163+Q98869+YOB,data=train_imp,method="class",minbucket=5,cp=0.025)
prp(train_cart)
cart_pre<-predict(train_cart,data=train_imp)
cart_pre
cart_pre1<-prediction(cart_pre[,2],train_imp$Party)
as.numeric(performance(cart_pre1,'auc')@y.values)
table(train_imp$Party,cart_pre)
prune(train_cart,cp=0.025)
str(train_imp$Party)
##

##RF
?randomForest
set.seed(100)
train_rf<-randomForest(Party~Q109244+Q115611+Q113181+Q98197+HouseholdStatus+Income+USER_ID+Q104996+Q98578+YOB,data=train_imp,mtry=2,ntree=500)
print(train_rf)
varImpPlot(train_rf)
sample_pre<-predict(train_rf)
sample_pre
sample_auc<-predictions(train_imp$Party,sample_pre)
sort(head(train_rf$importance),decreasing = TRUE)
MDSplot(train_rf)
rf_pre<-predict(train_rf,newdata=test_imp)
table(train_imp$Party,rf_pre)
write.csv(rf_pre,file = "rfnew1.csv")
##cv for RF

set.seed(101)
cont2<-trainControl(method = "cv",number = 10)
gr2<-expand.grid(.mtry=seq(2,7,0.1))
fit2<-train(Party~Q109244+Q115611+Q113181+Q98197+HouseholdStatus+Income+USER_ID+Q104996+Q98578+age,data=train_imp,method="rf",trControl=cont2,tuneGrid=gr2)
 print(fit2)

###tuning RF
x<-train_imp[,c("Q109244","Q115611","Q98197","HouseholdStatus","EducationLevel","YOB")]
y<-train_imp[,108]
?tuneRF
tune_rf<-tuneRF(x,y,trace=TRUE,plot=TRUE,improve=0.10,ntreeTry = 501,doBest=FALSE,mtryStart = seq(1,5,0.01))
plot(tune_rf,type="l")

##GBM
set.seed(100)
cont1<-trainControl(method = "cv",number = 10)
gr1<-expand.grid(interaction.depth = 2,
                 n.trees = 201,
                 shrinkage = 0.01,
                 n.minobsinnode = 10)
fit<-train(Party~Q109244+Q115611+Q113181+Q98197+HouseholdStatus+Income+USER_ID+Q104996+Q98578,data=train_imp,method="gbm",trControl=cont1,verbose=FALSE,tuneGrid=gr1,distribution = 'bernoulli')
print(fit)
plot(fit)
plot(varImp(fit))
gbm_pre<-predict(fit,data=test_imp)
table(train_imp$Party,gbm_pre)
gbm_pre1<-predict(fit,newdata = test_imp)
gbm_pre1
write.csv(gbm_pre1,file = "sub1.csv")
##XGBOOST
# ?xgboost
# ?sparse.model.matrix
train_only<-train_imp
str(train_only)
train_only$USER_ID<-NULL
train_xg<-sparse.model.matrix(Party~.-1,data=train_only)
head(train_xg)
test_xg<-sparse.model.matrix(USER_ID~.-1,test_imp)
y<-train_only[,"Party"]=="Democrat"
xg<-xgboost(data=train_xg,label=y,max.eta=0.1,max_depth = 10,nround=40,subsample = 0.5,colsample_bytree = 0.5,seed = 1,eval_metric = "merror",objective = "multi:softprob",num_class = 2,nthread = 3,prediction=T)
print(xg)
test_2<-test_imp
predictions<-predict(xg, newdata=test_xg)
table(as.numeric(predictions>0.5))
pred<-as.data.frame(predictions)
table(predictions>0.5)
##Feature imp
model<-xgb.dump(xg,with_stats = T)
model[1:10]
names<-dimnames(train_xg)
names
imp<-xgb.importance(feature_names = colnames(train_xg),model=xg)
xgb.plot.importance(imp[1:10,])
imp[225]
test<-chisq.test(train_imp$Party,y)
test
##SVM
train_svm<-svm(Party~Q109244+Q115611+Q113181+Q98197+HouseholdStatus+Income+USER_ID+Q104996+Q98578,data=train_imp)
summary(train_svm)
svm_predict<-predict(train_svm,newdata=test_imp)
write.csv(svm_predict,"svm.csv")
table(train_imp$Party,svm_predict)

