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
library(glmnet)
de<-read.csv("train2016.csv")
str(de)
train_new<-de[,c(-1:-6)]
str(train_new)
lg<-glm(Party~Q109244+Q120379,data=train_new,family=binomial)
summary(lg)
##data exploration
table(de$Party)
plot(de$Party,de$HouseholdStatus)
ggplot(data=de,aes(fill=de$Party,x=de$HouseholdStatus))
hist(de$YOB)
table(de$Gender)
hist(train_imp$age)
plot(train_imp$Gender,train_imp$Party)
ggplot(data=train_imp,aes(x=HouseholdStatus,y=Party))+geom_point()
plot(train_imp$HouseholdStatus,train_imp$Party)
train_imp[1,]
View(train_imp)
train_new[1,1:6]

##RFE (Recursive feature elimination)
set.seed(13)
control<-rfeControl(functions = rfFuncs,method = "repeatedcv",number=10,repeats=3,verbose = FALSE)
predictors<-names(train_imp)[!names(train_imp)%in% "Party"]
rfe.train<-rfe(train_imp[,predictors],train_imp[,"Party"],rfeControl = control)
rfe.train
plot(rfe.train,type="l",main="imp variables")
varImp(rfe.train)

names(getModelInfo())
getModelInfo(model="svm")
modelLookup(model="ada")
####ADA BOOST
ind<-train_imp[,c("Q109244","Q115611","Q113181","Q98197","HouseholdStatus","Income","USER_ID","Q104996","Q98578","age","Gender")]
set.seed(1001)
ada_control=trainControl(method = "cv",number=10)
ada_grid<-expand.grid(iter=100,maxdepth=3,nu=0.1)
ada<-train(ind,train_imp[,"Party"],method="ada",trControl=ada_control,tuneGrid=ada_grid)
ada
ada_test<-predict(ada,newdata = test_imp)
ada_test
write.csv(ada_test,"ada1.csv")
?train


##LASSO
set.seed(1)
x1<-model.matrix(Party~.,train_imp)[,-1]
dim(x1)
y1<-as.numeric(train_imp$Party)
str(y1)
la<-10^seq(10,-2,length=100)
cv_lasso<-cv.glmnet(x1,y1,alpha=1)
cv_lasso$lambda.min
cv_lasso$lambda.1se
lasso<-glmnet(x1,y1,alpha=1,lambda = la)
plot(lasso)
lasso_pre<-predict(lasso,type="coefficients",s=cv_lasso$lambda.min)
plot(lasso_pre)
lasso_pre
lasso_pre[lasso_pre!=0]

###
plot(train_imp$HouseholdStatus,train_imp$Party)
boxplot(train_imp$Income~train_imp$Party)
plot(train_imp$q109244,train_imp$Party)
train_imp$Q109244
plot(train_imp$Q109244,train_imp$Income)
ggplot(train_imp,aes(x=Income,y=Gender))+geom_point(color="red")
