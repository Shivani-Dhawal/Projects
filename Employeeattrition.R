data<-read.csv("D:/R/HR dataset.csv")

#*********DATA PREPARATION************************

#removing role and salary as we are using role_code and salary_code
data1<-data[,-c(9,10)]

#converting role_code and salary-code into factors
data1$role_code=as.factor(data1$role_code)
data1$salary_code=as.factor(data1$salary_code)

#view the data
View(data1)
summary(data1)

#**********Classification-EDA**************************
#boxplot b/w satisfaction level and whether employees left
boxplot(data1$satisfaction_level~ data1$left,main="Boxplot for satisfaction level")
#people who left company have low satisfaction levels

#boxplot b/w avg monthly hrs spent and whether employees left
boxplot(data1$average_montly_hours~ data1$left,main="Boxplot for Avg Monthly Hours")
#people who left had been putiing more hours at work

#boxplot b/w Time spent with company and whether employees left
boxplot(data1$time_spend_company~ data1$left,main="Boxplot for Time spent with the company")
#people with longer term seem to be leaving the company

#Mosiac plot to check relation b/t time spent with company and whether the employee left
mosaicplot(data1$left~data1$salary_code,color= "sky blue")
#majority of the people leaving are with salary code=1

#Mosiac plot to check relation b/t Promotion in last 5 years and whether the employee left
mosaicplot(data1$left~data1$promotion_last_5years,color= "sky blue ")
#majority of the people leaving did not get any promotionin the last 5 years 

#Correlation among variables
library(corrplot)
cor(data1[,1:8])
corrplot(cor(data1[,1:8]),method = "shade")

#*********STEP-3*********************
#creating data set for model building
#creating train and test data
library(caret)
set.seed(955)
splitIndex<-caret::createDataPartition(data1$left,p=0.7,list = F,times=1)
trainSplit<-data1[splitIndex,]
testSplit<-data1[-splitIndex,]
print(table(trainSplit$left))

#   0    1 
#8019 2481 
print(table(testSplit$left))
# 0    1 
#3409 1090 
#check for the event rate
prop.table(table(trainSplit$left))
#       0         1 
#0.7637143 0.2362857 

prop.table(table(testSplit$left))
#   0         1 
#0.7577239 0.2422761 
#0's and 1's are in equal proportion so we proceed forward

#********STEP-5**********************

#Classification Model
#************DECISION TREE*******************
library(rpart)
#decision tree using rpart algorithm
fit=rpart(left~.,data= trainSplit,method = "class")
fit
#plotting the decision tree
library(rpart.plot)
rpart.plot(fit,tweak =1.4)
#tweak and cex is used to change the size of the diagram
#there are total 10 leaf nodes (or decision nodes) which get created
#each node shows:
#the predicted class (left and retained )
#the predicted prob of left
#the % of obs in the node
fit=rpart(left~.,data= trainSplit,method = "class",control = rpart.control(minsplit = 500,cp=0.01))
rpart.plot(fit,tweak =1)
fit=rpart(left~.,data= trainSplit,method = "class",control = rpart.control(minsplit = 30,cp=0.01))
rpart.plot(fit,tweak =1.5)
#predicting the cp values
plotcp(fit)
#(complexity parameter is used to control the size of decision tree and to select the optimal tree size).
#checking confusion matrix train data.
library(caret)
predtr<-predict(fit,trainSplit,type = "class")
confusionMatrix(predtr,as.factor(trainSplit$left))
#checking confusion matrix on train data
predtest<-predict(fit,testSplit,type = "class")
confusionMatrix(predtest,as.factor(testSplit$left))


##************RANDOM FOREST MODEL******************
library(randomForest)
modelrf<-randomForest(as.factor(left)~., data= trainSplit,do.trace=T)
#do.trace=T will show all the trees and if do.trace=10 then output will be shown in intervals of 10
modelrf
#Out-of-bag estimate for the generalizatiion error is the error rate of the out-of-bag classifier 
#on the training set.

#prediction and model evaluation using confusion matrix
predrf_tr<-predict(modelrf,trainSplit)#train data
predrf_test<-predict(modelrf,testSplit)#test data

confusionMatrix(as.factor(predrf_tr),as.factor(trainSplit$left))#train data
# Accuracy : 0.9998    
confusionMatrix(as.factor(predrf_test),as.factor(testSplit$left))#train data
#Accuracy : 0.9927 
#similar performance on train and test data and hence we assure stability of our random forest model

#checking variable importance in random forest
importance(modelrf)
varImpPlot(modelrf)
#the varibale importance plot displays a plot with variables sorted by MeanDecreaseGini

#**************************
#Prediction and model evaluation using confusion matrix
# decision tree ROC
library(pROC)
auc1<- roc(as.numeric(testSplit$left),as.numeric(predtest))
plot(auc1, col= "blue", main = paste("AUC:",round(auc1$auc[[1]],3)))

#Random forest ROC
aucrf<-  roc(as.numeric(testSplit$left),as.numeric(predrf_test),ci=T)
plot(aucrf, ylim = c(0,1), print.threa=T,main = paste("random forest AUC: ",round(aucrf$auc[[1]],3)),col = "blue")

#Comparing both ROC curves
plot(aucrf ,ylim =  c(0,1),main =paste("ROC Comparison :RF(blue),DT(Black))" ),col = "blue")
par(new = T)
plot(auc1)
par(new = T)


#*********************NAIVE BAYES ALOGORITHM**********************8
#naives Bayes e1071
library(e1071)
modelnb<- naiveBayes(as.factor(left)~. , data= trainSplit)
modelnb
#performance of naive bayes using confusion matrix
prednd_tr<-predict(modelnb,trainSplit)# train data
prednd_test<-predict(modelnb,testSplit)# test data


library(caret)
confusionMatrix(as.factor(prednd_tr),as.factor(trainSplit$left))#train data
#  Accuracy : 0.8026
confusionMatrix(as.factor(prednd_test),as.factor(testSplit$left)) # test data
#  Accuracy : 0.8088 
   

#**************KNN (K NEAREST NEIGHBORS) ALGORITHM************************

#data preparation for KNN ALGORITHM
library(dummies)

#Creating dummy varibales for factor variables
dummy_df = dummy.data.frame(data[,c('role','salary')]) 
dummy_df

hr_data2=data
hr_data2=cbind.data.frame(hr_data2,dummy_df)

#removing  role code and salary code since we have created dummy variables
hr_data2=hr_data2[,!(names(hr_data2) %in% c('role_code','salary_code','role','salary'))]

#converting variables to numeric datatype
hr_data2$Work_accident=as.numeric(hr_data2$Work_accident)
hr_data2$promotion_last_5years=as.numeric(hr_data2$promotion_last_5years)

#data preparation for KNN algorithm 
# scale the var and check their final structure
X = hr_data2[,!(names(hr_data2) %in% c('left'))] #removing dependant part
hr_data2_scaled=as.data.frame(scale(X))
#scaled by Standard Normal Variate [Z=(Xbar -Mu)/sigma]
#here it is min/max scalingbecause of skewed distribution had it been normal distribution we would have used 3sd scaling
#Xsc=(X???Xmin)/(Xmax???Xmin).
str(hr_data2_scaled)
 
#Splitting the data for the model building
hr_train<-hr_data2_scaled[splitIndex,]  
hr_test<-hr_data2_scaled[-splitIndex,]  
View(hr_data2)
#y's
hr_train_lables<-hr_data2[splitIndex,'left']
hr_test_lables<-hr_data2[-splitIndex,'left']

#Applying KNN Algorithm on the data set
library(class)
library(gmodels)

test_pred_1<-knn(train = hr_train, test = hr_test, cl =hr_train_lables, k = 1)
CrossTable(x=hr_test_lables,y=test_pred_1, prop.chisq = F)
# for k=1 compute ,
#accuracy =(TP+TN)/TOTAL  =(3340+1002)/4499   =0.9651034  =96.51%
#Contingency table

#Thumb rule to decide on k for KNN is sqrt(n)/2
k = sqrt(nrow(hr_train))/2
k # = 51.23475

test_pred_rule<- knn(train = hr_train,test = hr_test,cl = hr_train_lables,k = k)
CrossTable(x = hr_test_lables,y = test_pred_rule,prop.chisq = F)
#Accuracy = (3278+744)/4499 = 0.8939764

#Another method to determine the k for KNN
set.seed(598)
library(caret)
ct<- trainControl(method = "repeatedcv",repeats = 3)
fit<- train(as.factor(left) ~., data = hr_data2, method = "knn",trControl = ct,preProcess = c("center","scale"),tuneLength = 20)
fit
#Accuracy was used to select the optimal model using the largest value.
#The final value used for the model was k = 7.
#Accuracy= 0.9423961 at k=7.

#Checking accuracy of the model with k= 7
test_pred_7<- knn(train = hr_train,test = hr_test,cl = hr_train_lables,k=7)
CrossTable(x = hr_test_lables,y = test_pred_7,prop.chisq = F)
#accuracy= (3304+922)/4499 = 0.9393198

#or alternatively we can use this below command
confusionMatrix(as.factor(hr_test_lables),as.factor(test_pred_7))
# Accuracy : 0.9393 


#data preparation for KNN algorithm 
# scale the var and check their final structure
X = hr_data2[,!(names(hr_data2) %in% c('left'))] #removing dependant part
hr_data2_scaled=as.data.frame(scale(X))
#scaled by Standard Normal Variate [Z=(Xbar -Mu)/sigma]
#here it is min/max scalingbecause of skewed distribution had it been normal distribution we would have used 3sd scaling
#Xsc=(X???Xmin)/(Xmax???Xmin).
str(hr_data2_scaled)

#Splitting the data for the model building
hr_train<-hr_data2_scaled[splitIndex,]  
hr_test<-hr_data2_scaled[-splitIndex,]  
View(hr_data2)
#y's
hr_train_lables<-hr_data2[splitIndex,'left']
hr_test_lables<-hr_data2[-splitIndex,'left']


#resampling by dividing into groups and performing cross validation  
fit<-train(left~.,data = hr_data2, method="knn",trControl = ct, preProcess = c("center","scale"),tuneLength=20);fit
#considered best method among all 

#Checking accuracy of the model with k=5
test_pred_5<-knn(train = hr_train ,test = hr_test, cl = hr_train_lables , k=5 )
CrossTable(x=hr_test_lables,y=test_pred_5,prop.chisq = F)
#accuracy = (3294+925)/4499 = 93.77%

#or alternatively we can use this below command
confusionMatrix(as.factor(hr_test_lables),test_pred_5)
#                           Accuracy : 0.9378   



