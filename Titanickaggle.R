
#Kaggle Dataset

rm(list = ls())
setwd("D:/R")

# Loading  the Dataset
data <- read.csv("train.csv")
summary(data)



#***********   DATA PREPARATION    ****************

data1 <- data[-c(4,9,10,11)]#removed name ,ticket, fare and cabin column 
View(data1)
summary(data1)
#177 NA's in Age column
data1$Age[is.na(data1$Age)]=28  #Using median to impute missing values 
bx=boxplot(data1$Age)
bx$stats
#head=3 and tail at 54
quantile(data1$Age,seq(0,1,0.01)) #3%=3 and 95% = 54
#Flooring and capping
data1$Age = ifelse(data1$Age>=54,54,data1$Age)
data1$Age = ifelse(data1$Age<=3,3,data1$Age)
boxplot(data1$Age)
library(caret)

#Converting the categorical data into dummy variables
library(dummies)
dummy = dummy.data.frame(data1[,c('Sex','Embarked')],names = NULL)
head(dummy)
dummy<- dummy[-c(3)]  #deleting the column made for 2 missing values 
head(data1)
data1<- cbind(data1,dummy)
data1 <- data1[-c(4,8)]#Deleting the Original variable 'Sex' and 'Embarked'
summary(data1)
data1 = data1[-c(8)]#males removed


#***********      BIVARIATE ANALYSIS    *****************
library(car)
scatterplot(x = data1$Age,y = data1$Survived,col= "salmon")
#survival of age 15-33 is low
scatterplot(data1$Pclass,data1$Survived)
#high class high chances of survivability 

#************    REGRESSION ANALYSIS     ********************
#dividing data into train and test
set.seed(125)
t = sample(1:nrow(data1),0.6*nrow(data1))  # to select randomly sample of 60-40
ttrain = data1[t,]
ttest = data1[-t,]

mod = lm(Survived ~.,data = data1)
summary(mod)
t = vif(mod)
sort(t,decreasing = T)
#since value of VIF  for all factors are less than 5 ,we consider all variables in model

model1 = glm(Survived ~.,data = ttrain,family = "binomial")
summary(model1)
stepmod = step(model1,direction = "both")
formula(stepmod)
summary(stepmod)
#using the above formula to build the final model 
modelfin =  glm(formula = Survived ~ Pclass + Age + SibSp + Sexfemale + EmbarkedS, 
                family = "binomial", data = ttrain)

#************** PREDICTIONS  *******************************
ttrain$score = predict(modelfin, newdata = ttrain,type = "response")
summary(ttrain$score)

#******************  MODEL ANALYSIS ****************************
library(caret)
library(ggplot2)
library(e1071)
library(InformationValue)
prediction = ifelse(ttrain$score>=0.54,1,0)
caret::confusionMatrix(as.factor(prediction),as.factor(ttrain$Survived))
# Accuracy : 0.8127      

# Now lets do same on test data 
ttest$score = predict(modelfin, newdata = ttest,type = "response")
head(ttest$score)

predictiontest = ifelse(ttest$score>= 0.54,1,0)  
caret::confusionMatrix(as.factor(predictiontest),as.factor(ttest$Survived))
#  Accuracy : 0.7927 

plotROC(actuals = ttrain$Survived,predictedScores = as.numeric(fitted(modelfin))) # ROC = 0.8644
ks_plot(actuals = ttrain$Survived,predictedScores = as.numeric(fitted(modelfin)))
ks_stat(actuals = ttrain$Survived,predictedScores = as.numeric(fitted(modelfin))) #  0.5926

#checking the ratio of males and females in the training and test data.
table(data1$Sex)
# female   male 
# 314    577 

table(data1$Sex)/length(data1$Sex)
#  female     male 
# 0.352413 0.647587 

table(ttrain$Sexfemale)/length(ttrain$Sexfemale)
#        0         1 
# 0.6385768 0.3614232 

table(ttest$Sexfemale)/length(ttest$Sexfemale)
#        0         1 
# 0.6610644 0.3389356 

table(data1$SibSp)/length(data1$SibSp)
table(ttrain$SibSp)/length(data1$SibSp)
table(ttest$SibSp)/length(data1$SibSp)

testdata<- read.csv("test.csv")
summary(testdata)
testdata= testdata[-c(3,8,9,10)] #Removing redundant variables like 'Name', 'Ticket','Fare' and 'Cabin'
testdata$Age[is.na(testdata$Age)]=28
testdata$Sexfemale = ifelse(testdata$Sex =="female",1,0)
testdata$EmbarkedS = ifelse(testdata$Embarked=="S",1,0)
testdata=testdata[-c(3,7)] # removing age and embarked


testdata$score = predict(modelfin, newdata = testdata,type = "response")
head(testdata$score)

testdata$score1 = ifelse(testdata$score>= 0.54,1,0)  
summary(testdata$score1)
summary(testdata)


testpredictions = cbind(testdata$PassengerId,testdata$score1)
head(testpredictions)
write.csv(testpredictions,file = "Predictions titanic.csv")
