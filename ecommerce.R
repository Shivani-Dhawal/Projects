# MARKETING ANALYTICS for e-commerce

rm(list = ls())
data<- read.csv("D:/R/Regression.csv")
View(data)
summary(data) #to find NA's in the data;here there are 13 NA's in age variable.
hist(data$Age)  #From Histogram ,we can see that data is normally distributed
data$Age[is.na(data$Age)]=39#to replace missing values with mean 39
summary(data)

#Creating dummy variables, 4 of our variables is categorical viz. Job.Type, Marital.Status,Education and Metro.City
data$Job.Type_employed<-as.numeric(data$Job.Type=="Employed")
data$Job.Type_retired<-as.numeric(data$Job.Type=="Retired")
data$Job.Type_unemployed<-as.numeric(data$Job.Type=="Unemployed")
data$Married_y<-as.numeric(data$Marital.Status=="Yes")
data$Education_secondary<-as.numeric(data$Education=="Secondry")
data$Education_gra<-as.numeric(data$Education=="Graduate")
data$Metro_y<-as.numeric(data$Metro.City=="Yes")
head(data)#checking the dummy variables
summary(data)#checking if there is any dummy variable where mean =0 i.e, data is same
final_data<-data[-c(2,3,4,5)]#removing the categorical variables column(2,3,4,5)
par(mfrow=c(1,2))#to partition the plots area
bx =boxplot(final_data$Age) 

#********Univariate analysis*************
quantile(final_data$Age,seq(0,1,0.02))#to check the distribution of var age between 0-1 by gap of 2%
bx$stats
#since the 98th percentile is 57,we cap the outliers with the same value
final_data$Age<-ifelse(final_data$Age>60,57,final_data$Age)
#box plot to again check the outliers
boxplot(final_data$Age)
#now checking the outlier for our other variable - sign in since days
bx1<-boxplot(final_data$Signed.in.since.Days.)
#outlier treatment for variable "signed.in.since.days.
quantile(final_data$Signed.in.since.Days.,seq(0,1,0.02))
bx1$stats
final_data$Signed.in.since.Days.<-ifelse(final_data$Signed.in.since.Days.<45,49,final_data$Signed.in.since.Days.)
boxplot(final_data$Signed.in.since.Days.)
hist(final_data$Purchase.made,main = "Dependent")
boxplot(final_data$Purchase.made)


#*********Bivariate Analysis***********
#to check relationship between dependent and independant var(age and purchase made)
library(car)
scatterplot(final_data$Age,final_data$Purchase.made)
cor(final_data$Age,final_data$Purchase.made)#r=0.0607536

#sign.in.days vs purchase made
scatterplot(final_data$Signed.in.since.Days.,final_data$Purchase.made)
cor(final_data$Signed.in.since.Days.,final_data$Purchase.made)#r=0.9000488


#************REGRESSION ANALYSIS*******************
#since we are done with the EDA, lets check the co-relation
cor(final_data)
model1<-lm(Purchase.made~.,data=final_data)
summary(model1)#the variables whose p values>0.05 is of no use,we'll delete them but first check for multicollinearity

#checking the multi-collinearity

vif(model1)#variation inflation factor
#graduation was highly colinear with the other variables ,lets verfiy once again using step function

step(model1)#the model with least AIC is better which is last model as step fuction arranges the model in decreasing order
model2<-lm(formula = Purchase.made ~ Age + Signed.in.since.Days. + Job.Type_retired +Job.Type_unemployed + Married_y + Education_gra + Metro_y, data = final_data)#usinf the last model
vif(model2)#to check multicollinearity,since all values are <5 ,there is absence of multicollinearity 
summary(model2)
#age factor is removed from the above model
model3<-lm(formula = Purchase.made ~ Signed.in.since.Days. + Job.Type_retired +Job.Type_unemployed + Married_y + Education_gra + Metro_y, data = final_data)
summary(model3)
#Age and retired removed
model4<-lm(formula = Purchase.made ~ Signed.in.since.Days.  +Job.Type_unemployed + Married_y + Education_gra + Metro_y, data = final_data)
summary(model4)

#we've made the model now to check its reliability we'll test the model 
#************Model Evaluation*****************
library(lmtest)
par(mfrow=c(2,2))
plot(model4)
#quantile values
quantile(final_data$Purchase.made,seq(0,1,0.02))
#capping 4% and 96% 
final_data_new=final_data[(final_data$Purchase.made>=510 & final_data$Purchase.made<=13500),]
#lets re-run the model on this filtered data
mod2<-lm(Purchase.made~Signed.in.since.Days.+ Married_y + Education_gra + Metro_y 
         + Job.Type_unemployed,data = final_data_new)
summary(mod2)
#p value of jobtype unemployed is >0.05 i.e, we have to create a new model removing this variable
#making new model
model5<-lm(formula = Purchase.made ~ Signed.in.since.Days. + Married_y + Education_gra + Metro_y, data = final_data_new)
summary(model5)

#which all variables who are contributing in purchase made ?
#signed in since days, married -yes,educatio- graduation and metro city-yes

#1) Autocorrelation using Durbin Watson test
library(car)
durbinWatsonTest(model5)
#in case of DURBIN WATSON test , the d-w statistics is considered good if it is <2. In this case it is <2.

#2) histogram : normality of errors
hist(residuals(model5))#tends to be normal

#3) now checking HOMOSCEDASTICITY
plot(final_data_new$Purchase.made,residuals(model5))
#scatter plot also shows errors are somewhat normally distributed.
library(predictmeans)

cooked=CookD(model5)
#ideally high cook's distance observation(above 0.0125(4/n) level in y-axis) should be removed from data and re-modelled.
new_data1<-final_data_new[-c(34,51,220),]#as we had already trimmed first 13 observations and the numbering is starting from 14 so we have to subract 13 from all the values, outliers were 47,64,233 but we will enter 34,51,220
#creating new model with this dataset 
model7<-lm( Purchase.made ~ Signed.in.since.Days. + Married_y + Education_gra + Metro_y, data = new_data1)
cooked=CookD(model7)#since all obs are less than 0.02 level there are no outliers present in the model

#next question is to identify the valueble customeers from the new list of 200 obs 
#so that sales can focus more on top30% of valuable customers.
#predicting the values on new data
data2<-read.csv("D:/R/MyData.csv")
data2$Purchase.made<-predict.lm(model7,data2)#making changes in this file and filling the empty column of purchase made 
write.csv(data2,file = "D:/R/ecommerce.csv")#making new csv file using this data ,sending this data outside r into file 

#sorting top 30% customers
data3<-data2[order(-data2$Purchase.made),]
write.csv(data3,file = "D:/R/ecommerce.csv")
data4<-head(data3,60)#top 30%=60 obs are inserted in a file
write.csv(data4,file = "D:/R/ecommerce.csv")

#**********calculating the predictive power of the model************
#function that returns root mean squared error
rmse<-function(error)
{   sqrt(mean(error^2))
}
#function that returns Mean Absolute Error
mae<-function(error)
  {   mean(abs(error))
}
#calculate error
actual<-final_data_new$Purchase.made
finalnew<-final_data_new[,-3]
predicted1<-predict(model7,finalnew)
error<-actual-predicted1
#Example of invocation of functions
rmse(error) #=1312.989
mae(error)  #=1068.868
#Percentage error
MAPE<-mae(error)/mean(actual)
MAPE     #0.145979


