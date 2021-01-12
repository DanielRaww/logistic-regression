rm(list=ls()) 
#Model 1: Qualitative Data Problem, ban
#run a linear regresion named lmModel1 that tests whether
#advertising restrictions on beer on Tv has an effect on Mortality Rate
ban<-read.csv("ban.csv", stringsAsFactors = TRUE)
summary(ban)
str(ban)
ban<-na.omit(ban)
summary(ban)
str(ban)
lmBan<-lm(MortalityRate~AdRestrictionsTVBeer, data=ban)
summary(lmBan) 
###p-value: 5.524e-07, Adjusted R-squared:  0.1574, 
###AdRestrictionsTVBeerno restriction, AdRestrictionsTVBeerpartial restriction, 
#The F-statistic reveals that the overall model is significant as it is less than 0.05
#The Adjusted r-squared shows that the adjusted r-squared only explains about 16% of the variance, which is low.
#in the individual hypothesis, all of them have a p-value that is less than 0.05 except for
#AdRestrictionsTVBeerno data and AdRestrictionsTVBeervoluntary/self-restricted
plot(MortalityRate~AdRestrictionsTVBeer, data=ban) #3 points seem like a concern
plot(lmBan)
#Residuals vs Fitted: No pattern, looks good. Points: 167, 33, and 97 seem like a concern
#Normal Q-Q: Points 163, 33, 97 are not very close to the line
#Residuals vs Leverage: Again, points 97, 33, 162 seem like a concern
lmtest::bptest(lmBan)
#The model is heteroskedastic as the p value is less than 0.05.
#Lets test for outliers and a lograthmic transformation
lmModel1<-lm(log(MortalityRate)~AdRestrictionsTVBeer, data=ban)
summary(lmModel1)
#The F-statistic shows that the logarithmic model is significant as it has a p-value of less than
#0.05.
#Importantly, this model has a significantly less residual standard error of ~0.5 compared to the
#linear model that has a residual standard error of 80.
#The Adjusted R-squared is only ~0.15, explaining only around 15% of the variance.
#In the individual hypothesis, the p value is less than 0.05 for only
#AdRestrictionsTVBeerpartial restriction and AdRestrictionsTVBeerno restriction
plot(log(MortalityRate)~AdRestrictionsTVBeer, data=ban) #1 point seems like a concern at first glance
plot(lmModel1)
#Residuals vs Fitted: looks much better than lmBan. Shows more of a random pattern.163,33,169
#Normal Q-Q: Points are along the line. (169, 163,33)
#Cooks distance suggests 33 and 2 other points are problematic.
lmtest::bptest(lmModel1)
#The model is homoskedastic as it has a p value greater than 0.05.
#Lets see if there are outliers that can improve the model
lmtest::bgtest(lmModel1)
#The p value is greater than 0.05 which means that there is no autocorrelation
library(EnvStats)
serialCorrelationTest(lmModel1)
car::leveragePlots(lmModel1) #169 and 33 seem like a point of concern
plot(lmModel1, which=c(6))
boxplot(lmModel1$residuals)
car::outlierTest(lmModel1) #Bonferroni test thinks that point 169 is a concern
boxplot(lmModel1$residuals)
sum(abs(lmModel1$residuals)/sd(lmModel1$residuals)>3) #0 points are of concern
outlier<-boxplot(lmModel1$residuals, plot=FALSE)$out
outliers<-as.numeric(names(outlier))
outliers #none
hist(lmModel1$residuals) #between -1.5 and 1.5
#There are no outliers since there are no points outside of 3 standard deviations
summary(lmModel1)
#MortalityRate =5.0042-0.18031(NoData)+0.28250(NoRestriction)-0.31901(partialRestriction)-0.11659(voluntary/self-restricted)

#This means that the mortality rate with

library(ISLR)
data(Hitters)
lm2<-lm(Salary~AtBat+Hits+Walks+CRuns+PutOuts+CWalks, data=Hitters)
sum(abs(lm2$residuals)/sd(lm2$residuals)>3)
summary(lm2)



#Model 2 Logistic Regression Problem CovidLR
#Delete NAs from dataset
#Build model where you predict "died" on the basis of the other 12 categorical variables. 
##Don't worry about testing for linearity of the log predictor
#Train the model using 80% of data

#Are there any influential observations in the overall dataset?
#What is your testing error rate? 
#How does your testing error rate compare to your training error rate?
#Is the full prediction model with all variables any better at predicting death from covid
##than a competing model only using the individual additive effects of gender, pneumonia, and asthma
#Give an interpretation of what the final model means.

rm(list=ls())
library(mlbench)
library(tidyverse)
covid<-read.csv("CovidLR.csv", stringsAsFactors = TRUE)
summary(covid)
str(covid)
covid<-na.omit(covid)
summary(covid)
str(covid)
#numericalData<-select_if(covid, is.numeric)
model<-glm(died~.,family=binomial, data = covid)
summary(model)
library(caret)
divideData<-createDataPartition(covid$died, p=0.8, list=FALSE)
train<-covid[divideData, ]
test<-covid[-divideData, ]
model<-glm(died~.,family=binomial, data=train)
probs<-predict(model, train, type ="response")
pred<-ifelse(probs>0.5, "Yes", "No")
mean(pred==train$died) #training accuracy rate ~ 93% which is good. [1] 0.9382942
mean(pred!=train$died) #training error rate ~6% which is good [1] 0.06170576
table(pred,train$died)
(1420+63)/24001
#[1] 0.06178909
probs<-predict(model, test, type="response")
pred<-ifelse(probs>0.5, "Yes", "No")
mean(pred==test$died) #testing accuracy rate ~ 93% which is good [1] 0.9388231. Slighty higher
mean(pred!=test$died) #testing error rate ~6% which is good [1] 0.06117686
table(pred,test$died)
model<-glm(died~., data=train, family=binomial)
summary(model)
modelT<-glm(died~.-pregnancy-diabetes-copd-inmsupr-hypertension-other_disease-cardiovascular-obesity-smoking, data=train, family=binomial)
#gender,pneumonia,asthma
summary(modelT)
probs<-predict(modelT, test, type="response")
pred<-ifelse(probs>0.5, "Yes", "No")
mean(pred==test$died) #testing accuracy rate ~ 93% (0.9394899) which is good and slightly higher than the original model
mean(pred!=test$died) #testing error rate is ~ 6% (0.06051009) which is good and slightly lower than the original model
probs<-predict(modelT, train, type ="response")
pred<-ifelse(probs>0.5, "Yes", "No")
mean(pred==train$died) #training accuracy rate ~ 93% (0.9393359) which is good and slightly higher than the original model
mean(pred!=train$died) #training error rate ~6% (0.06066414) which is good and lower than the original model
#Our testing error rate is similar to the training error rate, however, the training error rate is higher by 0.00015405
summary(modelT)
#Our final model, the model that only includes: gender, pneumonia, and asthma is a more accurate model
#than the original model that included all variables.  The final accuracy rate is 93.94899% which is
#extremely good, and this means that the model can predict 93.93899% of deaths with gender, pneumonia, and asthma.  