
#---------------------------------#
#------ Logistic Regressions -----#
#---------------------------------#

#Overview: The following Logistic Regression models alternate X variables included in the model to test for best acuracy. 
library(scales)

###Regression 1: LR using all variables
#--- Data Prep ---#

Kickstarter <- read.csv("KickstarterData.csv", header = T)
row.names(Kickstarter) <- Kickstarter$ID
Kickstarter.df <- Kickstarter[,-c(1:3,5:10,14,15,19)] 

#--- Training & Validation Sets ---#
set.seed(100)
train.index <- sample(rownames(Kickstarter.df), dim(Kickstarter.df) [1]*.6)
train.df<- Kickstarter.df[train.index,]
valid.index <-setdiff(rownames(Kickstarter.df), train.index) 
valid.df <-Kickstarter.df[valid.index,]

#--- Run Logistic Regression ---#
logit.reg <- glm(Status~., family = "binomial", data = train.df)
summary(logit.reg)

#--- Run Prediction Model ---#
logit.reg.pred<- predict(logit.reg, valid.df[,-2], type = "response")
logit.reg.pred

#--- Test the Model ---#
StatusActual<- valid.df$Status
StatusPredict<-rep("failed", dim(valid.df)[1])
StatusPredict

StatusPredict[logit.reg.pred>0.5]="successful"
table(StatusActual,StatusPredict)
mean(StatusActual != StatusPredict)

###Regression 2: LR using all variables, except "Country.US.vs.not"
#--- Run Logistic Regression ---#
logit1.reg <- glm(Status~ Main_category_condensed + backers + usd_pledged_real + usd_goal_real + proj_length, family = "binomial", data = train.df)
summary(logit1.reg)

#--- Run Prediction Model ---#
logit1.reg.pred<- predict(logit1.reg, valid.df[,-2], type = "response")
logit1.reg.pred

#--- Test the Model ---#
StatusActual1<- valid.df$Status
StatusPredict1<-rep("failed", dim(valid.df)[1])

StatusPredict1[logit1.reg.pred > 0.5]="successful"
table(StatusActual1,StatusPredict1)
mean(StatusActual1 != StatusPredict1)

###Regression 3: LR using all variables, except "backers" and "usd_pledged"
#--- Run Logistic Regression ---#
logit2.reg <- glm(Status~ Main_category_condensed + Country.US.vs.not  + usd_goal_real + proj_length, family = "binomial", data = train.df)
summary(logit2.reg)

#--- Run Prediction Model ---#
logit2.reg.pred<- predict(logit2.reg, valid.df[,-2], type = "response")
logit2.reg.pred

#--- Test the Model ---#
StatusActual2<- valid.df$Status
StatusPredict2<-rep("failed", dim(valid.df)[1])

StatusPredict2[logit2.reg.pred > 0.5]="successful"
table(StatusActual2,StatusPredict2)
mean(StatusActual2 != StatusPredict2)

###Regression 4: LR using all variables, except the nonsignificant variable: "project_length"
#--- Run Logistic Regression ---#
logit.reg.1 <- glm(Status~ Main_category_condensed + Country.US.vs.not + backers + usd_pledged_real + usd_goal_real, family = "binomial", data = train.df)
summary(logit.reg.1)

#--- Run Prediction Model ---#
logit.reg.pred.1<- predict(logit.reg.1, valid.df[,-2], type = "response")
logit.reg.pred.1

#--- Test the Model ---#
StatusActual3<- valid.df$Status
StatusPredict3<-rep("failed", dim(valid.df)[1])

StatusPredict3[logit.reg.pred.1 > 0.5]="successful"
table(StatusActual3,StatusPredict3)
mean(StatusActual3 != StatusPredict3)

###Regression 5: LR using all variables, except the nonsignificant variables: "project_length" and "Country.US.vs.not"
#--- Run Logistic Regression ---#
logit.reg.2 <- glm(Status~ Main_category_condensed + backers + usd_pledged_real + usd_goal_real, family = "binomial", data = train.df)
summary(logit.reg.2)

#--- Run Prediction Model ---#
logit.reg.pred.2<- predict(logit.reg.2, valid.df[,-2], type = "response")
logit.reg.pred.2

#--- Test the Model ---#
StatusActual4<- valid.df$Status
StatusPredict4<-rep("failed", dim(valid.df)[1])

StatusPredict4[logit.reg.pred.2 > 0.5]="successful"
table(StatusActual4,StatusPredict4)
mean(StatusActual4 != StatusPredict4) #Prediction Accuracy is 99.87%
percent(1- mean(StatusActual4 != StatusPredict4), accuracy = .001)

#--- Conclusion ---#
#Regression 5 is the best model only using the significant X variables with a  misclassification rate of .1334137%


#----------------------------------#
#--Logistic Regression Prediction--#
#----------------------------------#

#Overview: The following Logistic Regression model uses Regression 5 from the previous section to predict the success or failure of the "live" data. The live data includes kickstarter projects that are still in progress of taking off. 
library(magrittr)
library(dplyr)
library(scales)

#--- Data Prep ---#
Live <- read.csv("Live Data Final.csv", header = T)
row.names(Live) <- Live$name
Live <- Live[,-c(1)]

#--- Run Prediction Model ---#
logit.reg.pred.live <- predict(logit.reg.2,Live[,-5], type = "response")
logit.reg.pred.live
head(logit.reg.pred.live)

StatusPredictLive <-rep("failed", dim(Live)[1])
StatusPredictLive[logit.reg.pred.live > .5] = "successful"

head(StatusPredictLive)
StatusPredictLive
summary(StatusPredictLive)

#--- Add Column to Dataset ---#
LivePredict <- data.frame(Live, StatusPredictLive)
LivePredict$Main_category_condensed <-factor(LivePredict$Main_category_condensed, levels= c(1,2,3), labels=c("Creative","Media","Tech"))

#--- Drawing Insights ---#
LivePredict %>% group_by(Main_category_condensed) %>% summarise(n())
LivePredict %>% group_by(Main_category_condensed, StatusPredictLive) %>% summarise(CountSF = n())
#with Main_category_condensed
LivePredict %>% group_by(main_category, StatusPredictLive) %>% summarise(n())
#with main_category

#Next Step: Potential to make graphs out of data results 