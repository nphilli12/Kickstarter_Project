#if used, this will go behind all the classification and log-regressions so the libraries and datasets are all loaded properly. 

#---------------------------------#
#-ClassTrees & RandFor Categories-#
#---------------------------------#

#Overview: The following Classification Trees and Random Forests depict the specific rules associated with each of the 3 "Main_category_condensed" groups.

###Category 1: Creative
library(magrittr)
library(dplyr)
library(tibble)
#--- Subset Prep ---#
Creative <- kick.df %>% rownames_to_column('id') %>%
  select(Main_category_condensed, backers, Country.US.vs.not, usd_pledged_real, usd_goal_real, proj_length, id) %>% filter(Main_category_condensed %in% c("Creative")) %>% column_to_rownames('id')
Creative <- Creative[,-c(1)]

#--- Training & Validation Sets ---#
set.seed(100)
train.i.c <- sample(rownames(Creative), dim(Creative)[1]*.6)
valid.i.c <- setdiff(rownames(Creative), train.i.c)
train.c <- kick.df[train.i.c, ]
valid.c <- kick.df[valid.i.c, ]

#--- r part ---#
set.seed(100)
ks.ct.c <- rpart(Status~ ., data=train.c, method = "class", cp = .001, minsplit = 5, xval = 5)
printcp(ks.ct.c)

#---- Tree ----# 
set.seed(100)
class.tree.c <- rpart(Status~., data = train.c, method = "class")

prp(class.tree.c, type = 1, extra = 1, under = TRUE, split.font = 4, varlen = -20, main = "Creative Kickstarter Classification Tree")
fancyRpartPlot(class.tree.c, main = "Creative Kickstarter Classification Tree")

#--- test the model ---#
ctp.pred.train.c <- predict(class.tree.c, train.c, type = "class")
ctp.pred.train.c
table(ctp.pred.train.c, train.c$Status)
mean(train.c$Status != ctp.pred.train.c) #Training Accuracy Rate: 95.94%
percent(1- mean(train.c$Status != ctp.pred.train.c), accuracy = .001)

ctp.pred.valid.c <- predict(class.tree.c, valid.c, type = "class")
ctp.pred.valid.c 
table(ctp.pred.valid.c, valid.c$Status)
mean(valid.c$Status != ctp.pred.valid.c) #Valid Accuracy Rate: 96.69%
percent(1-mean(valid.c$Status != ctp.pred.valid.c), accuracy = .001) 

#--- Random Forest  ---# 
rf.c <- randomForest(Status ~ ., data = train.c, ntree = 500,  mtry = 4, nodesize = 5, importance = TRUE)

#--- Variable Importance Plot ---#  
varImpPlot(rf.c, type = 1, main = "Creative Variable Importance Plot")
pruned.ct.pred.valid.c <- predict(rf.c, valid.c, type = "class")
table(pruned.ct.pred.valid.c, valid.c$Status)
mean(pruned.ct.pred.valid.c!= valid.c$Status) #Random Forest Valid Accuracy Rate: 99.82%
percent(1-mean(pruned.ct.pred.valid.c!= valid.c$Status), accuracy = .001)



###Category 2: Media
#--- Subset Prep ---#
Media <- kick.df %>% rownames_to_column('id') %>%
  select(Main_category_condensed, backers, Country.US.vs.not, usd_pledged_real, usd_goal_real, 
         proj_length, id) %>% filter(Main_category_condensed %in% c("Media")) %>%
  column_to_rownames('id')
Media <- Media[,-c(1)]

#--- Training & Validation Sets ---#
set.seed(100)
train.i.m <- sample(rownames(Media), dim(Media)[1]*.6)
valid.i.m <- setdiff(rownames(Media), train.i.m)
train.m <- kick.df[train.i.m, ]
valid.m <- kick.df[valid.i.m, ]

#--- r part ---#
set.seed(100)
ks.ct.m <- rpart(Status~ ., data=train.m, method = "class", cp = .001, minsplit = 5, xval = 5)
printcp(ks.ct.m)

#---- Tree ----# 
set.seed(100)
class.tree.m <- rpart(Status~., data = train.m, method = "class")

prp(class.tree.m, type = 1, extra = 1, under = TRUE, split.font = 4, varlen = -20, main = "Media Kickstarter Classification Tree")
fancyRpartPlot(class.tree.m, main = "Media Kickstarter Classification Tree")

#--- test the model ---#
ctp.pred.train.m <- predict(class.tree.m, train.m, type = "class")
ctp.pred.train.m
table(ctp.pred.train.m, train.m$Status)
mean(train.m$Status != ctp.pred.train.m) #Training Accuracy Rate: 94.92%
percent(1- mean(train.m$Status != ctp.pred.train.m), accuracy = .001)

ctp.pred.valid.m <- predict(class.tree.m, valid.m, type = "class")
ctp.pred.valid.m 
table(ctp.pred.valid.m, valid.m$Status)
mean(valid.m$Status != ctp.pred.valid.m) #Valid Accuracy Rate: 94.86%
percent(1-mean(valid.m$Status != ctp.pred.valid.m), accuracy = .001) 

#--- Random Forest  ---# 
rf.m <- randomForest(Status ~ ., data = train.m, ntree = 500,  mtry = 4, nodesize = 5, importance = TRUE)

#--- Variable Importance Plot ---#  
varImpPlot(rf.m, type = 1, main = "Media Variable Importance Plot")
pruned.ct.pred.valid.m <- predict(rf.m, valid.m, type = "class")
table(pruned.ct.pred.valid.m, valid.m$Status)
mean(pruned.ct.pred.valid.m!= valid.m$Status) #Random Forest Valid Accuracy Rate: 99.88%
percent(1-mean(pruned.ct.pred.valid.m!= valid.m$Status), accuracy = .001)


###Category 3: Tech
#--- Subset Prep ---#
Tech <- kick.df %>% rownames_to_column('id') %>%
  select(Main_category_condensed, backers, Country.US.vs.not, usd_pledged_real, usd_goal_real, 
         proj_length, id) %>% filter(Main_category_condensed %in% c("Tech")) %>%
  column_to_rownames('id')
Tech <- Tech[,-c(1)]

#--- Training & Validation Sets ---#
set.seed(100)
train.i.t <- sample(rownames(Tech), dim(Tech)[1]*.6)
valid.i.t <- setdiff(rownames(Tech), train.i.t)
train.t <- kick.df[train.i.t, ]
valid.t <- kick.df[valid.i.t, ]

#--- r part ---#
set.seed(100)
ks.ct.t <- rpart(Status~ ., data=train.t, method = "class", cp = .001, minsplit = 5, xval = 5)
printcp(ks.ct.t)

#---- Tree ----# 
set.seed(100)
class.tree.t <- rpart(Status~., data = train.t, method = "class")

prp(class.tree.t, type = 1, extra = 1, under = TRUE, split.font = 4, varlen = -20, main = "Tech Kickstarter Classification Tree")
fancyRpartPlot(class.tree.t, main = "Tech Kickstarter Classification Tree")

#--- test the model ---#
ctp.pred.train.t <- predict(class.tree.t, train.t, type = "class")
ctp.pred.train.t
table(ctp.pred.train.t, train.t$Status)
mean(train.t$Status != ctp.pred.train.t) #Training Accuracy Rate: 96.44%
percent(1- mean(train.t$Status != ctp.pred.train.t), accuracy = .001)

ctp.pred.valid.t <- predict(class.tree.t, valid.t, type = "class")
ctp.pred.valid.t 
table(ctp.pred.valid.t, valid.t$Status)
mean(valid.t$Status != ctp.pred.valid.t) #Valid Accuracy Rate: 96.30%
percent(1-mean(valid.t$Status != ctp.pred.valid.t), accuracy = .001) 

#--- Random Forest  ---# 
rf.t <- randomForest(Status ~ ., data = train.t, ntree = 500,  mtry = 4, nodesize = 5, importance = TRUE)

#--- Variable Importance Plot ---#  
varImpPlot(rf.t, type = 1, main = "Tech Variable Importance Plot")
pruned.ct.pred.valid.t <- predict(rf.t, valid.t, type = "class")
table(pruned.ct.pred.valid.t, valid.t$Status)
mean(pruned.ct.pred.valid.t!= valid.t$Status) #Random Forest Valid Accuracy Rate: 99.83%
percent(1-mean(pruned.ct.pred.valid.t!= valid.t$Status), accuracy = .001)

