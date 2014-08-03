#####################################################################
## Summary of Analysis
#####################################################################
#
# 
# Model                	Outcome	        Type	Nodes	Result
#######                 #######         #####   #####   ###########################################
# Classification Tree	quality 1-9	factor	8	Misclassification error rate: 0.4307
# Pruned	              "		"	2	Misclassification error rate: 0.4435
# 
# Regression Tree       quality 1-9	numeric	9	Residual Mean deviance 0.4253
# Pruned                "		"	6	Residual Mean deviance 0.4561
# 
# Classification Tree	qualc: A,B,C	factor	12	Misclassification error rate: 0.1583
# Pruned	        qualc: A,B,C	factor	4	Misclassification error rate: 0.1583
# 
# Random Forest	        qualc: 1,2,3	numeric		Mean squared residuals: 0.1277
# (regression)
# 
# Random Forest	        qualc: 1,2,3	factor		OOB estimate of error rate: 16.91%
# (classification)
# 
# Random Forest	        quality 1-9	factor		OOB estimate of error rate: 39.43%
# (classification)
# 
# Random Forest	        quality 1-9	numeric		Mean squared of residuals: 0.421 (37.71% 				variance explained)
# (regression)                                            variance explained)

#####################################################################
## Data and Libraries
#####################################################################


# Scrubbed data.frames saved from Dennis's script in 
# workspace wine_trees_ws.RData:
# red.train
# red.test

rm(list=ls())
setwd("~/R/Wine Project")
load("~/R/Wine Project/wine_trees_ws.RData")

# load libraries
library(tree)
library(randomForest)


#####################################################################
# # Classification Tree model using quality 1-9 rating 
#####################################################################

grade <- rbind(red.train[,1:12], red.test[,1:12])
# Convert quality to a factor
grade$quality <- as.factor(grade$quality)
# create tree using all data (Not sure why we do this? ... following 
# the Lab 8.3.1 in the ISLR book)
tree.grade = tree(quality~. ,grade)
tree.grade
summary(tree.grade)
par(mfrow = c(1,1))
plot(tree.grade)
text(tree.grade ,pretty=0, cex=0.6)
title("Classification Tree Using All Data")
summary(tree.grade)
# Classification tree:
#         tree(formula = quality ~ ., data = grade)
# Variables actually used in tree construction:
#         [1] "alcohol"  "tot.sulf" "sulph"    "vol.acid"
# Number of terminal nodes:  8 
# Residual mean deviance:  1.935 = 2607 / 1347 
# Misclassification error rate: 0.4066 = 551 / 1355 

# Now split the data into train and test sets and build tree
set.seed(2)
# train =sample (1: nrow(grade), 338)  # 25% sample
train = 1:1017
grade.test = grade[-train ,]
quality.test=grade$quality[-train ]
tree.grade =tree(grade$quality~. , grade, subset =train )
summary(tree.grade)
# Classification tree:
#         tree(formula = grade$quality ~ ., data = grade, subset = train)
# Variables actually used in tree construction:
#         [1] "alcohol"  "tot.sulf" "sulph"    "vol.acid"
# Number of terminal nodes:  8 
# Residual mean deviance:  1.926 = 1943 / 1009 
# Misclassification error rate: 0.4307 = 438 / 1017 

plot(tree.grade)
text(tree.grade ,pretty=0, cex=0.6)
title("Classification Tree Using Training Data")
tree.pred=predict (tree.grade ,grade.test ,type ="class")
table(tree.pred ,quality.test)
# quality.test
# tree.pred  3  4  5  6  7  8
#         3  0  0  0  0  0  0
#         4  0  0  0  0  0  0
#         5  1 11 84 31  3  0
#         6  0  4 54 92 22  1
#         7  0  0  2 11 19  3
#         8  0  0  0  0  0  0
# (84+92+19)/338 = 0.5769231

# cross validate
set.seed (3)
cv.grade =cv.tree(tree.grade ,FUN=prune.misclass )
names(cv.grade )
cv.grade
par(mfrow =c(1,2))
plot(cv.grade$size ,cv.grade$dev ,type="b", xlab="Number of trees",
     ylab="Cross Validation Error Rate")
plot(cv.grade$k ,cv.grade$dev ,type="b",xlab="Cost Complexity Parameter",
     ylab="Cross Validation Error Rate")
par(mfrow =c(1,1))
cv.grade$size[which.min(cv.grade$dev)]
# min dev at size = 2
# try pruning
# select 2 branches based on left plot above
prune.grade =prune.misclass (tree.grade ,best =2)
plot(prune.grade )
text(prune.grade ,pretty =0, cex=0.6)
title("Pruned Classification tree based on training data")
tree.pred=predict (prune.grade , grade.test ,type="class")
table(tree.pred, quality.test)
# quality.test
#         quality.test
# tree.pred     3   4   5   6   7   8
#           3   0   0   0   0   0   0
#           4   0   0   0   0   0   0
#           5   0  11 106  61   9   0
#           6   1   4  34  73  35   4
#           7   0   0   0   0   0   0
#           8   0   0   0   0   0   0
# (106+73)/338 = 0.5295  WORSE!

summary(prune.grade)

# Classification tree:
#         snip.tree(tree = tree.grade, nodes = 2:3)
# Variables actually used in tree construction:
#         [1] "alcohol"
# Number of terminal nodes:  2 
# Residual mean deviance:  2.156 = 2188 / 1015 
# Misclassification error rate: 0.4435 = 451 / 1017 

#####################################################################
# # Regression Tree model using quality 1-9 rating (numeric)
#####################################################################

set.seed (1)
grade$quality <- as.numeric(grade$quality)
# train = sample(1:nrow(grade), nrow(grade)/2)
train = 1:1017


grade.test = grade[-train ,]
quality.test=grade$quality[-train ]
tree.grade =tree(grade$quality~. , grade, subset =train )
summary(tree.grade)
# Regression tree:
#         tree(formula = grade$quality ~ ., data = grade, subset = train)
# Variables actually used in tree construction:
#         [1] "alcohol"  "vol.acid" "sulph"   
# Number of terminal nodes:  9 
# Residual mean deviance:  0.4253 = 428.7 / 1008 
# Distribution of residuals:
#         Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -2.2630 -0.2830 -0.2020  0.0000  0.4444  2.2220 

plot(tree.grade )
text(tree.grade,pretty =0, cex=0.7)
title("Regression Tree Using Training Data")
# cross validate
set.seed (1)
cv.grade =cv.tree(tree.grade)
plot(cv.grade$size ,cv.grade$dev ,type="b")
cv.grade
cv.grade$size[which.min(cv.grade$dev)]
prune.grade = prune.tree(tree.grade, best = 6)
plot(prune.grade)
text(prune.grade,pretty=0)
title("Pruned Regression Tree Using Training Data")
summary(prune.grade)

# Regression tree:
#         snip.tree(tree = tree.grade, nodes = c(10L, 9L, 11L))
# Variables actually used in tree construction:
#         [1] "alcohol"  "vol.acid" "sulph"   
# Number of terminal nodes:  6 
# Residual mean deviance:  0.4561 = 461.2 / 1011 
# Distribution of residuals:
#         Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -2.4340 -0.4344 -0.1143  0.0000  0.5656  1.9250 

yhat=predict(tree.grade, newdata=grade[-train,])
grade.test=grade[-train,"quality"]
plot(yhat, grade.test)
abline(0,1)
mean((yhat-grade.test)^2)
# [1] 0.462394 
# MSE = 0.462394 so error is 0.462394^0.5 =  0.6799956

#######################################################################
# Classification Tree with combined ratings (1,2,3 converted to A,B,C)
#######################################################################
red.c <- rbind(red.train[,c(1:11,13)], red.test[,c(1:11,13)])

# I combined the data.frames sets to add the qualc variable...
red.c$qualClass <- ""
red.c$qualClass <- ifelse(red.c$qualc == 1,"A",red.c$qualClass)
red.c$qualClass <- ifelse(red.c$qualc == 2,"B",red.c$qualClass)
red.c$qualClass <- ifelse(red.c$qualc == 3,"C",red.c$qualClass)
red.c$qualClass <- as.factor(red.c$qualClass)

# Rather than re-split the data.frames, I'm just going to use the
# following variables to subset.

tr <- nrow(red.train)
tst <- nrow(red.test)

tree.red.c =tree(qualClass~.-qualc ,red.c[1:tr,])
tree.red.c
plot(tree.red.c)
text(tree.red.c, pretty=0, cex=0.7)
title("Classification Tree with Combined Ratings")
# Almost everything ends up a 'B' !
red.c.test <- red.c[tr+1:tst,]
tree.pred = predict(tree.red.c, red.c.test, type ="class")
summary(tree.red.c)
# Classification tree:
#         tree(formula = qualClass ~ . - qualc, data = red.c[1:tr, ])
# Variables actually used in tree construction:
#         [1] "alcohol"  "tot.sulf" "sulph"    "pH"       "vol.acid"
# Number of terminal nodes:  12 
# Residual mean deviance:  0.7691822 = 773.0281 / 1005 
# Misclassification error rate: 0.1583088 = 161 / 1017 

table(tree.pred, red.c.test$qualClass)
# tree.pred   A   B   C
#         A   0   0   0
#         B  16 268  28
#         C   0   6  20
# 288/(288+16+6+28)  85.2% accurate  not bad!


# prune
set.seed (3)
cv.red.c =cv.tree(tree.red.c, FUN=prune.misclass )
names(cv.red.c )
cv.red.c
par(mfrow =c(1,2))
# plot(cv.red.c$size ,cv.red.c$dev, type="b")
# plot(cv.red.c$k ,cv.red.c$dev, type="b")
plot(cv.red.c$size ,cv.red.c$dev, type="b", xlab="Number of trees",
     ylab="Cross Validation Error Rate")
plot(cv.red.c$k ,cv.red.c$dev, type="b",xlab="Cost Complexity Parameter",
     ylab="Cross Validation Error Rate")

par(mfrow = c(1,1))
# try pruning
# select 4 branches based on left plot above
prune.red.c = prune.misclass(tree.red.c, best = 4)
summary(prune.red.c)

# Classification tree:
#         snip.tree(tree = tree.red.c, nodes = c(2L, 6L, 14L))
# Variables actually used in tree construction:
#         [1] "alcohol" "sulph"  
# Number of terminal nodes:  4 
# Residual mean deviance:  0.9230127 = 935.0118 / 1013 
# Misclassification error rate: 0.1583088 = 161 / 1017 

plot(prune.red.c)
text(prune.red.c, pretty=0, cex=0.7)
title("Classification Tree with Combined Ratings")
tree.pred=predict(prune.red.c , red.c.test ,type="class")
table(tree.pred,red.c.test$qualClass)

# tree.pred   A   B   C
#         A   0   0   0
#         B  16 268  28
#         C   0   6  20
# 288/(288+16+6+28) = .852  # no improvement with pruning


# what is significance of k?

#######################################################################
# Random Forests -- Regression on Combined Ratings
#######################################################################


red.train2 <- red.train[,c(1:11,13)]
red.test2 <- red.test[,c(1:11,13)]

rf.train2=randomForest(qualc~.,data=red.train2)
# Warning message:
#         In randomForest.default(m, y, ...) :
#         The response has five or fewer unique values. 
# Are you sure you want to do regression?


rf.train2  # OOB estimate
# Call:
#         randomForest(formula = qualc ~ ., data = red.train2) 
# Type of random forest: regression
# Number of trees: 500
# No. of variables tried at each split: 3
# 
# Mean of squared residuals: 0.1277013368
# % Var explained: 24.66

set.seed(2)
oob.err=double(11)
test.err=double(11)
for(mtry in 1:11){
        fit=randomForest(qualc~.,data=red.train2,mtry=mtry,ntree=400)
        oob.err[mtry]=fit$mse[400]
        pred=predict(fit,red.test2)
        test.err[mtry]=with(red.test2,mean((qualc-pred)^2))
        cat(mtry," ")
}
dev.off()
matplot(1:mtry,cbind(test.err,oob.err),pch=19,col=c("red","blue"),type="b",ylab="Mean Squared Error")
legend("topright",legend=c("Test","Oob"),pch=19,col=c("red","blue"))

# MSE is minimized at mtry = 2, but the difference between MSE(8) and
# 
# Note: Oob error diverges from Test MSE. What does that mean?

# try different number of trees

trees = seq(from=50, to=600, by = 50)
oob.err=double(length(trees))
test.err=double(length(trees))
for(i in 1:length(trees)){
        numtrees = trees[i]
        fit=randomForest(qualc~.,data=red.train2,mtry=3,ntree=numtrees)
        oob.err[i]=fit$mse[numtrees]
        pred=predict(fit,red.test2)
        test.err[i]=with(red.test2,mean((qualc-pred)^2))
        cat(numtrees," ")
}
dev.off()
matplot(trees,cbind(test.err,oob.err),pch=19,col=c("red","blue"),type="b",ylab="Mean Squared Error")
legend("topright",legend=c("Test","Oob"),pch=19,col=c("red","blue"))
title("MSE for mtry=3 and various ntree")

# looks like 200 trees is enough

#######################################################################
# Random Forests (Classification) Combined Ratings
#######################################################################


red.train2 <- red.train[,c(1:11,13)]
red.test2 <- red.test[,c(1:11,13)]
red.train2$qualc <- as.factor(red.train2$qualc)
red.test2$qualc <- as.factor(red.test2$qualc)
set.seed(5)
rf.train2 = randomForest(qualc~.,data=red.train2)
rf.train2  #
# Call:
#         randomForest(formula = qualc ~ ., data = red.train2) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 3
# 
# OOB estimate of  error rate: 16.91%
# Confusion matrix:
#         1   2  3 class.error
#       1 0  45  1   1.0000000
#       2 2 800 35   0.0442055
#       3 0  89 45   0.6641791


# (803+45)/(800+45+2+45+89+1+35) =  .833825

summary(rf.train2)
# Length Class  Mode     
# call               3   -none- call     
# type               1   -none- character
# predicted       1017   factor numeric  
# err.rate        2000   -none- numeric  
# confusion         12   -none- numeric  
# votes           3051   matrix numeric  
# oob.times       1017   -none- numeric  
# classes            3   -none- character
# importance        11   -none- numeric  
# importanceSD       0   -none- NULL     
# localImportance    0   -none- NULL     
# proximity          0   -none- NULL     
# ntree              1   -none- numeric  
# mtry               1   -none- numeric  
# forest            14   -none- list     
# y               1017   factor numeric  
# test               0   -none- NULL     
# inbag              0   -none- NULL     
# terms              3   terms  call  

plot(rf.train2, type="l", main=deparse(substitute(rf.train2)))
# not sure what this plot is -- 4 curves...

mtry <- tuneRF(red.train2, red.train2$qualc, ntreeTry=500, 
               stepFactor=1.5,improve=0.01, trace=TRUE, 
               plot=TRUE, dobest=FALSE)
# Error in if (Improve > improve) { : missing value where TRUE/FALSE needed
# caused by correlated variables?

importance(rf.train2)
#                 MeanDecreaseGini
# fix.acid          22.71295
# vol.acid          37.07982
# cit.acid          23.89290
# res.sugar         25.15532
# chlor             23.82891
# free.sulf         19.94671
# tot.sulf          28.45012
# density           28.04331
# pH                19.65534
# sulph             33.26549
# alcohol           45.11337

varImpPlot(rf.train2, sort=TRUE, n.var=min(30, 
        nrow(rf.train2$importance)),
           type=NULL, class=NULL, scale=TRUE,
           main=deparse(substitute(rf.train2)))

getTree(rf.train2, k=1, labelVar=FALSE)
# not sure how to interpret the above output. 


# Try different number of variables (mtry) at each split
oob.err=double(11)
test.err=double(11)
for(mtry in 1:11){
        fit=randomForest(qualc~.,data=red.train2,mtry=mtry,ntree=400)
        oob.err[mtry]=fit$mse[400]
        pred=predict(fit,red.test2)
        test.err[mtry]=with(red.test2,mean((qualc-pred)^2))
        cat(mtry," ")
}

# Error in oob.err[mtry] = fit$mse[400] : replacement has length zero
# Is this because this is a classification rf and this algorithm is actually for a regression rf?
# the following lines are commented out because the above failed

# dev.off()
# matplot(1:mtry,cbind(test.err,oob.err),pch=19,col=c("red","blue"),type="b",ylab="Mean Squared Error")
# legend("topright",legend=c("Test","Oob"),pch=19,col=c("red","blue"))

# pick the mtry with the lowest test.err

# try different number of trees

# trees = seq(from=50, to=600, by = 50)
# oob.err=double(length(trees))
# test.err=double(length(trees))
# for(i in 1:length(trees)){
#         numtrees = trees[i]
#         fit=randomForest(qualc~.,data=red.train2,mtry=4,ntree=numtrees)
#         oob.err[i]=fit$mse[numtrees]
#         pred=predict(fit,red.test2)
#         test.err[i]=with(red.test2,mean((qualc-pred)^2))
#         cat(numtrees," ")
# }
# dev.off()
# matplot(trees,cbind(test.err,oob.err),pch=19,col=c("red","blue"),type="b",ylab="Mean Squared Error")
# legend("topright",legend=c("Test","Oob"),pch=19,col=c("red","blue"))
# title("MSE for mtry=4 and various ntree")


#######################################################################
# Random Forests (Classification with quality ratings 1-9)
#######################################################################

grade <- rbind(red.train[,1:12], red.test[,1:12])
# Convert quality to a factor
grade$quality <- as.factor(grade$quality)

train = 1:1017
grade.train = grade[train,]
grade.test = grade[-train ,]
quality.train=grade.train$quality
quality.test=grade.test$quality

set.seed(5)
rf.train2 = randomForest(quality.train~. -quality, data=grade.train)
rf.train2

# Call:
#         randomForest(formula = quality.train ~ ., data = grade.train) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 3
# 
# OOB estimate of  error rate: 39.43%
# Confusion matrix:
#         3 4   5   6  7 8 class.error
#       3 0 1   7   1  0 0   1.0000000
#       4 1 0  21  14  1 0   1.0000000
#       5 0 0 333 100  3 0   0.2362385
#       6 0 0 119 247 35 0   0.3840399
#       7 0 0   7  77 36 1   0.7024793
#       8 0 0   0   8  5 0   1.0000000

# (333+247+36)/1017 = 0.6057

importance(rf.train2)
#                 MeanDecreaseGini
# fix.acid          48.47823
# vol.acid          66.76547
# cit.acid          45.77974
# res.sugar         48.16457
# chlor             53.96019
# free.sulf         43.28367
# tot.sulf          68.43482
# density           61.63740
# pH                50.06823
# sulph             68.76051
# alcohol          100.03395

varImpPlot(rf.train2, sort=TRUE, n.var=min(30, 
                                           nrow(rf.train2$importance)),
           type=NULL, class=NULL, scale=TRUE,
           main=deparse(substitute(rf.train2)))

#######################################################################
# Random Forests (Regression with quality ratings 1-9)
#######################################################################

grade <- rbind(red.train[,1:12], red.test[,1:12])

train = 1:1017
grade.train = grade[train,]
grade.test = grade[-train ,]
quality.train=grade.train$quality
quality.test=grade.test$quality

set.seed(5)
rf.train2 = randomForest(quality.train~. -quality, data=grade.train)
rf.train2
# Call:
#         randomForest(formula = quality.train ~ . - quality, data = grade.train) 
# Type of random forest: regression
# Number of trees: 500
# No. of variables tried at each split: 3
# 
# Mean of squared residuals: 0.4216405
# % Var explained: 37.71


importance(rf.train2)
#              IncNodePurity
# fix.acid       38.45068
# vol.acid       86.11197
# cit.acid       40.95875
# res.sugar      38.22336
# chlor          47.68219
# free.sulf      31.67171
# tot.sulf       52.04065
# density        56.04986
# pH             36.05273
# sulph          84.41618
# alcohol       133.81554

#######################################################################
# cforest package Classification trees
#######################################################################

## Tried cforest, but ran into memory issues :-()
# Found a comment on the web, that regularized random forest are an 
# alternative to cforest when memory issues are a problem

library(languageR) 
library(rms) 
library(party)

data.controls <- cforest_unbiased(ntree=250, mtry=3)
set.seed(47) 
data.cforest <- cforest(qualc~.,data=red.train2, 
                        controls=data.controls) 
data.cforest.varimp <- varimp(data.cforest, conditional = TRUE) 
# Error: cannot allocate vector of size 5.7 Gb
# In addition: Warning messages:
# 1: In model.matrix.default(as.formula(f), data = blocks) :
#   Reached total allocation of 6026Mb: see help(memory.size)
# 2: In model.matrix.default(as.formula(f), data = blocks) :
#   Reached total allocation of 6026Mb: see help(memory.size)
# 3: In model.matrix.default(as.formula(f), data = blocks) :
#   Reached total allocation of 6026Mb: see help(memory.size)
# 4: In model.matrix.default(as.formula(f), data = blocks) :
#   Reached total allocation of 6026Mb: see help(memory.size)

