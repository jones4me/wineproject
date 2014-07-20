# Load libraries
library(ggplot2)
library(psych)
library(plyr)
library(neuralnet)
library(nnet)
library(e1071)
library(randomForest)


# Analysis of wine quality data, from study done for variants of the
# "Venho Verde" Portuguese wines.

# These data have been analyzed by:
#  P. Cortez, A. Cerdeira, F. Almeida, T. Matos and J. Reis. 
#  Modeling wine preferences by data mining from physicochemical properties.
#  In Decision Support Systems, Elsevier, 47(4):547-553. ISSN: 0167-9236.
# 
# http://www.vinhoverde.pt/en/ 

# Note:  This script file is not intended to be executed all at once,
#   as in a source() command.  It is instead a place to capture the
#   script sequences used in this data analysis.  This file also includes
#   comments on the steps being performed and intermediate & end results.
#   It could however later be used as a basis for source-able scripts,
#   especially for definition of functions.
# Only selected graphics and text information are included in the
#   companion Powerpoint file.

setwd("~/R/Wine Project")
wine.r <- read.csv("winequality-red.csv", header = T)
names(wine.r)

wine.w <- read.csv("winequality-white.csv", header = T)
names(wine.w)

# screen red wine dataset
# first look for duplicates
dup.red <- duplicated(wine.r, nmax=length(wine.r))
table(dup.red)
# shows 240 duplicated records
# is it reasonable to expect that any of these duplicates are legitimate?
plot(dup.red)
# there is no clear sequential pattern of dups through the file
wine.rr <- cbind(wine.r,dup.red)
table(wine.rr$quality,wine.rr$dup.red)
# the only pattern of dups compared to quality is that there are no dups
#  for quality = 3 or 4
#  There may have been a bias toward dups in the mid to high quality red wines
# examination does not show patterns/corrs of dups against other vars

uniq.dups <- unique(wine.rr[dup.red,])
length(uniq.dups$dup.red)
# there are 220 unique dups, so there are some triplicates or more

#remove duplicates
w.red <- wine.r[!dup.red,]
length(w.red$quality)
# now there are 1359 cases
table(w.red$quality)
# confirm there are no more duplicates
anyDuplicated(w.red)

# outliers in w.red:
# tot.sulf (2), all vars dup except for tot.sulf = 278, 289
# chlor(2), 0.610, 0.611, about 25% out of range of others

plot(w.red$tot.sulf, w.red$chlor)

out1 <- subset(w.red, tot.sulf >200 | chlor>0.5)
out1
# the tot.sulf outlier cases were about 100% higher than the range of other measuremnts
# these had identical values for all other measurements
# the chlorine outliers were about 25% higher than the range of other measurements
# these 4 cases to be removed

w.red <- subset(w.red, tot.sulf < 200 & chlor < 0.5)
# confirm that outliers removed
out2 <- subset(w.red, tot.sulf >200 | chlor>0.5)
out2
length(w.red$quality)
table(w.red$quality)
# so dataset reduced from 1359 to 1355 cases
p <- ggplot(w.red, aes(tot.sulf, chlor))
pt <- p + geom_point(size=5, alpha=0.1, colour="blue")
pt

p <- ggplot(w.red, aes(density, chlor))
pt <- p + geom_point(size=5, alpha=0.1, colour="blue")
pt

# data screening for white wines
#  look for duplicates
dup.white <- duplicated(wine.w, nmax=length(wine.w))
table(dup.white)
# there is a higher frequencey of dups for the whites, 937 in total

plot(dup.white)
# there is no clear sequential pattern of dups through the file
wine.ww <- cbind(wine.w,dup.white)
table(wine.ww$quality,wine.ww$dup.white)
#  There is no obvious pattern of dups vs. quality, except the 20 qual=3 had no dups
#  examination does not show patterns/corrs of dups against other vars
uniq.dups <- unique(wine.ww[dup.white,])
length(uniq.dups$dup.white)
# there are 772 unique dups, so there are many triplicates or more

w.wht <- wine.w[!dup.white,]
str(w.wht)
# now there are 3961 cases
table(w.wht$quality)
# confirm there are no more duplicates
anyDuplicated(w.wht)

# outliers in wine.w:
# density (1), res.sugar(1), free.sulf(1)
# density and res_sugar outliers are same case, quality=6, they are so suspect
# free.sulf outlier is quality=3, so keep that
# so dataset reduced from 3961 to 3960 cases
out1 <- subset(w.wht, res.sugar > 40)
out1
out2 <- subset(w.wht, free.sulf > 150)
out2
out3 <- subset(w.wht, density > 1.02)
out3

w.wht <- subset(w.wht, res.sugar < 40)

p <- ggplot(w.wht, aes(res.sugar, free.sulf))
pt <- p + geom_point(size=5, alpha=0.1, colour="blue")
pt

p <- ggplot(w.wht, aes(res.sugar, density))
pt <- p + geom_point(size=5, alpha=0.2, colour="blue")
pt

qual <- factor(wine.r$quality)
p <- ggplot(wine.r, aes(sulph, chlor, color = qual))
pt <- p + geom_point(style=21, size=5, alpha = 0.5)
pt <- p + geom_point(size=I(4), style=21)
pt


# data screening complete
# now look at scatter plots of both sets

pairs.panels(wine.r[, c(1:11)],gap=0)
pairs.panels(w.red[, c(1:11)],gap=0)

pairs.panels(wine.w[, c(1:11)],gap=0)
pairs.panels(w.wht[, c(1:11)],gap=0)

# the correlation structure looks quite different between the reds and whites
# this is as expected -- very different chemistry

cor.plot(cor(w.red[,c(1:11)]))
cor.plot(cor(w.wht[,c(1:11)]))

# Note:  It seems that the implementation of cor.plot does not allow
#   a layout presentation that could, for example, produce two such
#   plots side by side.  Need to identify other such functions.
#   Perhaps there is an alternate way to do this though.

#  reset graphics with this call:
dev.off()

p <- ggplot(w.red, aes(x= factor(quality), y=cit.acid))
pt <- p + geom_boxplot()
pt <- pt + theme(axis.title.x = element_text(size=24), axis.title.y = element_text(size=24))
pt <- pt + labs(x = "Quality", y = "Citric Acid")
pt

p <- ggplot(w.red, aes(x= factor(quality), y=res.sugar))
pt <- p + geom_boxplot()
pt <- pt + theme(axis.title.x = element_text(size=24), axis.title.y = element_text(size=24))
pt <- pt + labs(x = "Quality", y = "Residual Sugar")
pt

p <- ggplot(w.red, aes(x= factor(quality), y=alcohol))
pt <- p + geom_boxplot()
pt <- pt + theme(axis.title.x = element_text(size=24), axis.title.y = element_text(size=24))
pt <- pt + labs(x = "Quality", y = "Alcohol")
pt

# function for wine files, to do facet scatter plots by quality rating
# also plots means as red diamond shape
# can be applied to either red or white wine files

ScatterByQual <- function(frame, x.var, y.var) {
  frame.copy <- frame
  args <- as.list(match.call())
  frame.copy$x.var <- eval(substitute(x.var), frame.copy)
  frame.copy$y.var <- eval(substitute(y.var), frame.copy)
  qmean <- ddply(frame.copy, .(quality), summarize, m.x = mean(x.var), m.y = mean(y.var))
  p <- ggplot(frame.copy, aes(x= x.var, y=y.var))
  pt <- p + geom_point(size=4, alpha=0.3, color = "blue")
  pt <- pt + geom_point(data = qmean, aes(m.x, m.y), size=6, shape=18, colour="red")
  pt <- pt + facet_wrap( ~ quality, ncol=3)
  pt <- pt + theme(axis.title.x = element_blank())
  pt <- pt + theme(axis.title.y = element_blank())
  pt <- pt + ggtitle(paste("For each Quality category,  X= ", args$x.var, "  Y = ", args$y.var, "  red = mean"))
  pt <- pt + theme(plot.title = element_text(size = 20))
  print(pt)
}  

# Note: It would be possible to write a wrapper to cycle through all
#   the pairwise combinations of scatter plots within this, or other,
#   datasets.  I'm not spending the time to do that it this point,
#   because of other immediate objectives, and also because the issue
#   of many plots beyond the default 30 in the Plots buffer would
#   need to be dealt with.

sc <- ScatterByQual(w.red, fix.acid, vol.acid)
sc <- ScatterByQual(w.red, fix.acid, cit.acid)
sc <- ScatterByQual(w.red, fix.acid, res.sugar)
sc <- ScatterByQual(w.red, fix.acid, chlor)
sc <- ScatterByQual(w.red, fix.acid, free.sulf)
sc <- ScatterByQual(w.red, fix.acid, tot.sulf)
sc <- ScatterByQual(w.red, fix.acid, density)
sc <- ScatterByQual(w.red, fix.acid, pH)
sc <- ScatterByQual(w.red, fix.acid, sulph)
sc <- ScatterByQual(w.red, fix.acid, alcohol)

sc <- ScatterByQual(w.red, vol.acid, cit.acid)
sc <- ScatterByQual(w.red, vol.acid, res.sugar)
sc <- ScatterByQual(w.red, vol.acid, chlor)
sc <- ScatterByQual(w.red, vol.acid, free.sulf)
sc <- ScatterByQual(w.red, vol.acid, tot.sulf)
sc <- ScatterByQual(w.red, vol.acid, density)
sc <- ScatterByQual(w.red, vol.acid, pH)
sc <- ScatterByQual(w.red, vol.acid, sulph)
sc <- ScatterByQual(w.red, vol.acid, alcohol)

sc <- ScatterByQual(w.red, cit.acid, res.sugar)
sc <- ScatterByQual(w.red, cit.acid, chlor)
sc <- ScatterByQual(w.red, cit.acid, free.sulf)
sc <- ScatterByQual(w.red, cit.acid, tot.sulf)
sc <- ScatterByQual(w.red, cit.acid, density)
sc <- ScatterByQual(w.red, cit.acid, pH)
sc <- ScatterByQual(w.red, cit.acid, sulph)
sc <- ScatterByQual(w.red, cit.acid, alcohol)

sc <- ScatterByQual(w.red, res.sugar, chlor)
sc <- ScatterByQual(w.red, res.sugar, free.sulf)
sc <- ScatterByQual(w.red, res.sugar, tot.sulf)
sc <- ScatterByQual(w.red, res.sugar, density)
sc <- ScatterByQual(w.red, res.sugar, pH)
sc <- ScatterByQual(w.red, res.sugar, sulph)
sc <- ScatterByQual(w.red, res.sugar, alcohol)

sc <- ScatterByQual(w.red, chlor, free.sulf)
sc <- ScatterByQual(w.red, chlor, tot.sulf)
sc <- ScatterByQual(w.red, chlor, density)
sc <- ScatterByQual(w.red, chlor, pH)
sc <- ScatterByQual(w.red, chlor, sulph)
sc <- ScatterByQual(w.red, chlor, alcohol)

sc <- ScatterByQual(w.red, free.sulf, tot.sulf)
sc <- ScatterByQual(w.red, free.sulf, density)
sc <- ScatterByQual(w.red, free.sulf, pH)
sc <- ScatterByQual(w.red, free.sulf, sulph)
sc <- ScatterByQual(w.red, free.sulf, alcohol)

sc <- ScatterByQual(w.red, tot.sulf, density)
sc <- ScatterByQual(w.red, tot.sulf, pH)
sc <- ScatterByQual(w.red, tot.sulf, sulph)
sc <- ScatterByQual(w.red, tot.sulf, alcohol)

sc <- ScatterByQual(w.red, density, pH)
sc <- ScatterByQual(w.red, density, sulph)
sc <- ScatterByQual(w.red, density, alcohol)

sc <- ScatterByQual(w.red, pH, sulph)
sc <- ScatterByQual(w.red, pH, alcohol)

sc <- ScatterByQual(w.red, sulph, alcohol)

# Split data into training and test sets
# Systematic 25% sample for test set
# (Examination shows no reason to suspect patterns in the order of cases)
red.samp <- which(1:length(w.red[,1])%%4 == 0)
red.train <- w.red[-red.samp,]  # 1017 cases
red.test <- w.red[red.samp,]  # 338 cases

# check how similar the distributions are:
summary(red.train$quality)
summary(red.test$quality)
hist(red.train$quality)
hist(red.test$quality)

# Build simple linear regression model
red.lm <- lm(quality~., data=red.train)
summary(red.lm)
# adjusted R-squared is about 37%
# significant t values for vol.acid, sulph, and alcohol
# also sig for chlor and tot.sulf
# pH is marginally significant (p = 0.042)
# Apply model to test set
red.pred <- predict(red.lm, newdata=red.test)
red.pred.cat <- factor(round(red.pred))
summary(red.pred)
red.test.pr <- cbind(red.test, red.pred, red.pred.cat)
pr.means <- ddply(red.test.pr, .(quality), summarize, avg.pred = mean(red.pred))
pos <- position_jitter(width=0.2, height=0)
p <- ggplot(red.test.pr, aes(quality, red.pred)) + xlim(3,8) + ylim(2.5,8.5)
pt <- p + geom_point(size = 4, alpha = 0.4, color = "blue", position=pos)
pt <- pt + geom_point(data = pr.means, aes(quality, avg.pred), size=6, shape=18, colour="red")
pt <- pt + theme(axis.title.x = element_text(size=24), axis.title.y = element_text(size=24))
pt <- pt + labs(x = "Quality", y = "Linear Model Prediction")
pt <- pt + geom_line(data = pr.means, aes(quality, avg.pred), size=1, colour="red")
pt <- pt + annotate("text", label="Horizontal jitter applied", x= 4.0, y= 8.2, size=5, colour="blue")
pt <- pt + annotate("text", label="Adj R-sq = 36.7%", x= 6.3, y= 3.6, size=6, colour="red")
pt <- pt + annotate("text", label="Correct Prediction = 57.1%", x= 6.3, y= 3.2, size=6, colour="red")
pt <- pt + annotate("text", label="Correct Prediction within 1 level = 96.7%", x= 6.3, y= 2.8, size=6, colour="red")
pt
table(red.test.pr$quality, red.test.pr$red.pred.cat)
length(red.test.pr$quality)

# 57.1% agreement on exact quality rating
# But 96.7% are within one quality rating (top and bottom categories are underrepresented)
  
# try weighted least squares
# higher weights for the quality extremes
wt1 <- 8.5 - red.train$quality
# for moderately unequal weights, 3/2/1/1/2/3 for qualities 3/4/5/6/7/8
#  wt2 <- 3.5 - pmin(wt1, red.train$quality - 2.5)
# for disregarding qual = 5/6, 2/1/0/0/1/2 for qualities 3/4/5/6/7/8
#  wt2 <- 2.5 - pmin(wt1, red.train$quality - 2.5)
wt2 <- 2.5 - pmin(wt1, red.train$quality - 2.5)
plot(red.train$quality, wt2)
red.lm.wt <- lm(quality~., weights = wt2,data=red.train)
summary(red.lm.wt)
# moderately unequal has adjusted R-squared = 44.7%
# if disregard qual = 5/6 categories (weight = 0)
#  it is 64.9% with omission of qual = 5/6

# Apply model to test set
red.pred.wt <- predict(red.lm.wt, newdata=red.test)
red.pred.wt.cat <- factor(round(red.pred.wt))
summary(red.pred.wt)
red.test.pr.wt <- cbind(red.test, red.pred.wt, red.pred.wt.cat)
pr.means <- ddply(red.test.pr.wt, .(quality), summarize, avg.pred = mean(red.pred.wt))

pos <- position_jitter(width=0.2, height=0)
p <- ggplot(red.test.pr.wt, aes(quality, red.pred.wt)) + ylim(2.0,8.5)
pt <- p + geom_point(size = 4, alpha = 0.4, color = "blue", position=pos)
pt <- pt + geom_point(data = pr.means, aes(quality, avg.pred), size=6, shape=18, colour="red")
pt <- pt + theme(axis.title.x = element_text(size=24), axis.title.y = element_text(size=24))
pt <- pt + labs(x = "Quality", y = "Weighted Linear Model Prediction")
pt <- pt + geom_line(data = pr.means, aes(quality, avg.pred), size=1, colour="red")
pt <- pt + annotate("text", label="Adj R-sq = 64.9%", x= 6.3, y= 2.8, size=6, colour="red")
pt <- pt + annotate("text", label="Correct Prediction = 42.9%", x= 6.3, y= 2.4, size=6, colour="red")
pt <- pt + annotate("text", label="Correct Prediction within 1 level = 89.6%", x= 6.3, y= 2.0, size=6, colour="red")
pt
table(red.test.pr.wt$quality, red.test.pr.wt$red.pred.wt.cat)
length(red.test.pr.wt$quality)
# now 55.6% and 97.0%
# and with zero weights for qual =5/6, pct correct is now 42.9 and 89.6
# prediction is better for highest and lowest qualities, but
# but variance is increased throughout

# create combined quality categories:
#  qualc = 1, 2, 3 for quality = (3,4), (5,6), and (7,8)
red.train$qualc <- NA
red.train$qualc[red.train$quality <= 4] <- 1
red.train$qualc[red.train$quality == 5] <- 2
red.train$qualc[red.train$quality == 6] <- 2
red.train$qualc[red.train$quality >= 7] <- 3
table(red.train$quality, red.train$qualc)

red.test$qualc <- NA
red.test$qualc[red.test$quality <= 4] <- 1
red.test$qualc[red.test$quality == 5] <- 2
red.test$qualc[red.test$quality == 6] <- 2
red.test$qualc[red.test$quality >= 7] <- 3
table(red.test$quality, red.test$qualc)

# do OLS regression with combined qualc rating
red.lm.qc <- lm(qualc~., data=red.train[, c(1:11,13)])
summary(red.lm.qc)

# look at logistic regression for predicting either high or low quality wines
# high quality will be defined as quality = 7 or 8
# low quality is defined as quality = 3 or 4
# so quality = 5 and 6 show as FALSE for both indicator columns
# TBD, find way to do this with reshape
col1 <- data.frame(highQ =(red.train$quality >= 7))
col2 <- data.frame(lowQ = (red.train$quality <= 4))
w.red.tr <- cbind(red.train, col1, col2)

lgst.form1 <- "highQ ~ fix.acid + vol.acid + cit.acid + res.sugar + chlor + free.sulf"
lgst.form2 <- "+ tot.sulf + density + pH + sulph + alcohol"
formula <- paste(lgst.form1, lgst.form2)
formula
wr.logist <- glm(formula, data=w.red.tr, family="binomial")
wr.logist
wrlog.pr <- predict(wr.logist, newdata=red.test, type='response')
round(wrlog.pr, 3)
hist(wrlog.pr)
col3 <- data.frame(highQ = (red.test$quality >= 7))

red.test2 <- cbind(red.test, wrlog.pr, wl, col3)
p <- ggplot(red.test2, aes(wrlog.pr, highQ))
pt <- p + geom_point(position= position_jitter(height=0.2))
pt
pt <- p + geom_point(position= position_jitter(height=0.05))
pt

names(red.train)
names(w.red.tr)

# install.packages("neuralnet")
# library(neuralnet)
# install.packages("nnet")
# library(nnet)

# Create indicator variables
red.train.indic <- with(red.train, data.frame(class.ind(qualc)))
red.train.ext <- cbind(red.train, red.train.indic$X1, +
                        red.train.indic$X2, red.train.indic$X3)
names(red.train.ext)[14:16] = c('q.1', 'q.2', 'q.3')
str(red.train.ext)
rm(red.train.indic)

# Apply neural network algorithm
nn.wr <- neuralnet(q.1 + q.2 + q.3 ~ +
    fix.acid + vol.acid + cit.acid + res.sugar + chlor + free.sulf + tot.sulf +
    + density + pH + sulph + alcohol, data=red.train.ext, hidden=c(3))
# note: this took about 10 seconds wall clock time
plot(nn.wr)

mypredict <- compute(nn.wr, red.test[c(1:11)])$net.result
# Consolidate multiple binary output back to categorical output
GetMaxIdx <- function(arr) {
    +     return(which(arr == max(arr)))
}
idx <- apply(mypredict, c(1), GetMaxIdx)
nn1.prediction <- c('q.1.p', 'q.2.p', 'q.3.p')[idx]
table(nn1.prediction, red.test$quality)

# performance is poor on qualc = 1 and qualc = 3
# cannot increase training set sizes much
#  (already 46 or 63 samples for qualc=1, and 134 of 182 for qualc=3)

# produce weights favoring quality 1 and 3 vs quality 2 by 6-to-1
wt1 <- 3.3 - red.train$qualc
wt2 <- 1.5 - pmin(wt1, red.train.ext$qualc - 0.7)
plot(red.train.ext$quality, wt2)
summary(wt2)

red.train$wt2 <- wt2

# Apply neural network algorithm with weighted data
nn.wr.wt <- neuralnet(q.1 + q.2 + q.3 ~ +
    fix.acid + vol.acid + cit.acid + res.sugar + chlor + free.sulf + tot.sulf +
    + density + pH + sulph + alcohol, data=red.train.ext, startweights = wt2, hidden=c(3))
# note: this took about 10 seconds wall clock time
# in weighted case, it took about 24 seconds
plot(nn.wr.wt)
str(nn.wr.wt)

mypredict.wt <- compute(nn.wr.wt, red.test[c(1:11)])$net.result
str(mypredict.wt)
head(mypredict.wt)

idx <- apply(mypredict.wt, c(1), GetMaxIdx)
nn1.wt.prediction <- c('q.1.p', 'q.2.p', 'q.3.p')[idx]
table(nn1.wt.prediction, red.test$quality)

# Now use weighted data, but with resilient backpropagation, weighted backtracking
nn.wr.wt.rp <- neuralnet(q.1 + q.2 + q.3 ~ +
    fix.acid + vol.acid + cit.acid + res.sugar + chlor + free.sulf + tot.sulf +
    + density + pH + sulph + alcohol, data=red.train.ext, startweights = wt2, algorithm = "rprop+", hidden=c(3))
# this took about 23 seconds
dev.off()
plot(nn.wr.wt.rp)
str(nn.wr.wt.rp)

mypredict.wt.rp <- compute(nn.wr.wt.rp, red.test[c(1:11)])$net.result
str(mypredict.wt.rp)
head(mypredict.wt.rp)

idx <- apply(mypredict.wt.rp, c(1), GetMaxIdx)
nn1.wt.rp.prediction <- c('q.1.p', 'q.2.p', 'q.3.p')[idx]
table(nn1.wt.rp.prediction, red.test$qualc)

# Now look at Support Vector Machine
# install.packages("e1071")
# library(e1071)
# again I am looking at the 3 collapsed quality categories
tune.w.red <- tune.svm(qualc ~., data=red.train.ext[ ,c(1:11,13)], 
  gamma=10^(-6:-1), cost=10^(1:4))
# this took about 7 minutes
summary(tune.w.red)
# Parameter tuning of 'svm':
# - sampling method: 10-fold cross validation
# - best parameters:
# gamma  cost
# 0.1 100
# - best performance: 0.02420
# SVM using radial basis kernel
svm.w.red <- svm(qualc~., data=red.train[,c(1:11,13)],
  method="C-classification", kernel="radial",
  probability=T, gamma=0.1, cost=100)
svm.w.red.pred <- predict(svm.w.red, red.test, probability=T)
table(round(svm.w.red.pred,0), red.test$qualc)
str(svm.w.red)
# prediction accuracy is 275/(275+63) = 0.814

# SVM using linear kernel
svm.w.red.ln <- svm(qualc~., data=red.train[,c(1:11,13)],
  method="C-classification", kernel="linear",
  probability=T, gamma=0.1, cost=100)
svm.w.red.pred.ln <- predict(svm.w.red.ln, red.test, probability=T)
table(round(svm.w.red.pred.ln,0), red.test$qualc)
# classification failed:  all predicitons were qualc = 2

# SVM using 3rd order polynomial
svm.w.red.poly <- svm(qualc~., data=red.train[,c(1:11,13)],
  method="C-classification", kernel="polynomial", degree=3,
  probability=T, gamma=0.1, cost=100)
svm.w.red.pred.poly <- predict(svm.w.red.poly, red.test, probability=T)
table(round(svm.w.red.pred.poly,0), red.test$qualc)
# classification results were similar to radial kernel
#  in both the high and low qualc ratings

# try the random forest ensemble method
# install.packages("randomForest")
# library(randomForest)
# Train 500 trees, random selected attributes
ran.for.model <- randomForest(qualc~., data=red.train[,c(1:11,13)],
      nTree=500, mtry=4)
# how well does it predict within the training set?
table(round(ran.for.model$predicted), red.train$qualc)
# accuracy in training set = 853/(853+164) = 0.839
# Predict using the forest
rf.pred.1 <- predict(ran.for.model, newdata=red.test, type='class')
table(round(rf.pred.1), red.test$qualc)
# accuracy in test set = 287/(287+51) = 0.849
importance(ran.for.model)
ran.for.model

table(round(svm.w.red.pred,0), round(rf.pred.1))
# agreement between SVM and RF = 309/(309+29) = 0.914


# EJ futzing... reference Chapter 8 R session
red.train2 <- red.train[,c(1:11,13)]
red.test2 <- red.test[,c(1:11,13)]

rf.train2=randomForest(qualc~.,data=red.train2)
rf.train2  # OOB estimate
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

# try different number of trees

trees = seq(from=50, to=600, by = 50)
oob.err=double(length(trees))
test.err=double(length(trees))
for(i in 1:length(trees)){
        numtrees = trees[i]
        fit=randomForest(qualc~.,data=red.train2,mtry=4,ntree=numtrees)
        oob.err[i]=fit$mse[numtrees]
        pred=predict(fit,red.test2)
        test.err[i]=with(red.test2,mean((qualc-pred)^2))
        cat(numtrees," ")
}
dev.off()
matplot(trees,cbind(test.err,oob.err),pch=19,col=c("red","blue"),type="b",ylab="Mean Squared Error")
legend("topright",legend=c("Test","Oob"),pch=19,col=c("red","blue"))
title("MSE for mtry=4 and various ntree")