library(ISLR)
library(leaps)
attach(College)

#train/testr
set.seed(2)
train <- sample(1:nrow(College), nrow(College)/2)

fwd.college <- regsubsets(Outstate~., data = College[train,], 
                          nvmax = 17, method = "forward")
summary(fwd.college)


plot(summary(fwd.college)$cp, type = 'b', ylab = 'CP')
points(which.min(summary(fwd.college)$cp), min(summary(fwd.college)$cp), 
       col ='red' , pch = 'C')
plot(summary(fwd.college)$bic, type = 'b', ylab = 'BIC')
points(which.min(summary(fwd.college)$bic), min(summary(fwd.college)$bic), 
       col ='red' , pch = 'C')
plot(summary(fwd.college)$adjr2, type = 'b', ylab = 'Adjusted R2')
points(which.max(summary(fwd.college)$adjr2), max(summary(fwd.college)$adjr2), 
       col ='red' , pch = 'C')

which.min(summary(fwd.college)$cp)
which.min(summary(fwd.college)$bic)
which.max(summary(fwd.college)$adjr2)

coef(fwd.college, id = 11)

lm.college <- lm(Outstate~ Private + Apps + Accept + Enroll 
                 + Top10perc + Room.Board + Personal + Terminal 
                 + perc.alumni + Expend + Grad.Rate, data= College, subset = train)
yhat.lm <- predict(lm.college, newdata = College[-train,])
min((yhat.lm - College[-train, "Outstate"])^2)

####################################################################
#GAM
####################################################################
library(gam)

gam.college <- gam(Outstate~ Private + s(Apps, df=2) + s(Accept, df=2) 
                   + s(Enroll, df=2) + s(Top10perc, df=2) 
                   + s(Room.Board, df=2) + s(Personal, df=2) 
                   + s(Terminal, df=2) + s(perc.alumni, df=2) 
                   + s(Expend, df=2) + s(Grad.Rate, df=2), data= College, subset = train)
par(mfrow=c(1,2))
plot(gam.college, se=TRUE)

#########
gam.college <- gam(Outstate~ Private + s(Apps, df=5) + s(Accept, df=5) 
                   + s(Enroll, df=5) + s(Top10perc, df=5) 
                   + s(Room.Board, df=5) + s(Personal, df=5) 
                   + s(Terminal, df=5) + s(perc.alumni, df=5) 
                   + s(Expend, df=5) + s(Grad.Rate, df=5), data= College, subset = train)
par(mfrow=c(1,2))
plot(gam.college, se=TRUE)
#########

#performance on the test set
yhat.gam <- predict(gam.college, newdata = College[-train,])
mean((yhat.gam - College[-train, "Outstate"])^2)

summary(gam.college)

######################################################################
#Vijver
######################################################################
library(tree)
library(randomForest)
library(gbm)

#importing the data
setwd("/home/hamed/KUL/SEM2/stat/Rob/Labs+exercises/ex05")
load("VIJVER.Rdata")
data$meta <- factor(data$meta)


#dummy coding : DM->1 , NODM->0
#phenotype = ifelse(data$meta == "DM", 1, 0)
#vijver <- cbind(phenotype,data[,-1])

#vijver$phenotype <- as.factor(vijver$phenotype)
vijver <- data
#train/test
set.seed(3)
train <- sample(1:nrow(vijver), 2*nrow(vijver)/3)
vijver.test <- vijver[-train, "meta"]

#building a tree
vijver.tree <- tree(meta~., data = vijver[train,])
summary(vijver.tree)
plot(vijver.tree)
text(vijver.tree, pretty = 0, cex = 0.6)

tree.pred <- predict(vijver.tree, newdata = vijver[-train,], type = 'class')
table(tree.pred, vijver.test)

#pruning
#CV for pruning the tree
set.seed(3)
cv.vijver <- cv.tree(vijver.tree, FUN = prune.misclass)
names(vijver.tree)

par(mfrow = c(1,2))
plot(cv.vijver$size, cv.vijver$dev, type = 'b')
plot(cv.vijver$k, cv.vijver$dev, type = 'b')
#pruning
prune.vijver <- prune.misclass(vijver.tree, best = 7)
par(mfrow = c(1,1))
plot(prune.vijver)
text(prune.vijver, pretty = 0, cex = 0.6)
#calculating the test error
tree.pred <- predict(prune.vijver, vijver[-train,], type = "class")
table(tree.pred, vijver.test)

#######################################################################
#Bagging
#######################################################################
library(randomForest)
set.seed(3)
ptm <- proc.time()
bag.vijver <- randomForest(meta~., data = vijver, subset = train, mtry = 4948, importance = TRUE)
proc.time() - ptm

#adabag
#library(adabag)
#ptm <- proc.time()
#adabag.vijver <- bagging(meta~., data = vijver[train, ])
#proc.time() - ptm
#

bag.pred <- predict(bag.vijver, newdata = vijver[-train,])
table(bag.pred, vijver.test)

#######################################################################
#random forest
#######################################################################
set.seed(1)
ptm <- proc.time()# just for timing
rf.vijver <- randomForest(meta~., data = vijver, subset = train, 
                          mty = 10, importance = TRUE)
proc.time() - ptm

rf.pred <- predict(rf.vijver, newdata = vijver[-train, ])
table(rf.pred, vijver.test)

importance(rf.vijver) #importance of variables
varImpPlot(rf.vijver)

#######################################################################
#boosting
#######################################################################
library(gbm)

meta = ifelse(vijver$meta == "DM", 1, 0)
vijver$meta <- as.character(meta)

ptm <- proc.time() #just for timing
set.seed(1)
boost.vijver <- gbm(meta~., data = vijver[train,],
                    distribution = "bernoulli", n.trees = 5000,
                    interaction.depth = 4)
proc.time() - ptm
summary(boost.vijver) # error?!

boost.pred <- predict(boost.vijver, newdata = vijver[-train,], 
                      n.trees = 5000, type = 'response')
pred <- ifelse(boost.pred >0.50, 1, 0)
table(pred, vijver[-train, "meta"])
