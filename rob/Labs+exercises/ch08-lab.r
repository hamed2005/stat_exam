library(tree)
library(ISLR)

attach(Carseats)
High <- ifelse(Sales<=8, "No", "Yes")
Carseats <- data.frame(Carseats, High)
tree.carseats <- tree(High~.-Sales, Carseats)

summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats, pretty = 0, cex = 0.6)

# train/test set
set.seed(2)
train <- sample(1:nrow(Carseats), 200)
Carseats.test <- Carseats[-train,]
High.test <- High[-train]

tree.carseats <- tree(High~.-Sales, Carseats, subset = train)
tree.pred <- predict(tree.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)

#CV for pruning the tree
set.seed(3)
cv.carseats <- cv.tree(tree.carseats, FUN = prune.misclass)
names(cv.carseats)

par(mfrow = c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type = 'b')
plot(cv.carseats$k, cv.carseats$dev, type = 'b')
#pruning
prune.carseats <- prune.misclass(tree.carseats, best = 9)
par(mfrow = c(1,1))
plot(prune.carseats)
text(prune.carseats, pretty = 0, cex = 0.6)
#calculating the test error
tree.pred <- predict(prune.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)

########################################################################
#Regression Trees
########################################################################

library(MASS)
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston <- tree(medv~., Boston, subset = train)
summary(tree.boston)

plot(tree.boston)
text(tree.boston, pretty = 0, cex = 0.6)

#CV and prunung
cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = 'b')
#pruning
prune.boston <- prune.tree(tree.boston, best = 5)
plot(prune.boston)
text(prune.boston, pretty = 0, cex = 0.6)

#make predictions on the test set (using unpruned tree)
yhat <- predict(tree.boston, newdata = Boston[-train,])
boston.test <- Boston[-train, "medv"]
plot(yhat, boston.test)
abline(0 ,1)
mean((yhat-boston.test)^2)

########################################################################
#Bagging and Boosting
########################################################################
library(randomForest)

#bagging
set.seed(1)
bag.boston <- randomForest(medv~., data = Boston, sebset = train, 
                           mtry =13, importance = TRUE) #mtry : #of predictors to use
yhat.bag <- predict(bag.boston, newdata = Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag - boston.test)^2)

#changing the number of trees used to randomForest()
bag.boston <- randomForest(medv~., data = Boston, subset = train, 
                           mtry = 13, ntree = 25)
yhat.bag <- predict(bag.boston, newdata = Boston[-train,])
mean((yhat.bag - boston.test)^2)

#random forest
set.seed(1)
rf.boston <- randomForest(medv~., data = Boston, subset = train, 
                          mty = 6, importance = TRUE)
yhat.rf <- predict(rf.boston, newdata = Boston[-train,])
mean((yhat.rf - boston.test)^2)

importance(rf.boston) #importance of variables
varImpPlot(rf.boston)
#######################################################################
#Boosting
#######################################################################
library(gbm)

set.seed(1)
boost.boston <- gbm(medv~., data = Boston[train,], 
                    distribution = "gaussian",  n.trees = 5000, 
                    interaction.depth = 4)
#distribution = "bernouli" for classification
summary(boost.boston)

#plotting
par(mfrow = c(1,2))
plot(boost.boston, i = "rm")
plot(boost.boston, i = "lstat")

#make predictions on test set
yhat.boost <- predict(boost.boston, 
                      newdata = Boston[-train,], n.trees = 5000)
mean((yhat.boost - boston.test)^2)

#redoing the boosting with a different lambda
boost.boston <- gbm(medv~., data = Boston[train,], 
                    distribution = "gaussian", n.trees = 5000,
                    interaction.depth = 4, shrinkage = 0.2, 
                    verbose = FALSE)
yhat.boost <- predict(boost.boston, newdata = Boston[-train,], 
                      n.trees = 5000)
mean((yhat.boost - boston.test)^2)
