library(ISLR)
fix(Hitters)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters))

Hitters <- na.omit(Hitters)

library(leaps)
regfits.full <- regsubsets(Salary~., data = Hitters, nvmax = 19)
reg.summary <- summary(regfits.full)
plot(reg.summary$adjr2, type='l')
reg.summary$rsq
points(which.max(reg.summary$adjr2), reg.summary$adjr2[11], col = 'red', cex = 2, pch = 20)

plot(regfits.full, scale = "bic")

#Forward and Backward
regfit.fwd <- regsubsets(Salary~., data = Hitters, method = "forward")
summary(regfit.fwd)

regfit.bwd <- regsubsets(Salary~., data = Hitters, method = "backward")
summary(regfit.bwd)

#Validation set and CV
set.seed(1)
train <- sample(c(T, F), nrow(Hitters), rep = T)
test <- (!train)
regfit.best <- regsubsets(Salary~., data = Hitters[train,], nvmax = 19)

test.mat <- model.matrix(Salary~., data = Hitters[test,])
val.errors <- rep(NA, 19)
for(i in 1:19){
        coefi <- coef(regfit.best, id = i)
        pred <- test.mat[, names(coefi)]%*%coefi
        val.errors[i] <- mean((Hitters$Salary[test]-pred)^2)
}

val.errors
plot(val.errors, type = "l")
coef(regfit.best, 10)

#function to do previous lines
predict.regsubsets <- function(object, newdata, id, ...){
        form <- as.formula(object$call[[2]])
        mat <- model.matrix(form, newdata)
        coefi <- coef(object, id=id)
        xvars <- names(coefi)
        mat[,xvars]%*%coefi
}


regfit.best <- regsubsets(Salary~., data = Hitters, nvmax = 19)
coef(regfit.best, 10)

#CV
k <- 10
set.seed(1)
folds <- sample(1:k, nrow(Hitters), replace = T)
cv.errors <- matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))

for(j in 1:k){
        best.fit <- regsubsets(Salary~., data = Hitters[folds!=j,], nvmax = 19)
        for(i in 1:19){
                pred <- predict(best.fit, Hitters[folds == j,], id=i)
                cv.errors[j,i] <- mean((Hitters$Salary[folds == j]-pred)^2)
        }
}

#Avg over cols 
mean.cv.errors <- apply(cv.errors, 2, mean)
which.min(mean.cv.errors)
plot(mean.cv.errors, type  = 'b')

reg.best <- regsubsets(Salary~., data = Hitters, nvmax = 19)
coef(reg.best, 11)

###############################################################
##
##Lab2
library(glmnet)
x <- model.matrix(Salary~., Hitters)[,-1]
y <- Hitters$Salary

#Ridge  #alpha=0
grid <- 10^seq(10, -2, length = 100)
ridge.mod = glmnet(x, y, alpha = 0, lambda = grid)
#l2 norm of 60th lambda value in the grid
sqrt(sum(coef(ridge.mod)[-1,60]^2))

#train/test sets
set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]

ridge.mod <- glmnet(x[train,], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
#to compute the test MSE
ridge.pred <- predict(ridge.mod, s = 4, newx = x[test,])
#MSE
mean((ridge.pred - y.test)^2)

##choosing lambda tuning parameter by cv
set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha =0)
plot(cv.out)
bestlambda <- cv.out$lambda.min

#computing test MSE with this lambda
ridge.pred <- predict(ridge.mod, s = bestlambda, newx = x[test,])
mean((ridge.pred - y.test)^2)

out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlambda)[1:20,]


##lasso #alpha = 1
lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)

#CV
set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 1)
plot(cv.out)
bestlambda <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s = bestlambda, newx = x[test,])
mean((lasso.pred - y.test)^2)

out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients", s = bestlambda)[1:20,]
lasso.coef


#########################################################################
##
##PCR
library(pls)
set.seed(2)
pcr.fit <- pcr(Salary~., data = Hitters, scale = TRUE, validation = "CV")
summary(pcr.fit)

validationplot(pct.fit, val.type = "MSEP")

#PCR on training data
set.seed(1)
pcr.fit <- pcr(Salary~., data = Hitters, subset = train, scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")
#test MSE
pcr.pred <- predict(pcr.fit, x[test,], ncomp = 7)
mean((pcr.pred - y.test)^2)
#pcr on full dataset
pcr.fit <- pcr(y~x, scale = TRUE, ncomp = 7)
summary(pcr.fit)

##PLS
library(pls)
set.seed(1)
pls.fit <- plsr(Salary~., data = Hitters, subset = train, scale = TRUE, validation = "CV")
summary(pls.fit)
#test MSE
pls.pred <- predict(pls.fit, x[test,], ncomp = 2)
mean((pls.pred - y.test)^2)
#pls on full dataset
pls.fit <- plsr(Salary~., data = Hitters, scale = TRUE, ncomp = 2)
summary(pls.fit)
