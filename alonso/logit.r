setwd("~/KUL/SEM2/stat")

## Titanic-complete
com <- read.table("Titanic-complete.txt")

## CD Analysis
mylogit <- glm(survived ~ class + sex + age, data = com, family = binomial(link="logit"))
summary(mylogit)

#### Titanic-incomplete
titanic.missing <- read.table("Titanic-incomplete.txt")
#summary(titanic.missing)

#### CC Analysis
cc.logit <- glm(survived ~ class + sex + age, family = binomial(), data = titanic.missing)
summary(cc.logit)

#### MI Analysis
library(mice)
library(lattice)

## some analysis
pattern <- md.pattern(titanic.missing)
print(pattern)
pairs <- md.pairs(titanic.missing)
print(pairs)

## multiplr imputation
imp <- mice(titanic.missing, m = 100)
print(imp)

## stripplot
comp <- complete(imp, "long", inc = TRUE)
col <- rep(c("blue", "red")[1+as.numeric(is.na(imp$data$age))], 101)
stripplot(age~.imp, data=comp, jit=TRUE, fac= 0.8, col=col, pch=20
          , cex=1.4, xlab="Imputation number")

## analyzing imputed data
fit <- with(data = imp, exp = glm(survived ~ class + sex + age, family = binomial))
MI.matrix <- matrix(0,100,4)
for (k in 1:100){
        MI.matrix[k,] <- coefficients(fit$analyses[[k]])
}

MI.result <- data.frame(Intercept=MI.matrix[,1], class=MI.matrix[,2]
                        , sex=MI.matrix[,3], age=MI.matrix[,4])

MI.result[1:10,]

## combining the result with rubin rule
est <- pool(fit)
summary(est)

#### IPW Analysis

## loading the data
titanic.missing <- read.table("Titanic-incomplete.txt")
## creating r vector --r=0 : age is missing
age.missing <- is.na(titanic.missing$age)
r <- numeric(200)
for (i in 1: length(age.missing)[1]){
        
        if (age.missing[i]){
                r[i] <- 0
        }
        else {
                r[i] <- 1
        }
}
## augmenting titanic data frame with r column
titanic.missing <- cbind(titanic.missing, r)

## fitting a logistic regression to calculate the weight
ipw.logit <- glm(r ~ class + survived, family = binomial, data = titanic.missing)
summary(ipw.logit) ## both class and survived are significant

## calculating the weights
w <- 1/fitted(ipw.logit)
## augmenting titanic data frame with w column
titanic.missing <- cbind(titanic.missing, w)

## fit age using IPW
titanic.ipw <- glm(survived ~ class + sex + age, data = titanic.missing
                   , weights = titanic.missing$w, family = binomial)
summary(titanic.ipw)
#####################################################################################







# 
# 
# 
# #test
# newdata <- data.frame(class =0, sex =0, age =26)
# test <- predict(mylogit, com[,2:4], type = "response")
# test <- round(test)
# table(test == com$survived)
# 
# 
# #simulation
# 
# 
# set.seed(200)

sex <- rbinom(200, 1, 0.5)
class <- rbinom(200, 1, 0.5)
age <- rnorm(200, 3.26, 0.1)
age <- exp(age)
sim.formula <- 2.18 + 1.93*class + 3.04*sex - 0.05*age
pr <- 1/(1+exp(-sim.formula))
surv <- rbinom(200, 1, pr)
titanic.sim <- data.frame(surv, class, sex, age)
#surv.p <- surv >0.5
#surv.p <- as.integer(surv.p)
# fit the sim
titanic.sim$surv <- as.factor(titanic.sim$surv)
#class <- as.factor(class)
#sex <- as.factor(sex)

simlogit <- glm(surv ~ class + sex + age, family = binomial(link="logit"), data = titanic.sim)

simlogit$coefficients
summary(titanic.sim)

test <- data.frame(cbind(class, sex, age))
#test$class <- as.factor(test$class)
test$sex <- as.factor(test$sex)
test <- predict(mylogit, test, type = "response")
test <- round(test)table(test == com$survived)