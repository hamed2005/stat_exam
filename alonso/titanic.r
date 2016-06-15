setwd("~/KUL/SEM2/stat")

## Titanic-complete
titanic.cd <- read.table("Titanic-complete.txt")

cor <- cor(titanic.cd)

## CD Analysis
mylogit <- glm(survived ~ class + sex + age, data = titanic.cd, family = binomial(link="logit"))
summary(mylogit)
# # age is not significant, so let's discretize it and redo the analysis
# library(arules)
# titanic.cd$age <- discretize(titanic.cd$age, method = "interval"
#                              , categories = 3)
# titanic.cd$age <-as.factor(titanic.cd$age)
# mylogit <- glm(survived ~ class + sex + age, data = titanic.cd, family = binomial(link="logit"))
# summary(mylogit)

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

## multiple imputation
imp <- mice(titanic.missing, m = 5, printFlag = FALSE, method = "norm")
plot(densityplot(imp))
lines(densityplot(titanic.cd$age))
print(imp)

## stripplot
comp <- complete(imp, "long", inc = TRUE)
col <- rep(c("blue", "red")[1+as.numeric(is.na(imp$data$age))], 101)
stripplot(age~.imp, data=comp, jit=TRUE, fac= 0.8, col=col, pch=20
          , cex=1.4, xlab="Imputation number")

## analyzing imputed data
fit <- with(data = imp, exp = glm(survived ~ class + sex + age + age*class, family = binomial))
summary(fit)
MI.matrix <- matrix(0,5,4)
for (k in 1:5){
        MI.matrix[k,] <- coefficients(fit$analyses[[k]])
}

MI.result <- data.frame(Intercept=MI.matrix[,1], class=MI.matrix[,2]
                        , sex=MI.matrix[,3], age=MI.matrix[,4])

MI.result[1:10,]

## combining the result with rubin rule
est <- pool(fit)
summary(est)

#pool.compare(fit, fit2, method = "Wald")$pvalue


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

## fitting a logistic regression to calculate the weight --sex is not significant
ipw.logit <- glm(r ~ class + survived + sex, family = binomial, data = titanic.missing)
summary(ipw.logit) ## both class and survived are significant so this is MAR case

## calculating the weights
w <- 1/fitted(ipw.logit)
## augmenting titanic data frame with w column
titanic.missing <- cbind(titanic.missing, w)

## fit using IPW
titanic.ipw <- glm(survived ~ class + sex + age, data = titanic.missing
                   , weights = titanic.missing$w, family = binomial)
summary(titanic.ipw)

## fit without age --age was not significant
titanic.ipw <- glm(survived ~ class + sex, data = titanic.missing
                   , family = binomial)
summary(titanic.ipw)
## odds ratio analysis
exp(cbind(OR = titanic.ipw$coefficients, confint(titanic.ipw)))

##########################################################
#### extra analysis

# desicion tree
library(rpart)
titanic.tree <- rpart(survived~., data = titanic.cd)
summary(titanic.tree)

# random forest
library(randomForest)
titanic.rf <- randomForest(as.factor(survived)~., data = titanic.cd)
summary(titanic.rf)

# bayesian glm
library(arm)
titanic.bglm <- bayesglm(survived~., data = titanic.cd)
summary(titanic.bglm)

# Mixed Models #### Incomplete
library(lme4)
titanic.lmer1 <- lmer(survived ~ 1 + sex + (1 + age|class), REML = FALSE, data = titanic.cd)


# all subset logistic regression
library(glmulti)

glmulti.logistic.out <-
        glmulti(survived ~., data = titanic.cd,#missing[,1:4],#, weights = titanic.missing$w,
                level = 2,               # 1 = main effects, 2 = level 2 effects
                method = "h",            # Exhaustive approach
                crit = "aic",            # AIC as criteria
                confsetsize = 5,         # Keep 5 best models
                plotty = F, report = F,  # No plot or interim reports
                fitfunction = "glm",     # glm function
                family = binomial)       # binomial family for logistic regression

    #for MI
glmulti.logistic.out.mi <-
        glmulti(survived ~., data = imp[[2]],#, weights = titanic.missing$w,
                level = 2,               # 1 = main effects, 2 = level 2 effects
                method = "h",            # Exhaustive approach
                crit = "aic",            # AIC as criteria
                confsetsize = 5,         # Keep 5 best models
                plotty = F, report = F,  # No plot or interim reports
                fitfunction = "glm",     # glm function
                family = binomial)       # binomial family for logistic regression

summary(glmulti.logistic.out.mi)
## Show 5 best models (Use @ instead of $ for an S4 object)
glmulti.logistic.out@formulas
summary(glmulti.logistic.out@objects[[1]])

#### Checking if missing pattern is MCAR
library(MissMech)
TestMCARNormality(titanic.missing[,1:4]) ## MCAR rejected

library(VIM)
spineplot(as.factor(survived)~ age, data = titanic.missing) ##??
spineMiss(titanic.missing[,c("class", "age")])
spineMiss(titanic.missing[,c("survived", "age")])
spineMiss(titanic.missing[,c("sex", "age")])

#for imputed data ???why???
spineMiss(kNN(titanic.missing[,c("survived", "age")]), delimiter = "_imp")


#plotting
logit<-function(x){log(x/(1-x))}
ilogit<-function(x,a,b){exp(a+b*x)/(1+exp(a+b*x))}

col = c("red", "black")
plot(titanic.cd$age,jitter(titanic.cd$survived,.2),pch=20, col= col,
     cex=1.2,xlab="Age",ylab="Status (jittered)")

curve(ilogit(cl[1]+cl[2]*x+cl[3]*0,0,1),add=T)
curve(ilogit(cl[1]+cl[2]*x+cl[3]*1,0,1),add=T,col="red")
legend("topright",pch=20,lty="solid",col=c("red","black"),c("women","men"))


# GAM
library(mgcv)
titanic.gam <- gam (survived ~ class + sex + age, data = titanic.cd, family=quasibinomial(link = "logit"), method="GCV.Cp")
gam.pred <- predict.gam(titanic.gam, newdata = titanicnew, na.action = na.pass, type = "response")
gam.pred <- ifelse(gam.pred <= 0, 0, 1)
summary(titanic.gam)

#plot

plot(jitter(titanic.cd$survived,1)~titanic.cd$age
     , col = as.factor(titanic.cd$class)
     , pch = as.integer(titanic.cd$sex))

library(ggplot2)
dat <- data.frame(X =runif(20),
                  Y =runif(20),
                  att1 = gl(5,20/5),
                  att2 =gl(3,20/3))
ggplot(titanic.cd,aes(x=age,y=jitter(survived,1),color=as.factor(class),shape=as.factor(sex))) +
        geom_point(size=3) 


###Original Dataset

titanic<-read.csv("original_dataset/titanic.csv"
                  , header = T,stringsAsFactors = F)
titanic$pclass[titanic$pclass == 3 | titanic$pclass == 2] <- 0
titanic$sex[titanic$sex == "female"]<-'1'
titanic$sex[titanic$sex == "male"]<-'0'
titanic <- na.omit(titanic)

titanic.logit <- glm(survived ~pclass+sex+age, data = titanic, family = binomial(link = "logit"))
summary(titanic.logit)


###simulation with sampled age

set.seed(55)
newage<-sample(titanic$age, size = 200)
sex <- rbinom(200, 1, 0.5)
class <- rbinom(200, 1, 0.5)
sim.formula <- 2.18 + 1.93*class + 3.04*sex - 0.05*newage
pr <- 1/(1+exp(-sim.formula))
surv <- rbinom(200, 1, pr)
titanic.sim <- data.frame(surv, class, sex, newage)
# #surv.p <- surv >0.5
# #surv.p <- as.integer(surv.p)
# # fit the sim
titanic.sim$surv <- as.factor(titanic.sim$surv)
class <- as.factor(class)
sex <- as.factor(sex)
# 
simlogit <- glm(surv ~ class + sex + newage, family = binomial(link="logit"), data = titanic.sim)
# 
simlogit$coefficients
summary(simlogit)
# 
