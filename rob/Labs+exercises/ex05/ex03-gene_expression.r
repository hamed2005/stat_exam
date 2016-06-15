library(glmnet)

setwd("/home/hamed/KUL/SEM2/stat/Rob/Labs+exercises/vijver/")
load("VIJVER.Rdata")
data$meta <- factor(data$meta)

#dummy coding : DM->1 , NODM->0
phenotype = ifelse(data$meta == "DM", 1, 0)
data <- cbind(phenotype,data[,-1])

#logistic regression
logit.fit <- glm(phenotype~., data = data, family = binomial(link = "logit"))        #not converging

cor <- cor(data)
table((cor[1,] > 0.5)[-1])

names(data)[cor[1,]>0.4][-1] #then check them with rcorr
# [1] "NM_003258" "NM_001168"

logit.gene1 <- glm(phenotype ~ NM_003258, data = data, family = binomial(link = "logit"))
summary(logit.gene1)

logit.gene2 <- glm(phenotype ~ NM_001168, data = data, family = binomial(link = "logit"))
summary(logit.gene2)

        #to check the goodness of fit
library(ResourceSelection)
hoslem.test(data$phenotype, fitted(logit.gene1))


#spearman correlation
library(Hmisc)

spear_corrs <- rep(NA, ncol(data)-1)
pvals <- rep(NA, ncol(data)-1)

for (i in 2:ncol(data)){
        spear_cor <- rcorr(cbind(data$phenotype, data[,i]), type = "spearman")
        spear_corrs[i] <- spear_cor$r[1,2]
        pvals[i] <- spear_cor$P[1,2]
}
genes_high_spearman <- names(data)[which(spear_corrs > 0.4)]

pvals[which(spear_corrs > 0.4)]

formula <- paste("phenotype~", paste(genes_high_spearman, collapse = '+'), sep = '')
logit.fit <- glm(formula, data = data, family = binomial(link = "logit"))
summary(logit.fit)

#check the genes with cor > 0.4
rcorr(cbind(data[,1], data$NM_003258), type = "spearman")
rcorr(cbind(data[,1], data$NM_001168), type = "spearman")

corgenes <- paste((names(gdata)[cor[1,]>0.3])[-1], collapse ="+") #then check them with rcorr

formula <- as.formula(paste("meta ~ ", corgenes, sep = ""))
logit.corgenes <- glm(formula, data = data, family = binomial(link = "logit"))
summary(logit.corgenes)

#just for test
rcorr(cbind(data[,1], data$NM_003920), type = "spearman")


#################################################################################################
##check for multicollinearity
diag(cor) <- NA
table(cor[-1,-1] > 0.5)
table(cor[-1,-1] > 0.9)

#################################################################################################
#samplint -train/test
set.seed(1)
train <- sample(c(T, F), nrow(data), rep = T)
test <- (!train)

##ridge
library(glmnet)
x <- model.matrix(phenotype~., data)[,-1]
y <- data$phenotype

ridge.mod <- glmnet(x, y, alpha = 0)
plot(ridge.mod)
sqrt(sum(coef(ridge.mod)[-1,60]^2))


#train/test sets
set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]

##choosing lambda tuning parameter by cv
set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha =0)
plot(cv.out)
bestlambda <- cv.out$lambda.min         #[1] 59.90289

ridge.mod <- glmnet(x[train,], y[train], alpha = 0, lambda = 60)
#computing test MSE with this lambda
ridge.pred <- predict(ridge.mod, s = bestlambda, newx = x[test,])
mean((ridge.pred - y.test)^2)

out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlambda)[1:20,]


####################################################################


#lasso
lasso.mod <- glmnet(x, y, alpha = 1, family = "binomial")
plot(lasso.mod)
sqrt(sum(coef(lasso.mod)[-1,60]^2))

set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha =1, family = "binomial")
plot(cv.out)
bestlambda <- cv.out$lambda.min         #[1] 0.1203589

lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = bestlambda)
#computing test MSE with this lambda
lasso.pred <- predict(lasso.mod, s = bestlambda, newx = x[test,])
mean((lasso.pred - y.test)^2)

out <- glmnet(x, y, alpha = 1, lambda = bestlambda)
lasso.coefs <- predict(out, type = "coefficients", s = bestlambda)

lasso.coefs[lasso.coefs != 0]

plot(lasso.mod)

#PCR
library(pls)
set.seed(2)
pcr.fit <- pcr(phenotype~., data = data, scale = TRUE, validation = "CV")
summary(pcr.fit)        #PC = 10 has lowest CV error

validationplot(pcr.fit, val.type = "MSEP")
#PCR on training data
set.seed(1)
pcr.fit <- pcr(phenotype~., data = data, subset = train, scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")
#test MSE
pcr.pred <- predict(pcr.fit, x[test,], ncomp = 10)
mean((pcr.pred - y[test])^2)
#pcr on full dataset
pcr.fit <- pcr(y~x, scale = TRUE, ncomp = 10)
summary(pcr.fit)

#PLSR
###################################################################################################
##As an additional step: calculate all logistic regressions and check if its significant

sig_vars <- rep(FALSE, ncol(data))
pvals <- rep(NA, ncol(data))
for (i in 2:ncol(data)){
        formula <- paste("phenotype ~", names(data)[i], collapse = '', sep = '')
        logit.fit <- glm(formula, data = data, family = binomial(link = "logit"))
        sig_vars[i] <- TRUE
        pvals[i] <- summary(logit.fit)$coef[2,4]
}
table(pvals<0.001)
