library(ISLR)
set.seed(22)

attach(Default)

#5
#a.
fit.lor <- glm(default ~ income + balance, family = "binomial")

#b.
#i.
dim(Default)[1] #1000
training <- sample(1000, 500)
#.ii
fit.lor.val <- glm(default ~ income + balance, family = "binomial", subset = training)
#iii.
probabilities <- predict(fit.lor.val, newdata = Default[-training, ], type = "response")
predict <- rep("No", length(probabilities))
predict[probabilities > 0.5] <- "Yes"
#iv.
error <- mean(predict != Default[-training, ]$default)

#c.
training <- sample(1000, 500)
fit.lor.val <- glm(default ~ income + balance, family = "binomial", subset = training)
probabilities <- predict(fit.lor.val, newdata = Default[-training, ], type = "response")
predict <- rep("No", length(probabilities))
predict[probabilities > 0.5] <- "Yes"
error1 <- mean(predict != Default[-training, ]$default)

training <- sample(1000, 500)
fit.lor.val <- glm(default ~ income + balance, family = "binomial", subset = training)
probabilities <- predict(fit.lor.val, newdata = Default[-training, ], type = "response")
predict <- rep("No", length(probabilities))
predict[probabilities > 0.5] <- "Yes"
error2 <- mean(predict != Default[-training, ]$default)

training <- sample(1000, 500)
fit.lor.val <- glm(default ~ income + balance, family = "binomial", subset = training)
probabilities <- predict(fit.lor.val, newdata = Default[-training, ], type = "response")
predict <- rep("No", length(probabilities))
predict[probabilities > 0.5] <- "Yes"
error3 <- mean(predict != Default[-training, ]$default)

#d.
training <- sample(1000, 500)
fit.lor.val <- glm(default ~ income + balance + student, family = "binomial", subset = training)
probabilities <- predict(fit.lor.val, newdata = Default[-training, ], type = "response")
predict <- rep("No", length(probabilities))
predict[probabilities > 0.5] <- "Yes"
error <- mean(predict != Default[-training, ]$default)

#6.
set.seed(22)

#a.
fit.lor <- glm(default ~ income + balance, family = "binomial")
summary(fit.lor)

#b.
boot.fn <- function(data, index){
        return (coef(glm(default~income + balance, data = data, subset = index, family = "binomila")))
}



#c.

library(boot)
boot(Default, boot.fn, 5000)
