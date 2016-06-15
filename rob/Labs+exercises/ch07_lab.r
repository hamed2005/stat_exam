library(ISLR)
attach(Wage)

fit = lm(wage~poly(age, 4), data = Wage)
coef(summary(fit))

fit2 = lm(wage~poly(age, 4, raw = TRUE), data = Wage)
coef(summary(fit))

agelims = range(age)
age.grid = seq(from = agelims[1], to = agelims[2])
preds = predict(fit, newdata = list(age = age.grid), se = TRUE)
se.bands = cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)

par(mfrow = c(1,2), mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 4, 0))
plot(age, wage, xlim = agelims, cex = 0.5, col = 'darkgrey')
title("Degree-4 Polynomial", outer = T)
lines(age.grid, preds$fit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)



preds2 = predict(fit2, newdata = list(age = age.grid), se = TRUE)
plot(age, wage, xlim = agelims, cex = 0.5, col = 'darkgrey')
lines(age.grid, preds2$fit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)
################################################################################
# using anova

fit.1 = lm(wage ~ poly(age, 1), data = Wage)
fit.2 = lm(wage ~ poly(age, 2), data = Wage)
fit.3 = lm(wage ~ poly(age, 3), data = Wage)
fit.4 = lm(wage ~ poly(age, 4), data = Wage)
fit.5 = lm(wage ~ poly(age, 5), data = Wage)

anova(fit.1, fit.2, fit.3, fit.4, fit.5)
#poly function also provides p-values when raw=F (orthogonal polynomials)

fit = glm(I(wage>250)~poly(age,4), data = Wage, family = binomial)
preds = predict(fit, newdata = list(age = age.grid), se = TRUE)

#to calculate the confidence intervals
pfit = exp(preds$fit)/(1+exp(preds$fit))
se.bands.logit = cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)
se.bands = exp(se.bands.logit)/(1+exp(se.bands.logit))

#we can also do it using type = "response" --but it will lead to negative probabilites!
#preds = predict(fit, newdata = list(age = age.grid), type = "response", se = TRUE)

par(mfrow = c(1,1))
plot(age, I(wage > 250), xlim = agelims, type = "n", ylim = c(0,.2))
points(jitter(age), I((wage > 250)/5), cex = .5, pch = "|", col = "darkgrey")
lines(age.grid, pfit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

#to fit a step function
table(cut(age,4))
fit = lm(wage ~ cut(age, 4), data = Wage)
coef(summary(fit))


pred = predict(fit, newdata = list(age = age.grid), se = TRUE)
se.bands <- cbind(pred$fit + 2*pred$se.fit, pred$fit - 2*pred$se.fit)
plot(age, wage, col = "grey")
lines(age.grid, pred$fit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)


###################################################################################
# splines

library(splines)
fit <- lm(wage ~ bs(age, knots = c(25, 40, 60)), data = Wage)
pred <- predict(fit, newdata = list(age = age.grid), se = T)
plot(age, wage, col = "grey")
lines(age.grid, pred$fit, lwd = 2, col = "blue")
lines(age.grid, pred$fit + 2*pred$se.fit, lty = "dashed")
lines(age.grid, pred$fit - 2*pred$se.fit, lty = "dashed")

dim(bs(age, knots = c(25, 40, 60)))
# uniform splines
dim(bs(age, df = 6))
attr(bs(age, df = 6), "knots")

#natural spline
fit2 <- lm(wage ~ ns(age, df = 4), data = Wage)
pred2 <- predict(fit2, newdata = list(age = age.grid), se = T)
lines(age.grid, pred2$fit, col = "red", lwd = 2)

#smoothing spline
plot(age, wage, xlim = agelims, cex = 0.5, col = "darkgrey")
title("Smoothing Spline")
fit = smooth.spline(age, wage, df = 16)
fit2 = smooth.spline(age, wage, cv = TRUE)
fit2$df 
lines(fit, col = "red", lwd = 2)
lines(fit2, col = "blue", lwd = 2)
legend("topright", legend = c("16 DF", "6.8 DF"), col = c("red", "blue"), lty = 1, lwd = 2, cex = 0.8)

#local regression
plot(age, wage, col = "darkgrey", cex = 0.5, xlim = agelims)
title("Local Regression")
fit = loess(wage ~ age, span = 0.2, data = Wage)
fit2 = loess(wage ~ age, span = 0.5, data = Wage)
lines(age.grid, predict(fit, data.frame(age = age.grid)), col = "red", lwd = 2)
lines(age.grid, predict(fit2, data.frame(age = age.grid)), col = "blue", lwd = 2)
legend("topright", legend = c("Span = 0.2", "Span = 0.5"), col = c("red", "blue"), lty = 1, lwd = 2, cex = 0.8)

