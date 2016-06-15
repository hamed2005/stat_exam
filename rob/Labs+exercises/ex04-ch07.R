library(MASS)

fit = lm(nox ~ poly(dis, 3), data = Boston)
summary(fit)

dislims = range(Boston$dis)
dis.grid = seq(from = dislims[1], to = dislims[2])
preds = predict(fit, newdata = list(dis = dis.grid), se = TRUE)
se.bands = cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)

#par(mfrow = c(1,2), mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 4, 0))
plot(Boston$dis, Boston$nox, xlim = dislims, cex = 0.5, col = 'darkgrey')
title("Cubic Polynomial", outer = F)
lines(dis.grid, preds$fit, lwd = 2, col = "blue")
matlines(dis.grid, se.bands, lwd = 1, col = "blue", lty = 3)


##########
#plot polynomials 1 to 10 , and report rss

fit.1 = lm(nox ~ poly(dis, 1), data = Boston)
fit.2 = lm(nox ~ poly(dis, 2), data = Boston)
fit.3 = lm(nox ~ poly(dis, 3), data = Boston)
fit.4 = lm(nox ~ poly(dis, 4), data = Boston)
fit.5 = lm(nox ~ poly(dis, 5), data = Boston)
fit.6 = lm(nox ~ poly(dis, 6), data = Boston)
fit.7 = lm(nox ~ poly(dis, 7), data = Boston)
fit.8 = lm(nox ~ poly(dis, 8), data = Boston)
fit.9 = lm(nox ~ poly(dis, 9), data = Boston)
fit.10 = lm(nox ~ poly(dis, 10), data = Boston)

sum(fit.1$residuals^2)


#anova to select the best poly
anova(fit.1, fit.2, fit.3, fit.4, fit.5, fit.6, fit.7, fit.8, fit.9, fit.10)


#using bs to fit
library(splines)
fit <- lm(nox ~ bs(dis, df = 4), data = Boston)
pred <- predict(fit, newdata = list(dis = dis.grid), se = T)
plot(Boston$dis, Boston$nox, col = "grey")
lines(dis.grid, pred$fit, lwd = 2, col = "blue")
lines(dis.grid, pred$fit + 2*pred$se.fit, lty = "dashed")
lines(dis.grid, pred$fit - 2*pred$se.fit, lty = "dashed")



preds = predict(fit, newdata = list(dis = dis.grid), se = TRUE)
se.bands = cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)


#par(mfrow = c(1,2), mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 4, 0))
plot(Boston$dis, Boston$nox, xlim = dislims, cex = 0.5, col = 'darkgrey')
title("Cubic Polynomial", outer = F)
lines(dis.grid, preds$fit, lwd = 2, col = "blue")
matlines(dis.grid, se.bands, lwd = 1, col = "blue", lty = 3)