#a.
set.seed(21)
X = rnorm(100)
ep = rnorm(100)

#b.
Y = 1 + 3.7 * X + -0.25 * X^2 + 1.04 * X^3 + ep

#c.
library(leaps)
df <- data.frame(Y=Y,X=X)

regfit.full <- regsubsets(Y~poly(X, degree = 10, nvmax = 10), data = df)
reg.summary <- summary(regfit.full)

which.max(reg.summary$adjr2)
which.max(reg.summary$rsq)
which.min(reg.summary$cp)
which.min(reg.summary$bic)

par(mfrow = c(2,2))
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R2", type = "l")
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "CP", type = "l")
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")



par(mfrow = c(1,1))
plot(regfit.full, scale = "adjr2")
