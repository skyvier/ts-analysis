data <- read.csv("data/TestSource.csv")
fit <- lm(Y ~ X, data=data)

write.csv(fit$fitted.values, "data/FittedValues.csv", row.names=FALSE)
write.csv(fit$residuals, "data/Residuals.csv", row.names=FALSE)
write.csv(fit$coef, "data/Coefs.csv", row.names=FALSE)
