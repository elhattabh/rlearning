library(ISLR)
library(MASS)
library(car)

fix(Boston) #not working bec of xquartz package not running on this version of R
?Boston
lm.fit=lm(Boston$medv~Boston$lstat)
coef(lm.fit)
confint(lm.fit)
predict(lm.fit, data.frame(lstat=c(5,10,15)), interval = "confidence")
predict(lm.fit, data.frame(lstat=c(5,10,15)), interval = "prediction")
plot(Boston$lstat^2, Boston$medv)
abline(lm.fit)
abline(lm.fit, lwd=3)
abline(lm.fit, lwd=3,col="blue")
plot(Boston$lstat, medv, col="red")
plot(Boston$lstat, medv, pch=20)
plot(Boston$lstat, medv, pch="+")
par(mfrow=c(2,2))
plot(lm.fit)
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))
#multi linear
lm.fit=lm(medv~Boston$lstat + age, data=Boston)
summary(lm.fit)
vif(lm.fit)
fit1=lm(medv~.-age, data=Boston)
fit1=update(fit1, ~.-indus)
#interacting lstat and age and seeing what's up
summary(lm(medv~lstat*age, data=Boston))
plot(lm(medv~lstat*age, data=Boston))
#see a few high leverage points

#non linear transformations
fit2=lm(medv~Boston$lstat + I(Boston$lstat^2))
summary(fit2)
lm.fit=lm(medv~Boston$lstat)
#hypothesis testing the linear / quadratic to see which is better, it is clear
anova(lm.fit,fit2)
lm.fit5=lm(medv~poly(Boston$lstat,5))
summary(lm.fit5)
#log
summary(lm(medv~log(rm), data=Boston))

#Qualitative predictors
names(Carseats)
lm.fit=lm(Sales~ .+ Income:Advertising + Price:Age, data=Carseats)
summary(lm.fit)
contrasts(Carseats$ShelveLoc)

