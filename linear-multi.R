#Regressions
library(MASS)
library(ISLR)

#linear regression
names(Boston)
?Boston
head(Boston$black,10)
plot(medv~lstat, data=Boston)
fit1=lm(medv~lstat, data=Boston)
fit1
summary(fit1)
abline(fit1,col="red")
names(fit1)
confint(fit1)
predict(fit1,data.frame(lstat=c(5,10,15)), interval="confidence")
plot(fit1)

#Multiple Linear Regression
fit2=lm(medv~lstat+age, data=Boston)
fit2
summary(fit2)
fit3=lm(medv~., Boston)
summary(fit3)
plot(fit3)
fit4=update(fit3, ~.-age-indus)
summary(fit4)
fitrm=lm(medv~rm, data=Boston)
summary(fitrm)
plot(fitrm)

#Nonlinear terms and interactions
fit5=lm(medv~ lstat*age, Boston)
summary(fit5)
fit6=lm(medv~lstat + I(lstat^2), Boston); summary(fit6) #the I() is to make the quadratic
summary(fit6)

par(mfrow=c(1,1))
attach(Boston)

plot(medv~lstat)
#to include the quadratic and get the fitted value
#for each value of lstat, we get the value of the model
points(lstat, fitted(fit6), col="red", pch=21) #pch is the plotting character

#use poly function
fit7=lm(medv~poly(lstat,2)) #polynomoial of degree 4 of lstat, clearly overfitting
points(lstat,fitted(fit7),col="blue", pch=20)

fit7=lm(medv~poly(lstat,3)) #polynomoial of degree 4 of lstat, clearly overfitting
points(lstat,fitted(fit7),col="green", pch=20)
summary(fit7)
#^ the standard error is high, so this is bad

plot(1:20, 1:20, pch=1:20, cex=2) #cex=2 means double the size

#Qualitative predictors
fix(Carseats)
fit1= lm(Sales~.+Income:Advertising + Age: Price, Carseats) #interaction by colon
summary(fit1)
contrasts(Carseats$ShelveLoc)

#writing functions
regplot=function(x,y){
  fit=lm(y~x)
  plot(x,y)
  abline(fit,col="red")
  summary(fit)
}

attach(Carseats)
regplot(Price,Sales)

regplot=function(x,y, ...){
  fit=lm(y~x)
  plot(x,y,...)
  abline(fit,col="red")
  summary(fit)
}

regplot(Price,Sales, xlab="Price", ylab="Sales", col="Blue", pch=15)
