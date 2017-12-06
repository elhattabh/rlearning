library(ISLR)
library(boot)
?cv.glm

plot(mpg~horsepower,data=Auto)

#LOOCV
glm.fit=glm(mpg~horsepower,data=Auto)
summary(glm.fit)

cv.glm(Auto,glm.fit)$delta #pretty slow, doesnt use formula 5.2 on page 180

#lets write our own simple function to use formula (5.2)
loocv=function(fit){
  h=lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
}

#now we try it out
loocv(glm.fit)
#works

cv.error=rep(0,5)
degree=1:5
for(d in degree){
  glm.fit=glm(Auto$mpg~poly(Auto$horsepower,d), data=Auto)
  cv.error[d]=loocv(glm.fit)
}

plot(degree, cv.error, type="b")

#10 fold cross validation
cv.error10=rep(0,5) #like saying cv.error10=[ ]
for (d in degree){
  glm.fit=glm(mpg~poly(horsepower,d), data=Auto)
  cv.error10[d]=cv.glm(Auto, glm.fit, K=10)$delta[1]
}
lines(degree, cv.error10, type="b",col="red")


#bootstrap (exmaple 5.2)

#just the nonlinear formula
alpha=function(x,y){
  vx=var(x)
  vy=var(y)
  cxy=cov(x,y)
  (vy-cxy)/(vx+vy-2*cxy)
}

#what is the standard variable of alpha?
alpha(Portfolio$X, Portfolio$Y)

#to use the bootstrap, now we need to make a function

alpha.fn=function(data, index){ #data frame, rows of the data
  with(data[index,], alpha(X,Y))
}
#with command says using the data in the data frame, execute the following commands

alpha.fn(Portfolio, 1:100)
#we get same number as before

set.seed(1)
alpha.fn(Portfolio, sample(1:100, 100, replace=TRUE))
#do a thousand bootstraps
boot.out=boot(Portfolio, alpha.fn, R=1000)
boot.out
#now we have the standard error

plot(boot.out)
