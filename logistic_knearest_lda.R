library(ISLR)
#logistic regression
names(Smarket)
summary(Smarket)
?Smarket
pairs(Smarket, col=Smarket$Direction)
#Logistic Regression

glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial)
summary(glm.fit)
#null deviance is grand mean (only the intercept) in the model (chi square: high is high diff between observed and expected)
#residual is with our independent variables (also chi square result basically, probably should be a big diff between null and residual of good model)

glm.probs=predict(glm.fit, type="response")
glm.probs[1:5]
glm.pred=ifelse(glm.probs>0.5, "Up","Down")
attach(Smarket)
table(glm.pred,Direction)
mean(glm.pred==Direction)

#make train and test set
train=Year<2005

glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial, subset=train)
glm.probs=predict(glm.fit, nedata=Smarket[!train,] , type="response")
glm.pred=ifelse(glm.probs>0.5, "Up", "Down")
Direction.2005=Smarket$Direction[!train] #basically making test data , little weird
table(glm.pred, Direction.2005) #this isn't workign but basically we over fit

#fit smaller model
glm.fit=glm(Direction~Lag1+Lag2, data=Smarket, family=binomial, subset=train)
glm.probs1=predict(glm.fit, newdata=Smarket[!train,] , type="response")
glm.pred1=ifelse(glm.probs1>0.5, "Up", "Down")
table(glm.pred1, Direction.2005)
mean(glm.pred1==Direction.2005)
# 0.5595238 proportion on test data

summary(glm.fit)

#linear discriminant analysis
library(MASS)
library(ISLR)

lda.fit=lda(Direction~Lag1+Lag2, data=Smarket, subset=Year<2005)
lda.fit
#lda fits a linear function to seperate the two groups

plot(lda.fit)
smarket.2005=subset(Smarket,Year==2005)
lda.pred=predict(lda.fit,smarket.2005)
lda.pred[1:5,]
class(lda.pred)
data.frame(lda.pred)[1:5,]
table(lda.pred$class, smarket.2005$Direction) #lda.pred$class is the true classification, vs pred

#creates proportion of true/false
mean(lda.pred$class==smarket.2005$Direction)

#k nearest neighbors (do best about 1/3 of the time)
library(class)
?knn
attach(Smarket)

xlag=cbind(Lag1,Lag2)
train=Year<2005
knn.pred=knn(xlag[train,], xlag[!train,], Direction[train],k=1)
table(knn.pred,Direction[!train])
mean(knn.pred==Direction[!train])
#0.5 so useless, coin flip, so now we increase the k nearest neighbor? 

knn.pred=knn(xlag[train,], xlag[!train,], Direction[train],k=2)
table(knn.pred,Direction[!train])
mean(knn.pred==Direction[!train])



