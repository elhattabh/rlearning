library(dplyr)
#Lesson 1 Intro to stats learning

x=c(2,7,5)
y=seq(from=4, length=3, by=3)
?seq
y
x+y
x^y
x[2]
x[0]
x[-2]

z=matrix(seq(1,12),4,3)
z
z[3:4,2]
z[3:4,2:3]
dim(z)
ls()
x=runif(50)
y=rnorm(50)
head(y,10)

z=matrix(seq(1,12),4,3)
z
z[3:4,2:3]

plot(x,y,xlab="Random uniform", ylab='random normal', pch="*", col="blue")
#you can make two plots on top of each other: two rows 1 col
par(mfrow=c(2,1))
par(mfrow=c(3,1))
par(mfrow=c(1,1))

hist(y)

auto=read.csv("/Users/konafa/Desktop/Github/IntroToStatisticalLearningR-/data/Auto.csv")
names(auto)
dim(auto)
class(auto)
summary(auto)
plot(auto$cylinders,auto$mpg)

attach(auto)

auto=read.csv("/Users/konafa/Desktop/Github/IntroToStatisticalLearningR-/data/Auto.csv", header=T, na.strings="?")
dim(auto)
auto=na.omit(auto)

library(dplyr)
distinct_cyl = auto %>% distinct(cylinders)
distinct_cyl

plot(cylinders, mpg, col="red", varwidth=T, horizontal=T)
hist(mpg)
hist(mpg, col=7)
hist(mpg, col=7, breaks=16)

boxplot(mpg~cyl,data=mtcars, main="Car Milage Data", 
        xlab="Number of Cylinders", ylab="Miles Per Gallon")

pairs((auto))
pairs(~mpg + displacement + horsepower + weight + acceleration, auto)
pairs(~mpg + displacement, auto)

plot(horsepower, mpg)

identify(horsepower,mpg,name)
