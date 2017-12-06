library(ggplot2)
library(dplyr)
library(reshape2)


#forloop
for (year in c(2010,2011,2012,2013,2014,2015)){
  print(paste("The year is", year))
}

#loading a csv
auto=read.csv("/Users/konafa/Desktop/Github/IntroToStatisticalLearningR-/data/Auto.csv")


data(iris)
#column names
names(iris)

#head
head(iris,20)

#nrows and n columns
dim(iris)

#finding unique values
unique(iris$Species)

#converting df to table
newiris <- dplyr::tbl_df(iris)

#removing duplicate rows
iris=dplyr::distinct(iris)

#selecting all except one column
iris2 <- select(iris, -Species, -Sepal.Length)
#selecting multiple columns
iris2 <- select(iris, c(Petal.Length,Species))

#ordering
head(dplyr::arrange(iris, desc(Sepal.Width)))
head(dplyr::arrange(iris, Sepal.Width))

iris1<- dplyr::rename(iris, sepal_width=Sepal.Width)

#where clause
iris1 <- dplyr::filter(iris, Sepal.Length >= 7)

#slice
dplyr::slice(iris1, 10:15)

#column that starts with shrugs 
select(iris, starts_with("Sepal"))

#set operations
dplyr::union(y, z)
dplyr::intersect(y, z)
dplyr::setdiff(y,z)

#joins
dplyr::left_join(a, b, by = "x1")
dplyr::inner_join(a, b, by = "x1")
               
#aggs
dplyr::summarise(iris, avg = mean(Sepal.Length))
dplyr::summarise(iris, sd = sd(Sepal.Length))
dplyr::count(iris, shrug)

#mean of each column
dplyr::summarise_each(iris, funs(mean))

dplyr::summarise_each(iris2, funs(count))

#sum by a specific column
x<-iris1 %>% 
  group_by(Species) %>% 
  summarise(sumsepalwidth = sum(Sepal.Width)) 

#adding a column thats a result of others
head(dplyr::mutate(iris, sepal = Sepal.Length + Sepal.Width),20)

#case when
iris$color = ifelse(iris$Sepal.Length>5, 'green','yellow')
iris$shrug = ifelse(iris$Sepal.Length>2 & iris$Sepal.Width == 3.5, 'this','that' )

dplyr::filter(iris,iris$Sepal.Length>2 & iris$Sepal.Width == 3.5)



iris1 <- dplyr::filter(iris, Sepal.Length >= 7)

#group by 
iris %>%
  group_by(Species) %>%
  summarise(avg = mean(Sepal.Width)) %>%
  arrange(desc(avg))

#groupby with more than one col

iris %>%
  group_by(Species,color,shrug) %>%
  summarise(avg = mean(Sepal.Width)) %>%
  arrange(shrug, desc(avg))

#plotting a correlation matrix
cor(iris$Petal.Width, iris$Petal.Length)
plot(iris$Petal.Width, iris$Petal.Length)
fit2=lm(iris$Petal.Length~iris$Petal.Width)
summary(fit)
plot(fit2)
abline(fit2)

#plots
hist(iris$Petal.Width)
histogram <- ggplot(data=iris, aes(x=Sepal.Width))
histogram + geom_histogram(binwidth=0.2, color="black", aes(fill=Species)) + 
  xlab("Sepal Width") +  ylab("Frequency") + ggtitle("Histogram of Sepal Width")

#scatterplot
plot(x=iris$Sepal.Length, y=iris$Sepal.Width, 
     xlab="Sepal Length", ylab="Sepal Width",  main="Sepal Length-Width")

scatter <- ggplot(data=iris, aes(x = Sepal.Length, y = Sepal.Width)) 
scatter + geom_point(aes(color=Species, shape=Species)) +
  xlab("Sepal Length") +  ylab("Sepal Width") +
  ggtitle("Sepal Length-Width")

#barplot
iris1 <- iris[sample(1:nrow(iris), 110), ]
barplot(table(iris1$Species), col="black", xlab="Species", ylab="Count", main="Bar plot of Sepal Length")

#boxplot
boxplot(Sepal.Length~Species,data=iris, 
        xlab="Species", ylab="Sepal Length", main="Iris Boxplot")

box <- ggplot(data=iris, aes(x=Species, y=Sepal.Length))
box + geom_boxplot(aes(fill=Species)) + 
  ylab("Sepal Length") + ggtitle("Iris Boxplot") +
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) 

#melt , but for data
iris2 <- melt(iris, id.vars="Species")
iris2[1:5,]
#meh whatevs on this part

#time series
ggplot(data=ChickWeight, aes(x=Time, y=weight, color=Diet, group=Chick)) +
  geom_line() + ggtitle("Growth curve for individual chicks")

#gaussian modeling
formula = as.formula(medv ~ (.)^2)
#trying lasso not squaring to see all the features
formula = as.formula(medv ~ (.))

mm <- model.matrix(formula,data=train)
mmtest <- model.matrix(formula, data=test)
cv.glmnet(medv, dep0, family="gaussian", nfolds = 10, alpha = 0 )
train.cvmn <- cv.glmnet(mm[ , -1], train$medv, family="gaussian", nlambda=60, standardize=FALSE, type.measure="mse", nfold=10,alpha=.5)
cvmn <- glmnet(mm[ , -1], train$medv,family="gaussian",alpha=.5,standardize = FALSE, lambda = train.cvmn$lambda.min)
plot(cvmn)

#tosee the coefs by abs value
coef <- coef(cvmn, s=cvmn$lambda.1se)[order(abs(coef(cvmn, s=cvmn$lambda.1se)), decreasing = TRUE)]
names <- rownames(coef(cvmn, s=cvmn$lambda.1se))[order(abs(coef(cvmn, s=cvmn$lambda.1se)), decreasing = TRUE)][1:30]
for (iii in 1:30){
  cat(names[[iii]],"\t",coef[[iii]],"\n")
}

