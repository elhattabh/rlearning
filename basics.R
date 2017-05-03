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

# — to find the nulls in a data frame:
  apply(basefile,2,function(x){sum(is.na(x))})

# — to order things/loop through

head(df[order(-df$donations),],n=20)
# [4:10] 

coef <- coef(cvmnf, s=cvmnf$lambda.1se)[order(abs(coef(cvmnf, s=cvmnf$lambda.1se)), decreasing = TRUE)][1:6]
names <- rownames(coef(cvmnf, s=cvmnf$lambda.1se))[order(abs(coef(cvmnf, s=cvmnf$lambda.1se)), decreasing = TRUE)][1:6]
for (iii in 1:6){
  cat(names[[iii]],"\t",coef[[iii]],"\n")
}

history | grep 

# — for quantiles deciles

test$support_pp2016_score_decile = as.integer(cut(test$support_pp2016_score, breaks=c(quantile(test$support_pp2016_score, probs = seq(0, 1, by = 0.10),na.rm=TRUE))))
support_crosstabs = xtabs(test$support_pp2016_score~test$support_pp2016_score_decile)/xtabs(~test$support_pp2016_score_decile)

# --case whens
basefile$edu_flag_individual_filled = ifelse(is.na(basefile$edu_flag_individual) | basefile$edu_flag_individual=="0", 0,1)
basefile$edu_flag_hh_filled = ifelse(is.na(basefile$edu_flag_hh) | basefile$edu_flag_hh=="0", 0,1)

# — more xtabs - like stuff ( to get two matrices/ each other)
prop.table(table(basefile$incomebucket, basefile$ppi_bucket), margin = 2)

# —MELT/UMELT
subset_data <- subset(report_reshape, vendor == 'All' & state == 'All'  & race == 'All'   & poll_week == 'All')

unmelted <- dcast(subset_data, state+datecalled+poll_week+vendor+race ~ category )
head(melt(unmelted, measure.vars=c("avg_support", "hrc_strong")))

# —to find diff columns in two lists (in this case list of names)
setdiff(colnames(mm), colnames(mmtest))
 
# —to transform a date
head(as.Date(admins$administered_date,format="%d-%b-%Y"))

# — order asc
drugbyyear <- drugbyyear[drugbyyear$year<=2016, ][order(year),]


#writing functions
regplot=function(x,y){
  fit=lm(y~x)
  plot(x,y)
  abline(fit,col="red")
  summary(fit)
}

#basic lasso
formula = as.formula( to$outcome_contrib~ current_week + sex +
                        + media_market)
mm <- model.matrix(formula,data=train)
mmtest <- model.matrix(formula, data=test)

cvmn <- cv.glmnet(mm, to$outcome_contrib, family="binomial", standardize=FALSE, nlambda=30, type.measure="auc", nfold=10)


