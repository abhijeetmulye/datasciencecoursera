library(ISLR)
library(ggplot2)
library(caret)
library(Hmisc)
library(splines)

# Partition data
data(Wage)
inTrain = createDataPartition(y=Wage$wage, p=0.7, list=F)
training = Wage[inTrain,]
testing = Wage[-inTrain,]

# Feature plot
featurePlot(x=training[, c("age")], y=training$wage, plot="pairs")
qplot(age, wage, data=training)
qplot(age, wage, data=training, color=jobclass)
qplot(age, wage, data=training, color=education) + geom_smooth(method="glm", formula=y~x)
qplot(wage, data=training, geom="density", color=education)
qplot(wage, data=training, geom="density", color=jobclass)


cutWage = cut2(training$wage, g=3)
qplot(cutWage, age, data=training, fill=cutWage, geom=c("boxplot"))

# Zero covariates
nearZeroVar(training, saveMetrics=T)

# spline basis
bsBasis = bs(training$age, df=3)
bsBasis

lm1 = lm(wage~age, data=training)
lm2 = lm(wage~bsBasis, data=training)

plot(x=training$age, y=training$wage, data=training) 
points(training$age, predict(lm2, newdata=training), col="blue")


# Boosting v/s Regular regression v/s Regularized regression
rm(list=ls())

library(ISLR)
library(ggplot2) 
library(caret)

data(Wage)
Wage = subset(Wage,select=-c(logwage))

set.seed(9274)
inTrain = createDataPartition(y=Wage$wage,p=0.7, list=FALSE)
training = Wage[inTrain,]; testing <- Wage[-inTrain,]

lmFit = train(wage ~ ., method="lm",data=training)
lmFitBoost = train(wage ~ ., method="gbm",data=training, verbose=F)
lmFitRegularized = train(wage~., method="relaxo", data=training)

predLm = predict(lmFit,newdata=testing)
predLmBoost = predict(lmFitBoost, newdata=testing)
predLmRegularized = predict(lmFitRegularized, newdata=testing)

sqrt(sum((predLm-testing$wage)^2)) # RMSE 1028.14
sqrt(sum((predLmBoost-testing$wage)^2)) # RMSE 1030.235
sqrt(sum((predLmRegularized-testing$wage)^2)) # RMSE 3100.422
