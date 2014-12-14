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

###
# Boosting v/s Regular regression v/s Regularized regression
###

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

###
# Ensemble learning
###

rm(list=ls())
library(ISLR)
library(ggplot2) 
library(caret)

data(Wage)
Wage$logwage = NULL

set.seed(9274)

# Create a building data set and validation set
inBuild <- createDataPartition(y=Wage$wage,p=0.7, list=FALSE)
validation <- Wage[-inBuild,]; buildData <- Wage[inBuild,]

inTrain <- createDataPartition(y=buildData$wage,p=0.7, list=FALSE)
training <- buildData[inTrain,]; testing <- buildData[-inTrain,]

mod1 <- train(wage ~.,method="glm",data=training)
mod2 <- train(wage ~.,method="rf",data=training)

pred1 <- predict(mod1,testing); pred2 <- predict(mod2,testing)

predDF <- data.frame(pred1,pred2,wage=testing$wage)
combModFit <- train(wage ~.,method="gam",data=predDF)
#combPred <- predict(combModFit,predDF)

pred1V <- predict(mod1,validation); pred2V <- predict(mod2,validation)
predVDF <- data.frame(pred1=pred1V,pred2=pred2V)
combPredV <- predict(combModFit,predVDF)
