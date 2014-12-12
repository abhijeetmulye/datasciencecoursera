rm(list=ls())
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

fit = train(training$diagnosis~., data=training[,58:69], method="glm")
fit_PCA = train(training$diagnosis~., data=training[,58:69], method="glm", preProcess="pca")

confusionMatrix(testing$diagnosis, predict(fit, testing))
confusionMatrix(testing$diagnosis, predict(fit_PCA, testing))

preProc <- preProcess(training[, 58:69], method = "pca", thresh = 0.8)
preProc$rotation
