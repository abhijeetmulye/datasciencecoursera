rm(list=ls())
library(rpart.plot)
library(ggplot2)
library(caret)
library(rattle)
data(iris)

inTrain = createDataPartition(iris$Species, p=0.7, list=F)
training = iris[inTrain,]
test = iris[-inTrain,]

## Fit a singe tree
 
# fit = train(Species~., method="rpart", data=training)
# plot(fit$finalModel, uniform=T)
# text(fit$finalModel, cex=0.5)
# 
# fancyRpartPlot(fit$finalModel)
# 
# predictions = predict(fit, newdata=test)
# confusionMatrix(predictions, test$Species)

## Bagging improves accuracy

# fit = train(Species~., method="treebag", data=training)
# predictions = predict(fit, newdata=test)
# confusionMatrix(predictions, test$Species)


## Random Forests

fit = train(Species~., method="rf", data=training)
predictions = predict(fit, newdata=test)
confusionMatrix(predictions, test$Species)
