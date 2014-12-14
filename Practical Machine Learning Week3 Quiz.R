# Quiz 3 Q1

rm(list=ls())
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
library(rattle)

set.seed(125)
head(segmentationOriginal)

training = segmentationOriginal[segmentationOriginal$Case=="Train", ]
testing = segmentationOriginal[segmentationOriginal$Case=="Test", ]

model = train(Class~., method="rpart", data=training)
fancyRpartPlot(model$finalModel)

# a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2 PS
# b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100 WS
# c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100 PS
# d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2 


# Quiz 3 Q3

rm(list=ls())
library(pgmm)
data(olive)
olive = olive[,-1]

model = train(Area~., method="rpart", data=olive)

newdata = as.data.frame(t(colMeans(olive)))
fancyRpartPlot(model$finalModel)
predict(model, newdata)

# Quiz 3 Q4

# Then set the seed to 13234 and fit a logistic regression model method="glm", be sure to 
# specify family="binomial" with Coronary Heart Disease (chd) as the outcome and age at onset, 
# current alcohol consumption, obesity levels, cumulative tabacco, type-A behavior, and low density 
# lipoprotein cholesterol as predictors. Calculate the misclassification rate for your model using
# this function and a prediction on the "response" scale:
#   
# missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
# What is the misclassification rate on the training set? What is the misclassification rate 
# on the test set?

rm(list=ls())
set.seed(13234)
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

model = train(chd~., method="glm", family="binomial", 
              data=trainSA[,c("chd", "age","alcohol","obesity","typea", "ldl","tobacco")])

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

missClass(trainSA$chd, predict(model, trainSA))
missClass(testSA$chd, predict(model, testSA))


# Quiz 3 Q5

rm(list=ls())
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 
#set.seed(33833)

vowel.train$y = as.factor(vowel.train$y)
vowel.test$y = as.factor(vowel.test$y)

model = train(y~., data=vowel.train, method="rf", importance=T)
varImp(model)
