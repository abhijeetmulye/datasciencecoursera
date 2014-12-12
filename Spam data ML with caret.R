# Pre-processing guide
# http://topepo.github.io/caret/preprocess.html


library(caret); 
library(splines)
library(kernlab); 
data(spam)
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

# With small n-size: Standard errors of coefficients are unreliable because they rely on
# asymptomatic assumptions
# Bootstrapping: When normality/constant variance assumptions are violated
# Can be used so long as sample represents population

## Normalizing predictors

## Center and scale is useless. If you don't "center" the intercept of regression means expected value when
# predictors are zero, instead of expected value when predictors are set to their means
# If you don't scale, the coefficient of variable with large values will be very small
# Doesn't affect robustness or statistics at all

# One at a time
# preObj = preProcess(training[,-58])
# training_n = predict(preObj, training[, -58])
# training_n_type = cbind(training_n, training[,58])
# names(training_n_type)[names(training_n_type)=="training[, 58]"] = "type"
# set.seed(32343)
# modelFit2 = train(type ~., data = training_n_type, method = "glm")

# Pre-process and train at once
set.seed(32343)
modelFit1 = train(type ~., data = training, preProcess=c("center", "scale"), method = "glm")
modelFit3 = train(type ~., data = training, method = "glm")

# Box-Cox makes continuous data look like normally distributed data

## Some algos can't handle NA's => method = "knnImpute" 

# Fitting non-linear relationships using lm
# Use splines or gam method in caret training



# K-fold sampling

folds = createFolds(y= spam$type, k=10, list=T, returnTrain=T)
str(folds)

sapply(folds, length)

# PCA

modelPCA = train(training$type~., method="glm", preProcess="pca", data= training)
confusionMatrix(testing$type, predict(modelPCA, newdata=testing))

plot(training$age, modelPCA$fitted)



