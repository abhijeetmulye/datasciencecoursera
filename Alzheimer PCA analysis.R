rm(list=ls())
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

summary(prcomp(training[,58:69]))

# Importance of components:
#   PC1    PC2    PC3     PC4     PC5     PC6     PC7    PC8     PC9    PC10    PC11
# Standard deviation     1.4977 1.2353 1.1808 0.62607 0.54487 0.40123 0.36312 0.3028 0.25453 0.25274 0.02867
# Proportion of Variance 0.3523 0.2397 0.2190 0.06157 0.04663 0.02529 0.02071 0.0144 0.01018 0.01003 0.00013
# Cumulative Proportion  0.3523 0.5920 0.8111 0.87262 0.91925 0.94454 0.96525 0.9797 0.98983 0.99986 0.99999
# PC12
# Standard deviation     0.008856
# Proportion of Variance 0.000010
# Cumulative Proportion  1.000000

fit = train(training$diagnosis~., data=training[,58:69], method="glm")
fit_PCA = train(training$diagnosis~., data=training[,58:69], method="glm", preProcess="pca")

confusionMatrix(testing$diagnosis, predict(fit, testing))
confusionMatrix(testing$diagnosis, predict(fit_PCA, testing))

preProc <- preProcess(training[, 58:69], method = "pca", thresh = 0.8)
preProc$rotation
