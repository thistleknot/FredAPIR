
#cross validation
library(caret)
library(DAAG)

#filter
library(dplyr)

#xyplot
library(lattice)

#MAE
library(DescTools)

library(olsrr)
library(ggplot2)

#https://onlinecourses.science.psu.edu/stat501/node/334/
#RSS
PRESS <- function(linear.model) {
  pr <- residuals(linear.model)/(1 - lm.influence(linear.model)$hat)
  sum(pr^2)
}

MyData <- read.csv(file="prepped.csv", header=TRUE, sep=",")

colnames(MyData) 

nonXFields <- c('test2_z.date','BL_yFYield_CSUSHPINSA','yFYield_CSUSHPINSA')

#xfields (exclude nonXFields)
xList = colnames(MyData[ ,which((names(MyData) %in% nonXFields)==FALSE)])

MR_yField <- 'yFYield_CSUSHPINSA'
BL_yField <- 'BL_yFYield_CSUSHPINSA'

yField = MR_yField

#subset/filter by column
#https://stackoverflow.com/questions/41863722/r-filter-columns-in-a-data-frame-by-a-list
#x <- MyData[ ,which((names(MyData) %in% nonXFields)==FALSE)]

#preserves column names
x <- MyData[xList]
y <- MyData[yField]

nrow(x)
nrow(y)
#subset by sample

#https://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function/39403173
## 75% of the sample size
#single split
smp_size <- floor(0.75 * nrow(MyData))

## set the seed to make your partition reproducible
set.seed(123)

#possible to define two splits?
train_ind <- sample(seq_len(nrow(x)), size = smp_size)

#training x,y
#train_x <- x[train_ind, ]
#train_y <- y[train_ind, ]
train_xy_set <- MyData[train_ind, c(xList,yField)]
#View(training_set)

#testing x,y
#difference is -train
#test_x <- x[-train_ind, ]
#test_y <- y[-train_ind, ]
test_xy_set <- MyData[-train_ind, c(xList,yField)]

#http://r-statistics.co/Linear-Regression.html
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(MyData), 0.6*nrow(MyData))  # row indices for training data
trainingData <- MyData[trainingRowIndex, ]  # model training data
testData  <- MyData[-trainingRowIndex*.5, ]   # test data
validationData  <- MyData[-trainingRowIndex*.5, ]   # test data

#rename column
#http://rprogramming.net/rename-columns-in-r/
#colnames(data)[colnames(data)=="old_name"] <- "new_name"

colnames(train_x)
#colnames(trainingSet)

#merge quickly
#total <- merge(data frameA,data frameB,by="ID")

#SPSSReducedModel <- c('yFYield_CSUSHPINSA','Q1','Q2','Q3','Q4','xYield_CASTHPI','xYield_CPALTT01USQ657N','xYield_GS10','xYield_MSPNHSUS','xYield_MVLOAS','xYield_NYXRSA','xYield_POP','xYield_POPTHM',xYield_SDXRSA','xYield_TB3MS','xYield_UMCSENT','xYield_USSLIND')
#removed xYield_POP due to high correlation with xYield_POPTHM as well as Q2 for collinearity reasons
#trainingModel <- lm(yFYield_CSUSHPINSA ~ Q1 + Q3 + Q4 + xYield_CASTHPI + xYield_CPALTT01USQ657N + xYield_GS10 + xYield_MSPNHSUS + xYield_MVLOAS + xYield_NYXRSA + xYield_POPTHM + xYield_SDXRSA + xYield_TB3MS + xYield_UMCSENT + xYield_USSLIND, data = train_xy_set)
trainingModel <- lm(yFYield_CSUSHPINSA ~ Q1 + Q3 + Q4 + xYield_CASTHPI + xYield_CPALTT01USQ657N + xYield_GS10 + xYield_MSPNHSUS + xYield_MVLOAS + xYield_NYXRSA + xYield_POPTHM + xYield_SDXRSA + xYield_TB3MS + xYield_UMCSENT + xYield_USSLIND, data = train_xy_set)

#trainingModel <- lm(trainingSet[SPSSReducedModel])
#colnames(trainingSet[SPSSReducedModel])

resultsAll <- ols_step_all_possible(trainingModel, p=.05)

resultsSubSet <- ols_step_best_subset(trainingModel, p=.05)

#results <- ols_regress(trainingModel, p=.05)
#View(resultsAll)

#https://www.analyticsvidhya.com/blog/2018/05/improve-model-performance-cross-validation-in-python-r/
#cross validation
#rf = random forest
#nb = naive bayes
#method="rf"
#train_control <- trainControl(method="cv", number=10)
#model <- train(yFYield_CSUSHPINSA~., data=train_xy_set, trControl=train_control, contrasts = NULL)

#https://stats.stackexchange.com/questions/61090/how-to-split-a-data-set-to-do-10-fold-cross-validation
#flds <- createFolds(y, k = 10, list = TRUE, returnTrain = FALSE)
#names(flds)[1] <- "train"

#https://www.statmethods.net/stats/regression.html
cv.lm(test_xy_set, trainingModel, m=3) # 3 fold cross-validation

cv.lm(train_xy_set, trainingModel, m=3) # 3 fold cross-validation

#https://www.rdocumentation.org/packages/EnvStats/versions/2.3.1/topics/predict.lm
#how to do this for test data?
traindistPred <- predict(trainingModel)
plot(traindistPred,trainingModel$residuals)

nrow(test_xy_set)

#studentized residuals
olsrr::ols_plot_resid_stud_fit(trainingModel)
hist(trainingModel$residuals)
summary(trainingModel)

#RMSE of training
RMSE(trainingModel)

#RSS
PRESS(trainingModel)

print(trainingModel)
trainingModel$coefficients

#http://r-statistics.co/Linear-Regression.html
fit <- lm(trainingModel, data=test_xy_set)
testdistPred <- predict(fit)
plot(testdistPred,fit$residuals)
#https://www.rdocumentation.org/packages/DescTools/versions/0.99.19/topics/Measures%20of%20Accuracy
MAE(fit)
RMSE(fit)
MAPE(fit)
MSE(fit)
SMAPE(fit)
PRESS(fit)

#https://stats.stackexchange.com/questions/248603/how-can-one-compute-the-press-diagnostic
hist(fit$residuals)

summary(fit) # show results
plot(fit)

olsrr::ols_plot_resid_stud_fit(fit)

#results$predictors

write.csv(results,"bestSubset.csv")

summary.print(results)
View(summary.print(results))

#http://r-statistics.co/Linear-Regression.html

plot(results)

#https://dplyr.tidyverse.org/reference/filter.html

# Mallows Distance from n Filter, lower absolute distance from n is better
#better to allow for more higher mallow's CP values since they are so disparate and no need to punish so heavily early on

nrow(results)

#filter
#https://blog.exploratory.io/filter-data-with-dplyr-76cf5f1a258e

mcp <- round(results$cp-results$n,0)
mcp_floor <- max(mean(mcp),median(mcp))
subset1 <- filter(results, results$cp-results$n <= mcp_floor)
#adj R^2 filter
adjRfilter <- max(mean(subset1$adjr),median(subset1$adjr))
subset2 <- filter(subset1, subset1$adjr >= adjRfilter)
#n filter
sizefilter <- min(mean(subset2$n),median(subset2$n))
subset3 <- filter(subset2, subset2$n <= sizefilter)
#Error Filter
errorFilter <- min(mean(subset3$msep),median(subset3$msep))
subset4 <- filter(subset3, subset3$msep < errorFilter)
#AIC Filter
AICFilter <- min(mean(subset4$aic),median(subset4$aic))
subset5 <- filter(subset4, subset4$aic < AICFilter)
#SBC and SBIC 
SBICFilter <- min(mean(subset5$sbic),median(subset5$sbic))
SBCFilter <- min(mean(subset5$sbc),median(subset5$sbc))
subset6<- filter(subset5, subset5$sbc < SBCFilter & subset5$sbic <= SBICFilter)
#APC Filter Higher is better
#https://olsrr.rsquaredacademy.com/reference/ols_apc.html
APCFilter <- max(mean(subset6$apc),median(subset6$apc))
subset7<- filter(subset6, subset6$apc > APCFilter)
#Hocking's SP, lower is better
#https://rdrr.io/cran/olsrr/man/ols_hsp.html
#Average prediction mean squared error
HSPFilter <- min(mean(subset7$hsp),median(subset7$hsp))
subset8<- filter(subset7, subset7$hsp < HSPFilter)
#Final Prediction Error
FPEFilter <- min(mean(subset8$fpe),median(subset8$fpe))
subset9<- filter(subset8, subset8$fpe < FPEFilter)
#filtered
View(subset9)
print(subset9)

#best subselections
print(resultsSubSet)
View(resultsSubSet)

#do an if check more than 1 row, if so, pick minimimum error
if(nrow(subset9)>1) {
  #subset3 <- filter(subset3, subset3$msep == min(subset3$msep))
  print("yes")
  #View(subset4)
} else {
  print("no")
  #View(subset4)
}

plot(subset9$n,subset9$adjr)

#subset9$predictors[1]

a=0
for (i in subset9$predictors) {
  factor_list <- c()
  var <- c()
  a=a+1
  factor_list <- subset9$predictors[a]
  vars <- scan(text = factor_list, what = "")
  
  names <- c()
  
  names <- c(names, 'yFYield_CSUSHPINSA')
  
  names <- c(names, vars)
  
  print(names)
  
  trainingModel <- lm(train_xy_set[names])
  fit <- lm(trainingModel, data=test_xy_set[names])
  
  print(summary(fit))
  print(RMSE(fit))
  
  #https://stackoverflow.com/questions/24741541/split-a-string-by-any-number-of-spaces
  
}

