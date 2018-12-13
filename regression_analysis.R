
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

fieldOfInterest='yFYield_CSUSHPINSA'

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
train1_ind <- sample(seq_len(nrow(x)), size = smp_size)
train2_ind <- sample(seq_len(nrow(x)), size = smp_size)

#used for creating models
valid1_ind <- sample(seq_len(nrow(x)), size = smp_size)

#used for kfold bootstrap validation testing
test1_ind <- sample(seq_len(nrow(x)), size = smp_size)

#final RMSE should be evaluated against model that performs best against actual data.

#training x,y
#train_x <- x[train_ind, ]
#train_y <- y[train_ind, ]
#train_xy_set <- MyData[train_ind, c(xList,yField)]

#View(training_set)

#testing x,y
#difference is -train
#test_x <- x[-train_ind, ]
#test_y <- y[-train_ind, ]
#test_xy_set <- MyData[-train_ind, c(xList,yField)]

test1_xy_set <- MyData[test1_ind, c(xList,yField)]

#http://r-statistics.co/Linear-Regression.html
#set.seed(100)  # setting seed to reproduce results of random sampling

#not limited to just the first portion of the percent
#training1RowIndex <- sample(1:nrow(MyData), 0.6*nrow(MyData))  # row indices for training data
#training2RowIndex <- sample(1:nrow(MyData), 0.6*nrow(MyData))  # row indices for training data

validationRowIndex <- sample(1:nrow(MyData), 0.6*nrow(MyData))  # row indices for training data
testingRowIndex <- sample(1:nrow(MyData), 0.6*nrow(MyData))  # row indices for training data

training1Data <- MyData[train1_ind, ]  # model training data
training2Data <- MyData[train2_ind, ]  # model training data

train1_xy_set <- training1Data[c(yField,xList)]
train2_xy_set <- training2Data[c(yField,xList)]
#View(train_xy_set)

names <- c()
vars <- c()
names <- c(names, 'yFYield_CSUSHPINSA')
#SPSSReducedModel <- c('yFYield_CSUSHPINSA','Q1','Q2','Q3','Q4','xYield_CASTHPI','xYield_CPALTT01USQ657N','xYield_GS10','xYield_MSPNHSUS','xYield_MVLOAS','xYield_NYXRSA','xYield_POP','xYield_POPTHM',xYield_SDXRSA','xYield_TB3MS','xYield_UMCSENT','xYield_USSLIND')
#removed xYield_POP due to high correlation with xYield_POPTHM as well as Q2 for collinearity reasons
#trainingModel <- lm(yFYield_CSUSHPINSA ~ Q1 + Q3 + Q4 + xYield_CASTHPI + xYield_CPALTT01USQ657N + xYield_GS10 + xYield_MSPNHSUS + xYield_MVLOAS + xYield_NYXRSA + xYield_POPTHM + xYield_SDXRSA + xYield_TB3MS + xYield_UMCSENT + xYield_USSLIND, data = train_xy_set)
vars <- c('Q1','Q3','Q4','xYield_CASTHPI','xYield_CPALTT01USQ657N','xYield_GS10','xYield_MSPNHSUS','xYield_MVLOAS','xYield_NYXRSA','xYield_POPTHM','xYield_SDXRSA','xYield_TB3MS','xYield_UMCSENT','xYield_USSLIND')
names <- c(names, vars)

#doesn't throw an error here, but when I try to pass this model into ols_step_all_possible, it throws an error
#training1Model <- lm(train1_xy_set[names])
training1Model <- lm(yFYield_CSUSHPINSA ~ Q1 + Q3 + Q4 + xYield_CASTHPI + xYield_CPALTT01USQ657N + xYield_GS10 + xYield_MSPNHSUS + xYield_MVLOAS + xYield_NYXRSA + xYield_POPTHM + xYield_SDXRSA + xYield_TB3MS + xYield_UMCSENT + xYield_USSLIND, data = train1_xy_set)

#training2Model <- lm(train2_xy_set[names])
training2Model <- lm(yFYield_CSUSHPINSA ~ Q1 + Q3 + Q4 + xYield_CASTHPI + xYield_CPALTT01USQ657N + xYield_GS10 + xYield_MSPNHSUS + xYield_MVLOAS + xYield_NYXRSA + xYield_POPTHM + xYield_SDXRSA + xYield_TB3MS + xYield_UMCSENT + xYield_USSLIND, data = train2_xy_set)

table(is.na(train1_xy_set[names]))
table(is.na(train2_xy_set[names]))

#testingData  <- MyData[-trainingRowIndex, ]   # test data

#validationData  <- MyData[-trainingRowIndex, ]   # test data

#merge quickly
#total <- merge(data frameA,data frameB,by="ID")

summary(training1Model)
summary(training2Model)

#View(training2Model)
#View(resultsAll)
resultsAAll <- ols_step_all_possible(training1Model, p=.05)
resultsAAll <- results1All
resultsBAll <- ols_step_all_possible(training2Model, p=.05)
resultsBAll <- results2All

results2All$predictors

#Adj R^2 Filter
adjR_Afilter <- max(mean(resultsAAll$adjr),median(resultsAAll$adjr))
adjR_Bfilter <- max(mean(resultsBAll$adjr),median(resultsBAll$adjr))

#Mallows Filter
mcp_A_floor <- min(mean(resultsAAll$cp-resultsAAll$n),median(resultsAAll$cp-resultsAAll$n))
mcp_B_floor <- min(mean(resultsBAll$cp-resultsBAll$n),median(resultsBAll$cp-resultsBAll$n))

#size filter
size_A_floor <- min(mean(resultsAAll$n),median(resultsAAll$n))
size_B_floor <- min(mean(resultsBAll$n),median(resultsBAll$n))

#Error Filter
error_AFilter <- min(mean(resultsAAll$msep),median(resultsAAll$msep))
error_BFilter <- min(mean(resultsBAll$msep),median(resultsBAll$msep))

#AIC Filter
AIC_AFilter <- min(mean(resultsAAll$aic),median(resultsAAll$aic))
AIC_BFilter <- min(mean(resultsBAll$aic),median(resultsBAll$aic))

#SBC and SBIC 
SBIC_AFilter <- min(mean(resultsAAll$sbic),median(resultsAAll$sbic))
SBC_AFilter <- min(mean(resultsAAll$sbc),median(resultsAAll$sbc))
SBIC_BFilter <- min(mean(resultsBAll$sbic),median(resultsBAll$sbic))
SBC_BFilter <- min(mean(resultsBAll$sbc),median(resultsBAll$sbc))

#APC Filter Higher is better
#https://olsrr.rsquaredacademy.com/reference/ols_apc.html
#Amemiya's Prediction Criterion penalizes R-squared more heavily than does adjusted R-squared for each addition degree of freedom used on the right-hand-side of the equation. 
#The higher the better for this criterion.
APC_AFilter <- max(mean(resultsAAll$apc),median(resultsAAll$apc))
APC_BFilter <- max(mean(resultsBAll$apc),median(resultsBAll$apc))
#& (resultsAAll$apc > APCAFilter)
#decisive, heavily penalizes per factor

#Hocking's SP, lower is better
#https://rdrr.io/cran/olsrr/man/ols_hsp.html
HSP_AFilter <- min(mean(resultsAAll$hsp),median(resultsAAll$hsp))
HSP_BFilter <- min(mean(resultsBAll$hsp),median(resultsBAll$hsp))

#Average prediction mean squared error
#Final Prediction Error
FPE_AFilter <- min(mean(resultsAAll$fpe),median(resultsAAll$fpe))
FPE_BFilter <- min(mean(resultsBAll$fpe),median(resultsBAll$fpe))

#filtered
#AIC and BIC hold the same interpretation in terms of model comparison. That is, the larger difference in either AIC or BIC indicates stronger evidence for one model over the other 
#(the lower the better). It's just the the AIC doesn't penalize the number of parameters as strongly as BIC.Jan 7, 2014

subsetA <- filter(resultsAAll,  (resultsAAll$msep < error_AFilter) & (resultsAAll$adjr > adjR_Afilter)  & ((resultsAAll$cp-resultsAAll$n) <= mcp_A_floor) & (resultsAAll$n < size_A_floor)& (resultsAAll$aic < AIC_AFilter) & (resultsAAll$sbc < SBC_AFilter) & (resultsAAll$sbic < SBIC_AFilter)  & (resultsAAll$hsp < HSP_AFilter) & (resultsAAll$fpe < FPE_AFilter))
subsetB <- filter(resultsBAll,  (resultsBAll$msep < error_BFilter) & (resultsBAll$adjr > adjR_Bfilter)  & ((resultsBAll$cp-resultsBAll$n) <= mcp_B_floor) & (resultsBAll$n < size_B_floor)& (resultsBAll$aic < AIC_BFilter) & (resultsBAll$sbc < SBC_BFilter) & (resultsBAll$sbic < SBIC_BFilter)  & (resultsBAll$hsp < HSP_BFilter) & (resultsBAll$fpe < FPE_BFilter))

factor_test_list <- intersect(subsetA$predictors,subsetB$predictors)
View(factor_test_list)


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
cv.lm(test1_xy_set, training1Model, m=3) # 3 fold cross-validation

cv.lm(train1_xy_set, training1Model, m=3) # 3 fold cross-validation

#https://www.rdocumentation.org/packages/EnvStats/versions/2.3.1/topics/predict.lm
#how to do this for test data?
traindistPred <- predict(training1Model)
plot(traindistPred,training1Model$residuals)

nrow(test1_xy_set)

#studentized residuals
olsrr::ols_plot_resid_stud_fit(training1Model)
olsrr::ols_plot_resid_stud_fit(training2Model)
hist(training1Model$residuals)
hist(training2Model$residuals)
summary(training1Model)
summary(training2Model)

#RMSE of training
RMSE(training1Model)

#RSS
PRESS(training1Model)

print(training1Model)
training1Model$coefficients

#http://r-statistics.co/Linear-Regression.html
fit <- lm(training1Model, data=test1_xy_set)
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
  fit <- lm(trainingModel, data=test1_xy_set[names])
  
  print(summary(fit))
  print(RMSE(fit))
  
  #https://stackoverflow.com/questions/24741541/split-a-string-by-any-number-of-spaces
  
}

#appendix
#rename column
#http://rprogramming.net/rename-columns-in-r/
#colnames(data)[colnames(data)=="old_name"] <- "new_name"

