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
## set the seed to make your partition reproducible
set.seed(123)

#used for exhaustive lists (oversamples)
#possible to define two splits?
smp_size <- floor(.75 * nrow(MyData))
#https://stackoverflow.com/questions/14864275/randomize-w-no-repeats-using-r
training1 <- sample(smp_size, replace=F)
training2 <- sample(smp_size, replace=F)

#used to build models on new ranodmized data and then tested against holdout test data (to include cross validation)
vld_size <- floor(1.0 * nrow(MyData))
validation1 <- sample(vld_size, replace=F)

#ensures I'm creating a partition based on randomized #'s
#train1_ind <- training[1:smp_size*.75]
#train2_ind <- training[(smp_size*.75+1):nrow(x)]
#train2_ind <- sample(seq_len(nrow(x)), size = smp_size)

#used for creating models
#validation <- sample(seq_len(nrow(x)), replace=F)
#valid1_ind <- validation[1:vld_size]
test1_ind <- validation[(vld_size+1):nrow(x)]

#used for kfold bootstrap validation testing
#test1_ind <- sample(seq_len(nrow(x)), size = smp_size)

#not limited to just the first portion of the percent
#provides an index
#http://r-statistics.co/Linear-Regression.html
#training1RowIndex <- sample(1:nrow(MyData), 0.6*nrow(MyData))  # row indices for training data

training1Data <- c()
training2Data <- c()

#before I was using exactly half the data, now I'm using random sampling over the sample dataset and expecting overprovisioning to occur
training1Data <- MyData[training1, ]  # model training data
#ensures the model pulls from the other data
#this makes my pooled datasets sourced from smaller data pools than the validation pool (akin to encryption: reverse randomized partitioning, allows for multiple partitions to be layered over a disk that holds data))
training2Data <- MyData[training2, ]  # model training data

train1_xy_set <- c()
train2_xy_set <- c()

train1_xy_set <- training1Data[c(yField,xList)]
train2_xy_set <- training2Data[c(yField,xList)]

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
layout(matrix(c(1,2,3,4),2,2))
olsrr::ols_plot_resid_stud_fit(training1Model)
layout(matrix(c(1,2,3,4),2,2))
olsrr::ols_plot_resid_stud_fit(training2Model)

table(is.na(train1_xy_set[names]))
table(is.na(train2_xy_set[names]))

#testingData  <- MyData[-trainingRowIndex, ]   # test data

#validationData  <- MyData[-trainingRowIndex, ]   # test data

#merge quickly
#total <- merge(data frameA,data frameB,by="ID")

summary(training1Model)
summary(training2Model)

resultsAAll <- ols_step_all_possible(training1Model, p=.05)
#results1SubSet <- ols_step_best_subset(training1Model, p=.05)

resultsBAll <- ols_step_all_possible(training2Model, p=.05)

#Adj R^2 Filter
adjR_Afilter <- max(mean(resultsAAll$adjr),median(resultsAAll$adjr))
adjR_Bfilter <- max(mean(resultsBAll$adjr),median(resultsBAll$adjr))

# Mallows Distance from n Filter, lower absolute distance from n is better
#better to allow for more higher mallow's CP values since they are so disparate and no need to punish so heavily early on
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
hist(subsetA$adjr)
subsetB <- filter(resultsBAll,  (resultsBAll$msep < error_BFilter) & (resultsBAll$adjr > adjR_Bfilter)  & ((resultsBAll$cp-resultsBAll$n) <= mcp_B_floor) & (resultsBAll$n < size_B_floor)& (resultsBAll$aic < AIC_BFilter) & (resultsBAll$sbc < SBC_BFilter) & (resultsBAll$sbic < SBIC_BFilter)  & (resultsBAll$hsp < HSP_BFilter) & (resultsBAll$fpe < FPE_BFilter))
hist(subsetB$adjr)

factor_test_list <- intersect(subsetA$predictors,subsetB$predictors)
View(factor_test_list)

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

write.csv(factor_test_list,"factor_test_list.csv")

#v_model <- rbind(c(factor_list, RMSE(fit), PRESS(fit), resultsAll$adjr))
cv_model <- c()
#cv_model <- cbind('factor_list', 'cv', 'co-efficients', 'p-values', 'RMSE', 'RSS', 'adjR')
#colnames(cv_model) = c('factor_list', 'cv', 'co-efficients', 'p-values', 'RMSE', 'RSS', 'adjR')

#10% was too low (leverage of 1 threw an error, can only assume 10% CV window too small, I'd almost prefer to do 33%), doing 25% CV
#a=0
for (i in seq(factor_test_list)) {
 
  #10 CV Passes
  for (i in 1:4)
  {
    s_true=0
    upper_Pct = floor(.75*length(validation1))
    lower_Pct = ceiling(.25*length(validation1))
    
    leftStart <- (floor((i-1)/4*length(validation1)))
    print(leftStart)
    
    endLeftStart = leftStart+lower_Pct
    
    distanceFromEnd_EndLeftStart = length(validation1)- endLeftStart
    
    leftMakeup = upper_Pct - distanceFromEnd_EndLeftStart
    
    movingSide = validation1[leftStart:(leftStart+lower_Pct+distanceFromEnd_EndLeftStart)]
    makeupSide = validation1[1:leftMakeup]
    print((leftStart+lower_Pct+distanceFromEnd_EndLeftStart)-leftStart)
    print(leftMakeup)
    print(((leftStart+lower_Pct+distanceFromEnd_EndLeftStart)-leftStart)+(leftMakeup))
    
    #print(movingSide)
    #print(makeupSide)
    
    combined_list=c(movingSide,makeupSide)
    #print(combined_list)
    training = combined_list[1:upper_Pct]
    validation = combined_list[(upper_Pct+1):length(combined_list)]
    
    print(validation)
    
    fit<- c()
    trainingValidModel <-c()
    
    factor_list <- c()
    var <- c()
    #a=a+1
    factor_list <- factor_test_list[i]
    
    #https://stackoverflow.com/questions/24741541/split-a-string-by-any-number-of-spaces
    vars <- scan(text = factor_list, what = "")
    
    valid1_xy_set  <- c()
    test1_xy_set  <- c()
    
    valid1_xy_set <- MyData[training, c(xList,yField)]
    
    #test set will be 25% of training set and will not be the value of the training set
    test1_xy_set <- MyData[validation, c(yField,xList)]
    
    names <- c()
    names <- c(names, 'yFYield_CSUSHPINSA')
    names <- c(names, vars)
    
    print(names)
    
    #names2 was created to fix a bug with a reserved name in a later function
    names2 <-c()
    names2 <- names
    #using valid1 data
    trainingValidModel <- lm(valid1_xy_set[names])
    fit <- lm(trainingValidModel, data=test1_xy_set[names2])
    testdistPred <- predict(fit)
    
    trainValidPred <- predict(trainingValidModel)
    
    #plot(trainValidPred,trainingValidModel$residuals)
    
    #studentized residuals
    hist(trainingValidModel$residuals)
    summary(trainingValidModel)
    
    #RMSE of training
    RMSE(trainingValidModel)
    
    #RSS
    PRESS(trainingValidModel)
    
    print(trainingValidModel)
    trainingValidModel$coefficients
    
    olsrr::ols_plot_resid_stud_fit(trainingValidModel)
    
    #cross validation
    #https://www.statmethods.net/stats/regression.html
    layout(matrix(c(1),1))
    #cv.lm(test1_xy_set[names], trainingValidModel, m=2, plotit="Residual") # 3 fold cross-validation
    #cv.lm(test1_xy_set[names], trainingValidModel, m=3, plotit="Residual") # 3 fold cross-validation
    
    #Boot strapped Cross Validation
    #90% CV moving window over Validation1 random sample    
    
    #http://r-statistics.co/Linear-Regression.html
    
    #delta = (testdistPred - test1_xy_set[names][1])
    
    #testPred <- predict(fit) * test1_xy_set[1]
    
    #if(testPred>0){s_true=1}else s_true=0
    
    #https://www.rdocumentation.org/packages/DescTools/versions/0.99.19/topics/Measures%20of%20Accuracy
    MAE(fit)
    MAPE(fit)
    MSE(fit)
    SMAPE(fit)
    
    #https://stats.stackexchange.com/questions/248603/how-can-one-compute-the-press-diagnostic
    hist(fit$residuals)
    
    print(summary(fit)) # show results
    
    #final RMSE should be evaluated against model that performs best against actual data.
    
    print(RMSE(fit))
    print(PRESS(fit))
    
    layout(matrix(c(1,2,3,4),2,2))
    plot(fit)
    
    layout(matrix(c(1),1))
    olsrr::ols_plot_resid_stud_fit(fit)
    
    plot(testdistPred,fit$residuals)
    
    #provides residual stats
    resultsAll <- ols_regress(fit, p=.05)
    
    print(resultsAll)
    
    #cv_model <- rbind(v_model,c(factor_list, paste("cv", toString(i)), resultsAll$model, resultsAll$pvalues, RMSE(fit), PRESS(fit), resultsAll$adjr, s_true))
    #without strue
    cv_model <- rbind(cv_model,c(factor_list, paste("cv", toString(i)), toString(resultsAll$betas), toString(resultsAll$pvalues), RMSE(fit), PRESS(fit), resultsAll$adjr))
    #View(cv_model)
    #needs to be same n size (would be useful across cross validations)
    #anova(trainingValidModel,fit)
    
  }

  write.csv(cv_model,"cv_models.csv")
}

#appendix
#rename column
#http://rprogramming.net/rename-columns-in-r/
#colnames(data)[colnames(data)=="old_name"] <- "new_name"

#http://r-statistics.co/Linear-Regression.html
#https://www.rdocumentation.org/packages/EnvStats/versions/2.3.1/topics/predict.lm

#https://dplyr.tidyverse.org/reference/filter.html

#filter
#https://blog.exploratory.io/filter-data-with-dplyr-76cf5f1a258e

