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

#mallows
library(locfit)

#https://onlinecourses.science.psu.edu/stat501/node/334/
#RSS
PRESS <- function(linear.model) {
  pr <- residuals(linear.model)/(1 - lm.influence(linear.model)$hat)
  sum(pr^2)
}

ascending=function(var){  
  products_ascending = products[order(products[[var]]),]            
  return(products_ascending)
}

MyData <- read.csv(file="prepped.csv", header=TRUE, sep=",")
View(MyData)

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

#used for exhaustive lists (oversamples)
#possible to define two splits?

#alternative is 75% bootstraps of resampled data, having it split into 1/3's makes the data diverse for 6 years
#limit training size to 1/3? (6 years)

smp_size <- floor(1/2 * nrow(MyData))
vld_size <- floor(1.0 * nrow(MyData))
#https://stackoverflow.com/questions/14864275/randomize-w-no-repeats-using-r

#View(colnames(MyData))
vars <- c()
#data is too big for exhaustive search
factor_test_list<-c()
set.seed(123)
for (i in 1:5)
{
  training <- sample(nrow(MyData), replace=F)
  print(training)
  
  #goal is to find combinations of 6-7 that work
  #rule of thumb is n for factor analysis should be 5(k*2)
  #with /2 mine is 59, which is good for up to 11 factors (12 is 60), it would be ideal to have 80 since I'm starting with 14 factors, but oh well, since I'm aggregating the lists, I'm assuraedly going to lose thosee upper combitorial lists since those upper lists will be overfitted to the small sample sizes, which means wasted processing [that will be trimmed] (vs actually saving higher level models) and smaller pruned lists will only be stored and when the filter goes into affect, the affect will be much harsher.
  training1 <- training[1:smp_size]
  training2 <- training[(smp_size+1):nrow(MyData)]
  
  #http://r-statistics.co/Linear-Regression.html
  #used to build models on new ranodmized data and then tested against holdout test data (to include cross validation)
  #not limited to just the first portion of the percent
  #provides an index
  
  training1Data <- c()
  training2Data <- c()
  
  #new plan is to create two randomized partitions that will have complete dataset algo's done on them.
  training1Data <- MyData[training1, ]  # model training data
  training2Data <- MyData[training2, ]  # model training data
  
  train1_xy_set <- c()
  train2_xy_set <- c()
  
  train1_xy_set <- training1Data[c(yField,xList)]
  train2_xy_set <- training2Data[c(yField,xList)]
  
  names <- c()
  
  names <- c(names, 'yFYield_CSUSHPINSA')
  #SPSSReducedModel <- c('yFYield_CSUSHPINSA','Q1','Q2','Q3','Q4','xYield_CASTHPI','xYield_CPALTT01USQ657N','xYield_GS10','xYield_MSPNHSUS','xYield_MVLOAS','xYield_NYXRSA','xYield_POP','xYield_POPTHM',xYield_SDXRSA','xYield_TB3MS','xYield_UMCSENT','xYield_USSLIND')
  #removed xYield_POP due to high correlation with xYield_POPTHM as well as Q2 for collinearity reasons
  #trainingModel <- lm(yFYield_CSUSHPINSA ~ Q1 + Q3 + Q4 + xYield_CASTHPI + xYield_CPALTT01USQ657N + xYield_GS10 + xYield_MSPNHSUS + xYield_MVLOAS + xYield_NYXRSA + xYield_POPTHM + xYield_SDXRSA + xYield_TB3MS + xYield_UMCSENT + xYield_USSLIND, data = train_xy_set)
  vars <- xList
  names <- c(names, vars)
  
  #doesn't throw an error here, but when I try to pass this model into ols_step_all_possible, it throws an error
  #.null used with stepwise
  #training1Model.null = ~lm (yFYield_CSUSHPINSA ~ 1, data = train1_xy_set)
  training1Model <- lm(yFYield_CSUSHPINSA~.,train1_xy_set)
  #training2Model.null = ~lm (yFYield_CSUSHPINSA ~ 1, data = train2_xy_set)
  training2Model <- lm(yFYield_CSUSHPINSA~.,train2_xy_set)
  
  #obj1 = step(training1Model.null, scope=list(lower=training1Model.null, upper=training1Model), direction='forward')
  #obj2 = step(training2Model.null, scope=list(lower=training1Model.null, upper=training2Model), direction='forward')
  
  #olsrr::ols_plot_resid_stud_fit(training1Model)
  #layout(matrix(c(1,2,3,4),2,2))
  #olsrr::ols_plot_resid_stud_fit(training2Model)
  #layout(matrix(c(1,2,3,4),2,2))
  
  anova(training1Model,training2Model)
  
  summary(training1Model)
  summary(training2Model)
  
  #****
  #careful, takes a long time
  #resultsAAll <- ols_step_backward_p(training1Model, p=.05)
  #resultsBAll <- ols_step_backward_p(training2Model, p=.05)
  
  #ols_step_forward_p()
  
  #ols_step_both_()
  
  #resultsAAll <- ols_step_all_possible(training1Model, p=.05)
  resultsAAll <- ols_step_backward_p(training1Model, p=.05)
  resultsBAll <- ols_step_backward_p(training2Model, p=.05)
  #resultsBAll <- ols_step_all_possible(training2Model, p=.05)
  #careful, takes a long time
  #****
  
  tail(row.names(data.frame(resultsBAll$model$coefficients)),-1)
  
  #grab names of vars
  splitA.var <- strsplit(tail(row.names(data.frame(resultsAAll$model$coefficients)),-1), " ")
  splitB.var <- strsplit(tail(row.names(data.frame(resultsBAll$model$coefficients)),-1), " ")

  #max(subsetA$n,subsetB$n)
  
  #merge quickly (not merge function)  
    factor_test_list <- 
    factor_test_list <- c(factor_test_list,intersect(splitA.var,splitB.var))
    #View(unique(factor_test_list))
    View(intersect(splitA.var,splitB.var))
}

xyList <- c()
gnames <- c()
gnames <- c(gnames, 'yFYield_CSUSHPINSA')

gnames <- c(gnames, factor_test_list)

xyList = colnames(MyData[ ,which((names(MyData) %in% gnames)==TRUE)])

training1Model <- lm(yFYield_CSUSHPINSA~.,train1_xy_set[xyList])
training2Model <- lm(yFYield_CSUSHPINSA~.,train2_xy_set[xyList])

resultsAll1 <- ols_step_all_possible(training1Model, p=.05)
resultsAll2 <- ols_step_all_possible(training2Model, p=.05)

{
  
  #Adj R^2 Filter
  adjR_Afilter <-  max(mean(resultsAll1$adjr),median(resultsAll1$adjr))
  adjR_Bfilter <-  max(mean(resultsAll2$adjr),median(resultsAll2$adjr))
  
  #Mallows' Cp-statistic estimates the size of the bias that is introduced into the predicted responses by having an underspecified model.
  # Mallows Distance from n Filter, lower absolute distance from n is better
  #better to allow for more higher mallow's CP values since they are so disparate and no need to punish so heavily early on
  #book says close to p+1
  mcp_A_floor <- min(mean(resultsAll1$cp-resultsAll1$n+1),median(resultsAll1$cp-resultsAll1$n+1))
  mcp_B_floor <- min(mean(resultsAll2$cp-resultsAll2$n+1),median(resultsAll2$cp-resultsAll2$n+1))
  
  #size filter
  size_A_floor <- min(mean(resultsAll1$n),median(resultsAll1$n))
  size_B_floor <- min(mean(resultsAll2$n),median(resultsAll2$n))
  
  #Error Filter
  error_AFilter <- min(mean(resultsAll1$msep),median(resultsAll1$msep))
  error_BFilter <- min(mean(resultsAll2$msep),median(resultsAll2$msep))
  
  #AIC Filter
  AIC_AFilter <- min(mean(resultsAll1$aic),median(resultsAll1$aic))
  AIC_BFilter <- min(mean(resultsAll2$aic),median(resultsAll2$aic))
  
  #SBC and SBIC 
  #SBIC_AFilter <- min(mean(resultsAll1$sbic),median(resultsAll1$sbic))
  SBC_AFilter <- min(mean(resultsAll1$sbc),median(resultsAll1$sbc))
  #SBIC_BFilter <- min(mean(resultsAll2$sbic),median(resultsAll2$sbic))
  SBC_BFilter <- min(mean(resultsAll2$sbc),median(resultsAll2$sbc))
  
  #APC Filter Higher is better
  #https://olsrr.rsquaredacademy.com/reference/ols_apc.html
  #Amemiya's Prediction Criterion penalizes R-squared more heavily than does adjusted R-squared for each addition degree of freedom used on the right-hand-side of the equation. 
  #The higher the better for this criterion.
  APC_AFilter <- max(mean(resultsAll1$apc),median(resultsAll1$apc))
  APC_BFilter <- max(mean(resultsAll2$apc),median(resultsAll2$apc))
  #& (resultsAAll$apc > APC_AFilter)
  #decisive, heavily penalizes per factor
  
  #Hocking's SP, lower is better
  #https://rdrr.io/cran/olsrr/man/ols_hsp.html
  HSP_AFilter <- min(mean(resultsAll1$hsp),median(resultsAll1$hsp))
  HSP_BFilter <- min(mean(resultsAll2$hsp),median(resultsAll2$hsp))
  
  #Average prediction mean squared error
  #Final Prediction Error
  FPE_AFilter <- min(mean(resultsAll1$fpe),median(resultsAll1$fpe))
  FPE_BFilter <- min(mean(resultsAll2$fpe),median(resultsAll2$fpe))
  
  #filtered
  #AIC and BIC hold the same interpretation in terms of model comparison. That is, the larger difference in either AIC or BIC indicates stronger evidence for one model over the other 
  #(the lower the better). It's just the the AIC doesn't penalize the number of parameters as strongly as BIC.Jan 7, 2014
  
  #results in none... but does not change final outcome when inverted, so removed
  #(resultsAAll$apc > APC_AFilter) &
  #subsetA <- filter(resultsAAll,  (resultsAAll$msep < error_AFilter) & (resultsAAll$adjr > adjR_Afilter)  & ((resultsAAll$cp-resultsAAll$n) <= mcp_A_floor) & (resultsAAll$n < size_A_floor)& (resultsAAll$aic < AIC_AFilter) & (resultsAAll$sbc < SBC_AFilter) & (resultsAAll$sbic < SBIC_AFilter)  & (resultsAAll$hsp < HSP_AFilter) & (resultsAAll$fpe < FPE_AFilter)) 
  #subsetB <- filter(resultsBAll,  (resultsBAll$msep < error_BFilter) & (resultsBAll$adjr > adjR_Bfilter)  & ((resultsBAll$cp-resultsBAll$n) <= mcp_B_floor) & (resultsBAll$n < size_B_floor)& (resultsBAll$aic < AIC_BFilter) & (resultsBAll$sbc < SBC_BFilter) & (resultsBAll$sbic < SBIC_BFilter)  & (resultsBAll$hsp < HSP_BFilter) & (resultsBAll$fpe < FPE_BFilter))
  
  #resultsAAll$
  subsetA <- filter(resultsAll1,  (resultsAll1$msep < error_AFilter) & (resultsAll1$adjr > adjR_Afilter)  & ((resultsAll1$cp-resultsAll1$n) <= mcp_A_floor) & (resultsAll1$n < size_A_floor)& (resultsAll1$aic < AIC_AFilter) &(resultsAll1$sbc < SBC_AFilter)  & (resultsAll1$hsp < HSP_AFilter) & (resultsAll1$fpe < FPE_AFilter)) 
  subsetB <- filter(resultsAll2,  (resultsAll2$msep < error_BFilter) & (resultsAll2$adjr > adjR_Bfilter)  & ((resultsAll2$cp-resultsAll2$n) <= mcp_B_floor) & (resultsAll2$n < size_B_floor)& (resultsAll2$aic < AIC_BFilter) & (resultsAll2$sbc < SBC_BFilter)  & (resultsAll2$hsp < HSP_BFilter) & (resultsAll2$fpe < FPE_BFilter)) 
  
}

f_list <- intersect(subsetA$predictors,subsetB$predictors)
View(f_list)

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

#https://stackoverflow.com/questions/32712301/create-empty-data-frame-with-column-names-by-assigning-a-string-vector

#used to hold all models
cv_model <- c()

#due to the nature of ad-hoc runs, the seed could be in any state unless known to have run the code from start to finish
#having the seed here ensures static results each run [even if ad hoc]
set.seed(124)
cv_model <- data.frame(matrix(ncol=9,nrow=0))
cv_model10 <- data.frame(matrix(ncol=9,nrow=0))
cv_model10_log <- cv_model10 <- data.frame(matrix(ncol=9,nrow=0))
cnames <- c('factor_list', 'n', 'cv', 'co-efficients', 'p-values', 'RMSE', 'RSS', 'adjR', 'model_p_sign')
colnames(cv_model) <- cnames
colnames(cv_model10) <- cnames
colnames(cv_model10_log) <- cnames

sub_average_object <- c()
sub_holding <- c()

cv_colnames <- c('factor_list','n','RMSE', 'RSS', 'adjR', 'model_p_sign')

sub_average_object <- data.frame(matrix(ncol=9,nrow=0))
sub_average_object<- data.frame(matrix(ncol=6,nrow=0))
sub_holding <- data.frame(matrix(ncol=6,nrow=divisions))

colnames(sub_holding) <- cv_colnames
colnames(sub_average_object) <- cv_colnames

#10% was too low (leverage of 1 threw an error, can only assume 10% CV window too small, I'd almost prefer to do 33%), doing 25% CV
#a=0
#i=3
for (i in seq(f_list)) 
{
  #higher level index
  sub_cv_model <- c()
  sub_cv_model <- data.frame(matrix(ncol=6,nrow=0))
  colnames(sub_cv_model) <- cv_colnames
  
  factor_list <- c()
  var <- c()
  #a=a+1
  factor_list <- f_list[i]
  
  #https://stackoverflow.com/questions/24741541/split-a-string-by-any-number-of-spaces
  vars <- scan(text = factor_list, what = "")
 
  # #divisions in CV Passes
  
  #lowest I can go is 4, 1/4 = 25% * 118 = ~30 for model building.  CV passes need to be done on completely different data.
  #3 40 for training and results in 25 per fold
  #4 results in a 25/75% split with 22 values per fold
  
  #3 is good for metric (triangulation) purposes, for my intended goal of finding centerpoints for models, triangles are perfect
  divisions=3 #(5 results in a 20% training  n=14 records / 60% validation split, each validation will be same size as training on potentially overfitted, but by having holdout data, ovefit concern is removed)
  
  # used for averages
  
  #i=3
  for (i in 1:3)
  {
    s_true=0

    validation1 <- sample(vld_size, replace=F)

    #unique non repeat sampled data, bootstrapped in terms that a new sample is generated each iteration    
    training = validation1[1:(nrow(x)/2)]
    validation = validation1[((nrow(x)/2)+1):(nrow(x))]
    
    trainingValidModel <-c()
    fit<- c()
    
    valid1_xy_set  <- c()
    test1_xy_set  <- c()
    
    valid1_xy_set <- MyData[training, c(xList,yField)]
    
    #test set will be 25% of training set and will not be the value of the training set
    test1_xy_set <- MyData[validation, c(yField,xList)]
    
    names <- c()
    names <- c(names, 'yFYield_CSUSHPINSA')
    names <- c(names, vars)
    
    #print(names)
    
    #names2 was created to fix a bug with a reserved name in a later function
    names2 <-c()
    names2 <- names
    #using valid1 data
    colnames(valid1_xy_set)
    trainingValidModel <- lm(valid1_xy_set[names2])
    
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

        #https://www.rdocumentation.org/packages/DescTools/versions/0.99.19/topics/Measures%20of%20Accuracy
    MAE(fit)
    MAPE(fit)
    MSE(fit)
    SMAPE(fit)
    
    #https://stats.stackexchange.com/questions/248603/how-can-one-compute-the-press-diagnostic
    hist(fit$residuals)
    
    #print(summary(fit)) # show results

    layout(matrix(c(1,2,3,4),2,2))
    plot(fit)
    
    layout(matrix(c(1),1))
    olsrr::ols_plot_resid_stud_fit(fit)
    
    plot(testdistPred,fit$residuals)
    
    #provides residual stats
    resultsAll <- ols_regress(fit, p=.05)

    #http://ugrad.stat.ubc.ca/R/library/locfit/html/cp.html
    #need example use
    #cp(valid1_xy_set[names],sig2=1)
    
    holding <- rbind(c(factor_list, resultsAll$n, paste("cv", toString(i)), toString(resultsAll$betas), toString(resultsAll$pvalues), RMSE(fit), PRESS(fit), resultsAll$adjr, resultsAll$p))
    
    #final RMSE should be evaluated against model that performs best against actual data.
    cv_model <- rbind(cv_model,holding)
    #sub_holding <- rbind(c(factor_list, resultsAll$n, RMSE(fit), PRESS(fit), resultsAll$adjr, resultsAll$p))
    cv_model10 <- rbind(cv_model10,holding)
  }

  #RMSE
  #https://stackoverflow.com/questions/18045096/r-error-sum-not-meaningful-for-factors
  #R error “sum not meaningful for factors”
  #RMSE vs RSS
  #https://stats.stackexchange.com/questions/206274/relationship-between-rmse-and-rss
  #Having the mathematical derivations, you might ask yourself why use one measure over the other to assess the performance of a given model? You could use either, but 
  #the advantage of RMSE is that it will come out in more interpretable units. 
  #For example, if you were building a model that used house features to predict house prices, 
  #RSS would come out in dollars squared and would be a really huge number. 
  #RMSE would come out in dollars and its magnitude would make more sense given the range of your house price predictions.
  #Mine are not in dollars, but values based on returns [of already normalized CSUSHPINSA]
  
  #reset cv_model10
  
  holding <- rbind(c(factor_list, resultsAll$n, paste("cv", toString(i)), "betas", "pvalues", mean(as.numeric(as.character(cv_model10[,6]))),mean(as.numeric(as.character(cv_model10[,7]))),mean(as.numeric(as.character(cv_model10[,8]))),mean(as.numeric(as.character(cv_model10[,9])))))
  #colnames(cv_model10_log) <- cnames
  cv_model10_log <- rbind(cv_model10_log,holding)
  View(cv_model10_log)
  
  #reset cv_model10
  cv_model10 <- c()
}

#within 1 standard deviation from the minimum, but also at least 1 standard deviation away from the mean
RMSE_error_Filter = sd(as.numeric(as.character(cv_model10_log$RMSE)))+min(as.numeric(as.character(cv_model10_log$RMSE)))

topPicks <- filter(cv_model10_log, (as.numeric(as.character(cv_model10_log$RMSE)) < RMSE_error_Filter))

colnames(cv_model10_log) <- cnames
colnames(topPicks) <- cnames
par(mfrow=2:1)
{
  hist((as.numeric(as.character(cv_model10_log$RMSE))))
  hist((as.numeric(as.character(topPicks$RMSE))))
}
View(topPicks)

colnames(topPicks) <- cnames
#trying to order by RMSE

#View(cbind(topPicks$factor_list, topPicks$RMSE))
#nogo

#test = ascending((topPicks$RMSE),topPicks)

#index of order
#order(topPicks['RMSE'])

#order(as.numeric(as.character(topPicks$'RMSE')))

#manual sort.... but order is not confirmed as correct
for (i in 1:nrow(topPicks))
{
  
}


#View()

#wish to tabulate picked factor names
#as.table(topPicks)
write.csv(topPicks,"cv_models.csv")
#appendix
#rename column
#http://rprogramming.net/rename-columns-in-r/
#colnames(data)[colnames(data)=="old_name"] <- "new_name"

#http://r-statistics.co/Linear-Regression.html
#https://www.rdocumentation.org/packages/EnvStats/versions/2.3.1/topics/predict.lm

#https://dplyr.tidyverse.org/reference/filter.html

#filter
#https://blog.exploratory.io/filter-data-with-dplyr-76cf5f1a258e

