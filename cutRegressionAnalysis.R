
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

cv_colnames <- c('factor_list','n','RMSE', 'RSS', 'adjR', 'model_p_sign')

sub_average_object <- data.frame(matrix(ncol=9,nrow=0))
sub_average_object<- data.frame(matrix(ncol=6,nrow=0))

colnames(sub_average_object) <- cv_colnames

#10% was too low (leverage of 1 threw an error, can only assume 10% CV window too small, I'd almost prefer to do 33%), doing 25% CV
#a=0
#i=3
for (i in seq(f_list)) 
{
  #rescramble set1
  set1 <- preset1[sample(length(preset1), replace=F)]
  
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
    
    validation1 <- sample(length(set1), replace=F)
    
    #unique non repeat sampled data, bootstrapped in terms that a new sample is generated each iteration    
    training = validation1[1:floor(length(set1)/2)]
    validation = validation1[ceiling(length(set1)/2+1):length(set1)]
    
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
    
    #olsrr::ols_plot_resid_stud_fit(trainingValidModel)
    
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
    #olsrr::ols_plot_resid_stud_fit(fit)
    
    plot(testdistPred,fit$residuals)
    
    #provides residual stats
    resultsAll <- ols_regress(fit, p=.05)
    
    #http://ugrad.stat.ubc.ca/R/library/locfit/html/cp.html
    #need example use
    #cp(valid1_xy_set[names],sig2=1)
    
    holding <- rbind(c(factor_list, resultsAll$n, paste("cv", toString(i)), toString(resultsAll$betas), toString(resultsAll$pvalues), RMSE(fit), PRESS(fit), resultsAll$adjr, resultsAll$p))
    
    #final RMSE should be evaluated against model that performs best against actual data.
    cv_model <- rbind(cv_model,holding)
    
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
  #View(cv_model10_log)
  
  #reset cv_model10
  cv_model10 <- c()
}

#within 1 standard deviation from the minimum, but also at least 1 standard deviation away from the mean
colnames(cv_model10_log) <- cnames
RMSE_error_Filter = sd(as.numeric(as.character(cv_model10_log$RMSE)))+min(as.numeric(as.character(cv_model10_log$RMSE)))

topPicks <- filter(cv_model10_log, (as.numeric(as.character(cv_model10_log$RMSE)) < RMSE_error_Filter) )

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

post_holding <- c()
#colnames(cv_model10_log) <- cnames
post_cv_model10 <- c()
#View(cv_model10_log)

#reset cv_model10
post_cv_model <- c()
for (i in topPicks$factor_list)
{
  
  
  #upper (test)
  
  
  names <- c()
  
  sub_cv_model <- c()
  sub_cv_model <- data.frame(matrix(ncol=6,nrow=0))
  colnames(sub_cv_model) <- cv_colnames
  
  factor_list <- c()
  var <- c()
  #a=a+1
  factor_list <- i
  
  #https://stackoverflow.com/questions/24741541/split-a-string-by-any-number-of-spaces
  
  names <- c(names, 'yFYield_CSUSHPINSA')
  
  vars <- scan(text = factor_list, what = "")
  
  names <- c(names, vars)
  for (i in 1:3)
  {
    #rescramble set2
    set2 <- preset2[sample(length(preset2), replace=F)]
    
    #great question, train on old data (set1), or train on new data? (set2)
    #equal split
    post_training <- set2[1:floor(length(set2)*.5)]
    post_testing <- set2[ceiling(length(set2)*.5):length(set2)]
    
    post_training_data <- c()
    post_testing_data <- c()
    
    post_training_data <- MyData[post_training, ][c(names)]  # model training data
    post_testing_data <- MyData[post_testing, ][c(names)]  # model training data
    
    post_testing_xy_set <- c()
    post_testing_xy_set <- post_testing_data
    
    post_training_xy_set <- c()
    post_training_xy_set <- post_training_data
    
    post_trainingModel <- lm(post_training_data)
    #post_testing_Model <- lm(post_testing_data)
    
    post_fit <- lm(post_trainingModel, data=post_testing_xy_set)
    post_testdistPred <- predict(post_fit)
    #post_trainValidPred <- predict(trainingValidModel)
    print(summary(post_fit))
    
    post_resultsAll <- ols_regress(fit, p=.05)
    
    #http://ugrad.stat.ubc.ca/R/library/locfit/html/cp.html
    #need example use
    #cp(valid1_xy_set[names],sig2=1)
    
    post_holding <- rbind(c(factor_list, post_resultsAll$n, paste("cv", toString(i)), toString(post_resultsAll$betas), toString(post_resultsAll$pvalues), RMSE(fit), PRESS(fit), post_resultsAll$adjr, post_resultsAll$p))
    
    #final RMSE should be evaluated against model that performs best against actual data.
    post_cv_model <- rbind(post_cv_model,post_holding)
    
    post_cv_model10 <- rbind(post_cv_model10,post_holding)    
    
  }
  post_holding <- rbind(c(factor_list, resultsAll$n, paste("cv", toString(i)), "betas", "pvalues", mean(as.numeric(as.character(post_cv_model10[,6]))),mean(as.numeric(as.character(post_cv_model10[,7]))),mean(as.numeric(as.character(post_cv_model10[,8]))),mean(as.numeric(as.character(post_cv_model10[,9])))))
  #colnames(cv_model10_log) <- cnames
  post_cv_model10 <- rbind(post_cv_model10,holding)
  #View(cv_model10_log)
  
  #reset cv_model10
  post_cv_model <- c()
  
}

View(post_cv_model10)

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



#used for comprehensive lists
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
  #error_AFilter <- min(mean(resultsAll1$rmse),median(resultsAll1$rmse))
  error_BFilter <- min(mean(resultsAll2$msep),median(resultsAll2$msep))
  #error_BFilter <- min(mean(resultsAll2$rmse),median(resultsAll2$rmse))
  
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
  #doesnt work with backwards
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
  #& (resultsAll1$fpe < FPE_AFilter)
  subsetA <- filter(resultsAll1,  (resultsAll1$msep < error_AFilter) & (resultsAll1$adjr > adjR_Afilter)  & ((resultsAll1$cp-resultsAll1$n) <= mcp_A_floor) & (resultsAll1$n < size_A_floor)& (resultsAll1$aic < AIC_AFilter) &(resultsAll1$sbc < SBC_AFilter)  & (resultsAll1$hsp < HSP_AFilter) & (resultsAll1$fpe < FPE_AFilter) ) 
  subsetB <- filter(resultsAll2,  (resultsAll2$msep < error_BFilter) & (resultsAll2$adjr > adjR_Bfilter)  & ((resultsAll2$cp-resultsAll2$n) <= mcp_B_floor) & (resultsAll2$n < size_B_floor)& (resultsAll2$aic < AIC_BFilter) & (resultsAll2$sbc < SBC_BFilter)  & (resultsAll2$hsp < HSP_BFilter) & (resultsAll2$fpe < FPE_BFilter)) 
  
}

#f_list <- intersect(subsetA$predictors,subsetB$predictors)

if (length(f_list)==0)
{
  f_list <- intersect(resultsAll1$predictors,resultsAll2$predictors)
}
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


folds = sample(1:divisions,nrow(subset),replace=TRUE)
View(folds)

length(folds[folds %in% 8])


for(j in 1:divisions){
  best.fit = regsubsets(yFYield_CSUSHPINSA ~., data=subset[folds != j,], nvmax = length(factor_test_list)-1)
  for (i in 1:(length(factor_test_list)-1)){
    pred = predict.regsubsets(best.fit, subset[folds == j, ], id = i)  
    #MSE
    cv.errors[j, i] = mean((subset$yFYield_CSUSHPINSA[folds == j] - pred)^2)
  }
}
#  colnames(x)


#cv.errors=matrix(NA,divisions,,)

for(j in 1:divisions){
  #best.fit = regsubsets(yFYield_CSUSHPINSA ~., data=subset[folds != j,], nvmax = length(factor_test_list)-1)
  knn_model <- knn.reg.bestK(subset[folds == j,], subset[folds != j,], subset[folds == j,]$yFYield_CSUSHPINSA, subset[folds != j,]$yFYield_CSUSHPINSA)
  #predictions
  #knn.reg(dat[id.train, ], test = dat[id.test, ], dat$yFYield_CSUSHPINSA[id.train], k = knn_model$opt)
  #for (i in 1:(length(factor_test_list)-1)){
  #pred = predict.regsubsets(best.fit, subset[folds == j, ], id = i)  
  yhat = knn.reg(subset[folds == j,], subset[folds != j,], subset[folds == j,]$yFYield_CSUSHPINSA, knn_model$k.opt)
  yhat.test = rep(0, length(id.test))
  yhat.test[yhat$pred > 0] = 1
  print(yhat.test)
  #MSE
  #cv.errors[j, i] = mean((subset$yFYield_CSUSHPINSA[folds == j] - pred)^2)
  #cv.errors[j, i] = mean(yhat.test != dat2$BL_yFYield_CSUSHPINSA[id.test])
  cv.errors[j] = mean(yhat.test != MyData[folds != j,]$BL_yFYield_CSUSHPINSA)
  #}
}


#https://en.wikipedia.org/wiki/Cross-validation_(statistics)
#repeated random sub-sampling validation

#weird method to bootstrap.  Assigns each value to a k-fold

train.control <- trainControl(method = "cv", number = 10)

model <- train(yFYield_CSUSHPINSA ~., data = subset, method = "knn",
               trControl = train.control)


write.csv(factor_test_list,"factor_test_list.csv")
