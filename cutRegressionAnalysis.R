
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

