#cross validation
library(caret)
library(DAAG)

library(ISLR)
library(knitr)
library(printr)

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

library(leaps)
library(car)
library(ggvis)

#https://lagunita.stanford.edu/c4x/HumanitiesSciences/StatLearning/asset/ch6.html
predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  mat[, names(coefi)] %*% coefi
}

knn.reg.bestK = function(Xtrain, Xtest, ytrain, ytest, kmax=20) {
  vec.rmse = rep(NA, kmax)
  for (k in 1:kmax) {
    yhat.test = knn.reg(Xtrain, Xtest, ytrain, k)$pred
    vec.rmse[k] = rmse(yhat.test, ytest)
  }
  list(k.opt = which.min(vec.rmse), rmse.min = min(vec.rmse), vec.rmse)
}

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

pre_MyData <- read.csv(file="prepped.csv", header=TRUE, sep=",")

#fedvar was causing too much chaos with trainingDatasets having all 0's
dropColumns = colSums(pre_MyData == 0, na.rm = TRUE)


filtered <- c()
for (i in 1:nrow(data.frame(dropColumns)))
{
  
  #parsedList2 <- parsedList[!parsedList %in% c(filtered)]  
  
  #if(i>(floor*nrow(test1_z)))
  print(data.frame(dropColumns[i])[,1])
  #not really a percentage, more so a minimal acceptable loss, quarters are 75%, fedmin was 92%
  #if(data.frame(dropColumns[i])[,1]>=(seasonalConstant+2))
  if((data.frame(dropColumns[i])[,1])>=.80*nrow(pre_MyData))
  {
    print("yes")
    
    #works
    filtered <- rbind (filtered,i)
    #works
    print(colnames(pre_MyData)[i])
    #does not work
    #filtered <- rbind(filtered,colnames(test2_z)[i-1])
  }
  else
  {
    print("no")
  }
  
}

MyData <- (pre_MyData[,-c(filtered)])

fieldOfInterest='yFYield_CSUSHPINSA'

#MyData <- pre_MyData

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

#else doing expensive error try blocks due to aliased coefficients
train_size = .90
#test_size = 1- train_size

#View(colnames(MyData))
vars <- c()
#data is too big for exhaustive search
factor_test_list<-c()

a=1
set.seed(255)
preset_rng <- sample(nrow(MyData), replace=F)

#contemplating counting factors as they appear in coefficients of the 
#resultant linear model and tabulating the end result and deriving my 
#own frequency chart and pick factors that appears more than the median/median

colnames(MyData)

divisions=10
for (i in 1:divisions)
{
  i=1
  
  final_end=length(preset_rng)
  unitsize=floor(final_end*.1)
  
  initial_start=1+((i-1)*unitsize)
  initial_start

  distance=final_end-initial_start
  
  #90%
  part_size=floor(final_end*.9)
  
  difference=(distance-part_size)
  
  if (distance>=part_size)
  {
    end_position=initial_start+part_size
    preset1=(c(preset_rng[initial_start:end_position]))
  }
  
  if (distance<part_size)
  {
    end_position=final_end
    left=part_size-(end_position-initial_start)
    left
    preset1=(c(preset_rng[initial_start:end_position],preset_rng[1:left]))
  }
  
  #static training set
  
  #training/validation sets (split into rebootstrapped 1:1 distinct partitions)
  #preset1 <- preset_rng[1:floor(nrow(MyData)*train_size)]
  preset1 <- preset_rng[1:nrow(MyData)*train_size]
  #static testing set
  #preset2 <- preset_rng[ceiling(nrow(MyData)*train_size):nrow(MyData)]
  preset2 <- setdiff(1:nrow(MyData), preset1) # setdiff gives the set difference
  
  print(i)
  #lower
  
  #rescramble set1
  set1 <- preset1[sample(length(preset1), replace=F)]
  
  #training <- sample(smp_size, replace=F)
  #print(training)
  
  #goal is to find combinations of 6-7 that work
  #rule of thumb is n for factor analysis should be 5(k*2)
  #with /2 mine is 59, which is good for up to 11 factors (12 is 60), it would be ideal to have 80 since I'm starting with 14 factors, but oh well, since I'm aggregating the lists, I'm assuraedly going to lose thosee upper combitorial lists since those upper lists will be overfitted to the small sample sizes, which means wasted processing [that will be trimmed] (vs actually saving higher level models) and smaller pruned lists will only be stored and when the filter goes into affect, the affect will be much harsher.
  training1 <- set1[1:floor(length(set1))]
  #training2 <- set1[(ceiling(length(set1)/2)+1):length(set1)]
  
  #http://r-statistics.co/Linear-Regression.html
  #used to build models on new ranodmized data and then tested against holdout test data (to include cross validation)
  #not limited to just the first portion of the percent
  #provides an index
  
  training1Data <- c()
  #training2Data <- c()

  #new plan is to create two randomized partitions that will have complete dataset algo's done on them.
  training1Data <- MyData[training1, ]  # model training data
  #training2Data <- MyData[training2, ]  # model training data
  
  train1_xy_set <- c()
  #train2_xy_set <- c()
  
  train1_xy_set <- training1Data[c(yField,xList)]
  #train2_xy_set <- training2Data[c(yField,xList)]
  
  names <- c()
  
  names <- c(names, fieldOfInterest)
  #SPSSReducedModel <- c('yFYield_CSUSHPINSA','Q1','Q2','Q3','Q4','xYield_CASTHPI','xYield_CPALTT01USQ657N','xYield_GS10','xYield_MSPNHSUS','xYield_MVLOAS','xYield_NYXRSA','xYield_POP','xYield_POPTHM',xYield_SDXRSA','xYield_TB3MS','xYield_UMCSENT','xYield_USSLIND')
  #removed xYield_POP due to high correlation with xYield_POPTHM as well as Q2 for collinearity reasons
  #trainingModel <- lm(yFYield_CSUSHPINSA ~ Q1 + Q3 + Q4 + xYield_CASTHPI + xYield_CPALTT01USQ657N + xYield_GS10 + xYield_MSPNHSUS + xYield_MVLOAS + xYield_NYXRSA + xYield_POPTHM + xYield_SDXRSA + xYield_TB3MS + xYield_UMCSENT + xYield_USSLIND, data = train_xy_set)
  vars <- xList
  names <- c(names, vars)
  training1Model <- lm(yFYield_CSUSHPINSA~.,train1_xy_set)
  #olsrr::ols_plot_resid_stud_fit(training1Model)
  #anova(training1Model,training2Model)
  #linearHypothesis(training1Model)
  #alias
  
  summary(training1Model)
  #summary(training2Model)
  
  #https://stackoverflow.com/questions/27128455/r-try-catch-block
  alias(training1Model)
  
  #deal with "there are aliased coefficients in the model" from ols_step funtion
  #https://stats.stackexchange.com/questions/112442/what-are-aliased-coefficients

  resultsAAll <- c()
  aflag = 0
  result = tryCatch({
    #vs step all
    #resultsAAll <- ols_step_forward_p(training1Model, penter=.1)
  }, warning = function(w) {
    #warning-handler-code
    print("warning")
    aflag = 0
    #resultsAAll <- ols_step_forward_p(training1Model, penter=.075)
  }, error = function(e) {
    #break
    resultsAAll <- c()
    print("error")
    aflag = 1
  }, finally = {
    
  })
  print(aflag)
  if(aflag==0)
  {
    resultsAAll <- ols_step_backward_p(training1Model, penter=.05)
  }
  
  resultsBAll <- c()
  bflag = 0
  result = tryCatch({
    #resultsBAll <- ols_step_forward_p(training2Model, penter=.1)
  }, warning = function(w) {
    #warning-handler-code
    print("warning")
    bflag = 0
    #resultsBAll <- ols_step_forward_p(training2Model, penter=.075)
  }, error = function(e) {
    resultsBAll <- c()
    #break
    print("error")
    bflag = 1
    #break
  }, finally = {
    
  })
  print(bflag)
  if(bflag==0)
  {
    #resultsBAll <- ols_step_forward_p(training2Model, penter=.05)
  }
  
  #resultsBAll <- ols_step_all_possible(training2Model, p=.05)
  #careful, takes a long time
  #****
  
  #remove intercept
  tail(row.names(data.frame(resultsBAll$model$coefficients)),-1)
  
  #grab names of vars
  splitA.var <- strsplit(tail(row.names(data.frame(resultsAAll$model$coefficients)),-1), " ")
  #splitB.var <- strsplit(tail(row.names(data.frame(resultsBAll$model$coefficients)),-1), " ")

  #max(subsetA$n,subsetB$n)

  #https://stackoverflow.com/questions/28885160/vifs-returning-aliased-coefficients-in-r
  checkColLins <- c('yFYield_CSUSHPINSA',tail(row.names(data.frame(resultsAAll$model$coefficients)),-1))
    
  signif_all <- c()
  
  lmMod <- lm(train1_xy_set[checkColLins])
  selectedMod <- lmMod
  ld.vars <- attributes(alias(selectedMod)$Complete)$dimnames[[1]]
  
  #collinearity check
  while(any(ld.vars > 1)){
    signif_all <- checkColLins[!checkColLins %in% c(ld.vars)]  # remove 
    myForm <- lm(train1_xy_set[signif_all])
    selectedMod <- lm(myForm)  # re-build model with new formula
    #all_vifs <- car::vif(selectedMod)
    ld.vars <- attributes(alias(selectedMod)$Complete)$dimnames[[1]]
  }

  #VIF check
  #merge quickly (not merge function)  
  
  oldset <- signif_all
  
  lmMod <- lm(train1_xy_set[oldset])
  selectedMod <- lmMod
  all_vifs <- round(car::vif(selectedMod),0)
  
  while(any(ld.vars > 10)){
    var_with_max_vif <- names(which(all_vifs == max(all_vifs)))
    signif_all <- signif_all[!signif_all %in% c(names(all_vifs))]  # remove 
    myForm <- lm(train1_xy_set[signif_all])
    selectedMod <- lm(myForm)  # re-build model with new formula
    #all_vifs <- car::vif(selectedMod)
    all_vifs <- round(car::vif(selectedMod),0)
    #print(signif_all)
  }
  
  #https://stackoverflow.com/questions/45960255/r-error-unexpected-else-in-else
  if(a==1){
  print("yes")
  factor_test_list <- c(factor_test_list,signif_all)
  print(factor_test_list)
  } else {
  factor_test_list <- intersect(splitA.var,factor_test_list)
  print(factor_test_list)
  print("no")
  }
  
  a=a+1
}

View(unique(factor_test_list))
#27 when I fixed the /2 bug (when I continue to add)

#reshuffle sets
set1 <- preset1[sample(length(preset1), replace=F)]
#set2 <- preset1[sample(length(preset1), replace=F)]
#training1 <- set1[1:floor(length(set1))]
#training2 <- set1[(ceiling(length(set1)/2)+1):length(set1)]

#same xylist to two different training partitions
xyList=c('yFYield_CSUSHPINSA',c((unique(factor_test_list))))

#t1 and t2 needed for ols_step_all_possible
#this juncture is important, even though prior i'm testing the whole dataset with cv passes... 
#here I really am just getting a list.  I just don't want the expense of doing the cv, 
#which I WOULD do if it wasn't so expensive

#if doing CV
#t1 <- train1_xy_set[set1,xyList]
t1 <- MyData[xyList]
#t2 <- train2_xy_set[c(yField,xyList)]
training1Model <- lm(t1)
#training2Model <- lm(yFYield_CSUSHPINSA~.,t2)

#resultsAll1 <- c()
#resultsAll2 <- c()
#look into leaps for iterarting this.
#x= colnames(t1[!colnames(t1) %in% c('yFYield_CSUSHPINSA')])
x= t1[!colnames(t1) %in% c('yFYield_CSUSHPINSA')]
y= t1['yFYield_CSUSHPINSA']

#length(x)
#resultsLeapsAll <- leaps(x, y)
#resultsAll1 <- ols_step_all_possible(training1Model, p=.05)
#resultsAll2 <- ols_step_all_possible(training1Model, p=.05)

#this is for subsets
train_size = .90
#test_size = 1- train_size

#View(colnames(MyData))
vars <- c()
#data is too big for exhaustive search
#factor_test_list<-c()

#subset <- dat[set1,][xyList]
subset <- MyData[xyList]

#weird method to bootstrap.  Assigns each value to a k-fold
folds = sample(1:divisions,nrow(subset),replace=TRUE)

#https://rpubs.com/davoodastaraky/subset
cv.errors=matrix(NA,divisions,(length(factor_test_list)-1), dimnames=list(NULL, paste(1:(length(factor_test_list)-1))))

for(j in 1:divisions){
  best.fit = regsubsets(yFYield_CSUSHPINSA ~., data=subset[folds != j,], nvmax = length(factor_test_list)-1)
  for (i in 1:(length(factor_test_list)-1)){
    pred = predict.regsubsets(best.fit, subset[folds == j, ], id = i)  
    #MSE
    cv.errors[j, i] = mean((subset$yFYield_CSUSHPINSA[folds == j] - pred)^2)
  }

}
#  colnames(x)

which.min(cv.errors)  
#View(cv.errors)

#https://rpubs.com/davoodastaraky/subset
mean.cv.errors = apply(cv.errors ,2,mean)

#find ideal size
plot(mean.cv.errors, pch = length(factor_test_list), type = "b")

bestSize = which(mean.cv.errors %in% min(mean.cv.errors))

bestsubset = regsubsets(yFYield_CSUSHPINSA ~ ., data = subset, nbest=1, nvmax=bestSize, method=c("exhaustive"))

#https://rstudio-pubs-static.s3.amazonaws.com/2897_9220b21cfc0c43a396ff9abf122bb351.html
plot(bestsubset, scale = "adjr2", main = "Adjusted R^2")

finish = coef(bestsubset, bestSize)

cv.errors=matrix(NA,divisions,,)

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





#kNNdistplot(dat[id.train, ], k=knn_model$k.opt)


print(finish)
#plot(subsets)
View(finish)
write.csv(finish, "coefficients.csv")
