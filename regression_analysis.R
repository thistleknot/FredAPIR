#cross validation
library(caret)
library(DAAG)

library(ISLR)
library(knitr)
library(printr)
library(FNN)

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
library(mclust)
library(dbscan)


#https://lagunita.stanford.edu/c4x/HumanitiesSciences/StatLearning/asset/ch6.html
predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  mat[, names(coefi)] %*% coefi
}

#Thumb rule is k = n^(1/2)
knn.reg.bestK = function(Xtrain, Xtest, ytrain, ytest, kmax=20) {
  vec.rmse = rep(NA, kmax)
  for (k in seq(1, kmax, by = 2)) {
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

#Bootstrap w resampling k-folds, assigns each value to a k-fold
#folds = sample(1:divisions,nrow(subset),replace=TRUE)
print(unique(factor_test_list))

#same xylist to two different training partitions
xyList=c('yFYield_CSUSHPINSA',c((unique(factor_test_list))))

#all data, factor_list
t1 <- MyData[xyList]

x= t1[!colnames(t1) %in% c('yFYield_CSUSHPINSA')]
y= t1['yFYield_CSUSHPINSA']

vars <- c()

#divisions = folds
# #factors (-1 due to response included in factor_test_list)
k=length(factor_test_list)-1
#k=13
cv.errors=matrix(NA,divisions,k, dimnames=list(NULL, paste(1:k)))
#reshuffle
set.seed(256)
preset_rng <- sample(nrow(MyData), replace=F)
divisions=10
for (i in 1:divisions)
{
  
  #iterator constant (for testing)
  #i = 1
  #preset1 = train index
  #preset2 = text index
  
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
  
  preset2 = preset_rng[!preset_rng %in% c(preset1)]
  
  training1Model <- lm(yFYield_CSUSHPINSA~.,MyData[preset1,xyList])
  summary(training1Model)

  #https://rstudio-pubs-static.s3.amazonaws.com/117080_1924569ef91b426c8ea7bff8dcbaf4f3.html
  
    ## Training set
    best.fit <- regsubsets(yFYield_CSUSHPINSA~.,data=MyData[xyList],nvmax=k)
    #best.fit$
    
    #up to k factors
    for(j in 1:k){
      #don't care about what type of model, wish to map the average error of the best model given a set # of factors
      pred <- predict.regsubsets(best.fit,MyData[preset2,xyList],id=j)
      #RMSE
      cv.errors[i,j] <- sqrt(mean((MyData[preset2,colnames(y)] - pred)^2))
    }
  
  #print()
  mean.cv.errors <- apply(cv.errors,2,mean)
  median.cv.errors <- apply(cv.errors,2,median)
  
 #View(MyData[preset1,xyList]) 
}

which.min(median.cv.errors)

layout(matrix(c(1,2,3,4),2,2))
plot(mean.cv.errors, type="b")
plot(median.cv.errors, type="b")

#minimum size within 1 standard error of minimum error (across a model using all 26 factors)
bestSize = min(which(mean.cv.errors <= (min(mean.cv.errors)+sd(mean.cv.errors))))
#bestSize = min(which(mean.cv.errors <= (min(median.cv.errors)+sd(median.cv.errors))))

bestsubset = regsubsets(yFYield_CSUSHPINSA ~ ., data = MyData[xyList], nbest=1, nvmax=bestSize, method=c("exhaustive"))

#https://rstudio-pubs-static.s3.amazonaws.com/2897_9220b21cfc0c43a396ff9abf122bb351.html

plot(bestsubset, scale = "adjr2", main = "Adjusted R^2")

finish = coef(bestsubset, bestSize)


summary(finishedModel)

#max and min yield for housing is 5.4%
max(MyData$yFYield_CSUSHPINSA)
min(MyData$yFYield_CSUSHPINSA)

reducedXList <- tail(row.names(data.frame(finish)),-1)
finalSet <- c('yFYield_CSUSHPINSA',reducedXList)

finishedModel <- lm(MyData[finalSet])

dates <- MyData[c('test2_z.date')]
scaled.dat <- scale(MyData[c('BL_yFYield_CSUSHPINSA',finalSet)])
reduced <- MyData[c('BL_yFYield_CSUSHPINSA',finalSet)]

distance <- data.frame(distance=sqrt(rowSums(scaled.dat[,3:ncol(scaled.dat)]^2)))
distance1 <- sqrt(rowSums(scaled.dat[,3:ncol(scaled.dat)]^2))
distance2 <- (rowSums(scaled.dat[,3:ncol(scaled.dat)]))
#sum of yields
distance3 <- (rowSums(reduced[,3:ncol(scaled.dat)]))


distance4 <- apply(scaled.dat[,3:ncol(scaled.dat)], 2, function(x) pnorm(x))

#average of cdf's

layout(matrix(c(1,2,3,4),2,2)) # check the difference by removing this line

#layout(matrix(c(1,2,3,4),1,2))
#standard knn distance based on mean
plot(distance1,MyData$yFYield_CSUSHPINSA)
#scaled x, not y
#plot(distance2,scale(MyData$yFYield_CSUSHPINSA))
plot(distance2,MyData$yFYield_CSUSHPINSA)
#unscaled
plot(distance3,MyData$yFYield_CSUSHPINSA)

distanceModel <- lm((MyData$yFYield_CSUSHPINSA)~distance2)
summary(distanceModel)
max(distanceModel$residuals)
hist(distanceModel$residuals)

#https://stackoverflow.com/questions/11106964/rename-the-columns-name-after-cbind-the-data
#rename on cbind


layout(matrix(c(1,2,3,4),1,2))
boxplot(reduced)
boxplot(scaled.dat)
write.csv(cbind(dates,scaled=scaled.dat,unscaled=reduced,distance=distance), file = "reduced.csv")

#kfinalSet <- c('yFYield_CSUSHPINSA',xList)

#goal of knn is to build it against it's own xlist

#however, because  want to map my above regression model to knn using kdistplot, I need to actually use the variables above...
#book recommends odd up to 20, so 19
k=19

cv.errors<-matrix(NA,divisions,k, dimnames=list(NULL, paste(1:k)))

for(i in 1:divisions){
  
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
  
  preset2 = preset_rng[!preset_rng %in% c(preset1)]
  
  knn_model <- knn.reg.bestK(MyData[preset1,finalSet], MyData[preset2,finalSet], MyData[preset1,finalSet]$yFYield_CSUSHPINSA, MyData[preset2,finalSet]$yFYield_CSUSHPINSA,k)
  #up to k factors
  for(j in seq(1, k, by = 2)){
    #predictions
    yhat = knn.reg(MyData[preset1,finalSet], MyData[preset2,finalSet], MyData[preset1,finalSet]$yFYield_CSUSHPINSA, j)
    #yhat.test = rep(0, length(preset2))
    #yhat.test[yhat$pred > 0] = 1
    #yhat.test[yhat$pred < 0] = 0
    print(yhat)
    
    #RMSE
    cv.errors[i,j] <- sqrt(mean((MyData[preset2,'yFYield_CSUSHPINSA'] - yhat$pred)^2))
    #cv.errors[i,j] <- knn_model$k.opt
    #cv.errors[i,j] <- knn_model$rmse.min
    
  }
 
}

#MSE
mean.cv.errors <- apply(cv.errors,2,mean)

oddvals <- seq(1, length(mean.cv.errors), by=2)

plot(oddvals,mean.cv.errors[oddvals], type="b")

#minimum errors
filter <- mean.cv.errors[oddvals] %in% mean.cv.errors[which(mean.cv.errors[oddvals] <=(min(mean.cv.errors[oddvals])+sd(mean.cv.errors[oddvals])))]

#minimum size within 1 standard error of minimum error (across a model using all 26 factors)

#knn_model <- knn.reg.bestK(MyData[reducedXList], test=NULL, MyData[reducedXList]$yFYield_CSUSHPINSA,k)

yhat = knn.reg(MyData[preset1,finalSet], MyData[preset2,finalSet], MyData[preset1,finalSet]$yFYield_CSUSHPINSA, j)
  
kbestSize = which(mean.cv.errors == mean.cv.errors[oddvals][filter])

knn_model <- knn.reg(MyData[reducedXList], test=NULL, MyData$yFYield_CSUSHPINSA, kbestSize)

summary(knn_model)

#unique(round(knn_model$pred,2))

layout(matrix(c(1,1,1,1),1,1))
#fit <- Mclust(MyData[finalSet])
#plot(fit) # plot results 
#summary(fit)

pairs(MyData[colnames(reduced)], pch = 19,lower.panel = NULL)

my_cols <- c("#00AFBB", "#E7B800") 
#my_cols <- c("#00AFBB", "#E7B800", "#FC4E07")  
pairs(MyData[colnames(reduced)], pch = 2,  cex = 0.75,
      col = my_cols[MyData$BL_yFYield_CSUSHPINSA+1],
      lower.panel=NULL)

pairs(cbind(distance2,MyData$yFYield_CSUSHPINSA), pch = 2,  cex = 0.75,
      col = my_cols[MyData$BL_yFYield_CSUSHPINSA+1],
      lower.panel=NULL)


#cols <- c("red","green","blue","orange")
#cols <- c("red","green")
#plot(MyData$xYield_CASTHPI~MyData$xYield_TB3MS, data = MyData[colnames(reduced)], col = cols[MyData$BL_yFYield_CSUSHPINSA], pch = 19)
#legend("topleft", legend = levels(MyData$BL_yFYield_CSUSHPINSA), col = cols, pch = 19, bty = "n")

sum(kNNdist(MyData[finalSet], kbestSize))/kbestSize

kNNdist(MyData[finalSet], kbestSize)

kNNdistplot(MyData[finalSet], kbestSize)

dataM <- as.matrix(MyData[finalSet])

#kNNdist(iris, k=kbestSize, search="kd")
cl <- dbscan(dataM, eps = .05, minPts = kbestSize)
pairs(dataM, col = cl$cluster+1L)

cl2 <- hdbscan(dataM, minPts = kbestSize)
plot(cl2$hc, main="HDBSCAN* Hierarchy")
plot(cl2)
#kNNdistplot(MyData[finalSet], kbestSize)

