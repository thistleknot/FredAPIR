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

train_size = .70
#test_size = 1- train_size

preset_rng <- sample(nrow(MyData), replace=F)
#static training set

#training/validation sets (split into rebootstrapped 1:1 distinct partitions)

#static testing set
#preset2 <- preset_rng[ceiling(nrow(MyData)*train_size):nrow(MyData)]

#dat <- dat[-which(colnames(dat) == 'Met_Color')]
#dat is yxlist
dat <- cbind(y,x)

#View(colnames(MyData))
vars <- c()
#data is too big for exhaustive search
factor_test_list<-c()
set.seed(256)

  print(i)
  #lower
  
  #rescramble set1
  id.train <- preset_rng[1:floor(nrow(MyData)*train_size)]
  id.test = setdiff(1:nrow(dat), id.train) # setdiff gives the set difference
  
  training1Data <- c()
  
  #new plan is to create two randomized partitions that will have complete dataset algo's done on them.
  training1Data <- MyData[id.train, ]  # model training data
  
  train1_xy_set <- c()
  
  train1_xy_set <- training1Data[c(yField,xList)]
  
  names <- c()
  
  #keep y in front, add x's
  names <- c(fieldOfInterest, names)
  vars <- xList
  names <- c(names, vars)

  training1Model <- lm(train1_xy_set)

  
## below is just an example with manually selected variable: age, fuel type and weight; just for illustration purpose
#obj = lm(Price ~ Age_08_04 + factor(Fuel_Type) + Weight, data = dat[id.train, ])
# dat[id.train, ] takes rows of id.train
# if you put some integer after the comma, it will take the corresponding columns
# for example, if you put dat[, c(1, 5, 3)], it gives you the 1st, 5th, 3rd columns of the data
#plot(obj) # see what happened?!
## Model diagnosis automatically implemented with the basic function plot()
## plot like magic
# residuals vs fitted: check Independence and Homoscedasticity
# Normal Q-Q: check Normality; want to see a straight line
# A good example: http://data.library.virginia.edu/diagnostic-plots/

#summary(obj) # gives the summary of output; you can also just type obj
# check the difference with and without summary()

#names(obj) # check what were saved in obj
#obj$resid
#class(obj$resid) # check the variable type
#plot(obj$resid)
#obj$fitted

# predict the future observation!
#yhat = predict(obj, newdata = dat5[id.test, ])
#length(yhat)
#length(id.test) # just to check if the same number of predicted value as id.test is reported

#plot(dat[id.test, 'Price'], yhat, xlab='Actual y', ylab='Fitted y')
#abline(0, 1, col='red') # add a line with intercept 0 and slope 1; we want to see points around this line

# install a package below if not yet
# install.packages('hydroGOF')
require(hydroGOF)

#sqrt(mean((dat5[id.test, 'Price'] - yhat)^2, na.rm=T)) ## manually calculate it! same

#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
# Milestone! We now know how to do MLR with manually selected variables
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################

######################################################
## Additional topic: variable selection (backward, forward, best subset)
# a good resource: http://www.stat.columbia.edu/~martin/W2024/R10.pdf
library(MASS)

### forward selection ###
## step 1: fit a null model and a full model first


#can't use dat[[1]] because it will add it to the current dataset, nor fieldOfInterest because it isn't read properly as a string nor toString(fieldOfInterest)
obj.null = lm(yFYield_CSUSHPINSA ~ 1, dat) # only intercept, 1, is included in the model
# obj.full = lm(Price ~ ., dat = dat5[id.train, ]) # if not specifying any independent variables, all other than Price will be independent variables
obj.full = lm(yFYield_CSUSHPINSA ~ .,dat = dat)
## scope is to start from a null model and end with a full model; direction is forward

#these are best final models after stepwise is applied
obj1 = step(obj.null, scope=list(lower=obj.null, upper=obj.full), direction='forward') # forward selection by Akaike information criterion (AIC)
# Mallows's Cp is equivalent to AIC in the case of (Gaussian) linear regression
### backward elimination ###
obj2 = step(obj.full, scope=list(lower=obj.null, upper=obj.full), direction='backward') # start with full and end with null; reversed comparing to forward
### stepwise selection ###
obj3 = step(obj.null, scope=list(lower=obj.null, upper=obj.full), direction='both') # start with full and end with null

summary(obj1) ## ending up with a model with 13 variables
length(obj1$coefficients)
summary(obj2) # end up with a model with 15 variables
length(obj2$coefficients)
summary(obj3) # end up with a model with 12 variables, final model same as backward
length(obj3$coefficients)

data.frame((obj1$coefficients))
data.frame((obj2$coefficients))
data.frame((obj3$coefficients))

#looking at backwards
rownames(data.frame((obj2$coefficients)))

#### Check some other model assumptions
# normality (of the residual)
hist(obj1$resid)
hist(obj2$resid)
hist(obj3$resid)

# Homoscedasticity
plot(obj1$resid, obj1$fitted)
plot(obj2$resid, obj2$fitted)
plot(obj2$resid, obj3$fitted)

#use this on final model

# linearity
layout(matrix(c(1,2,3,4,5,6,7,8,9),3,3)) # check the difference by removing this line
nc = length(obj2$coefficients)-1
for (i in 2:nc)
{
  plot(dat[[tail(row.names(data.frame(obj2$coefficients)),-1)[i]]], dat[[1]])
}

#correlation
cor(dat[[1]], dat, use='na.or.complete')
# check collinearity #
#par(mfrow=c(1,1))
layout(matrix(c(1,2,3,4,5,6,7,8,9),3,3))
for (i in 2:nc)
{
  
}

library("corrplot")
layout(matrix(c(1),1,1))
#correlation matrix of backwards
corrplot( cor(dat[c('yFYield_CSUSHPINSA',tail(row.names(data.frame(obj2$coefficients)),-1))], use='na.or.complete'))

#normality
# actually, plot(obj) gives you QQ plot, better way to check it

# plot(obj1) can also give plot like this

# there are some other assumptions such as "No Multicollinearity"
#qq
## A simpler approach to do model diagnosis
layout(matrix(c(1,2,3,4),2,2)) # check the difference by removing this line
plot(obj1)
plot(obj2)
plot(obj3)

yhat1 = predict(obj1, newdata = dat[id.test, ])
yhat2 = predict(obj2, newdata = dat[id.test, ])
yhat3 = predict(obj3, newdata = dat[id.test, ])

rmse(dat[id.test, 'yFYield_CSUSHPINSA'], yhat1) ## RMSE for test data
rmse(dat[id.test, 'yFYield_CSUSHPINSA'], yhat2) ## RMSE for test data
rmse(dat[id.test, 'yFYield_CSUSHPINSA'], yhat3) ## RMSE for test data

plot(dat[id.test, 'yFYield_CSUSHPINSA'], yhat1, xlab='Actual y', ylab='Fitted y')
abline(0,1,col='red')
plot(dat[id.test, 'yFYield_CSUSHPINSA'], yhat2, xlab='Actual y', ylab='Fitted y')
abline(0,1,col='red')
plot(dat[id.test, 'yFYield_CSUSHPINSA'], yhat3, xlab='Actual y', ylab='Fitted y')
abline(0,1,col='red')

bestRMSE = min(rmse(dat[id.test, 'yFYield_CSUSHPINSA'], yhat1),rmse(dat[id.test, 'yFYield_CSUSHPINSA'], yhat2),rmse(dat[id.test, 'yFYield_CSUSHPINSA'], yhat3))

#pick filtered subset from above
# best subset
library(leaps)
obj4 = regsubsets(yFYield_CSUSHPINSA ~ ., data = dat[id.train, ], nvmax=16)
## allow up to 20 variables in the model; we should put a constraint like this otherwise it will run forever
summary(obj4)

par(mfrow=c(1,1)) # par(mfrow=c(m,n)) allows us to put m*n figures in a single plot; if m=n=1, only one figure in the plot
plot(obj4, scale="adjr2") # black color indicates a variable is used in the model

# not recommended to use best subset at all

####################################################
# compare prediction results of forward, backward, stepwise


# rmse are 1111.592, 1115.026, 1115.026 for each
# rmse are very similar
# we choose whichever one of the three methods


# model diagnosis #
par(mfrow = c(2, 2))
plot(obj2) # not bad

yhat = predict(obj2, newdata=dat[id.test, ])
require(hydroGOF)
ytest = dat[id.test, 'yFYield_CSUSHPINSA']
rmse(ytest, yhat)

## 2. kNN prediction ##
library(FNN)

knn.reg(dat[id.train, ], test = dat[id.test, ], dat$yFYield_CSUSHPINSA[id.train], k = 3)

# not working since there are 2 string variables: Fuel_Type, Color
library(fastDummies)
#dat5 = fastDummies::dummy_cols(dat4)
#dat6 = subset(dat5, select = -c(Fuel_Type, Color))

knn.reg.bestK = function(Xtrain, Xtest, ytrain, ytest, kmax=20) {
  vec.rmse = rep(NA, kmax)
  for (k in 1:kmax) {
    yhat.test = knn.reg(Xtrain, Xtest, ytrain, k)$pred
    vec.rmse[k] = rmse(yhat.test, ytest)
  }
  list(k.opt = which.min(vec.rmse), rmse.min = min(vec.rmse), vec.rmse)
}

#knn.reg.bestK(x_training, x_test, y_training, y_test)
knn.reg.bestK(dat[id.train, ], dat[id.test, ], dat$yFYield_CSUSHPINSA[id.train], dat$yFYield_CSUSHPINSA[id.test])

## 3. Regression Tree ##
library(rpart); library(rpart.plot)
fit = rpart(Price~., method="anova", data=dat4[id.train,])
par(mfrow=c(1,1))
rpart.plot(fit)

yhat.test = predict(fit, newdata = dat4[id.test,-1])
rmse(yhat.test, ytest)

#### looks like MLR is the best one! ####
round(summary(obj1)$coef, 3)

