
#cross validation
library(caret)
library(DAAG)

library(olsrr)
library(ggplot2)


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

#results <- ols_step_all_possible(trainingModel, p=.05)

results <- ols_step_best_subset(trainingModel, p=.05)

#results <- ols_regress(trainingModel, p=.05)
View(results)

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

olsrr::ols_plot_resid_stud_fit(trainingModel)

#distPred <- predict(trainingModel, test_xy_set)
#http://r-statistics.co/Linear-Regression.html
fit <- lm(trainingModel, data=test_xy_set)
summary(fit) # show results

distPred

print(trainingModel)
trainingModel$coefnames

#results$predictors

write.csv(results,"bestSubset.csv")

summary.print(results)
View(summary.print(results))

#http://r-statistics.co/Linear-Regression.html


plot(results)
results
#results$aic

#jpeg('rplot.jpg')
#ggplot(results)
#dev.off()
#break immediately
#ols_step_backward_p(trainingModel, p=.1)

#colnames(merge(train_y,train_x))
