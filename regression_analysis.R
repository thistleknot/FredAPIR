library(olsrr)

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
smp_size <- floor(0.75 * nrow(MyData))

## set the seed to make your partition reproducible
set.seed(123)

train_ind <- sample(seq_len(nrow(x)), size = smp_size)

#training x,y
train_x <- x[train_ind, ]
train_y <- y[train_ind, ]
train_xy_set <- MyData[train_ind, c(xList,yField)]
#View(training_set)

#testing x,y
#difference is -train
test_x <- x[-train_ind, ]
test_y <- y[-train_ind, ]
test_xy_set <- MyData[-train_ind, c(xList,yField)]

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
results <- ols_step_all_possible(trainingModel, p=.05)

#break immediately
#ols_step_backward_p(trainingModel, p=.1)

#colnames(merge(train_y,train_x))
