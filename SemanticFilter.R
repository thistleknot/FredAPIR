#written by Joshua Laferriere
#2018-05-10

devtools::install_github("jcizel/FredR")

#install.packages("zoo", restart=TRUE)
#install.packages("xts")
#install.packages("tidyquant")
#install.packages("gridExtra")
#install.packages("magrittr")
#install.packages("readr") # you only need to do this one time on your system

library("FredR")
library(data.table)
library(xts)
library(tidyquant)
library(zoo)
library(pipeR)
library(dplyr)
library(magrittr)
library(data.table)

#ggplots
library(ggplot2)
library(gridExtra)
#library(grid)
library(ggplot2)
library(lattice)
library(readr)
library("FactoMineR")
library("devtools")
library("factoextra")
install_github("kassambara/factoextra")
library("factoextra")
library("corrplot")
library("PerformanceAnalytics")
library("mondate")
library(anytime)
library(lubridate)

require(ggplot2)
require(gridExtra)
require(zoo)

#api.key <- read_file("apiKey.txt")
api.key = '3ee8b91b17078df0b51bea5c9cfd11c6'
fred <- FredR(api.key)

#note
#switched housing index to USSTHPI which goes back to 1980!
#test for modes, if mode = min, or max, dataset was extended. then remove column

windowSize=50
seasonalConstant=0
#grab past 30 years


todayIs <- as.Date(as.POSIXlt(as.Date(Sys.Date())))
#end_date="2018-04-30"
start_date=as.Date(mondate(as.Date(todayIs)) - 360)
end_date=todayIs

minLag=-5

#hack to reduce time
semanticList = c("Population", "Price", "Employment","Consumer", "500", "Monetary Base", "Real", "Money Stock", "Treasury",  "Spread")

#semanticList = c("Population")

semanticScore=50

semantic<-c()

parsedList<-c()

names<-c()
popularityScores<-c()

x=1
for (i in semanticList)
{
  y=1
  count=1
  
  #load up data
  semantic<-fred$series.search(semanticList[x])
  
  #loop through popularity scores
  #print(semanticList[x])
  for (y in semantic$popularity)
  {
    #if specific score is greater than 77, capture
    
    #as.numeric(as.character(semantic$popularity[count]))
    
    #https://stackoverflow.com/questions/2288485/how-to-convert-a-data-frame-column-to-numeric-type
    #if((semantic$popularity[count])>77)
    
    if((as.numeric(as.character(semantic$popularity[count])))>semanticScore)
    {
      {
        #print("above")
        #print(semantic$id[count])
        
        #print(semantic$popularity[count])
        
        names<-c(names,semantic$id[count])
        popularityScores<-c(popularityScores,semantic$popularity[count])
        
      }
      
    }
    else
    {
      
    }
    
    count=count+1
  }
  
  x=x+1
  
}

#doing gold atm
parsedList<-c(unique(names),'TTLHH','EMRATIO','GOLDAMGBD228NLBM','POPTOTUSA647NWDB','USSTHPI')
#parsedList<-c(parsedList,"BAA10Y","DCOILBRENTEU","FPCPITOTLZGUSA","IC4WSA","ICSA","MPRIME","TCU","GOLDAMGBD228NLBM")


print(parsedList)
#Sorted by importance
#parsedList<-c("SPCS20RSA", "DGS1", "PAYEMS", "T10Y3M","TEDRATE","WPU0911","T5YIE","PSAVERT","A191RL1Q225SBEA")
#date and SPC not important for y.
#correction, SPC is important. It has a large prediction of the outcome direction, but doesn't explain all the movement.
#parsedList<-c("A191RL1Q225SBEA","WPU0911","DGS1","PSAVERT","TEDRATE","T10Y3M","T5YIE","SPCS20RSA")
#ussthpi predictors
#parsedList<-c("DCOILBRENTEU","HDTGPDUSQ163N","M1V","INTDSRUSM193N","BAMLH0A3HYC","TREAST","DGS10","MEHOINUSA672N","DFII10","DCOILWTICO","VIXCLS","TCU","UNRATE","BAMLC0A0CM","STLFSI","ICSA","IC4WSA","USSTHPI")

#problems:
#start late: DCOILBRENTEU, TREAST, STLFSI,VIXCLS,DGS1MO,HDTGPDUSQ163N

#100% model, used to predict?
#parsedList<-c("DCOILBRENTEU","TREAST","INTDSRUSM193N","UNRATE","M1V","MEHOINUSA672N","STLFSI","BAMLH0A3HYC","DGS1MO","TCU","VIXCLS", "IC4WSA","USSTHPI")

#parsedList<-c("INTDSRUSM193N","UNRATE","M1V","MEHOINUSA672N","BAMLH0A3HYC","TCU", "IC4WSA","USSTHPI")

#all I need for 100% binary logistic regression success.
#parsedList<-c("INTDSRUSM193N","VIXCLS","M1V","TCU","USSTHPI")

#initial gold
#parsedList<-c("DCOILWTICO","DGS30","T5YIE","DGS5","UMCSENT","SP500","TEDRATE","DGS1MO","T10Y2Y","M1","T5YIFR","IC4WSA","ICSA","VIXCLS","BAA10Y","BASE","DGS1","A191RL1Q225SBEA","INTDSRUSM193N","TCU","CIVPART","UNRATE","GS10","USSLIND","BAMLH0A3HYC","PSAVERT","DFF","TB3MS","T10Y3M","GOLDAMGBD228NLBM","WPU0911","RECPROUSM156N","PPIACO")

#newGold
#SP500 is copyrighted and was throwing errors when included.
#parsedList<-c("A191RL1Q225SBEA","BAA10Y","BASE","DCOILBRENTEU","DFF","DGS1","FPCPITOTLZGUSA","GS10","IC4WSA","ICSA","INTDSRUSM193N","MPRIME","PSAVERT","SP500","STLFSI","TCU","TEDRATE","UMCSENT","UNRATE","USSLIND","GOLDAMGBD228NLBM")
#parsedList<-c("A191RL1Q225SBEA","BAA10Y","BASE","DCOILBRENTEU","DFF","DGS1","FPCPITOTLZGUSA","GS10","IC4WSA","ICSA","INTDSRUSM193N","MPRIME","PSAVERT","STLFSI","TCU","TEDRATE","UMCSENT","UNRATE","USSLIND","GOLDAMGBD228NLBM")

print(parsedList)

#https://stackoverflow.com/questions/50118593/r-join-all-datasets-by-date/50139902#50139902
# first download all the data    
#by Gregor 2018-05-2
#troubleshoot series with this function
data_list = lapply(parsedList, function(a)
  fred$series.observations(
    series_id = a,
    observation_start = start_date,
    observation_end = end_date
  )
)

#print(parsedList[14])

# define function to process the data
# we plan on re-naming the "value" column so each one is distinct
process_data = function(d, value_name) {
  d = d[, c("date", "value")]
  d$date = as.Date(d$date)
  d$value = as.numeric(d$value)
  names(d)[2] = value_name
  return(d)
}

#filtered<-c()
#difference = (as.Date(end_date)-as.Date(start_date))
#filterThreshold=as.integer(difference/365*12)*.95

#done incorrectly due to way yearly and monthly reporting is done.
#for (i in seq_along(data_list)) {
#  #if less than 95% of data points, let's drop it
#  if( (nrow(data.frame(data_list[i])))<=filterThreshold)
#  {
#    print(parsedList[i]) 
#    print(i)
#    filtered <- c(filter,parsedList[i])
#    
#  }
#}
#filtered

#parsedList2 <- parsedList[!parsedList %in% c(filtered)]

#re-add important ones
parsedList2<-c(unique(c(parsedList,'TTLHH','EMRATIO','GOLDAMGBD228NLBM','POPTOTUSA647NWDB','USSTHPI','MEHOINUSA672N','DEXBZUS','GFDEBTN','M2V','GDPC1')))

data_list2 = lapply(parsedList2, function(a)
  fred$series.observations(
    series_id = a,
    observation_start = start_date,
    observation_end = end_date
  )
)

# process the data again
#errorList = c()
filtered <- c()
for (i in seq_along(data_list2)) {
  #if error, skip
  #apply names
  t <- try(process_data(data_list2[[i]], value_name = parsedList2[i]))
  #get empty lists!
  if ("try-error" %in% class(t)) {
    print (i)
    filtered <- c(filtered,parsedList2[i])
    #errorList <- rbind(errorList,i)
    }
  #else {print i}
  #else data_list_processed[[i]] = process_data(data_list2[[i]], value_name = parsedList2[i])
  
}
parsedList3 <- parsedList2[!parsedList2 %in% c(filtered)]

data_list3 <- data_list2[!parsedList2 %in% c(filtered)]

data_list_processed = list()

for (i in seq_along(parsedList3)) {
  #print(i)
  print(parsedList3[i])
  
  #print(data_list2[parsedList3[i]])

  #colnames(parsedList3)
  
  data_list_processed[[i]] = process_data(data_list3[[i]], value_name = parsedList3[i])
  
}

#initial pivot
#merge data by date

#combined_data = Reduce(merge, data_list_processed)

#combined_date is daily, lots of na's, hence quaterly transform below
#creates two extra dates in front that are na
head(combined_data)
combined_data = Reduce(function(x, y) merge(x, y, all = TRUE), data_list_processed)
#colnames(combined_data)
#aggregate/reduce data from daily (due to join by date operation) to weekly
#https://stackoverflow.com/questions/10085806/extracting-specific-columns-from-a-data-frame

df3 <- c()

a=2
#okay to go from 2 due to date
#and from i in parsedList because a is what's offset from 2.
#34 has bad data

for (i in parsedList3)
{
  
  #select subset of combined data, in this case, date, and column a (starts at 2)
  df <- subset(combined_data, select = c(1, a))
  
  seasonalConstant = 4  
  df2 <- df %>%
    #select 2nd column, #1st is date
    tq_transmute(select = 2,
                 #I desire to do quarterly, as most financial numbers are reported quarterly, but this requires a lot of data
                 #standard sample size for normal analysis is 20-50, so x3 50 so 150 should be adequate.  Others would argue building based off required confidence interval would be required.  However, we're doing model building based on available data.  The next payoff ratio is much bigger data, say 10's of thousands of records.  Arguably to get a decent confidence interval requires maybe a few thousand, but in data science, tens of thousands are recommended.
                 #to save time on analysis, my sets will not be this size.  They will be 50.
                 mutate_fun = apply.quarterly,
                 #http://www.business-science.io/timeseries-analysis/2017/07/02/tidy-timeseries-analysis.html
                 na.rm = TRUE,
                 FUN = mean)
  
  print(parsedList3[i])
  
  #1st pass has date (single dataframe includes two columns)
  if(a==2)
  {
    print("a==2")
    df3 <- df2
    a=a+1  
  }
  else
    #subsequent passes include two columns across two dataframes
  {
    print("else")
    df3 <- c(df3, df2[,2])
    a=a+1
  }
  
}
#print(df3)

View(df3)
combined_data_z <- df3
#https://stackoverflow.com/a/50173660/1731972

#file begins with numeric iterations
#ncol(combined_data_z)

dates <- combined_data_z[1]

print(dates)

#important to start at 2!, otherwise na.approx will not work!

#either copy from 2: on or copy whole and drop first column (date)
#test1 <- combined_data_z[c(2:length(parsedList)+1)]

#drop date
#test1 <- combined_data_z
#test1[1] <- NULL

#print(test1)                  

#wtf, had to add data.frame today!
#test1_z <- zoo(data.frame(test1))
test1_z <- zoo(data.frame(combined_data_z))
#View(test1_z)

ncol(test1_z)
nrow(test1_z)

date_z <- zoo(data.frame(dates))

ncol(date_z)

print(dates)

#error here
table(is.na(test1_z))

#https://sebastiansauer.github.io/sum-isna/
# # na's per column
#View(test1_z)
dropColumns = sapply(test1_z, function(x) sum(is.na(x)))
View(dropColumns)

#some are quarterly and are getting truncated because of it
#if i > 80%
#percent=.8
#floor=1-percent
#data.frame(dropColumns)

#filter through by removing those where na is equal to all but 1 value
filtered <- c()
for (i in 1:nrow(data.frame(dropColumns)))
{
  #parsedList2 <- parsedList[!parsedList %in% c(filtered)]  
  
  #if(i>(floor*nrow(test1_z)))
  print(data.frame(dropColumns[i])[,1])
  if(data.frame(dropColumns[i])[,1]>=(nrow(test1_z)-1))
  {
    #print(dropColumns[i])
    
    filtered <- rbind(filtered,colnames(test1_z)[i])
  }
  #{
    #filtered <- rbind(filtered,colnames(test1_z)[i])
  #}
  
}
#View(sapply(test1_z[,2:ncol(test1_z[!parsedList3 %in% c(filtered)])], function(x) sum(is.na(x))))

#View(parsedList3)
#data.frame(filtered)
data_list4 <- c()
#data_list4 <- c("date")

data_list4 <-c("date",parsedList3[!parsedList3 %in% c(filtered)])
#commit this shit
#ncol(test1_z[,c(data_list4)])

test2_z <- test1_z[,c(data_list4)]
#View(test2_z)
ncol(test2_z)
nrow(test2_z)
#View(test2_z)
#table(is.na(test2_z))

#test2_z_approx <- na.fill(na.approx(test2_z, test2_z$date, rule=q, na.rm = FALSE), c("extend",NA))
#will not extend na's and will fill in only between seasonalconstant gap :)
test2_z_approx <- na.approx(test1_z[,2:ncol(test2_z)], method="linear",n=seasonalConstant, na.rm = FALSE)

#View(test2_z_approx)
ncol(test2_z_approx)
dropColumns = sapply(test2_z_approx, function(x) sum(is.na(x)))
View(dropColumns)

#some are quarterly and are getting truncated because of it
#if i > 80%
#percent=.8
#floor=1-percent
#data.frame(dropColumns)

filtered <- c()
#start from last column
for (i in 1:nrow(data.frame(dropColumns)))
{
  #parsedList2 <- parsedList[!parsedList %in% c(filtered)]  
  
  #if(i>(floor*nrow(test1_z)))
  print(data.frame(dropColumns[i])[,1])
  #not really a percentage, more so a minimal acceptable loss
  #if(data.frame(dropColumns[i])[,1]>=(seasonalConstant*2))
  if((data.frame(dropColumns[i])[,1])>=(seasonalConstant+2))
  {
    print("yes")
    
    #works
    filtered <- rbind (filtered,i)
    #works
    print(colnames(test2_z_approx)[i])
    #does not work
    #filtered <- rbind(filtered,colnames(test2_z)[i-1])
  }
  else
  {
    print("no")
  }

}
#do something with filtered
#this is the subset
test2_z_approxSubset <- (test2_z_approx[,-c(filtered)])
ncol(test2_z_approxSubset)

#not actually filtering columns
#test2_z <- test1_z[parsedList3 %in% c(filtered)]
#View(sapply(test2_z, function(x) sum(is.na(x))))

ncol(test2_z)
#View(test2_z)
sapply(test2_z_approxSubset, function(x) sum(is.na(x)))

#write.csv(test1_z,"test1_z.csv")
ncol(test2_z_approxSubset)
#print(test1_z_approx)

#automatically create n lags

#automatically create lags
#reset outside

new <- c()
new2 <- c()
new3 <- c()
past <- c()
past2 <- c()
past3 <- c()
#dates are off
#odd SMPOPNETMUSA

#can't truncate date here
new <- c(data.frame(test2_z$date),data.frame(test2_z_approxSubset))

#View(new)
parsedList4 <- colnames(test2_z_approxSubset)
ncol(data.frame(new))

ncol(new)
#https://stackoverflow.com/questions/28055927/how-can-i-automatically-create-n-lags-in-a-timeseries
#manually remove SP500

#fill in na?

#what is a supposed to be?

for (i in 1:(length(parsedList4)+1))
{
  #i=1
  #a=1
  #naming
  print(i)
  #print(a)
  past <- c(stats::lag(zoo(c(new[[i]])), c(-1:minLag), na.pad =TRUE))
  
  #creates list to hold names, need to set names down to minLag (-5)
  names(past) <- c( paste(names(new[i]), "-1"), paste(names(new[i]), "-2") ,paste(names(new[i]), "-3"),paste(names(new[i]), "-4"),paste(names(new[i]), "-5"))
  names(new[a])
  
  past2=data.frame(past)
  
  #join based on date
  if(i==1)
  {
    past3 <- past2
    
  }
  else
  {
    past3<-cbind(past3,past2)
  }
  
  #a=a+1
  
} 

#print(past3)

#y=SPCS20RSA
#set future here (y)
#set future to CSUSHPINSA
#use stats::lag(zoo( for some normalized version for PCA?  I assumed never need to transform y
#future <- lag(new$CSUSHPINSA, k = -2, na.pad = TRUE)
future <- shift(new$CSUSHPINSA, n=1L, fill=NA, type=c("lead"), give.names=FALSE)
#View(future)
#future <- stats::lag(zoo(c(new$GOLDAMGBD228NLBM)), c(1), na.pad = TRUE)
future2 <- data.frame(future)

#combine current set and past sets
#View(new)
new2=cbind(new,past3)

#combine current (past, and current), and future set
#include past
#new3=cbind(new2,future2)

#include no past
new3=cbind(new,future2)

new4=new3

#remove first three lines (1st 2 are na's, 3rd is removed due to bad future value extended)

x <- new3[!colnames(new3) %in% c('future')]
y <- new3[colnames(new3) %in% c('CSUSHPINSA')]

xLagged = shift(x, n=1L, fill=NA, type=c("lead"), give.names=TRUE)

#nrow(x)
#nrow(y)
#x.shift(+1)

yLagged = shift(y, n=1L, fill=NA, type=c("lead"), give.names=TRUE)

#y.shift(-1)
yFuture = shift(y, n=1L, fill=NA, type=c("lag"), give.names=TRUE)

#can't filter this out

#https://www.rdocumentation.org/packages/base/versions/3.5.1/topics/interaction

#x <- as.matrix(y)
#y <- as.matrix(df2)

#xInteraction <- as.data.frame(as.matrix(x) %*% as.matrix(xLagged))

#yInteraction = interaction(data.frame(y),data.frame(yLagged))

#nrow(data.frame(xInteraction))

View(new4)
ncol(new4)
#View(new4)
new4$CSUSHPINSA
new4$csus

# # of na's in future
naFuture = sum(is.na(new4$future))

new5=head(new4,-naFuture)

#fixed
#new3=cbind(new2,future)

#https://stackoverflow.com/questions/28523404/r-multiple-linear-regression-with-a-specific-range-of-variables
#https://stats.stackexchange.com/questions/29477/how-to-write-a-linear-model-formula-with-100-variables-in-r
#https://stackoverflow.com/questions/21148498/remove-last-n-rows-in-data-frame-with-the-arbitrary-number-of-rows

#linear model
#remove last future value

#example code
# # of elements
n<-dim(df)[1]

#example code
#nrow(df), all but last row (this is offsetting)
df<-df[1:(n-1),]
#not used
future3<-df$future

#not used
#remove future (all but last row)
#https://stackoverflow.com/questions/10162480/copy-many-columns-from-one-data-frame-to-another
data2<-df[,c(1:ncol(df)-1)]

#acquire # of rows
#n<-dim(df2)[1]

#nrow(df)

#not exactly what I want, but I see how multiple regression is implemented now.
#rollapply()

#remove the 1st few lines due to minlag (n/a's)
temp1Data<-head(tail(new3[1:ncol(new3)-1],-5),6)

#temp1Dates<-tail(new3[1:1],-5)
temp1Future<-head(tail(new3[ncol(new3):ncol(new3)],-5),6)

nrow(temp1Data)
nrow(temp1Future)

write.csv(new5, file = "output_test.csv")
write.csv(MRpredict, file ="predictions.csv")




