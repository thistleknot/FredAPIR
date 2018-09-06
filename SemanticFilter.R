#written by Joshua Laferriere
#2018-05-10

devtools::install_github("jcizel/FredR")

#install.packages("zoo", restart=TRUE)
#install.packages("xts")
#install.packages("tidyquant")
#install.packages("gridExtra")
#install.packages("magrittr")
#install.packages("readr") # you only need to do this one time on your system

library(data.table)
library(xts)
library(tidyquant)
library(zoo)
library(pipeR)
library(dplyr)
library(magrittr)

#ggplots
library(ggplot2)
library(gridExtra)
#library(grid)
library(ggplot2)
library(lattice)
library(readr)

require(ggplot2)
require(gridExtra)
require(zoo)

api.key <- read_file("C:/Users/user/Documents/AlphaAdvantageAPI/FredAPIR/apiKey.txt")

fred <- FredR::FredR(api.key)

#note
#switched housing index to USSTHPI which goes back to 1980!
#test for modes, if mode = min, or max, dataset was extended. then remove column

windowSize=100

start_date="1991-01-01"
end_date="2018-04-30"

minLag=-5

#hack to reduce time
semanticList = c("Population", "Price", "Employment","Consumer", "500", "Monetary Base", "Real", "Money Stock", "Treasury",  "Spread")

#semanticList = c("Population")

semanticScore=77

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

parsedList<-unique(names)

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
parsedList<-c("A191RL1Q225SBEA","BAA10Y","BASE","DCOILBRENTEU","DFF","DGS1","FPCPITOTLZGUSA","GS10","IC4WSA","ICSA","INTDSRUSM193N","MPRIME","PSAVERT","STLFSI","TCU","TEDRATE","UMCSENT","UNRATE","USSLIND","GOLDAMGBD228NLBM")

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

# process the data
data_list_processed = list()
for (i in seq_along(data_list)) {
  
  #apply names
  data_list_processed[[i]] = process_data(data_list[[i]], value_name = parsedList[i])
  
}

#merge data by date

#combined_data = Reduce(merge, data_list_processed)
combined_data = Reduce(function(x, y) merge(x, y, all = TRUE), data_list_processed)

#aggregate/reduce data from daily (due to join by date operation) to weekly
#https://stackoverflow.com/questions/10085806/extracting-specific-columns-from-a-data-frame

df3 <- c()

a=2
#okay to go from 2 due ot date
#and from i in parsedList because a is what's offset from 2.
for (i in parsedList)
{
  #select subset of combined data, in this case, date, and column a (starts at 2)
  df <- subset(combined_data, select = c(1, a))
  
  df2 <- df %>%
    tq_transmute(select = 2,
                 mutate_fun = apply.monthly,
                 #http://www.business-science.io/timeseries-analysis/2017/07/02/tidy-timeseries-analysis.html
                 na.rm = TRUE,
                 FUN        = mean)
  #print(df2)
  
  #1st pass has date (single dataframe includes two columns)
  if(a==2)
  {
    df3 <- df2
    a=a+1  
  }
  else
    #subsequent passes include two columns across two dataframes
  {
    df3 <- c(df3, df2[,2])
    a=a+1
  }
  
}
#print(df3)

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
test1 <- combined_data_z
test1[1] <- NULL

#print(test1)                  

#wtf, had to add data.frame today!
test1_z <- zoo(data.frame(test1))

date_z <- zoo(data.frame(dates))

print(dates)

test1_z_approx <- na.fill(na.approx(test1_z, dates$date, rule=2, na.rm = FALSE), "extend")

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
new <- c(data.frame(dates),data.frame(test1_z_approx))
ncol(data.frame(new))

#https://stackoverflow.com/questions/28055927/how-can-i-automatically-create-n-lags-in-a-timeseries
#manually remove SP500

#fill in na?
count=length(parsedList)+1
a=1
for (i in 1:count)
{
  #i=1
  #a=1
  #naming
  print(i)
  print(a)
  past <- c(stats::lag(zoo(c(new[[a]])), c(-1:minLag), na.pad =TRUE))
  
  #need to loop down to minLag
  names(past) <- c( paste(names(new[a]), "-1"), paste(names(new[a]), "-2") ,paste(names(new[a]), "-3"),paste(names(new[a]), "-4"),paste(names(new[a]), "-5"))
  names(new[a])
  
  past2=data.frame(past)
  
  #join
  if(a==1)
  {
    past3 <- past2
    
  }
  else
  {
    past3<-cbind(past3,past2)
  }
  
  a=a+1
  
} 

#print(past3)

future <- stats::lag(zoo(c(new$GOLDAMGBD228NLBM)), c(1), na.pad = TRUE)
future2 <- data.frame(future)

new2=cbind(new,past3)

new3=cbind(new2,future2)

#https://stackoverflow.com/questions/28523404/r-multiple-linear-regression-with-a-specific-range-of-variables
#https://stats.stackexchange.com/questions/29477/how-to-write-a-linear-model-formula-with-100-variables-in-r
#https://stackoverflow.com/questions/21148498/remove-last-n-rows-in-data-frame-with-the-arbitrary-number-of-rows

#linear model
#remove last future value
n<-dim(df)[1]

#nrow(df)
df<-df[1:(n-1),]

future3<-df$future

#remove future
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

#fit <- lm(y ~ head(temp1Future$future,6), data=head(temp1Data,6), subset=1:ncol(temp1Data))

#bring in variable names

n <- names(temp1Data)
f <- as.formula(paste("temp1Future$future ~", paste(n[!n %in% "y"], collapse = " + ")))
#datafilename = " data = temp1Data"

#testd <- lm(as.formula(paste(f, ",", datafilename))

#everything
dataSet<-(tail(new3, -5))

# # of Loops
numLoops=nrow(tail(new3, -5))-windowSize

#x<-c(as.Date(min(dataSet$date)),as.Date(max(dataSet$date)))
#y<-c(x<-c(min(dataSet$future),max(dataSet$future)))


#plot(x ,y)
#plot.new()

#add elements http://www.dummies.com/programming/r/how-to-add-observations-to-a-data-frame-in-r/
#date, lower, expected, upper, Adjusted R^2, CorrectDirection?

MRpredict <- c()

for(i in 1:numLoops)
{
  #i=numLoops
  wdataSet <- NULL
  futureSet <- NULL
  presentSet <- NULL
  windowMRModel <- NULL
  #i=2
  #print(i)

  #iterate here
  
  #data model with first windowSize # of elements
  wdataSet <- new3[(i+4):(windowSize+i+4),]
  
  #last record of wdataSet
  #presentSet <- new3[(windowSize+i+4):(windowSize+i+4),]
  
  presentSet <- new3[(windowSize+i+5):(windowSize+i+5),]
  
  #new3[(i+4):(i+5),]
  
  #wdataSet <- head(tail(new3, -4-i), windowSize)
  #print(wdataSet)
  #input
  #single last record of windowSize+1
  #futureSet <- tail(head(tail(new3, -4-i), windowSize+1),1)
  
  #print(futureSet[ncol(futureSet)])
  
  #present, single last record of windowSize
  #presentSet <- tail(head(tail(new3, -4-i), windowSize),1)
  
  #past 6 quarters
  #testd <- lm(future ~ A191RL1Q225SBEA + BAA10Y + BASE + DCOILBRENTEU + DFF + DGS1 + FPCPITOTLZGUSA + GS10 + IC4WSA + ICSA + INTDSRUSM193N + MPRIME + PSAVERT + SP500 + STLFSI + TCU + TEDRATE + UMCSENT + UNRATE + USSLIND + GOLDAMGBD228NLBM + A191RL1Q225SBEA..1 + A191RL1Q225SBEA..2 + A191RL1Q225SBEA..3 + A191RL1Q225SBEA..4 + A191RL1Q225SBEA..5 + BAA10Y..1 + BAA10Y..2 + BAA10Y..3 + BAA10Y..4 + BAA10Y..5 + BASE..1 + BASE..2 + BASE..3 + BASE..4 + BASE..5 + DCOILBRENTEU..1 + DCOILBRENTEU..2 + DCOILBRENTEU..3 + DCOILBRENTEU..4 + DCOILBRENTEU..5 + DCOILBRENTEU..1 + DCOILBRENTEU..2 + DCOILBRENTEU..3 + DCOILBRENTEU..4 + DCOILBRENTEU..5 + DFF..1 + DFF..2 + DFF..3 + DFF..4 + DFF..5 + DGS1..1 + DGS1..2 + DGS1..3 + DGS1..4 + DGS1..5 + FPCPITOTLZGUSA..1 + FPCPITOTLZGUSA..2 + FPCPITOTLZGUSA..3 + FPCPITOTLZGUSA..4 + FPCPITOTLZGUSA..5 + GS10..1 + GS10..2 + GS10..3 + GS10..4 + GS10..5 + IC4WSA..1 + IC4WSA..2 + IC4WSA..3 + IC4WSA..4 + IC4WSA..5 + ICSA..1 + ICSA..2 + ICSA..3 + ICSA..4 + ICSA..5 + INTDSRUSM193N..1 + INTDSRUSM193N..2 + INTDSRUSM193N..3 + INTDSRUSM193N..4 + INTDSRUSM193N..5 + MPRIME..1 + MPRIME..2 + MPRIME..3 + MPRIME..4 + MPRIME..5 + PSAVERT..1 + PSAVERT..2 + PSAVERT..3 + PSAVERT..4 + PSAVERT..5 + SP500..1 + SP500..2 + SP500..3 + SP500..4 + SP500..5 + STLFSI..1 + STLFSI..2 + STLFSI..3 + STLFSI..4 + STLFSI..5 + TCU..1 + TCU..2 + TCU..3 + TCU..4 + TCU..5 + TEDRATE..1 + TEDRATE..2 + TEDRATE..3 + TEDRATE..4 + TEDRATE..5 + UMCSENT..1 + UMCSENT..2 + UMCSENT..3 + UMCSENT..4 + UMCSENT..5 + UNRATE..1 + UNRATE..2 + UNRATE..3 + UNRATE..4 + UNRATE..5 + USSLIND..1 + USSLIND..2 + USSLIND..3 + USSLIND..4 + USSLIND..5 + GOLDAMGBD228NLBM..1 + GOLDAMGBD228NLBM..2 + GOLDAMGBD228NLBM..3 + GOLDAMGBD228NLBM..4 + GOLDAMGBD228NLBM..5,  data = head(tail(new3, -5), 150))
  
  #this is where I need to do correlation analysis using each value against each value and find those with the highest match with objective, yet lowest correlation between each other.
  
  #past 4 quarters to reduce # of columns < rows
  #sp500 is copyrighted
  windowMRModel <- lm(future ~ A191RL1Q225SBEA + BAA10Y + BASE + DCOILBRENTEU + DFF + DGS1 + FPCPITOTLZGUSA + GS10 + IC4WSA + ICSA + INTDSRUSM193N + MPRIME + PSAVERT + STLFSI + TCU + TEDRATE + UMCSENT + UNRATE + USSLIND + GOLDAMGBD228NLBM + A191RL1Q225SBEA..1 + A191RL1Q225SBEA..2 + A191RL1Q225SBEA..3 + BAA10Y..1 + BAA10Y..2 + BAA10Y..3 + BASE..1 + BASE..2 + BASE..3 + DCOILBRENTEU..1 + DCOILBRENTEU..2 + DCOILBRENTEU..3 + DCOILBRENTEU..1 + DCOILBRENTEU..2 + DCOILBRENTEU..3 + DFF..1 + DFF..2 + DFF..3 + DGS1..1 + DGS1..2 + DGS1..3 + FPCPITOTLZGUSA..1 + FPCPITOTLZGUSA..2 + FPCPITOTLZGUSA..3 + GS10..1 + GS10..2 + GS10..3 + IC4WSA..1 + IC4WSA..2 + IC4WSA..3 + ICSA..1 + ICSA..2 + ICSA..3 + INTDSRUSM193N..1 + INTDSRUSM193N..2 + INTDSRUSM193N..3 + MPRIME..1 + MPRIME..2 + MPRIME..3 + PSAVERT..1 + PSAVERT..2 + PSAVERT..3 + STLFSI..1 + STLFSI..2 + STLFSI..3 + TCU..1 + TCU..2 + TCU..3 + TEDRATE..1 + TEDRATE..2 + TEDRATE..3 + UMCSENT..1 + UMCSENT..2 + UMCSENT..3 + UNRATE..1 + UNRATE..2 + UNRATE..3 + USSLIND..1 + USSLIND..2 + USSLIND..3 + GOLDAMGBD228NLBM..1 + GOLDAMGBD228NLBM..2 + GOLDAMGBD228NLBM..3, data = wdataSet)

  #https://stackoverflow.com/questions/31824863/how-to-simply-multiply-two-columns-of-a-dataframe
  #https://stackoverflow.com/questions/33122515/applying-if-statement-to-entire-column-in-r

  #[U]p, [D]own, [S]ame Training Data
  wdataSet$LBUModel <- ifelse(with(wdataSet,(future - GOLDAMGBD228NLBM)) > 0, 1, 0)
  wdataSet$LBDModel <- ifelse(with(wdataSet,(future - GOLDAMGBD228NLBM)) < 0, 1, 0)
  wdataSet$LBSModel <- ifelse(with(wdataSet,(future - GOLDAMGBD228NLBM)) == 0, 1, 0)
  
  #Binary Logistic, Model on Training Data
  windowLBUModel <- glm( LBUModel ~ A191RL1Q225SBEA + BAA10Y + BASE + DCOILBRENTEU + DFF + DGS1 + FPCPITOTLZGUSA + GS10 + IC4WSA + ICSA + INTDSRUSM193N + MPRIME + PSAVERT + STLFSI + TCU + TEDRATE + UMCSENT + UNRATE + USSLIND + GOLDAMGBD228NLBM + A191RL1Q225SBEA..1 + A191RL1Q225SBEA..2 + A191RL1Q225SBEA..3 + BAA10Y..1 + BAA10Y..2 + BAA10Y..3 + BASE..1 + BASE..2 + BASE..3 + DCOILBRENTEU..1 + DCOILBRENTEU..2 + DCOILBRENTEU..3 + DCOILBRENTEU..1 + DCOILBRENTEU..2 + DCOILBRENTEU..3 + DFF..1 + DFF..2 + DFF..3 + DGS1..1 + DGS1..2 + DGS1..3 + FPCPITOTLZGUSA..1 + FPCPITOTLZGUSA..2 + FPCPITOTLZGUSA..3 + GS10..1 + GS10..2 + GS10..3 + IC4WSA..1 + IC4WSA..2 + IC4WSA..3 + ICSA..1 + ICSA..2 + ICSA..3 + INTDSRUSM193N..1 + INTDSRUSM193N..2 + INTDSRUSM193N..3 + MPRIME..1 + MPRIME..2 + MPRIME..3 + PSAVERT..1 + PSAVERT..2 + PSAVERT..3 + STLFSI..1 + STLFSI..2 + STLFSI..3 + TCU..1 + TCU..2 + TCU..3 + TEDRATE..1 + TEDRATE..2 + TEDRATE..3 + UMCSENT..1 + UMCSENT..2 + UMCSENT..3 + UNRATE..1 + UNRATE..2 + UNRATE..3 + USSLIND..1 + USSLIND..2 + USSLIND..3 + GOLDAMGBD228NLBM..1 + GOLDAMGBD228NLBM..2 + GOLDAMGBD228NLBM..3, family=binomial(link='logit'), data = wdataSet)
  windowLBDModel <- glm( LBDModel ~ A191RL1Q225SBEA + BAA10Y + BASE + DCOILBRENTEU + DFF + DGS1 + FPCPITOTLZGUSA + GS10 + IC4WSA + ICSA + INTDSRUSM193N + MPRIME + PSAVERT + STLFSI + TCU + TEDRATE + UMCSENT + UNRATE + USSLIND + GOLDAMGBD228NLBM + A191RL1Q225SBEA..1 + A191RL1Q225SBEA..2 + A191RL1Q225SBEA..3 + BAA10Y..1 + BAA10Y..2 + BAA10Y..3 + BASE..1 + BASE..2 + BASE..3 + DCOILBRENTEU..1 + DCOILBRENTEU..2 + DCOILBRENTEU..3 + DCOILBRENTEU..1 + DCOILBRENTEU..2 + DCOILBRENTEU..3 + DFF..1 + DFF..2 + DFF..3 + DGS1..1 + DGS1..2 + DGS1..3 + FPCPITOTLZGUSA..1 + FPCPITOTLZGUSA..2 + FPCPITOTLZGUSA..3 + GS10..1 + GS10..2 + GS10..3 + IC4WSA..1 + IC4WSA..2 + IC4WSA..3 + ICSA..1 + ICSA..2 + ICSA..3 + INTDSRUSM193N..1 + INTDSRUSM193N..2 + INTDSRUSM193N..3 + MPRIME..1 + MPRIME..2 + MPRIME..3 + PSAVERT..1 + PSAVERT..2 + PSAVERT..3 + STLFSI..1 + STLFSI..2 + STLFSI..3 + TCU..1 + TCU..2 + TCU..3 + TEDRATE..1 + TEDRATE..2 + TEDRATE..3 + UMCSENT..1 + UMCSENT..2 + UMCSENT..3 + UNRATE..1 + UNRATE..2 + UNRATE..3 + USSLIND..1 + USSLIND..2 + USSLIND..3 + GOLDAMGBD228NLBM..1 + GOLDAMGBD228NLBM..2 + GOLDAMGBD228NLBM..3, family=binomial(link='logit'), data = wdataSet)
  windowLBSModel <- glm( LBSModel ~ A191RL1Q225SBEA + BAA10Y + BASE + DCOILBRENTEU + DFF + DGS1 + FPCPITOTLZGUSA + GS10 + IC4WSA + ICSA + INTDSRUSM193N + MPRIME + PSAVERT + STLFSI + TCU + TEDRATE + UMCSENT + UNRATE + USSLIND + GOLDAMGBD228NLBM + A191RL1Q225SBEA..1 + A191RL1Q225SBEA..2 + A191RL1Q225SBEA..3 + BAA10Y..1 + BAA10Y..2 + BAA10Y..3 + BASE..1 + BASE..2 + BASE..3 + DCOILBRENTEU..1 + DCOILBRENTEU..2 + DCOILBRENTEU..3 + DCOILBRENTEU..1 + DCOILBRENTEU..2 + DCOILBRENTEU..3 + DFF..1 + DFF..2 + DFF..3 + DGS1..1 + DGS1..2 + DGS1..3 + FPCPITOTLZGUSA..1 + FPCPITOTLZGUSA..2 + FPCPITOTLZGUSA..3 + GS10..1 + GS10..2 + GS10..3 + IC4WSA..1 + IC4WSA..2 + IC4WSA..3 + ICSA..1 + ICSA..2 + ICSA..3 + INTDSRUSM193N..1 + INTDSRUSM193N..2 + INTDSRUSM193N..3 + MPRIME..1 + MPRIME..2 + MPRIME..3 + PSAVERT..1 + PSAVERT..2 + PSAVERT..3 + STLFSI..1 + STLFSI..2 + STLFSI..3 + TCU..1 + TCU..2 + TCU..3 + TEDRATE..1 + TEDRATE..2 + TEDRATE..3 + UMCSENT..1 + UMCSENT..2 + UMCSENT..3 + UNRATE..1 + UNRATE..2 + UNRATE..3 + USSLIND..1 + USSLIND..2 + USSLIND..3 + GOLDAMGBD228NLBM..1 + GOLDAMGBD228NLBM..2 + GOLDAMGBD228NLBM..3, family=binomial(link='logit'), data = wdataSet)
  #summary(windowLBIModel)
  
  #summary(windowModel)$adj.r.squared

  MRpredict <- rbind(MRpredict, c("date" = as.Date(presentSet$date),"present" = presentSet$GOLDAMGBD228NLBM,"future" = presentSet$future, "UpBL"=predict(windowLBUModel,presentSet,type="response"),"DownBL"=predict(windowLBDModel,presentSet,type="response"),"SameBL"=predict(windowLBSModel,presentSet,type="response"),data.frame(predict(windowMRModel,presentSet,interval="predict",level=.95))))
  
  #https://www.tatvic.com/blog/logistic-regression-with-r/
  #predict(Model_1,in_frame, type="response")
  #predict(windowLBUModel,presentSet,type="response")
  
  
  #windowMRModel$fitted.values
  
  
}

#p2 <- plot(testd,2)

#colnames(MRpredict)<-c("dates", "fit", "lwr", "upr")
#plot futures
#plot(GOLDAMGBD228NLBM ~ date, data=new3)
plot(NULL)
plot(future ~ date, data=tail(new3,numLoops))
lines(future ~ date, data=tail(new3,numLoops))

#going to have to supply dates based on old records using the loop numLoops
#points(fit ~ print.futureSet.date., data=data.frame(MRpredict), col=254)

#-1 pushes the records forward by 1 date to map the future expected value to the actual value (along with the prediction intervals!)
#plot(lwr ~ tail(new3$date,numLoops-1), data=data.frame(tail(MRpredict,numLoops-1)), col=253)
lines(lwr ~ tail(new3$date,numLoops), data=data.frame(tail(MRpredict,numLoops)), col=253)

#forecasted
#plot(fit ~ tail(new3$date,numLoops-1), data=data.frame(tail(MRpredict,numLoops-1)), col=254)
lines(fit ~ tail(new3$date,(numLoops)), data=data.frame(tail(MRpredict,numLoops)), col=254)

#plot(fit ~ tail(new3$date,numLoops-1), data=data.frame(tail(MRpredict,numLoops-1)), col=254)
lines(upr ~ tail(new3$date,numLoops), data=data.frame(tail(MRpredict,numLoops)), col=252)

tail(new3$future[],1)
tail(MRpredict[],1)
nrow(MRpredict)

#as.Date(13909, origin = "1961-03-04")

#some reason, first value and last value are not ordered correctly.  Recommend only focussing on dates when exporting.

#fit predicted model

#plot two graphs per model
#https://stackoverflow.com/questions/2564258/plot-two-graphs-in-same-plot-in-r
#lines(windowModel$fitted.values ~ date, data=dataSet)
#points(futureSet$date, predict(windowModel,futureSet), col=254)

#next is to build flags for binary logistic regression

#[last] value of new3 to be predicted
print("Current Date of Value")
print(data.frame(tail(new3,1)$date))
print("Present Value")
print(data.frame(tail(new3,1)$GOLDAMGBD228NLBM))
print("Next Month's value")

#predict(windowMRModel,data.frame(tail(new3,1)),interval="predict",level=.90)
#predict(windowMRModel,data.frame(tail(new3,1)),interval="predict",level=.95)
#spredict(windowMRModel,data.frame(tail(new3,1)),interval="predict",level=.99)


write.csv(new3, file = "output_test.csv")
write.csv(MRpredict, file ="predictions.csv")




