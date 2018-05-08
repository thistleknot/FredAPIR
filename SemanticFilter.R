

devtools::install_github("jcizel/FredR")

api.key = '661c0a90e914477da5a7518293de5f8e'
fred <- FredR::FredR(api.key= '661c0a90e914477da5a7518293de5f8e')

install.packages("zoo")
install.packages("xts")
install.packages("tidyquant")

#note
#2008 05 01 most important datasets start here

start_date="2008-05-01"

library(data.table)
library(zoo)
library(xts)
library(tidyquant)


library(pipeR)
library(dplyr)

require(ggplot2)
require(gridExtra)

semanticList = c("Population", "Price", "Employment","Consumer", "500", "Monetary Base", "Real", "Money Stock", "Treasury",  "Spread")

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

#https://stackoverflow.com/questions/50118593/r-join-all-datasets-by-date/50139902#50139902
# first download all the data    
#by Gregor 2018-05-2
data_list = lapply(parsedList, function(a)
  fred$series.observations(
    series_id = a,
    observation_start = start_date,
    observation_end = "2018-03-01"
  )
)

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
for (i in parsedList)
{
  #select subset of combined data, in this case, date, and column a (starts at 2)
  df <- subset(combined_data, select = c(1, a))
  
  df2 <- df %>%
    tq_transmute(select = 2,
                 mutate_fun = apply.quarterly,
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
print(df3)

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

print(test1)                  

#wtf, had to add data.frame today!
test1_z <- zoo(data.frame(test1))

date_z <- zoo(data.frame(dates))

print(test1_z)

#colnames(test1_z)

print(dates)

test1_z_approx <- na.fill(na.approx(test1_z, dates$date, rule=2, na.rm = FALSE), "extend")

print(test1_z_approx)

#new <- NULL
#print(new)
new <- c(data.frame(dates),data.frame(test1_z_approx))
#print(new)

#setup sliding windows up to past 6 iterations

size=nrow(data.frame(new))

future <- c()

#offset 
#https://stackoverflow.com/questions/4219715/r-create-a-copy-of-a-column-where-the-new-column-is-offset-by-some-fixed-amount

#apply(lag(zoo(dta), c(-1,0), na.pad = TRUE), 1L, diff)
#https://stackoverflow.com/questions/45638529/zoo-lag-diff-back-in-data-frame/45639642?noredirect=1
past1Iteration=lag(new$SPCS20RSA,-1)

future <- c(new$SPCS20RSA)
require(zoo)
apply(lag(zoo(future), c(-1,0), na.pad = TRUE), 1L, diff)

print(past1Iteration)

futureSPC=lag(new$SPCS20RSA,1)
print(futureSPC)

#write.csv(new, file = "output_test.csv")



