install.packages("zoo")
install.packages("xts")
install.packages("tidyquant")

library(data.table)

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

data<-c()

a=1
for (i in parsedList)
{
  test <- fred$series.observations(series_id = parsedList[a], observation_start = "2000-01-01", observation_end = "2018-03-01")

  test %>%
    select(
      date,
      value
    ) %>%
    mutate(
      date = as.Date(date),
      value = as.numeric(value)
    ) ->
    dt

  require(ggplot2)
  #print(dt)
  #write.csv(dt, file = parsedList[a])
  qplot(data = dt, x = date, y = value, geom = 'line')

  a=a+1
}

dt3<-c()
dt4<-c()
dt5<-c()

print(length(parsedList))

#join by date
a=1
for (i in parsedList)
{

  {
    test <- fred$series.observations(series_id = parsedList[a], observation_start = "2000-01-01", observation_end = "2018-03-01")

    test %>%
      select(
        date,
        value
      ) %>%
      mutate(
        date = as.Date(date),
        value = as.numeric(value)
      ) ->
      dt[a]

    #use merge

  }
  a=a+1
  #break

}

#https://stackoverflow.com/questions/50118593/r-join-all-datasets-by-date/50139902#50139902
# first download all the data    
#by Gregor 2018-05-2
data_list = lapply(parsedList, function(a)
  fred$series.observations(
    series_id = a,
    observation_start = "2000-01-01",
    observation_end = "2018-03-01"
  )
)

#use a column join function to inject a layer of rows for names?
a=1
for (i in parsedList)
{
  print(parsedList[a])
  print(data_list[a])
  a=a+1
}

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
  data_list_processed[[i]] = process_data(data_list[[i]], value_name = parsedList[i])
  print(data_list_processed[[1]])
  #print(process_data(data_list[[i]], value_name = paste0(parsedList[i])))
        
  #combined_data = Reduce(merge, data_list_processed)
  
  #set
  #df2[df1, on = c('id','dates')]
}

# merge the data
#combined_data = Reduce(merge, data_list_processed)

combined_data = Reduce(function(x, y) merge(x, y, all = TRUE), data_list_processed)

#
library(zoo)
library(xts)
library(tidyquant)

#aggregate
#https://stackoverflow.com/questions/10085806/extracting-specific-columns-from-a-data-frame

#reduced data set to weekly
df3 <- c()
a=2
for (i in parsedList)
{
  df <- subset(combined_data, select = c(1, a))
  
  df2 <- df %>%
    tq_transmute(select = 2,
                 mutate_fun = apply.weekly,
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


sample_matrix <- zoo(combined_data[,3])
data(sample_matrix)
x <- as.xts(sample_matrix, dateFormat = "Date")
(m <- apply.monthly(x, mean, na.rm = TRUE))
print(m)

#1 = date
#2 = 1st dataset
dates <- combined_data_z[,1]; 
test1 <- combined_data_z[,2];

test1_z <- zoo(test1);
test1_z_approx <- na.fill(na.approx(test1, x=dates, rule=2), "extend")

print(test1_z_approx)

write.csv(test1_z_approx, file = "output.csv")

#m <- combined_data
#na.locf(na.approx(m))
## "first observation carry backwards" too:
#na.locf(na.locf(na.approx(m)), fromLast = TRUE)

#print(m)
#print(Cz_approx)

#print(data_list_processed)

#print(combined_data)

