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
  test <- fred$series.observations(series_id = parsedList[a])

  test %>>%
    select(
      date,
      value
    ) %>>%
    mutate(
      date = as.Date(date),
      value = as.numeric(value)
    ) ->
    dt

  require(ggplot2)
  #print(dt)
  write.csv(dt, file = parsedList[a])
  #qplot(data = dt, x = date, y = value, geom = 'line')

  a=a+1
}
