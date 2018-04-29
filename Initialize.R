#https://www.rstudio.com/products/rpackages/devtools/
install.packages("devtools")
install.packages("ggplot2")

#https://stackoverflow.com/questions/1249548/side-by-side-plots-with-ggplot2
install.packages("gridExtra")

#https://github.com/thistleknot/FredAPI/commit/1f777ad5b0fea562e04093d4a89213bc09b38e30

#https://github.com/jcizel/FredR
devtools::install_github("jcizel/FredR")

api.key = '661c0a90e914477da5a7518293de5f8e'

fred <- FredR::FredR(api.key= '661c0a90e914477da5a7518293de5f8e')

library(pipeR)
library(dplyr)

require(ggplot2)
require(gridExtra)

gdp.series %>>%
  select(
    id,
    title,
    observation_start,
    observation_end,
    popularity
  ) %>>%
  arrange(
    desc(as.numeric(popularity))
  )

pop <- fred$series.observations(series_id = 'POP')

gdp <- fred$series.observations(series_id = 'GDPC1')

epr <- fred$series.observations(series_id = 'EMRATIO')

gdp %>>%
  select(
    date,
    value
  ) %>>%
  mutate(
    date = as.Date(date),
    value = as.numeric(value)
  ) ->
  dtgdp

pop %>>%
  select(
    date,
    value
  ) %>>%
  mutate(
    date = as.Date(date),
    value = as.numeric(value)
  ) ->
  dtpop

epr %>>%
  select(
    date,
    value
  ) %>>%
  mutate(
    date = as.Date(date),
    value = as.numeric(value)
  ) ->
  dtepr
plot

s = c("aa", "bb", "cc", "dd", "ee")

plotgdp <- qplot(data = dtgdp, x = date, y = value, geom = 'line')
plotpop <- qplot(data = dtpop, x = date, y = value, geom = 'line')
plotepr <- qplot(data = dtepr, x = date, y = value, geom = 'line')

grid.arrange(plotgdp, plotpop, plotepr, ncol=1)

