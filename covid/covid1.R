
# Load the data set and tidy up
library(readr)
confirmedDF = read_csv("d:/source/repos/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_us.csv")



datesDT = as.Date(names(confirmedDF[,-(1:11)]), "%m/%d/%y")
confirmed = mapply(sum, confirmedDF[(confirmedDF$Province_State == 'Minnesota'), -(1:11)], USE.NAMES = FALSE)
names(confirmed) <- datesDT

library(ggplot2)

#deathDF = read.csv("d:/source/repos/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_us.csv")

