

#Data retrieved from https://catalog.data.gov/dataset/2019-green-taxi-trip-data

library(data.table)
library(readit)
library(dplyr)

taxi <- readit('2019_Green_Taxi_Trip_Data.csv')
taxi <- as.data.frame(taxi)

colnames(taxi)
taxi <- sample_n(taxi, size = 100000)
save(taxi, file = 'taxi.Rdata')


