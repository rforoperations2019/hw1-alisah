# Directions: 
# * Create three (3) different kinds of plots/figures 
# * Use DT to create one (1) data table
# * Include at least three (3) different types of inputs
# * One (1) functioning downloadButton() 
# * One (1) observer in the server
# * Inputs must use reactivity in a logical manner with all outputs displayed to users


# histogram of tipping by trip distance, trip duration, fare_amount (radio buttons)
# scatter plot of dates on the x, tip amount on y. color points by number of passengers (dateRangeinput?)
# box plots of number of passengers, vary plot by fare, distance, tip (selecinput)


#Data retrieved from https://catalog.data.gov/dataset/2019-green-taxi-trip-data

taxi <- readit('2019_Green_Taxi_Trip_Data.csv')

head(taxi)


