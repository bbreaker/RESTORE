# THis is a simple function to compute the moving average 
# of the specified series of daily values (series) by the specified 
# number of days (numDays)

moveAve <- function(series, numDays) {
  
  stats::filter(series,rep(1/numDays,numDays), sides=2)
  
}
