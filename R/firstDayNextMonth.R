# function to round available data to first day of the month

firstDayNextMonth <- function(x) {
  x <- as.Date(as.character(x))
  lubridate::month(x) <- lubridate::month(x) + 1
  lubridate::day(x) <- 1
  return(as.Date(x))
}
