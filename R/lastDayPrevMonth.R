# function to round available data to last day 
# of the previous month
lastDayPrevMonth <- function(x) {
  x <- as.Date(as.character(x))
  dayx <- dplyr::if_else((lubridate::month(x) - 1) == 0, as.integer(31), 
                         as.integer(lubridate::days_in_month(lubridate::month(x) - 1)))
  monthx <- dplyr::if_else((lubridate::month(x) - 1) == 0, as.integer(12), 
                           as.integer(lubridate::month(x) - 1))
  yearx <- dplyr::if_else(lubridate::month(x) == 1, lubridate::year(x) - 1, 
                          as.double(lubridate::year(x)))
  x <- as.Date(paste0(yearx, "-", monthx, "-", dayx), 
               format = "%Y-%m-%d")
  return(x)
}
