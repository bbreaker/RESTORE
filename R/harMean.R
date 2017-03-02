harmMean <- function(x, na.rm = FALSE) {
  
  if(na.rm)
    
    x <- x[!is.na(x)]
  
  if(length(x) == 0L)
    
    stop("there is no data")
  
  if(any(is.na(x)))
    
    stop("there are NA values")
  
  n <- length(x)
  
  x <- x[x != 0]
  
  nNot0 <- length(x)
  
  if(nNot0 == 0L)
    
    return(0)
  
  val <- signif((nNot0/n * nNot0/sum(1/x)), 3)
  
  return(val)
  
}
