sevQtwo <- function(date, flow, yearType = NULL) {
  
  if(is.null(yearType)) {
    
    stop("must enter watYear, calYear, or climYear")
    
  }
  
  else if(length(date) != length(flow)) {
    
    stop("length of dates and flows does not match")
    
  }
  
  newDF <- data.frame(date = date, flow = flow)
  
  if(yearType == "calYear") {
    
    newDFAve <- newDF %>%
      mutate(yr = format(date,'%Y'),
             mn = months(date, abbreviate=T),
             dvMove = moveAve(flow, 7)) %>%
      na.omit() %>%
      group_by(yr) %>%
      dplyr::filter(n() > 328) %>% 
      summarize(yearMin = min(dvMove)) %>%
      ungroup() %>%
      data.frame()
    
  }
  
  else if(yearType == "watYear") {
    
    newDFAve <- newDF %>%
      mutate(yr = format(date,'%Y'),
             mn = format(date, "%m"),
             dvMove = moveAve(flow, 7)) %>%
      mutate(watYr = as.character(as.numeric(yr) + if_else(as.numeric(mn) < 10, 0, 1))) %>%
      na.omit() %>%
      group_by(watYr) %>%
      dplyr::filter(n() > 328) %>% 
      summarize(yearMin = min(dvMove)) %>%
      ungroup() %>%
      data.frame()
    
  }
  
  else if(yearType == "climYear") {
    
    newDFAve <- newDF %>%
      mutate(yr = format(date, "%Y"),
             mn = format(date, "%m"),
             dvMove = moveAve(flow, 7)) %>%
      mutate(climYr = as.character(as.numeric(yr) + if_else(as.numeric(mn) < 4, 0, 1))) %>%
      na.omit() %>%
      group_by(climYr) %>%
      dplyr::filter(n() > 328) %>% 
      summarize(yearMin = min(dvMove)) %>%
      ungroup() %>%
      data.frame()
    
  }
  
  if(any(newDFAve$yearMin == 0)) {
    
    val <- 0
    
  }
  
  else if(any(newDFAve$yearMin != 0)) {
    
    compLmoms <- tryCatch({
      
      lmomco::lmoms(log10(newDFAve$yearMin))
      
    },
    
    error = function(cond) {
      
      NA
      
    })
    
    distPar <- tryCatch({
      
      lmom2par(compLmoms, type = "pe3")
      
    },
    
    error = function(cond) {
      
      NA
      
    })
    
    val <- tryCatch({
      
      signif(10^(par2qua(f = 0.5, para = distPar)), 3)
      
    },
    
    error = function(cond) {
      
      NA
      
    })

  }
  
  return(val)
  
}
