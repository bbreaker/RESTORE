runLowFlowStats <- function(site, startDate, endDate, yearType) {
  
  dvs <- tryCatch({
    
    readNWISdv(site, parameterCd = "00060", statCd = "00003", startDate = as.character(startDate), 
               endDate = as.character(endDate))
    
  },
  
  error = function(cond){
    
    data.frame(site_no = as.character(site), comment = "NWIS retrieval failure")
    
  })
  
  if(nrow(dvs) == 1) {
    
    newDF <- dvs
    
    return(newDF)
    
  }
  
  else if(nrow(dvs) > 1){
    
    colnames(dvs)[4] <- "Flow"
    
    newDF <- data.frame(site_no = as.character(site), 
                        min_date = dvs[1,3],
                        max_date = dvs[nrow(dvs),3],
                        sevQ10 = as.numeric(sevQten(dvs$Date, dvs$Flow, yearType)), 
                        sevQ2 = as.numeric(sevQtwo(dvs$Date, dvs$Flow, yearType)), 
                        oneQ10 = as.numeric(oneQten(dvs$Date, dvs$Flow, yearType)), 
                        thirtyQ5 = as.numeric(thirtyQfive(dvs$Date, dvs$Flow, yearType)), 
                        thirtyQ2 = as.numeric(thirtyQtwo(dvs$Date, dvs$Flow, yearType)), 
                        harmMean = as.numeric(harmMean(dvs$Flow, na.rm = TRUE)),
                        comment = NA)
    
    return(newDF)
    
  }
  
}