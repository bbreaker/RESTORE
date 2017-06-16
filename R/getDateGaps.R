### After you run the function below on a vector of site IDs, use this to get sites that have > 3600 dvs
### 

getDateGaps <- function(site, startDt, endDt) {
  
  # Get the DVs or return comment if failure occurs
  newDvs <- tryCatch({
    
    readNWISdv(site, parameterCd = "00060", statCd = "00003", startDate = startDt, endDate = endDt)
    
  },
  
  error = function(cond) {
    
    data.frame(site_no = site, comment = "NWIS retrieval failure")
    
  })
  
  if(nrow(newDvs) == 1) {
    
    newDF <- data.frame(site_no = site, comment = "insufficient data", stringsAsFactors = FALSE)
    
    return(newDF)
    
  }
  
  else if(nrow(newDvs) > 1) {
    
    # Rename flows in the data frame
    colnames(newDvs)[4] <- "Flow"
    
    # get the site file
    siteInfo <- readNWISsite(site)
    
    if(is.na(siteInfo$drain_area_va)) {
      
      newDF <- data.frame(site_no = as.character(site), comment = "has no DA in site file", 
                          stringsAsFactors = FALSE)
      
      return(newDF)
      
    }
    
    # create a new data frame with a 'Date' column that starts and end at the min and max 'Date'
    newDate <- data.frame(Date = seq(min(newDvs$Date), max(newDvs$Date), by = "1 days"))
    
    # merge the data frames
    newDvs <- merge(x = newDate, y = newDvs, by = "Date", all.x = TRUE)
    
    # create a new column that has 0's for NA values and 1's for any other value
    refDvs <- mutate(newDvs, chunk = if_else(is.na(Flow), 0, 1))
    
    # create vectors to reference where the data is
    reflengths <- rle(refDvs$chunk)$lengths; refVals <- rle(refDvs$chunk)$values
    
    # put those vectors back in a data frame
    refDF <- data.frame(reflengths, refVals)
    
    # get the cumulative sums of the reflenghts
    refDF$totals <- cumsum(refDF[,1])
    
    # sites with no missing DVs
    if(min(refVals) == 1) {
      
      newDF <- data.frame(site_no = as.character(site), startDate = newDvs[1,1], endDate = newDvs[nrow(newDvs),1],
                          comment = "1", stringsAsFactors = FALSE)
      
      return(newDF)
      
    }
    
    else if(min(refVals) == 0) {
      
      newDF <- data.frame(site_no = as.character(site), startDate = newDvs[1,1], endDate = newDvs[refDF[1,3],1],
                          comment = as.character(refDF[1,2]), stringsAsFactors = FALSE)
      
      for (i in seq(2, nrow(refDF), 1)) {
        
        newerDF <- data.frame(site_no = as.character(site), startDate = newDvs[refDF[(i-1),3] + 1,1], 
                              endDate = newDvs[refDF[i,3],1],comment = as.character(refDF[i,2]), 
                              stringsAsFactors = FALSE)
        
        newDF <- bind_rows(newDF, newerDF); rm(newerDF)
        
      }
      
      return(newDF)
      
    }
    
  }
  
}
