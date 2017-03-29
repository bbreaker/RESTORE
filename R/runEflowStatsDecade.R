runEflowStatsDecde <- function(site, startDate, endDate) {
  
  # Get the DVs or return comment if failure occurs
  newDvs <- tryCatch({
    
    readNWISdv(site, parameterCd = "00060", statCd = "00003")
    
  },
  
  error = function(cond) {
    
    message(paste("NWIS retrieval failure for", site))
    
    data.frame(site_no = as.character(site), comment = "NWIS retrieval failure",
               stringsAsFactors = FALSE)
    
  })
  
  # return info about site and failure
  if (nrow(newDvs) == 1) {
    
    EflowDat <- newDvs
    
    return(EflowDat)
    
  }
  
  # work with the Dvs
  else if(nrow(newDvs) != 1) {
    
    # Rename flows in the data frame
    colnames(newDvs)[4] <- "Flow"
    
    # get the site file
    siteInfo <- readNWISsite(site)
    
    # sites that have excessive amounts of 0 flows crash EflowStats
    if(is.na(siteInfo$drain_area_va)) {
      
      EflowDat <- data.frame(site_no = as.character(site), 
                             comment = "has no DA in site file", stringsAsFactors = FALSE)
      
      return(EflowDat)
      
    }
    
    # sites that don't have excessive 0 flows and do have published drainage areas
    else if(!is.na(siteInfo$drain_area_va)) {
      
      # add a decade column to the data frame
      newDvs <- newDvs %>% 
        mutate(yr = as.numeric(format(Date, "%Y")), mn = as.numeric(format(Date, "%m"))) %>% 
        mutate(waterYr = yr + if_else(mn < 10, 0, 1)) %>% 
        mutate(decade = (waterYr %/% 10) * 10)
      
      # create a vector of unique decades in the data frame
      decs <- unique(newDvs$decade)
      
      if(length(decs) == 1) {
        
        # subset the DVs by the first decade
        subDvs <- filter(newDvs, decade == decs[1])
        
        # format data for EflowStats
        flowForstats <- data.frame(datetime = subDvs$Date, discharge = subDvs$Flow)
        
        EflowDat <- tryCatch({
          
          ObservedStatsOther(daily_data = flowForstats, drain_area = siteInfo$drain_area_va, 
                             site_id = newDvs[1,2], stats = stats)
          
        },
        
        error = function(cond){
          
          data.frame(site_no = as.character(site), comment = "EflowStats failure",
                     stringsAsFactors = FALSE)
        })
        
        EflowDat <- EflowDat[!is.na(names(EflowDat))]
        
        return(EflowDat)
        
      }
      
      else if(length(decs) > 1) {
        
        # subset the DVs by the first decade
        subDvs <- filter(newDvs, decade == decs[1])
        
        # format data for EflowStats
        flowForstats <- data.frame(datetime = subDvs$Date, discharge = subDvs$Flow)
        
        EflowDat <- tryCatch({
          
          ObservedStatsOther(daily_data = flowForstats, drain_area = siteInfo$drain_area_va, 
                             site_id = newDvs[1,2], stats = stats)
          
        },
        
        error = function(cond){
          
          data.frame(site_no = as.character(site), comment = "EflowStats failure",
                     stringsAsFactors = FALSE)
        })
        
        EflowDat <- EflowDat[!is.na(names(EflowDat))]
        
        for(i in seq(2, length(decs), 1)) {
          
          # subset the DVs by the first decade
          subDvs <- filter(newDvs, decade == decs[i])
          
          # format data for EflowStats
          flowForstats <- data.frame(datetime = subDvs$Date, discharge = subDvs$Flow)
          
          EflowDatNext <- tryCatch({
            
            ObservedStatsOther(daily_data = flowForstats, drain_area = siteInfo$drain_area_va, 
                               site_id = newDvs[1,2], stats = stats)
            
          },
          
          error = function(cond){
            
            data.frame(site_no = as.character(site), comment = "EflowStats failure",
                       stringsAsFactors = FALSE)
          })
          
          EflowDatNext <- EflowDatNext[!is.na(names(EflowDatNext))]
          
          EflowDat <- dplyr::bind_rows(EflowDat, EflowDatNext); rm(EflowDatNext)
          
        }
        
        return(EflowDat)
        
      }
      
    }
    
  }
  
}
