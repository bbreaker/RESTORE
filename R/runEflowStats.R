runEflowStats <- function(site, startDt, endDt) {
  
  startDtNew <- as.POSIXlt(startDt)
  
  startYr <- as.integer(format(startDtNew, "%Y"))
  
  startMn <- as.integer(format(startDtNew, "%m"))
  
  startDy <- as.integer(format(startDtNew, "%d"))
  
  if(startMn <= 10) {
    
    startYrNew <- startYr
    
  } 
  
  if(startMn > 10) {
    
    startYrNew <- startYr + 1L
    
  } 
  
  if(startMn == 10 & startDy != 1L) {
    
    startYrNew <- startYr + 1L
    
  }
  
  #startYrNew <- startYr + ifelse(startMn <= 10L & startDy == 1L, 0L, 1L)
  
  startDtNew <- paste0(startYrNew, "-10-01")
  
  endDtNew <- as.POSIXlt(endDt)
  
  endYr <- as.integer(format(endDtNew, "%Y"))
  
  endMn <- as.integer(format(endDtNew, "%m"))
  
  endDy <- as.integer(format(endDtNew, "%d"))
  
  if(endMn >= 9L) {
    
    endYrNew <- endYr 
    
  } 
  
  if(endMn < 9L) {
    
    endYrNew <- endYr - 1L
    
  } 
  
  if(endMn == 9L & endDy != 30L) {
    
    endYrNew <- endYr - 1L
    
  }
  
  endDtNew <- paste0(endYrNew, "-09-30")
  
  if(as.numeric(as.POSIXlt(endDtNew) - as.POSIXlt(startDtNew)) >= 364) {
    
    newDvs <- tryCatch({
    
    dataRetrieval::readNWISdv(site, parameterCd = "00060", statCd = "00003", 
                              startDate = startDtNew, endDate = endDtNew)
    
    },
    
    error = function(cond) {
      
      data.frame(site_no = as.character(site), start_date = startDtNew,
                 end_date = endDtNew, comment = "NWIS retrieval failure",
                 stringsAsFactors = FALSE)
      
    })
    
    # return info about site and failure
    if (nrow(newDvs) == 1) {
      
      EflowDat <- data.frame(site_no = site, start_date = startDtNew, end_date = endDtNew, 
                             comment = "no flow data", stringsAsFactors = FALSE)
      
      return(EflowDat)
      
    }
    
    # work with the Dvs
    else if(nrow(newDvs) != 1) {
      
      # get the site file
      siteInfo <- readNWISsite(site)
      
      if(is.na(siteInfo$drain_area_va)) {
        
        EflowDat <- data.frame(site_no = as.character(site), start_date = startDtNew, 
                               end_date = endDtNew, comment = "has no DA in site file", 
                               stringsAsFactors = FALSE)
        
        return(EflowDat)
        
      }
      
      else if(!is.na(siteInfo$drain_area_va)) {
        
        names(newDvs)[4] <- "Flow"
        
        pkFile <- readNWISpeak(site, convertType = FALSE)
        
        pkFile$peak_dt <- dplyr::if_else(stringr::str_sub(pkFile$peak_dt, start = 9, end = 10) == "00",
                                         paste0(stringr::str_sub(pkFile$peak_dt, 1, 8), "01"), pkFile$peak_dt)
        
        pkFile$peak_dt <- dplyr::if_else(stringr::str_sub(pkFile$peak_dt, start = 6, end = 7) == "00",
                                         paste0(stringr::str_sub(pkFile$peak_dt, 1, 5), "04-01"), pkFile$peak_dt)
        
        pkFile$peak_dt <- as.Date(pkFile$peak_dt, format = "%Y-%m-%d")
        
        pkFile$peak_va <- as.numeric(pkFile$peak_va)
        
        pkFile <- pkFile[!is.na(pkFile$peak_va),]
        
        pkFile <- dplyr::filter(pkFile, !peak_va == 0)
        
        if(nrow(pkFile) < 2) {
          
          EflowDat <- data.frame(site_no = as.character(site),
                                 comment = "less than two peak flows", stringsAsFactors = FALSE)
          
          return(EflowDat)
          
        }
        
        else if(nrow(pkFile) >= 2) {
          
          dailyQClean <- validate_data(newDvs[c("Date", "Flow")], yearType = "water")
          
          drainageArea <- siteInfo$drain_area_va
          
          floodThresh <- get_peakThreshold(dailyQClean[c("date","discharge")], pkFile[c("peak_dt","peak_va")])
          
          calc_allHITOut <- tryCatch({
            
            calc_allHIT(dailyQClean, drainArea = drainageArea, floodThreshold = floodThresh)
            
          },
          
          error = function(cond) {
            
            data.frame(comment = "EflowStats error", stringsAsFactors = FALSE)
            
          })
          
          magnifStatsOut <- tryCatch({
            
            calc_magnifSeven(dailyQClean, yearType = "water", digits = 3)
            
          },
          
          error = function(cond) {
            
            data.frame(comment = "EflowStats error", stringsAsFactors = FALSE)
            
          })
          
          info <- data.frame(site_no = site, start_date = startDtNew, end_date = endDtNew,
                             stringsAsFactors = FALSE)
          
          if(nrow(calc_allHITOut) == 1 & nrow(magnifStatsOut) == 1) {
          
            EflowDat <- info
            
            EflowDat$comment <- "EflowStats failure"
            
            return(EflowDat)
            
          }
          
          else if(nrow(calc_allHITOut) == 1 & nrow(magnifStatsOut) > 1) {
            
            magnifStatsOutDF <- t(magnifStatsOut$statistic)
            
            magnifStatsOutDF <- data.frame(magnifStatsOutDF)
            
            names(magnifStatsOutDF) <- magnifStatsOut$indice
          
            calc_allHITOutDF <- calc_allHITOut
            
          }
          
          else if(nrow(calc_allHITOut) > 1 & nrow(magnifStatsOut) == 1) {
            
            magnifStatsOutDF <- magnifStatsOut
            
            calc_allHITOutDF <- t(calc_allHITOut$statistic)
            
            calc_allHITOutDF <- data.frame(calc_allHITOutDF)
            
            names(calc_allHITOutDF) <- calc_allHITOut$indice
            
          }
          
          else if(nrow(calc_allHITOut) > 1 & nrow(magnifStatsOut) > 1) {
            
            magnifStatsOutDF <- t(magnifStatsOut$statistic)
            
            magnifStatsOutDF <- data.frame(magnifStatsOutDF)
            
            names(magnifStatsOutDF) <- magnifStatsOut$indice
          
            calc_allHITOutDF <- t(calc_allHITOut$statistic)
            
            calc_allHITOutDF <- data.frame(calc_allHITOutDF)
            
            names(calc_allHITOutDF) <- calc_allHITOut$indice
            
            calc_allHITOutDF$comment = "none"
            
          }
          
          EflowDat <- dplyr::bind_cols(info, magnifStatsOutDF, calc_allHITOutDF)
  
          return(EflowDat)
          
        }
        
      }
      
    }
    
  }
  
  else if(as.numeric(as.POSIXlt(endDtNew) - as.POSIXlt(startDtNew)) < 364) {
    
    EflowDat <- data.frame(site_no = as.character(site), start_date = as.character(startDtNew), 
                           end_date = as.character(endDtNew), comment = "period less than 1 water year", 
                           stringsAsFactors = FALSE)
    
    return(EflowDat)
    
  }
  
}
