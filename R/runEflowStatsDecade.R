runEflowStatsDecade <- function(site, startDt, endDt) {
  
  startDtNew <- as.POSIXlt(startDt)
  
  startYr <- as.integer(format(startDtNew, "%Y"))
  
  startMn <- as.integer(format(startDtNew, "%m"))
  
  startDy <- as.integer(format(startDtNew, "%d"))
  
  if(startMn == 10 & startDy == 1L) {
    
    startYrNew <- startYr
    
  }
  
  else if(startMn < 10 ) {
    
    startYrNew <- startYr
    
  }
  
  else if(startMn > 10) {
    
    startYrNew <- startYr + 1L
    
  }
  
  else if(startMn == 10 & startDy != 1L) {
    
    startYrNew <- startYr + 1L
    
  }
  
  startDtNew <- paste0(startYrNew, "-10-01")
  
  endDtNew <- as.POSIXlt(endDt)
  
  endYr <- as.integer(format(endDtNew, "%Y"))
  
  endMn <- as.integer(format(endDtNew, "%m"))
  
  endDt <- as.integer(format(endDtNew, "%d"))
  
  endDy <- as.integer(format(endDtNew, "%d"))
  
  if(endMn == 9L & endDy == 30L) {
    
    endYrNew <- endYr 
    
  } 
  
  else if(endMn > 9L) {
    
    endYrNew <- endYr
    
  }
  
  else if(endMn < 9L) {
    
    endYrNew <- endYr - 1L
    
  } 
  
  else if(endMn == 9L & endDy != 30L) {
    
    endYrNew <- endYr - 1L
    
  }
  
  endDtNew <- paste0(endYrNew, "-09-30")
  
  newDvs <- tryCatch({
    
    readNWISdv(site, parameterCd = "00060", statCd = "00003", startDate = startDtNew, endDate = endDtNew)
    
  },
  
  error = function(cond) {
    
    message(paste("NWIS retrieval failure for", site))
    
    data.frame(site_no = as.character(site), comment = "NWIS retrieval failure",
               stringsAsFactors = FALSE)
    
  })
  
  # return info about site and failure
  if (nrow(newDvs) == 1) {
    
    EflowDat <- data.frame(site_no = site, comment = "no flow data")
    
    return(EflowDat)
    
  }
  
  # work with the Dvs
  else if(nrow(newDvs) != 1) {
    
    # get the site file
    siteInfo <- readNWISsite(site)
    
    if(is.na(siteInfo$drain_area_va)) {
      
      EflowDat <- data.frame(site_no = as.character(site),
                             comment = "has no DA in site file", stringsAsFactors = FALSE)
      
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
      
      pkFile <- dplyr::filter(pkFile, !is.na(peak_dt))
      
      if(nrow(pkFile) < 2) {
        
        EflowDat <- data.frame(site_no = as.character(site),
                               comment = "less than two peak flows", stringsAsFactors = FALSE)
        
        return(EflowDat)
        
      }
      
      else if(nrow(pkFile) >= 2) {
        
        newDvs <- newDvs %>%
          mutate(yr = as.numeric(format(Date, "%Y")),
                 mn = as.numeric(format(Date, "%m"))) %>%
          mutate(waterYr = yr + if_else(mn < 10, 0, 1)) %>%
          mutate(decade = (waterYr %/% 10) * 10)
        
        decs <- unique(newDvs$decade)
        
        if(length(decs) == 1) {
          
          dailyQClean <- validate_data(newDvs[c("Date", "Flow")], yearType = "water")
        
          drainageArea <- siteInfo$drain_area_va
        
          floodThresh <- get_peakThreshold(dailyQClean[c("date","discharge")], pkFile[c("peak_dt","peak_va")])
        
          calc_allHITOut <- tryCatch({
          
            calc_allHIT(dailyQClean, drainArea = drainageArea, floodThreshold = floodThresh)
          
          },
        
          error = function(cond) {
          
            message(paste("all HIT error for", site))
          
            data.frame(comment = "EflowStats calc_allHIT error")
          
          })
        
          magnifStatsOut <- tryCatch({
            
            calc_magnifSeven(dailyQClean, yearType = "water", digits = 3)
            
          },
          
          error = function(cond) {
            
            data.frame(comment = "EflowStats error", stringsAsFactors = FALSE)
            
          })
        
          info <- data.frame(site_no = site, start_date = startDtNew, end_date = endDtNew)
          
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
        
        else if(length(decs) > 1) {
          
          subDvs <- filter(newDvs, decade == decs[1])
          
          dailyQClean <- validate_data(subDvs[c("Date", "Flow")], yearType = "water")
          
          drainageArea <- siteInfo$drain_area_va
          
          floodThresh <- get_peakThreshold(dailyQClean[c("date","discharge")], pkFile[c("peak_dt","peak_va")])
          
          calc_allHITOut <- tryCatch({
            
            calc_allHIT(dailyQClean, drainArea = drainageArea, floodThreshold = floodThresh)
            
          },
          
          error = function(cond) {
            
            message(paste("all HIT error for", site))
            
            data.frame(comment = "EflowStats calc_allHIT error")
            
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
          
          for(i in seq(2, length(decs), 1)) {
            
            subDvs <- filter(newDvs, decade == decs[i])
          
            dailyQClean <- validate_data(subDvs[c("Date", "Flow")], yearType = "water")
            
            drainageArea <- siteInfo$drain_area_va
            
            floodThresh <- get_peakThreshold(dailyQClean[c("date","discharge")], pkFile[c("peak_dt","peak_va")])
            
            calc_allHITOut <- tryCatch({
              
              calc_allHIT(dailyQClean, drainArea = drainageArea, floodThreshold = floodThresh)
              
            },
            
            error = function(cond) {
              
              message(paste("all HIT error for", site))
              
              data.frame(comment = "EflowStats calc_allHIT error")
              
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
          
            newEflowDat <- dplyr::bind_cols(info, magnifStatsOutDF, calc_allHITOutDF)
            
            newEflowDat$startDate <- startDtNew;  newEflowDat$endDate <- endDtNew
            
            EflowDat <- dplyr::bind_rows(EflowDat, newEflowDat)
            
          }
          
          return(EflowDat)
          
        }
        
      }
      
    }
    
  }
  
}
