# function that takes site IDs and returns a data frame,
# with information regarding regulated periods and unregulated periods
# as determined from codes in the peak-flow files

getRegInfo <- function(siteIDs) {
  
  require(dplyr, quietly = TRUE)
  require(lubridate, quietly = TRUE)
  require(dataRetrieval, quietly = TRUE)
  
  # create a blank data frame that contains all of the information needed
  regList <- data.frame(siteNo = as.character(), beginRegDate = as.character(), 
                        endRegDate = as.character(), beginDatePk = as.character(), 
                        endDatePk = as.character(), beginDtDv = as.character(), 
                        endDateDv = as.character(), lat = as.numeric(),
                        long = as.numeric(), beginDtDvNew = as.character(),
                        endDateDvNew = as.character(), stringsAsFactors = F)
  
  # create a dummy peak flow data frame
  pkDummy <- readNWISpeak("07048600", convertType = FALSE)
  pkDummy <- pkDummy[0,]
  
  # create a new vector from the siteIDs
  sitesX <- siteIDs
  
  # start a loop to add info to the data frame
  for (i in seq(1, length(sitesX), 1)) {
    
    # read in the peak flow file
    pkFile <- readNWISpeak(sitesX[i], convertType = FALSE)
    
    # change any -00 days to -01 days on the historic peaks in the file
    pkFile$peak_dt <- dplyr::if_else(stringr::str_sub(pkFile$peak_dt, start = 9, end = 10) == "00", 
                                     paste0(stringr::str_sub(pkFile$peak_dt, 1, 8), "01"), pkFile$peak_dt)
    
    # if there is no peak flow data, use the dummy
    if (nrow(pkFile) == 0) {
      
      pkFile <- pkDummy
      
    }
    
    # if there is peak flow data, use it
    else if (nrow(pkFile) > 0) {
      
      pkFile <- pkFile
      
    }
    
    # make sure there are discharge values in the peak flow file;
    # some only contain peak stages
    pkTest <- pkFile[!is.na(pkFile$peak_va),]
    
    # get information about the availability of daily discharge data
    datFile <- whatNWISdata(siteNumber = siteIDs[i], service = "dv", parameterCd = "00060", statCd = "00003")
    
    # get rid of rows in the peak flow file with no peaks
    pkFile <- pkFile[!is.na(pkFile$peak_va),]
    
    # keep rows in the peak flow file that contain codes indicating 
    # regulation, channelization, or diversion
    pkFileReg <- pkFile[grepl("5|6|C", pkFile$peak_cd),]
    
    # keep rows in the peak flow file that contain codes indicating 
    # regulation, channelization, or diversion
    pkFileUnreg <- pkFile[!grepl("5|6|C", pkFile$peak_cd),]
    
    # for sites that don't have any peak flows in the peak flow file
    if (nrow(pkTest) == 0) {
      
      # add that information to the data frame
      regListNew <- data.frame(siteNo = sitesX[i], beginDtDv = as.character(datFile[1,22]), 
                               endDateDv = as.character(datFile[1,23]),
                               lat = as.numeric(datFile[1,5]), long = as.numeric(datFile[1,6]),
                               stringsAsFactors = F)
      
      # add the rows to the new data frame
      regList <- bind_rows(regList, regListNew); rm(regListNew)
      
    }
    
    # for sites that have all "altered" flows
    else if (nrow(pkFileReg) == 0) {
      
      # add that information to the data frame
      regListNew <- data.frame(siteNo = sitesX[i], beginDatePk = as.character(pkFile[1,3]), 
                               endDatePk = as.character(pkFile[nrow(pkFile),3]), 
                               beginDtDv = as.character(datFile[1,22]), 
                               endDateDv = as.character(datFile[1,23]), 
                               lat = as.numeric(datFile[1,5]), long = as.numeric(datFile[1,6]), 
                               stringsAsFactors = F)
      
      # add the rows to the new data frame
      regList <- bind_rows(regList, regListNew); rm(regListNew)
      
    }
    
    # for sites that have some or no "altered" flows
    else if (nrow(pkFileReg) > 1) {
      
      # add that information to the data frame
      regListNew <- data.frame(siteNo = sitesX[i], beginRegDate = as.character(pkFileReg[1,3]), 
                               endRegDate = as.character(pkFileReg[nrow(pkFileReg),3,]), 
                               beginDatePk = as.character(pkFile[1,3]), 
                               endDatePk = as.character(pkFile[nrow(pkFile),3]),
                               beginDtDv = as.character(datFile[1,22]), 
                               endDateDv = as.character(datFile[1,23]),
                               lat = as.numeric(datFile[1,5]), long = as.numeric(datFile[1,6]),
                               stringsAsFactors = F)
      
      # add the rows to the new data frame
      regList <- bind_rows(regList, regListNew); rm(regListNew)
      
    }
    
  }
  
  # make the dates Date objects
  regList[,2:7] <- lapply(regList[,2:7], as.Date)
  
  # create a new column of dates to start to think about a new start date
  # to use for this site... will require to evaluate
  regList$beginDtDvNew <- if_else(format(regList$beginDtDv, "%d") == "01", regList$beginDtDv, 
                                  firstDayNextMonth(regList$beginDtDv))
  
  # create a new column of dates to start to think about a new end date
  # to use for this site... will require to evaluate
  regList$endDateDvNew <- if_else(format(regList$endDateDv, "%d") != 
                                    days_in_month(as.numeric(format(regList$endDateDv, "%m"))), 
                                  lastDayPrevMonth(regList$endDateDv), regList$endDateDv)
  
  # make the dates Date objects
  regList[,10:11] <- lapply(regList[,10:11], as.Date)
  
  # create a new column of dates to start to think about a new start date
  # to use for this site if the peak flow file indicates 'altered' and 'unaltered' periods
  regList$beginDtDvNew <- if_else(is.na(regList$beginRegDate), regList$beginDtDv, 
                                  firstDayNextMonth(regList$beginDtDv))
  
  # create a new column of dates to start to think about a new end date
  # to use for this site if the peak flow file indicates 'altered' and 'unaltered' periods
  regList$endDateDvNew <- if_else(is.na(regList$beginRegDate), regList$endDateDv, 
                                  lastDayPrevMonth(regList$beginRegDate))
  
  # return the final data frame
  return(regList)
  
}
