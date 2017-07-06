# library(dataRetrieval)

isValidID <- function(siteID, ...) {
  testDF <- tryCatch({
    dataRetrieval::readNWISsite(siteNumbers = siteID)
  },
  error = function(cond){
    dataRetrieval::readNWISsite(siteNumbers = paste0("0",siteID))
  })
  testDF <- dplyr::filter(testDF, agency_cd == "USGS")
  return(testDF$site_no)
}
