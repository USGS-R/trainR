#' @title add_annual_peak_flow
#' 
#' @description Calculate annual peak flow
#' 
#' @param flow.ts numeric USGS flow time series
#' 
#' @return flow.ts.pk numeric USGS annual peak flow
#' 
#' @examples 
#' \dontrun{library(dataRetrieval)
#' site_id <- '05114000'
#' parameterCd <- '00060'
#' startDate <- "2014-10-01"
#' endDate <- "2014-09-30"
#' flow.ts <- readNWISuv(site_id, parameterCd, startDate, endDate)
#' peak_flow <- flow.ts()
#' }
#' 
add_annual_peak_flow <- function(flow.ts){
  flow.ts.grpd <- group_by(flow.ts, year)
  flow.ts.pk <- mutate(flow.ts.grpd, flow.pk = max(flow))
  return(flow.ts.pk)
}

#' @title add_annual_median_flow
#' 
#' @description Calculate annual median flow
#' 
#' @param flow.ts numeric USGS flow time series
#' 
#' @return flow.ts.med numeric USGS annual median flow
#' 
#' @examples 
#' 
add_annual_median_flow <- function(flow.ts){
  flow.ts.grpd <- group_by(flow.ts, year)
  flow.ts.med <- mutate(flow.ts.grpd, flow.med = median(flow))
  return(flow.ts.med)
}

#' @title add_annual_low_flow
#' 
#' @description Calculate annual low flow
#' 
#' @param flow.ts numeric USGS flow time series
#' 
#' @return flow.ts.low numeric USGS annual low flow
#' 
#' @examples 
#' 
add_annual_low_flow <- function(flow.ts){
  flow.ts.grpd <- group_by(flow.ts, year)
  flow.ts.low <- mutate(flow.ts.grpd, flow.low = min(flow))
  return(flow.ts_low)
}
