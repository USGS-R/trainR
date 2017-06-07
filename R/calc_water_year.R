#' @title calc_water_year
#' @description Calculate the water year given a string of dates
#' @param date.vec Vector with a string of dates
#' @details Based on the USGS definition, a water year starts on October 1 of the year before, 
#' and ends on September 30. For example, water year 2015 started on 2014-10-01 and ended on 2015-09-30. 
#' See the USGS definition at https://water.usgs.gov/nwc/explain_data.html.
#' @return Vector of the water year in which the given date occurs
#' @importFrom graphics axis plot
#' @importFrom stats median
#' @export
#' @examples 
#' mydates <- seq(as.POSIXct("2010-01-01"), as.POSIXct("2011-01-01"), by="day")
#' waterYear <- calc_water_year(date.vec = mydates)

calc_water_year <- function(date.vec){
  if(!is.vector(date.vec)) {
    stop("date.vec must be a vector for this cool computation to continue")
  }
  # POSIXlt years start at 100, POSIXlt months start at 0
  date.time.vec <- as.POSIXlt(date.vec)
  cal.year <- date.time.vec[['year']] + 1900
  cal.mon <- date.time.vec[['mon']] + 1
  
  # address NA dates and made a new comment
  # when the date is NA, it should not try to add 1
  which.past.oct <- cal.mon >= 10 & !is.na(cal.mon)
  
  # add one to the year if it is in October or after
  # October is end of water year
  water.year <- cal.year
  water.year[which.past.oct] <- cal.year[which.past.oct] + 1
  
  return(water.year)
}

