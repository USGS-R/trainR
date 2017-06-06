#' @title Censored Data
#' @description Flags data censored.
#' @param  vals vector amount of bad censored
#' @examples
#' 
#' data <- c(1,2,3,4,5,16,34)
#' censored <- pct_censored(data)
#' @export
#' 
#' 
pct_censored <- function(vals, digits=2){
  cen.i <- which_censored(vals)
  cen.pct <- calc_pct(ninterest = length(cen.i),
                      ntotal = length(vals),
                      digits = digits)
  return(cen.pct)
}

#' @title Missing Data
#' @description  Flags missing data
#' @param vals  vector amount of missing data
#' @examples
#' 
#' data <- c(1,2,3,4,5,NA)
#' missing <- pct_missing(data)
#' @export
#'  
pct_missing <- function(vals, digits=2){
  na.i <- which(is.na(vals))
  na.pct <- calc_pct(ninterest = length(na.i),
                     ntotal = length(vals),
                     digits = digits)
  return(na.pct)
}
#' @title Percent of missing or bad data
#' @param ninterest vector amount of bad data
#' @param ntotal  vector total amount of data
#' @examples 
#' 
#' data <- c(1,2,3,4,5)
#' data2 <- c(2,3,4,5,6)
#' percent <- calc_pct(data,data2)
#' 
#' @export
calc_pct <- function(ninterest, ntotal, digits=2){
  pct <- ninterest/ntotal*100
  pct.rnd <- round(pct)
  return(pct.rnd)
}
