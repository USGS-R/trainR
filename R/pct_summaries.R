#' @title Censored Data
#' @description Flags data censored.
#' @param  vals vector amount of bad censored
#' @param digits integer number of digits to print
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
                      digits=digits)
  return(cen.pct)
}

#' @title Missing Data
#' @description  Flags missing data
#' @param vals  vector amount of missing data
#' @param digits integer number of digits to print
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
#' @description Percent of missing or bad data
#' @param ninterest vector amount of bad data
#' @param ntotal  vector total amount of data
#' @param digits integer number of digits
#' @examples 
#' 
#' data <- c(1,2,3,4,5)
#' data2 <- c(2,3,4,5,6)
#' percent <- calc_pct(data,data2)
#' 
#' @export
calc_pct <- function(ninterest, ntotal, digits=2){
  if (!is.vector(ninterest,mode="numeric") && length(ninterest) == 1 ) {stop("ninterest must be numeric vectors of length one")}
  if (!is.vector(ntotal,mode="numeric") && length(ntotal) == 1 ) {stop("ntotal must be numeric vectors of length one")}
  if (!is.vector(digits,mode="numeric") && length(digits) == 1 ) {stop("digits must be numeric vectors of length one")}
  pct <- ninterest/ntotal*100
  pct.rnd <- round(pct,digits=digits)
  return(pct.rnd)
}
