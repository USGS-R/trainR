#' @title Censored Data
#' @description Flags data censored.
#' @param  vals numeric amount of bad censored
#' @examples
#' 
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
#' @param vals  numeric amount of missing data
#' @examples
#' censored <- pct_missing()
#'  
pct_missing <- function(vals, digits=2){
  na.i <- which(is.na(vals))
  na.pct <- calc_pct(ninterest = length(na.i),
                     ntotal = length(vals),
                     digits = digits)
  return(na.pct)
}
#' @title Percent of missing or bad data
#' @param ninterest numeric amount of bad data
#' @param ntotal  numeric total amount of data
#' @examples 
#' 
#' 
calc_pct <- function(ninterest, ntotal, digits=2){
  pct <- ninterest/ntotal*100
  pct.rnd <- round(pct)
  return(pct.rnd)
}
