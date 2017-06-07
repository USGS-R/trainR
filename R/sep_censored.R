#' @export
sep_censored <- function(vals){
  if(!is.vector(vals)) {
    stop("vals need to be a vector instead of a dataframe")
  }
  cen.i <- which_censored(vals)
  just.values <- as.numeric(gsub("<|>", "", vals))
  
  just.symbols <- rep(NA, length(just.values))
  just.symbols[cen.i] <- gsub("[0-9]", "", vals[cen.i])
  
  return(list(numeric.vals = just.values, 
              censored = just.symbols))
}

which_censored <- function(vals){
  grep("<|>", vals)
}
