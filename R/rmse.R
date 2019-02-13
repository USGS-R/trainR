#' @title Root mean squared error (rmse)
#' @description calculates the rmse between predicted and observed values.
#' @param obs observed values
#' @param pred predicted values
#' @return returns a single rmse value
#' @examples 
#' obs <- 1:50
#' e <- rnorm(n=50,mean=0,sd=5)
#' pred <- obs + e
#' rmse(obs,pred)
#' plot(obs,pred)
#' @export

rmse <- function(obs, pred){
  #calculate root-mean-squared-error
  sqr.error <- (obs - pred)^2
  mn.sqr.error <- mean(sqr.error,na.rm=TRUE)
  rmse <- sqrt(mn.sqr.error)
  return(rmse)
}


#' @title Root mean squared error (rmse) by year
#' @description calculates the rmse between predicted and observed values for each year in a dataset.
#' @param df data frame with columns named "year", "obs", and "pred".
#' @return returns a rmse value for each year 
#' @examples 
#' year <- c(rep(1,25),rep(2,25),rep(3,25))
#' obs <- c(1:75)
#' e <- c(rnorm(n=25,mean=0,sd=2),rnorm(n=25,mean=0,sd=10),rnorm(n=25,mean=0,sd=50))
#' pred <- obs + e
#' df <- data.frame(year,obs,pred)
#' rmse.by.year(df)
#' @export

rmse.by.year <- function(df){
  rmse.all <- NULL
  for(y in unique(df[['year']])){
    df.y <- df[which(df[['year']] == y), ]
    rmse.y <- rmse(df.y[['obs']], df.y[['pred']])
    rmse.all <- c(rmse.all, rmse.y)
    names(rmse.all)[length(rmse.all)] <- y
  }
  
  return(rmse.all)
}
