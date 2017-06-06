rmse <- function(obs, pred){
  #calculate root-mean-squared-error
  sqr.error <- (obs - pred)^2
  mse <- mean(sqr.error)
  rmse <- sqrt(mse)
  return(rmse)
}

rmse.by.year <- function(df){
  
  for(y in unique(df[['year']])){
    df.y <- df[which(df[['year']] == y), ]
    rmse.y <- rmse(df.y[['obs']], df.y[['pred']])
    rmse.all <- c(rmse.all, rmse.y)
    names(rmse.all)[length(rmse.all)] <- y
  }
  
  return(rmse.all)
}
