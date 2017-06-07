## ---- fig.align='center'-------------------------------------------------
require(ggplot2)
require(trainR)

# create observed
obs <- 1:50

# create error and predicted
e <- rnorm(n=50,mean=0,sd=5)
pred <- obs + e

# run function
rmse(obs,pred)

# plot
qplot(obs,pred)

## ------------------------------------------------------------------------
# generate year and observed
year <- c(rep(1,25),rep(2,25),rep(3,25))
obs <- c(1:75)

# generate errors that change by year
e <- c(rnorm(n=25,mean=0,sd=2),rnorm(n=25,mean=0,sd=10),rnorm(n=25,mean=0,sd=50))
pred <- obs + e

# create data frame
df <- data.frame(year,obs,pred)

# run function
rmse.by.year(df)

