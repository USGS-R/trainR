## ---- fig.show='hold'----------------------------------------------------
library(trainR)

data <- c(1,2,3,4,5,6,NA,NA)
percent_missing <- pct_missing(data)
print(percent_missing)

plot(1:10)
plot(10:1)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(mtcars, 10))

