library(readr)
library(changepoint)

data <- read_csv("~/shortexport.csv")
x <- table(factor(format(data$postingdate, "%M")))
changes <- cpt.mean(as.vector(x), method="BinSeg", test.stat = "CUSUM", penalty = "None", class = TRUE)
plot(changes)
lines(x, type="l")
abline(v=changes@cpts, col="blue")
