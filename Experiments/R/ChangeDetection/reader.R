library(readr)
library(changepoint)
library(ggplot2)

# read in the CSV
data <- read_csv("~/shortexport.csv")
# order the dataset by postingdate
order(data$postingdate)
# calculate factor, indexing on posting date 
x <- table(factor(format(data$postingdate, "%m%d")))
# detect changes, store results
changes <- cpt.mean(as.vector(x), method="BinSeg", penalty = "MBIC" , class = TRUE, minseglen = 2)
# plot dataset as line graph, draw vlines at change points
plot(changes)
lines(x, type="l")
abline(v=changes@cpts, col="blue")