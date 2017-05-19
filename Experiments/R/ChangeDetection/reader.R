library(readr)
library(changepoint)
library(scales)
library(ggthemes)

# read in the CSV
data <- read_csv("~/shortexport.csv")
# order the dataset by postingdate
order(data$postingdate)
# calculate factor, indexing on posting date 
sums <- as.data.frame(table(factor(format(data$postingdate, "%Y-%m-%d"))))
colnames(sums) <- c("Date", "Freq")
# detect changes, store results
changes <- cpt.mean(sums[["Freq"]], method="BinSeg", penalty = "MBIC" , class = TRUE, minseglen = 2)
changepoints <- as.vector(changes@cpts)

# plot dataset as line graph, draw vlines at change points
plot <- ggplot(sums, aes(as.numeric(Date), Freq, group=1)) + geom_line() + xlab("Date") + ylab("Total Tweets") + geom_vline(xintercept=changepoints, linetype="dashed", colour="red")
print(plot)