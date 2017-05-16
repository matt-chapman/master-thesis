library(readr)

data <- read_csv("~/shortexport.csv")
table(factor(format(data$postingdate, "%D")))