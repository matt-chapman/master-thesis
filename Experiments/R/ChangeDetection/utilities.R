library(readr)
library(changepoint)

#' Generates gaussian noise & poisson process signal, approx 10 jumps
#' 
#' could do multiple jumps or fixed number of jumps, take stat. significance
GenerateNormalSignal <- function() {
  # set.seed(10)
  # number of changepoints
  N <- rpois(1,10)
  # true changepoints
  true.cpt <- sample(1000,N)
  
  # generate the signal
  m1 <- matrix(rep(1:1000,N),1000,N,byrow=FALSE)
  m2 <- matrix(rep(true.cpt,1000),1000,N,byrow=TRUE)
  x <- as.data.frame(rnorm(1000) + apply(m1>=m2,1,sum))
  colnames(x) <- "Freq"
  return(list(x, true.cpt))
}

#' Split a vector at given indices
SplitVector <- function(data, splitpoints) {
  unname(split(data, cumsum(seq_along(data) %in% splitpoints)))
}

AnnotateClusters <- function(data, splitpoints) {
  cumsum(seq_along(data) %in% splitpoints)
}

#' Read in csv files and format properly
ProcessData <- function(query, daily = TRUE) {
  if (query == "ing") {
    input <- "~/Repos/master-thesis/Data/ing.csv"
  } else if (query == "rabobank") {
    input <- "~/Repos/master-thesis/Data/rabobank.csv"
  } else if (query == "reddit") {
    input <- "~/Repos/master-thesis/Data/reddit.csv"
  }
  
  # Switch depending on export type
  if (daily) {
    dataset <- read_delim(
      file = input,
      ",",
      escape_double = FALSE,
      col_types = cols(Date = col_date(format = "%d-%m-%Y")),
      trim_ws = TRUE
    )
    colnames(dataset) <- c("Date", "Freq")
    
    return(dataset)
    
  } else {
    data.preprocess <- read_csv(input)
    # order the dataset by postingdate
    order(data.preprocess$postingdate)
    # calculate factor, indexing on posting date
    dataset <-
      as.data.frame(table(factor(
        format(data.preprocess$postingdate, "%Y-%m-%d")
      )))
    colnames(dataset) <- c("Date", "Freq")
    return(dataset)
  }
}

#' read in CSV file containing ground truth points
GetGroundTruth <- function(query) {
  if (query == "ing") {
    input <- "~/Repos/master-thesis/Data/ing_points.csv"
  } else if (query == "rabobank") {
    input <- "~/Repos/master-thesis/Data/rabobank_points.csv"
  } else if (query == "reddit") {
    input <- "~/Repos/master-thesis/Data/reddit_points.csv"
  }
  
  groundtruth <-
    read_csv(
      input,
      col_names = FALSE,
      col_types = cols(X1 = col_date(format = "%Y-%m-%d"))
    )
  return(groundtruth$X1)
}

#' helper for plotting ground truth points
PlotGroundTruth <- function(groundtruth) {
  for (point in groundtruth) {
    abline(v = point, col = "blue", lty = 2)
  }
}

PlotChangePoints <- function(points) {
  for (point in points) {
    abline(v = point, col = "red")
  }
}

#' utility for running CROPS algorithm
GetPenalties <- function(input, daily = TRUE) {
  dataset <- ProcessData(input, daily)
  
  mean.pelt.crops <- cpt.mean(
    dataset$Freq,
    method = "PELT",
    penalty = "CROPS",
    test.stat = "Normal",
    pen.value = c(log(length(dataset$Freq)), 100 *
                    log(length(dataset$Freq))),
    minseglen = 5,
    class = TRUE
  )
  
  var.pelt.crops <- cpt.var(
    dataset$Freq,
    method = "PELT",
    penalty = "CROPS",
    test.stat = "Normal",
    pen.value = c(log(length(dataset$Freq)), 100 *
                    log(length(dataset$Freq))),
    minseglen = 5,
    class = TRUE
  )
  
  meanvar.pelt.crops <- cpt.meanvar(
    dataset$Freq,
    method = "PELT",
    penalty = "CROPS",
    test.stat = "Poisson",
    pen.value = c(log(length(dataset$Freq)), 100 *
                    log(length(dataset$Freq))),
    minseglen = 5,
    class = TRUE
  )
  
  plot(mean.pelt.crops, diagnostic = TRUE, main = "Mean PELT")
  plot(var.pelt.crops, diagnostic = TRUE, main = "Variance PELT")
  plot(meanvar.pelt.crops, diagnostic = TRUE, main = "Mean & Variance PELT")
}