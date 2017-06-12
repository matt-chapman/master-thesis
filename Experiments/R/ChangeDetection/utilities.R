library(readr)
library(changepoint)

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
      "~/Repos/master-thesis/Data/rabobank_points.csv",
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