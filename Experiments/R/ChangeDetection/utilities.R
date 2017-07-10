library(readr)
library(changepoint)

PlotTruths <- function() {
  
  par(mfrow = c(4, 3))
  dirk <- ProcessData('dirk')
  dirk.truth <- GetGroundTruth('dirk')
  ziggo <- ProcessData('ziggo')
  ziggo.truth <- GetGroundTruth('ziggo')
  bol <- ProcessData('bol')
  bol.truth <- GetGroundTruth('bol')
  connexxion <- ProcessData('connexxion')
  connexxion.truth <- GetGroundTruth('connexxion')
  dap <- ProcessData('dap')
  dap.truth <- GetGroundTruth('dap')
  jumbo <- ProcessData('jumbo')
  jumbo.truth <- GetGroundTruth('jumbo')
  kvk <- ProcessData('kvk')
  kvk.truth <- GetGroundTruth('kvk')
  rabobank <- ProcessData('rabobank')
  rabobank.truth <- GetGroundTruth('rabobank')
  tele2 <- ProcessData('tele2')
  tele2.truth <- GetGroundTruth('tele2')
  uwv <- ProcessData('uwv')
  uwv.truth <- GetGroundTruth('uwv')
  
  plot(dirk, type='l', main = "Dirk", ylab = "Postings")
  PlotGroundTruth(dirk.truth)
  
  plot(ziggo, type='l', main = "Ziggo", ylab = "Postings")
  PlotGroundTruth(ziggo.truth)
  
  plot(bol, type='l', main = "Bol.com", ylab = "Postings")
  PlotGroundTruth(bol.truth)
  
  plot(connexxion, type='l', main = "Connexxion", ylab = "Postings")
  PlotGroundTruth(connexxion.truth)
  
  plot(dap, type='l', main = "Dakota Access Pipeline", ylab = "Postings")
  PlotGroundTruth(dap.truth)
  
  plot(jumbo, type='l', main = "Jumbo", ylab = "Postings")
  PlotGroundTruth(jumbo.truth)
  
  plot(kvk, type='l', main = "KvK", ylab = "Postings")
  PlotGroundTruth(kvk.truth)
  
  plot(rabobank, type='l', main = "Rabobank", ylab = "Postings")
  PlotGroundTruth(rabobank.truth)
  
  plot(tele2, type='l', main = "Tele2", ylab = "Postings")
  PlotGroundTruth(tele2.truth)
  
  plot(uwv, type='l', main = "UWV", ylab = "Postings")
  PlotGroundTruth(uwv.truth)
}

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
  if (query == "dirk") {
    input <- "~/Repos/master-thesis/Data/dirk.csv"
  } else if (query == "ziggo") {
    input <- "~/Repos/master-thesis/Data/ziggo.csv"
  } else if (query == "bol") {
    input <- "~/Repos/master-thesis/Data/bol.csv"
  } else if (query == "connexxion") {
    input <- "~/Repos/master-thesis/Data/connexxion.csv"
  } else if (query == "dap") {
    input <- "~/Repos/master-thesis/Data/dap.csv"
  } else if (query == "jumbo") {
    input <- "~/Repos/master-thesis/Data/jumbo.csv"
  } else if (query == "kvk") {
    input <- "~/Repos/master-thesis/Data/kvk.csv"
  } else if (query == "rabobank") {
    input <- "~/Repos/master-thesis/Data/rabobank.csv"
  } else if (query == "tele2") {
    input <- "~/Repos/master-thesis/Data/tele2.csv"
  } else if (query == "uwv") {
    input <- "~/Repos/master-thesis/Data/uwv.csv"
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
  if (query == "dirk") {
    input <- "~/Repos/master-thesis/Data/dirk_points.csv"
  } else if (query == "ziggo") {
    input <- "~/Repos/master-thesis/Data/ziggo_points.csv"
  } else if (query == "bol") {
    input <- "~/Repos/master-thesis/Data/bol_points.csv"
  } else if (query == "connexxion") {
    input <- "~/Repos/master-thesis/Data/connexxion_points.csv"
  } else if (query == "dap") {
    input <- "~/Repos/master-thesis/Data/dap_points.csv"
  } else if (query == "jumbo") {
    input <- "~/Repos/master-thesis/Data/jumbo_points.csv"
  } else if (query == "kvk") {
    input <- "~/Repos/master-thesis/Data/kvk_points.csv"
  } else if (query == "rabobank") {
    input <- "~/Repos/master-thesis/Data/rabobank_points.csv"
  } else if (query == "tele2") {
    input <- "~/Repos/master-thesis/Data/tele2_points.csv"
  } else if (query == "uwv") {
    input <- "~/Repos/master-thesis/Data/uwv_points.csv"
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

PrettyResultsPlot <- function(df1) {
  
  ggplot(df1, aes(x = as.factor(Algorithm), y = mean, fill=Metric)) +
    geom_bar(position=position_dodge(), stat="identity", colour='black') +
    geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,position=position_dodge(.9)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
}