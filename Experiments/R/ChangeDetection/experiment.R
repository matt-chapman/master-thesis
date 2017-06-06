library(changepoint)

#' segment data set based on changepoints
SegmentDataset <- function(dataset, points) {
  
}

#' main experiment method
RunExperiment <- function(input, daily = TRUE) {
  dataset <- ProcessData(input, daily)
  groundtruth <- GetGroundTruth(input)
  
  #get indices for ground truth
  library(foreach)
  groundtruth.indexed <-
    foreach(i = groundtruth) %do% which(dataset$Date == i)
  
  # set up the plot area
  par(mfrow = c(3, 3))
  
  # run the experiments!
  
  # Mean PELT
  mean.pelt <- cpt.mean(
    dataset$Freq,
    method = "PELT",
    penalty = "Hannan-Quinn",
    minseglen = 5
  )
  
  plot(mean.pelt, main = "Mean w/PELT", ylab = "Postings")
  
  PlotGroundTruth(groundtruth.indexed)
  
  # Mean SegNeigh
  mean.segneigh <- cpt.mean(dataset$Freq,
                            method = "SegNeigh",
                            penalty = "Hannan-Quinn",
                            Q = 5)
  
  plot(mean.segneigh, main = "Mean w/SegNeigh", ylab = "Postings")
  PlotGroundTruth(groundtruth.indexed)
  
  
  # Mean BinSeg
  mean.binseg <- cpt.mean(
    dataset$Freq,
    method = "BinSeg",
    test.stat = "CUSUM",
    penalty = "Hannan-Quinn",
    minseglen = 5,
    Q = 5
  )
  
  plot(mean.binseg, main = "Mean w/BinSeg", ylab = "Postings")
  PlotGroundTruth(groundtruth.indexed)
  
  
  # Var PELT
  var.pelt <- cpt.var(
    dataset$Freq,
    method = "PELT",
    penalty = "Hannan-Quinn",
    minseglen = 5
  )
  
  plot(var.pelt, main = "Variance w/PELT", ylab = "Postings")
  PlotGroundTruth(groundtruth.indexed)
  
  
  # Var SegNeigh
  var.segneigh <- cpt.var(dataset$Freq,
                          method = "SegNeigh",
                          penalty = "Hannan-Quinn",
                          Q = 5)
  
  plot(var.segneigh, main = "Variance w/SegNeigh", ylab = "Postings")
  PlotGroundTruth(groundtruth.indexed)
  
  # Var BinSeg
  var.binseg <- cpt.var(dataset$Freq,
                        method = "BinSeg",
                        #test.stat = "CSS",
                        penalty = "Hannan-Quinn",
                        Q = 5)
  
  plot(var.binseg, main = "Variance w/BinSeg", ylab = "Postings")
  PlotGroundTruth(groundtruth.indexed)
  
  
  # MeanVar PELT
  meanvar.pelt <- cpt.meanvar(
    dataset$Freq,
    method = "PELT",
    test.stat = "Poisson",
    penalty = "Hannan-Quinn",
    minseglen = 5
  )
  
  plot(meanvar.pelt, main = "Mean & Variance w/PELT", ylab = "Postings")
  PlotGroundTruth(groundtruth.indexed)
  
  
  # MeanVar SegNeigh
  meanvar.segneigh <- cpt.meanvar(dataset$Freq,
                                  method = "SegNeigh",
                                  penalty = "Hannan-Quinn",
                                  Q = 5)
  
  plot(meanvar.segneigh, main = "Mean & Variance w/SegNeigh", ylab = "Postings")
  PlotGroundTruth(groundtruth.indexed)
  
  
  # MeanVar BinSeg
  meanvar.binseg <- cpt.meanvar(
    dataset$Freq,
    method = "BinSeg",
    test.stat = "Poisson",
    penalty = "Hannan-Quinn",
    Q = 5
  )
  
  plot(meanvar.binseg, main = "Mean & Variance w/BinSeg", ylab = "Postings")
  PlotGroundTruth(groundtruth.indexed)
  
}