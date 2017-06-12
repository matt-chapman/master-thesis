library(changepoint)
library(ROCR)
options( warn = -1 )

#' calculate F1 score based on ground truth
CalculateF1 <- function(dataset, points, truth) {
  algo.predictions <- numeric(length(dataset))
  ground.truth <- numeric(length(dataset))
  
  for(i in truth) {
    ground.truth[i] <- 1
  }
  
  for(i in points) {
    algo.predictions[i] <- 1
  }
  
  pred.obj <- prediction(algo.predictions, ground.truth)
  perf <- performance(pred.obj, "f")
  print(perf@y.values)
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
  CalculateF1(mean.pelt@data.set, mean.pelt@cpts, groundtruth.indexed)
  print(AnnotateClusters(mean.pelt@data.set, mean.pelt@cpts))
  
  # Mean SegNeigh
  mean.segneigh <- cpt.mean(dataset$Freq,
                            method = "SegNeigh",
                            penalty = "Hannan-Quinn",
                            Q = 5)
  
  plot(mean.segneigh, main = "Mean w/SegNeigh", ylab = "Postings")
  PlotGroundTruth(groundtruth.indexed)
  CalculateF1(mean.segneigh@data.set, mean.segneigh@cpts, groundtruth.indexed)
  
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
  CalculateF1(mean.binseg@data.set, mean.binseg@cpts, groundtruth.indexed)
  
  # Var PELT
  var.pelt <- cpt.var(
    dataset$Freq,
    method = "PELT",
    penalty = "Hannan-Quinn",
    minseglen = 5
  )
  
  plot(var.pelt, main = "Variance w/PELT", ylab = "Postings")
  PlotGroundTruth(groundtruth.indexed)
  CalculateF1(var.pelt@data.set, var.pelt@cpts, groundtruth.indexed)
  
  # Var SegNeigh
  var.segneigh <- cpt.var(dataset$Freq,
                          method = "SegNeigh",
                          penalty = "Hannan-Quinn",
                          Q = 5)
  
  plot(var.segneigh, main = "Variance w/SegNeigh", ylab = "Postings")
  PlotGroundTruth(groundtruth.indexed)
  CalculateF1(var.segneigh@data.set, var.segneigh@cpts, groundtruth.indexed)
  
  # Var BinSeg
  var.binseg <- cpt.var(dataset$Freq,
                        method = "BinSeg",
                        #test.stat = "CSS",
                        penalty = "Hannan-Quinn",
                        Q = 5)
  
  plot(var.binseg, main = "Variance w/BinSeg", ylab = "Postings")
  PlotGroundTruth(groundtruth.indexed)
  CalculateF1(var.binseg@data.set, var.binseg@cpts, groundtruth.indexed)
  
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
  CalculateF1(meanvar.pelt@data.set, meanvar.pelt@cpts, groundtruth.indexed)
  
  # MeanVar SegNeigh
  meanvar.segneigh <- cpt.meanvar(dataset$Freq,
                                  method = "SegNeigh",
                                  penalty = "Hannan-Quinn",
                                  Q = 5)
  
  plot(meanvar.segneigh, main = "Mean & Variance w/SegNeigh", ylab = "Postings")
  PlotGroundTruth(groundtruth.indexed)
  CalculateF1(meanvar.segneigh@data.set, meanvar.segneigh@cpts, groundtruth.indexed)
  
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
  CalculateF1(meanvar.binseg@data.set, meanvar.binseg@cpts, groundtruth.indexed)
  
}