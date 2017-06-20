library(changepoint)
library(caret)
library(phyclust)
library(rPython)
options(warn = -1)

source("utilities.R")

#' calculate F1 score based on ground truth
CalculateF1 <- function(dataset, points, truth) {
  algo.predictions <- numeric(length(dataset))
  ground.truth <- numeric(length(dataset))
  
  for (i in truth) {
    ground.truth[i] <- 1
  }
  
  for (i in points) {
    algo.predictions[i] <- 1
  }
  
  result <- confusionMatrix(
    data = algo.predictions,
    reference = ground.truth,
    mode = "prec_recall"
  )
  
  print(result$byClass)
}

#' Calculate Rand & Adjusted Rand
CalculateRand <- function(input, changepoints, truthpoints) {
  clustering <- AnnotateClusters(input, changepoints)
  truth <- AnnotateClusters(input, truthpoints)
  
  clustering <- clustering + 1
  truth <- truth + 1
  
  print(RRand(truth, clustering))
}

#' Call python script to calculate BCubed values
CalculateBCubed <- function(input) {
  print(system(paste("python RunBCubed.py", input), intern = T))
}

#' main experiment method
RunExperiment <- function(input, daily = TRUE) {
  penalty.function = "Hannan-Quinn"
  min.seglength = 0
  
  dataset <- ProcessData(input, daily)
  groundtruth <- GetGroundTruth(input)
  
  #get indices for ground truth
  library(foreach)
  groundtruth.indexed <-
    foreach(i = groundtruth) %do% which(dataset$Date == i)
  
  write.csv(AnnotateClusters(dataset$Freq, groundtruth.indexed),
            "GroundTruthClusters.csv")
  
  # set up the plot area
  par(mfrow = c(3, 3))
  
  # run the experiments!
  
  # Mean PELT
  mean.pelt <- cpt.mean(
    dataset$Freq,
    method = "PELT",
    penalty = penalty.function,
    minseglen = min.seglength
  )
  
  print('/// Mean PELT ///')
  
  plot(mean.pelt, main = "Mean w/PELT", ylab = "Postings")
  PlotGroundTruth(groundtruth.indexed)
  CalculateF1(mean.pelt@data.set, mean.pelt@cpts, groundtruth.indexed)
  CalculateRand(mean.pelt@data.set, mean.pelt@cpts, groundtruth.indexed)
  write.csv(AnnotateClusters(mean.pelt@data.set, mean.pelt@cpts),
            "MeanPeltClusters.csv")
  CalculateBCubed("MeanPeltClusters.csv")
  
  # Mean SegNeigh
  mean.segneigh <- cpt.mean(
    dataset$Freq,
    method = "SegNeigh",
    penalty = penalty.function,
    minseglen = min.seglength
  )
  
  print('/// Mean SegNeigh ///')
  
  plot(mean.segneigh, main = "Mean w/SegNeigh", ylab = "Postings")
  PlotGroundTruth(groundtruth.indexed)
  CalculateF1(mean.segneigh@data.set,
              mean.segneigh@cpts,
              groundtruth.indexed)
  CalculateRand(mean.segneigh@data.set,
                mean.segneigh@cpts,
                groundtruth.indexed)
  write.csv(
    AnnotateClusters(mean.segneigh@data.set, mean.segneigh@cpts),
    "MeanSegNeighClusters.csv"
  )
  CalculateBCubed("MeanSegNeighClusters.csv")
  
  # Mean BinSeg
  mean.binseg <- cpt.mean(
    dataset$Freq,
    method = "BinSeg",
    penalty = penalty.function,
    minseglen = min.seglength
  )
  
  print('/// Mean BinSeg ///')
  
  plot(mean.binseg, main = "Mean w/BinSeg", ylab = "Postings")
  PlotGroundTruth(groundtruth.indexed)
  CalculateF1(mean.binseg@data.set,
              mean.binseg@cpts,
              groundtruth.indexed)
  CalculateRand(mean.binseg@data.set,
                mean.binseg@cpts,
                groundtruth.indexed)
  write.csv(
    AnnotateClusters(mean.binseg@data.set, mean.binseg@cpts),
    "MeanBinSegClusters.csv"
  )
  CalculateBCubed("MeanBinSegClusters.csv")
  
  # Var PELT
  var.pelt <- cpt.var(
    dataset$Freq,
    method = "PELT",
    penalty = penalty.function,
    minseglen = min.seglength
  )
  
  print('/// Var PELT ///')
  
  plot(var.pelt, main = "Variance w/PELT", ylab = "Postings")
  PlotGroundTruth(groundtruth.indexed)
  CalculateF1(var.pelt@data.set, var.pelt@cpts, groundtruth.indexed)
  CalculateRand(var.pelt@data.set, var.pelt@cpts, groundtruth.indexed)
  write.csv(AnnotateClusters(var.pelt@data.set, var.pelt@cpts),
            "VarPeltClusters.csv")
  CalculateBCubed("VarPeltClusters.csv")
  
  # Var SegNeigh
  var.segneigh <- cpt.var(
    dataset$Freq,
    method = "SegNeigh",
    penalty = penalty.function,
    minseglen = min.seglength
  )
  
  print('/// Var SegNeigh ///')
  
  plot(var.segneigh, main = "Variance w/SegNeigh", ylab = "Postings")
  PlotGroundTruth(groundtruth.indexed)
  CalculateF1(var.segneigh@data.set,
              var.segneigh@cpts,
              groundtruth.indexed)
  CalculateRand(var.segneigh@data.set,
                var.segneigh@cpts,
                groundtruth.indexed)
  write.csv(
    AnnotateClusters(var.segneigh@data.set, var.segneigh@cpts),
    "VarSegNeighClusters.csv"
  )
  CalculateBCubed("VarSegNeighClusters.csv")
  
  # Var BinSeg
  var.binseg <- cpt.var(
    dataset$Freq,
    method = "BinSeg",
    penalty = penalty.function,
    minseglen = min.seglength
  )
  
  print('/// Var BinSeg ///')
  
  plot(var.binseg, main = "Variance w/BinSeg", ylab = "Postings")
  PlotGroundTruth(groundtruth.indexed)
  CalculateF1(var.binseg@data.set, var.binseg@cpts, groundtruth.indexed)
  CalculateRand(var.binseg@data.set, var.binseg@cpts, groundtruth.indexed)
  write.csv(
    AnnotateClusters(var.binseg@data.set, var.binseg@cpts),
    "VarBinSegClusters.csv"
  )
  CalculateBCubed("VarBinSegClusters.csv")
  
  # MeanVar PELT
  meanvar.pelt <- cpt.meanvar(
    dataset$Freq,
    method = "PELT",
    test.stat = "Poisson",
    penalty = penalty.function,
    minseglen = min.seglength
  )
  
  print('/// MeanVar PELT ///')
  
  plot(meanvar.pelt, main = "Mean & Variance w/PELT", ylab = "Postings")
  PlotGroundTruth(groundtruth.indexed)
  CalculateF1(meanvar.pelt@data.set,
              meanvar.pelt@cpts,
              groundtruth.indexed)
  CalculateRand(meanvar.pelt@data.set,
                meanvar.pelt@cpts,
                groundtruth.indexed)
  write.csv(
    AnnotateClusters(meanvar.pelt@data.set, meanvar.pelt@cpts),
    "MeanVarPeltClusters.csv"
  )
  CalculateBCubed("MeanVarPeltClusters.csv")
  
  # MeanVar SegNeigh
  meanvar.segneigh <- cpt.meanvar(
    dataset$Freq,
    method = "SegNeigh",
    penalty = penalty.function,
    minseglen = min.seglength
  )
  
  print('/// MeanVar SegNeigh ///')
  
  plot(meanvar.segneigh, main = "Mean & Variance w/SegNeigh", ylab = "Postings")
  PlotGroundTruth(groundtruth.indexed)
  CalculateF1(meanvar.segneigh@data.set,
              meanvar.segneigh@cpts,
              groundtruth.indexed)
  CalculateRand(meanvar.segneigh@data.set,
                meanvar.segneigh@cpts,
                groundtruth.indexed)
  write.csv(
    AnnotateClusters(meanvar.segneigh@data.set, meanvar.segneigh@cpts),
    "MeanVarSegNeighClusters.csv"
  )
  CalculateBCubed("MeanVarSegNeighClusters.csv")
  
  # MeanVar BinSeg
  meanvar.binseg <- cpt.meanvar(
    dataset$Freq,
    method = "BinSeg",
    test.stat = "Poisson",
    penalty = penalty.function,
    minseglen = min.seglength
  )
  
  print('/// MeanVar BinSeg ///')
  
  plot(meanvar.binseg, main = "Mean & Variance w/BinSeg", ylab = "Postings")
  PlotGroundTruth(groundtruth.indexed)
  CalculateF1(meanvar.binseg@data.set,
              meanvar.binseg@cpts,
              groundtruth.indexed)
  CalculateRand(meanvar.binseg@data.set,
                meanvar.binseg@cpts,
                groundtruth.indexed)
  write.csv(
    AnnotateClusters(meanvar.binseg@data.set, meanvar.binseg@cpts),
    "MeanVarBinSegClusters.csv"
  )
  CalculateBCubed("MeanVarBinSegClusters.csv")
}