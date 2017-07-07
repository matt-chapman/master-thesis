library(changepoint)
library(caret)
library(phyclust)
library(rPython)
options(warn = -1)
options(digits=12)

source("utilities.R")

#' main experiment method
RunExperiment <- function(input, truth = NULL, no.process = FALSE) {
  penalty.function = "SIC"
  min.seglength = 0
  
  if(no.process == FALSE) {
    dataset <- ProcessData(input)
    groundtruth <- GetGroundTruth(input)
    #get indices for ground truth
    library(foreach)
    groundtruth.indexed <-
      foreach(i = groundtruth) %do% which(dataset$Date == i)
    
    write.csv(AnnotateClusters(dataset$Freq, groundtruth.indexed),
              "GroundTruthClusters.csv")
  } else {
    dataset <- input
    groundtruth.indexed <- truth 
    write.csv(AnnotateClusters(dataset$Freq, groundtruth.indexed), "GroundTruthClusters.csv")
  }
  
  full.results = data.frame(
    'Algorithm' = character(0),
    'Precision' = numeric(0),
    'Recall' = numeric(0),
    'F1' = numeric(0),
    'Accuracy' = numeric(0),
    'Rand' = numeric(0),
    'Adj Rand' = numeric(0),
    'BCubed Precision' = numeric(0),
    'BCubed Recall' = numeric(0),
    'BCubed FScore' = numeric(0)
    )
  

  
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
  
  algo <- 'Mean PELT'
  plot(mean.pelt, main = "Mean w/PELT", ylab = "Postings")
  PlotGroundTruth(groundtruth.indexed)
  f1 <- CalculateF1(mean.pelt@data.set, mean.pelt@cpts, groundtruth.indexed)
  rand <- CalculateRand(mean.pelt@data.set, mean.pelt@cpts, groundtruth.indexed)
  write.csv(AnnotateClusters(mean.pelt@data.set, mean.pelt@cpts),
            "MeanPeltClusters.csv")
  bcubed <- CalculateBCubed("MeanPeltClusters.csv")
  
  frame <- data.frame(
    'Algorithm' = algo,
    'Precision' = f1['Precision'],
    'Recall' = f1['Recall'],
    'F1' = f1['F1'],
    'Accuracy' = f1['Accuracy'],
    'Rand' = rand['Rand'],
    'Adj Rand' = rand['adjRand'],
    'BCubed Precision' = as.numeric(bcubed[1]),
    'BCubed Recall' = as.numeric(bcubed[2]),
    'BCubed FScore' = as.numeric(bcubed[3])
  )
  full.results <- rbind(frame, full.results)
  
  # Mean SegNeigh
  mean.segneigh <- cpt.mean(
    dataset$Freq,
    method = "SegNeigh",
    penalty = penalty.function
  )
  
  algo <- 'Mean SegNeigh'
  plot(mean.segneigh, main = "Mean w/SegNeigh", ylab = "Postings")
  PlotGroundTruth(groundtruth.indexed)
  f1 <- CalculateF1(mean.segneigh@data.set,
              mean.segneigh@cpts,
              groundtruth.indexed)
  rand <- CalculateRand(mean.segneigh@data.set,
                mean.segneigh@cpts,
                groundtruth.indexed)
  write.csv(
    AnnotateClusters(mean.segneigh@data.set, mean.segneigh@cpts),
    "MeanSegNeighClusters.csv"
  )
  bcubed <- CalculateBCubed("MeanSegNeighClusters.csv")
  
  frame <- data.frame(
    'Algorithm' = algo,
    'Precision' = f1['Precision'],
    'Recall' = f1['Recall'],
    'F1' = f1['F1'],
    'Accuracy' = f1['Accuracy'],
    'Rand' = rand['Rand'],
    'Adj Rand' = rand['adjRand'],
    'BCubed Precision' = as.numeric(bcubed[1]),
    'BCubed Recall' = as.numeric(bcubed[2]),
    'BCubed FScore' = as.numeric(bcubed[3])
  )
  full.results <- rbind(frame, full.results)
  
  # Mean BinSeg
  mean.binseg <- cpt.mean(
    dataset$Freq,
    method = "BinSeg",
    penalty = penalty.function,
    minseglen = min.seglength
  )
  
  algo <- 'Mean BinSeg'
  
  plot(mean.binseg, main = "Mean w/BinSeg", ylab = "Postings")
  PlotGroundTruth(groundtruth.indexed)
  f1 <- CalculateF1(mean.binseg@data.set,
              mean.binseg@cpts,
              groundtruth.indexed)
  rand <- CalculateRand(mean.binseg@data.set,
                mean.binseg@cpts,
                groundtruth.indexed)
  write.csv(
    AnnotateClusters(mean.binseg@data.set, mean.binseg@cpts),
    "MeanBinSegClusters.csv"
  )
  bcubed <- CalculateBCubed("MeanBinSegClusters.csv")
  
  frame <- data.frame(
    'Algorithm' = algo,
    'Precision' = f1['Precision'],
    'Recall' = f1['Recall'],
    'F1' = f1['F1'],
    'Accuracy' = f1['Accuracy'],
    'Rand' = rand['Rand'],
    'Adj Rand' = rand['adjRand'],
    'BCubed Precision' = as.numeric(bcubed[1]),
    'BCubed Recall' = as.numeric(bcubed[2]),
    'BCubed FScore' = as.numeric(bcubed[3])
  )
  full.results <- rbind(frame, full.results)
  
  # Var PELT
  var.pelt <- cpt.var(
    dataset$Freq,
    method = "PELT",
    penalty = penalty.function,
    minseglen = min.seglength
  )
  
  algo <- 'Var PELT'
  
  plot(var.pelt, main = "Variance w/PELT", ylab = "Postings")
  PlotGroundTruth(groundtruth.indexed)
  f1 <- CalculateF1(var.pelt@data.set, var.pelt@cpts, groundtruth.indexed)
  rand <- CalculateRand(var.pelt@data.set, var.pelt@cpts, groundtruth.indexed)
  write.csv(AnnotateClusters(var.pelt@data.set, var.pelt@cpts),
            "VarPeltClusters.csv")
  bcubed <- CalculateBCubed("VarPeltClusters.csv")
  
  frame <- data.frame(
    'Algorithm' = algo,
    'Precision' = f1['Precision'],
    'Recall' = f1['Recall'],
    'F1' = f1['F1'],
    'Accuracy' = f1['Accuracy'],
    'Rand' = rand['Rand'],
    'Adj Rand' = rand['adjRand'],
    'BCubed Precision' = as.numeric(bcubed[1]),
    'BCubed Recall' = as.numeric(bcubed[2]),
    'BCubed FScore' = as.numeric(bcubed[3])
  )
  full.results <- rbind(frame, full.results)
  
  # Var SegNeigh
  var.segneigh <- cpt.var(
    dataset$Freq,
    method = "SegNeigh",
    penalty = penalty.function
  )
  
  algo <- 'Var SegNeigh'
  
  plot(var.segneigh, main = "Variance w/SegNeigh", ylab = "Postings")
  PlotGroundTruth(groundtruth.indexed)
  f1 <- CalculateF1(var.segneigh@data.set,
              var.segneigh@cpts,
              groundtruth.indexed)
  rand <- CalculateRand(var.segneigh@data.set,
                var.segneigh@cpts,
                groundtruth.indexed)
  write.csv(
    AnnotateClusters(var.segneigh@data.set, var.segneigh@cpts),
    "VarSegNeighClusters.csv"
  )
  bcubed <- CalculateBCubed("VarSegNeighClusters.csv")
  
  frame <- data.frame(
    'Algorithm' = algo,
    'Precision' = f1['Precision'],
    'Recall' = f1['Recall'],
    'F1' = f1['F1'],
    'Accuracy' = f1['Accuracy'],
    'Rand' = rand['Rand'],
    'Adj Rand' = rand['adjRand'],
    'BCubed Precision' = as.numeric(bcubed[1]),
    'BCubed Recall' = as.numeric(bcubed[2]),
    'BCubed FScore' = as.numeric(bcubed[3])
  )
  full.results <- rbind(frame, full.results)
  
  # Var BinSeg
  var.binseg <- cpt.var(
    dataset$Freq,
    method = "BinSeg",
    penalty = penalty.function,
    minseglen = min.seglength
  )
  
  algo <- 'Var BinSeg'
  
  plot(var.binseg, main = "Variance w/BinSeg", ylab = "Postings")
  PlotGroundTruth(groundtruth.indexed)
  f1 <- CalculateF1(var.binseg@data.set, var.binseg@cpts, groundtruth.indexed)
  rand <- CalculateRand(var.binseg@data.set, var.binseg@cpts, groundtruth.indexed)
  write.csv(
    AnnotateClusters(var.binseg@data.set, var.binseg@cpts),
    "VarBinSegClusters.csv"
  )
  bcubed <- CalculateBCubed("VarBinSegClusters.csv")
  
  frame <- data.frame(
    'Algorithm' = algo,
    'Precision' = f1['Precision'],
    'Recall' = f1['Recall'],
    'F1' = f1['F1'],
    'Accuracy' = f1['Accuracy'],
    'Rand' = rand['Rand'],
    'Adj Rand' = rand['adjRand'],
    'BCubed Precision' = as.numeric(bcubed[1]),
    'BCubed Recall' = as.numeric(bcubed[2]),
    'BCubed FScore' = as.numeric(bcubed[3])
  )
  full.results <- rbind(frame, full.results)
  
  # MeanVar PELT
  meanvar.pelt <- cpt.meanvar(
    dataset$Freq,
    method = "PELT",
    penalty = penalty.function,
    minseglen = min.seglength
  )
  
  algo <- 'MeanVar PELT'
  
  plot(meanvar.pelt, main = "Mean & Variance w/PELT", ylab = "Postings")
  PlotGroundTruth(groundtruth.indexed)
  f1 <- CalculateF1(meanvar.pelt@data.set,
              meanvar.pelt@cpts,
              groundtruth.indexed)
  rand <- CalculateRand(meanvar.pelt@data.set,
                meanvar.pelt@cpts,
                groundtruth.indexed)
  write.csv(
    AnnotateClusters(meanvar.pelt@data.set, meanvar.pelt@cpts),
    "MeanVarPeltClusters.csv"
  )
  bcubed <- CalculateBCubed("MeanVarPeltClusters.csv")
  
  frame <- data.frame(
    'Algorithm' = algo,
    'Precision' = f1['Precision'],
    'Recall' = f1['Recall'],
    'F1' = f1['F1'],
    'Accuracy' = f1['Accuracy'],
    'Rand' = rand['Rand'],
    'Adj Rand' = rand['adjRand'],
    'BCubed Precision' = as.numeric(bcubed[1]),
    'BCubed Recall' = as.numeric(bcubed[2]),
    'BCubed FScore' = as.numeric(bcubed[3])
  )
  full.results <- rbind(frame, full.results)
  
  # MeanVar SegNeigh
  meanvar.segneigh <- cpt.meanvar(
    dataset$Freq,
    method = "SegNeigh",
    penalty = penalty.function
  )
  
  algo <- 'MeanVar SegNeigh'
  
  plot(meanvar.segneigh, main = "Mean & Variance w/SegNeigh", ylab = "Postings")
  PlotGroundTruth(groundtruth.indexed)
  f1 <- CalculateF1(meanvar.segneigh@data.set,
              meanvar.segneigh@cpts,
              groundtruth.indexed)
  rand <- CalculateRand(meanvar.segneigh@data.set,
                meanvar.segneigh@cpts,
                groundtruth.indexed)
  write.csv(
    AnnotateClusters(meanvar.segneigh@data.set, meanvar.segneigh@cpts),
    "MeanVarSegNeighClusters.csv"
  )
  bcubed <- CalculateBCubed("MeanVarSegNeighClusters.csv")
  
  frame <- data.frame(
    'Algorithm' = algo,
    'Precision' = f1['Precision'],
    'Recall' = f1['Recall'],
    'F1' = f1['F1'],
    'Accuracy' = f1['Accuracy'],
    'Rand' = rand['Rand'],
    'Adj Rand' = rand['adjRand'],
    'BCubed Precision' = as.numeric(bcubed[1]),
    'BCubed Recall' = as.numeric(bcubed[2]),
    'BCubed FScore' = as.numeric(bcubed[3])
  )
  full.results <- rbind(frame, full.results)
  
  # MeanVar BinSeg
  meanvar.binseg <- cpt.meanvar(
    dataset$Freq,
    method = "BinSeg",
    penalty = penalty.function,
    minseglen = min.seglength
  )
  
  algo <- 'MeanVar BinSeg'
  
  plot(meanvar.binseg, main = "Mean & Variance w/BinSeg", ylab = "Postings")
  PlotGroundTruth(groundtruth.indexed)
  f1 <- CalculateF1(meanvar.binseg@data.set,
              meanvar.binseg@cpts,
              groundtruth.indexed)
  rand <- CalculateRand(meanvar.binseg@data.set,
                meanvar.binseg@cpts,
                groundtruth.indexed)
  write.csv(
    AnnotateClusters(meanvar.binseg@data.set, meanvar.binseg@cpts),
    "MeanVarBinSegClusters.csv"
  )
  bcubed <- CalculateBCubed("MeanVarBinSegClusters.csv")
  
  frame <- data.frame(
    'Algorithm' = algo,
    'Precision' = f1['Precision'],
    'Recall' = f1['Recall'],
    'F1' = f1['F1'],
    'Accuracy' = f1['Accuracy'],
    'Rand' = rand['Rand'],
    'Adj Rand' = rand['adjRand'],
    'BCubed Precision' = as.numeric(bcubed[1]),
    'BCubed Recall' = as.numeric(bcubed[2]),
    'BCubed FScore' = as.numeric(bcubed[3])
  )
  full.results <- rbind(frame, full.results)
  
  rownames(full.results) <- NULL
  write.csv(full.results, "fullmeasures.csv")
  
  return(full.results)
}