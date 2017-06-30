library(caret)
library(phyclust)
library(ROCR)
library(rPython)

#' calculate scores based on generated data
CalculateArbitrary <- function(dataset, points, truth) {
  f1 <- CalculateF1(dataset, points, truth)
  rand <- CalculateRand(dataset, points, truth)
  
  write.csv(AnnotateClusters(dataset, points),
            "ArbitraryDataClusters.csv")
  write.csv(AnnotateClusters(dataset, truth),
            "GroundTruthClusters.csv")
  
  bcubed <- CalculateBCubed('ArbitraryDataClusters.csv')
  
  results <- data.frame(
    'Precision' = f1['Precision'],
    'Recall' = f1['Recall'],
    'F1' = f1['F1'],
    'Rand' = rand['Rand'],
    'Adj Rand' = rand['adjRand'],
    'BCubed Precision' = as.numeric(bcubed[1]),
    'BCubed Recall' = as.numeric(bcubed[2]),
    'BCubed FScore' = as.numeric(bcubed[3])
  )
  
  par(mfrow = c(1, 1))
  plot(dataset)
  PlotGroundTruth(truth)
  PlotChangePoints(points)
  
  return(results)
}


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
    positive = '1',
    mode = "prec_recall"
  )
  print(result)
  return(c(
    result$byClass['Precision'],
    result$byClass['Recall'],
    result$byClass['F1'],
    result$overall['Accuracy']
  ))
  
}

BuildROCPlot <- function(dataset, points, truth) {
  algo.predictions <- numeric(length(dataset))
  ground.truth <- numeric(length(dataset))
  
  for (i in truth) {
    ground.truth[i] <- 1
  }
  
  for (i in points) {
    algo.predictions[i] <- 1
  }
}

#' Calculate Rand & Adjusted Rand
CalculateRand <- function(input, changepoints, truthpoints) {
  clustering <- AnnotateClusters(input, changepoints)
  truth <- AnnotateClusters(input, truthpoints)
  
  clustering <- clustering + 1
  truth <- truth + 1
  
  result <- RRand(truth, clustering)
  return(c(result['Rand'], result['adjRand']))
}

#' Call python script to calculate BCubed values
CalculateBCubed <- function(input) {
  result <- system(paste("python RunBCubed.py", input), intern = T)
  return(c(result[1], result[2], result[3]))
}

AlternativeBCubed <- function(dataset, points, truth) {
  algo.predictions <- numeric(length(dataset))
  ground.truth <- numeric(length(dataset))
  
  for (i in truth) {
    ground.truth[i] <- 1
  }
  
  ground.truth <- ground.truth + 1
  
  for (i in points) {
    algo.predictions[i] <- 1
  }
  
  algo.predictions <- algo.predictions + 1
  
  return(BCubed_metric(ground.truth, algo.predictions, 0.5))
}