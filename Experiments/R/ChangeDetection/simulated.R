library(reshape2)

long.delay <- c(rep(0, 900), rep(1, 100))
short.delay <- c(rep(0, 100), rep(1, 900))

long.delay.cpt <- 900
short.delay.cpt <- 100

small.data <- c(rep(0, 50), rep(1, 50))
large.data <- c(rep(0, 2500), rep(1, 2500))

small.data.cpt <- 50
large.data.cpt <- 2500

#' comparative analysis - when comparing algorithms, do they agree?
#' Statistical significance
#' Mathematics
#' Principles - what a good metric should look like (axioms)

RunAllSimulations <- function() {
  data1 <- Experiment1()
  data2 <- Experiment2()
  data3 <- Experiment3()
  data4 <- Experiment4()
  data5 <- Experiment5()
  
  data1melted <- melt(data1, id = 'Distance')
  data2melted <- melt(data2, id = 'Distance')
  data3melted <- melt(data3, id = 'Distance')
  data4melted <- melt(data4, id = 'Distance')
  data5melted <- melt(data5, id = 'Distance')
}

# clustering - false positives can have large effect on measures, smaller
# datasets also make measure more sensitive

#' Increases tail of dataset, and dataset length
Experiment1 <- function() {
  results = data.frame(
    row.names = 'Distance',
    'Distance' = numeric(0),
    'Precision' = numeric(0),
    'Recall' = numeric(0),
    'F1' = numeric(0),
    'Rand' = numeric(0),
    'Adj Rand' = numeric(0),
    'BCubed Precision' = numeric(0),
    'BCubed Recall' = numeric(0),
    'BCubed FScore' = numeric(0)
  )
  
  for (i in 5:500) {
    data <- c(rep(0, 50), rep(1, i))
    interim <- CalculateArbitrary(data, 55, 51)
    interim$Distance <- i
    results <- rbind(interim, results)
  }
  
  return(results)
  
}

#' Moves true and detected changepoint through constant length data
Experiment2 <- function() {
  results = data.frame(
    row.names = 'Distance',
    'Distance' = numeric(0),
    'Precision' = numeric(0),
    'Recall' = numeric(0),
    'F1' = numeric(0),
    'Rand' = numeric(0),
    'Adj Rand' = numeric(0),
    'BCubed Precision' = numeric(0),
    'BCubed Recall' = numeric(0),
    'BCubed FScore' = numeric(0)
  )
  
  for (i in 5:500) {
    data <- c(rep(0, i), rep(1, (505 - i)))
    interim <- CalculateArbitrary(data, (i+5), (i + 1))
    interim$Distance <- i
    results <- rbind(interim, results)
  }
  
  return(results)
  
}

#' Moves changepoint through dataset, temporal penalty
Experiment3 <- function() {
  results = data.frame(
    row.names = 'Distance',
    'Distance' = numeric(0),
    'Precision' = numeric(0),
    'Recall' = numeric(0),
    'F1' = numeric(0),
    'Rand' = numeric(0),
    'Adj Rand' = numeric(0),
    'BCubed Precision' = numeric(0),
    'BCubed Recall' = numeric(0),
    'BCubed FScore' = numeric(0)
  )
  
  data <- c(rep(0, 49), rep(1, 451))
  
  for (i in 1:500) {
    interim <- CalculateArbitrary(data, i, 51)
    interim$Distance <- i
    results <- rbind(interim, results)
  }
  
  return(results)
}

#' Increase head length & dataset length
Experiment4 <- function() {
  results = data.frame(
    row.names = 'Distance',
    'Distance' = numeric(0),
    'Precision' = numeric(0),
    'Recall' = numeric(0),
    'F1' = numeric(0),
    'Rand' = numeric(0),
    'Adj Rand' = numeric(0),
    'BCubed Precision' = numeric(0),
    'BCubed Recall' = numeric(0),
    'BCubed FScore' = numeric(0)
  )
  
  for (i in 5:500) {
    data <- c(rep(0, i), rep(1, 50))
    interim <- CalculateArbitrary(data, i +5, i + 1)
    interim$Distance <- i
    results <- rbind(interim, results)
  }
  
  return(results)
}

#' False Positives
Experiment5 <- function() {
  results = data.frame(
    row.names = 'Distance',
    'Distance' = numeric(0),
    'Precision' = numeric(0),
    'Recall' = numeric(0),
    'F1' = numeric(0),
    'Rand' = numeric(0),
    'Adj Rand' = numeric(0),
    'BCubed Precision' = numeric(0),
    'BCubed Recall' = numeric(0),
    'BCubed FScore' = numeric(0)
  )
  
  changepoints = numeric(0)
  numchangepoints = 0
  
  for (i in c(51, seq(55, 500, 5))) {
    data <- c(rep(0, 50), rep(1, 450))
    changepoints <- c(changepoints, i)
    numchangepoints <- numchangepoints + 1
    interim <- CalculateArbitrary(data, changepoints, 51)
    interim$Distance <- numchangepoints
    results <- rbind(interim, results)
  }
  
  return(results)
}

#' False Negatives
Experiment6 <- function() {
  results = data.frame(
    row.names = 'Distance',
    'Distance' = numeric(0),
    'Precision' = numeric(0),
    'Recall' = numeric(0),
    'F1' = numeric(0),
    'Rand' = numeric(0),
    'Adj Rand' = numeric(0),
    'BCubed Precision' = numeric(0),
    'BCubed Recall' = numeric(0),
    'BCubed FScore' = numeric(0)
  )
  
  data <- rep(c(rep(0, 100), rep(1, 100)), 5)
  truepoints <- seq(100, 900, 100)
  points = numeric(0)
  numchangepoints <- 10
  
  for (i in seq(0, 900, 100)) {
    points <- c(points, i)
    numchangepoints <- numchangepoints - 1
    interim <- CalculateArbitrary(data, points, truepoints)
    interim$Distance <- numchangepoints
    results <- rbind(interim, results)
  }
  
  return(results)
  
}

#' add true and detected changepoints
Experiment7 <- function() {
  results = data.frame(
    row.names = 'Distance',
    'Distance' = numeric(0),
    'Precision' = numeric(0),
    'Recall' = numeric(0),
    'F1' = numeric(0),
    'Rand' = numeric(0),
    'Adj Rand' = numeric(0),
    'BCubed Precision' = numeric(0),
    'BCubed Recall' = numeric(0),
    'BCubed FScore' = numeric(0)
  )

  changes <- c(rep(0,25), rep(1,25))
  data <- changes
  changepoints <- 28
  truthpoints <- 26
  interim <- CalculateArbitrary(data, changepoints, truthpoints)
  xval <- 1
  interim$Distance <- xval
  results <- rbind(interim, results)
  
  for(i in seq(50, 1000, 50)) {
    xval <- xval + 2
    data <- c(data, changes)
    truthpoints <- c(truthpoints, (i+1), (i+26))
    changepoints <- c(changepoints, (i+2), (i+28))
    interim <- CalculateArbitrary(data, changepoints, truthpoints)
    interim$Distance <- xval
    results <- rbind(interim, results)
  }

  return(results)
}

#' add true and detected changepoints
Experiment8 <- function() {
  results = data.frame(
    row.names = 'Distance',
    'Distance' = numeric(0),
    'Precision' = numeric(0),
    'Recall' = numeric(0),
    'F1' = numeric(0),
    'Rand' = numeric(0),
    'Adj Rand' = numeric(0),
    'BCubed Precision' = numeric(0),
    'BCubed Recall' = numeric(0),
    'BCubed FScore' = numeric(0)
  )
  
  data <- numeric(1050)
  changepoints <- numeric(0)
  truthpoints <- numeric(0)
  #data[i:j] <- data[i:j] + 1

  #interim <- CalculateArbitrary(data, changepoints, truthpoints)
  #interim$Distance <- 1
  #results <- rbind(interim, results)
  x <- 0
  for(i in seq(25, 1000, 50)) {
    j <- i + 25
    data[i:j] <- data[i:j] + 1
    truth1 <- i + 1
    truth2 <- j + 1
    point1 <- truth1 + 2
    point2 <- truth2 + 2
    x <- x + 2
    truthpoints <- c(truthpoints, truth1, truth2)
    changepoints <- c(changepoints, point1, point2)
    interim <- CalculateArbitrary(data, changepoints, truthpoints)
    interim$Distance <- x
    results <- rbind(interim, results)
  }
  
  return(results)
}

PlotResults <- function(data, xlab) {
  colours <-
    c(
      "#89C5DA",
      "#AD6F3B",
      "#74D944",
      "#CE50CA",
      "#3F4921",
      "#C0717C",
      "#D3D93E",
      "#673770"
    )
  
  altcolours <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#7f9094")
  
  ggplot(data, aes(x = Distance, y = value, colour = variable)) +
    geom_line() +
    ylab(label = "Score") +
    xlab(label = xlab) +
    scale_color_manual(values = altcolours, labels=c('Precision', 'Recall', 'F1', 'Rand Index', 'Adjusted Rand Index', 'BCubed Precision', 'BCubed Recall', 'BCubed F-Score')) +
    labs(color = 'Metric')
}
