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
  
  for (i in 2:500) {
    data <- c(rep(0, 50), rep(1, i))
    interim <- CalculateArbitrary(data, 45, 51)
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
    data <- c(rep(0, i), rep(1, (501 - i)))
    interim <- CalculateArbitrary(data, (i - 4), (i + 1))
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
  
  for (i in 51:500) {
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
    interim <- CalculateArbitrary(data, i - 4, i + 1)
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
    'BCubedAlt' = numeric(0),
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
    interim$BCubedAlt <- AlternativeBCubed(data, changepoints, 51)
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

PlotResults <- function(data, xlab) {
  colours <-
    c(
      "#89C5DA",
      "#DA5724",
      "#74D944",
      "#CE50CA",
      "#3F4921",
      "#C0717C",
      "#CBD588",
      "#5F7FC7",
      "#673770",
      "#D3D93E",
      "#38333E",
      "#508578",
      "#D7C1B1",
      "#689030",
      "#AD6F3B",
      "#CD9BCD",
      "#D14285",
      "#6DDE88",
      "#652926",
      "#7FDCC0",
      "#C84248",
      "#8569D5",
      "#5E738F",
      "#D1A33D",
      "#8A7C64",
      "#599861"
    )
  
  ggplot(data, aes(x = Distance, y = value, colour = variable)) +
    geom_line() +
    ylab(label = "Score") +
    xlab(label = xlab) +
    scale_color_manual(values = colours) +
    labs(color = 'Metric')
}
