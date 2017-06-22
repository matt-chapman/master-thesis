long.delay <- c(rep(0,900), rep(1,100))
short.delay <- c(rep(0,100), rep(1,900))

long.delay.cpt <- 900
short.delay.cpt <- 100

small.data <- c(rep(0,50), rep(1,50))
large.data <- c(rep(0,2500), rep(1,2500))

small.data.cpt <- 50
large.data.cpt <- 2500

# clustering - false positives can have large effect on measures, smaller
# datasets also make measure more sensitive

#' Increases tail of dataset
TestExperiment <- function() {
  
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
  
  for ( i in 1:500) {
    data <- c(rep(0,50), rep(1, i))
    interim <- CalculateArbitrary(data, 51, 50)
    interim$Distance <- i
    results <- rbind(interim, results)
  }
  
  return(results)
  
}

#' Increases head of dataset
TestExperiment3 <- function() {
  
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
  
  for ( i in 1:500) {
    data <- c(rep(0,i), rep(1, (501 - i)))
    interim <- CalculateArbitrary(data, (i - 1), (i + 1))
    interim$Distance <- i
    results <- rbind(interim, results)
  }
  
  return(results)
  
}

#' Moves changepoint through dataset
TestExperiment2 <- function() {
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
  
  data <- c(rep(0,50), rep(1, 450))
  
  for (i in 1:500) {
    interim <- CalculateArbitrary(data, i, 51)
    interim$Distance <- i
    results <- rbind(interim, results)
  }
  
  return(results)
}



PlotResults <- function(data) {
  
  colours <- c("#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7", 
                    "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD", 
                    "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D", 
                    "#8A7C64", "#599861")
  
  ggplot(data, aes(x = Distance, y = value, colour = variable)) +
    geom_line() +
    ylab(label="Score") +
    xlab(label="Additional Points") +
    scale_color_manual(values = colours)
}