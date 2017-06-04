library(readr)
library(changepoint)

ing <- "~/Repos/master-thesis/Data/ing.csv"
rabobank <- "~/Repos/master-thesis/Data/rabobank.csv"
reddit <- "~/Repos/master-thesis/Data/reddit.csv"

ProcessData <- function(input, daily=TRUE) {
  # Switch depending on export type
  if (daily) {
    dataset <- read_delim(file = input, 
                          ",", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d-%m-%Y")), 
                          trim_ws = TRUE)
    colnames(dataset) <- c("Date", "Freq")
    
    return(dataset)
    
  } else {
    data.preprocess <- read_csv(input)
    # order the dataset by postingdate
    order(data.preprocess$postingdate)
    # calculate factor, indexing on posting date 
    dataset <- as.data.frame(table(factor(format(data.preprocess$postingdate, "%Y-%m-%d"))))
    colnames(dataset) <- c("Date", "Freq")
    return(dataset)
  }
}

GetPenalties <- function(input, daily=TRUE){
  dataset <- ProcessData(input, daily)

  mean.pelt.crops <- cpt.mean(dataset$Freq,
                        method="PELT",
                        penalty = "CROPS",
                        test.stat = "Normal",
                        pen.value = c(log(length(dataset$Freq)), 100*log(length(dataset$Freq))),
                        minseglen = 5,
                        class = TRUE)
  
  var.pelt.crops <- cpt.var(dataset$Freq,
                      method="PELT",
                      penalty = "CROPS",
                      test.stat = "Normal",
                      pen.value = c(log(length(dataset$Freq)), 100*log(length(dataset$Freq))),
                      minseglen = 5,
                      class = TRUE)
  
  meanvar.pelt.crops <- cpt.meanvar(dataset$Freq,
                              method="PELT",
                              penalty = "CROPS",
                              test.stat="Poisson",
                              pen.value = c(log(length(dataset$Freq)), 100*log(length(dataset$Freq))),
                              minseglen = 5,
                              class = TRUE)
  
  plot(mean.pelt.crops, diagnostic=TRUE, main="Mean PELT")
  plot(var.pelt.crops, diagnostic=TRUE, main="Variance PELT")
  plot(meanvar.pelt.crops, diagnostic=TRUE, main="Mean & Variance PELT")
}

RunExperiment <- function(input, daily=TRUE) {
  
  dataset <- ProcessData(input, daily)

  # set up the plot area
  par(mfrow = c(3,3))

  # run the experiments! 
  
  # Mean PELT
  mean.pelt <- cpt.mean(dataset$Freq,
                        method="PELT",
                        penalty = "Hannan-Quinn",
                        minseglen = 5
                        )
  
  plot(mean.pelt, main="Mean w/PELT", ylab="Postings")
  abline(v=33, col="blue", lty=2)
  abline(v=40, col="blue", lty=2)
  
  # Mean SegNeigh
  mean.segneigh <- cpt.mean(dataset$Freq,
                            method="SegNeigh",
                            penalty = "Hannan-Quinn",
                            Q=5)
  
  plot(mean.segneigh, main="Mean w/SegNeigh", ylab="Postings")
  abline(v=33, col="blue", lty=2)
  abline(v=40, col="blue", lty=2)
  
  # Mean BinSeg
  mean.binseg <- cpt.mean(dataset$Freq,
                          method="BinSeg",
                          test.stat = "CUSUM",
                          penalty = "Hannan-Quinn",
                          minseglen = 5,
                          Q=5)
  
  plot(mean.binseg, main="Mean w/BinSeg", ylab="Postings")
  abline(v=33, col="blue", lty=2)
  abline(v=40, col="blue", lty=2)
  
  # Var PELT
  var.pelt <- cpt.var(dataset$Freq,
                      method="PELT",
                      penalty = "Hannan-Quinn",
                      minseglen = 5)
  
  plot(var.pelt, main="Variance w/PELT", ylab="Postings")
  abline(v=33, col="blue", lty=2)
  abline(v=40, col="blue", lty=2)
  
  # Var SegNeigh
  var.segneigh <- cpt.var(dataset$Freq,
                          method="SegNeigh",
                          penalty = "Hannan-Quinn",
                          Q=5)
  
  plot(var.segneigh, main="Variance w/SegNeigh", ylab="Postings")
  
  # Var BinSeg
  var.binseg <- cpt.var(dataset$Freq,
                        method="BinSeg",
                        #test.stat = "CSS",
                        penalty = "Hannan-Quinn",
                        Q=5)
  
  plot(var.binseg, main="Variance w/BinSeg", ylab="Postings")
  abline(v=33, col="blue", lty=2)
  abline(v=40, col="blue", lty=2)
  
  # MeanVar PELT
  meanvar.pelt <- cpt.meanvar(dataset$Freq,
                              method="PELT",
                              test.stat = "Poisson",
                              penalty = "Hannan-Quinn",
                              minseglen = 5)
  
  plot(meanvar.pelt, main="Mean & Variance w/PELT", ylab="Postings")
  abline(v=33, col="blue", lty=2)
  abline(v=40, col="blue", lty=2)
  
  # MeanVar SegNeigh
  meanvar.segneigh <- cpt.meanvar(dataset$Freq,
                                  method="SegNeigh",
                                  penalty = "Hannan-Quinn",
                                  Q=5)
  
  plot(meanvar.segneigh, main="Mean & Variance w/SegNeigh", ylab="Postings")
  abline(v=33, col="blue", lty=2)
  abline(v=40, col="blue", lty=2)
  
  # MeanVar BinSeg
  meanvar.binseg <- cpt.meanvar(dataset$Freq,
                                method="BinSeg",
                                test.stat = "Poisson",
                                penalty = "Hannan-Quinn",
                                Q=5)
  
  plot(meanvar.binseg, main="Mean & Variance w/BinSeg", ylab="Postings")
  abline(v=33, col="blue", lty=2)
  abline(v=40, col="blue", lty=2)
  
}

ComparePenalties <- function(method="AMOC", input, daily=TRUE) {
  dataset <- ProcessData(input, daily)
  
  seglen = 4
  
  sic <- cpt.mean(dataset$Freq,
                  method="PELT",
                  penalty = "SIC",
                  minseglen = seglen)
  
  bic <- cpt.mean(dataset$Freq,
                  method="PELT",
                  penalty = "BIC",
                  minseglen = seglen)
  
  mbic <- cpt.mean(dataset$Freq,
                  method="PELT",
                  penalty = "MBIC",
                  minseglen = seglen)
  
  aic <- cpt.mean(dataset$Freq,
                  method="PELT",
                  penalty = "AIC",
                  minseglen = seglen)
  
  hannan <- cpt.mean(dataset$Freq,
                  method="PELT",
                  penalty = "Hannan-Quinn",
                  minseglen = seglen)
  
  asym <- cpt.mean(dataset$Freq,
                  method="PELT",
                  penalty = "Asymptotic",
                  pen.value = 0.05,
                  minseglen = seglen)
  
  par(mfrow = c(3,2))
  
  plot(sic, main="SIC", ylab="Postings")
  plot(bic, main="BIC", ylab="Postings")
  plot(mbic, main="MBIC", ylab="Postings")
  plot(aic, main="AIC", ylab="Postings")
  plot(hannan, main="Hannan-Quinn", ylab="Postings")
  plot(asym, main="Asymptotic (95%)", ylab="Postings")
}