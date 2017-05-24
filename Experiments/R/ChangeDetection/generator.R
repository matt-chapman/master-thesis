generateData <- function(mean=100, sd=2, n=59, output=NULL) {
    dates <- seq.Date(from = as.Date("2017/01/01"), length.out = n, 
        by = "day")
    points <- rnorm(n, mean, sd)
    
    df <- data.frame(dates, points)
    names(df) <- c("Date", "Postings")
    
    if (is.null(output)) {
        return(df)
    } else {
        write.csv(df, file=output)
    }
}