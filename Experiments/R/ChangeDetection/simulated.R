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