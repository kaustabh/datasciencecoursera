add2 <- function(a, b) {
  a + b
}

above10 <- function(x) {
  use <- x > 10 #gets a subset of vector x with elements > 10
  x[use]
}

above <- function(x, n = 10) { #default cutoff value is 10
  use <- x > n #gets a subset of vector x with elements > n
  x[use]
}

#iterate through all columns of a matrix and find mean of all
columnmean <- function(y, removeNA = TRUE) {
  nc <- ncol(y)
  means <- numeric(nc)
  for (i in 1:nc) {
    means[i] <- mean(y[, i], na.rm = removeNA)
  }
  
  means
}