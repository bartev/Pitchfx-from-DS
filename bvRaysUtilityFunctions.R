# Bartev Vartanian
# Utility functions
# 2011-10-04

# Running sum of a vector

# Running sum of a vector
RunningSum <- function(x) {
#   Returns a vector containing the running sum of its elements
#   e.g. v <- 1:10 returns 1, 3, 6, ..., 45, 55
  rsum <- vector(length=length(x))
  rsum[1] <- x[1]
  if (length(x) > 1) {
    for (i in 2:length(x))
      rsum[i] <- rsum[i-1] + x[i]
  }
  return(rsum)
}
# Usage example:
# v <- 1:10
# s <- RunningSum(v)
# s

## function to scale x using attributes from trained
ScaleFromScaled <- function(x, trained){
  x <- sweep(x, 2, attr(trained, 'scaled:center'), FUN='-')
  x <- sweep(x, 2, attr(trained, 'scaled:scale'), FUN='/')
  return(x)
}
UnScaleFromScaled <- function(x, trained){
  x <- sweep(x, 2, attr(trained, 'scaled:scale'), FUN='*')
  x <- sweep(x, 2, attr(trained, 'scaled:center'), FUN='+')
  return(x)
}
