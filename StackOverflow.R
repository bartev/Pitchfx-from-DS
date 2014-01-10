col1 <- c(rep("a", 10), rep('b', 8), rep('c',10))
col2 <- c(rep('b1', 3), rep('b2', 2), rep('b3', 5), rep('b1', 4), rep('b2', 4), rep('b1', 1), rep('b2', 3), rep('b3', 2), rep('b4', 4))
col3 <- c(1:3, 1:2, 1:5, 1:4, 1:4, 1, 1:3, 1:2, 1:4)
col4 <- c('F', 'F', 'T', 'F', 'T', 'F', 'F', 'F', 'F', 'T', 'F', 'F', 'F', 'T', 'F', 'F', 'F', 'T', 'T', 'F', 'F', 'T', 'F', 'T', 'F', 'F', 'F', 'T')


df <- data.frame(col1, col2, col3, col4)

df <- data.frame(col1, col2)
df$col3 <- 0
df$col4 <- FALSE
stopHere <- nrow(df)
c1 <- 'xxx'
c2 <- 'xxx'
for (i in 1:stopHere) {
  if (df[i, "col1"] != c1) {
    c2 <- 0
    c3 <- 1
    c1 <- df[i, "col1"]
  }
  if (df[i, "col2"] != c2) {
    df[i - 1, "col4"] <- TRUE
    c3 <- 1
    c2  <- df[i, "col2"]
  }
  df[i, "col3"] <- c3
  c3  <- c3 + 1
}
df