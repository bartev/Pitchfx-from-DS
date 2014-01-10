# Useful stuff, maybe


# Contingency table using tapply
require(stats)
groups <- as.factor(rbinom(32, n = 10, prob = 0.4))
g <- rbinom(32, n = 10, prob = 0.4)
g
groups

# tapply and table give the same (almost?) result
tapply(groups, groups, length)
table(groups)

table(g)
tapply(g, g, length)

# Contingency table
head(warpbreaks)
str(warpbreaks)
tapply(warpbreaks$breaks, warpbreaks[, -1], sum)
tapply((1:length(warpbreaks$breaks)), warpbreaks[, -1], sum)
# Sum up thebreaks for each instance of A/B & L/M/H
sum(1:nrow(warpbreaks))
45+126+207+288+369+450

head(warpbreaks[, -1])
# table is different from tapply here
table(warpbreaks[, -1])
# 9 instances of each A/B & L/M/H


# SumPitchPerBatter response to question on stackoverflow
# helps understand how tapply works
# the result of tapply is a table of different objects.
# In this case, each object is a list of different lengths
# unlist will turn the table into a long vector starting with column 1,
# working its way down the rows, then column 2, etc.
# That's why we transpose first.

n = 200
col1 = sample(c('a', 'b', 'c'), n, replace=T)
col2 = sample(paste('b', 1:4, sep=''), n, replace=T)
data <- data.frame(col1, col2, col3=0, col4=FALSE)
data <- data[do.call(order,data), ]
data

r1 <- tapply(as.numeric(data$col2), data[, 1:2], function(x) 1:length(x))
table(data[,1:2])
r1
str(r1)
# Look at individual cells in r1
r1[1,1]
r1[2,4]

t(r1)
unlist(r1)
unlist(t(r1))

data$col4[c(diff(data$col3), -1) <= 0] = TRUE
# the diff function creates a vector which is the current vector minus itself with some lag
x <- (cumsum(cumsum(1:10))); x
diff(x)
# > x <- (cumsum(cumsum(1:10))); x
#  [1]   1   4  10  20  35  56  84 120 165 220
# > diff(x)
# [1]  3  6 10 15 21 28 36 45 55