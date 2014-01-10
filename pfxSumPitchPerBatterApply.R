# Bartev Vartanian
# 10/16/11

# Do pitch num using apply

# Add before running this function
# source("pfxSumPitchPerBatter.R")
library(sqldf)
# Load output from pfxModifyData.R
# Do this to test this funcion
load("pfxData-pp.Rdat")

# Make a function to sum up pitch number per batter
# The idea is to compare pitch number per pitcher with next pitch - strike/ball/hit

################
# USAGE EXAMPLE - PitchNumPerBatter
# Sample of running this function
# pp is a data frame that contains at least the columns
#   game_id - unique game id
#   num - at bat number for the game
# df1 <- PitchNumPerBatter(pp)
# head(df1, 20)
# head(pp)

head(pp)
df1 <- sqldf('select game_id, num from pp')
head(df1)
unique(df1$game_id)

# Running sum of pitches for each batter
PitchNumPerBatterApply <- function(pitchDf) {
#   Adds the column pitchNum to the pitchDf, returns a df with the added column.
#   For each batter number in a game (atbat), number the  pitches taken
#   Input is a df with game_id, and num (at bat number)

#   Requires library(sqldf)
  
#   First, take only the columns needed.
#   Working on all columns takes too long
  df <- sqldf('select game_id, num from pitchDf')

#   Create new column
  df$pitchNum <- 0
  df$finalPitch <- FALSE

#   Initialize variables
  numPitchesInSeason <- nrow(df)
  gameId <- "xxx"
  atBat <- 0
  curPitchNum <- 1
  
  for (pn in 1:numPitchesInSeason) {
    if (df[pn, "game_id"] != gameId) {
      atBat <- 0
      curPitchNum <- 1
      gameId <- df[pn, "game_id"]
    }
    if (df[pn, "num"] != atBat) {
      df[pn - 1, "finalPitch"] <- TRUE
      curPitchNum <- 1
      atBat  <- df[pn, "num"]
    }
    df[pn, "pitchNum"] <- curPitchNum
    curPitchNum  <- curPitchNum + 1
  }
  pitchNum <- df$pitchNum
  finalPitch <- df$finalPitch
  return(cbind(pitchDf, pitchNum, finalPitch))
}

supportFunction <- function(df){
  #   df has 4 columns: game_id, num, pitchNum, finalPitch (T/F)
  
}
    
fun1 <- function(pitchDf) {
#   Works great. Simple. Elegant
  df <- sqldf('select game_id, num from pitchDf')  
  #   Create new column
  df$pitchNum <- 0
  df$finalPitch <- FALSE
  
  zz <- rle(as.character(df$num))$lengths
  df$pitchNum <- sequence(zz)
  df$finalPitch[head(cumsum(zz), -1 )]  <- TRUE
  return(df)
}

# Test fun1
# Works great
df1 <- fun1(ppz)
head(df1)

df2 <- fun1(pitch09)
head(df2, 50)


# This solution is more confusing.
# It uses tapply and a function(x) 1:length(x)
fun2 <- function(pitchDf) {
  data <- sqldf('select game_id, num from pitchDf')
#   data = data.frame(pitchDf$game_id, pitchDf$num, pitchNum=0, finalPitch=FALSE)
  # data = data[do.call(order, data), ]
  data$pitchNum  <- 0
  data$finalPitch <- FALSE
  data$pitchNum = unlist(
    t(
      tapply(
        as.numeric(data$num), data[, 1:2], function(x) 1:length(x))
      )
    )
  
  data$finalPitch[c(diff(data$pitchNum), -1) < 0] = TRUE
  return(data)
}

fun3 <- function(pitchDf){
#   Combine fun1 & fun2 methods
  df <- sqldf('select game_id, num from pitchDf')  
  #   Create new column
  df$pitchNum <- 0
  df$finalPitch <- FALSE
  
  zz <- rle(as.character(df$num))$lengths
  df$pitchNum <- sequence(zz)
  data$finalPitch[c(diff(data$pitchNum), -1) < 0] = TRUE
#   df$finalPitch[head(cumsum(zz), -1 )]  <- TRUE
  return(df)

}

df1 <- fun1(pitch09) # ~12 sec
df2 <- fun2(pitch09) # ~14 sec
df3 <- fun3(pitch09) # ~12 sec

df3 <- fun2(ppz)
head(df3)
tail(df3)

n = 25

col1 = sample(c('a', 'b', 'c'), n, replace=T)
# col1 = sample(c('a', 'b', 'c', 'd'), n, replace=T)
col2 = sample(paste('b', 1:4, sep=''), n, replace=T)
data <- data.frame(col1, col2, col3=0, col4=FALSE)
data <- data[do.call(order,data), ]
data

data$col2 <- as.character(data$col2)
runs <- rle(data$col2)
runs

runs.s <- sapply(runs$lengths, function(l) seq(length.out = l))
unlist(runs.s)

runs.l <- runs$lengths
sequence(runs.l)


seq(col1)
col1 <- sort(col1)

length(col1)
length(col2)
length(seq(col1))

c1 <- data$col1
c2 <- data$col2
a1 <- aggregate(seq(c1), by = list(x = c1, y = c2), min)
a2 <- aggregate(seq(c1), by = list(x = c1, y = c2), max)
data
a1
a1; a2
aggregate(seq(c1), by = list(x = c1), max)
data
merge(a1, a2)
minmax <- with(data,merge(a1, a2))
names(minmax)[3:4] <- c('min', 'max')
result <- with(merge(data, minmax),
               data.frame(x, y, count = seq(x) - min + 1, last = seq(x) == max))
result

minmax

head(pitch09)
head(pp)
head(ppz)

hits <- aggregate(ppz$Hit, by = list(ppz$pitcher), sum)$x
summary(hits$x)
plot(hits)
balls <- aggregate(ppz$Ball, by = list(ppz$pitcher), sum)$x
plot(balls)
strikes <- aggregate(ppz$Strike, by = list(ppz$pitcher), sum)$x
plot(strikes)
sum(hits)+
sum(balls)+
sum(strikes)
head(balls)
df <- data.frame(balls, strikes, hits)
head(df)
plot(df)

attach(df)
plot(balls, balls)
plot(balls, strikes)
plot(balls, hits)
plot(strikes, hits)
l <- lm(strikes ~ hits)
plot(l)
