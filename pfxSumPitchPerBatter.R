# Bartev Vartanian
# 10/16/11

# Function definition only


# Before running this function in another script, add this line
# source("pfxSumPitchPerBatter.R")

# Make a function to sum up pitch number per batter
# The idea is to compare pitch number per pitcher with next pitch - strike/ball/hit

# library(sqldf)



################
# USAGE EXAMPLE - PitchNumPerBatter

# To test this function,
# Load output from pfxModifyData.R
# load("pfxData-pp.Rdat")

# Sample of running this function
# pp is a data frame that contains at least the columns
#   game_id - unique game id
#   num - at bat number for the game
# df1 <- PitchNumPerBatter(pp)
# head(df1, 20)
# head(pp)


################
# Running sum of pitches for each batter
PitchNumPerBatter <- function(pitchDf) {
#   Adds the columns pitchNum and finalPitch (bool) to the pitchDf
#   Returns a df with the added columns.
  
#   For each batter number in a game (atbat), number the  pitches taken
#   Input is a df with game_id, and num (at bat number)
#   Got some great help from stack overflow. Cut the time from hours to seconds for 700k rows.
  #   Combine fun1 & fun2 methods
#   df <- sqldf('select game_id, num from pitchDf') 
#   selecting the 2 columns is MUCH faster than using sqldf
  df <- pitchDf[, c('game_id', 'num')]
  #   Create new column
  df$pitchNum <- 0
  df$finalPitch <- FALSE
  
  zz <- rle(as.character(df$num))$lengths
  df$pitchNum <- sequence(zz)
#   data$finalPitch[c(diff(data$pitchNum), -1) < 0] = TRUE
  df$finalPitch[head(cumsum(zz), -1 )]  <- TRUE
  
  pitchNum <- df$pitchNum
  finalPitch <- df$finalPitch
  df <- cbind(pitchDf, pitchNum, finalPitch)
  return(df)
}






# df <- sqldf('select game_id, num, batterNum, id from pp')
# head(df)
# df$game_id <- as.factor(df$game_id)

# NOT WORKING. NOT SURE WHY THIS SQLDF STATEMENT WON'T WORK
# I ended up using a for loop over every line, so didn't even need this
# I thought it might have to do with '-', '/', or '.' in the string, but even getting rid
# of all of those (in pfxModifyData.R) didn't seem to fix the problem
# p2 <- sqldf(paste('select id, num, batterNum from df where game_id = ', (g[1])))
# Do it using subset instead
# gg <- g[2]
# p2 <- subset(df, game_id == gg, select = c(id, num, batterNum))
# p2$batterNum <- factor(p2$batterNum) # this resets the factors

