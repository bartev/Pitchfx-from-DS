# Bartev Vartanian
# 10/24/11
# 
# Function definition to modify pitch data
# library(sqldf)
source('pfxSumPitchPerBatter.R')

ModifyPitchCols <- function(pp) {
#   pp is one of the pitch dataframes (p09, pitch, pitch09)
#   This function will modify the columns adding additional variables.
  
#   Work with subsets of the data to speed things up?

  # edit game_id
  # ~7 sec
  pp$game_id <- gsub("[/-]", "", pp$game_id);
  
  # Add the count to use as a factor later
  # Count has 12 factors: "0 0", "0 1", ...
  # 7 seconds
  pp$count <- as.factor(paste(pp$b, sapply(pp$s, function (s) min(s, 2))))
  
  # Add pitch number per batter - runs through entire database.
  # Use function PitchNumPerBatter(df) from pfxSumPitchPerBatter.R
  # ~3-4 sec
  pp <- PitchNumPerBatter(pp)  # defined in 'pfxSumPitchPerBatter.R'
  
  # Add unique pitch id (game_id + id)
  temp <- pp[, c('game_id', 'num')]
  pp$upid <- paste(temp$game_id, temp$id)
  # Add unique batter id (game_id + num)
  pp$ubnum <- paste(temp$game_id, temp$num)
  
  # Add columns for Pitch results Ball/Strike/Hit
#   temp <- sqldf('select type from pp')
#   transform was slower? than doing 3 assignments
#   pp <- transform(pp, Ball = as.numeric(type == 'B'), Strike = as.numeric(type == 'S'), Hit = as.numeric(type == 'X'))
  pp$Ball  <-  as.numeric(pp$type == 'B')
  pp$Strike <- as.numeric(pp$type == 'S')
  pp$Hit <- as.numeric(pp$type == 'X')
    
  # Clean up NA's in data
  pp$on_1b[which(is.na(pp$on_1b))] <- 0
  pp$on_2b[which(is.na(pp$on_2b))] <- 0
  pp$on_3b[which(is.na(pp$on_3b))] <- 0
  
  # Scale columns for strike zone
  temp <- subset(pp, , select=c(px, pz, sz_top, sz_bot))
#   temp <- sqldf('select px, pz, sz_top, sz_bot from pp')
#   temp <- pp[, c('px', 'pz', 'sz_top', 'sz_bot')]
  maxX <- 17/12/2
  temp <- transform(temp,
                   pxScale = (px / maxX),
                   pzScale = 2 * (pz - (sz_top + sz_bot) / 2) / (sz_top - sz_bot)
                   # pzRange = sz_top - sz_bot
                   )
  temp <- transform(temp, inZone = (abs(pxScale) <= 1) & (abs(pzScale) <= 1))
  # ~ 4 sec
  temp <- subset(temp, , select = pxScale:inZone)
  pp <- cbind(pp, temp)
  
  return(pp)
}