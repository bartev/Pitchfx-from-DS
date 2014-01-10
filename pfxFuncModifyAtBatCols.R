# Bartev Vartanian
# 10/25/11
#
# Function definition to modify atbat data


###### Usage example
# Use 'atbat' data.
# This function will 
# * Modify the game_id (get rid of [/-])
# * Remove NA from the home/away_team_runs colums
# * Create a unique batter/game ide (ubnum)
# * calculate the runs scored for home team (htRuns/atRuns) and away team
# * Get the running score for the game (homeScore/awayScore)
# * Get a value for how many runs the pitcher is winning/losing by (pAhead)

# To test this, can use atbat, which can be loaded here
# load("pfxDataSample.Rdat")  # pitchfx '09 data - 5000 row max
# ab <- atbat
# ab1 <- ModifyAtBatCols(ab)


ModifyAtBatCols <- function (ab) {
  # Modify and add colums to atbat file
  # edit game_id
  ab$game_id <- gsub("[/-]", "", ab$game_id)
  # Get rid of NA's in home/away team runs
  ab$home_team_runs[is.na(ab$home_team_runs)] <- 0
  ab$away_team_runs[is.na(ab$away_team_runs)] <- 0
  # Add unique batter id (game_id + num)
  ab$ubnum <- paste(ab$game_id, ab$num)
  
  # Create new columns
  ab <- transform(ab, htRuns=0, atRuns=0, homeScore=0, awayScore=0)
  
  # this function will return the cumulative max for the columns in df
  # Didn't use this. Used an inline function instead.
  # GetCumRuns <- function(df) apply(df, 2, cummax)
  
  ##### Good here!
  # Now run GetCumRuns on groups of game_id & half
  # d3 <- sapply(split(df1[,c('home_team_runs', 'away_team_runs')], df1$game_id), GetCumRuns) # alternate method using sapply & split
  abm <- by(ab[,c('home_team_runs', 'away_team_runs')], ab$game_id, function(d) apply(d, 2, cummax))
  # Combine output of 'by' into a dataframe (stack output together)
  abm <- as.data.frame(do.call(rbind, abm))
  # Add new columns to original data frame
  ab$homeScore <- abm$home_team_runs
  ab$awayScore <- abm$away_team_runs
  
  # GetDiffVec <- function(v) diff(c(0, v))
  # I had trouble using by here, perhaps because too few dimensions?
  hs <- sapply(split(ab[, ('homeScore')], ab$game_id), function(v) diff(c(0, v)))
  as <- sapply(split(ab[, ('awayScore')], ab$game_id), function(v) diff(c(0, v)))
  ab$htRuns <- as.vector(unlist(hs))
  ab$atRuns <- as.vector(unlist(as))

  # Add a column for the number of runs scored on this atbat against the pitcher.
#   ab <- transform(ab, runsOnPitcher=(atRuns * as.numeric(half == 'top')) + (htRuns * as.numeric(half == 'bottom')))
  ab$runsOnPitcher = (ab$atRuns * as.numeric(ab$half == 'top')) + (ab$htRuns * as.numeric(ab$half == 'bottom'))

  # Add a column to show how many runs the pitcher is leading by.
#   ab <- transform(ab, pAhead=(homeScore - awayScore)*2*(0.5 - as.numeric(half=='bottom')))
  ab$pAhead = (ab$homeScore - ab$awayScore)*2*(0.5 - as.numeric(ab$half=='bottom'))
  return(ab)
}


