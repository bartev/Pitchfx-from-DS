# Bartev Vartanian
# 10/17/11
# Vectorize this function
# Look at ...

# Add before running this function
# source("pfxCumRunsFunction.R")
library(sqldf)
# Load output from pfxModifyAtbatData.R
# Do this to test this funcion
# load(" ")

# Make a function to keep track of how many runs are scored at each plate appearance
# and what the current score is
# Will use to see how a pitcher pitches in different game situations


################
# USAGE EXAMPLE - PitchNumPerBatter
# load("pfxDataSample.Rdat")
# Change game_id as we did for pitch data
# This gives common game_id between pitch & atbat data
# May not have to do this if '/' & '-' don't matter
# pa <- atbat
# pa$game_id <- gsub("[/-]", "", pa$game_id)
# # Get rid of NA's in home/away team runs
# pa$home_team_runs[which(is.na(pa$home_team_runs))]  <-  0
# pa$away_team_runs[which(is.na(pa$away_team_runs))]  <-  0
# 
# df <- pa[1:100,]
# df <- RunsScoredAtBat(df)
# head(df)


# Get updated score and runs scored per plate appearance
RunsScoredAtBat <- function(atbatDf) {
#   Adds the columns:
#     htRuns runs scored by home team at current plate appearance
#     atRuns runs scored by away team
#     homeScore current score home team (running total)
#     awayScore current score away team#   

#   For each plate appearance (pa), keep a running count of runs scored by home/away team on that pa,
#   and current score

#     Input is a df with at least game_id, half, inning, num, score, home_team_runs, away_team_runs
#     (columns from atbat df)
    
#   Requires library(sqldf)

#   First, take only the columns needed.
#   Working on all columns takes too long
  df <- sqldf('select game_id, half, inning, num, score, home_team_runs, away_team_runs from atbatDf')

#   Create new columns
  df$htRuns <- 0
  df$atRuns <- 0
  df$homeScore <- 0
  df$awayScore <- 0

  #   Initialize variables
  gameId <- 'xxx'
  
  numberBatters <- nrow(df)
  
  for (batter in 1:numberBatters) {
    if (df[batter, "game_id"] != gameId) {
      # Reset for new game
      scoreHome <- 0 # current running score - home
      scoreAway <- 0
      runsHome <- 0 # runs scored at current plate appearance - home
      runsAway <- 0

      gameId <- df[batter, "game_id"]
    }
    
    if (df[batter, 'score']) {
      # only update htRuns & atRuns if somebody scored
      if (df[batter, "half"] == 'top') {
      # top of inning (visitor batting)
        curScore <- df[batter, 'away_team_runs'] # new score for away team
        df[batter, 'atRuns'] <- curScore - scoreAway # runs scored this at bat (curScore - prevScore)
        scoreAway <- curScore # update scoreAway to be the curScore
      } else {
      # bottom of inning (home batting)
        curScore <- df[batter, 'home_team_runs']
        df[batter, 'htRuns'] <- curScore - scoreHome
        scoreHome <- curScore
      }
    }
    df[batter, 'homeScore'] <- scoreHome
    df[batter, 'awayScore'] <- scoreAway
  }
  htRuns <- df[, 'htRuns']
  atRuns <- df[, 'atRuns']
  homeScore <- df[, 'homeScore']
  awayScore <- df[, 'awayScore']
  return(cbind(atbatDf, htRuns, atRuns, homeScore, awayScore))
}
