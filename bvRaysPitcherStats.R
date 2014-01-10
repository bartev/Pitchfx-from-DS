# Look at 1 pitcher, figure out some statistics
# Includes function GetPitcherStatsForGame which gets pitching stats for a given pitcher/game combo
# Xtab (table of des vs inZone counts)

# ngp is a df with pitcher id and number of games pitched (see StartingPitchersOnly.R)

# Get pitcherId for 1st pitcher (most games pitched)
pid <- ngp[1,1]
pid <- as.data.frame(ngp[1, ])
pid <- as.character(pid)
class(pid)
pid

# build a df with pitcher info
# add columns for all pitches

# head(atbat09)
# head(pitch09)
# str(pitch09)

# g1 = game 1
pid[1,1]
gamesPitched <- sqldf(paste("select distinct game_id, pitcher from atbat09 where pitcher = ", pid[1,1]))

game1 <- gamesPitched[1,]
game1
# Might be faster to turn this into a sqldf satement
maxX <- 17/12/2
pitchZone <- transform(pitch09, inZone = ((abs(px) < maxX) & (pz > sz_bot) & (pz < sz_top)))
shortPitch <- sqldf(
  "select game_id, num, pitcher, batter, b, s, des, id, type, inZone, on_1b, on_2b, on_3b, pitch_type, zone, month, day, vis_team, home_team from pitchZone")

# balls <- sqldf("select * from shortPitch where type = 'B'");
# strikes <- sqldf("select * from shortPitch where type = 'S'")
# hits <- sqldf("select * from shortPitch where type = 'X'")

gid <- game1$game_id
gid

GetPitcherStatsForGame(gid=gid, pid=pid[1,1])

GetPitcherStatsForGame <- function(gid, pid, data=shortPitch){
#   Return a df with 1 row containing
#   gameId, pitcherId, numBalls, numStrikes, numHits
#   Data is from shortPitch df (or a subset of it)
  sdf <- data

  sdf <- subset(sdf, game_id == gid)
  sdf <- subset(sdf, pitcher == pid)
  balls <- nrow(subset(sdf, type == 'B'))
  strikes <- nrow(subset(sdf, type == 'S'))
  hits <- nrow(subset(sdf, type == 'X'))
  inZone <- nrow(subset(sdf, inZone == "TRUE"))
  d <- data.frame(game_id = gid, 
                   pitcher = p1, 
                   numBalls = balls, 
                   numStrikes = strikes, 
                   numHits = hits,
                   numInZone = inZone
                   )
  d <- transform(d, numPitches = numBalls + numStrikes + numHits)
  d <- transform(d, pctStrikes = numStrikes / numPitches,
                 pctHits = numHits / numPitches,
                 pctInZone = numInZone / numPitches,
                 pctHitsInZone = numHits / numInZone
                 )
  return(d)
}

# Creat table of description vs in/out of Strike Zone
### Interesting stuff here to see what the batter did with different types of pitches
d11 <- sqldf("select inZone, des, count(1) from shortPitch group by inZone, des ")
names(d11)[3] = "count"
d11$inZone[is.na(d11$inZone)] <- "Unknown"
xtabs(count ~ des + inZone, data=d11)


# Try running GetPitcherStatsForGame for multiple games and put in a single df
GetPitcherStatsForGame(gid=gid, pid=pid[1,1])
pid <- ngp[1,1]
pid

onePid <- pid
onePid

# Sample call
GetOnePitcherStatsAllGames(onePid)

GetOnePitcherStatsAllGames <- function (pitcherId, data= "atbat09") {
# Will return a data frame containing various pitching statistics for the pitcherId
# Uses the database atbat09 by default
  gamesPitched <- sqldf(paste("select distinct game_id, pitcher from ", data, " where pitcher = ", pitcherId))
  curDf <- data.frame()  
  numGames <- nrow(gamesPitched)
  gameId <- gamesPitched[1,1]
  gameId
  for (g in 1:numGames) {
    curPitchStats <- GetPitcherStatsForGame(gid= gamesPitched[g,1], pid= gamesPitched[g,2])
    if (g == 1) curDf <- curPitchStats else 
      curDf <- rbind(curDf, curPitchStats)
  }
  return(curDf)
}


pid1Stats <- GetOnePitcherStatsAllGames(onePid)
pid
head(ngp)
nrow(ngp)


# Starting pitchers and number of games pitched (ngp)
gamesPitched <- sqldf(paste("select distinct game_id, pitcher from atbat09 where pitcher = ", pid[1,1]))
startingPitchers <- ngp

GetAllPitchersStatsAllGames <- function (startingPitchers, data= "atbat09") {
# WARNING! This will take a while to run
# Loop over all starting pitchers
# GetAllPitchersStatsAllGames will return a data frame consisting of 1 line of stats for each game
# a pitcher pitched in.
# The input is a data frame consisting of pitcherID's in the first column
  numStarters <- nrow(startingPitchers)
  data <- "atbat09"
  curDf <- data.frame()
# I started modifying this. Not sure if it works. Before, it had the same pitcher id for everything
  for (i in 1:numStarters) {
    curPitcherAllGamesStats <- GetOnePitcherStatsAllGames(pitcherId= startingPitchers[i, 1], data)
    if (i == 1) {
      curDf <- curPitcherAllGamesStats }      else
      curDf <- rbind(curDf, curPitcherAllGamesStats)
    print(paste(i, startingPitchers[i, 1]))
  }
  return(curDf)
}
dim(aps)
atest <- aps
dim(aps)    
nrow(aps) <- 5
    
# This took ~47 minutes for 310 pitchers
# Wasted - all pitcher id's came out to 434378 - WHY?
allPitcherStatsAllGames <- GetAllPitchersStatsAllGames(startingPitchers)

# look at startingPitchers
head(startingPitchers)
startingPitchers[3,1]


head(allPitcherStatsAllGames)
dim(allPitcherStatsAllGames)
nrow(uniqueGames)
?print

# Wrote to output file
write.table(allPitcherStatsAllGames, "bvAllPitcherStatsAllGames09.rda")
aps <- read.table("bvAllPitcherStatsAllGames09.rda")
head(aps)
aps.2 <- sqldf("select game_id, pitcher from aps")
head(aps.2)
table(aps.2$pitcher)
rm(allPitcherStatsAllGames)

# Look at some plots
paes <- ggplot(aps, aes(x=numPitches, y=numInZone))
p <- paes + geom_point(); p

summary(aps$numPitches)
acc <- acc[order(acc[,3], decreasing = TRUE), ]

apsP <- aps[order(aps$numPitches, decreasing = TRUE), ]
head(apsP)
p <- qplot(numInZone)

p <- ggplot(aps, aes(y=numPitches))
pg <- p + geom_boxplot(); pg

paes <- ggplot(aps, aes(x= pctInZone, y= pctHits))
p <- paes + geom_point(); p

apsZ <- aps[order(aps$numInZone), ]
head(apsZ, 60)
nrow(subset(aps, pitcher = "434378"))
head(aps)
  
paes <- ggplot(aps, aes(x= pctInZone, y= pctStrikes))
p <- paes + geom_point(); p

paes <- ggplot(aps, aes(x= pctHits, y= numHits))
p <- paes + geom_point(); p
