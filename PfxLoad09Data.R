## Bartev Vartanian
## Start here to load Pitchfx data from 09 into local variables
## If using another year's data, functions will refer to the yearless variables
## used here.
## atbat
## hitchart
## pitch
## players

## Removed
## Teamnames
## above files referring to 2009 data

# load pitch data environment (I downloaded to local drive)
setwd("~/Documents/Pitchfx")
library(sqldf)

# start fresh
rm(list = ls())


## Load Pitchfx data.
## Start here, then copy subsets for variables:
# atbat  <-  atbat09
# hitchart <- hitchart09
# pitch <- pitch09
# players <- players09


# Shows all players' personal stats (name, DOB, size, etc)
load("players09.Rda")
# head(players09)

#     team     id pos    type first_name last_name jersey_number height weight bats throws        dob
# 908  tor 449908   P pitcher      Lance    Loftin          <NA>    0-0      0    R      R 03/03/1986
# 
#     team     id pos   type first_name last_name jersey_number height weight bats throws        dob
# 1380  sdn 474384   P batter     Nathan   Freiman          <NA>    6-7    225 <NA>   <NA> 12/31/1986
# 1390  sdn 475340   C batter    Griffin  Benedict          <NA>    6-0    185 <NA>   <NA> 07/04/1987
# 1393  nyn 475431  OF batter     Joseph    August          <NA>    6-1    190 <NA>   <NA> 09/23/1986
# 1405  sln 476594   C batter     Robert     Stock          <NA>    6-0    175 <NA>   <NA> 11/21/1989
# 
#      team     id pos   type first_name last_name jersey_number height weight bats throws        dob
# 1380  sdn 474384   P batter     Nathan   Freiman          <NA>    6-7    225 <NA>   <NA> 12/31/1986
# 1390  sdn 475340   C batter    Griffin  Benedict          <NA>    6-0    185 <NA>   <NA> 07/04/1987
# 1393  nyn 475431  OF batter     Joseph    August          <NA>    6-1    190 <NA>   <NA> 09/23/1986
# 1405  sln 476594   C batter     Robert     Stock          <NA>    6-0    175 <NA>   <NA> 11/21/1989
# 1722  oak 572836   P batter       Joel   Eusebio          <NA>    5-8    175    R   <NA> 02/01/1985
# 1805  col 573241   C batter    Brandon    Whitby          <NA>    6-2    205    R   <NA> 01/09/1987


# List of team names, League, Division, Pfx abbreviation, MLB abbreviation, city/state, ballpark name
teamNames <- read.csv("TeamNames.csv")
teamNamesShort <- teamNames[, c("League", "Division", "PfxTeam", "TeamName")]
# Equivalent method using sqldf
# teamNamesShortSql <- sqldf("select League, Division, PfxTeam, TeamName from teamNames")

# Add League, Division, TeamName to players
# colNames <- c("team", "id", "League")
players09 <- sqldf("select * from players09, teamNamesShort where team = PfxTeam")
rm (teamNames, teamNamesShort)



# Not sure what this is. only 2434 lines
load("hitchart09.Rda")
# head(hitchart09)

# Description of each pitch result (187,553 lines)
# event field
load("atbat09.Rda")

# head(atbat09)

# Convert atbat$score to 1 = TRUE, 0 = FALSE/NA
# To convert to TRUE/FALSE, omit the as.numeric part
atbat09 <- transform(atbat09, score = as.numeric(!is.na(score)))
atbat09 <- subset(atbat09, game_id != '2009/08/08/flomlb-phimlb-1') # why remove this? (see below. error for count info in this game)
# toRemove <- subset(atbat09, game_id == '2009/08/08/flomlb-phimlb-1')


# Load the 'pitch09' data - big file, ~ 45MB
# Pitch data from each game - 717,409 lines!
load("pitch09.Rda")
# Clean the data - this game did not reset the count to 0-0 for each batter. b & s columns are all off
# ? looks ok
# Bad data starting ~line 300 in this game until almost the end
pitch09 <- subset(pitch09, game_id != '2009/08/08/flomlb-phimlb-1')
# toRemove <- subset(pitch09, game_id == '2009/08/08/flomlb-phimlb-1')
pitch09$seasonPitchNum <- 1:nrow(pitch09)
# head(pitch09)

#
#
# Strike zone data missing or bad
missingTops <- sqldf('select batter, avg(sz_top) szTopAvg, median(sz_top) szTopMed from pitch09 where (sz_top < 5.0) & (sz_top > 2.0) group by batter')
missingBots <- sqldf('select batter, avg(sz_bot) szBotAvg, median(sz_bot) szBotMed from pitch09 where (sz_bot < 2.5) & (sz_bot > 1.0) group by batter')
batters <- sqldf('select distinct batter from pitch09')

batSz <- subset(pitch09, , select=c(batter, sz_top, sz_bot))
batSz <- merge(batSz, missingTops)
batSz <- merge(batSz, missingBots)
# Get these numbers from plots below of sz_top & sz_bot
batSz <- transform(batSz, szTop=ifelse(sz_top > 5.0 | sz_top < 2.0 | is.na(sz_top), szTopAvg, sz_top), 
                   szBot=(ifelse(sz_bot > 2.5 | sz_bot < 1.0 | is.na(sz_bot), szBotAvg, sz_bot)))
batSz <- transform(batSz, dBot=szBot - sz_bot, dTop=szTop - sz_top)
#
#
#
# Create plots of strike zone to see where outliers are
# library(ggplot2)
# gtop1 <- ggplot(batSz[order(batSz$sz_top),], aes(x=1:nrow(batSz), y=sz_top)) + geom_point(alpha=I(0.5), colour='blue')
# gtop2 <- ggplot(batSz, aes(x=1:nrow(batSz), y=sz_top)) + geom_point(alpha=I(0.5), colour='blue')
# gbot1 <- ggplot(batSz[order(batSz$sz_bot),], aes(x=1:nrow(batSz), y=sz_bot)) + geom_point(alpha=I(0.5), colour='red') + geom_hline(yintercept = c(0.5, 2.5))
# gbot2 <- ggplot(batSz, aes(x=1:nrow(batSz), y=sz_bot)) + geom_point(alpha=I(0.5), colour='red')
# gtop1
# gtop2
# gbot1
# gbot2

# Add team, League, Division to the pitch09 data
# May not be very good because players may have been traded
pltemp <- subset(players09, , select=c(id, team, League, Division))
pitch09 <- merge(pitch09, pltemp, by.x='pitcher', by.y='id')
rm(pltemp)

# Use atbat to find home/away
# Couldn't use player data because 44k rows were neither home nor away - perhaps
# due to trading players midseason
up <- unique(subset(atbat09, , select=c(game_id, half, pitcher)))
up$home <- up$half == 'top'
pitch09 <- merge(pitch09, up)
pitch09 <- pitch09[order(pitch09$seasonPitchNum),]
rm(up)

save(atbat09, hitchart09, pitch09, players09, file = "pfxData09.Rdat")

# Use a subset while testing stuff
atbat  <-  atbat09[1:5000, ]
hitchart <- hitchart09
pitch <- pitch09[1:5000, ]
players <- players09

ab09 <- atbat09
hc09 <- hitchart09 
p09 <- pitch09
pl09 <- players09


# Clean up - we don't need these now.

rm(atbat09)
rm(hitchart09)
rm(pitch09)
rm(players09)

save(atbat, hitchart, pitch, players, file = "pfxDataSample.Rdat")
save(ab09, hc09, p09, pl09, file = "pfxDataWorkingCopy09.Rdat")