# Model 9 - put on hold temporarily to look at batter level data.
# Look at ERA on a per game basis
library(ggplot2)
library(sqldf)

setwd("~/Rmac/Pitchfx/PitchFX")
load('pfxData_atbatMod.Rdat') # 5000 row and all rows of modified atbat data
load('pfxData_p09.Rdat')    # Full monty '09 data, .m, .s, .z
ab <- ab09.m
rm(p09.s, p09.z, atbat.m, ab09.m)

# How many runs per game for each pitcher?
df <- sqldf('select pitcher, game_id, half, sum(runsOnPitcher) ER from ab group by pitcher, game_id')
# head(df, 20)
# head(ab)

# find who pitched in what inning
up <- sqldf('select pitcher, game_id, count(distinct inning) Innings from ab group by pitcher, game_id')
# head(up)

pg <- merge(df, up) # pg = Pitcher-Game info
pg <- transform(pg, gERA=9*ER/Innings)

# How many batters?
df <- sqldf('select pitcher, game_id, count(distinct batter) numAtBats from ab group by pitcher, game_id')
pg <- merge(pg, df)

# df <- sqldf('select pitcher, game_id, count(1) swingStrike where 


pg$pitcher <- as.factor(pg$pitcher)
head(pg)
#
#
#
# More stuff to add
# How many batters did each pitcher pitch to?
up <- subset(ab, , select=c(pitcher, game_id, inning, batter))
t <- with(up, table(pitcher))
# t <- table(up$pitcher) # use with(up, table(pitcher)) - keeps label 'pitcher' on 1st column
df <- as.data.frame(t)
names(df)[2] <- 'numAtBats'
head(df)
sum(df$numAtBats)
