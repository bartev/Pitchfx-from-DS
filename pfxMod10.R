# Model 10
# Develop dataframe containing at bat level data to do regressions
# abpb (atbat and pitcher/batter info)
# pitchPerBatter - how many pitches/types of pitches per at bat
# pitcherdf10 - more pitcher specific columns
# allPitch - each pitch, featurues to look at


rm(list=ls())
library(ggplot2)
library(sqldf)

setwd("~/Rmac/Pitchfx/PitchFX")
load('pfxData_p09.Rdat')    # Full monty '09 data, .m, .s, .z
load('pfxData_atbatMod.Rdat') # 5000 row and all rows of modified atbat data
ab <- subset(ab09.m, select=c(game_id, ubnum, half, inning, num, b, s, o, score, batter, b_height, pitcher, p_throws, stand,
                              event, htRuns, atRuns, homeScore, awayScore, runsOnPitcher, pAhead))
rm(p09.s, p09.z, atbat.m, ab09.m)
load('pfxMod3pitcherdf.Rdat') # stats summed up by pitcher
load("players09.Rda")


# convert height to inches
HeightToInches <- function(h) {
  h <- as.character(h)
  h <- strsplit(h, '-') # strsplit acts on the whole list, creates list of vectors
  h <- do.call(rbind, h) # convert to matrix
  #   height <- 12*feet + inches
  height <- as.numeric(h[,1]) * 12 + as.numeric(h[,2]) # h is now a matrix
  return(height)  
}

#
# Convert heights to inches
pls <- players09
pls$hgt <- HeightToInches(pls$height)

# Pitcher throws and batter stands (R R, R L, L R, L L)
ab$bHgt <- HeightToInches(ab$b_height)
ab <- transform(ab, pitchBat=paste(p_throws, stand), pbSame=(p_throws==stand))
head(ab)

#
# Get player age (in days)
startDate <- as.Date('2009-05-01')
pls$bdate <- as.Date(pls$dob, '%m/%d/%Y')
pls <- transform(pls, age=as.numeric(startDate - bdate))

# Clean up dataset - 
# Lance Loftin - no height/weight info - Toronto.
badId <- 449908

#
#
# Strike zone data missing or bad
p09 <- p09.m
# Get these numbers from plots of sz_top & sz_bot in PfxLoad09Data.R
missingTops <- sqldf('select batter, avg(sz_top) szTopAvg, median(sz_top) szTopMed from p09 where (sz_top < 5.0) & (sz_top > 2.0) group by batter')
missingBots <- sqldf('select batter, avg(sz_bot) szBotAvg, median(sz_bot) szBotMed from p09 where (sz_bot < 2.5) & (sz_bot > 1.0) group by batter')
batters <- sqldf('select distinct batter from p09') # some batters are also pitchers
ps <- sqldf('select distinct pitcher from p09')
batters <- merge(batters, missingTops)
batters <- merge(batters, missingBots)
medSzDim <- subset(batters, select=c(batter, szBotMed, szTopMed)) # median strike zone dimensions


# add more pitcher specific info
pl <- subset(pls, type=='pitcher' & id!= badId, select=c(id, hgt, weight, bats, throws, bdate, age))
names(pl) <- c('pitcher', 'hgtPit', 'wgtPit', 'batsPit', 'throwsPit', 'dobPit', 'agePit')
batterInfo <- subset(pls, select=c(id, hgt, weight, bats, throws, bdate, age, pos))
names(batterInfo) <- c('batter', 'hgtBat', 'wgtBat', 'batsBat', 'throwsBat', 'dobBat', 'ageBat', 'posBat')
biSz <- merge(batterInfo, medSzDim) # add median strike zone info to list of batters

pitcherdf10 <- merge(pitcherdf, pl)
head(pitcherdf10)

save(pitcherdf10, biSz, file='pfxMod10pitcherdf10.Rdat')

# get subset of p09 data
allPitch <- subset(p09, , select=c(pitcher, ubnum, game_id, id, seasonPitchNum, num, batter, b, s, count, des, 
                                     type, Ball, Strike, Hit,
                                     on_1b, on_2b, on_3b, start_speed, end_speed, 
                                     px, pz, sz_top, sz_bot, 
                                     pxScale, pzScale, inZone, pitchNum, finalPitch, pitch_type,
                                     spin_dir, spin_rate, 
                                     pfx_x, pfx_z, x0, y0, z0, vx0, vy0, vz0, ax, ay, az,
                                     team, League, Division, home))



# pitchDataToMerge <- subset(pitcherdf10, , select=c(pitcher, medianInning, startPitcher, gbf, best, hgt, wgt))
# allPitch <- merge(allPitch, pitcherdf10)

# 
#           
#                            
#                            
#                            
#                            
#                            
#                            
# # How many runs per game for each pitcher?
# df <- sqldf('select pitcher, game_id, half, sum(runsOnPitcher) ER from ab group by pitcher, game_id')
# # head(df, 20)
# # head(ab)
# 
# # find who pitched in what inning
# up <- sqldf('select pitcher, game_id, count(distinct inning) Innings from ab group by pitcher, game_id')
# # head(up)
# 
# pg <- merge(df, up) # pg = Pitcher-Game info
# pg <- transform(pg, gERA=9*ER/Innings)
# 
# # How many batters?
# df <- sqldf('select pitcher, game_id, count(distinct batter) numAtBats from ab group by pitcher, game_id')
# pg <- merge(pg, df)
# 
# # df <- sqldf('select pitcher, game_id, count(1) swingStrike where 
# 
# 
# pg$pitcher <- as.factor(pg$pitcher)
# head(pg)
# #
# #
# #
# # More stuff to add
# # How many batters did each pitcher pitch to?
# up <- subset(ab, , select=c(pitcher, game_id, inning, batter))
# t <- with(up, table(pitcher))
# # t <- table(up$pitcher) # use with(up, table(pitcher)) - keeps label 'pitcher' on 1st column
# df <- as.data.frame(t)
# names(df)[2] <- 'numAtBats'
# head(df)
# sum(df$numAtBats)


# Batter level. How many different pitches per batter?
# How many pitches per batter?
# Location of pitch
# strike vs pitch type


#
#
#
# Finds count(unique pitches per batter), numPitchesPerBatter
# Some pitch_types are unknown. Label them as such to keep number of pitch types at least 1
pitchPerBatter <- subset(allPitch, , select=c(pitcher, ubnum, game_id, batter, num, seasonPitchNum, pitchNum, pitch_type))
pitchPerBatter <- transform(pitchPerBatter, pitch_type=ifelse(is.na(pitch_type), 'Unknown', levels(pitch_type)[pitch_type]))

# ppb <- sqldf('select pitcher, game_id, batter, num, seasonPitchNum, count(distinct pitch_type) diffPitches, max(pitchNum) numPitches from pitchPerBatter group by pitcher, game_id, num')
ppb <- sqldf('select pitcher, ubnum, game_id, batter, seasonPitchNum, count(distinct pitch_type) diffPitches, max(pitchNum) numPitches from pitchPerBatter group by ubnum')
ppb <- ppb[order(ppb$seasonPitchNum),]
ppb <- transform(ppb, pitchChgPct=diffPitches / numPitches)
head(ppb)

#
#
# Merge at bat level information
tomerge <- subset(ppb, select=c(ubnum, diffPitches, numPitches, pitchChgPct))
abpb <- merge(ab, tomerge)
head(abpb)
summary(abpb)
# atBatPitchData <- subset(abpb, select=c(pitcher, game_id, ubnum, inning, num, b, s, o, score, event, htRuns, atRuns, homeScore, awayScore,
#                                         runsOnPitcher, pAhead, batter, bHght, ))
abpb <- merge(abpb, pitcherdf10)
abpb <- subset(abpb, select=c(game_id, ubnum, pitcher, batter, inning, half, num, b, s, o, event, homeScore, awayScore, htRuns, atRuns, score,
                                runsOnPitcher, pAhead,
                                ClusterK5, gbf, best, 
                                bHgt, hgtPit, wgtPit, throwsPit, p_throws, stand, pitchBat, pbSame,
                                diffPitches, numPitches, pitchChgPct,
                                medianInning, startPitcher, numInningsPitched, ER, ERA, numAtBats, totPitches, numSwingStrikes, pctSwingStrike,
                                dobPit, agePit))
abpb <- abpb[do.call(order, list(abpb$game_id, abpb$num)),]
head(abpb)

save(abpb, file='pfxMod10abpb.Rdat')

# 
# 
# Misc
# Ways to exclude columns by name
# df[, names(df) != "a"]
# Or, BTW, you can use within()
# aq <- within(airquality, rm(Day))

