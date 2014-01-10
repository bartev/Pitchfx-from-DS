# pfxMod12.R
# Look at prop tables of different pitches

# Develop dataframe containing pitch level data to do regressions
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
load('pfxMod10abpb.Rdat')
pd <- p09.m
rm(p09.m)


#
#
# Cool table showing avg speed/deflection/spin/px/pz etc. for each pitch type
ptdf <- sqldf('select pitch_type, count(1) NumPitches, avg(start_speed) startSpeed,
            avg(end_speed) endSpeed, avg(px) pxAvg, avg(pz) pxAvg, 
            avg(spin_dir) spinDirAvg, avg(spin_rate) spinRateAvg, 
            avg(pxScale) pxScaleAvg from pd group by pitch_type')
ptScaleDf <- sqldf('select pitch_type, avg(pzScale) pzScaleAvg from pd
                   where abs(pzScale) < 10 group by pitch_type')
ptdf <- merge(ptdf, ptScaleDf)
rm(ptScaleDf)
ptdf

# Merge at bat info with pitch data
abToMerge <- subset(abpb, select=c(ubnum, event, ClusterK5, gbf, best, bHgt, hgtPit, wgtPit, 
                                   throwsPit, p_throws, stand, pitchBat, pbSame, 
                                   diffPitches, numPitches, pitchChgPct, medianInning,
                                   startPitcher, numInningsPitched,
                                   ER, ERA, numAtBats, totPitches, numSwingStrikes, 
                                   dobPit, agePit))
# pdMerge <- merge(pd, abToMerge)
# pdMerge <- pdMerge[order(pdMerge$seasonPitchNum),]
# save(pdMerge, file='pfxMod12pdMerge.Rdat')
load('pfxMod12pdMerge.Rdat')
head(pdMerge)


#
#
# Prop table of pitch type vs same/different stance for pitcher/batter
tlr <- xtabs(~ pitch_type + pitchBat, data=pdMerge)
ptlr <- prop.table(tlr,2)
tlr2 <- xtabs(~ pitch_type + pbSame, data=pdMerge)
ptlr2 <- prop.table(tlr2,2)
ptlr2
round(ptlr2, 2)

# getOption('digits')
# options(digits=2)
# options(scipen=3)
# options()

#
#
# Prop table of pitch type vs count
tct <- xtabs(~ pitch_type + count, data=pdMerge)
ptct <- prop.table(tct, 2)
tct
ptct
ptct <- round(ptct, 3)

head(pdMerge)
fbdf <- subset(pdMerge, pitch_type=='FF')
fbdf <- subset(fbdf, abs(pzScale) < 10)

# try the model on 60% of the data
trainIndices <- sample(1:nrow(fbdf), 0.6 * nrow(fbdf), replace=FALSE)
testIndices <- which(! 1:nrow(fbdf) %in% trainIndices)
fbdf$trainSet <- 'test'
fbdf[trainIndices, 'trainSet'] <- 'train'
fbTrain <- subset(fbdf, trainSet=='train')
fbTest <- subset(fbdf, trainSet != 'train')
save(fbdf, fbTrain, fbTest, trainIndices, file='pfxMod12regressionData')

fit <- glm(best ~ pxScale + pzScale + start_speed + end_speed, data=fbTrain, family=binomial)
#   tp    fn fp     tn    p       r fmeas lift
# 1 11 21086  1 115757 0.92 0.00052 0.001  5.9
fit <- glm(best ~ pxScale + pzScale + log(start_speed) + log(end_speed) + inZone,
           data=fbTrain, family=binomial)
#   tp    fn fp     tn p       r   fmeas lift
# 1  4 21093  0 115758 1 0.00019 0.00038  6.5
fit <- glm(best ~ pxScale + pzScale + log(start_speed) + log(end_speed),
           data=fbTrain, family=binomial)
#   tp    fn fp     tn p       r   fmeas lift
# 1  5 21092  0 115758 1 0.00024 0.00047  6.5

fit <- glm(best ~ pxScale + pzScale + start_speed + end_speed +
  log(spin_dir) + log(spin_rate), data=fbTrain, family=binomial)
#   tp    fn fp     tn    p       r  fmeas lift
# 1 16 21081  2 115756 0.89 0.00076 0.0015  5.8

l <- GetLift(fbTest, fit); l
fit
summary(fit)

library(randomForest)
randFor <- randomForest(best ~ ., data=fbTrain, importance=TRUE)

