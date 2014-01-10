# pfxMod14
# look at a df of just strikes?
# Add columns for change in speed, position, angle, spin_dir, spin_rate, location
# Does varying speed/position affect outcome?
setwd("~/Rmac/Pitchfx/PitchFX")
rm(list=ls())

library(MASS) # for stepAIC

# load('pfxData_p09.Rdat')    # Full monty '09 data, .m, .s, .z
load('pfxMod12pdMerge.Rdat')
load('pfxMod14toRegress.Rdat')  #from later in this file

# pitch data characteristics
pdChar <- subset(pdMerge, select=c(ubnum, pitcher, seasonPitchNum, game_id, num, batter, b, s, des, id, type, start_speed, end_speed,
                                   sz_top, sz_bot, px, pz, x0, y0, z0, vx0, vy0, vz0, ax, ay, az, break_y, break_angle, break_length,
                                   pitch_type, spin_dir, spin_rate, League, home, count, pitchNum, finalPitch,
                                   pxScale, pzScale, inZone, event, bHgt, throwsPit, stand, pitchBat, pbSame,
                                   diffPitches, numPitches, pitchChgPct, startPitcher, numInningsPitched, agePit))
rm(pdMerge)


# data$finalPitch[c(diff(data$pitchNum), -1) < 0] = TRUE

pdChar <- pdChar[order(pdChar$seasonPitchNum),]
pd <- pdChar
head(pd)


#
#
#
# Function to get a vector containing the current row value minus the previous row's value
CurMinusPrev <- function(x) {x - c(0,  x[1:(length(x) - 1) ]) }
# df <- df[order(df$seasonPitchNum, decreasing=TRUE),]
# df$dd2 <- c(-diff(df$pxScale), 0)
# df <- df[order(df$seasonPitchNum),]


# Look at the current value - the last value
# Later filter out pitchNum==1
pd <- transform(pd, ppx=(stand=='R') * px - (1-(stand=='R'))*px)
pd <- transform(pd, ppxScale=(stand=='R') * pxScale - (1-(stand=='R'))*pxScale)
# pd <- transform(pd, dppx=CurMinusPrev(ppx))
pd <- transform(pd, dend_speed=CurMinusPrev(end_speed), dppx=CurMinusPrev(ppx), dpz=CurMinusPrev(pz), dbreak_y=CurMinusPrev(break_y), dbreak_angle=CurMinusPrev(break_angle),
                dbreak_length=CurMinusPrev(break_length), dspin_dir=CurMinusPrev(spin_dir), dspin_rate=CurMinusPrev(spin_rate),
                dpxScale=CurMinusPrev(pxScale), dpzScale=CurMinusPrev(pzScale))
head(pd)

pd <- transform(pd, y=type=='S')


# try the model on 60% of the data
toRegress <- pd

#
#
# Clean up strikezone data
toReg <- subset(toRegress, (sz_bot >= 0.5) & (sz_bot <= 2.5) & (sz_top >= 2.0) & (sz_top <= 5) & (sz_bot < sz_top))
# toRegress <- transform(toRegress, ppxScale=(stand=='R') * pxScale - (1-(stand=='R'))*pxScale)
# toRegress <- transform(toRegress, dppxScale=CurMinusPrev(ppxScale))

rm(pd)
trainIndices <- sample(1:nrow(toReg), 0.6 * nrow(toReg), replace=FALSE)
testIndices <- which(! 1:nrow(toReg) %in% trainIndices)
toReg$trainSet <- 'test'
toReg[trainIndices, 'trainSet'] <- 'train'
# pdTrain <- subset(toRegress, trainSet=='train')
# pdTest <- subset(toRegress, trainSet != 'train')


source('pfxFuncConfusionMat.R')
save(toRegress, toReg, file='pfxMod14toRegress.Rdat')
rm(toRegress)
glm1 <- glm(y ~ end_speed + ppxScale + pzScale + spin_dir + spin_rate, data=subset(toReg, trainSet=='train'), family='binomial')

toReg <- subset(toReg, select=c(b, s, type, end_speed, break_y, break_angle, break_length, spin_dir, spin_rate, pitchNum,
                                 finalPitch, ppxScale, pzScale, pitchBat, startPitcher,
                                 dend_speed, dbreak_y, dbreak_angle, dbreak_length, dspin_dir, dspin_rate, dpxScale, dpzScale, y,
                                 trainSet))
#
# With bad pz's in the data
#
# lift.train
#      tp     fn    fp     tn         p          r     fmeas      lift
# 1 12030 173388 32304 207040 0.2713493 0.06488043 0.1047216 0.6216164
# > lift.test
#     tp     fn    fp     tn         p          r    fmeas      lift
# 1 7993 115347 21589 138348 0.2701981 0.06480461 0.104537 0.6205684

# Clean up sz_top/sz_bot data
#
# > lift.train
#      tp     fn    fp     tn         p          r     fmeas      lift
# 1 13112 171902 33482 205835 0.2814096 0.07087031 0.1132258 0.6454151
# > lift.test
#     tp     fn    fp     tn         p          r     fmeas      lift
# 1 8795 114518 22329 137246 0.2825794 0.07132257 0.1138976 0.6482553
glm1
summary(glm1)

GetConfusionMatrix2(toReg, glm1)
lift.train <- GetLift2(subset(toReg, trainSet=='train'), glm1)
lift.test <- GetLift2(subset(toReg, trainSet!='train'), glm1)
lift.train
lift.test

# need to clean up data for srike zone top/bottom in order to use pzScale
f2 <- formula(y ~ end_speed + ppxScale + pzScale + spin_dir + spin_rate + 
  end_speed:ppxScale + end_speed:pzScale + end_speed:spin_dir + end_speed:spin_rate +
  ppxScale:pzScale + ppxScale:spin_dir + ppxScale:spin_rate +
  pzScale:spin_dir + pzScale:spin_rate +
  spin_dir:spin_rate)
# > lift2.train
#      tp     fn    fp     tn    p          r     fmeas      lift
# 1 17143 167871 31837 207480 0.35 0.09265785 0.1465251 0.8027276
# > lift2.test
#      tp     fn    fp     tn         p          r     fmeas      lift
# 1 11566 111747 21258 138317 0.3523641 0.09379384 0.1481519 0.8083461

glm2 <- glm(f2, data=subset(toReg, trainSet=='train'), family='binomial')
summary(glm2)
lift2.train <- GetLift2(subset(toReg, trainSet=='train'), glm2)  # should get lift on test data, just looking to see how well it trained
lift2.test <- GetLift2(subset(toReg, trainSet!='train'), glm2)
lift2.train
lift2.test

f3 <- formula(y ~ end_speed + ppxScale + pzScale + spin_dir + spin_rate + 
  end_speed:ppxScale + end_speed:pzScale + end_speed:spin_dir + end_speed:spin_rate +
  ppxScale:pzScale + ppxScale:spin_dir + ppxScale:spin_rate +
  pzScale:spin_dir + pzScale:spin_rate +
  spin_dir:spin_rate +
  I(end_speed^2) + I(ppxScale^2) + I(pzScale^2) + I(spin_dir^2) + I(spin_rate^2))
# remove bad pzScale data
# ppxScale is negative for LH batters
# > lift3.train
#       tp    fn    fp     tn         p         r     fmeas    lift
# 1 129459 55555 77026 162291 0.6269656 0.6997254 0.6613503 1.43795
# > lift3.test
#      tp    fn    fp     tn         p         r     fmeas     lift
# 1 86441 36872 51256 108319 0.6277624 0.7009885 0.6623578 1.440128


glm3 <- glm(
  y ~ end_speed + ppxScale + pzScale + spin_dir + spin_rate + 
  end_speed:ppxScale + end_speed:pzScale + end_speed:spin_dir + end_speed:spin_rate +
  ppxScale:pzScale + ppxScale:spin_dir + ppxScale:spin_rate +
  pzScale:spin_dir + pzScale:spin_rate +
  I(end_speed^2) + I(ppxScale^2) + I(pzScale^2) + I(spin_dir^2) + I(spin_rate^2)
  , data=subset(toReg, trainSet=='train'), family='binomial')
summary(glm3)
#       tp    fn    fp     tn         p       r     fmeas     lift
# 1 129458 55556 77041 162276 0.6269183 0.69972 0.6613216 1.437842
# > lift3.test
#      tp    fn    fp     tn        p         r     fmeas     lift
# 1 86420 36893 51275 108300 0.627619 0.7008182 0.6622019 1.439799

glm3 <- glm(
  y ~ end_speed + ppxScale + pzScale + spin_dir + spin_rate + 
  end_speed:ppxScale + end_speed:pzScale + end_speed:spin_dir + end_speed:spin_rate +
  ppxScale:pzScale + ppxScale:spin_dir + ppxScale:spin_rate +
  pzScale:spin_rate +
  I(end_speed^2) + I(ppxScale^2) + I(pzScale^2) + I(spin_dir^2) + I(spin_rate^2)
  , data=subset(toReg, trainSet=='train'), family='binomial')
summary(glm3)
# lift3.train
#       tp    fn    fp     tn         p         r     fmeas     lift
# 1 129465 55549 77028 162289 0.6269704 0.6997579 0.6613675 1.437961
# > lift3.test
#      tp    fn    fp     tn         p         r     fmeas     lift
# 1 86404 36909 51263 108312 0.6276304 0.7006885 0.6621504 1.439825

glm3 <- glm(
  y ~ end_speed + ppxScale + pzScale + spin_dir + spin_rate + 
  end_speed:ppxScale + end_speed:pzScale + end_speed:spin_dir + end_speed:spin_rate +
  ppxScale:pzScale + ppxScale:spin_rate +
  pzScale:spin_rate +
  I(end_speed^2) + I(ppxScale^2) + I(pzScale^2) + I(spin_dir^2) + I(spin_rate^2)
  , data=subset(toReg, trainSet=='train'), family='binomial')
summary(glm3)
# lift3.train
#       tp    fn    fp     tn         p         r     fmeas     lift
# 1 129462 55552 77004 162313 0.6270379 0.6997416 0.6613978 1.438116
# > lift3.test
#      tp    fn    fp     tn         p         r     fmeas     lift
# 1 86432 36881 51269 108306 0.6276788 0.7009156 0.6622787 1.439936

glm3 <- glm(
  y ~ end_speed + ppxScale + pzScale + spin_rate + 
  end_speed:ppxScale + end_speed:pzScale + end_speed:spin_dir + end_speed:spin_rate +
  ppxScale:pzScale + ppxScale:spin_rate +
  pzScale:spin_rate +
  I(end_speed^2) + I(ppxScale^2) + I(pzScale^2) + I(spin_dir^2) + I(spin_rate^2)
  , data=subset(toReg, trainSet=='train'), family='binomial')
summary(glm3)
# lift3.train
#       tp    fn    fp     tn         p         r     fmeas     lift
# 1 129457 55557 76994 162323 0.6270592 0.6997146 0.6613976 1.438165
# > lift3.test
#      tp    fn    fp     tn        p         r     fmeas     lift
# 1 86431 36882 51262 108313 0.627708 0.7009074 0.6622913 1.440003

lift3.train <- GetLift2(subset(toReg, trainSet=='train'), glm3)  # should get lift on test data, just looking to see how well it trained
lift3.test <- GetLift2(subset(toReg, trainSet!='train'), glm3)
lift3.train
lift3.test

step <- stepAIC(glm3, direction='both')
step$anova # display results



#
#
#
# Add difference from previous pitch
# Don't use 1st pitch to a batter
toRegDiff <- subset(toReg, pitchNum > 1)

glm4 <- glm(
  y ~ end_speed + dend_speed + I(end_speed^2) + I(dend_speed^2) +
    ppxScale + dppxScale + I(ppxScale^2) + I(dppxScale^2) +
    pzScale + dpzScale + I(pzScale^2) + I(dpzScale^2) +
    spin_dir + dspin_dir + I(spin_dir^2) + I(dspin_dir^2) +
    spin_rate + dspin_rate + I(spin_rate^2) + I(dspin_rate^2) +
    break_angle + dbreak_angle + I(break_angle^2) + I(dbreak_angle^2) +
    break_length + dbreak_length + I(break_length^2) + I(dbreak_length^2),
  data=toRegDiff, family='binomial')

toRegDiff2 <- subset(toRegDiff, select=c(y, seasonPitchNum,
                                         end_speed, dend_speed,
                                         ppxScale, dppxScale,
                                         pzScale, dpzScale,
                                         spin_dir, dspin_dir,
                                         spin_rate, dspin_rate,
                                         break_angle, dbreak_angle,
                                         break_length, dbreak_length))
toRegDiff3 <- subset(toRegDiff2, !is.na(dend_speed))
toRegDiff3 <- toRegDiff3[order(toRegDiff3$seasonPitchNum),]
toRegDiff3$dpzScale <- CurMinusPrev(toRegDiff3$pzScale)
summary(toRegDiff3)

trainIndices <- sample(1:nrow(toRegDiff3), 0.6 * nrow(toRegDiff3), replace=FALSE)
testIndices <- which(! 1:nrow(toRegDiff3) %in% trainIndices)
toRegDiff3$trainSet <- 'test'
toRegDiff3[trainIndices, 'trainSet'] <- 'train'
save(toRegDiff3, file='pfxMod14toRegDiff3.Rdat')


glm4 <- glm(
  y ~ end_speed + dend_speed + I(end_speed^2) + I(dend_speed^2) +
    ppxScale + dppxScale + I(ppxScale^2) + I(dppxScale^2) +
    pzScale + dpzScale + I(pzScale^2) + I(dpzScale^2) +
    spin_dir + dspin_dir + I(spin_dir^2) + I(dspin_dir^2) +
    spin_rate + dspin_rate + I(spin_rate^2) + I(dspin_rate^2) +
    break_angle + dbreak_angle + I(break_angle^2) + I(dbreak_angle^2) +
    break_length + dbreak_length + I(break_length^2) + I(dbreak_length^2),
  data=subset(toRegDiff3, trainSet=='train'), family='binomial')
# > lift4.train
#      tp    fn    fp     tn         p         r     fmeas     lift
# 1 82799 49285 57186 122797 0.5914848 0.6268662 0.6086618 1.397466
# > lift4.test
#      tp    fn    fp    tn         p         r     fmeas     lift
# 1 55259 32837 37926 82024 0.5930032 0.6272589 0.6096502 1.400426

glm4 <- glm(
  y ~ end_speed + dend_speed + abs(dend_speed) + I(end_speed^2) + I(dend_speed^2) +
    ppxScale + dppxScale + abs(dppxScale) + I(ppxScale^2) + I(dppxScale^2) +
    pzScale + dpzScale + abs(dpzScale) + I(pzScale^2) + I(dpzScale^2) +
    spin_dir + dspin_dir + abs(dspin_dir) + I(spin_dir^2) + I(dspin_dir^2) +
    spin_rate + dspin_rate + abs(dspin_rate) + I(spin_rate^2) + I(dspin_rate^2) +
    break_angle + dbreak_angle + abs(dbreak_angle) + I(break_angle^2) + I(dbreak_angle^2) +
    break_length + dbreak_length + abs(dbreak_length) + I(break_length^2) + I(dbreak_length^2),
  data=subset(toRegDiff3, trainSet=='train'), family='binomial')
# > lift4.train
#      tp    fn    fp     tn         p         r     fmeas    lift
# 1 82720 49364 57130 122853 0.5914909 0.6262681 0.6083829 1.39748
# > lift4.test
#      tp    fn    fp    tn        p         r     fmeas     lift
# 1 55256 32840 37907 82043 0.593111 0.6272248 0.6096911 1.400681
#
# p went down here. Added more features (differences), but took out interaction terms.
# Also, took out all first pitches.


summary(glm4)
lift4.train <- GetLift2(subset(toRegDiff3, trainSet=='train'), glm4)
lift4.test <- GetLift2(subset(toRegDiff3, trainSet!='train'), glm4)
lift4.train
lift4.test