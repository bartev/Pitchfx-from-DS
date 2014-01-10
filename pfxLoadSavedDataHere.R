# Bartev Vartanian
# 10/17/11

# The place to go to just load pre-processed data
# Load stuff here
getwd()
setwd("~/Rmac/Pitchfx/PitchFX")
rm(list = ls())

# From PfxLoad09Data.R
load("pfxData09.Rdat")      # pitchfx '09 data - full monty
load("pfxDataWorkingCopy09.Rdat")      # pitchfx '09 data - full monty
load("pfxDataSample.Rdat")  # pitchfx '09 data - 5000 row max

# From pfxStartHere.R
# Pitch data after processed by 'ModifyPitchCols' (in 'pfxFuncModifyPitchCols.R')
load('pfxData_pitch.Rdat')  # 5000 row max, pitch.m (modified), .s (smaller), .z (strike zone)
load('pfxData_p09.Rdat')    # Full monty '09 data, .m, .s, .z

load('pfxData_atbatMod.Rdat') # 5000 row and all rows of modified atbat data

# From pfxMod1.R
load('pfxMod1.Rdat') # SwingingStrikes ~ TotalPitches

# From pfxMod3.R
load('pfxMod3.Rdat') # ERA, starting/relief, etc.
load('pfxMod3pitcherdf.Rdat') # stats summed up by pitcher
# from pfxMod4.R
load('pfxMod4.Rdat') # df with pitchNum, type(B/S/X), Freq(for given pitchNum), Start/Relief, also eraDf

# From pfxMod7.R
load('pfxMod7PitcherSummary.Rdat')


# from pfxMod10.R
load('pfxMod10pitcherdf10.Rdat') # df with pitcher specific data. (more than from model3)
load('pfxMod10abpb.Rdat') # df with at bat level data to do regressions on. Includes rankings of pitchers, num different pitches for each at bat, pitcher & batter stats


# From pfxMod12.R
load('pfxMod12pdMerge.Rdat')
load('pfxMod12regressionData')

# from pfxMod13-Ariel.R
# save(sdf, tdf, tdfPerInning, file='pfxMod13StatsPerInning.Rdat')
load('pfxMod13StatsPerInning.Rdat')
# save(toRegress, ppiTrain, ppiTest, trainIndices, file='pfxMod13regressionData')
# save(scaling, scaleParam, file='pfxMod13scaling.Rdat')
load('pfxMod13scaling.Rdat')  # scaling parameters (see bvRaysUtilityFunctions for functions to scale/unscale data)


# Incorporated in pfxStartHere, and vectorized.
# Used in pfxModifyAtbatData.R
# library(sqldf)
# source("pfxCumRunsFunction.R")

# From pfxModifyAtbatData.R
# load("pfxData-plateAppearance.Rdat")


