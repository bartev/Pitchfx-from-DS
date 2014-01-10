# Bartev Vartanian
# 10/24/11
# 
# Start here to load data that has been modified.

######
# Start with a clean slate
getwd()
setwd("~/Rmac/Pitchfx/PitchFX")
rm(list = ls())

# Load all '09 data
# This takes a while (1-2 min). Can load results instead (below)
# source('PfxLoad09Data.R')

# load("pfxData09.Rdat")      # pitchfx '09 data - full monty
load("pfxDataWorkingCopy09.Rdat")      # pitchfx '09 data - full monty
load("pfxDataSample.Rdat")  # pitchfx '09 data - 5000 row max

######
# Modify the pitch columns.
source('pfxFuncModifyPitchCols.R')
pitch.m <- ModifyPitchCols(pitch)  # ~ 2 sec
p09.m <- ModifyPitchCols(p09)  # ~ 50 sec

# Create some subsets of the pitch columns
# And save them to files
colsForSmall <- c('seasonPitchNum', 'game_id', 'upid', 'ubnum', 'num', 'id', 'pitcher', 'batter',
                  'b', 's', 'count', 'des', 'type', 'Ball', 'Strike', 'Hit', 'pitchNum', 'finalPitch',
                  'on_1b', 'on_2b', 'on_3b')
colsForZone <- c('seasonPitchNum', 'game_id', 'upid', 'ubnum', 'num', 'id', 'pitcher', 'batter',
                 'type', 'Ball', 'Strike', 'Hit', 'pitchNum', 'finalPitch',
                 'on_1b', 'on_2b', 'on_3b',
                 'inZone', 'pxScale', 'pzScale', 'start_speed', 'pitch_type')

pitch.s <- subset(pitch.m, select = colsForSmall)
pitch.z <- subset(pitch.m, select = colsForZone)

p09.s <- subset(p09.m, select = colsForSmall)
p09.z <- subset(p09.m, select = colsForZone)

rm(colsForSmall, colsForZone)

# Save modified pitch data
save(pitch.m, pitch.s, pitch.z, file = 'pfxData_pitch.Rdat')
save(p09.m, p09.s, p09.z, file = 'pfxData_p09.Rdat')


######
# Modify the atbat colums

# Function definition in another file
# ModifyAtBatCols(atbat)
source('pfxFuncModifyAtBatCols.R')

atbat.m <- ModifyAtBatCols(atbat)
ab09.m <- ModifyAtBatCols(ab09)  # ~ ? sec for 187k lines!

# Save modified atbat data
save(atbat.m, ab09.m, file = 'pfxData_atbatMod.Rdat')


# Look at pfxMod1.R
# investigate numSwingingStrikes ~ totPitches
# Data saved in 'pfxMod1.Rdat'


