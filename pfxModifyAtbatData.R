# Bartev Vartanian
# 10/17/11
# 
# Modify atbat data
# Use pa for plate appearance (a la Understanding Sabermetrics, Gabriel Costa)

# Function definition in another file
# ModifyAtBatCols(atbat)
source('pfxFuncModifyAtBatCols.R')

# From PfxLoad09Data.R
load("pfxDataSample.Rdat")

# This function in vectorized form is performed by ModifyAtBatCols
# Add runs, and current score
# This function is in pfxCumRunsFunction.R
# source("pfxCumRunsFunction.R")
# pa <- RunsScoredAtBat(pa)

# Save data
save(pa, file = "pfxData-plateAppearance.Rdat")




# TRY STUFF HERE
# visitor bats first (top of the inning)

pps <- pp[1:25,]
pas <- pa[1:25,]

# dpp <- sqldf('select game_id, upid, ubnum, num, count from pps where finalPitch = "TRUE"')

# dpp <- sqldf('select * from pps where finalPitch')
# dpa <- sqldf('select game_id, ubnum, half, htRuns, atRuns from pas')

# dm <- merge(dpp, dpa, by = 'ubnum', all.x = TRUE)
# dmm <- merge(pps, dm, by = intersect(names(pps), names(dm)), all.x = TRUE, sort = FALSE)

head(pas)
head(pps)

matchCols <- c('game_id', 'batter', 'pitcher', 'ubnum')
ppst <- subset(pps, finalPitch == TRUE)

mg <- merge(ppst, pas, by = c('ubnum', 'game_id', 'pitcher', 'batter', 'num'), all.x = FALSE, sort = FALSE)
mg

intNames <- intersect(names(pps), names(mg))
intNames

mgg <- merge(pps, mg, by = intersect(names(pps), names(mg)), all.x = TRUE, sort = FALSE)
mgg <- mgg[with(mgg, order(seasonPitchNum))]

mgg.sort <- mgg[order(mgg$seasonPitchNum), ]
mgg.sort

upidmgg <- mgg$upid
upidmgg

sort(mg, by ~ upid)
mg[with(mg, order(upid))]

ppst

mgg <- merge(pps, ppst, by = intersect(names(pps), names(ppst)), all.x = TRUE, sort = FALSE)
mgg
plot(mgg$id)
dpp

pps[1:13, ]
head(pps)
head(dpp)
dpa
names(pps)
names(pas)

str(pps)
str(pas)
# dm <- sqldf('select * from dpp inner join dpa')

head(dpp)
dm

head(dm, 20)
head(pps)