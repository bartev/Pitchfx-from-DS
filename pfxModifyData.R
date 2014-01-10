## Edit pitch data
# Do this after Load09Data.R

# 'id' is the pitch number thrown. Numbers reset on each game? (how about double headers?)
# 'num' is the batter number

# Sample data from Pitchfx '09
load("pfxDataSample.Rdat")  # pitchfx '09 data - 5000 row max
load("pfxDataWorkingCopy09.Rdat")      # pitchfx '09 data - full monty
# load("pfxData09.Rdat") # data from 2009 download - full monty

source("pfxSumPitchPerBatter.R")

library(sqldf)

# Keep the variable pitch as is. Create a new variable, pp, to work with
pp <- pitch
# pp <- pitch09

# edit game_id
pp$game_id <- gsub("[/-]", "", pp$game_id)


# Add the count to use as a factor later
# Count has 12 factors: "0 0", "0 1", ...
pp$count <- as.factor(pp(pitch$b, sapply(pp$s, function (s) min(s, 2))))

# Add batter number as a factor
# pp$batterNum <- as.factor(pp$num)

# Add pitch number per batter - runs through entire database.
# Use function PitchNumPerBatter(df) from pfxSumPitchPerBatter.R
pp <- PitchNumPerBatter(pp)

# Add unique pitch id (game_id + id)
pp$upid <- paste(pp$game_id, pp$id)

# Add unique batter id (game_id + num)
pp$ubnum <- paste(pp$game_id, pp$num)

# Add columns for Pitch results Ball/Strike/Hit
pp <- transform(pp, Ball = as.numeric(type == 'B'), Strike = as.numeric(type == 'S'), Hit = as.numeric(type == 'X'))

# Clean up NA's in data
pp$on_1b[which(is.na(pp$on_1b))] <- 0
pp$on_2b[which(is.na(pp$on_2b))] <- 0
pp$on_3b[which(is.na(pp$on_3b))] <- 0

# Select columns I want only
pp1 <- pp
pp <- subset(pp1, select = c(seasonPitchNum, game_id, upid, ubnum, num, id,
                             pitcher, batter,
                             b, s, count, des,
                             type, Ball, Strike, Hit,
                             pitchNum, finalPitch,
                             on_1b, on_2b, on_3b))
head(pp)
str(pp1)
# Scale columns for strike zone
maxX <- 17/12/2
ppz <- transform(pp1,
                 pxScale = (px / maxX),
                 pzScale = 2 * (pz - (sz_top + sz_bot) / 2) / (sz_top - sz_bot)
                 # pzRange = sz_top - sz_bot
                 )
ppz <- transform(ppz, inZone = (abs(pxScale) <= 1) & (abs(pzScale) <= 1))
ppz <- subset(ppz, select = c(seasonPitchNum, game_id, upid, ubnum, num, id,
                              pitcher, batter,
                              b, s, count, des,
                              type, Ball, Strike, Hit,
                              pitchNum, finalPitch,
                              on_1b, on_2b, on_3b,
                              inZone, pxScale, pzScale,
                              start_speed, pitch_type
                              ))

head(ppz)


# Save data
save(pp1, pp, ppz, file = "pfxData-pp.Rdat")

head(pp)
head(ppz)




# TRY STUFF HERE





# Plot out id column for each game
games <- sqldf('select distinct game_id from pp')
# WHY DOESN'T THIS SQL CODE WORK?
# p1 <- sqldf(paste("select id from pp where game_id == ", games[1,]))
p1 <- subset(pp, game_id == games[3,], select = c(game_id, num, pitcher, batter, count, id, type))
games[1,]
p1
plot(p1$id) # 1 game
plot(pp$id) # all games
