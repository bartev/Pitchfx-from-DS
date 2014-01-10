# Bartev Vartanian
# 10/17/11

# Take pitch data (pp), and add the result of the  pitch
# H Hit
# BB Walk (bases on balls)
# IBB Intentional base on balls
# B1 Single
# B2 Double
# B3 Triple
# HR Homerun
# SF Sacrifice fly
# SH Bunt (Sacrifice hit)
# PO Pitchout (intentional ball to catcher to prevent stolen base)

# Get pp from saved file
load("pfxData-pp.Rdat")

ev <- atbat$event
levels(ev)

# head(atbat)
# head(pp)

# num column resets for each game
# plot(atbat$num)
# plot(pp$num)

# stfs is a useless column?
# stfs <- atbat$start_tfs
# head(stfs, 50)
# str(stfs)


ab <- atbat[1:500,]
# From pfxModifyData.R
# pp <- transform(pp, Ball = as.numeric(type == 'B'), Strike = as.numeric(type == 'S'), Hit = as.numeric(type == 'X'))
ab <- subset(ab, select = c(game_id, num, b, s, o, score, batter, pitcher, des, event, home_team_runs, away_team_runs))
head(ab)
ab$home_team_runs[which(is.na(ab$home_team_runs))] = 0
ab$away_team_runs[which(is.na(ab$away_team_runs))] = 0

ab2 <- transform(ab, 
                 B1 = as.numeric(event == 'Single'),
                 B2 = as.numeric(event == 'Double'),
                 B3 = as.numeric(event == 'Triple'),
                 HR = as.numeric(event == 'Home Run'),
                 Out = as.numeric(event == 'Batter Interference' |
                                  event == 'Bunt Ground Out' |
                                  event == 'Bunt Groundout' | 
                                  event == 'Bunt Pop Out' |
                                  event == 'Double Play' |
                                  event == 'Fielders Choice' |
                                  event == 'Fielders Choice Out' |
                                  event == 'Fly Out' |
                                  event == 'Flyout' |
                                  event == 'Force Out' |
                                  event == 'Forceout' |
                                  event == 'Ground Out' |
                                  event == 'Grounded Into DP' |
                                  event == 'Groundout' |
                                  event == 'Line Out' |
                                  event == 'Lineout' |
                                  event == 'Pop Out' |
                                  event == 'Runner Out' |
                                  event == 'Sac Bunt' |
                                  event == 'Sac Fly' |
                                  event == 'Sac Fly DP' |
                                  event == 'Strikeout' |
                                  event == 'Strikeout = DP' |
                                  event == 'Triple Play'
                                  ),
                 W = as.numeric(event == 'Hit By Pitch' |
                                event == 'Intent Walk' |
                                event == 'Walk')
                 )
head(ab2)
