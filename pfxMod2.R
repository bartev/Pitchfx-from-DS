# Bartev Vartanian
# 2011-10-31
# Model 1
# Runs ~ single + double + triple + homer + BB + K

rm(list=ls())
setwd("~/Rmac/Pitchfx/PitchFX")
load('pfxData_p09.Rdat')    # Full monty '09 data, .m, .s, .z

head(p09.m)
names(p09.m)

# p09.s <- subset(p09.m, 
#                 select=c(pitcher, des, type, game_id, num, batter, 
#                          b, s, id, Ball, Hit, Strike,
#                          x, y, on_1b, on_2b, on_3b,
#                          seasonPitchNum, home, count, pitchNum, finalPitch,
#                          inZone, pxScale, pzScale,
#                          start_speed, end_speed, sz_top, sz_bot, 
#                          pfx_x, pfx_z, px, pz, x0, y0, z0, vx0, vy0, vz0, ax, ay, az, 
#                          break_y, break_angle, break_length, 
#                          pitch_type, spin_dir, spin_rate))

# Good outcomes
# resGoodAndBad <- levels(p09.m$des)
# resGood <- c('Called Strike', 'Foul', 'Foul Bunt', 'Foul Tip', 'In play, out(s)', 
#              'Intent Ball', 'Missed Bunt', 'Pitchout', 'Swinging Pitchout', 
#              'Swinging Strike', 'Swinging Strike (Blocked)', 'Unknown Strike')
# resBad <- resGoodAndBad[!resGoodAndBad %in% resGood]

# Add a column to the dataset for good or bad result as defined above
# p09.s <- transform(p09.s, resGB=(des %in% resGood))
# head(p09.s)

# fit1 <- glm(chd ~ sbp + tobacco + ldl + adiposity + famhist + typea + obesity + alcohol + age,
#            data=sahTest, family=binomial())
# summary(fit1)
# dataToFit <- transform(p09.s, on1B=(on_1b > 0), on2B=(on_2b > 0), on3B=(on_3b > 0))
# fit1 <- glm(resGB ~ type + b + s + count + x + y + )

load('pfxData_atbatMod.Rdat') # 5000 row and all rows of modified atbat data

# finPitchOnly <- subset(p09.m, finalPitch==TRUE)
# finPitchOnly <- merge(finPitchOnly, ab09.m)

# finPitchOnly <- transform(finPitchOnly, event='none', htRuns=0, atRuns=0, homeScore=0, awayScore=0, runsOnPitcher=0, pAhead=0)
toMerge <- subset(ab09.m, select=c(game_id, inning, num, o, stand, event, 
                                                            htRuns, atRuns, homeScore, awayScore, runsOnPitcher, pAhead))
tm3 <- merge(p09.m, toMerge)  # takes a couple minutes - result, 717k x 71
tm4 <- tm3[which(tm3$finalPitch), ] # the dim for tm4 don't match up with ab09.m. Maybe because of the game we took out in Load09Data?
tm4$des2 <- tm4$event
tfinal <- tm4
tm4 <- tm3[which(!tm3$finalPitch), ]
tm4$des2 <- tm4$des
tm4$htRuns=0
tm4$atRuns=0
tfinal <- rbind(tfinal, tm4)

rm(tm3, tm4, toMerge, p09.m, ab09.m)
save(tfinal, file='pfxMod2tfinal.Rdat')

namesAll <- names(tfinal)
namesToRemove <- c('sv_id', 'sz_top', 'sz_bot', 'type_confidence', 'zone', 'month', 'day', 'vis_team', 'home_team', 'team',
                   'League', 'Division', 'half', 'upid', 'ubnum')
namesToKeep <-namesAll[!namesAll %in% namesToRemove]
pitchData <- subset(tfinal, select=namesToKeep)


# Good outcomes
# resGoodAndBad <- levels(p09.m$des)
# resGood <- c('Called Strike', 'Foul', 'Foul Bunt', 'Foul Tip', 'In play, out(s)', 
#              'Intent Ball', 'Missed Bunt', 'Pitchout', 'Swinging Pitchout', 
#              'Swinging Strike', 'Swinging Strike (Blocked)', 'Unknown Strike')
# resBad <- resGoodAndBad[!resGoodAndBad %in% resGood]

des2GoodAndBad <- levels(pitchData$des2)
des2Bad <- c('Double', 'Hit By Pitch', 'Home Run', 'Single', 'Triple', 'Walk', 'Automatic Ball', 'Ball', 'Ball In Dirt')


t.des2 <- xtabs(~ pitcher + des2, data=pitchData)
t.runs <- xtabs(~ pitcher + runsOnPitcher, data=pitchData)
head(t.runs)
t2.runs <- table(pitchData$pitcher, pitchData$runsOnPitcher)
head(t2.runs)
t <- with(pitchData, table(subset(pitchData, select=c(pitcher, runsOnPitcher))))

# t <- tapply(select=c(pitcher, runsOnPitcher), 1, sum)


#
# Useful - total runs scored on a pitcher
t.totRuns <- sqldf('select pitcher, sum(runsOnPitcher) totRuns from pitchData group by pitcher')
#

# Create table of pitcher vs des2
xt <- xtabs(~pitcher + des2, data=pitchData)
x1 <- xt[,'Double']  # gives column 'Double' of the table
# Get a list of the column names
xnames <- levels(pitchData$des2)
# df <- data.frame(B1=xt[,'Single'],B2=xt[, 'Double'])
# Create a dataframe from the table (note: converts spaces to periods '.')
# df <- data.frame(dimnames(xt)[1], xt[,dimnames(xt)[2]])
n1 <- dimnames(xt)[[1]]
n2 <- dimnames(xt)[[2]]
df <- data.frame(pitcher=n1, xt[, n2])
df.runsDes2 <- merge(t.totRuns, df)


# Convert the table into a more useful df. Rename columns, then group (some columns fall under
# multiple categories - e.g. StrikeOut2P -> AllStrike, StrikeOut, DoublePlay

df <- data.frame(pitcher=dimnames(xt)$pitcher, 
                 b1=xt[,'Single'], 
                 b2=xt[,'Double'],
                 b3=xt[,'Triple'],
                 hr=xt[,'Home Run'],
                 Ball=xt[,'Ball'],
                 BallDirt=xt[,'Ball In Dirt'],
                 BallInt=xt[,'Intent Ball'],
                 BallAuto=xt[,'Automatic Ball'],
                 StrikeOut=xt[,'Strikeout'],
                 StrikeOut2P=xt[,'Strikeout - DP'],
                 StrikeCalled=xt[,'Called Strike'],
                 StrikeSwing=xt[,'Swinging Strike'],
                 StrikeSwingBlocked=xt[,'Swinging Strike (Blocked)'],
                 StrikeUnknown=xt[,'Unknown Strike'],
                 BuntGroundOut=xt[,'Bunt Ground Out'],
                 BuntGroundOut2=xt[,'Bunt Groundout'],
                 BuntPopOut=xt[,'Bunt Pop Out'],
                 OutDP=xt[,'Double Play'],
                 OutDPGround=xt[,'Grounded Into DP'],
                 OutDPSacFly=xt[,'Sac Fly DP'],
                 OutFieldChoice=xt[,'Fielders Choice Out'],
                 OutFly=xt[,'Fly Out'],
                 OutFly2=xt[,'Flyout'],
                 OutForce=xt[,'Force Out'],
                 OutForce2=xt[,'Forceout'],
                 OutGround=xt[,'Ground Out'],
                 OutGround2=xt[,'Groundout'],
                 OutLineOut=xt[,'Line Out'],
                 OutLineOut2=xt[,'Lineout'],
                 OutPop=xt[,'Pop Out'],
                 OutRunner=xt[,'Runner Out'],
                 OutSacBunt=xt[,'Sac Bunt'],
                 OutSacFly=xt[,'Sac Fly'],
                 OutTP=xt[,'Triple Play'],
                 Foul=xt[,'Foul'],
                 FoulRunner=xt[,'Foul (Runner Going)'],
                 FoulBunt=xt[,'Foul Bunt'],
                 FoulTip=xt[,'Foul Tip'],
                 Walk=xt[,'Walk']
                 )
df2 <- transform(df, 
                 AllBall=Ball + BallDirt + BallInt + BallAuto,
                 AllStrikeOut=StrikeOut + StrikeOut2P,
                 AllStrike=StrikeOut + StrikeOut2P + StrikeCalled + StrikeSwing + StrikeSwingBlocked + StrikeUnknown,
                 AllBunt=BuntGroundOut + BuntGroundOut2 + BuntPopOut,
                 AllOut=StrikeOut + StrikeOut2P + BuntGroundOut + BuntGroundOut2 + BuntPopOut + OutDP +
                   OutDPGround + OutDPSacFly + OutFieldChoice + OutFly + OutFly2 + OutForce + OutForce2 +
                   OutGround + OutGround2 + OutLineOut + OutLineOut2 + OutPop + OutRunner + OutSacBunt +
                   OutSacFly + OutTP,
                 AllOutDP=StrikeOut2P + OutDP + OutDPGround + OutDPSacFly,
                 AllOutTP=OutTP,
                 AllFoul=Foul + FoulRunner + FoulBunt + FoulTip,
                 AllWalk=Walk
                 )
head(df2)
head(t.totRuns)
runsEventsDf <- merge(t.totRuns, df2)

# re.scale <- subset(runsEventsDf, , select=c(totRuns, b1, b2, b3, hr, AllBall, AllStrikeOut, AllStrike, AllFoul, AllWalk))
# head(re.scale)
# model runs ~ 1b + 2b + 3b + hr + walks + strikeouts
mod1 <- lm(totRuns ~ b1 + b2 + b3 + hr + AllBall + AllStrikeOut + AllStrike + AllFoul + AllWalk + 0, data=runsEventsDf)
# Remove AllFoul (t = -0.016)
mod1 <- lm(totRuns ~ b1 + b2 + b3 + hr + AllBall + AllStrikeOut + AllStrike + AllWalk + 0, data=runsEventsDf)
# Remove AllStrikeOut (t = 0.433)
mod1 <- lm(totRuns ~ b1 + b2 + b3 + hr + AllBall + AllStrike + AllWalk + 0, data=runsEventsDf)
# Remove AllBall (t = 0.768)

###
# 6 features, all significant
mod1 <- lm(totRuns ~ b1 + b2 + b3 + hr + AllStrike + AllWalk + 0, data=runsEventsDf)
###

mod1 <- lm(totRuns ~ b1 + 0, data=runsEventsDf)
mod2 <- lm(totRuns ~ b1 + b2 + 0, data=runsEventsDf)
mod3 <- lm(totRuns ~ b1 + b2 + b3 + 0, data=runsEventsDf)
mod4 <- lm(totRuns ~ b1 + b2 + b3 + hr + 0, data=runsEventsDf)
mod5 <- lm(totRuns ~ b1 + b2 + b3 + hr + AllStrike + 0, data=runsEventsDf)

###
# AllStrike only significant with AllWalk
mod6 <- lm(totRuns ~ b1 + b2 + b3 + hr + AllStrike + AllWalk + 0, data=runsEventsDf)
summary(mod6)

anova(mod1, test='Chisq')
pchisq(25.36, 658)

toPlot <- data.frame(mod1$model)
toPlot <- cbind(toPlot, fit1b=fitted(mod1))
toPlot <- cbind(toPlot, fit2b=fitted(mod2))
toPlot <- cbind(toPlot, fit3b=fitted(mod3))
toPlot <- cbind(toPlot, fithr=fitted(mod4))
toPlot <- cbind(toPlot, fitStrike=fitted(mod5))
toPlot <- cbind(toPlot, fitWalk=fitted(mod6))


library(ggplot2)
toPlot <- data.frame((mod1$model), fit=fitted(mod1))
p <- ggplot(data=toPlot) + geom_point() + scale_y_log10() + scale_x_log10()
p <- ggplot(data=toPlot) + geom_point()
# geom_abline adds a line with intercept=..., slope = ...
p <- p + geom_abline(slope = 1, intercept=0, color = alpha('steelblue', 1.0), size = 2)
pp <- p
pp <- pp + geom_point(aes(x=totRuns, y=fit1b), color='red', alpha=I(1.0))
pp
pp <- p
pp <- pp + geom_point(aes(x=totRuns, y=fit2b), color='blue', alpha=I(1.0))
pp
pp <- p
pp <- pp + geom_point(aes(x=totRuns, y=fit3b), color='green', alpha=I(1.0))
pp
pp <- p
pp <- pp + geom_point(aes(x=totRuns, y=fithr), color='orange', alpha=I(1.0))
pp
pp <- p
pp <- pp + geom_point(aes(x=totRuns, y=fitStrike), color='purple', alpha=I(1.0))
pp
pp <- p
pp <- pp + geom_point(aes(x=totRuns, y=fitWalk), color='black', alpha=I(1.0))
pp

