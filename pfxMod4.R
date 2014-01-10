# Mod4
# B/S/X vs pitch number
# how does performance change as more pitches against batter?

head(pitchData)
rm(list=ls())
library(sqldf)
library(ggplot2)

load('pfxData_atbatMod.Rdat') # 5000 row and all rows of modified atbat data
load('pfxData_p09.Rdat')    # Full monty '09 data, .m, .s, .z
rm(p09.s, p09.z, atbat.m)
p09 <- p09.m  # sqldf has problems with the '.' in a df name
rm(p09.m)

# From pfxMod1.R
load('pfxMod1.Rdat') # SwingingStrikes ~ TotalPitches
rm(p, innPitchedDf, mdf)

# From pfxMod3.R
load('pfxMod3.Rdat') # ERA, starting/relief, etc., keep eraDf
rm(erh, erl, up, upp, tdf, df)
rm(freqTable, innPitchedTable, m1)

# table of b/s/h vs pitch number
t <- xtabs( ~ type + pitchNum, data=p09)
head(t)

# Each column shows the probability of a pitch resulting in a Ball, Strike or Hit (X) on the nth pitch to the batter
t.p <- prop.table(t, 2)
t.p <- t(t.p)
tpDf <- as.data.frame(t.p)
tpDf$pitchNum <- as.integer(tpDf$pitchNum)
head(tpDf)

# df <- data.frame(B1=xt[,'Single'],B2=xt[, 'Double'])
# MIKE, what's a good way to get from a table to a df?
# df <- data.frame(PitchNum=1:nrow(t.p), Ball=t.p[, 'B'], Strike=t.p[,'S'], Hit=t.p[,'X'])
# df
# p <- ggplot(data=df, aes(x=PitchNum))
# p <- p + geom_point(aes(y=Ball), colour='blue')
# p <- p + geom_point(aes(y=Strike), colour='red')
# p <- p + geom_point(aes(y=Hit), colour='black')
# p

#
# Great plot showing P(B, S, X | pitchNum)
p1 <- ggplot(data=tpDf, aes(x=pitchNum, y=Freq)) + facet_grid(. ~ type) + geom_point()
p1

# Now, get a lm for each of these cases
# Don't include cases where pitchNum >= 15 - too few samples
ball.lm <- lm(Freq ~ pitchNum, data=subset(tpDf, type=='B' & pitchNum < 15))
strike.lm <- lm(Freq ~ pitchNum, data=subset(tpDf, type=='S' & pitchNum < 15))
hit.lm <- lm(Freq ~ pitchNum, data=subset(tpDf, type=='X' & pitchNum < 15))

summary(ball.lm)
summary(strike.lm)
summary(hit.lm)

###
# MIKE, Is this the best way to do this?
#
df1 <- transform(subset(tpDf, type=='B'), fit=predict(ball.lm, subset(tpDf, type=='B') ))
df2 <- transform(subset(tpDf, type=='S'), fit=predict(strike.lm, subset(tpDf, type=='S') ))
df3 <- transform(subset(tpDf, type=='X'), fit=predict(hit.lm, subset(tpDf, type=='X') ))
df <- rbind(df1, df2)
df <- rbind(df, df3)

#
# Great plot showing P(B, S, X | pitchNum), this time with linear fit on there too.
plot1 <- ggplot(data=df, aes(x=pitchNum, y=Freq, fit)) + facet_grid(. ~ type) + geom_point()
plot1 <- plot1 + geom_line(aes(y=fit), color='red')
plot1

#
#
#
# Add starter/relief to analysis
#
#
#

# This info is in eraDf from pfxMod3.R
# table of b/s/h vs pitch number
p1 <- subset(p09, select=c(pitcher, type, pitchNum))
p1 <- merge(p1, subset(eraDf, select=c(pitcher, startPitcher)))

# use sqldf instead of xtabs here
# t <- xtabs( ~ type + pitchNum, data=p09)
# head(t)

t1 <- sqldf('select startPitcher, type, pitchNum, count() numPitches from p1 group by startPitcher, type, pitchNum')
t1
# Not what I want. divides by all pitches. I want the fraction of pitches for start & relief pitchers
# t1 <- transform(t1, freq=numPitches/sum(numPitches))

# Still not what I wanted - this gives prob(b/s/x/pitchNum) I want prob(b/s/x | pitchNum)
# t1start <- transform(subset(t1, startPitcher==TRUE), Freq=numPitches/sum(numPitches))
# t1relief <- transform(subset(t1, startPitcher==FALSE), Freq=numPitches/sum(numPitches))

head(t1)

xt <- xtabs(numPitches ~ pitchNum + type + startPitcher, data=t1)
# totPitchesRelief <- as.data.frame(margin.table(xt[,,1], 1))
# totPitchesRelief <- data.frame(totPitches=margin.table(xt[, , 1], 1), startPitcher='FALSE', pitchNum)
# totPitchesStart <- data.frame(totPitches=margin.table(xt[, , 2], 1))
# head(t1)
# totPitchesRelief

ptRelief <- prop.table(xt[,,1], 1) # prop table for reliefPitcher
ptStart <- prop.table(xt[,,2], 1) # prop table for starting Pitcher
# ptRelief
# ptStart

# Convert table back to dataframe
dfStart <- as.data.frame(as.table(ptStart))
dfStart$startPitcher <- 'Start'

dfRelief <- as.data.frame(as.table(ptRelief))
dfRelief$startPitcher <- 'Relief'

dfAll <- rbind(dfStart, dfRelief)
dfAll$pitchNum <- as.integer(dfAll$pitchNum)
dfAll


# Great plot showing P(B, S, X | pitchNum), this time with linear fit on there too.
plot2 <- ggplot(data=dfAll, aes(x=pitchNum, y=Freq)) + geom_point() + facet_grid(startPitcher ~ type)
plot2

# Now, get a lm for each of these cases
# Don't include cases where pitchNum >= 15 - too few samples
ballS.lm <- lm(Freq ~ pitchNum, data=subset(dfAll, type=='B' & pitchNum < 15 & startPitcher=='Start'))
strikeS.lm <- lm(Freq ~ pitchNum, data=subset(dfAll, type=='S' & pitchNum < 15 & startPitcher=='Start'))
hitS.lm <- lm(Freq ~ pitchNum, data=subset(dfAll, type=='X' & pitchNum < 15 & startPitcher=='Start'))

ballR.lm <- lm(Freq ~ pitchNum, data=subset(dfAll, type=='B' & pitchNum < 15 & startPitcher=='Relief'))
strikeR.lm <- lm(Freq ~ pitchNum, data=subset(dfAll, type=='S' & pitchNum < 15 & startPitcher=='Relief'))
hitR.lm <- lm(Freq ~ pitchNum, data=subset(dfAll, type=='X' & pitchNum < 15 & startPitcher=='Relief'))

summary(ball.lm)
summary(strike.lm)
summary(hit.lm)

###
# MIKE, Is this the best way to do this?
#
df1 <- transform(subset(dfAll, type=='B' & startPitcher=='Start'), fit=predict(ballS.lm, subset(dfAll, type=='B' & startPitcher=='Start') ))
df2 <- transform(subset(dfAll, type=='B' & startPitcher=='Relief'), fit=predict(ballR.lm, subset(dfAll, type=='B' & startPitcher=='Relief') ))

df3 <- transform(subset(dfAll, type=='S' & startPitcher=='Start'), fit=predict(strikeS.lm, subset(dfAll, type=='S' & startPitcher=='Start') ))
df4 <- transform(subset(dfAll, type=='S' & startPitcher=='Relief'), fit=predict(strikeR.lm, subset(dfAll, type=='S' & startPitcher=='Relief') ))

df5 <- transform(subset(dfAll, type=='X' & startPitcher=='Start'), fit=predict(hitS.lm, subset(dfAll, type=='X' & startPitcher=='Start') ))
df6 <- transform(subset(dfAll, type=='X' & startPitcher=='Relief'), fit=predict(hitR.lm, subset(dfAll, type=='X' & startPitcher=='Relief') ))

df <- rbind(df1, df2)
df <- rbind(df, df3)
df <- rbind(df, df4)
df <- rbind(df, df5)
df <- rbind(df, df6)

#
# Great plot showing P(B, S, X | pitchNum), this time with linear fit on there too.
plot3 <- ggplot(data=df, aes(x=pitchNum, y=Freq, fit)) + facet_grid(startPitcher ~ type) + geom_point()
plot3 <- plot3 + geom_line(aes(y=fit), color='red')
plot3

dfPitchFreqTypeRelief <- df
save(dfPitchFreqTypeRelief, eraDf, file='pfxMod4.Rdat')