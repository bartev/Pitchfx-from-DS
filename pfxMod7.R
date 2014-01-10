# Mod7
# summarize data by pitcher

rm(list=ls())
library(sqldf)
library(ggplot2)

# From pfxMod3.R
load('pfxMod3.Rdat') # ERA, starting/relief, etc., keep eraDf
rm(erh, erl, up, upp, tdf, df)
rm(freqTable, innPitchedTable, m1)

load('pfxData_atbatMod.Rdat') # 5000 row and all rows of modified atbat data
load('pfxData_p09.Rdat')    # Full monty '09 data, .m, .s, .z
rm(p09.s, p09.z, atbat.m)
p09 <- p09.m  # sqldf has problems with the '.' in a df name
atbat <- ab09.m
rm(p09.m, ab09.m)
head(eraDf)
# add starter/relief column
eraDf <- transform(eraDf, startPitcher=ifelse(startPitcher, 'Start', 'Relief'))

# look at S/B/X vs runs scored
head(atbat)
levels(atbat$event)


#
#
# Get num S/B/X/Runs scored per pitcher
#
#
# building up df of pitcher stats
pitch <- subset(p09, select=c(pitcher, type, Ball, Strike, Hit, pxScale, pzScale, inZone, pitch_type))
sdf <- sqldf('select pitcher, sum(Ball) Balls, sum(Strike) Strikes, sum(Hit) Hits from pitch group by pitcher')
sdf <- transform(sdf, totPitches=Balls + Strikes + Hits)
sdf <- transform(sdf, pctB=Balls/totPitches, pctS=Strikes/totPitches, pctH=Hits/totPitches)
sdf1 <- sqldf('select pitcher, sum(runsOnPitcher) Runs, count(1) numBatters from atbat group by pitcher')
sdf <- merge(sdf, sdf1)
sdf <- transform(sdf, runsPerPitch=Runs/totPitches, runsPerBat=Runs/numBatters)
sdf <- merge(sdf, subset(eraDf, select=c(pitcher, startPitcher)))
sdf <- transform(sdf, pitchPerBat=totPitches/numBatters, hitPerBat=Hits/numBatters)

# soDf <- transform(soDf, strikeout=event %in% c('Strikeout', 'Strikeout - DP'))
groundB <- c('Bunt Ground Out', 'Bunt Groundout', 'Ground Out', 'Grounded Into DP', 'Groundout', 'Sac Bunt', 'Sacrifice Bunt DP')
outB <- c('Bunt Ground Out', 'Bunt Groundout', 'Bunt Pop Out', 'Double Play', 'Fielders Choice', 
         'Fielders Choice Out', 'Fly Out', 'Flyout', 'Force Out', 'Forceout', 'Ground Out', 
         'Grounded Into DP', 'Groundout', 'Line Out', 'Lineout', 'Pop Out', 'Runner Out', 
         'Sac Bunt', 'Sac Fly', 'Sac Fly DP', 'Sacrifice Bunt DP', 'Strikeout', 'Strikout - DP',
         'Triple Play')
flyB <- c('Fly Out', 'Flyout', 'Line Out', 'Lineout', 'Pop Out', 'Sac Fly', 'Sac Fly DP')
b1B <- c('Single')
b2B <- c('Double')
b3B <- c('Triple')
hrB <- c('Home Run')
walkB <- c('Hit By Pitch', 'Walk')

tempdf <- subset(atbat, select=c(pitcher, event))
tempdf <- transform(tempdf, ground=event %in% groundB, out=event %in% outB,
                    fly=event %in% flyB, b1=event %in% b1B, b2=event %in% b2B, b3=event %in% b3B,
                    hr=event %in% hrB, walk=event %in% walkB)
tempdf <- transform(tempdf, go=ground & out, fo=fly & out)
head(tempdf)
tdf <- sqldf('select pitcher, sum(ground) Ground, sum(out) Out, sum(fly) Fly, 
             sum(b1) B1, sum(b2) B2, sum(b3) B3, sum(hr) HR, 
             sum(walk) Walk from tempdf group by pitcher')
head(tdf)
tdf <- merge(tdf, subset(sdf, select=c(pitcher, Balls, Strikes, Hits, totPitches, Runs, numBatters, startPitcher)))

sdf$pitcher <- as.factor(sdf$pitcher)
tdf$pitcher <- as.factor(tdf$pitcher)
save(sdf, tdf, file='pfxMod7PitcherSummary.Rdat')


# sum(sdf$numBatters)
# sum(sdf$Balls + sdf$Strikes + sdf$Hits)
# Data looks like this
# > head(sdf)
#   pitcher Balls Strikes Hits totPitches      pctB      pctS      pctH Runs numBatters runsPerPitch runsPerBat startPitcher pitchPerBat hitPerBat
# 1  110683   492     500  233       1225 0.4016327 0.4081633 0.1902041   39        326   0.03183673 0.11963190       Relief    3.757669 0.7147239
# 2  111492   139     130   62        331 0.4199396 0.3927492 0.1873112   12         85   0.03625378 0.14117647       Relief    3.894118 0.7294118
# 3  111838   188     234  133        555 0.3387387 0.4216216 0.2396396   20        156   0.03603604 0.12820513        Start    3.557692 0.8525641


#
#
#
# try the model on 25% of the data
#
#
#
trainIndices <- sample(1:nrow(sdf), 0.6 * nrow(sdf), replace=FALSE)
testIndices <- which(! 1:nrow(sdf) %in% trainIndices)
sdf$trainSet <- 'test'
sdf[trainIndices, 'trainSet'] <- 'train'
head(sdf)

gp1 <- ggplot(data=sdf, aes(x=numBatters, y=Runs)) + geom_point() + facet_grid(. ~ startPitcher)
gp1
gp2 <- ggplot(data=sdf, aes(x=pitchPerBat, numBatters)) + geom_point() + facet_grid(. ~ startPitcher)
gp2
gp3 <- ggplot(data=sdf, aes(x=hitPerBat, y=numBatters)) + geom_point() + facet_grid(. ~ startPitcher)
gp3
gp4 <- ggplot(data=sdf, aes(x=runsPerBat, y=numBatters)) + geom_point() + facet_grid(. ~ startPitcher)
gp4
gp5 <- ggplot(data=sdf, aes(x=runsPerBat, y=hitPerBat)) + geom_point() + facet_grid(. ~ startPitcher)
gp5
gp6 <- ggplot(data=sdf, aes(x=runsPerBat, y=pitchPerBat)) + geom_point() + facet_grid(. ~ startPitcher)
gp6

hist(sdf$pitchPerBat)
plot(density(sdf$pitchPerBat))
ppbs <- subset(sdf, startPitcher=='Start', pitchPerBat)$pitchPerBat
ppbr <- subset(sdf, startPitcher=='Relief', pitchPerBat)$pitchPerBat
ggplot(data=sdf) + geom_density(aes(x=pitchPerBat)) + facet_grid(startPitcher ~ .) + 
  geom_histogram(aes(x=pitchPerBat, y=..density..), alpha=I(0.5), binwidth=0.1, fill='red')

# ECDF?
ggplot(data=sdf) + 
  geom_step(aes(x=sort(pitchPerBat), y=seq_along(pitchPerBat)/length(pitchPerBat))) +
  facet_grid(startPitcher ~ .)

# Can't figure out another way to do ecdf using ggplot
plot(ecdf(ppbs))
# mydf=data.frame(a = rnorm(100, 0, 1), b=rnorm(100, 2, 1), c=rnorm(100, -2, 0.5))
# mydfm <- melt(mydf)
# ggplot(mydfm, aes(x=value)) + geom_density(aes(group=variable, colour=variable)) + opts(legend.position=c(0.85, 0.85))
qqnorm(scale(ppbs))
qqnorm(scale(ppbr))
chisq.test(ppbr)
library(MASS)
fitdistr(ppbs, 'normal')
fitdistr(ppbs, 'gamma')


ggplot(data=sdf, aes(x=runsPerPitch, y=pctB)) + geom_point() + facet_grid(. ~ startPitcher)
ggplot(data=sdf, aes(x=runsPerPitch, y=pctS)) + geom_point() + facet_grid(. ~ startPitcher)
ggplot(data=sdf, aes(x=runsPerPitch, y=pctH)) + geom_point() + facet_grid(. ~ startPitcher)

lm1 <- lm(runsPerPitch ~ pctB + pctS + pctH, data=sdf)
summary(lm1)
lm2 <- lm(runsPerPitch ~ pctH + pctB + pctS, data=sdf)
summary(lm2)
lm3 <- lm(Runs ~ Balls + Strikes + Hits + 0, data=sdf)
summary(lm3)
lm4 <- lm(Hits ~ Balls + Strikes, data=sdf)
summary(lm4)

head(tdf)

#
#
# Linear models
tdf$trainSet <- 'test'
tdf[trainIndices, 'trainSet'] <- 'train'

tlm1 <- lm(Runs ~ B1 + B2 + B3 + HR + Walk + Balls + Strikes + Hits + Ground + Fly + Out, data=tdf)
tlm1.S <- lm(Runs ~ B1 + B2 + B3 + HR + Walk + Fly + Out + 0, data=subset(tdf[trainIndices,], startPitcher=='Start'))
tlm1.R <- lm(Runs ~ B1 + B2 + B3 + HR + Walk + Fly + Out + 0, data=subset(tdf[trainIndices,], startPitcher=='Relief'))
df1 <- transform(subset(tdf, startPitcher=='Start'), fitRuns1=predict(tlm1.S, subset(tdf, startPitcher=='Start')))
df2 <- transform(subset(tdf, startPitcher=='Relief'), fitRuns1=predict(tlm1.R, subset(tdf, startPitcher=='Relief')))
df <- rbind(df1, df2)
head(df)
summary(tlm1.S)
summary(tlm1.R)

g1 <- ggplot(data=df, aes(x=Runs, y=fitRuns1)) + geom_point() + facet_grid(startPitcher ~ trainSet)
g1
g2 <- ggplot(data=df, aes(x=B1, y=Runs)) + geom_point() + facet_grid(startPitcher ~ trainSet) +
  geom_point(aes(y=fitRuns1), colour = 'red', alpha=I(0.5))
g2
g3 <- ggplot(data=df, aes(x=B2, y=Runs)) + geom_point() + facet_grid(startPitcher ~ trainSet) +
  geom_point(aes(y=fitRuns1), colour = 'red', alpha=I(0.5))
g3
g4 <- ggplot(data=df, aes(x=B3, y=Runs)) + geom_point() + facet_grid(startPitcher ~ trainSet) +
  geom_point(aes(y=fitRuns1), colour = 'red', alpha=I(0.5))
g4
g5 <- ggplot(data=df, aes(x=HR, y=Runs)) + geom_point() + facet_grid(startPitcher ~ trainSet) +
  geom_point(aes(y=fitRuns1), colour = 'red', alpha=I(0.5))
g5
g6 <- ggplot(data=df, aes(x=Walk, y=Runs)) + geom_point() + facet_grid(startPitcher ~ trainSet) +
  geom_point(aes(y=fitRuns1), colour = 'red', alpha=I(0.5))
g6
g7 <- ggplot(data=df, aes(x=Fly, y=Runs)) + geom_point() + facet_grid(startPitcher ~ trainSet) +
  geom_point(aes(y=fitRuns1), colour = 'red', alpha=I(0.5))
g7
g8 <- ggplot(data=df, aes(x=Out, y=Runs)) + geom_point() + facet_grid(startPitcher ~ trainSet) +
  geom_point(aes(y=fitRuns1), colour = 'red', alpha=I(0.5))
g8

tdf$Ground <- as.numeric(tdf$Ground)
tdf$Out <- as.numeric(tdf$Out)
tdf$Fly <- as.numeric(tdf$Fly)
tdf$B1 <- as.numeric(tdf$B1)
tdf$B2 <- as.numeric(tdf$B2)
tdf$B3 <- as.numeric(tdf$B3)
tdf$HR <- as.numeric(tdf$HR)
tdf$Walk <- as.numeric(tdf$Walk)
tpctdf <- sqldf('select pitcher, Ground/numBatters pctGrnd, Out/numBatters pctOut,
                Fly/numBatters pctFly, B1/numBatters pctB1, B2/numBatters pctB2,
                B3/numBatters pctB3, HR/numBatters pctHr, Walk/numBatters pctWalk,
                Runs/numBatters pctRuns, startPitcher, trainSet from tdf')
head(tpctdf)
head(tdf)
str(tdf)
#
#
#
# Linear models using percentages
#
#
#
# pct.lm.S <- lm(pctRuns ~ pctB1 + pctB2 + pctB3 + pctHr + pctWalk + pctGrnd + pctFly + pctOut + 0, data=subset(tpctdf[trainIndices,], startPitcher=='Start'))
# pct.lm.R <- lm(pctRuns ~ pctB1 + pctB2 + pctB3 + pctHr + pctWalk + pctGrnd + pctFly + pctOut + 0, data=subset(tpctdf[trainIndices,], startPitcher=='Relief'))
pct.lm.S <- lm(pctRuns ~ pctB1 + pctB2 + pctB3 + pctHr + pctWalk + pctOut + 0, data=subset(tpctdf[trainIndices,], startPitcher=='Start'))
pct.lm.R <- lm(pctRuns ~ pctB1 + pctB2 + pctB3 + pctHr + pctWalk + pctOut + 0, data=subset(tpctdf[trainIndices,], startPitcher=='Relief'))

summary(pct.lm.S)
summary(pct.lm.R)
df1 <- transform(subset(tpctdf, startPitcher=='Start'), fitRuns1=predict(pct.lm.S, subset(tpctdf, startPitcher=='Start')))
df2 <- transform(subset(tpctdf, startPitcher=='Relief'), fitRuns1=predict(pct.lm.R, subset(tpctdf, startPitcher=='Relief')))
df <- rbind(df1, df2)

g1 <- ggplot(data=df, aes(x=pctRuns, y=fitRuns1)) + geom_point() + facet_grid(startPitcher ~ trainSet)
g1
ggplot(data=df, aes(x=pctB1, y=pctRuns)) + geom_point() + facet_grid(startPitcher ~ trainSet) + geom_point(aes(y=fitRuns1), colour = 'red', alpha=I(0.5))
ggplot(data=df, aes(x=pctB2, y=pctRuns)) + geom_point() + facet_grid(startPitcher ~ trainSet) + geom_point(aes(y=fitRuns1), colour = 'red', alpha=I(0.5))
ggplot(data=df, aes(x=pctB3, y=pctRuns)) + geom_point() + facet_grid(startPitcher ~ trainSet) + geom_point(aes(y=fitRuns1), colour = 'red', alpha=I(0.5))
ggplot(data=df, aes(x=pctHr, y=pctRuns)) + geom_point() + facet_grid(startPitcher ~ trainSet) + geom_point(aes(y=fitRuns1), colour = 'red', alpha=I(0.5))

ggplot(data=df, aes(x=pctWalk, y=pctRuns)) + geom_point() + facet_grid(startPitcher ~ trainSet) + geom_point(aes(y=fitRuns1), colour = 'red', alpha=I(0.5))
ggplot(data=df, aes(x=pctOut, y=pctRuns)) + geom_point() + facet_grid(startPitcher ~ trainSet) + geom_point(aes(y=fitRuns1), colour = 'red', alpha=I(0.5))
