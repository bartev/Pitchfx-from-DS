setwd("~/Rmac/Pitchfx/PitchFX")
load('pfxData_atbatMod.Rdat') # 5000 row and all rows of modified atbat data


# head(ab09.m)
# rm(ab09.m, atbat.m)
# 
# load("pfxDataSample.Rdat")  # pitchfx '09 data - 5000 row max


# Mod7
# summarize data by pitcher

rm(list=ls())
library(sqldf)
library(ggplot2)
library(MASS) # for stepAIC
source('bvRaysUtilityFunctions.R') # Scaling functions


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

# Get rid of rows where the batter is a pitcher?
# Shows all players' personal stats (name, DOB, size, etc)
# load("players09.Rda")


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
sdf <- merge(sdf, subset(eraDf, select=c(pitcher, startPitcher, numInningsPitched)))
sdf <- transform(sdf, pitchPerBat=totPitches/numBatters, hitPerBat=Hits/numBatters)


# soDf <- transform(soDf, strikeout=event %in% c('Strikeout', 'Strikeout - DP'))
# fly into outfield - nearly HR
# line drive - med trajectory infield or outfield
# pop into infield
groundB <- c('Ground Out', 'Grounded Into DP', 'Groundout', 'Fielders Choice', 'Fielders Choice Out', 'Force Out', 'Forceout')
flyB <- c('Fly Out', 'Flyout', 'Sac Fly', 'Sac Fly DP')
lineB <- c('Line Out', 'Lineout')
popB <- c('Pop Out')
# groundB <- c('Bunt Ground Out', 'Bunt Groundout', 'Ground Out', 'Grounded Into DP', 'Groundout', 'Sac Bunt', 'Sacrifice Bunt DP')
outB <- c('Double Play', 'Runner Out', 'Triple Play')
strikeB <- c('Strikeout', 'Strikeout - DP')
miscB <- c('Batter Interference', 'Bunt Ground Out', 'Bunt Groundout', 'Bunt Pop Out',
           'Catcher Interference', 'Fan interference', 'Field Error', 'Intent Walk', 'Sac Bunt',
           'Sacrifice Bunt DP')
b1B <- c('Single')
b2B <- c('Double')
b3B <- c('Triple')
hrB <- c('Home Run')
walkB <- c('Hit By Pitch', 'Walk')


tempdf <- subset(atbat, select=c(pitcher, event))
tempdf <- transform(tempdf, 
                    ground=event %in% groundB, 
                    fly=event %in% flyB, 
                    line=event %in% lineB,
                    pop=event %in% popB,
                    out=event %in% outB,
                    K=event %in% strikeB,
                    misc=event %in% miscB,
                    b1=event %in% b1B, b2=event %in% b2B, b3=event %in% b3B,
                    hr=event %in% hrB, walk=event %in% walkB)
# tempdf <- transform(tempdf, go=ground & out, fo=fly & out)
head(tempdf)
tdf <- sqldf('select pitcher, sum(ground) Ground, sum(fly) Fly, sum(line) Line,
             sum(pop) Pop, sum(out) Out, sum(K) SO, sum(misc) Misc,
             sum(b1) B1, sum(b2) B2, sum(b3) B3, sum(hr) HR, 
             sum(walk) Walk from tempdf group by pitcher')

tdf1 <- merge(tdf, subset(sdf, select=c(pitcher, Balls, Strikes, Hits, totPitches, Runs, numBatters, startPitcher)))
tdf <- merge(tdf, subset(sdf, select=c(pitcher, startPitcher, numInningsPitched, Runs, numBatters, Hits)))
tdf2 <- transform(tdf, notKorW=Ground + Fly + Line + Pop + Out + Misc + B1 + B2 + B3 + HR - Hits)
tdfPerInning <- cbind(subset(tdf2, select=c(pitcher, startPitcher, numInningsPitched, numBatters)), subset(tdf2, select=c(Ground, Fly, Line, Pop, Out, SO, Misc, B1, B2, B3, HR, Walk, Runs, numBatters, Hits))/tdf2$numInningsPitched)

sdf$pitcher <- as.factor(sdf$pitcher)
tdf$pitcher <- as.factor(tdf$pitcher)
tdfPerInning$pitcher <- as.factor(tdfPerInning$pitcher)

save(sdf, tdf, tdfPerInning, file='pfxMod13StatsPerInning.Rdat')

# toregress <- merge(tdf, subset(eraDf, select=c(pitcher, numInningsPitched, startPitcher)))
# toreg2 <- toregress[,2:13]/toregress$numInningsPitched
# toreg3 <- cbind(tdf$pitcher, toreg2)
# head(eraDf)
# toreg3 <- cbind(toreg3, eraDf$ER)
# head(toreg3)

toRegStart <- subset(tdfPerInning, startPitcher=='Start')
toRegRelief <- subset(tdfPerInning, startPitcher=='Relief')
toRegress <- subset(tdfPerInning, numInningsPitched >=4)

# try the model on 60% of the data
trainIndices <- sample(1:nrow(toRegress), 0.6 * nrow(toRegress), replace=FALSE)
testIndices <- which(! 1:nrow(toRegress) %in% trainIndices)
toRegress$trainSet <- 'test'
toRegress[trainIndices, 'trainSet'] <- 'train'
ppiTrain <- subset(toRegress, trainSet=='train')
ppiTest <- subset(toRegress, trainSet != 'train')
save(toRegress, ppiTrain, ppiTest, trainIndices, file='pfxMod13regressionData')

fit1 <- lm(Runs ~ B1 + B2 + B3 + HR + Walk + SO, data=subset(toRegress, trainSet=='train'))
pred1 <- predict(fit1, toRegress)
testError <- cbind(toRegress, pred1)
plotError <- ggplot(data=testError, aes(x=Runs, y=pred1)) + geom_point() + facet_grid( ~ trainSet) + geom_abline(slope=1, color='blue')
plotError
summary(fit1)

# doesn't work - zeros in the data
fitLog1 <- lm(log(Runs) ~ log(B1) + log(B2) + log(B3) + log(HR) + log(Walk) + log(SO), data=subset(toRegress, numInningsPitched >= 4, trainSet=='train'))


#
#
#
# How to read statistics on fit1 & fit2?
# Which is better fit?
# AIC is lower on fit2
fit2 <- lm(Runs ~ B1 + B2 + B3 + HR + Walk + SO + 0, data=subset(toRegress, trainSet=='train' & startPitcher=='Start'))
pred2 <- predict(fit2, toRegress)
testError <- cbind(testError, pred2)
plotError <- ggplot(data=testError, aes(x=Runs, y=pred2)) + geom_point() + facet_grid( ~ trainSet) + geom_abline(slope=1, color='red')
plotError
summary(fit2)

head(testError)
pred1error <- sum((testError$pred1-testError$Runs)^2)
pred2error <- sum((testError$pred2-testError$Runs)^2)
pred1error
pred2error



step <- stepAIC(fit1, direction='both')
step <- stepAIC(fit2, direction='both')
step$anova # display results


# 
# 
# 
# Try fits on scaled data
toRegressNorm <- toRegress[,5:17]
trainOnly <- toRegressNorm[trainIndices,]
scaling <- scale(trainOnly)  # get scaling params from training data

trdf <- cbind(toRegress[, 1:4], ScaleFromScaled(toRegressNorm, scaling)) # Scale all the data

trdf$trainSet <- 'test'
trdf[trainIndices, 'trainSet'] <- 'train' # trainIndices defined earlier

# try the model on 60% of the data
fit3 <- lm(Runs ~ B1 + B2 + B3 + HR + Walk + SO, data=subset(trdf, trainSet=='train'))
pred3 <- predict(fit3, trdf)
testError <- cbind(trdf, pred3)
plotError <- ggplot(data=testError, aes(x=Runs, y=pred3)) + geom_point() + facet_grid( ~ trainSet) + geom_abline(slope=1, color='blue')
plotError
summary(fit3)

fit4 <- lm(Runs ~ B1 + B2 + B3 + HR + Walk + SO + 0, data=subset(trdf, trainSet=='train' & startPitcher=='Start'))
pred4 <- predict(fit4, trdf)
testError <- cbind(testError, pred2)
plotError <- ggplot(data=testError, aes(x=Runs, y=pred4)) + geom_point() + facet_grid( ~ trainSet) + geom_abline(slope=1, color='red')
plotError <- ggplot(data=testError, aes(x=Runs, y=pred4)) + geom_point() + facet_grid( startPitcher ~ trainSet) + geom_abline(slope=1, color='red')
plotError
summary(fit4)

head(testError)
pred3error <- sum((testError$pred3-testError$Runs)^2)
pred4error <- sum((testError$pred4-testError$Runs)^2)
pred3error
pred4error

scaleParam <- rbind(attr(scaling, 'scaled:center'), attr(scaling, 'scaled:scale'))
t(scaleParam)

save(scaling, scaleParam, file='pfxMod13scaling.Rdat')

# try the model on 60% of the data
fit5.start <- lm(Runs ~ B1 + B2 + B3 + HR + Walk + SO, data=subset(trdf, trainSet=='train' & startPitcher=='Start'))
fit5.relief <- lm(Runs ~ B1 + B2 + B3 + HR + Walk + SO, data=subset(trdf, trainSet=='train' & startPitcher=='Relief'))
pred5start <- predict(fit5.start, trdf)
pred5relief <- predict(fit5.relief, trdf)
testError <- cbind(trdf, pred5start)
testError <- cbind(testError, pred5relief)
testError <- transform(testError, pred=((startPitcher=='Start')*pred5start + (startPitcher=='Relief') * pred5relief))
head(testError)
plotError <- ggplot(data=testError, aes(x=Runs, y=pred5relief)) + geom_point() + facet_grid(startPitcher ~ trainSet) + geom_abline(slope=1, color='blue')
plotError
summary(fit5.start)
summary(fit5.relief)
pred5error <- sum((testError$pred-testError$Runs)^2)  # squared error
pred5error


fit6.start <- lm(Runs ~ B1 + B2 + B3 + HR + Walk + SO + 0, data=subset(trdf, trainSet=='train' & startPitcher=='Start'))
fit6.relief <- lm(Runs ~ B1 + B2 + B3 + HR + Walk + SO + 0, data=subset(trdf, trainSet=='train' & startPitcher=='Relief'))

pred6start <- predict(fit6.start, trdf)
pred6relief <- predict(fit6.relief, trdf)

testError <- cbind(trdf, pred6start)
testError <- cbind(testError, pred6relief)
testError <- transform(testError, pred=((startPitcher=='Start')*pred6start + (startPitcher=='Relief') * pred6relief))

plotError <- ggplot(data=testError, aes(x=Runs, y=pred4)) + geom_point() + facet_grid( startPitcher ~ trainSet) + geom_abline(slope=1, color='red')
plotError <- ggplot(data=testError, aes(x=Runs, y=pred)) + geom_point() + facet_grid(startPitcher ~ trainSet) + geom_abline(slope=1, color='red')
plotError <- ggplot(data=testError, aes(x=Runs, y=pred5start)) + geom_point() + facet_grid(startPitcher ~ trainSet) + geom_abline(slope=1, color='red')
plotError <- ggplot(data=testError, aes(x=Runs, y=pred5relief)) + geom_point() + facet_grid(startPitcher ~ trainSet) + geom_abline(slope=1, color='red')
plotError
summary(fit6.start)
summary(fit6.relief)
head(testError)
pred6error <- sum((testError$pred-testError$Runs)^2) # squared error
pred6error