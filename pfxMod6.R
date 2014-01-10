# Mod6
# Look at (the number of runs on a pitcher) vs. (strikeouts)
# look at linear, quadratic, sqrt, log, inverse
# Best fit was pctRuns vs sqrt(pctK)?

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

table(eraDf$startPitcher)


# look at strikeouts vs runs scored
head(atbat)
levels(atbat$event)

# Add strikeout column
soDf <- subset(atbat, select=c(game_id, pitcher, event, runsOnPitcher, pAhead))
soDf$pitcher <- as.factor(soDf$pitcher)
soDf <- transform(soDf, strikeout=event %in% c('Strikeout', 'Strikeout - DP'))
head(soDf)
rop <- sqldf('select pitcher, sum(runsOnPitcher) runsOnPitcher from soDf group by pitcher')
head(rop)
sbp <- sqldf('select pitcher, sum(strikeout) strikeouts, count(strikeout) numBatters from soDf group by pitcher')
head(sbp)
ropDf <- merge(rop, sbp)
head(ropDf)
# add starter/relief column
eraDf <- transform(eraDf, startPitcher=ifelse(startPitcher, 'Start', 'Relief'))
ropDf <- merge(ropDf, subset(eraDf, select=c(pitcher, startPitcher)))
ropDf <- transform(ropDf, pctRuns=runsOnPitcher/numBatters, pctK=strikeouts/numBatters)

# 
# now have df that looks like this:
#    pitcher runsOnPitcher strikeouts numPitches
# 1  110683            39         52        326
# 2  111492            12          9         85
# 3  111838            20         11        156


#
#
# get a linear model
#
#

# try the model on 25% of the data
trainIndices <- sample(1:nrow(ropDf), 0.6 * nrow(ropDf), replace=FALSE)
testIndices <- which(! 1:nrow(ropDf) %in% trainIndices)
ropDf$trainSet <- 'test'
ropDf[trainIndices, 'trainSet'] <- 'train'


rop.lm <- lm(runsOnPitcher ~ strikeouts + 0, data=ropDf[trainIndices,])
rop.lm
summary(rop.lm)

str(rop.lm)

# df3 <- transform(subset(dfAll, type=='S' & startPitcher=='Start'), fit=predict(strikeS.lm, subset(dfAll, type=='S' & startPitcher=='Start') ))
# plot3 <- ggplot(data=df, aes(x=pitchNum, y=Freq, fit)) + facet_grid(startPitcher ~ type) + geom_point()
# plot3 <- plot3 + geom_line(aes(y=fit), color='red')

ropDf$fit <- predict(rop.lm, ropDf)
p1 <- ggplot(data=ropDf, aes(x=strikeouts, y=runsOnPitcher)) + geom_point() + facet_grid(startPitcher ~ trainSet)
p1 <- p1 + geom_line(aes(y=fit), color='red')
p1

# Plot looks to level off. try a quadratic
rop2.lm <- lm(runsOnPitcher ~ poly(strikeouts, 2), data=ropDf[trainIndices,])
summary(rop2.lm)

ropDf$fit2 <- predict(rop2.lm, ropDf)
p2 <- ggplot(data=ropDf, aes(x=strikeouts, y=runsOnPitcher)) + geom_point() + facet_grid(startPitcher ~ trainSet)
p2 <- p2 + geom_line(aes(y=fit), color='red')
p2 <- p2 + geom_line(aes(y=fit2), color='blue')
p2

# Square root
rop3.lm <- lm(runsOnPitcher ~ strikeouts + sqrt(strikeouts) + 0, data=ropDf[trainIndices,])
summary(rop3.lm)
ropDf$fit3 <- predict(rop3.lm, ropDf)
p2 <- ggplot(data=ropDf, aes(x=strikeouts, y=runsOnPitcher)) + geom_point() + facet_grid(startPitcher ~ trainSet)
p2 <- p2 + geom_line(aes(y=fit), color='red')
p2 <- p2 + geom_line(aes(y=fit2), color='blue')
p2 <- p2 + geom_line(aes(y=fit3), color='green')
p2


# Cube root
ropDf$so3 <- ropDf$strikeout^(1/3)
rop4.lm <- lm(runsOnPitcher ~ sqrt(strikeouts) + so3 + 0, data=ropDf[trainIndices,])
summary(rop4.lm)
ropDf$fit4 <- predict(rop4.lm, ropDf)
p2 <- ggplot(data=ropDf, aes(x=strikeouts, y=runsOnPitcher)) + geom_point() + facet_grid(startPitcher ~ trainSet)
p2 <- p2 + geom_line(aes(y=fit), color='red')
p2 <- p2 + geom_line(aes(y=fit2), color='blue')
p2 <- p2 + geom_line(aes(y=fit3), color='green')
p2 <- p2 + geom_line(aes(y=fit4), color='orange', size=1, alpha=I(0.5))
p2

# log
rop5.lm <- lm(runsOnPitcher ~ strikeouts + log(strikeouts + 1) + 0, data=ropDf[trainIndices,])
summary(rop5.lm)
ropDf$fit5 <- predict(rop5.lm, ropDf)
p2 <- ggplot(data=ropDf, aes(x=strikeouts, y=runsOnPitcher)) + geom_point() + facet_grid(startPitcher ~ trainSet)
p2 <- p2 + geom_line(aes(y=fit), color='red')
p2 <- p2 + geom_line(aes(y=fit2), color='blue')
p2 <- p2 + geom_line(aes(y=fit3), color='green')
p2 <- p2 + geom_line(aes(y=fit4), color='orange', size=1, alpha=I(0.5))
p2 <- p2 + geom_line(aes(y=fit5), color='steelblue', size=1, alpha=I(0.5))
p2

rop6.lm <- lm(runsOnPitcher ~ sqrt(strikeouts) + I(1/(strikeouts + 1)), data=ropDf[trainIndices,])
summary(rop6.lm)
ropDf$fit6 <- predict(rop6.lm, ropDf)
p2 <- ggplot(data=ropDf, aes(x=strikeouts, y=runsOnPitcher)) + geom_point() + facet_grid(startPitcher ~ trainSet)
p2 <- p2 + geom_line(aes(y=fit), color='red')
p2 <- p2 + geom_line(aes(y=fit2), color='blue')
p2 <- p2 + geom_line(aes(y=fit3), color='green')
p2 <- p2 + geom_line(aes(y=fit4), color='orange', size=1, alpha=I(0.5))
p2 <- p2 + geom_line(aes(y=fit5), color='steelblue', size=1, alpha=I(0.5))
p2 <- p2 + geom_line(aes(y=fit6), color='purple', size=1, alpha=I(0.5))
p2


#
#
#
# Try models with percentage runs & K instead of absolute numbers
#
#
#

# Linear
pctRop1.lm <- lm(pctRuns ~ pctK, data=ropDf[trainIndices,])
pctRop1.lm
summary(pctRop1.lm)

ropDf$fitPct1 <- predict(pctRop1.lm, ropDf)
pct1 <- ggplot(data=ropDf, aes(x=pctK, y=pctRuns)) + geom_point() + facet_grid(startPitcher ~ trainSet)
pct1 <- pct1 + geom_line(aes(y=fitPct1), color='red')
pct1

# Polynomial
pctRop2.lm <- lm(pctRuns ~ poly(pctK, 2), data=ropDf[trainIndices,])
ropDf$fitPct2 <- predict(pctRop2.lm, ropDf)
summary(pctRop2.lm)

ropDf$fitPct2 <- predict(pctRop2.lm, ropDf)
pct1 <- ggplot(data=ropDf, aes(x=pctK, y=pctRuns)) + geom_point() + facet_grid(startPitcher ~ trainSet)
pct1 <- pct1 + geom_line(aes(y=fitPct1), color='red')
pct1 <- pct1 + geom_line(aes(y=fitPct2), color='blue')
pct1

# Square root - nice fit t = -7.9
pctRop3.lm <- lm(pctRuns ~ sqrt(pctK), data=ropDf[trainIndices,])
ropDf$fitPct3 <- predict(pctRop3.lm, ropDf)
summary(pctRop3.lm)

ropDf$fitPct3 <- predict(pctRop3.lm, ropDf)
pct1 <- ggplot(data=ropDf, aes(x=pctK, y=pctRuns)) + geom_point() + facet_grid(startPitcher ~ trainSet)
pct1 <- pct1 + geom_line(aes(y=fitPct1), color='red')
pct1 <- pct1 + geom_line(aes(y=fitPct2), color='blue')
pct1 <- pct1 + geom_line(aes(y=fitPct3), color='green')
pct1

# Log
rop5.lm <- lm(runsOnPitcher ~ strikeouts + log(strikeouts + 1) + 0, data=ropDf[trainIndices,])

pctRop4.lm <- lm(pctRuns ~ pctK + log(pctK + 0.001), data=ropDf[trainIndices,])
ropDf$fitPct4 <- predict(pctRop4.lm, ropDf)
summary(pctRop4.lm)

# exponential
rop5.lm <- lm(runsOnPitcher ~ strikeouts + exp(strikeouts) + 0, data=ropDf[trainIndices,])

pctRop5.lm <- lm(pctRuns ~ pctK + exp(pctK), data=ropDf[trainIndices,])
ropDf$fitPct5 <- predict(pctRop5.lm, ropDf)
summary(pctRop5.lm)
ropDf$fitPct5 <- predict(pctRop5.lm, ropDf)
pct1 <- ggplot(data=ropDf, aes(x=pctK, y=pctRuns)) + geom_point() + facet_grid(startPitcher ~ trainSet)
pct1 <- pct1 + geom_line(aes(y=fitPct1), color='red')
pct1 <- pct1 + geom_line(aes(y=fitPct2), color='blue')
pct1 <- pct1 + geom_line(aes(y=fitPct3), color='green')
pct1 <- pct1 + geom_line(aes(y=fitPct5), color='orange')
pct1
