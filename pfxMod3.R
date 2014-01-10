# 2011/11/2
# Bartev Vartanian
# Find ER & ERA
# Cluster analysis

# Results
# Use Kmeans on ERA & pctSwingingStrikes to cluster (5 clusters)
# Put these results in piterdf

setwd("~/Rmac/Pitchfx/PitchFX")
library(ggplot2)
# select(unique game_id, inning & pitcher) from atbat.m
load('pfxData_atbatMod.Rdat') # 5000 row and all rows of modified atbat data
ab <- ab09.m
# ab <- atbat.m
head(ab)

# Table of pitchers vs inning number they pitched in (only > 9th inning)
t <- table(subset(ab, inning > 9, select=c(pitcher, inning)))
t <- table(subset(ab, , select=c(pitcher, inning)))
tdf <- as.data.frame(t)


# Freq distribution showing what how often a pitcher pitched in an inning
qplot(x=inning, y=Freq, data=tdf)
qplot(x=inning, y=Freq, data=tdf, geom='boxplot')
qplot(Freq, data=tdf, geom='histogram', colour=inning)
qplot(inning, data=tdf, geom='density', colour='Freq') # looks cool, doesn't mean anything!


# How many batters did each pitcher pitch to?
up <- subset(ab, , select=c(pitcher, game_id, inning, batter))
t <- with(up, table(pitcher))
# t <- table(up$pitcher) # use with(up, table(pitcher)) - keeps label 'pitcher' on 1st column
df <- as.data.frame(t)
names(df)[2] <- 'numAtBats'
head(df)
sum(df$numAtBats)


# find who pitched in what inning
up <- subset(ab, , select=c(pitcher, game_id, inning))
upp <- unique(up)
# Table showing pitcher number vs. inning number he pitched in
# Dataframe showing pitcherID, inningNumber, Frequency. 
# If sum up Freq grouped by pitcherID, will get numInningsPitched

# freqTable has the frequency of each 'inning'
# upp is a set of unique pitcher, game_id, inning combinations
freqTable <- table(upp[, c('pitcher', 'inning')])
freqDf <- as.data.frame(freqTable)
head(freqDf)
innPitchedTable <- margin.table(freqTable, 1)
innPitchedDf <- as.data.frame(innPitchedTable)
names(innPitchedDf)[2] <- 'numInningsPitched'
head(innPitchedDf)
# Sort in reverse order of number of innings pitched
innPitchedDf <- innPitchedDf[with(innPitchedDf, order(-numInningsPitched)),]
sum(innPitchedDf$numInningsPitched)
qplot(numInningsPitched, data=innPitchedDf)

# Num of atbat's/number of pitcher-inning combos
# 3.87 batters per inning pitched - not too useful
sum(df$numAtBats)/sum(innPitchedDf$numInningsPitched)

head(ab)
###########
# Find ERA
library(sqldf)
eraDf <- sqldf('select pitcher, sum(runsOnPitcher) ER from ab group by pitcher')
# medianDf gives the median inning the pitcher pitched in. 
# Lower median indicates starter
# Higher median indicates closer.
medianDf <- sqldf('select pitcher, median(inning) medianInning from upp group by pitcher')
medianDf <- transform(medianDf, startPitcher=(medianInning <= 5))
# modeDf <- sqldf('select pitcher, mode(inning) modeInning from upp group by pitcher') # how to calculate mode of a distribution in R?



#
#
#
# Linear model, set intercept to 0
# m1 <- lm(mdf$numSwingStrikes ~ mdf$totPitches + 0)
m1 <- lm(numSwingStrikes ~ totPitches + 0, data = mdf)
# Excellent linear fit. p-value < 2e-16 for both

##### Aside - relook at model 1 conditioned on starting vs relief pitcher
mdf.sr <- merge(mdf, subset(medianDf, select=c(pitcher, startPitcher)))
ml.sr.true <- lm(numSwingStrikes ~ totPitches + 0, data=subset(mdf.sr, startPitcher==TRUE))
ml.sr.false <- lm(numSwingStrikes ~ totPitches + 0, data=subset(mdf.sr, startPitcher!=TRUE))
summary(ml.sr.true) # estimate = 0.076620
summary(ml.sr.false) # estimate = 0.089873

# Plot out numSwingStrikes vs totPitches
bluePoints <- geom_point(color = 'blue')
g <- ggplot(mdf.sr, aes(totPitches, numSwingStrikes))
gg <- g + bluePoints + 
  facet_grid(.~startPitcher) + 
  geom_abline(slope=coef(ml.sr.true), color='red') +  # starting pitchers
  geom_abline(slope=coef(ml.sr.false), color='green') # relief pitchers

gg



#
#
#
# Start building pitcher specific data frame
#
#
load('pfxMod1.Rdat') # gets mdf from model 1
pitcherdf <- merge(eraDf, innPitchedDf, all = TRUE)
pitcherdf <- transform(pitcherdf, ERA=9*ER/numInningsPitched)
pitcherdf <- merge(pitcherdf, df, all = TRUE)
pitcherdf <- merge(pitcherdf, medianDf, all = TRUE)
pitcherdf <- merge(pitcherdf, subset(mdf, , select=c(pitcher, totPitches, numSwingStrikes)), all = TRUE)
pitcherdf <- transform(pitcherdf, pctSwingStrike=numSwingStrikes/totPitches)
pitcherdf$pitcher <- as.factor(pitcherdf$pitcher)

# innPitchedDf shows how many innings a pitcher pitched in

eraDf <- merge(eraDf, innPitchedDf, all=TRUE)
eraDf <- transform(eraDf, ERA=9*ER/numInningsPitched)
eraDf <- merge(eraDf, medianDf, all=TRUE)

# Very high ERA
erh <- subset(eraDf, ERA > quantile(ERA, 0.75))
dim(erh)
# Very low ERA
erl <- subset(eraDf, ERA < quantile(ERA, 0.25))
dim(erl)

# Save data
save(eraDf, mdf.sr, df, freqDf, tdf, up, upp, freqTable, innPitchedTable, file='pfxMod3.Rdat')
save(pitcherdf, file='pfxMod3pitcherdf.Rdat')

# compare ERA & swingingStrikes & totalPitches
load('pfxMod1.Rdat')
head(mdf)
dim(mdf)

swingAndEraDf <- merge(mdf.sr, eraDf)
dim(swingAndEraDf)
head(swingAndEraDf)

# Plot Pct swinging strike vs total pitches
# Upper right quadrant is best
# This color scale is'nt very useful becuase of the large range of ERA
p <- ggplot(data=subset(swingAndEraDf, ERA < 9), aes(x=totPitches, y=pctSwingStrike, colour = ERA))           
p + geom_point()

p.1 <- ggplot(data=subset(swingAndEraDf, ERA < 90), aes(x=totPitches, y=pctSwingStrike, colour = ERA))           
p.1 + geom_point() + scale_x_continuous(breaks = c(5.5, 6.5))
p.1 <- p.1 + geom_point()
p.1 + scale_colour_gradient(breaks = c(2, 4, 8, 10))



# Don't like this so much
pp <- p + geom_point(aes(y=numSwingStrikes))
pp + geom_point()

# Limit plot to pitchers who've pitched at least 500 pitches.
# BOTTOM RIGHT with RED color is best on this plot
# Draw lines for 1st quartile in ERA
p1 <- ggplot(data=subset(swingAndEraDf, totPitches > 500), aes(x=totPitches, y=ERA, colour = pctSwingStrike))
p1 + geom_point()

# Look at ERA vs pctSwingStrike
# Better pitchers will be clumped in the top left corner of this plot
# It would be useful to group these, perhaps using numInningsPitched as a weighting
ggSubset <- subset(swingAndEraDf, totPitches > 500)
eraQ1 <- quantile(ggSubset$ERA, 0.25)
pssQ3 <- quantile(ggSubset$pctSwingStrike, 0.75)
p2 <- ggplot(data=subset(swingAndEraDf, totPitches > 500), aes(x=ERA, y=pctSwingStrike, colour = numInningsPitched))
p2 + geom_point() + geom_vline(x=eraQ1) + geom_hline(y=pssQ3)
# p2 + geom_point()







# Do some cluster analysis
# From: http://www.statmethods.net/advstats/cluster.html

# Prepare Data
LabelQuantiles <- function(v) {
  a <- 1 + (v>quantile(v, 0.25)) + (v>quantile(v, 0.5)) + (v>quantile(v, 0.75))
}
# sedf = Swing and ERA df
# sedf <- subset(swingAndEraDf, (ERA < 1) & (totPitches > 500), select=c(totPitches, pctSwingStrike, ERA, numInningsPitched))
summary(swingAndEraDf)

sedf <- subset(pitcherdf, (ERA < 9) & (totPitches > 500), select=c(pitcher, pctSwingStrike, ERA, numInningsPitched))
sedf <- subset(pitcherdf, (ERA < 9), select=c(pitcher, pctSwingStrike, ERA, numInningsPitched))
# sedf <- subset(swingAndEraDf, (ERA < 9) & (totPitches > 500), select=c(pctSwingStrike, ERA, numInningsPitched))

p3 <- ggplot(data=sedf, aes(x=ERA, y=pctSwingStrike, colour=numInningsPitched))
p3 + geom_point()

# scale data
# mean = 0, sd = 1
sedf.s <- scale(sedf[,-1])
sedf.m <- attr(sedf.s, 'scaled:center')
sedf.sd <- attr(sedf.s, 'scaled:scale')
savedPars <- as.data.frame(rbind(sedf.m, sedf.sd))
sedf.s <- as.data.frame(sedf.s)



p4 <- ggplot(data=sedf.s, aes(x=ERA, y=pctSwingStrike, colour=numInningsPitched))
eraQ1 <- quantile(sedf.s$ERA, 0.25)
pssQ3 <- quantile(sedf.s$pctSwingStrike, 0.75)

p4 <- p4 + geom_vline(x=0) + geom_hline(y=0)
vertLine <- geom_vline(x=eraQ1, color='blue', size=1, alpha=I(0.5))
horizLine <- geom_hline(y=pssQ3, color='blue', size=1, alpha=I(0.5))
p4 + geom_point() + vertLine + horizLine

# look on log scale
p4.l <- ggplot(data=sedf, aes(x=log(ERA), y=log(pctSwingStrike), colour = log(numInningsPitched)))
eraQ1.l <- log(quantile(sedf$ERA, 0.25))
pssQ3.l <- log(quantile(sedf$pctSwingStrike, 0.75))
vertLine.l <- geom_vline(x=eraQ1.l, color='blue', size=2, alpha=I(0.5))
horizLine.l <- geom_hline(y=pssQ3.l, color='blue', size=2, alpha=I(0.5))
p4.l + geom_point() + vertLine.l + horizLine.l

# Partitioning

# K-means
# Determine number of clusters
wss <- (nrow(sedf.s)-1) * sum(apply(sedf.s, 2, var))
for (i in 2:15) wss[i] <- sum(kmeans(sedf.s, centers=i)$withinss)
plot(1:15, wss, type='b', xlab='Number of Clusters', ylab='within groups sum of squars')

# K-Means Cluster Analysis
fit <- kmeans(sedf.s[,-3], 5) # 3 cluster solution
# The centers change as I run this multiple times.

# get cluster means
aggregate(sedf.s, by=list(fit$cluster), FUN=mean)

names(fit)
fit$centers
fit$totss
fit$withinss
fit$withinss
fit$betweensss
fit$size
head(fit$cluster)
fit$cluster

sedf.s.k5 <- cbind(sedf.s, fit$cluster)
names(sedf.s.k5) <- c('sclPSS', 'sclERA', 'sclNIP', 'ClusterK5')
sedf.clus <- cbind(sedf, sedf.s.k5)
head(sedf.clus)

cplot <- ggplot(data=sedf.clus, aes(x=sclERA, y=sclPSS)) + geom_point(aes(colour=ClusterK5)) + facet_grid(. ~ ClusterK5)
cplot + vertLine + horizLine

# Repeat with ERA, pctSwingStrikes & numInningsPitched
# K-Means Cluster Analysis
fit2 <- kmeans(sedf.s, 5) # 3 cluster solution

sedf.s.k5.2 <- cbind(sedf.s, fit2$cluster)
names(sedf.s.k5.2) <- c('sclPSS', 'sclERA', 'sclNIP', 'ClusterK5')
sedf.clus2 <- cbind(sedf, sedf.s.k5.2)
head(sedf.clus2)

cplot <- ggplot(data=sedf.clus2, aes(x=sclERA, y=sclPSS)) + geom_point(aes(colour=ClusterK5)) + facet_grid(. ~ ClusterK5)
cplot + vertLine + horizLine

sedf.clus$model <- 'Fit1'
sedf.clus2$model <- 'Fit2'
sedf.clus.all <- rbind(sedf.clus, sedf.clus2)
# cm <- xtabs(~ pitcher + ClusterK5, data=sedf.clus.all)
# head(cm)
# cmdf <- data.frame(cm)

cplot <- ggplot(data=sedf.clus.all, aes(x=sclERA, y=sclPSS)) + geom_point(aes(colour=ClusterK5)) + facet_grid(model ~ ClusterK5)
cplot + vertLine + horizLine

temp1 <- subset(sedf.clus, , select=c(pitcher, ClusterK5))
temp2 <- subset(sedf.clus2, , select=c(pitcher, ClusterK5))
names(temp1)[2] <- 'Fit1'
names(temp2)[2] <- 'Fit2'
compareModels <- merge(temp1, temp2)
head(compareModels)

# Table to show how clusters map.
with(compareModels, table(Fit1, Fit2))
#     Fit2
# Fit1  1  2  3  4  5
#    1  0  0 66  0 18
#    2  0 42  5 22 41
#    3 39  0  0  4  0
#    4 10 53 13 11  8
#    5 20 60  0 22  0

head(pitcherdf)
head(sedf.clus.all)

#
#
#
# adjust pitcherdf to add best/fair/worst labels
#
# The clusters changes each time I run it.
# I'm going to sete clusters based on Cluster5 of Fit1 to pitcherdf now
bestPitchers <- subset(sedf.clus.all, model == 'Fit1', select=c(pitcher, ClusterK5))
bestPitchers <- transform(bestPitchers, gbf=ifelse(ClusterK5==5, 'Good', ifelse(ClusterK5==3, 'Bad', 'Fair')))
bestPitchers$best <- bestPitchers$gbf == 'Good'
head(bestPitchers, 20)
with(bestPitchers, table(ClusterK5, best))
with(bestPitchers, table(ClusterK5, gbf))

pitcherdf <- merge(pitcherdf, bestPitchers, all=TRUE)

save(pitcherdf, file='pfxMod3pitcherdf.Rdat') # Save again



# > with(compareModels, table(Fit1, Fit2))
#     Fit2
# Fit1   1   2   3   4   5
#    1  67   0   0   1  18
#    2   0   6  46  36 120
#    3  27  33  27   0  55
#    4   0  50  19   4   5
#    5   0   0   9  90   0

# Hierarchical agglomerative
# Ward Hierarchical clustering
d <- dist(sedf.s, method = 'euclidean') # distance matrix
fit <- hclust(d, method='ward')
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw a dendogram with red borders around the 5 clusters
rect.hclust(fit, k=5, border='red')

head(sedf.s)
dim(sedf.s)

# Model based
install.packages('mclust')
library(mclust)
fit <- Mclust(sedf.s)
plot(fit, sedf.s) # great! see several plots. Choose best?


# Plotting cluster solutions

# K-means clustering with 5 clusters
# Cluster plot against 1st 2 principal components
# vary parameters for most readable graph
library(cluster)
fit <- kmeans(sedf.s, 5)
clusplot(sedf.s, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
names(fit)
fit$centers
fit$cluster
sedf.s
names(fit)
head(sedf.s)

# What do Component 1 & 2 stand for? May be pctSwingSrike & ERA

# Centroid plot against 1st 2 discriminant functions
# install.packages('fpc')
library(fpc)
plotcluster(sedf.s, fit$cluster)


# Validating Cluster Solutions
# comparing 2 cluster solutions
library(fpc)
fit2 <- kmeans(sedf.s, 2)
fit3 <- kmeans(sedf.s, 3)
fit4 <- kmeans(sedf.s, 4)
fit5 <- kmeans(sedf.s, 5)
fit6 <- kmeans(sedf.s, 6)
clusplot(sedf.s, fit5$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
fit2$centers
fit3$centers
fit4$centers
fit5$centers # cluster 4 here fit well
fit6$centers # cluster 3 (same as cluster 4 above)

sedf.s <- cbind(sedf.s, k5Clust=fit5$cluster)
sedf.s <- cbind(sedf.s, k6Clust=fit6$cluster)
sedf.s <- transform(sedf.s, k54k63=(k5Clust==4 & k6Clust==3))

d23 <- cluster.stats(d, fit2$cluster, fit3$cluster)
d24 <- cluster.stats(d, fit2$cluster, fit4$cluster)
d25 <- cluster.stats(d, fit2$cluster, fit5$cluster)

d34 <- cluster.stats(d, fit3$cluster, fit4$cluster)
d35 <- cluster.stats(d, fit3$cluster, fit5$cluster)

d45 <- cluster.stats(d, fit4$cluster, fit5$cluster)




# What I want to do is convert this into a classification problem
# I'm most interested in batters with low ERA, high pctSwingStrikes, that do it consistently
# ERA Quantiles are labeled 1 for highest ERA, 4 for lowest
# PctSwingStrikes and numInningsPitched are 1 for lowest, 4 for highest
# the higher sumQ, the "better" the pitcher
sedf.s <- transform(sedf.s, pssQ=LabelQuantiles(pctSwingStrike), eraQ=(5-LabelQuantiles(ERA)), nipQ=LabelQuantiles(numInningsPitched))
sedf.s <- transform(sedf.s, sumQ=pssQ + eraQ + nipQ)
# Look at how well the kmeans cluster matches up with my ranking
sedf.s.xt <- xtabs(~ k5Clust + sumQ, data=sedf.s)
sedf.s.xt

sedf.s <- transform(sedf.s, y=(pssQ > 2 & eraQ > 2 & nipQ > 2 & sumQ > 8)*1)
head(sedf.s)
sum(sedf.s$y)

# Not sure if this even makes sense to do.
# Is this circular? I've defined what I think to be good pitchers by defining 'y'
# Now I'm doing a logistic regression to model it.
m3 <- glm(y ~ pctSwingStrike + ERA + numInningsPitched, data=sedf.s, family=binomial)
m3
summary(m3)
pchisq(227.72, 430) # very small
pchisq(373.44, 433)
anova(m3, test='Chisq')

# Now try it on all the data
# sedf <- subset(swingAndEraDf, (ERA < 9) & (totPitches > 500), select=c(pctSwingStrike, ERA, numInningsPitched))
sedf.a <- swingAndEraDf
head(sedf.a)
sedf.a <- transform(sedf.a, pssQ=LabelQuantiles(pctSwingStrike), eraQ=(5-LabelQuantiles(ERA)), nipQ=LabelQuantiles(numInningsPitched))
sedf.a <- transform(sedf.a, sumQ=pssQ + eraQ + nipQ)
sedf.a <- transform(sedf.a, y=(pssQ > 2 & eraQ > 2 & nipQ > 2 & sumQ > 8)*1)
head(sedf.a)

# WARNING HERE
# Fit is very different from m3 ?!? why?
m3.a <- glm(y ~ pctSwingStrike + ERA + numInningsPitched, data=sedf.a, family=binomial)
m3.a

sedf.a <- cbind(sedf.a, predLogit=predict(m3.a))
sedf.a <- cbind(sedf.a, predFit=fitted(m3.a))
sedf.a <- transform(sedf.a, yPred=1*(predLogit >= 0))
# NOTE: fitted(fit) = sigmoid(pred.logit)

# Accuracy
xt.sedfa <- xtabs(~ y + yPred, data=sedf.a)  # better - used data= , gives better headers
# how often does the model correctly classify for a given y?
# pretty good for y=0, pretty bad for y=1 - a lot of false negatives
prop.table(xt.sedfa, 1) # P(yPred=0 | y=0) ~ 0.95, P(yPred=1 | y=1) ~ 0.5
prop.table(xt.sedfa, 2) # P(y=0 |yPred = 0) ~ 0.88, P(y=1|yPred=1) ~ 0.71
sum(diag(prop.table(xt.sedfa))) # ~ 86% accuracy overall
head(sedf.a)

ggplot(data=sedf.a, aes(x=predLogit, y=predFit)) + geom_point(alpha = I(1/3)) + facet_grid(. ~ y)
p <- ggplot(data=sedf.a, aes(x=predLogit, y=predFit))
p + facet_grid(. ~ sumQ) + geom_boxplot()
p + facet_grid(pssQ ~ eraQ) + geom_point() + geom_hline(y=0.5)
p + facet_grid(pssQ ~ nipQ) + geom_point() + geom_hline(y=0.5)

dim(sedf.a)
str(sedf.a)

# fit sedf.s using only ERA & pctSwingStrike
m3.a <- glm(y ~ pctSwingStrike + ERA + numInningsPitched, data=sedf.a, family=binomial)
sedf.s.fit1 <- glm(k54k63 ~ pctSwingStrike + ERA + numInningsPitched, data=sedf.s, family=binomial)
sedf.s.fit1
anova(sedf.s.fit1, test='Chisq')
pchisq(51.06, 430)
summary(sedf.s.fit1)

sedf.s.fit2 <- glm(k54k63 ~ pctSwingStrike + ERA, data=sedf.s, family=binomial)
summary(sedf.s.fit2)