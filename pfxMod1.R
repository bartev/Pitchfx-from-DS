# Bartev Vartanian
# 2011-10-20
# Model 1
# Look at swinging strikes vs swings


# Load p09 first 
rm(list = ls())
load("pfxData_p09.Rdat")      # processed pitchfx '09 data - full monty
rm(p09.s, p09.z)

# Get a sample of the data, say 25%
p <- p09.m
# p <- p09.m[sample(1:nrow(p09.m), 0.25*nrow(p09.m), replace=FALSE), ]

# freqTable has the frequency of each type of 'des'
freqTable <- table(p[,c('pitcher', 'des')])
freqDf <- as.data.frame(freqTable)
# Alternate method using xtabs
xt <- xtabs(~ pitcher + des, data=p)
xtDf <- as.data.frame(xt)
xt.pt <- prop.table(xt, 1)  # each column contains the Num(Des(i))/Sum(Des(i)) (proportion)
xtDf.pt <- as.data.frame(xt.pt)
head(xt.pt)
sum(xt.pt[1,])



# Look at 'swings & misses' vs. 'swings'

# Get swinging strikes only
ft1 <- subset(freqDf, des == "Swinging Strike", select=c(pitcher, Freq))
names(ft1) <- c('pitcher', 'numSwingStrikes')
# Get number of pitches for each batter
ft2 <- as.data.frame(table(p$pitcher))
names(ft2) <- c('pitcher','totPitches')

# Merge totPitches with numSwingStrikes into a new df
mdf <- merge(ft2, ft1, by='pitcher')
mdf <- transform(mdf, pctSwingStrike = numSwingStrikes/totPitches)

# Alternate method - slower
# Tried doing this with sqldf (more direct), but it took longer than subset
# # Take a few sec
# p09df <- p09.m
# t1 <- sqldf('select pitcher, count(des) from p09df where des = "Swinging Strike" group by pitcher')
# t2 <- sqldf('select pitcher, count(1) from p09df group by pitcher')
# t3 <- merge(t1, t2, by = 'pitcher')
# names(t3) <- c("Pitcher", "SwingStrike", "TotalPitches")

library(ggplot2)
names(mdf)
# Note: I defined 'bluePoints' and 'bestfit' so I can reuse them later (maybe)
# the variable 'bluePoints' can be used to add blue points to a plot
bluePoints <- geom_point(color = 'blue')

# bestfit can be used elsewhere to. Define it as a lm fit with color = red, alpha = 0.5.
bestfit <- geom_smooth(method = "lm", 
                       se = F, colour = alpha("red", 0.5),
                       size = 4)


# Plot out numSwingStrikes vs totPitches
g <- ggplot(mdf, aes(totPitches, numSwingStrikes))
gg <- g + bluePoints
gg

# Linear model, set intercept to 0
# m1 <- lm(mdf$numSwingStrikes ~ mdf$totPitches + 0)
m1 <- lm(numSwingStrikes ~ totPitches + 0, data = mdf)
summary(m1)
# Excellent linear fit. p-value < 2.2e-16
# F-stat = 1.2e4 (663 DOF)

# try the model on 25% of the data
mdf.s <- mdf[sample(1:nrow(mdf), 0.25*nrow(mdf), replace=FALSE), ]
m1s <- lm(numSwingStrikes ~ totPitches + 0, data = mdf.s)
summary(m1s)
mdf$fit <- predict.lm(m1s, mdf)


mdf.s <- cbind(mdf.s, fit=fitted(m1s))
p.m1s <- ggplot(data=mdf.s, aes(x=numSwingStrikes, y=fit)) + geom_point(color='blue') + geom_abline(slope=1)

# Plot the fit(sample) vs numSwingStrikes in blue
# Plot the fit(all data) vs numSwingStrikes in red
# Plot both on the same plot
# http://r.789695.n4.nabble.com/Plotting-from-different-data-sources-on-the-same-plot-with-ggplot2-td835473.html
p.1 <- ggplot(mapping = aes(x=numSwingStrikes, y=fit)) + geom_abline(slope=1) #
p.sample <- geom_point(data=mdf.s, color='blue')
p.all <- geom_point(data=mdf, color='red')
p.1 + p.sample
p.1 + p.all
p.1 + p.all + p.sample


# Plot out the model using geom_abline(intercept=0, slope= (from lm output))
mdfmdf.g1 <- ggplot(mdf, aes(totPitches, numSwingStrikes, colour = pctSwingStrike))
# g1 <- ggplot(mdf, aes(totPitches, numSwingStrikes, colour = resid))
# gg1 <- g1 + geom_point(color = 'red')
gg1 <- g1 + geom_point()
# geom_abline adds a line with intercept=..., slope = ...
gg1 <- gg1 + geom_abline(slope = m1$coef, color = alpha('steelblue', 0.5), size = 2)
gg1 <- gg1 + geom_abline(slope = m1$coef, color = alpha('steelblue', 0.5), size = 6)
# gg1 <- gg1 + geom_abline(slope = m1s$coef, color = alpha('red', 0.5), size = 2)
gg1
ggsave('ggSwingVsPitches1.pdf')
# gg1  <- g +  geom_abline(intercept= 0, slope = m1$coef[1], color = alpha('steelblue', 0.5), size = 6)

# Note: curved line is ggplot's smoothed curve. I think it uses a polynomial model, not sure
gg2 <- g + bluePoints + geom_smooth(color = alpha('steelblue', 0.5))
gg2
ggsave('ggSwingVsPitchesSmooth1.pdf')

# pctSwingStrike is the slope to every point. Higher slope = greater value added
mdf <- transform(mdf, pctSSlm=pctSwingStrike - m1$coef)
# Note to self: resid(m1) gives the same result as m1$residuals
mdf$resid <- resid(m1)
# Higher slope and large residual (upper right quadrant) is greatest value added by swinging strikes
gr <- ggplot(mdf, aes(pctSSlm, resid(m1))) + geom_point()
gr                    

ggsave('ggSwingVsPitchesResidVsSlope.pdf')

op <- par(mfrow=c(2, 2), # 2x2 plots on one plot
          pty = 's')     # square plotting region
plot(m1)
par(op)
# Save data
save(freqDf, mdf, p, freqTable, m1, file='pfxMod1.Rdat')

          
g3 <- ggplot(mdf, aes(pctSwingStrike, resid)) + geom_point()
g3          
          
head(mdf)
str(m1)
as.data.frame(m1$residuals)
m1res <- data.frame(mdf$pctSSlm, m1$residuals)
head(m1res)
# Now look at log relationships
glog <- ggplot(t3, aes(log(TotalPitches), log(SwingStrike)))
glog <- glog + bluePoints + bestfit
m1log <- lm(log(t3$SwingStrike) ~ log(t3$TotalPitches))

# Have to look into why the abline is offset so much
glog + geom_abline(slope = m1log$coef)
m1log



