# pfxMod11.R
# Use dataframe from pfxMod10 - atbat level info to do regressions
# Use logistic models


# 
# This model doesn't give a lot of accuracy. Going to have to drill down further into pitch-by-pitch data
# 
#
# fit <- glm(best ~ log(hgtPit) + pitchBat + diffPitches + numPitches + log(pitchChgPct) + 
#   log(medianInning) + log(numInningsPitched) + log(agePit),
#            data=rdTrain, family=binomial)
#   tp    fn  fp    tn         p           r      fmeas     lift
# 1 61 10044 111 63952 0.3546512 0.006036616 0.01187117 2.603045
# > GetConfusionMatrix(rdTest, fit)
#       TRUE FALSE
# TRUE    61 10044
# FALSE  111 63952


rm(list=ls())
library(ggplot2)
library(sqldf)
library(MASS) # for stepAIC

setwd("~/Rmac/Pitchfx/PitchFX")
load('pfxMod10abpb.Rdat') # df with at bat level data to do regressions on. Includes rankings of pitchers, num different pitches for each at bat, pitcher & batter stats

# m3 <- glm(y ~ pctSwingStrike + ERA + numInningsPitched, data=sedf.s, family=binomial)

rd <- abpb # regression data
rm(abpb)

# try the model on 60% of the data
trainIndices <- sample(1:nrow(rd), 0.6 * nrow(rd), replace=FALSE)
testIndices <- which(! 1:nrow(rd) %in% trainIndices)
rd$trainSet <- 'test'
rd[trainIndices, 'trainSet'] <- 'train'

rd$game_id <- as.factor(rd$game_id)
rd$pitcher <- as.factor(rd$pitcher)
rd$batter <- as.factor(rd$batter)
head(rd)
str(rd)
# Strip out features we aren't interested in.
rd <- subset(rd, , select=c(best, inning, half, num, b, s, o, event, score, runsOnPitcher, pAhead, 
                            bHgt, hgtPit, wgtPit, throwsPit, p_throws, stand, pitchBat, pbSame, 
                            diffPitches, numPitches, pitchChgPct, medianInning, startPitcher,
                            numInningsPitched, agePit, trainSet))

full.lr <- glm(best ~ inning + half + num + b + s + o + event + score + runsOnPitcher +
  pAhead + log(bHgt) + log(hgtPit) + log(wgtPit) + throwsPit + stand + pitchBat + pbSame +
  diffPitches + numPitches + log(pitchChgPct) + log(medianInning) + startPitcher +
  log(numInningsPitched) + log(agePit), 
               data=subset(rd, trainSet=='train'))

full.lr <- glm(best ~ inning + half + num + b + s + o + runsOnPitcher + pAhead + 
  bHgt + hgtPit + wgtPit + throwsPit + p_throws + pitchBat + pbSame +
  diffPitches + numPitches + pitchChgPct + medianInning + startPitcher + 
  numInningsPitched + numAtBats + totPitches + agePit, 
               data=subset(rd, trainSet=='train'))
# Don't include outs 'o', because 'o' is only an indication of the current number of outs. It changes, but doesn't relate to quality of pitcher.
# Only keep pitchBat (4 way factor), not throwsPit, stand)
full.lr <- glm(best ~ inning + half + num + b + s + score + runsOnPitcher +
  pAhead + log(bHgt) + log(hgtPit) + log(wgtPit) + pitchBat + pbSame +
  diffPitches + numPitches + log(pitchChgPct) + log(medianInning) + startPitcher +
  log(numInningsPitched) + log(agePit), 
               data=subset(rd, trainSet=='train'))

full.lr <- glm(best ~ half, data=subset(rd, trainSet=='train'))
summary(full.lr)


rdTrain <- subset(rd, trainSet=='train')
rdTest <- subset(rd, trainSet != 'train')
fit <- glm(best ~ 1, data=rdTrain, family=binomial)
fit <- glm(best ~ inning, data=rdTrain, family=binomial)
fit <- glm(best ~ half, data=rdTrain, family=binomial)
fit <- glm(best ~ num, data=rdTrain, family=binomial)
fit <- glm(best ~ b, data=rdTrain, family=binomial)
fit <- glm(best ~ s, data=rdTrain, family=binomial)
fit <- glm(best ~ score, data=rdTrain, family=binomial)
fit <- glm(best ~ runsOnPitcher, data=rdTrain, family=binomial)
fit <- glm(best ~ pAhead, data=rdTrain, family=binomial)
fit <- glm(best ~ log(bHgt), data=rdTrain, family=binomial)
fit <- glm(best ~ log(hgtPit), data=rdTrain, family=binomial)
fit <- glm(best ~ log(wgtPit), data=rdTrain, family=binomial)
fit <- glm(best ~ pitchBat, data=rdTrain, family=binomial)
fit <- glm(best ~ pbSame, data=rdTrain, family=binomial)
fit <- glm(best ~ diffPitches, data=rdTrain, family=binomial)
fit <- glm(best ~ numPitches, data=rdTrain, family=binomial)
fit <- glm(best ~ log(pitchChgPct), data=rdTrain, family=binomial)
fit <- glm(best ~ log(medianInning), data=rdTrain, family=binomial)
fit <- glm(best ~ startPitcher, data=rdTrain, family=binomial)
fit <- glm(best ~ log(numInningsPitched), data=rdTrain, family=binomial)
fit <- glm(best ~ log(agePit), data=rdTrain, family=binomial)

fit <- glm(best ~ inning + half + num + b + s, data=rdTrain, family=binomial)
fit <- glm(best ~ half + b + s + score + runsOnPitcher + pAhead + log(bHgt)
           + log(hgtPit) + log(wgtPit) + pitchBat + pbSame + diffPitches + numPitches +
             log(pitchChgPct) + log(medianInning) + startPitcher + log(numInningsPitched) + log(agePit), data=rdTrain, family=binomial)
#    tp   fn  fp    tn         p          r      fmeas     lift
# 1 252 9853 264 63799 0.4883721 0.02493815 0.04745316 3.584521
# Call:
# glm(formula = best ~ half + b + s + score + runsOnPitcher + pAhead + 
#     log(bHgt) + log(hgtPit) + log(wgtPit) + pitchBat + pbSame + 
#     diffPitches + numPitches + log(pitchChgPct) + log(medianInning) + 
#     startPitcher + log(numInningsPitched) + log(agePit), family = binomial, 
#     data = rdTrain)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -1.3972  -0.5236  -0.4167  -0.2643   3.9197  
# 
# Coefficients: (1 not defined because of singularities)
#                         Estimate Std. Error z value Pr(>|z|)    
# (Intercept)            12.452109   2.038866   6.107 1.01e-09 ***
# halftop                -0.038870   0.018404  -2.112  0.03468 *  
# b                      -0.005917   0.015251  -0.388  0.69802    
# s                       0.227240   0.016097  14.117  < 2e-16 ***
# score                   0.072389   0.082590   0.876  0.38077    
# runsOnPitcher          -0.117465   0.060349  -1.946  0.05160 .  
# pAhead                  0.027740   0.002792   9.935  < 2e-16 ***
# log(bHgt)              -0.451509   0.314135  -1.437  0.15063    
# log(hgtPit)            -3.630318   0.372575  -9.744  < 2e-16 ***
# log(wgtPit)             0.024098   0.107429   0.224  0.82251    
# pitchBatL R            -0.113844   0.038313  -2.971  0.00296 ** 
# pitchBatR L            -0.045129   0.034534  -1.307  0.19128    
# pitchBatR R            -0.058506   0.034172  -1.712  0.08688 .  
# pbSameTRUE                    NA         NA      NA       NA    
# diffPitches            -0.267629   0.031366  -8.533  < 2e-16 ***
# numPitches              0.030782   0.016965   1.814  0.06960 .  
# log(pitchChgPct)        0.151951   0.060098   2.528  0.01146 *  
# log(medianInning)       3.082629   0.093702  32.898  < 2e-16 ***
# startPitcherTRUE       -0.187492   0.065807  -2.849  0.00438 ** 
# log(numInningsPitched)  0.941139   0.024651  38.178  < 2e-16 ***
# log(agePit)            -0.645498   0.067910  -9.505  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 89425  on 111184  degrees of freedom
# Residual deviance: 78657  on 111165  degrees of freedom
#   (1240 observations deleted due to missingness)
# AIC: 78697
summary(fit)
l <- GetLift(rdTest, fit); l
l <- GetLift(rd, fit); l
step <- stepAIC(fit, direction='both')
step$anova # display results
#  step$anova # display results
# Stepwise Model Path 
# Analysis of Deviance Table
# 
# Initial Model:
# best ~ half + b + s + score + runsOnPitcher + pAhead + log(bHgt) + 
#     log(hgtPit) + log(wgtPit) + pitchBat + pbSame + diffPitches + 
#     numPitches + log(pitchChgPct) + log(medianInning) + startPitcher + 
#     log(numInningsPitched) + log(agePit)
# 
# Final Model:
# best ~ half + s + runsOnPitcher + pAhead + log(bHgt) + log(hgtPit) + 
#     pitchBat + diffPitches + numPitches + log(pitchChgPct) + 
#     log(medianInning) + startPitcher + log(numInningsPitched) + 
#     log(agePit)
# 
# 
#            Step Df   Deviance Resid. Df Resid. Dev      AIC
# 1                                111165   78657.00 78697.00
# 2      - pbSame  0 0.00000000    111165   78657.00 78697.00
# 3 - log(wgtPit)  1 0.05031216    111166   78657.05 78695.05
# 4           - b  1 0.14904607    111167   78657.20 78693.20
# 5       - score  1 0.79030952    111168   78657.99 78691.99

fit <- glm(best ~ half + s + runsOnPitcher + pAhead + log(bHgt) +
  log(hgtPit) + pitchBat + diffPitches + numPitches + log(pitchChgPct) + 
  log(medianInning) + startPitcher + log(numInningsPitched) + log(agePit), data=rdTrain, family=binomial)
#    tp   fn  fp    tn         p          r      fmeas     lift
# 1 250 9855 266 63797 0.4844961 0.02474023 0.04707655 3.556072

fit <- glm(best ~ half + s + runsOnPitcher + pAhead + 
  log(hgtPit) + pitchBat + diffPitches + numPitches + log(pitchChgPct) + 
  log(medianInning) + startPitcher + log(numInningsPitched) + log(agePit), data=rdTrain, family=binomial)
#    tp   fn  fp    tn         p          r      fmeas     lift
# 1 247 9858 260 63803 0.4871795 0.02444334 0.04655107 3.575767

fit <- glm(best ~ half + s + runsOnPitcher + pAhead + 
  log(hgtPit) + pitchBat + diffPitches + log(pitchChgPct) + 
  log(medianInning) + startPitcher + log(numInningsPitched) + log(agePit), data=rdTrain, family=binomial)
#    tp   fn  fp    tn         p          r      fmeas     lift
# 1 248 9857 273 63790 0.4760077 0.02454231 0.04667796 3.493769

fit <- glm(best ~ s + runsOnPitcher + pAhead + 
  log(hgtPit) + pitchBat + diffPitches + log(pitchChgPct) + 
  log(medianInning) + startPitcher + log(numInningsPitched) + log(agePit), data=rdTrain, family=binomial)
#    tp   fn  fp    tn         p          r      fmeas    lift
# 1 245 9860 266 63797 0.4794521 0.02424542 0.04615674 3.51905

fit <- glm(best ~ s + pAhead + 
  log(hgtPit) + pitchBat + diffPitches + log(pitchChgPct) + 
  log(medianInning) + startPitcher + log(numInningsPitched) + log(agePit), data=rdTrain, family=binomial)
#    tp   fn  fp    tn        p          r      fmeas     lift
# 1 242 9863 266 63797 0.476378 0.02394854 0.04560445 3.496487

fit <- glm(best ~ s + pAhead + 
  log(hgtPit) + pitchBat + diffPitches + log(pitchChgPct) + 
  log(medianInning) + log(numInningsPitched) + log(agePit), data=rdTrain, family=binomial)
#    tp   fn  fp    tn        p         r     fmeas     lift
# 1 261 9844 303 63760 0.462766 0.0258288 0.0489268 3.396578

fit <- glm(best ~ s + pAhead + 
  log(hgtPit) + pitchBat + log(pitchChgPct) + 
  log(medianInning) + log(numInningsPitched) + log(agePit), data=rdTrain, family=binomial)
#    tp   fn  fp    tn         p          r     fmeas     lift
# 1 172 9933 246 63817 0.4114833 0.01702128 0.0326903 3.020177
# Bad - put diffPitches back in

fit <- glm(best ~ s + pAhead + 
  log(hgtPit) + pitchBat + diffPitches + 
  log(medianInning) + log(numInningsPitched) + log(agePit), data=rdTrain, family=binomial)
#    tp   fn  fp    tn         p          r      fmeas     lift
# 1 264 9841 304 63759 0.4647887 0.02612568 0.04947063 3.411425

fit <- glm(best ~ s + pAhead + log(hgtPit) + diffPitches + 
  log(medianInning) + log(numInningsPitched) + log(agePit), data=rdTrain, family=binomial)
#   tp   fn  fp    tn        p          r      fmeas    lift
# 1 263 9842 296 63767 0.470483 0.02602672 0.04932483 3.45322

fit <- glm(best ~ s + pAhead + log(hgtPit) + diffPitches + 
  log(medianInning) + log(numInningsPitched), data=rdTrain, family=binomial)
#    tp   fn  fp    tn         p          r      fmeas     lift
# 1 195 9910 312 63751 0.3846154 0.01929738 0.03675085 2.822974
# age matters?

fit <- glm(best ~ pAhead + log(hgtPit) + diffPitches + 
  log(medianInning) + log(numInningsPitched) + log(agePit), data=rdTrain, family=binomial)
#   tp    fn  fp    tn         p           r      fmeas     lift
# 1 58 10047 112 63951 0.3411765 0.005739733 0.01128954 2.504144
# strike count really matters

#
#
#
# Only look at features specific to the pitcher & batter. Ignore current count/score
fit <- glm(best ~ log(bHgt) + log(hgtPit) + log(wgtPit) + pitchBat + 
  diffPitches + numPitches + log(pitchChgPct) + log(medianInning) + startPitcher + 
  log(numInningsPitched) + log(agePit), data=rdTrain, family=binomial)
#   tp    fn fp    tn         p           r       fmeas     lift 
# 1 47 10058 87 63976 0.3507463 0.004651163 0.009180584 2.574384# AIC: 79194

summary(fit)
l <- GetLift(rdTest, fit); l

step <- stepAIC(fit, direction='both')
step$anova # display results

> step$anova # display results
# Stepwise Model Path 
# Analysis of Deviance Table
# 
# Initial Model:
# best ~ log(bHgt) + log(hgtPit) + log(wgtPit) + pitchBat + diffPitches + 
#     numPitches + log(pitchChgPct) + log(medianInning) + startPitcher + 
#     log(numInningsPitched) + log(agePit)
# 
# Final Model:
# best ~ log(hgtPit) + pitchBat + diffPitches + numPitches + log(pitchChgPct) + 
#     log(medianInning) + log(numInningsPitched) + log(agePit)
# 
# 
#             Step Df  Deviance Resid. Df Resid. Dev      AIC
# 1                                111171   79165.78 79193.78
# 2  - log(wgtPit)  1 0.1143762    111172   79165.90 79191.90
# 3 - startPitcher  1 1.3361801    111173   79167.23 79191.23
# 4    - log(bHgt)  1 1.5022655    111174   79168.73 79190.73
fit <- glm(best ~ log(hgtPit) + pitchBat + diffPitches + numPitches + log(pitchChgPct) + 
  log(medianInning) + log(numInningsPitched) + log(agePit),
           data=rdTrain, family=binomial)
# 
# This model doesn't give a lot of accuracy. Going to have to drill down further into pitch-by-pitch data
# 
# 

#   tp    fn  fp    tn         p           r      fmeas     lift
# 1 61 10044 111 63952 0.3546512 0.006036616 0.01187117 2.603045
# Call:
# glm(formula = best ~ log(hgtPit) + pitchBat + diffPitches + numPitches + 
#     log(pitchChgPct) + log(medianInning) + log(numInningsPitched) + 
#     log(agePit), family = binomial, data = rdTrain)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -1.3150  -0.5141  -0.4264  -0.2629   3.8697  
# 
# Coefficients:
#                        Estimate Std. Error z value Pr(>|z|)    
# (Intercept)             9.63192    1.48342   6.493 8.41e-11 ***
# log(hgtPit)            -3.53042    0.31789 -11.106  < 2e-16 ***
# pitchBatL R            -0.14159    0.03789  -3.737 0.000186 ***
# pitchBatR L            -0.07421    0.03418  -2.171 0.029916 *  
# pitchBatR R            -0.07125    0.03392  -2.100 0.035689 *  
# diffPitches            -0.09763    0.02793  -3.496 0.000473 ***
# numPitches              0.02614    0.01565   1.670 0.094943 .  
# log(pitchChgPct)       -0.13027    0.05366  -2.428 0.015191 *  
# log(medianInning)       3.38398    0.04055  83.456  < 2e-16 ***
# log(numInningsPitched)  0.96864    0.02361  41.027  < 2e-16 ***
# log(agePit)            -0.68027    0.06728 -10.111  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 89425  on 111184  degrees of freedom
# Residual deviance: 79169  on 111174  degrees of freedom
#   (1240 observations deleted due to missingness)
# AIC: 79191
# > GetConfusionMatrix(rdTest, fit)
#       TRUE FALSE
# TRUE    61 10044
# FALSE  111 63952
summary(fit)
l <- GetLift(rdTest, fit); l
GetConfusionMatrix(rdTest, fit)

step <- stepAIC(fit, direction='both')
step$anova # display results
