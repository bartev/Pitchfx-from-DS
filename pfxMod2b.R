# Continue from pfxMod2.R

pitchData <- pitchData[order(pitchData$seasonPitchNum),]
# Save data
save(pitchData, file='pfxMod2bStart.Rdat')

head(pitchData)

des2GoodAndBad <- levels(pitchData$des2)

SetGoodNeutBad <- function(p){
  des2Bad <- c('Double', 'Hit By Pitch', 'Home Run', 'Single', 'Triple',
               'Walk', 'Automatic Ball', 'Ball', 'Ball In Dirt', 'In play, no out', 
               'In play, run(s)')
  des2Neutral <- c('Batter Interference', 'Catcher Interference', 'Fan interference', 'Field Error',
                   'Intent Walk', 'Intent Ball', 'Pitchout')
  des2Good <- c('Bunt Ground Out', 'Bunt Groundout', 'Bunt Pop Out', 'Double Play', 'Fielders Choice',
                'Fielders Choice Out', 'Fly Out', 'Flyout', 'Force Out', 'Forceout', 'Ground Out',
                'Grounded Into DP', 'Groundout', 'Line Out', 'Lineout', 'Pop Out', 'Called Strike', 
                'Foul Tip', 'Foul', 'Strikeout', 'Sac Bunt', 'Swinging Strike', 'Sac Fly',
                'Foul Bunt', 'Swinging Strike (Blocked)', 'Foul (Runner Going)', 'Missed Bunt',
                'Strikeout - DP', 'Runner Out', 'Sac Fly DP', 'Sacrifice Bunt DP', 'Triple Play',
                'Swinging Pitchout', 'In play, out(s)')
#   p <- transform(p, gnb = ifelse(des2 %in% des2Bad, 'bad', 'none'))
#   p <- transform(p, gnb = ifelse(des2 %in% des2Neutral, 'neut', gnb))
#   p <- transform(p, gnb = ifelse(des2 %in% des2Good, 'good', gnb))
  
  p <- transform(p, gnb = ifelse(des2 %in% des2Bad, 'bad', 
                                 ifelse(des2 %in% des2Neutral, 'neut', 
                                        ifelse(des2 %in% des2Good, 'good', 'none'))))
}
# p <- SetGoodNeutBad(pitchData[1:1000,])
p <- SetGoodNeutBad(pitchData)

# GNB <- function(des2) {
#   
#   des2 <- gsub('Batter Interference', 'Neutral', des2);
#   des2 <- gsub('Catcher Interference', 'Neutral', des2);
#   des2 <- gsub('Fan interference', 'Neutral', des2);
#   des2 <- gsub('Field Error', 'Neutral', des2);
#   des2 <- gsub('Intent Walk', 'Neutral', des2);
#   des2 <- gsub('Intent Ball', 'Neutral', des2);
#   des2 <- gsub('Pitchout', 'Neutral', des2);
#   
#   des2 <- gsub('Bunt Ground Out', 'Good', des2);
#   des2 <- gsub('Bunt Groundout', 'Good', des2);
#   des2 <- gsub('Bunt Pop Out', 'Good', des2);
#   des2 <- gsub('Double Play', 'Good', des2);
#   des2 <- gsub('Called Strike', 'Good', des2);
#   des2 <- gsub('Fielders Choice', 'Good', des2);
#   des2 <- gsub('Fielders Choice Out', 'Good', des2);
#   des2 <- gsub('Fly Out', 'Good', des2);
#   des2 <- gsub('Flyout', 'Good', des2);
#   des2 <- gsub('Force Out', 'Good', des2);
#   des2 <- gsub('Forceout', 'Good', des2);
#   des2 <- gsub('Foul Tip', 'Good', des2);
#   des2 <- gsub('Ground Out', 'Good', des2);
#   des2 <- gsub('Grounded Into DP', 'Good', des2);
#   des2 <- gsub('Groundout', 'Good', des2);
#   des2 <- gsub('Line Out', 'Good', des2);
#   des2 <- gsub('Lineout', 'Good', des2);
#   des2 <- gsub('Pop Out', 'Good', des2);
#   des2 <- gsub('Foul (Runner Going)', 'Good', des2);
#   des2 <- gsub('Foul', 'Good', des2);
#   des2 <- gsub('Foul Bunt', 'Good', des2);
#   des2 <- gsub('Missed Bunt', 'Good', des2);
#   des2 <- gsub('Runner Out', 'Good', des2);
#   des2 <- gsub('Sac Bunt', 'Good', des2);
#   des2 <- gsub('Sac Fly DP', 'Good', des2);
#   des2 <- gsub('Sac Fly', 'Good', des2);
#   des2 <- gsub('Sacrifice Bunt DP', 'Good', des2);
#   des2 <- gsub('Swinging Strike', 'Good', des2);
#   des2 <- gsub('Strikeout - DP', 'Good', des2);
#   des2 <- gsub('Strikeout', 'Good', des2);
#   
#   des2 <- gsub('Double', 'Bad', des2);
#   des2 <- gsub('Hit By Pitch', 'Bad', des2);
#   des2 <- gsub('Home Run', 'Bad', des2);
#   des2 <- gsub('Single', 'Bad', des2);
#   des2 <- gsub('Triple', 'Bad', des2);
#   des2 <- gsub('Walk', 'Bad', des2);
#   des2 <- gsub('Automatic Ball', 'Bad', des2);
#   des2 <- gsub('Ball In Dirt', 'Bad', des2);
#   des2 <- gsub('In play, no out', 'Bad', des2);
#   des2 <- gsub('In play, run(s)', 'Bad', des2);
#   des2 <- gsub('Ball', 'Bad', des2);
#   
#   return(des2)
# }


# Now try logistic model using p$gnb as the result (good, neutral, bad)
gnb <- p$gnb
str(gnb)
names(p.test)

pNoNeut <- subset(p, gnb != 'neut')

p <- subset(p, pzScale < 20)
p.test <- p[sample(1:nrow(p), nrow(p)/100, replace=FALSE),]
p.test <- transform(p.test, goodBad=(gnb=='good'))
fit1 <- glm(goodBad ~ Ball + Strike + Hit, data=p.test, family=binomial())
fit2 <- glm(goodBad ~ b + s + factor(type) + start_speed + pxScale + pzScale + spin_dir + spin_rate, data=p.test, family=binomial())
fit3 <- glm(goodBad ~ factor(type) + b + s + start_speed + pxScale + pzScale, data=p.test, family=binomial())
summary(fit3)
summary(p.test$Hit)
str(p.test$type)
pchisq(1842, 7164)
summary(p.test$pzScale)

bad.pzScale <- subset(p.test, pzScale > 2)
bad.pzScale2 <- subset(p09.m, sz_top <= sz_bot)
good.pzScale <- subset(p09.m, sz_top > sz_bot)