# Before looked at B/S/X vs pitch num
# Now look at B/S/X vs count

head(pitchData)
rm(list=ls())

load('pfxData_atbatMod.Rdat') # 5000 row and all rows of modified atbat data
load('pfxData_p09.Rdat')    # Full monty '09 data, .m, .s, .z
rm(p09.s, p09.z)

# From pfxMod1.R
load('pfxMod1.Rdat') # SwingingStrikes ~ TotalPitches

# From pfxMod3.R
load('pfxMod3.Rdat') # ERA, starting/relief, etc.


# table of b/s/h vs pitch number
p09m <- p09.m
df1 <- sqldf('select type, pitchNum from p09m')
t <- xtabs( ~ type + pitchNum + pitch_type, data=p09m)
head(t)

# Each column shows the probability of a pitch resulting in a Ball, Strike or Hit (X) on the nth pitch to the batter
t.p <- prop.table(t, 2)
t.p <- t(t.p)
tp.df <- as.data.frame(t.p)
# tp.df$pitchNum <- as.integer(tp.df$pitchNum)
# head(tp.df)
# t.p

#
# Great plot showing P(B, S, X | pitchNum)
p1 <- ggplot(data=tp.df, aes(x=pitchNum, y=Freq)) + facet_grid(pitch_type ~ type) + geom_point()
p1
