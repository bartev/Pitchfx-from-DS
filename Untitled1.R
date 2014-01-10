## Bartev Vartanian
## 10/16/11

# Needs data frames 
#   atbat, hitchart, pitch, players
# Can be loaded at PfxLoad09DataR or 
# load("pfxDataSample.Rdat")

# Sample data from Pitchfx '09
load("pfxDataSample.Rdat")


head(atbat)
# Convert atbat$score to 1 = TRUE, 0 = FALSE/NA
ab <- atbat
ab$score <- replace(ab$score, is.na(ab$score), FALSE)

abs <- ab$score
abs1 <- replace(abs, is.na(abs), FALSE)
head(abs1)

t <- table(atbat$score, atbat$s)
t
str(ab)
# Try making some linear models
# table of how often scored vs strike count
t <- table(df2$score, df2$s)
pt.cell <- prop.table(t)
pt.row <- prop.table(t, 1)
pt.col <- prop.table(t, 2)
pt.cell
pt.row
t
# Fraction of score/(score + out) vs strike count
pt.col
str(t)
str(df2)

class(pt.col)
colnames(pt.col)
rownames(pt.col)
class(s.score)
s.score <- as.data.frame(s.score)
s.score
pt.col
probScore <- as.data.frame(pt.col)
names(probScore) <- c("score", "s", "Freq")
probScore
probScore$s <- as.integer(probScore$s)
lm1 <- lm(Freq ~ s, data = subset(probScore, score == TRUE))
lm1
summary(lm1)

str(probScore)
probScore

###
# Create a plot of Freq vs # Strikes and the lm (in red)
# Add a column for the predicted values
probScoreT$lm1 <- predict(lm1, newdata = probScoreT)
probScore <- subset(probScore, select = 1:3)
probScoreT
probScoreT <- subset(probScore, score == TRUE)

p <- ggplot(probScoreT, aes(s, Freq))
pg <-  p + geom_line()
pg <- pg + geom_point()
pg <- pg + geom_line(aes(y = lm1), colour = "red")
pg