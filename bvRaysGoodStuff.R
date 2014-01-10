# load pitch data environment (I downloaded to local drive)
gsetwd("/Users/bartev/Rmac/Baseball - Rays")

# start fresh
# rm(list = ls())

# Shows all players' personal stats (name, DOB, size, etc)
load("players09.Rda")
# head(players09)
# dim(players09)    # 1,827 players

# Load the 'pitch09' data - big file, ~ 45MB
# Pitch data from each game - 717,409 lines!
load("pitch09.Rda")
# head(pitch09)
# dim(pitch09)

# Not sure what this is. only 2434 lines
load("hitchart09.Rda")
head(hitchart09)
# rm(hitchart09)
# dim(hitchart09)
# str(hitchart09)

# Description of each pitch result (187,553 lines)
# event field
load("atbat09.Rda")
dim(atbat09)
head(atbat09, 25)
str(atbat09)

# Allows me to run SQL queries
library(sqldf)

# Create all possible tuples
# Getting an error reading from a file. Ask chat room?
# df <- read.csv.sql(file = "TeamNames.csv", sql = "select League, Division, PfxTeam, TeamName from file")

# List of team names, League, Division, Pfx abbreviation, MLB abbreviation, city/state, ballpark name
teamNames <- read.csv("TeamNames.csv")
teamNamesShort <- teamNames[, c("League", "Division", "PfxTeam", "TeamName")]
# Equivalent method using sqldf
# teamNamesShortSql <- sqldf("select League, Division, PfxTeam, TeamName from teamNames")

# head(allPitchers.div)
# 
# Add League, Division, TeamName to players09
colNames <- c("team", "id", "League")
players09.more <- sqldf("select * from players09, teamNamesShort where team = PfxTeam")
# Similar to above sqldf statement
# Difference is, in the sql statement, the PfxTeam column showed up too
# allPitchers.div <- merge(players09, teamNamesShort, 
#   by.x = "team", by.y = "PfxTeam")
# head(allPitchers.div)
# head(players09.more)


# There was some stuff in between (bvRays20111004a.R)


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