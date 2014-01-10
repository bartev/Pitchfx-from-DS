# find all starting pitchers
head(pitch09, 30)

# Step 
# 1: get list of unique games
# 2: get 1st pitcher for each game
# 3: get 2nd pitcher for each game (confirm team vs roster)?
# 4: Get df of all pitches thrown by 1st pitcher
# 5: Sort by game_id (includes date yyyy/mm/dd)
# 6: Sort by num (at bat number)
# 7: Add running count of Ball, Strike, Hit, PitchesThrown, 1B, 2B, 3B, HR


head(atbat09)
# uniqueGames <- sqldf("select distinct game_id from pitch09")
# atbat09 is a small dataframe than pitch09
uniqueGames <- sqldf("select distinct game_id from atbat09")

# 2434 unique games
dim(uniqueGames)

startPitcherHome <- sqldf("select distinct pitcher from atbat09 where half = 'top' and inning = 1")
startPitcherAway <- sqldf("select distinct pitcher from atbat09 where half = 'bottom' and inning = 1")
startPitcher <- sqldf("select distinct pitcher, game_id from atbat09 where inning = 1")

head(startPitcher, 20)
dim(startPitcherHome)
dim(startPitcherAway)
# 310 starting pitchers during the season
dim(startPitcher)

pitch1 <- startPitcher[1,]
pitch1

head(atbat09)

###
# How many games did each pitcher pitch?
table(startPitcher$pitcher)

numGamesPitched <- table(startPitcher$pitcher)
head(numGamesPitched)
str(startPitcher)
startPitcher$pitcher <- as.factor(startPitcher$pitcher)
class(numGamesPitched)

ngp <- as.data.frame(numGamesPitched)
names(ngp)[1] <- "pitcher"

# Build up plot
p <- ggplot(gp, aes(Freq))  # use Freq along x axis, gp as data source
pg <- p + geom_histogram(breaks = seq(-0.5, 35.5, 1)) # create histogram, breakpoints on the 0.5
pg <- pg + ylab("Number of pitchers that pitched x games")  # change y-axis label
pg  # display label

# gp is numGamesPitched in descending order of frequency
ngp <- ngp[order(ngp$Freq, decreasing=TRUE),]
head(ngp)
tail(ngp)
summary(ngp$Freq)

###
# Shows Median games pitched is 13.5 (mean = 15.74)


###
# How about number of pitches thrown by each pitcher?
