# Bartev Vartanian
# 10/17/11

# Merge pp & atbat data



abb <- atbat
abb$game_id <- gsub("[/-]", "", abb$game_id)
head(abb)




# in atbat, b, s give the final ball & strike count. can compare to b, s in pitch to see probability
# of getting say 3 strikes when have 1, 2 ...6 strikes.

# get a subset of pp
ppSub <- sqldf('select game_id, num, pitcher, batter, b, s, des, id, count, pitchNum, upid, type, Ball, Strike, Hit from pp')
head(ppSub)
abb$count <- paste(abb$b, abb$s)
abSub <- sqldf('select game_id, half, inning, num, b as bFinal, s as sFinal, o as oFinal, score, batter, pitcher, des, event, home_team_runs, away_team_runs, count from abb')
head(abSub)

ppSub <- ppSub[1:5,]
abSub <- abSub[1:5,]
ppSub
abSub
all <- sqldf('select ppSub$game_id from ppSub, abSub where ppSub$game_id = abSub$game_id')

a <- sqldf('select ppSub$game_id  as gid from abb')
head(a)
all <- merge(abb, pp, by = c("game_id"))

head(all)

head(abb)

abb$count <- as.factor(abb$count)
some <- sqldf('select * from abb where s = 3')
someBalls <- sqldf('select * from abb where b = 4')
head(some)

head(someBalls)

cnt <- abb$count
cnt
cnt <- as.factor(cnt)

levels(cnt)

puni <- sort(pp, reverse = TRUE)

head(hitchart)
head(pp)
head(abb)


# Try some sqldf to group by game_id & batter number
output <- sqldf('select  *
                from ppSub
                group by game_id, num')
# Now try to join this with atbat



head(output, 100)
head(atbat)
head(output)
output
m <- merge(output, abb, by = c("game_id", "num"))
head(m)

head(atbat)

head(output)
head(m)

A <- output[1:10,]
B <- abb[1:10, ]
ms <- sqldf('select A.*, B.* from A inner join B using(game_id, num)')
head(ms)
ms