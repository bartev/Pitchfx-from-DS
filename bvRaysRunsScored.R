# Look at how many runs scored

head(atbat09, 50)

# Get list of games
uniqueGamesId <- sqldf("select distinct game_id from atbat09")
nrow(uniqueGamesId)

gameId <- uniqueGamesId[1,1]
head(uniqueGamesId)
gameId

gamesPitched <- sqldf(paste("select distinct game_id, pitcher from atbat09 where pitcher = ", pid[1,1]))
pitchersInThisGame <- sqldf(paste("select distinct pitcher from atbat09 where game_id = '", gameId, "'"))
nrow(pitchersInThisGame)
head(atbat09)
pitchersInThisGame
paste("select distinct pitcher from atbat09 where game_id =", gameId)

sqldf("select distinct game_id from atbat09 where game_id like '2009/04/06/chnmlb-houmlb-1'")

d <- sqldf(paste("select distinct pitcher, game_id from atbat09 where game_id like %", as.character(gameId), "%"))
head(d)
nrow(d)
gameId

f <- function(s) {
  s2 <- gsub( "(-)", "\\", s)
  return (s2)
}
gid <- strsplit(gameId, "c")
?strsplit

gameId; f(gameId)