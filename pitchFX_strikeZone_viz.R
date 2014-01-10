library(graphics)
library(RColorBrewer)

setwd("/Groups/Data/Projects/Baseball - Rays")

load("pitch09.Rda")
load("players09.Rdat")

player <- "Percival"

brewer.pal(11, "RdYlBu")
buylrd <- c("#313695", "#4575B4", "#74ADD1", "#ABD9E9", "#E0F3F8", "#FFFFBF", "#FEE090", "#FDAE61", "#F46D43", "#D73027", "#A50026") 

pitcher.id <- subset(players09, last_name == player)$id
pitches <- subset(pitch09, pitcher==pitcher.id)


bot<-mean(pitches$sz_bot, na.rm=TRUE)
top<-mean(pitches$sz_top, na.rm=TRUE)


smoothScatter(pitches$pz~pitches$px, nbin=1000, colramp = colorRampPalette(c(buylrd)), nrpoints=Inf, pch="", cex=.7, transformation = function(x) x^.6, col="black", main=paste(player, "Pitch Location"), xlab="Horizontal Location", ylab="Vertical Location") 
lines(c(0.708335, 0.708335), c(bot, top), col="white", lty="dashed", lwd=2) 
lines(c(-0.708335, -0.708335), c(bot, top), col="white", lty="dashed", lwd=2) 
lines(c(-0.708335, 0.708335), c(bot, bot), col="white", lty="dashed", lwd=2) 
lines(c(-0.708335, 0.708335), c(top, top), col="white", lty="dashed", lwd=2) 

