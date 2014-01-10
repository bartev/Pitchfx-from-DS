# pfxMod13 continued

# trdf is the scaled per inning data for hits, outs, b1, b2, etc.
# when looking at runs, B1, B2, B3, HR Walks all contribute positively, SO (strikeouts) contribute negatively (see pfxMod13)

# How do Ground, Fly, Line & Pop contribute?

# hit type model (htm) .s/.r for start/relief
# not very informative
# each of these is already an out (Ground, Fly, Line, Pop). I don't have data on where the ball was fielded
htm1.s <- lm(Runs ~ Ground + Fly + Line + Pop, data=subset(trdf, trainSet=='train' & startPitcher=='Start'))
htm1.r <- lm(Runs ~ Ground + Fly + Line + Pop, data=subset(trdf, trainSet=='train' & startPitcher=='Relief'))
summary(htm1.s)
step <- stepAIC(htm1.s, direction='both')
step$anova # display results

htm2.s <- lm(Runs ~ SO, data=subset(trdf, trainSet=='train' & startPitcher=='Start'))
summary(htm2.s)

pred1.s <- predict(htm1.s, trdf)
pred1.r <- predict(htm1.r, trdf)

testError <- cbind(trdf, pred1.s, pred1.r)
testError <- transform(testError, pred=((startPitcher=='Start')*pred1.s + (startPitcher=='Relief') * pred1.r))

plotError <- ggplot(data=testError, aes(x=Runs, y=pred)) + geom_point() + facet_grid(startPitcher ~ trainSet) + geom_abline(slope=1, color='red')
plotError

summary(htm1.s)
summary(htm1.r)
head(testError)
pred6error <- sum((testError$pred-testError$Runs)^2) # squared error
pred6error

htm1.s

lmb1 <- lm(B1 ~ Ground, data=subset(trdf,trainSet=='train' & startPitcher == 'Start'))
lmb2 <- lm(B2 ~ Ground, data=subset(trdf,trainSet=='train' & startPitcher == 'Start'))
lmb3 <- lm(B3 ~ Ground, data=subset(trdf,trainSet=='train' & startPitcher == 'Start'))
summary(lmb1)
summary(lmb2)
summary(lmb3)
toplot <- cbind(trdf, pred=predict(lm1, trdf))
p <- ggplot(data=toplot, aes(x=Ground, y=B1)) + geom_point() + facet_grid(startPitcher ~ trainSet) + geom_abline(slope=1, color='green'); p
p1 <- geom_point(aes(y=B1))
p + p1

add2plot <- geom_point()
ggplot(data=toplot, aes(x=SO, y=Runs)) + add2plot + facet_grid(.~startPitcher)
ggplot(data=toplot, aes(x=B1, y=Runs)) + add2plot + facet_grid(.~startPitcher)
ggplot(data=toplot, aes(x=B2, y=Runs)) + add2plot + facet_grid(.~startPitcher)
ggplot(data=toplot, aes(x=B3, y=Runs)) + add2plot + facet_grid(.~startPitcher)
ggplot(data=toplot, aes(x=HR, y=Runs)) + add2plot + facet_grid(.~startPitcher)
ggplot(data=toplot, aes(x=Walk, y=Runs)) + add2plot + facet_grid(.~startPitcher)



# try the model on 60% of the data
toRegress <- subset(tdfPerInning, numInningsPitched >=4)
trainIndices <- sample(1:nrow(toRegress), 0.6 * nrow(toRegress), replace=FALSE)
testIndices <- which(! 1:nrow(toRegress) %in% trainIndices)
toRegress$trainSet <- 'test'
toRegress[trainIndices, 'trainSet'] <- 'train'
head(toRegress)
b1.lm.start <- lm(Runs ~ B1, data=subset(toRegress, trainSet=='train' & startPitcher=='Start'))
b1.lm.relief <- lm(Runs ~ B1, data=subset(toRegress, trainSet=='train' & startPitcher=='Relief'))

b2.lm.start <- lm(Runs ~ B2, data=subset(toRegress, trainSet=='train' & startPitcher=='Start'))
b2.lm.relief <- lm(Runs ~ B2, data=subset(toRegress, trainSet=='train' & startPitcher=='Relief'))

b3.lm.start <- lm(Runs ~ B3, data=subset(toRegress, trainSet=='train' & startPitcher=='Start'))
b3.lm.relief <- lm(Runs ~ B3, data=subset(toRegress, trainSet=='train' & startPitcher=='Relief'))

hr.lm.start <- lm(Runs ~ HR, data=subset(toRegress, trainSet=='train' & startPitcher=='Start'))
hr.lm.relief <- lm(Runs ~ HR, data=subset(toRegress, trainSet=='train' & startPitcher=='Relief'))

wk.lm.start <- lm(Runs ~ Walk, data=subset(toRegress, trainSet=='train' & startPitcher=='Start'))
wk.lm.relief <- lm(Runs ~ Walk, data=subset(toRegress, trainSet=='train' & startPitcher=='Relief'))

so.lm.start <- lm(Runs ~ SO, data=subset(toRegress, trainSet=='train' & startPitcher=='Start'))
so.lm.relief <- lm(Runs ~ SO, data=subset(toRegress, trainSet=='train' & startPitcher=='Relief'))


bluePoints <- geom_point(color = 'blue')

modS=b1.lm.start
modR=b1.lm.relief
ggplot(toRegress, aes(x=B1, y=Runs)) + bluePoints + facet_grid( ~ startPitcher) + 
  geom_abline(slope=coef(modS)[2], intercept=coef(modS)[1], color = 'red') +
  geom_abline(slope=coef(modR)[2], intercept=coef(modR)[1], color = 'blue')

modS=b2.lm.start
modR=b2.lm.relief
ggplot(toRegress, aes(x=B2, y=Runs)) + bluePoints + facet_grid( ~ startPitcher) + 
  geom_abline(slope=coef(modS)[2], intercept=coef(modS)[1], color = 'red') +
  geom_abline(slope=coef(modR)[2], intercept=coef(modR)[1], color = 'blue')

modS=b3.lm.start
modR=b3.lm.relief
ggplot(toRegress, aes(x=B3, y=Runs)) + bluePoints + facet_grid( ~ startPitcher) + 
  geom_abline(slope=coef(modS)[2], intercept=coef(modS)[1], color = 'red') +
  geom_abline(slope=coef(modR)[2], intercept=coef(modR)[1], color = 'blue')

modS=hr.lm.start
modR=hr.lm.relief
ggplot(toRegress, aes(x=HR, y=Runs)) + bluePoints + facet_grid( ~ startPitcher) + 
  geom_abline(slope=coef(modS)[2], intercept=coef(modS)[1], color = 'red') +
  geom_abline(slope=coef(modR)[2], intercept=coef(modR)[1], color = 'blue')

modS=wk.lm.start
modR=wk.lm.relief
ggplot(toRegress, aes(x=Walk, y=Runs)) + bluePoints + facet_grid( ~ startPitcher) + 
  geom_abline(slope=coef(modS)[2], intercept=coef(modS)[1], color = 'red') +
  geom_abline(slope=coef(modR)[2], intercept=coef(modR)[1], color = 'blue')

modS=so.lm.start
modR=so.lm.relief
ggplot(toRegress, aes(x=SO, y=Runs)) + bluePoints + facet_grid( ~ startPitcher) + 
  geom_abline(slope=coef(modS)[2], intercept=coef(modS)[1], color = 'red') +
  geom_abline(slope=coef(modR)[2], intercept=coef(modR)[1], color = 'blue') + 
  geom_text(coef(modS))
