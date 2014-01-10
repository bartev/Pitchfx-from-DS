# Use clustering from pfxMod3 to do logistic regression

load('pfxMod3pitcherdf.Rdat') # stats summed up by pitcher


# Not sure if this even makes sense to do.
# Is this circular? I've defined what I think to be good pitchers by defining 'y'
# Now I'm doing a logistic regression to model it.
# m3 <- glm(y ~ pctSwingStrike + ERA + numInningsPitched, data=sedf.s, family=binomial)
# m3
# summary(m3)
# pchisq(227.72, 430) # very small
# pchisq(373.44, 433)
# anova(m3, test='Chisq')
# 
# sedf.a <- cbind(sedf.a, predLogit=predict(m3.a))
# sedf.a <- cbind(sedf.a, predFit=fitted(m3.a))
# sedf.a <- transform(sedf.a, yPred=1*(predLogit >= 0))
# NOTE: fitted(fit) = sigmoid(pred.logit)


pdf <- pitcherdf
# pdf$best <- 1*pdf$best
head(pdf)
m1 <- glm(best ~ medianInning + factor(startPitcher) + numInningsPitched + ERA + pctSwingStrike, 
          data=pdf, family=binomial)

m1 <- glm(best ~ medianInning, data=pdf, family=binomial)
summary(m1)
pdf1 <- cbind(pdf, predLogit1=predict(m1, pdf))
pdf1 <- transform(pdf1, yPred=predLogit1 >= 0)
conmatrix1 <- with(pdf1, xtabs(~ yPred + best))
conmatrix1
nrow(subset(pdf1, best==TRUE))
nrow(subset(pdf1, yPred==TRUE))
nrow(subset(pdf1, predLogit1 >= 0))

head(fitted(m1))
range(pdf1$predLogit1)
summary(pdf1$yPred)
head(pdf1)
sig <- function(x) { 1/(1 + exp(-x)) }

#
#
# Model 2
m2 <- glm(best ~ ERA, data= pdf, family=binomial)
summary(m2)
pdf2 <- cbind(pdf, predLogit2=predict(m2, pdf))
pdf2 <- transform(pdf2, yPred=predLogit2 >= 0)
conmatrix2 <- with(pdf2, xtabs(~ yPred + best))
conmatrix2

#
#
# Model 3
m3 <- glm(best ~ pctSwingStrike, data= pdf, family=binomial)
summary(m3)
pdf3 <- cbind(pdf, predLogit3=predict(m3, pdf))
pdf3 <- transform(pdf3, yPred=predLogit3 >= 0)
conmatrix3 <- with(pdf3, xtabs(~ yPred + best))
conmatrix3

#
#
# Model 4
m4 <- glm(best ~ ERA + pctSwingStrike, data= pdf, family=binomial)
summary(m4)
pdf4 <- cbind(pdf, predLogit4=predict(m4, pdf))
pdf4 <- transform(pdf4, yPred=predLogit4 >= 0)
conmatrix4 <- with(pdf4, xtabs(~ yPred + best))
conmatrix4


prec <- function(cm) {
  tn <- cm[[1]]; fp <- cm[[3]];
  fn <- cm[[2]]; tp <- cm[[4]];
  p <- tp/(tp + fp);
  r <- tp/(tp + fn);
  fmeas <- 2*p*r/(p + r);
  predRate <- (tp + fp)/(tp + fp + tn + fn);
  lift <- r/predRate;
  result <- data.frame(tp=tp, fn=fn, fp=fp, tn=tn, p=p, r=r, fmeas=fmeas, lift=lift);
}
# Look at confusion matrices' results in a dataframe
cm3.prec <- prec(conmatrix3)
cm2.prec <- prec(conmatrix2)
cm1.prec <- prec(conmatrix1)
cm4.prec <- prec(conmatrix4)

# put all the confusion matrices together in 1 df
cm.prec <- data.frame(model=2:4)
temp <- rbind(cm2.prec, cm3.prec)
temp <- rbind(temp, cm4.prec)
cm.prec <- cbind(cm.prec, temp)
cm.prec

#
#
# Back to looking at models

#
#
# Model 5
m5 <- glm(best ~ ERA + pctSwingStrike + medianInning + totPitches + numInningsPitched, data= pdf, family=binomial)
summary(m5)
pdf5 <- cbind(pdf, predLogit5=predict(m5, pdf))
pdf5 <- transform(pdf5, yPred=predLogit5 >= 0)
conmatrix5 <- with(pdf5, xtabs(~ yPred + best))
conmatrix5
cm5.prec <- prec(conmatrix5)
cm5.prec <- data.frame(model=5, cm5.prec)
cm.prec <- rbind(cm.prec, cm5.prec)
cm.prec

#
#
# Model 6
m6 <- glm(best ~ ERA + pctSwingStrike + totPitches + numInningsPitched, data= pdf, family=binomial)
summary(m6)
pdf6 <- cbind(pdf, predLogit6=predict(m6, pdf))
pdf6 <- transform(pdf6, yPred=predLogit6 >= 0)
conmatrix6 <- xtabs(~yPred + best, data=pdf6)  # Better way to write this instead of using with(...)
conmatrix6
cm6.prec <- prec(conmatrix6)
cm6.prec <- data.frame(model=6, cm6.prec)
cm.prec <- rbind(cm.prec, cm6.prec)
cm.prec


#
#
# Relook at model 4 with normalized parameters
# Model 4
pdf.scale <- pdf
pdf.scale$ERA <- scale(pdf$ERA)
pdf.scale$pctSwingStrike <- scale(pdf$pctSwingStrike)
m4.scale <- glm(best ~ ERA + pctSwingStrike, data= pdf.scale, family=binomial)
summary(m4.scale)
pdf4.scale <- cbind(pdf.scale, predLogit4=predict(m4.scale, pdf.scale))
pdf4.scale <- transform(pdf4.scale, yPred=predLogit4 >= 0)
conmatrix4.scale <- with(pdf4.scale, xtabs(~ yPred + best))
conmatrix4.scale
cm4.scale <- prec(conmatrix4.scale)
cm4.scale