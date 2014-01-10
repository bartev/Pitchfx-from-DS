# Useful functions
# 
# get confusion matrix, and precision
# See pfxMod8

sigmoid <- function(x){
  1/(1 + exp(-x))
}

GetConfusionMatrix <- function(df, model){
  newdf <- cbind(df, predLogit=predict(model, df))
  newdf <- transform(newdf, yPred=predLogit >= 0)
  confusionMatrix <- with(newdf, xtabs(~ best + yPred))
  cm <- Get2x2ConfMatrix(confusionMatrix)  
  return(cm)
}
GetLift <- function(df, model){
  newdf <- cbind(df, predLogit=predict(model, df))
  newdf <- transform(newdf, yPred=predLogit >= 0)
  confusionMatrix <- with(newdf, xtabs(~ best + yPred))
  result <- prec(confusionMatrix)
}

GetConfusionMatrix2 <- function(df, model){
  newdf <- cbind(df, predLogit=predict(model, df))
  newdf <- transform(newdf, yPred=predLogit >= 0)
  confusionMatrix <- with(newdf, xtabs(~ y + yPred))
  cm <- Get2x2ConfMatrix(confusionMatrix)  
  return(cm)
}
GetLift2 <- function(df, model){
  newdf <- cbind(df, predLogit=predict(model, df))
  newdf <- transform(newdf, yPred=predLogit >= 0)
  confusionMatrix <- with(newdf, xtabs(~ y + yPred))
  result <- prec(confusionMatrix)
}

prec <- function(cm) {
#   Get precision, recall, lift & f-measure for 2x2 confusion matrix
  tfmat <- Get2x2ConfMatrix(cm)
  tp <- tfmat[1,1]; fn <- tfmat[1,2];
  fp <- tfmat[2,1]; tn <- tfmat[2,2];
  p <- tp/(tp + fp);
  r <- tp/(tp + fn);
  fmeas <- 2*p*r/(p + r);
  predRate <- (tp + fp)/(tp + fp + tn + fn);
  lift <- r/predRate;
  result <- data.frame(tp=tp, fn=fn, fp=fp, tn=tn, p=p, r=r, fmeas=fmeas, lift=lift);
}


Get2x2ConfMatrix <- function(cm){
#   Convert up to 2x2 confusion matrix into 2x2 matrix with t/f labels
  numrow <- length(rownames(cm))
  numcol <- length(colnames(cm))
  tfmat <- matrix(rep(0, 4), nrow=2, ncol=2, dimnames=list(c('TRUE', 'FALSE'), c('TRUE', 'FALSE')))

  if (numcol == 1) {
    if (colnames(cm)[1] == TRUE)
      tfmat[,1] <- cm[,1]
    else
      tfmat[,2] <- cm[,1]
  }
  else if (numcol == 2)
    if (colnames(cm)[1] == TRUE) {
      tfmat[,1] <- cm[,1]
      tfmat[,2] <- cm[,2]
    }
    else {
      tfmat[,1] <- cm[,2]
      tfmat[,2] <- cm[,1]
    }
  if (rownames(cm)[1] == FALSE) {
    temp <- tfmat[2,]
    tfmat[2,] <- tfmat[1,]
    tfmat[1,] <- temp
  }
  return(tfmat)
}
