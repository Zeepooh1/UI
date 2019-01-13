setwd("D:\\faks/ui/seminarska_1/")

regular <- read.table("regular.txt", header = T, sep = ",")

library(plyr)
library(rpart)
library(ipred)
library(CORElearn)
library(randomForest)
library(adabag)
library(e1071)
HWIN <- c()
# insert home wins into vector HWIN
for(r in 1:nrow(regular)){
  if(regular[r,4] > regular[r,19]){
    HWIN[length(HWIN)+1] = 1
  }
  else{
    HWIN[length(HWIN)+1] = 0
  }
}
# insert HWIN into data.frame
regular["HWIN"] <- HWIN

# 20151206  MEM LAL20151206

regular$HPTS <- NULL
regular$APTS <- NULL
regular$H2PM <- c(regular$H2PM/regular$H2PA)
regular$H2PA <- NULL
colnames(regular)[colnames(regular) == 'H2PM'] <- 'H2PC'
regular$H3PM <- c(regular$H3PM/regular$H3PA)
regular$H3PA <- NULL
colnames(regular)[colnames(regular) == 'H3PM'] <- 'H3PC'
regular$HFTM <- c(regular$HFTM/regular$HFTA)
colnames(regular)[colnames(regular) == 'HFTM'] <- 'HFTC'
regular$HFTA <- NULL

regular$A2PM <- c(regular$A2PM/regular$A2PA)
regular$A2PA <- NULL
colnames(regular)[colnames(regular) == 'A2PM'] <- 'A2PC'
regular$A3PM <- c(regular$A3PM/regular$A3PA)
regular$A3PA <- NULL
colnames(regular)[colnames(regular) == 'A3PM'] <- 'A3PC'
regular$AFTM <- c(regular$AFTM/regular$AFTA)
regular$AFTA <- NULL
colnames(regular)[colnames(regular) == 'AFTM'] <- 'AFTC'
regular$SEASON <- NULL
histAvg <- function(inData){
   retFrame <- data.frame(inData[0,])
   for(i in 48:nrow(inData)){
     date <- inData[i, 1]
     date <- as.numeric(as.character(date))
     home <- inData[i, 2]
     away <- inData[i, 13]
     havg <- ddply(inData[inData$HOME == home & inData$DATE < date,], ~HOME, summarise, H2PC=mean(H2PC), H3PC=mean(H3PC), HFTC=mean(HFTC), HORB=mean(HORB), HDRB=mean(HDRB), HAST=mean(HAST), HSTL=mean(HSTL), HTOV=mean(HTOV), HBLK=mean(HBLK), HPF=mean(HPF))
     aavg <- ddply(inData[inData$AWAY == away & inData$DATE < date,], ~AWAY, summarise, A2PC=mean(A2PC), A3PC=mean(A3PC), AFTC=mean(AFTC), AORB=mean(AORB), ADRB=mean(ADRB), AAST=mean(AAST), ASTL=mean(ASTL), ATOV=mean(ATOV), ABLK=mean(ABLK), APF=mean(APF))
     gavg <- cbind(havg, aavg, HWIN = inData[i, 24])
     retFrame <- rbind(retFrame, gavg)
   }
   
   retFrame
}

#ddply(regular[regular$HOME == home & regular$DATE < date,], ~HOME, summarise, H2PC=mean(H2PC), H3PC=mean(H3PC), HFTC=mean(HFTC), HORB=mean(HORB), HDRB=mean(HDRB), HAST=mean(HAST), HSTL=mean(HSTL), HTOV=mean(HTOV), HBLK=mean(HBLK), HPF=mean(HPF))
#ddply(regular[regular$AWAY == away & regular$DATE < date,], ~AWAY, summarise, A2PC=mean(A2PC), A3PC=mean(A3PC), AFTC=mean(AFTC), AORB=mean(AORB), ADRB=mean(ADRB), AAST=mean(AAST), ASTL=mean(ASTL), ATOV=mean(ATOV), ABLK=mean(ABLK), APF=mean(APF))

b <- histAvg(regular)


b$HOME <- NULL
b$AWAY <- NULL
b$HWIN <- as.factor(b$HWIN)
learn <- b[1:2415,]
test <- b[2416:nrow(b),]

library(CORElearn)

#funkcija za izraèun klasifikacijsko toènost
CA <- function(pred, observed){
  t <- table(pred, test$HWIN)
  sum(diag(t)) / sum(t)
}

#funkcija za izraèun brier scora
brier.score <- function(observedMatrix, predictedMatrix){
  sum((observedMatrix - predictedMatrix) ^ 2) / nrow(predictedMatrix)
}
dt <- CoreModel(HWIN ~., learn, model="tree", minNodeWeightTree = 10)

#Sensitivity funkcija
Sensitivity <- function(observed, predicted, pos.class)
{
  t <- table(observed, predicted)
  
  t[pos.class, pos.class] / sum(t[pos.class,])
}

#Specificity funkcija
Specificity <- function(observed, predicted, pos.class)
{
  t <- table(observed, predicted)
  neg.class <- which(row.names(t) != pos.class)
  
  t[neg.class, neg.class] / sum(t[neg.class,])
}

#scale.data funkcija
scale.data <- function(data)
{
  norm.data <- data
  
  for (i in 1:ncol(data))
  {
    if (!is.factor(data[,i]))
      norm.data[,i] <- scale(data[,i])
  }
  
  norm.data
}

#funkcija, ki izbere veèinski klasifikator
voting <- function(predictions)
{
  res <- vector()
  
  for (i in 1 : nrow(predictions))  	
  {
    vec <- unlist(predictions[i,])
    res[i] <- names(which.max(table(vec)))
  }
  
  factor(res, levels=levels(predictions[,1]))
}

#izgradnja modelov

modelDT <- CoreModel(HWIN ~., learn, model="tree", selectionEstimator="ReliefFequalK", minNodeWeightTree = 10)
modelNB <- CoreModel(HWIN ~., learn, model="bayes", selectionEstimator="ReliefFequalK")
modelKNN <- CoreModel(HWIN ~., learn, model="knn", kInNN = 5, selectionEstimator="ReliefFequalK")
modelRF <- randomForest(HWIN ~., learn, selectionEstimator="ReliefFequalK")
modelBM <- boosting(HWIN ~., learn, selectionEstimator="ReliefFequalK")

#izraèun predikcij

predDT <- predict(modelDT, test, type="class")
predNB <- predict(modelNB, test, type="class")
predKNN <- predict(modelKNN, test, type="class")
predRF <- predict(modelRF, test, type="class")
predBM <- predict(modelBM, test)

#izraèun predikcijskih matrik
predMatDT <- predict(modelDT, test, type="probability")
predMatNB <- predict(modelNB, test, type="probability")
predMatKNN <- predict(modelKNN, test, type="probability")
predMatRF <- predict(modelRF, test, type="prob")
#predMatBM <- predict(modelBM, test, type="prob")

#izraèun klasifikacijske toènosti
caDT <- CA(predDT , test$HWIN)
caNB <- CA(predNB , test$HWIN)
caKNN <- CA(predKNN, test$HWIN)
caRF <- CA(predRF , test$HWIN)
caBM <- CA(predBM$class, test$HWIN)

#izraèun birer score
obsMat <- model.matrix(~HWIN-1, test)

brierDT <- brier.score(obsMat, predMatDT)
brierNB <- brier.score(obsMat, predMatNB)
brierKNN <- brier.score(obsMat, predMatKNN)
brierRF <- brier.score(obsMat, predMatRF)
brierBM <- brier.score(obsMat, predBM$prob)

#glasovanje med modeli
predVOTE <- data.frame(predDT, predNB, predKNN, predRF, predBM$class)
voted <- voting(predVOTE)
caVOTE <- CA(voted, test$HWIN)
