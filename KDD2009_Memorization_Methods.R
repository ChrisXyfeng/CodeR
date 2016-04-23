#Function to build single-variable models for categorical variables
mkPredC <- function(outCol,varCol,appCol) {     # Note: 1 
    pPos <- sum(outCol==pos)/length(outCol)      # Note: 2 
    naTab <- table(as.factor(outCol[is.na(varCol)]))
    pPosWna <- (naTab/sum(naTab))[pos]   # Note: 3 
    vTab <- table(as.factor(outCol),varCol)
    pPosWv <- (vTab[pos,]+1.0e-3*pPos)/(colSums(vTab)+1.0e-3)    # Note: 4 
    pred <- pPosWv[appCol]       # Note: 5 
    pred[is.na(appCol)] <- pPosWna       # Note: 6 
    pred[is.na(pred)] <- pPos    # Note: 7 
    pred         # Note: 8 
}

#Applying single-variable variable models
for (v in catVars) {
    pi <- paste0('pred', v)
    dTrain[,pi] <- mkPredC(dTrain[,outcome],dTrain[,v],dTrain[,v])
    dCal[,pi] <- mkPredC(dTrain[,outcome],dTrain[,v],dCal[,v])
    dTest[,pi] <- mkPredC(dTrain[,outcome],dTrain[,v],dTest[,v])
}

#Scoring AUC
library(ROCR)
calcAUC <- function(predcol,outcol) {
    perf <- performance(prediction(predcol,outcol==pos),'auc')
    as.numeric(perf@y.values)
}

for (v in catVars) {
    pi <- paste0('pred', v)
    aucTrain <- calcAUC(dTrain[,pi],dTrain[,outcome])
    aucCal <- calcAUC(dCal[,pi],dCal[,outcome])
    cat(pi,'train:',aucTrain,'cal:',aucCal,'\n')
}

#For numeric variables
mkPredN <- function(outCol,varCol,appCol) {
    cuts <- unique(as.numeric(quantile(varCol,
                                       probs=seq(0, 1, 0.1),na.rm=T)))
    varC <- cut(varCol,cuts)
    appC <- cut(appCol,cuts)
    mkPredC(outCol,varC,appC)
}

for (v in numericVars) {
    pi <- paste0('pred', v)
    dTrain[,pi] <- mkPredN(dTrain[,outcome],dTrain[,v],dTrain[,v])
    dCal[,pi] <- mkPredN(dTrain[,outcome],dTrain[,v],dCal[,v])
    dTest[,pi] <- mkPredN(dTrain[,outcome],dTrain[,v],dTest[,v])
    aucTrain <- calcAUC(dTrain[,pi],dTrain[,outcome])
    aucCal <- calcAUC(dCal[,pi],dCal[,outcome])
    cat(pi,'train:',aucTrain,'cal:',aucCal,'\n')
}

#Plot
ggplot(data=dCal)+
    geom_density(aes(x=kpred,
                     color=as.factor(churn),
                     linetype=as.factor(churn)))
#Plot ROC
plotROC <- function(predcol,outcol) {
    perf <- performance(prediction(predcol,outcol==pos),'tpr','fpr')
    pf <- data.frame(
        FalsePositiveRate=perf@x.values[[1]],
        TruePositiveRate=perf@y.values[[1]])
    ggplot() +
        geom_line(data=pf,aes(x=FalsePositiveRate,y=TruePositiveRate)) +
        geom_line(aes(x=c(0,1),y=c(0,1)))
}
print(plotROC)             
