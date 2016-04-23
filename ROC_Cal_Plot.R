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
