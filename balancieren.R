
ergebnisse_neu_balanciert<-ergebnisse_neu


ergebnisse_max<-read.csv("RandomForest_Tabelle_MaxDist.csv")

ergebnisse_neu_balanciert<-ergebnisse_max

for(i in 1:63400){

    
  x<-as.integer(runif(1,1,length(ergebnisse_neu_balanciert$truth)))
  
  while (ergebnisse_neu_balanciert[x,]$truth=="aenderung") {
    
    x<-as.integer(runif(1,1,length(ergebnisse_neu_balanciert$truth)))
    #print(x)
 
  }
    #print(paste("i",i))
    ergebnisse_neu_balanciert<-ergebnisse_neu_balanciert[-x,]
    
}
  


length(ergebnisse_neu_balanciert$truth)
head(ergebnisse_neu_balanciert)

library(randomForest)
rF_balanciert<-randomForest(truth ~ ., ergebnisse_neu_balanciert, ntree=200, norm.votes=FALSE,mtry=3,nodesize=4)
rF_balanciert
plot(rF_balanciert)
