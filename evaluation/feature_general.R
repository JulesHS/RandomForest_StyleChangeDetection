Ergebnis<-read.csv("./Zwischenergebnisse/Arithmetisches_Mittel_Tabelle.csv")
Ergebnis<-Ergebnis[,-1]

#Input: Feature Vektor und truth Vektor
#Output: Mittelwert, Standardabweichung, Varianz (Vektor)
mean_var <-function(feature, truth){

feature_true<-c()
feature_false<-c()
for(i in 1:length(truth)){
  
  if(truth[i]=="aenderung"){
    feature_true<-append(feature_true,feature[i])
    
  }else{
    feature_false<-append(feature_false,feature[i])
  }
  
}
  
analyse_werte<-c(1:7)  
analyse_werte[1]<-mean(feature_true)
analyse_werte[2]<-sd(feature_true)
analyse_werte[3]<-var(feature_true)
analyse_werte[4]<-("-------------")
analyse_werte[5]<-mean(feature_false)
analyse_werte[6]<-sd(feature_false)
analyse_werte[7]<-var(feature_false)  
return(analyse_werte)
}


#leere Tabelle mit allen Featurenamen
analyse_tab<-Ergebnis[1:7,-length(Ergebnis)]
rownames(analyse_tab)<-c("Aenderung Mittelwert","Aenderung Standardabweichung","Aenderung Varianz","------------------------------","Keine Aenderung Mittelwert","Keine Aenderung Standardabweichung","Keine Aenderung Varianz")
analyse_tab

#Analyseergebnisse jedes Features in Tabelle 
for(i in 1:(length(Ergebnis)-1)){
  
  analyse_tab[,i]<-mean_var(Ergebnis[,i],Ergebnis$truth_all_changes)
  
}

#analyse_tab



#-------------------------------------------------------------------------------------------------------------------------------------------
#Random Forest alle möglichen Kombinationen

library(gtools) 
library(randomForest)


alle_feature_balanciert<-rF_balanciert
head(alle_feature_balanciert)

#für 8 Feature
for(i in 1:9){
#test<-c(1,2,3,4,5,6,7,8)
com<-combinations(n=9,r=i,repeats.allowed=F)

for(x in 1:length(com[,1])){
  
  #alle_feature_balanciert => Tabelle mit allen Featuren
  #tab => tabelle mit aktueller Featurekombination
  tab<-alle_feature_balanciert[com[x,]]
  tab<-cbind(tab,"truth"=alle_feature_balanciert$truth)
 
  rf<-randomForest(truth~.,tab,ntree=500)
  
  err_rate<-(rf$confusion[1,3]+rf$confusion[2,3])/2
  
  #falls error rate ungefähr besser als bisher (<=0.4)
  if(err_rate<=0.4){
  #speichere Konfusionsmatrix des rF in csv
  #Name der Datei -> verwendete Feature-Kombi für rF
  write.csv(rf$confusion,paste("./rF/",toString(names(tab)),".csv",sep=''))
  }
}
}


#library(stringr)

#alle Dateien im Ordner
files<-list.files("rF/")

combination_vec<-c()
mean_error_vec<-c()

#Dateien mit Konfusionsmatrix für jede der besten Kombinationen wieder einlesen :D
for(i in files){
  confusion<-read.csv(paste("./rF/",i,sep=""))
  mean_error<-(confusion[1,4]+confusion[2,4])/2
  #mean_error aus Konfusionsmatrix errechnen
  mean_error_vec<-append(mean_error_vec,mean_error)
}

#Tabelle Kombination (Dateiname) und mean_error Rate
beste_kombinationen<-data.frame("Kombination"=files,"Error_Rate"=mean_error_vec)

#nach mean_error Rate sortieren
beste_kombinationen_sort<-beste_kombinationen[order(beste_kombinationen$Error_Rate),]

beste_kombinationen_sort[1:10,]

write.csv(beste_kombinationen_sort,"beste_kombinationen_sort_stand_18_Dez.csv")
