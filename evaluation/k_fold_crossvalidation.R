library(ROSE)
library(caret)

ergebnisse<-read.csv("<Tabelle_Featurewerte_Truth_Klassifikationsergebnis>.csv")

#kzusätzliche Spalte durch Einlesen der csv Datei entfernen:
ergebnisse<-ergebnisse[,-1]

#------------------------------------------------------------------------------------------------------
#erst in Test und Trainigsdaten aufteilen, dann balancieren

flds <- createFolds(ergebnisse$truth_all_changes, k = 10, list = T, returnTrain = FALSE)
#flds[[1]] enthält Zeilennummern für ersten Fold


#i ist Rundennummer (k runden) 
for(i in 1:10){
  
  #Tabelle mit Testdaten
  ergebnisse_test<-ergebnisse[flds[[i]],]
  
  #Tabelle mit Trainingsdaten
  flds_train<-c(1:length(ergebnisse$truth_all_changes))
  remove<-flds[[i]]
  flds_train<- flds_train[!flds_train %in% remove]   
  ergebnisse_train<-ergebnisse[flds_train,]
  
  #Traingsdaten balancieren (Undersampling)
  ergebnisse_train_balanciert <- ovun.sample(truth_all_changes~., data=ergebnisse_train,
                                             seed=1, method="under")$data
  #length(ergebnisse_train_balanciert$truth_all_changes)
  #random Forest mit den Trainingsdaten der Runde trainieren
  rF_balanciert<-randomForest(truth_all_changes ~ ., ergebnisse_train_balanciert, ntree=200, norm.votes=FALSE,mtry=3,nodesize=4)
  
  #truth Spalte entfernen für Prediction
  ergebnisse_test_x<-ergebnisse_test[,-length(ergebnisse)]

  #Prediction Vektor
  p<-predict(rF_balanciert, ergebnisse_test_x)
  
  #Testdaten Prediction Spalte dranhängen und mit truth vergleichen
  ergebnisse_test_c<-cbind(ergebnisse_test,p)

  tn<-0
  tp<-0
  fn<-0
  fp<-0
  
  #Precision und Recall bestimmen
  for(j in 1:length(ergebnisse_test_c$p)){
    
    if((ergebnisse_test_c[j,10]=="keine aenderung")&&(ergebnisse_test_c[j,11]=="keine aenderung")){
      
      tn=tn+1
      
    }
    if((ergebnisse_test_c[j,10]=="aenderung")&&(ergebnisse_test_c[j,11]=="aenderung")){
      
      tp=tp+1
      
    }
    
    if((ergebnisse_test_c[j,10]=="keine aenderung")&&(ergebnisse_test_c[j,11]=="aenderung")){
      
      fp=fp+1
      
    }
    
    if((ergebnisse_test_c[j,10]=="aenderung")&&(ergebnisse_test_c[j,11]=="keine aenderung")){
      
      fn=fn+1
      
    } 
  }

  pr<-tp/(tp+fp) #Precision 
  re<-tp/(tp+fn) #Recall 
  
  f<-2*((pr*re)/(pr+re)) #F-1 Maß
  print(paste("Runde ",i,": Precision:",pr))
  print(paste("Runde ",i,": Recall:",re))
  print(paste("Runde ",i,": F_Maß: ",f))
  print("...")
}

#-----------------------------------------------------------------------
#erst gesamten Datensatz balancieren, dann in Trainings und Testdaten aufteilen

#gesamten Datensatz balancieren, undersampling
ergebnisse_balanciert <- ovun.sample(truth_all_changes~., data=ergebnisse,
                                     seed=1, method="under")$data

#folds Zeilen in der Tabelle bestimmen
flds <- createFolds(ergebnisse_balanciert$truth_all_changes, k = 10, list = T, returnTrain = FALSE)


for(i in 1:10){
  
  #Testdatensatz Tabelle
  ergebnisse_test<-ergebnisse_balanciert[flds[[i]],]
  
  #Trainingsdatensatz Tabelle
  flds_train<-c(1:length(ergebnisse_balanciert$truth_all_changes))
  remove<-flds[[i]]
  flds_train<- flds_train[!flds_train %in% remove]   
  ergebnisse_train<-ergebnisse_balanciert[flds_train,]
  
  #Random Forest trainieren
  rF_balanciert<-randomForest(truth_all_changes ~ ., ergebnisse_train, ntree=200, norm.votes=FALSE,mtry=3,nodesize=4)
  
  
  #truth spalte entfernen für prediction
  ergebnisse_test_x<-ergebnisse_test[,-length(ergebnisse_balanciert)]
  
  #prediction Vektor 
  p<-predict(rF_balanciert, ergebnisse_test_x)
  
  #Prediction Vektor an Testdaten Tabelle anhängen
  ergebnisse_test_c<-cbind(ergebnisse_test,p)
  
  #Prescision und Recall bestimmen
  tn<-0
  tp<-0
  fn<-0
  fp<-0
  
  for(j in 1:length(ergebnisse_test_c$p)){
    
    if((ergebnisse_test_c[j,10]=="keine aenderung")&&(ergebnisse_test_c[j,11]=="keine aenderung")){
      
      tn=tn+1
      
      
    }
    if((ergebnisse_test_c[j,10]=="aenderung")&&(ergebnisse_test_c[j,11]=="aenderung")){
      
      tp=tp+1
      
      
    }
    
    if((ergebnisse_test_c[j,10]=="keine aenderung")&&(ergebnisse_test_c[j,11]=="aenderung")){
      
      fp=fp+1
      
      
    }
    
    if((ergebnisse_test_c[j,10]=="aenderung")&&(ergebnisse_test_c[j,11]=="keine aenderung")){    
      fn=fn+1
    }
    
  }
  
  tn
  tp
  fn
  fp
  pr<-tp/(tp+fp) #Precision
  re<-tp/(tp+fn) #Recall
  
  #2*(tp/(tp+fp)*tp/(tp+fn))/(tp/(tp+fp)+tp/(tp+fn)) -> Prüfen
  f<-2*((pr*re)/(pr+re))
  print(paste("Runde ",i,": Precision:",pr))
  print(paste("Runde ",i,": Recall:",re))
  print(paste("Runde ",i,": F_Maß: ",f))
  print("...")
  
}
