#TRUTH
#Länge: 83012 für Dataset Narrow

truth_all<-in_json(1,3442,"./dataset-narrow/")
truth_all<-remove_empty_docs(truth_all)
length(truth_all[[1]]$changes)

truth_all_changes<-vector("list")

for(i in 1:3418){
  
  truth_all_changes[[i]]<-truth_all[[i]]$changes
  
}

truth_all_changes<-truth_all$changes


#-----------------------------------------------------------------------------------
#SATZZEICHEN

library(plyr)
#empty(satzzeichen_dist_all[[454]][[9]])
#class(satzzeichen_dist_all[[454]][[10]]$prob_dist)
#str_detect("NULL",satzzeichen_dist_all[[454]][[10]]$prob_dist)
#arithmetisches Mittel von Satzzeichendistanzen für jeden Absatzwechsel berechnen


sz_mean_list_all<-vector("list")
for( i in 1:length(satzzeichen_dist_all)){
  
  
  mean_vec<-c()
  
  #ein Paragraphenvergleich
  for(x in 1:length(satzzeichen_dist_all[[i]])){
    
    sum<-0
    
    #leere Dataframes werden durch "0" ersetzt
    if(!empty(satzzeichen_dist_all[[i]][[x]])){
    
      #ein Zeichen
      for(c in 1:length(satzzeichen_dist_all[[i]][[x]]$prob_dist)){
      sum<-sum+satzzeichen_dist_all[[i]][[x]]$prob_dist[c]
  
      }
      mean<-sum/length(satzzeichen_dist_all[[i]][[x]]$prob_dist)
      mean_vec<-append(mean_vec,mean)  
    }else{
     mean_vec<-append(mean_vec,0) 
      
    }  
  
  }
   sz_mean_list_all[[i]]<-mean_vec
  
}


#----------------------------------------------------------------------------------------------------------------------
#STOPPWOERTER

#Stoppwörter Distanzen aus Textdatei lesen:
sw_dist_all<-vector("list")

for( i in 1:3418){

  filename<-paste("./Feature_Distanz_Ergebnisse/Stoppwoerter_Distanzen/stoppwoerter_distanz",i,".txt",sep="")  
  sw_dist<-dget(filename)

  sw_dist_all[[i]]<-sw_dist
}

#arithmetisches Mittel ausrechnen:

sw_mean_list_all<-vector("list")
for( i in 1:length(sw_dist_all)){
  
  
  mean_vec<-c()
  
  #ein Paragraphenvergleich
  for(x in 1:length(sw_dist_all[[i]])){
    
    sum<-0
    
    #leere Dataframes werden durch "0" ersetzt
    if(!empty(sw_dist_all[[i]][[x]])){
      
      #ein Zeichen
      for(c in 1:length(sw_dist_all[[i]][[x]]$prob_dist)){
        sum<-sum+sw_dist_all[[i]][[x]]$prob_dist[c]
        
      }
      mean<-sum/length(sw_dist_all[[i]][[x]]$prob_dist)
      mean_vec<-append(mean_vec,mean)  
    }else{
      mean_vec<-append(mean_vec,0) 
      
    }  
    
  }
  sw_mean_list_all[[i]]<-mean_vec
  

  
#sw_mean_list_all_unlist<-unlist(sw_mean_list_all)
#tabelle['stoppwoerter_dist']<-sw_mean_list_all_unlist


#------------------------------------------------------------------------------------------------------
#SATZLAENGE

#schon fertig....
#tabelle['satzlaenge_dist']<-unlist(satzlaenge_dist_all)

#------------------------------------------------------------------------------------------------------
#POS 
  
#POS Distanzen aus Textdatei auslesen  
pos_dist_all<-vector("list")
for( i in 1:3418){
  filename<-paste("./POS_Distanzen/pos_distanz",i,".txt",sep="")  
  pos_dist<-dget(filename)
  pos_dist_all[[i]]<-pos_dist
}


#arithmetisches Mittel ausrechnen:  
pos_mean_list_all<-vector("list")
  
for( i in 1:length(pos_dist_all)){
  
  
  mean_vec<-c()
  
  #ein Paragraphenvergleich
  for(x in 1:length(pos_dist_all[[i]])){
    
    sum<-0
    
    #leere Dataframes werden durch "0" ersetzt
    if(!empty(pos_dist_all[[i]][[x]])){
      
      #ein Zeichen
      for(c in 1:length(pos_dist_all[[i]][[x]]$prob.dist)){
        sum<-sum+pos_dist_all[[i]][[x]]$prob.dist[c]
        
      }
      mean<-sum/length(pos_dist_all[[i]][[x]]$prob.dist)
      mean_vec<-append(mean_vec,mean)  
    }else{
      mean_vec<-append(mean_vec,0) 
      
    }  
    
  }
  pos_mean_list_all[[i]]<-mean_vec
  
}

#tabelle['pos_dist']<-unlist(pos_mean_list_all)

#-------------------------------------------------------------------------------------------------------------------- 
#ERGEBNISSE FÜR RANDOM FOREST IN TABELLE ZUSAMMENFASSEN

ergebnisse<-data.frame("satzzeichen_dist"=unlist(sz_mean_list_all),"stoppwoerter_dist"=unlist(sw_mean_list_all), "satzleange_dist"=unlist(satzlaenge_dist_all),"pos_dist"=unlist(pos_mean_list_all),"truth"=unlist(truth_all_changes))
#head(ergebnisse)
#write.csv(ergebnisse,"Ergebnisse.csv",row.names=F)

#ODER mit "aenderung" und "keine aenderung", statt 0 und 1:  

truth_boolean<-c()
for(i in 1:83012){
  
  if(unlist_truth_all_changes[i]==1){
    
    truth_boolean[i]="aenderung"
    
  }else{  
    truth_boolean[i]="keine aenderung"
  }    
}  

 ergebnisse_neu<-data.frame("satzzeichen_dist"=unlist(mean_list_all),"stoppwoerter_dist"=unlist(sw_mean_list_all), "satzleange_dist"=unlist(satzlaenge_dist_all),"pos_dist"=unlist(pos_mean_list_all),"truth"=truth_boolean

#-------------------------------------------------------------------------------

library(randomForest)
rf1 <- randomForest(truth ~ ., ergebnisse, ntree=50, norm.votes=FALSE)

#bzw 

rf1 <- randomForest(truth ~ ., ergebnisse_neu, ntree=50, norm.votes=FALSE)

#Ergebnis nicht so gut, weil unbalanciert... -> nutze balanciert.R auf erstellte Tabelle 

