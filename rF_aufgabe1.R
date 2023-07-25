tab<-read.csv("./Zwischenergebnisse/Arithmetisches_Mittel_Tabelle.csv")
tab<-tab[,-1]

#Lösungen einlesen (für Doklänge [Anzahl changes] und truth Vektor)
idk<-in_json(1,3442,"../Datensaetze/dataset-narrow/")
idk<-remove_empty_docs(idk)

#Ergebnisse für Dokumente zusammenfassen
tab_dok<-tab[1,] #alles bis auf die erste Zeile löschen
tab_dok<-tab_dok[-1,] #erste Zeile löschen:D



start<-1
ende<-length(idk[[1]]$changes)
multi_author_vec<-c()
for(dok_nr in 1:3418){
  
  #truth aus json Datei bzw [idk] an Truth Vektor anhängen
  multi_author_vec<-append(multi_author_vec,idk[[dok_nr]]$`multi-author`)

  #tabelle für je ein Dokument aus Gesamt Tabelle   
  hilfstab<-tab[start:ende,]
  
  #Mittelwert für jedes Feature (jede Spalte) berechnen
  for(spaltennr in 1:length(hilfstab)){
  tab_dok[dok_nr,spaltennr]<-mean(hilfstab[,spaltennr])
  }
  
  
  start<-ende+1
  if(dok_nr!=3418){
  ende<-(start+length(idk[[dok_nr+1]]$changes))-1
  }
}

#truth Vektor in Tabelle (als Spalte)
tab_dok["truth"]<-multi_author_vec


#truth Spalte in String umwandeln
for(e in 1:3418){
  if(tab_dok$truth[e]==1){
    
    tab_dok$truth[e]="aenderung"
    
    
  }else{
    
    tab_dok$truth[e]="keine aenderung"
    
  }
  
}

#Dok mit NA (wegen awf) entfernen
tab_dok<-na.omit(tab_dok)

#truth Spalte wieder in factor umwandeln ¯\_(ツ)_/¯
tab_dok$truth<-as.factor(tab_dok$truth)
