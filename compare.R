
  #Funktion bekommt alle Dataframes aus einem mit Stoppwortfrequenzen
  #Ergebnis von stoppwoerter Funktion
  
stopwords_prob_dist <- function(doc){
  words<-c()
  stopwords_dist<-vector("list")
  #Vektor mit allen Stoppwörtern    
  stopwords<-doc[[1]]$stopwords
  
  #für alle Paragraphen in EINEM Dokument
  for(c in 1:(length(doc)-1)){
    
    
    #Vektor mit Probs aus Paragraph 1
    para1_freq<-doc[[c]]$prob
    
    #Vektor mit Probs aus Paragraph 2
    para2_freq<-doc[[c+1]]$prob
    
    prob_dist_vector<-c()
    words<-c()
    
    #für alle wörter im Paragraph
    for(b in 1:length(para1_freq)){
      
      
      #wird nur durchgeführt, falls in einem der beiden Paragraphen das aktuelle Stoppwort enthalten ist 
      if((para1_freq[b]!=0)|(para2_freq[b]!=0)){
        
        
        prob_dist<-sqrt((para1_freq[b]-para2_freq[b])^2)
        words<-append(words,as.character(stopwords[b]))
        prob_dist_vector<-append(prob_dist_vector, prob_dist)
      }
      
    }
    
    para_df<-data.frame(stopword=words,prob_dist=prob_dist_vector) 
    stopwords_dist[[c]]<-para_df
    
  }
  #Liste mit Dataframes mit Prob Distanzen für je zwei Paragraphen
  return(stopwords_dist)
  
}


#stopwords_dist<-stopwords_prob_dist(doc)
#stopwords_dist[[47]]

#---------------------------------------------------------------------------------------------------------------

#für Satzlaenge / für Wortlaenge
satzlaenge_dist<-function(doc){
  
  
  prob_satzl_vec <- c()
  for( i in 1:(length(doc)-1)){
    
    
    satzl_1<-doc[i]
    satzl_2<-doc[i+1]
    
    
    prob_satz<-sqrt((satzl_1-satzl_2)^2)
    
    prob_satzl_vec[[i]]<-prob_satz
    
  }  
  
  prob_satzl_vec
  
  return(prob_satzl_vec)
  
}

#-----------------------------------------------------------------------------------------------------------------

satzzeichen_dist<-function(doc){
  
  zeichen <- doc[[1]]$zeichen
  satzzeichen_dist<-vector("list")
  satzzeichen_dist_vector<-c()
  #für alle Paragraphen in EINEM Dokument
  for(c in 1:(length(doc)-1)){
    
    
    #Vektor mit Probs aus Paragraph 1
    satzzeichen_1_freq<-doc[[c]]$prob
    
    #Vektor mit Probs aus Paragraph 2
    satzzeichen_2_freq<-doc[[c+1]]$prob
    
    satzzeichen_dist_vector<-c()
    z<-c()
    
    #für alle Satzzeichen im Paragraph
    for(b in 1:length(satzzeichen_1_freq)){
      
      
      #wird nur durchgeführt, falls in einem der beiden Paragraphen das aktuelle Stoppwort enthalten ist 
      if((satzzeichen_1_freq[b]!=0)|(satzzeichen_2_freq[b]!=0)){
        
        
        prob_dist<-sqrt((satzzeichen_1_freq[b]-satzzeichen_2_freq[b])^2)
        z<-append(z,as.character(zeichen[b]))
        satzzeichen_dist_vector<-append(satzzeichen_dist_vector, prob_dist)
      }
      
    }
    
    para_df<-data.frame(Zeichen=z,prob_dist=satzzeichen_dist_vector) 
    satzzeichen_dist[[c]]<-para_df
    
  }
  #Lissatzzeichen_dist_vectore mit Dataframes mit Prob Distanzen für je zwei Paragraphen
  return(satzzeichen_dist)
  
}

#----------------------------------------------------------------------------------------------------------------------------
#für POS_frequenzen / N-Grams

#Input: POS_freq Ergebnis für ein Dokument
#Output: POS Distanz für je zwei paragraphen in Liste
pos_dist<-function(dok){

pos_prob_dist_list<-vector("list")

for( x in 1:(length(dok)-1)){

#zwei Paragraphen POS Tabellen
pos_freq_abs1<-dok[[x]]
pos_freq_abs2<-dok[[x+1]]

#extrahiere Vektor mit POS_ngrams
pos_freq_tags_abs1<-pos_freq_abs1$ngrams
pos_freq_tags_abs2<-pos_freq_abs2$ngrams

#Vektoren kombinieren
pos_freq_tags<-c(pos_freq_tags_abs1,pos_freq_tags_abs2)

#dopplungen löschen
pos_freq_tags<-unique(pos_freq_tags)
pos_freq_tags


prob_abs1_vec<-c()
prob_abs2_vec<-c()


pos_prob_dist_vec<-c()
#für jedes Wort
for(i in 1:length(pos_freq_tags)){
  
  such_pos<-pos_freq_tags[i]
  
  #null, falls es nicht vorkommt 
  pos_prob_abs1<-0
  pos_prob_abs2<-0
  
  pos_prob_dist<-0

  #wenn erstes POS dings in Tabelle von Absatz 1
  if(such_pos %in% pos_freq_abs1[,1]){
    
    
   pos_prob_abs1=pos_freq_abs1[which(pos_freq_abs1$ngrams==such_pos),3]
    
    
    
  }#sonst 0
  
  if(such_pos %in% pos_freq_abs2[,1]){
    
    pos_prob_abs2=pos_freq_abs2[which(pos_freq_abs2$ngrams==such_pos),3] 
    
  }#sonst 0

  
  pos_prob_dist<-sqrt((pos_prob_abs1-pos_prob_abs2)^2)
  pos_prob_dist_vec[i]<-pos_prob_dist
  
}#end for (words)
  pos_prob_dist_df<-data.frame("pos tags"=pos_freq_tags,"prob dist"=pos_prob_dist_vec)
  
  #liste mit allen Vergleichstabellen
  pos_prob_dist_list[[x]]<-pos_prob_dist_df

}#end for (paragraphs)
return(pos_prob_dist_list)
}


#ergebnis<-pos_dist(dok)
#ergebnis

#---------------------------------------------------------------------------------------------------------------------------------

#NA Werte: in einer der beiden Paragraphen (die verglichen werden) kommt kein Wort aus der Abkürzungswörterliste vor

#Input: Liste mit Abkürzungsfrequenzen (Tabelle) für ein Dokument
#Output: Liste mit Vektoren => jedes Element steht für die Distanz der Wkt,
# dass ein Autor abkürzt wenn er ein Wort aus der Abkürzungswörterliste verwendet
abr_prob_dist<-function(abr_dok){

  
contraction_prob_dist_vec<-c()

#für jeden Paragraphen
for(i in 1:(length(abr_dok)-1)){
  
  #1.Zähle alle Wörter im Paragraph für die Abkürzungen möglich wären 
  #2.Zähle unter diesen alle Wörter, die abgekürzt wurden 
  
    
    contraction_possible_i<-0
    contraction_true_i<-0
    
    contraction_possible_next<-0
    contraction_true_next<-0
    
    contraction_prob_i<-0
    contraction_prob_next<-0
    
    contraction_prob_dist<-0
    
    #für jede Zeile in der Tabelle
    for(x in 1:length(abr_dok[[i]]$contraction)){
      
      
      #Paragraph i +++++++++++++++++++++++++++++++++++++++++++
      #zähle contraction_possible hoch, falls zeile 1 enthält
      if(abr_dok[[i]][x,3]==1|abr_dok[[i]][x,4]==1){
        
          contraction_possible_i=contraction_possible_i+1
      
          if(abr_dok[[i]][x,3]==1){
            contraction_true_i=contraction_true_i+1
          }
          
      }
      
      #Paragraph(next) ++++++++++++++++++++++++++++++++++++++++++++++++      
      
      if(abr_dok[[i+1]][x,3]==1|abr_dok[[i+1]][x,4]==1){
        
        contraction_possible_next=contraction_possible_next+1
        
        if(abr_dok[[i+1]][x,3]==1){
          contraction_true_next=contraction_true_next+1
          
        }
        
      }
      

    }###end for (Tabelle)
    
    #Wkt Abürzung falls möglich, für jeden Paragraphen    
    if(contraction_possible_i!=0){
    contraction_prob_i=contraction_true_i/contraction_possible_i
    }else{contraction_prob_i=NA}
    #wkt null wird nicht mit einbezogen
    
    
    if(contraction_possible_next!=0){
    contraction_prob_next=contraction_true_next/contraction_possible_next
    }else{contraction_prob_next=NA}
    
    
    #Distanz berechnen => nur falls beide einen Wert (Abkürzungsmöglichkeit) enthalten:
    if((is.na(contraction_prob_i))|(is.na(contraction_prob_next))){
      contraction_prob_dist=NA
    }else{
      contraction_prob_dist=sqrt(((contraction_prob_i-contraction_prob_next)^2))

    }
    
    
    #Distanz an Vektor anhängen:
    contraction_prob_dist_vec<-append(contraction_prob_dist_vec,contraction_prob_dist)

}###end for (Paragraph i)


  
 return(contraction_prob_dist_vec)
}  
#Problem: lasse Fall Abkürzung UND keine Abkürzung außer acht 
# -> zählt als nur Abkürzung

#ergebnis<-abr_prob_dist(abr_dok)
#ergebnis
----------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Vergleichsfunktion der Freq-Prob-Class für ein Dokument
compare_prob_class<- function(terms){
  #Summenvektor initialisieren 
  sum_vec<-c()
  #Durchgehen der Absätze
  for(j in 1:length(terms)){
    sum<-0
    #Berechnung der Summe
    for(i in 1:length(terms[[j]]$WORD)){
      sum<-sum+(terms[[j]][i,2]*terms[[j]][i,3])
    }
    #Normalisierung der Summe und anhängen an Vecot
    sum_vec[j]<-sum/sum(terms[[j]]$FREQ)
  }
  #Initialisieren von Distanzvektor
  dist_vec<-c()
  #Vergleich von jeweils zwei Absätzen 
   for(i in 1:length(sum_vec)-1){
    
    dist_vec[i]<-sqrt((sum_vec[i]-sum_vec[i+1])^2)
    
   }
  #Rückgabe des Vergleichs
  return(dist_vec)

}

#dist_vec<-compare_prob_class(terms)
#dist_vec
