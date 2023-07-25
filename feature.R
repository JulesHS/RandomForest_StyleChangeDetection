#install.packages('tokenizers')
#library(tokenizers)
#install.packages('stringr')
#library(stringr)
#install.packages('tm')
#library(tm)
#install.packages('stopwords')
#library(stopwords)
#install.packages('qdap')
#library(qdap)


#Durchschnittliche Satzlaenge für jeden Paragraphen in einer Dokumentensammlung
satzlaenge<- function(liste){ #Uebergabe Paragraphenliste
  
  #Initialiserung von Hilfsparametern
  vek_liste<- vector(mode = 'list')
  hilfi <- 1
   
  #Durchgehen von allen Paragraphen eines Dokumentes
   for (paragraphs in liste){
      mean_satzlaenge<-c()
      
      #Durchgehen einzelner Paragraphen 
      for(y in paragraphs){
         
         #Aufteilen der Paragraphen in Saetze 
         satz_vektor <- tokenize_sentences(y)
         anzahl_woerter<-c() 
         #Zaehlen der Woerter pro Satz
         for(satz in satz_vektor){
           anzahl_woerter <- append(anzahl_woerter, count_words(satz))
           
         }
         #Durchschnittliche Satzlaenge berechnen
        mean_satzlaenge<-append(mean_satzlaenge, mean(anzahl_woerter))
        
      }
    #Durchschnitt in die Liste anfuegen
    vek_liste[[hilfi]] <-mean_satzlaenge
    
    hilfi= hilfi+1
    
   }
  return(vek_liste)
  
}

#liste<- split_doc(1,100,'C:\\Users\\Besitzer\\Desktop\\Softwareprojekt\\train\\dataset-narrow\\')
#satz<-satzlaenge(liste)
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Erstellen einer Liste mit Termfrequenzen für jeden einzelnen Absatz aus einem Dokument
para_freq <- function(paragraphs){
  #Initialisieren einer Liste dessen Laenge die Anzahl der Absaetze ist
  freq <- vector(mode= 'list',length = length(paragraphs))
  #Termfrequenzen werden fuer jeden Paragraph in die Liste eingetragen
  for (i in 1:length(paragraphs)){
    freq[[i]] <- freq_terms(paragraphs[i])
    
    }
  
  return(freq)
}


----------------------------------------------------------------------------------------------------------------------------------------------------------------------
stoppwoerter <- function(freq){
  #Initialisieren des Hilfsliste 
  stopp <- vector(mode= 'list')
  y<-1
  
  #Durchgehen von allen DataFrames in der Liste Freq
  for(i in 1:length(freq)){
    #HilfsDataFrames und -vektoren
    
    df<-freq[[i]]
    df
    ges<-sum(df[[2]])#Gesamtzahl aller Terme eines Absatzs
    #Durchgehen jedes Wortes aus einem DataFrame
    
    
    stopword_vector<- stopwords("en")
    #prob_vector <- c(1:length(stopword_vector)) 
    vokabular <- data.frame(stopwords=stopword_vector,freq=numeric(length(stopword_vector)),prob=numeric(length(stopword_vector)))
    
    for(x in df$WORD){
      #Ist ein Stoppwort im DF vorhanden? -> Wort und Frequenz an Hilfsvektoren anhaengen
      
      
      if(x %in% stopwords("en")){
        vokabular[which(vokabular$stopwords==x),2] <- df[which(df$WORD==x),2] #Frequenz reinschreiben
        vokabular[which(vokabular$stopwords==x),3] <- df[which(df$WORD==x),2]/ges #Wkt reinschreiben
      }
      
      
      
      
    }
    #Anhaengen eines Dataframes mit Hilfsvektoren
    #vokabular
    stopp[[y]]<-vokabular  
    y= y+1 #Erhoehung der Hilfsvariable
    
    
  }
  #Rueckgabe der Liste mit allen Stoppwoerter-DataFrames 
  return(stopp)
}
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Erstellung einer Liste mit allen 2-Grams(plus Frequenze und Probability) für alle Paragraphen aus einem Dokument
n_grams_stop<- function(paragraphs){
  #Initialiesierung einer leeren Liste
  n_gram_list<- vector(mode = 'list')
  i<-1 #Hilfsvariable
  
  #Durchgehen aller Paragraphen in einer Liste
  for (paragraph in paragraphs){
     #Erstellung eines DataFrames mit allen 2-Grams eines Paragraphen, plus Frequenz und Prob.
     tab<-get.phrasetable(ngram(paragraph,n = 2))
    #Einfügen des DataFrames in die Liste
    n_gram_list[[i]]<-tab 
    #Erhoehung der Hilfvariable
    i<- i+1
  }
 
  
  return(n_gram_list)
  
}





#ngram<-n_grams_stop(liste[[1]])
--------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Zaehlt die Satzzeichen fuer jeden Paragraphen in einem Dokument und normalisiert die Frequenz
count_satzzeichen<- function(paragraphs){
  #Initialisieren einer leeren Liste
  satzzeichen<- vector(mode = 'list')
  #Hilfsvariable
  i<-1
  #Initialisieren eines Vektors mit gaengigen Satzzeichen
  zeichen <- c('.' , ',' , '?' , '!' , '...' , '"' , ':' , ';' , '-', '_', '(', '[', '{', ')', ']', '}', '/', '&', '\'','#')
   
  #Schleife fuer jeden Paragraphen des Dokumentes
  for(paragraph in paragraphs){
     #Initialiserung des Frequenzvektors
     count<-c()
     #Initialisierung des Wahrscheinlichkeitsvektors
     prob<-c()
     #Zaehlen aller Zeichen im Paragraphen
     ges<-count_characters(paragraph) #bzw. count_words(paragraph) !!
     
     #Schleife fuer jedes Zeichen im Zeichenvektor
     for(v in zeichen){
       #Zaehlen des Zeichens im Paragraphen
       count<-append(count,str_count(paragraph,pattern = fixed(v)))
     }
     #Fuer jede Frequenz im Frequenzvektor
     for(x in count){
      #Berechnung der jeweiligen Wahrscheinlichkeit in Abhaengigkeit der Anzahl der Gesamtzeichen
      prob<-append(prob, x/ges)
       
     }
     
     #Einfuegen eines DataFrames mit allen Infos aus diesem Paragraphen
     satzzeichen[[i]]<-data.frame(zeichen,count,prob)
     #Erhoehung der Hilfsvariable
     i <- i+1
     
   }
  #Rueckgabe der Liste aus DataFrames
  return(satzzeichen)
}

#count<-count_satzzeichen(liste[[1]])


--------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(rJava)
options(java.parameters = "- Xmx1024m")
library(ngram)
library(stringr)
library(tokenizers)
library(qdap)

#POStagging und 3-Gram Frequenzenerstellung
pos_frequenzen <- function(paragraphs){
  
  
  #Initialisieren leerer Liste
  pos_list <- vector(mode = 'list')
  i<-1 #Hilfsvariable
  
  #Fuer jeden Paragraph in Paragraphen
  for (paragraph in paragraphs){
    
    tag <- pos(paragraph)
      string<-toString(unlist(tag$POStagged$POStags))
      string <- str_remove_all(string,pattern=fixed(","))
              #gc()
    
    #3-Gram Erstellung fuer den POS-String
    tab<-get.phrasetable(ngram(string,n=3))
    
    #Einfuegen in die Liste
    pos_list[[i]]<-tab 
  
    i<- i+1
    gc()  #GarbageCollection fuer Speicherplatzfreigabe 
  }
  
return(pos_list)
  
}

#ngram_pos<-n_grams_stop(liste[[1]])


--------------------------------------------------------------------------------------------------------------------------------------------------------------------

library(qdap)
library(stringr)

#Abkuerzungen oder nicht abgekuerzten Pendants zaehlen und in DataFrame eintragen fuer alle Paragraphen eines Dokuments
count_abr<- function(paragraphs){
  #Rueckgabeliste initialisieren
  abr<- vector(mode = 'list')
  #Hilfsvariable
  i<-1
  
  #Für jeden Paragraphen im Dokument
  for(paragraph in paragraphs){
    
    #Leerer Vektor mit leeren Vektor Laenge 70
    vek<-rep(0,70)
    #Erstellen von Liste mit bekannten englischen Abkuerzungen und anfuegen von binären Vektoren
    contractions_truth<-data.frame(contractions,'truth_c'=vek,'truth_e'=vek)
    
   #Durchgehen aller Zeilen der Spalten
   for(x in 1:70){  
    #Wenn das Wort in der angegeben Zeile im Paragraphen vorkommt, setze den dazugehoerigen Vektor auf 1
     if (str_detect(paragraph,contractions_truth[[1]][x]))
       {contractions_truth$truth_c[x]<-1}
     
     if (str_detect(paragraph,contractions_truth[[2]][x]))
       {contractions_truth$truth_e[x]<-1}   
 
  }
   #Anfuegen des erstellten DataFrames an die Liste
   abr[[i]]<-contractions_truth
   i<-i+1
   
  }
  #Rueckgabe der Liste
  return(abr)
}


----------------------------------------------------------------------------------------------------------------------------------------------------------------------
for(i in 1:3442){
pfad<-paste('C:\\Users\\Besitzer\\Desktop\\Softwareprojekt\\train\\dataset-narrow\\problem-',i,'.txt',sep ='')

tryCatch({
    text <- readChar(pfad,file.info(pfad)$size)
    texts[i]<-text
  },
  error= function(e){print(paste("Datei",i,"nicht gefunden."))} #Fehlerausgabe
  )
 

  
}
texts[[3145]]
length(texts)
#AverageClass für jeden Absatz eines Dokuments
getAverageFrequencyClass<- function(text,freqs){
  #Text muss vorher eingelesen werden
  
  #Zählen der Termfrequenzen
  terms<- freq_terms(text, top = count_words(text))
  #Frequency-Probability-Vektor initialisieren 
  prob_class<-c()
  #Berechnen der Häufigkeitsklasse für jeden Term
  for(i in 1:length(terms$WORD)){
    prob_class[i]<-round(log2(terms[1,2]/terms[i,2]))
    
  }
  #Anfügen des Vektors
  terms['prob_class']<-prob_class
  #Initialisieren eines Rückgabevektors
  freq_prob<-vector(mode = 'list')
  #Durchgehen der Frequenzabsätze
  for(i in 1:length(freqs)){
    
    freq<-freqs[[i]]
    #Anhängen Frequency-Probability-Vektor
    freq['prob_class']<-c(0)
    #Für jedes Wort im Absatz wird die Klasse gefunden
    for(word in freq$WORD){
      klasse<-terms[which(word==terms$WORD),3]
      freq[which(word==freq$WORD),3]<-klasse
    }
    #Anhähngen des Absatzes in Liste
    freq_prob[[i]]<-freq
   
  }
  #Rückgabe für die Terms eines Dokumentes
  return(freq_prob)
}

#terms<-getAverageFrequencyClass('C:\\Users\\Besitzer\\Desktop\\Softwareprojekt\\train\\dataset-narrow\\problem-1.txt',freq)
#terms


#----------------------------------------------------------------------------------------------------------------------------------------
#Character n-grams
#ein Dok
character_ngrams<-function(paragraphs){
  #Initialiesierung einer leeren Liste
  character_ngrams_list<- vector(mode = 'list')
  i<-1 #Hilfsvariable
  
  #Durchgehen aller Paragraphen in einer Liste
  for (paragraph in paragraphs){
    #Erstellung eines DataFrames mit allen 2-Grams eines Paragraphen, plus Frequenz und Prob.
    txt<-splitter(paragraph,split.space=FALSE ,split.char=TRUE)
    txt<-preprocess(txt,remove.punct=T)
    tab<-get.phrasetable(ngram(txt,n = 2))
    #Einfügen des DataFrames in die Liste
    character_ngrams_list[[i]]<-tab 
    #Erhoehung der Hilfvariable
    i<-i+1
  }
  
  
  return(character_ngrams_list)
  
}

#test<-character_ngrams(docs[[1]])
#test

#------------------------------------------------------------------------------------------------------------------------------------------
#Durchschnittliche Wortlänge pro Absatz:

#ein Dok
wortlaenge<-function(dok){
  
    mean_wortlaenge<-c()
    
    #Durchgehen einzelner Paragraphen 
    for(y in dok){
      
      #Aufteilen der Paragraphen in Saetze 
      wort_vektor <- unlist(tokenize_words(y))
      anzahl_zeichen<-c() 
      #Zaehlen der Woerter pro Satz
      for(wort in wort_vektor){
        anzahl_zeichen <- append(anzahl_zeichen, count_characters(wort))
        
      }
      #Durchschnittliche Satzlaenge berechnen
      mean_wortlaenge<-append(mean_wortlaenge, mean(anzahl_zeichen))
      
    }
 
    
  return(mean_wortlaenge)
  
}

#wort_vec_test<-wortlaenge(docs[[1]])
#wort_vec_test

#---------------------------------------------------------------------------------------------------------------------------
#readability
#library(quanteda)

readability<-function(doc){
  
  readability_vec<-c()
  
  for(i in 1:length(doc)){
    
    
    readability_vec<-append(readability_vec,textstat_readability(doc[i],measure="Flesch")[[1,2]])
    
  }
  
  return(readability_vec)
  
}



