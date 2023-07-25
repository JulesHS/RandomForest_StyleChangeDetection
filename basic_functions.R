#install.packages('stringr')
#library(stringr)
#install.packages('tm')
#library(tm)

#Aufteilen von Textdokumenten in einen "Paragraphen-Vektor", Vektor in eine Gesamtliste
split_doc <- function(start, ende, pfad){ #anzahl = Anzahl an gewuenschten Dokumenten, pfad = "Pfad des Ordners"
  
  document_liste <- vector(mode = 'list')  #Initialiserung von leerer Liste fuer "Paragraphen-Vektoren"

  #Schleife in der jedes angegebene Dokumente in Paragraphen aufgeteilt wird
   
    for(i in start:ende){  
      tryCatch(
      {  
      #Text variable des Dokumentes "problem-i.txt"
      text <- readChar(paste(pfad,'problem-',i,'.txt', sep = ''),file.info(paste(pfad,'problem-',i,'.txt', sep = ''))$size)
      #Splitten an der Stelle "\n\n"(Absatz),Liste eintragen
      paragraphs <- str_split(text,pattern=fixed("\n\n")) 
      paragraphs <- unlist(paragraphs) #Liste in Vektor umwandeln
      #Eintragen des Paragraphenvektors in Dokumenten-Liste
      document_liste[[i]]<-paragraphs},
      error= function(e){print(paste("Datei",i,"nicht gefunden."))} #Fehlerausgabe
      )
    }

  return(document_liste) 

}


#liste<- split_doc(10,'C:\\Users\\Besitzer\\Desktop\\Softwareprojekt\\train\\dataset-narrow\\')

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Einlesen von json Dateien aus einem Ordner
install.packages("rjson")
install.packages("stringr")
library("rjson")
library("stringr")
#Einlesen von Json Dateien. anzahl=gewünschte Anzahl von Dateien, Pfad= Pfadangabe von Ordner
in_json <- function(start, ende, pfad){


truth <-vector(mode='list')

for (i in start:ende){
  #Dateien sind nicht durchgaengig nummeriert. z.B. fehlt 'truth-problem-77.json'
   tryCatch( 
    {truth[[i]]<-fromJSON(file=paste(pfad,'truth-problem-',i,'.json', sep = ''))}, #Sequentielles durchgehen des angegebenen Ordners
    error= function(e){print(paste("Datei",i,"nicht gefunden."))} #Fehlerausgabe
  )

  
  }
truth #Liste .json Dateien von 1-Anzahl 
return(truth)
}
truth<- in_json(500,'C:\\Users\\Besitzer\\Desktop\\Softwareprojekt\\train\\dataset-narrow\\')


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#für pos_frequenzen() Funktion 
#entfernt "NULL" Einträge für leere Dokumente aus Liste
#input: Liste mit allen Dokumenten

remove_empty_docs <- function(document_list){
  
  #install.packages("rlist")
  #library(rlist)
  
  clean_document_list <-vector("list")
  a<-1
  for (i in 1:length(document_list)){
    if(!(is.null(document_list[[i]]))){
      
      clean_document_list[[a]]<-document_list[[i]]
      a=a+1
    }
  } 
  
  return(clean_document_list)
}

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Erstellung eines leeren Style Change Vektor
#eventuell für Speicherung des Ergebnisses zum Schluss
#Input: Vektor mit Paragraphen des Dokuments 

#create_style_change_vector <- function(doc_vector){
#  style_change_vector <- logical(length=(length(doc_vector))-1)
#  return(style_change_vector)
#}
