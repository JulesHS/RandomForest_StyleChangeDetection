library(ggplot2)

satz<- dget('satzzeichen_all.txt')
satz[[1]][1]
abbr<-dget('abr_all.txt')
POStg_narrow<-abbr

library(ggplot2)

for(x in 1:10){
  probl<-c()
  graph<-c()  
for (j in 1:20){

for(i in 1:length(satz[[x]])){
 probl[i]<- satz[[x]][[i]]$prob[j]
 
 }
string<-toString(satz[[x]][[1]]$zeichen[j])
filename<-paste('C:\\Users\\Besitzer\\Desktop\\Softwareprojekt\\featureergebnisse\\Dokument',x,'Satzzeichen\\',j,'.png', sep='')
png(file=filename)

print(ggplot()+ 
geom_col(aes(1:length(satz[[x]]), probl),fill="blue",
             colour="darkblue", size=1)+xlab(string))
dev.off()

}
}
stop<-vector(mode = 'list')
for(i in 1:10){
  
  string<-paste('C:\\Users\\Besitzer\\Desktop\\Softwareprojekt\\featureergebnisse\\Feature_Ergebnisse_Dataset-narrow\\Stoppwoerter_Frequenz\\stoppwoerter_frequenz',i,'.txt', sep='') 
  test<-dget(string)
  stop[[i]]<-test
}

stop[[1]][[1]]




for (i in 1:10){
  
  stop_einzeln<- stop[[1]][[i]]
  sortedStop<- stop_einzeln[order(stop_einzeln$freq, decreasing = T),]
  sortierte_stop[[i]]<- sortedStop
  
  
  
}
for (i in 1:10){
  print(head(sortierte_stop[[i]], n=10))

}

vis_vector<- c('the','i','is', 'it','i\'ve','of','to','that','be','a' )


for(x in 1:10){
  probl<-c()
  graph<-c()  
  ordnerpath<-paste('C:\\Users\\Besitzer\\Desktop\\Softwareprojekt\\featureergebnisse\\StoppwoerterProbabilityColGraph\\Dokument',x ,sep='')
  dir.create(ordnerpath)
for (j in 1:10){
    
    zeichen<-vis_vector[j]
    
    for(i in 1:length(stop[[x]])){
      hilfi<-stop[[1]][[i]]
      probl[i]<- hilfi[which( hilfi$stopwords==zeichen),3]
      
    }
    string<-toString(zeichen)
    
    
    filename<-paste(ordnerpath,'\\',zeichen,'.png',sep='')
 
    png(file=filename)
    
    print(ggplot()+ 
            geom_col(aes(1:length(stop[[x]]), probl),fill="blue",
                     colour="darkblue", size=1)+ggtitle(string)+xlab(label='Absätze'))
    dev.off()
    
  }
}

POS<-vector(mode = 'list')
liste<- split_doc(1,10,'C:\\Users\\Besitzer\\Desktop\\Softwareprojekt\\train\\dataset-narrow\\')
liste[[1]]
laenge<-satzlaenge(liste)

for(i in 1:10){
  filename<-paste('C:\\Users\\Besitzer\\Desktop\\Softwareprojekt\\featureergebnisse\\SatzlaengePointGraph\\Dokument',i,'.png',sep='' )  
  string<-paste('Dokument',i) 

  png(file=filename, width = 1000, height = 1000)
  
 print(ggplot()+ 
       geom_point(aes(1:length(laenge[[i]]), laenge[[i]]),fill="red",
                colour="darkred", size=4)+ggtitle(string)+xlab(label='Absätze')+ylab(label='Satzlänge'))
  dev.off()
  
}
length(laenge[[1]])
vector()




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

author1<- vector('list')
author2<- vector('list')
hilf<-0
for(i in 1:length(satz[[1]])){
  
  absatz<-satz[[1]][i]
  
  if(hilf==0){
    author1<-append(author1,absatz)
   
    if(i<47)
    {
      if(truth[[1]]$changes[i]==1){
        hilf<-1
        
      }
    
    }
  }
  
  else{
    author2<-append(author2,absatz)
    if(i<47)
    {
      if(truth[[1]]$changes[i]==1){
        hilf<-0
        
      }
      
    }
  }

}

length(author1)
length(author2)

zeichensumme<-c()
summe<-c()
for (i in 1:length(author1[[1]]$zeichen)){
  
  z<-as.character(author1[[1]]$zeichen[[i]])
  bzeichensumme<-c()
  
  for(y in 1:length(author1)){
    
    zeichensumme[y]<-author1[[y]][which(author1[[y]]$zeichen==z),2]
    
  }
  summe[i]<-sum(zeichensumme)
  
  
}

df_auth1_zeichen<-data.frame(author1[[1]]$zeichen,summe)


for (i in 1:length(author2[[1]]$zeichen)){
  
  z<-as.character(author2[[1]]$zeichen[[i]])
  bzeichensumme<-c()
  
  for(y in 1:length(author2)){
    
    zeichensumme[y]<-author2[[y]][which(author2[[y]]$zeichen==z),2]
    
  }
  summe[i]<-sum(zeichensumme)
  
  
}

df_auth2_zeichen<-data.frame(author2[[1]]$zeichen,summe)

author1<- vector('list')
author2<- vector('list')
hilf<-0
absaetze[[1]]
for(i in 1:length(absaetze[[1]])){
  
  absatz<-absaetze[[1]][i]
  
  if(hilf==0){
    author1<-append(author1,absatz)
    
    if(i<47)
    {
      if(truth[[1]]$changes[i]==1){
        hilf<-1
        
      }
      
    }
  }
  
  else{
    author2<-append(author2,absatz)
    if(i<47)
    {
      if(truth[[1]]$changes[i]==1){
        hilf<-0
        
      }
      
    }
  }
  
}

length(author1)

length(author2)

string_auth1<-''

for(i in 1:length(author1)){

  string_auth1<-paste(string_auth1,toString(author1[[i]]))
  
  
  
}
string_auth1


string_auth2<-''

for(i in 1:length(author2)){
 
  string_auth2<-paste(string_auth2,toString(author2[[i]]))
  
  
  
}

prob_auth1<-c()


for(i in 1:length(df_auth1_zeichen$summe)){
  prob_auth1[i]<-df_auth1_zeichen$summe[i]/nchar(string_auth1)
  
  
  
}
df_auth1_zeichen['Prob']<-prob_auth1

prob_auth2<-c()
for(i in 1:length(df_auth2_zeichen$summe)){
  prob_auth2[i]<-df_auth2_zeichen$summe[i]/nchar(string_auth2)
  
  
  
}
library(plyr)
df_auth2_zeichen['Prob']<-prob_auth2
df_auth1_zeichen$author1..1...zeichen
colnames(df_auth2_zeichen)[1]<-'Zeichen'
library(ggplot2)
install.packages('ggpubr')
library(ggpubr)
p1<-ggplot()+ 
    geom_col(aes(df_auth1_zeichen$Zeichen, df_auth1_zeichen$Prob),fill="blue",
           colour="darkblue", size=1)+ggtitle('Zeichennutzung Dokument1 Author1')+ylab('Probability')+xlab('Zeichen')+
           ylim(0.0,0.008)

p2<-ggplot()+ 
  geom_col(aes(df_auth2_zeichen$Zeichen, df_auth2_zeichen$Prob),fill="red",
           colour="darkred", size=1)+ggtitle('Zeichennutzung Dokument1 Author2')+ylab('Probability')+xlab('Zeichen')+
           ylim(0.0,0.008)

auth_combine<-df_auth1_zeichen
auth_combine['Prob_Auth2']<-df_auth2_zeichen$Prob

  


freq1<-para_freq(author1)
stopp1<-stoppwoerter(freq1)
freq2<-para_freq(author2)
stopp2<-stoppwoerter(freq2)

sum_vec<-c()
for(i in 1:length(stopp1[[1]]$stopwords)){
  wort<-stopp1[[1]]$stopwords[i]
  sum<-0
  for(y in 1:length(stopp1)){
  sum<-sum+stopp1[[y]][which(stopp1[[y]]$stopwords==wort),2]
  }
  sum_vec[i]<-sum 
}
sum_vec

df_stopp1_ges<- data.frame(stopp1[[1]]$stopwords, sum_vec)

sum_vec<-c()
for(i in 1:length(stopp2[[1]]$stopwords)){
  wort<-stopp2[[1]]$stopwords[i]
  sum<-0
  for(y in 1:length(stopp1)){
    sum<-sum+stopp2[[y]][which(stopp2[[y]]$stopwords==wort),2]
  }
  sum_vec[i]<-sum 
}
sum_vec
df_stopp2_ges<- data.frame(stopp2[[1]]$stopwords, sum_vec)
df_stopp2_ges
df_stopp1_ges

library(dplyr)
df_stopp1_ges_not_zero <- filter(df_stopp1_ges, sum_vec !=0)
df_stopp2_ges_not_zero <- filter(df_stopp2_ges, sum_vec !=0)



summe1<-0
for(i in freq1){
  
  summe1<- summe1+sum(i$FREQ)
  
  
  
}
summe1

summe2<-0
for(i in freq2){
  
  summe2<- summe2+sum(i$FREQ)
  
  
  
}
summe2
prob1<-c()
for(i in 1:length(df_stopp1_ges_not_zero$sum_vec)){
  
  prob1[i]<-df_stopp1_ges_not_zero$sum_vec[i]/summe1

}
prob1

authr1_df<-data.frame(df_stopp1_ges_not_zero, prob1)

prob2<-c()
for(i in 1:length(df_stopp2_ges_not_zero$sum_vec)){
  
  prob2[i]<-df_stopp2_ges_not_zero$sum_vec[i]/summe2
  
}
prob2

authr2_df<-data.frame(df_stopp2_ges_not_zero, prob2)
names(authr1_df)[1]<-'stopwörter'

names(authr2_df)[1]<-'stopwörter'

max(authr1_df$prob1)
max(authr2_df$prob2)
sort_auth1_stoppwörter<- authr1_df[order(prob1,decreasing = T),]
sort_auth2_stoppwörter<- authr2_df[order(prob2,decreasing = T),]
library(ggplot2)
p1<-ggplot()+ 
  geom_col(aes(x= reorder(sort_auth1_stoppwörter$stopwörter, -sort_auth1_stoppwörter$prob1), y=sort_auth1_stoppwörter$prob1),
  fill="blue",colour="darkblue", size=1)+
  ggtitle('Stoppwörterbenutzung Dokument1 Author1')+
  ylab('Probability')+xlab('Stoppwörter')+
  ylim(0.0,0.06)+
  theme(axis.text.x = element_text(angle = 90))
p2<-ggplot()+ 
  geom_col(aes(x= reorder(sort_auth2_stoppwörter$stopwörter, -sort_auth2_stoppwörter$prob2), y=sort_auth2_stoppwörter$prob2),
           fill="red",colour="darkred", size=1)+
  ggtitle('Stoppwörterbenutzung Dokument1 Author2')+
  ylab('Probability')+xlab('Stoppwörter')+
  ylim(0.0,0.06)+
  theme(axis.text.x = element_text(angle = 90))



