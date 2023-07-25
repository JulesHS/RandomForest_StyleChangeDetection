#1.Teile Satzzeichen Tabelle in true und false 2. Sortiere nach größter Distanz

satzzeichen_tab<-read.csv("../Zwischenergebnisse/Satzzeichen_Tabelle_mit_NAs.csv")
head(satzzeichen_tab)
#satzzeichen_tab<-satzzeichen_tab[,-1]
#satzzeichen_tab[1,-16]

m<-matrix(ncol=length(tabelle)-1)
as.data.frame(m)
m
tab_true<-m
tab_false<-m

names(tabelle)[-length(tabelle)]


#entferne truth und false aus tabelle (optional)  
colnames(tab_true)<-names(tabelle)[-length(tabelle)]
colnames(tab_false)<-names(tabelle)[-length(tabelle)]


#Teile Tabelle in Tabelle mit "keine Änderung" und "Änderung"
for(i in 1:length(tabelle$truth)){
  
  if(toString(tabelle[i,]$truth)=="aenderung"){
    tab_true<-rbind(tab_true,tabelle[i,-length(tabelle)])
    
    
  }else{
    
    tab_false<-rbind(tab_false,tabelle[i,-length(tabelle)])
  }
}





#Ziel: liniendiagramm, welches die Distanzen anzeigt für Änderung und keine Änderung
x<-c(1,2,3,4)
y<-c(1,2,3,4)
plot(x,y,type="l")

satzzeichen_tab_true[1:50,]$komma

head(satzzeichen_tab_true)
head(satzzeichen_tab)

satzzeichen_dist_true[1:50,]
satzzeichen_dist_false[1:50,]


#betrachte nur kommata:

length(satzzeichen_tab_true$komma)
length(satzzeichen_tab_false$komma)

kommas_true<-satzzeichen_tab_true$komma[!is.na(satzzeichen_tab_true$komma)]
length(kommas_true)

kommas_false<-satzzeichen_tab_false$komma[!is.na(satzzeichen_tab_false$komma)]
length(kommas_false)

plot(c(1:length(kommas_true)),kommas_true,type="l")
plot(c(1:length(kommas_false)),kommas_false,type="l")

#---------------------------------------------------------------------------------

#für balanciert_test1

balanciert<-read.csv("../Zwischenergebnisse/balanciert_test1.csv")
head(balanciert)
balanciert<-balanciert[,-1]
length(balanciert)
tabelle<-balanciert
head(tab_true)
length(tab_true$satzzeichen_dist)
length(tab_false)

#0en und 1en in strings "aenderung",...
truth_boolean<-c()
for(i in 1:length(tabelle$truth)){
  
  if(tabelle$truth[i]==1){
    
    truth_boolean[i]="aenderung"
    
  }else{  
    truth_boolean[i]="keine aenderung"
  }    
}  

head(tabelle)
tabelle<-tabelle[-5]
tabelle$truth
tabelle['truth']<-truth_boolean
length(truth_boolean)

length(tab_true$satzzeichen_dist)
length(tab_false$satzzeichen_dist)

head(tab_false)
head(tabelle)
tab_true<-tab_true[-1,]
tab_false<-tab_false[-1,]

head(tab_true)

x<-c(1:50)
Distanzen<-tab_false$satzzeichen_dist[1:50]

plot(x,Distanzen,type="l",main="Satzzeichen Distanzen für \"Keine Änderung\"")
#savePlot(filename="../plots/satzzeichen_dist_true.jpg",type="jpeg")
dev.copy(png,"../plots/pos_dist_false.png")
dev.off()
