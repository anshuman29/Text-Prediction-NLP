#Modeling
modeling<-function(x){
  x$Prediction<-stri_extract_last_words(x$Token1)
  x$Token1<-as.character(x$Token1)
  x$Token1<-substring(x$Token1,1,nchar(x$Token1)-nchar(x$Prediction)-1)
  x<-data.frame(x$Token1,x$Prediction,x$Freq)
  names(x)<-c("Token1","Prediction","Freq")
  x<-as.data.table(x)
  x$Token1<-as.character(x$Token1)
  x$Prediction<-as.character(x$Prediction)  
  x
}

##Adding Probability Column
addProbabilityColumn<-function(table){
  
  tokens<-table$Token1
  freq<-table$Freq
  tableLength<-nrow(table)
  
  sumColumn<-vector(mode="numeric", length=tableLength)
  
  sum<-0
  i<-1
  start<-1
  end<-1
  
  oldToken<-tokens[1]
  
  while(i<=tableLength){
    newToken<-tokens[i]
    if(oldToken==newToken){
      sum<-sum + freq[i]
      i<-i+1  
    }
    else{
      end<-i-1
      sumColumn[start:end]<-sum
      start<-i
      sum<-freq[i]
      oldToken<-tokens[i]
      i<-i+1
    }
  }
  sumColumn[start:tableLength]<-sum
  
  probabilities<-freq/sumColumn
  table[,Probability:=probabilities]
  table
}


##Modelling
gc(reset=TRUE)
oneWordTable<-as.data.table(oneWord)
twoWordTable<-modeling(twoWord)
threeWordTable<-modeling(threeWord)
fourWordTable<-modeling(fourWord)
gc()

##Adding Probability Column
Sum<-sum(oneWordTable[,Freq])
oneWordTable<-oneWordTable[,Probability:=Freq/Sum]
twoWordTable<-addProbabilityColumn(twoWordTable)
threeWordTable<-addProbabilityColumn(threeWordTable)
fourWordTable<-addProbabilityColumn(fourWordTable)


##ReModeling 3Word and 4Word Tables
ReModeling<-function(x){
  
  x$Token1<-as.character(x$Token1)
  str<-x$Token1[1]
  words<-strsplit(str,' ')[[1]]
  words<-words[words!=""]
  numberOfWords<-length(words)
  
  if(numberOfWords==3){
    x$Token3<-stri_extract_last_words(x$Token1)
    x$Token1<-as.character(x$Token1)
    x$Token1<-substring(x$Token1,1,nchar(x$Token1)-nchar(x$Token3)-1)
    x<-data.frame(x$Token1,x$Token3,x$Prediction,x$Freq,x$Probability)
    names(x)<-c("Token1","Token3","Prediction","Freq","Probability")
    x$Token2<-stri_extract_last_words(x$Token1)
    x$Token1<-as.character(x$Token1)
    x$Token1<-substring(x$Token1,1,nchar(x$Token1)-nchar(x$Token2)-1)
    x<-data.frame(x$Token1,x$Token2,x$Token3,x$Prediction,x$Freq,x$Probability)
    names(x)<-c("Token1","Token2","Token3","Prediction","Freq","Probability")
    x<-as.data.table(x)
    x$Token1<-as.character(x$Token1)
    x$Token2<-as.character(x$Token2)
    x$Token3<-as.character(x$Token3)
    x$Prediction<-as.character(x$Prediction)
  }
  else if(numberOfWords==2){
    x$Token2<-stri_extract_last_words(x$Token1)
    x$Token1<-as.character(x$Token1)
    x$Token1<-substring(x$Token1,1,nchar(x$Token1)-nchar(x$Token2)-1)
    x<-data.frame(x$Token1,x$Token2,x$Prediction,x$Freq,x$Probability)
    names(x)<-c("Token1","Token2","Prediction","Freq","Probability")
    x<-as.data.table(x)
    x$Token1<-as.character(x$Token1)
    x$Token2<-as.character(x$Token2)
    x$Prediction<-as.character(x$Prediction)  
  }
  
  x
}

##Remodeling 3Word and 4Word Tables
threeWordTable<-ReModeling(threeWordTable)
fourWordTable<-ReModeling(fourWordTable)

##Setting Keys
setkey(oneWordTable,Token1)
setkey(twoWordTable,Token1)
setkey(threeWordTable,Token1,Token2)
setkey(fourWordTable,Token1,Token2,Token3)

oneWordTable <- oneWordTable[order(Token1,-Probability)]
twoWordTable <- twoWordTable[order(Token1,-Probability)]
threeWordTable <- threeWordTable[order(Token1,Token2,-Probability)]
fourWordTable <- fourWordTable[order(Token1,Token2,Token3,-Probability)]

##Removing Freq Column
oneWordTable[,Freq:=NULL]
twoWordTable[,Freq:=NULL]
threeWordTable[,Freq:=NULL]
fourWordTable[,Freq:=NULL]

#save.image(file="Backup.RData")
#load("./Backup.RData")

#saveRDS(oneWordTable, "oneWordTable.rds")
#saveRDS(twoWordTable, "twoWordTable.rds")
#saveRDS(threeWordTable, "threeWordTable.rds")
#saveRDS(fourWordTable, "fourWordTable.rds")

#oneWordTable<-readRDS("./data/oneWordTable.rds")
#twoWordTable<-readRDS("./data/twoWordTable.rds")
#threeWordTable<-readRDS("./data/threeWordTable.rds")
#fourWordTable<-readRDS("./data/fourWordTable.rds")
#source("Prediction.R")
