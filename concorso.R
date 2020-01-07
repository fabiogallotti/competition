rm(list=ls())

library(car)
library(MASS)
library(readxl)

#leggo file
datitraining<- read_excel("training.xlsx")
#tolgo la prima colonna
training<-datitraining[,-1]

head(training)
colnames(training)

#riduco dataset alle sole variabili che mi interessano
training<-training[,-c(4,5,6,7,9,10,11,14,15,16)]
colnames(training)

#lavoro su variabili dataset, rendendole categoriche
training$Quartiere <- factor(training$Quartiere)
training$`Tipo cucina` <- factor(training$`Tipo cucina`)
training$Criticità <- factor(training$Criticità)
training$`Tipo ispezione` <- factor(training$`Tipo ispezione`)

#trasformo i dati mancanti nel punteggio come zeri
training$Punteggio[which(is.na(training$Punteggio))]<-0


#Assegno quelli riaperti o richiusi a violazioni generiche
training[which(training$Azione == 'No violations recorded after the inspection.'),]$Azione <- 'N'
training[which(training$Azione == 'Violations recorded after the inspection.'),]$Azione<- 'V'
training[which(training$Azione == 'Establishment re-opened by DOHMH'),]$Azione<- 'V' 
training[which(training$Azione == 'Establishment re-closed by DOHMH'),]$Azione<- 'V'
training[which(training$Azione == 'Establishment closed.'),]$Azione<- 'C'

#rendo categorica anche la variabile risposta
training$Azione <- factor(training$Azione)
levels(training$Azione)
colnames(training)


##CLASSIFICAZIONE
str(training)
#rendo numeriche le variabili categoriche (verifico al termine del file la correttezza
#di ciò che sto facendo)
training$Quartiere <- as.numeric(training$Quartiere)
training$`Tipo cucina` <- as.numeric(training$`Tipo cucina`)
training$Criticità <- as.numeric(training$Criticità)
training$`Tipo ispezione` <- as.numeric(training$`Tipo ispezione`)

str(training)

#creo nuove variabili per fare analisi
dati <- training[, c(8,3,4,5,6,7)]
head(dati)

y<-dati$Azione
x<-dati[,2:6]


##creo il classificatore, utilizzo come prior probabilities le frequenze relative 
#delle varie risposte
dati.qda <- qda(x,y)
dati.qda


##PREDIZIONE
#leggo test
file <- read_excel("test.xlsx")
#tolgo prima colonna
test <- file[,-1]
head(test)
colnames(test)

#riduco dataset a solo variabili che mi interessano
test<-test[,-c(4,5,6,7,9,10,11,14,15,16,18,19,20)]
colnames(test)

#uniformo le variabili a quelle del training set
test$Quartiere <- factor(test$Quartiere)
test$`Tipo cucina` <- factor(test$`Tipo cucina`)
test$Criticità <- factor(test$Criticità)
test$`Tipo ispezione` <- factor(test$`Tipo ispezione`)
test$Punteggio[which(is.na(test$Punteggio))]<-0

#
test$Quartiere <- as.numeric(test$Quartiere)
test$`Tipo cucina` <- as.numeric(test$`Tipo cucina`)
test$Criticità <- as.numeric(test$Criticità)
test$`Tipo ispezione` <- as.numeric(test$`Tipo ispezione`)

#isolo le variabili per predirre
dati_test <- test[,-c(1,2)]
head(dati_test)

#faccio predizione sui dati test
Dati.qda <- predict(dati.qda, dati_test)
Dati.qda$class

#metto a 1 le risposte 'N','V','C'
noviol <- ifelse(Dati.qda$class == 'N', 1, 0)
viol <- ifelse(Dati.qda$class == 'V', 1, 0)
close <- ifelse(Dati.qda$class == 'C', 1, 0)

#creo file con gli 1 al posto giusto
colnames(file)

predizione <- file[,-c(19,20,21)]
colnames(predizione)
predizione <- cbind(predizione,
                    'No violations recorded after the inspection.' = noviol,
                    'Violations recorded after the inspection.' = viol,
                    'Establishment closed.' = close)
colnames(predizione)

#scrivo file .csv
write.csv(predizione,file="test_set.csv",row.names = F)


########CONTROLLO LIVELLI 
training$Quartiere <- factor(training$Quartiere)
training$`Tipo cucina` <- factor(training$`Tipo cucina`)
training$Criticità <- factor(training$Criticità)
training$`Tipo ispezione` <- factor(training$`Tipo ispezione`)

test$Quartiere <- factor(test$Quartiere)
test$`Tipo cucina` <- factor(test$`Tipo cucina`)
test$Criticità <- factor(test$Criticità)
test$`Tipo ispezione` <- factor(test$`Tipo ispezione`)

levels(training$Quartiere)
levels(test$Quartiere)

levels(training$`Tipo cucina`)
levels(test$`Tipo cucina`)

levels(training$Criticità)
levels(test$Criticità)

levels(training$`Tipo ispezione`)
levels(test$`Tipo ispezione`)

#OK SONO GLI STESSI, NON SBAGLIO TRASFORMANDOLI IN NUMERI!