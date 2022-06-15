library(tidyverse)
library(readxl)
library(dplyr)


urlCreditBancaire <- "https://raw.githubusercontent.com/raulincadet/INF1052022J/main/CreditBancaire.csv"
destfileC <- "CreditBancaire.csv"
download.file(urlCreditBancaire, destfileC)
creditbancaireRaw <- read.csv(file = destfileC, header = TRUE)


urldonne <- "https://raw.githubusercontent.com/raulincadet/INF1052022J/main/donnees.csv"
destfileP <- "donnees.csv"
download.file(urldonne, destfileP)
donneRaw <- read.csv(file = destfileP, header = TRUE)


urlworld_bank <- "https://raw.githubusercontent.com/raulincadet/INF1052022J/main/world_bank.csv"
destfileW <- "world_bank.csv"
download.file(urlworld_bank, destfileW)
worldbankRaw <- read.csv(file = destfileW, header = FALSE)



# Graphique en Baton nous utiliserons les données du crédit bancaire 

# nettoyage et formmattage des données

ggplot(creditbancaireRaw, aes(Type,Credit)) + geom_bar(stat="identity") 


# Graphique en nuage de points 

ggplot(donneRaw, aes(round(age),sal)) + geom_point()

# ggplot(, aes()) + geom_point() 


# Graphique en Ligne

Nettoyage des données
enteteWB <- worldbankRaw[5,]
entete2WB <- as.vector(enteteWB)
stagingDataWB <- worldbankRaw[-(1:5), ]
names(stagingDataWB) <- entete2WB
stagingDataWB[-1,]
## Conversion de wide a long. Methode nous permettant d'allonger le dataframe 
df_worldbank_long <-gather(stagingDataWB,year,value,'1960':'2015')
df_worldbank_long[,"value"][is.na(df_worldbank_long[,"value"])] <- 0
df_worldbank_long[, "year"] <- sapply(df_worldbank_long[, "year"], as.numeric)

df_plot <- df_worldbank_long %>%filter(`Country Code` == 'HTI')

ggplot(df_plot, aes(year,value)) + geom_line() 
