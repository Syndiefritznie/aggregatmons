#Sindy Charles

#Importation des libraries 

library(tidyverse)
library(readxl)
library(dplyr)

all_na <- function(x) any(!is.na(x))

#Acquisition du fichier 

url <- "https://www.brh.ht/wp-content/uploads/agregatsmon.xls"
destfile <- "output.xls"
download.file(url, destfile)

#Lecture du Fichier 
aggregaMonsRaw <- read_excel(destfile, range = "A3:CE521")

#Traitement et Netoyyage de Données

##Transformation en DataFrame
df_aggrega_raw <- data.frame(aggregaMonsRaw)

##Elimination des colonnes vides et des liges inutiles
df_aggrega_raw = df_aggrega_raw[-1,]
df_aggrega_raw_nona <- df_aggrega_raw %>% select_if(all_na)

##Assignation de colonnes
c0 <- c("ReportDate")
c1 <- c("M1_Gourdes","M2_Gourdes","M3_Gourdes","M1_Dollars","M2_Dollars","M3_Dollars","Taux_Change_BRH","Base_Monetaire_Gourdes","Base_Monetaire_Dollars","Monnaie_Circulation_Gourdes")
c2 <- c("Crédit_sect_pubm","Multiplicateur_M3","Multiplicateur_M2","Multiplicateur_M1","Réserves_Nettes_De_Change","Reserves_nettes_de_change_system_Banc","Inflation_glisse_annuel","Cred_sect_priv_Gdes","Cred_sect_priv_Dolalr","Credit_total_sect_priv","Cred_sect_priv_Dollar","Credit_net_Etat","Credit_net_Coll_loc")
c3 <- c("Cred_net_Ent_Pub","Cred_net_Sect_Pub","Depots_gdes","Depots_dollars_gourdes","Depots_tot_gourdes_dollar","Depots_dollars","Depots_Dollar_sur_Dep_gourdes","Creances_nettes_Etat","Creances_nettes_collectiv_locales","Creances_nettes_entreprises_pubmiq")
c4 <- c("Creances_nettes_secteur_pubmic","Reserves_nettes_de_changes_BRH_avec_depots_des_BCMs_dollar","Reserves_nettes_de_change_du_syst_banc_dollar","Reserves_brutes_de_change_BRH_avec_depot_dollar","Reserves_brutes_de_change_du_syst_banc","Depots_dollars_des_BCM_BRH_CAM_Transfert","Depots_dollars_des_BCM_BRH","Reserves_nettes_de_change_BRH_sans_depots_des_BCM_gourdes","Avoirs_exterieurs_nets_sans_depotBanques","Reserves_brutes_de_change_BRH_sans_depots_des_BCM_gourdes_dollars")
c5 <- c("Reserves_nettes_de_change_TMU","Depots_a_vue","Depots_epargne","Depots_terme","Total_Depots","DAV_DT","DE_DT","DAT_DT","Dep_Dollar_DT","DAV_G_DAT_G")
c6 <- c("DE_G_DAT_G_","DAV_dollar","DE_Dollars","DAT_dollar","Total_Depots_Dollar","Avoirs_Exter_bruts_Système_banc_gourdes","Engagem_Exter_du_Système_banc_gourdes","Avoirs_Exter_nets_Système_banc_gourdes","Avoirs_Exter_Bruts_Bques_Commerc_gourdes","Engagem_Exter_Bques_Commerc_gourdes")
c7 <- c("Avoirs_Exter_Nets_Bques_Commerc_gourdes","Depot_Dollar_Deptot","Credit_dollar_Depot_dollar","Credit_Dollar_Credit_total","Depot_M3")

colnames(df_aggrega_raw_nona) <- c(c0,c1,c2,c3,c4,c5,c6,c7)

## Conversion des colonnes en type de donnees appropriées
cols.num <- c(c0,c1,c2,c3,c4,c5,c6,c7)
df_aggrega_raw_nona[cols.num] <- sapply(df_aggrega_raw_nona[cols.num],as.numeric)
## lors de l'importation en excel, les dates apparaissent en numérique, je convertis en date avec l'origine de l'excel
df_aggrega_raw_nona$ReportDate <- as.Date(df_aggrega_raw_nona$ReportDate , origin = "1899-12-30")


## selection des données pour l'année 90
df_aggrega_90plus <- df_aggrega_raw_nona %>% filter(ReportDate >= "1990-01-01")

## selection des variables M3 et des réserves

df_aggrega_M3Reserves <- df_aggrega_90plus %>% 
        select(Taux_Change_BRH,M3_Dollars,Reserves_nettes_de_changes_BRH_avec_depots_des_BCMs_dollar, Reserves_nettes_de_change_system_Banc, Reserves_brutes_de_change_BRH_sans_depots_des_BCM_gourdes_dollars)

#Prediction taux de change 

#Creation du model linéaire
model <- lm(Taux_Change_BRH~ M3_Dollars + Reserves_nettes_de_changes_BRH_avec_depots_des_BCMs_dollar + Reserves_nettes_de_change_system_Banc + Reserves_brutes_de_change_BRH_sans_depots_des_BCM_gourdes_dollars,data = df_aggrega_M3Reserves)

#Resultats
summary(model)







