####PACKAGES####

library(tidyverse)
library(shiny)
library(haven)
library(ggplot2)
library(plotly)
library(shinythemes)
library(lubridate)
library(ggiraph)
####INITIALISATION####



Achats <- read.csv2(file = "Achats_csv.csv", fileEncoding = "UTF-8-BOM")
Correspondances <- read.csv2(file = "Correspondance_sites.csv", fileEncoding = "UTF-8-BOM")
Clients <- read_sas('clients.sas7bdat')

####TRAITEMENT####

Achats <-  filter(Achats, Num.Site != 7) #Supprimer le site 7 de la base de donnée

Correspondances <- rename(Correspondances, Num.Site = NUM_SITE) #Renommer les variables afin de pouvoir les joindre

Achats$Date.Achat <- dmy(Achats$Date.Achat)

Clients <- rename(Clients, Id.Client = ID_CLIENT)
Clients <- Clients %>%
  mutate(
    COD_SEXE = recode(COD_SEXE, "1" = "Homme", "2" = "Femme"), #Recode la variable sexe
    Age = floor(interval(DATE_NAIS, today()) / years(1)), #Calcul de l'age
    
    TrancheAge = case_when(
      Age < 30 ~ "Moins de 30",
      Age >= 30 & Age <= 45 ~ "30 à 45",
      Age > 45 ~ "Plus de 45",
      #TRUE ~ NA_character_
    )
  )

###JOINTURE###

Achats <- left_join(Achats, Correspondances, by = "Num.Site")
Achats <- left_join(Achats, Clients, by = "Id.Client")

sites_levels <- sort(unique(Achats$NOM_SITE))






