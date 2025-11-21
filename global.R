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
Clients <- rename(Clients, Id.Client = ID_CLIENT)

Achats$Date.Achat <- dmy(Achats$Date.Achat)

Achats <- left_join(Achats, Correspondances, by = "Num.Site")
Achats <- left_join(Achats, Clients, by = "Id.Client")
