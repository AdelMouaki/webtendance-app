#### PACKAGES ####
library(tidyverse)
library(shiny)
library(haven)
library(ggplot2)
library(plotly)
library(lubridate)
library(ggiraph)
library(sf)
library(bslib)
library(bsicons)

#### INITIALISATION ####
Achats <- read.csv2(file = "Achats_csv.csv", fileEncoding = "UTF-8-BOM")
Correspondances <- read.csv2(file = "Correspondance_sites.csv", fileEncoding = "UTF-8-BOM")
Clients <- read_sas('clients.sas7bdat')

#### NETTOYAGE ####

Achats <- filter(Achats, Num.Site != 7) 
Achats$Date.Achat <- dmy(Achats$Date.Achat)

Correspondances <- rename(Correspondances, Num.Site = NUM_SITE)

Clients <- rename(Clients, Id.Client = ID_CLIENT)
Clients <- Clients %>%
  mutate(
    COD_SEXE = recode(COD_SEXE, "1" = "Homme", "2" = "Femme"),
    Age = floor(interval(DATE_NAIS, today()) / years(1)),
    TrancheAge = case_when(
      Age < 30 ~ "Moins de 30",
      Age >= 30 & Age <= 45 ~ "30 à 45",
      Age > 45 ~ "Plus de 45"
    )
  )

#### JOINTURES ####
Achats <- left_join(Achats, Correspondances, by = "Num.Site")
Achats <- left_join(Achats, Clients, by = "Id.Client")

# Site pour liste 
sites_levels <- sort(unique(Achats$NOM_SITE))

#### CRÉATION DE LA TABLE POUR LE SERVER ####

achats_traites <- Achats %>%
  mutate(
    dep = case_when(
      # Gestion de la Corse (20 -> 2A ou 2B)
      substr(COD_POSTAL, 1, 2) == "20" & substr(COD_POSTAL, 1, 3) < "202" ~ "2A",
      substr(COD_POSTAL, 1, 2) == "20" & substr(COD_POSTAL, 1, 3) >= "202" ~ "2B",
      # Cas général (les 2 premiers chiffres)
      TRUE ~ substr(COD_POSTAL, 1, 2)
    )
  )
