#### PACKAGES ####
library(tidyverse)
library(shiny)
library(haven) # Pour lire le SAS si besoin, sinon read_sas n'est pas utilisé dans le global fourni mais dans le traitement
library(ggplot2)
library(plotly)
library(lubridate)
library(ggiraph)
library(sf)

# Nouveaux packages pour le design
library(bslib)
library(bsicons)

#### INITIALISATION ####
# (Assure-toi que les chemins de fichiers sont corrects sur ta machine)
Achats <- read.csv2(file = "Achats_csv.csv", fileEncoding = "UTF-8-BOM")
Correspondances <- read.csv2(file = "Correspondance_sites.csv", fileEncoding = "UTF-8-BOM")
Clients <- read_sas('clients.sas7bdat')

#### TRAITEMENT ####

Achats <- filter(Achats, Num.Site != 7) 
Correspondances <- rename(Correspondances, Num.Site = NUM_SITE)
Achats$Date.Achat <- dmy(Achats$Date.Achat)

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

### JOINTURE ###
Achats <- left_join(Achats, Correspondances, by = "Num.Site")
Achats <- left_join(Achats, Clients, by = "Id.Client")

sites_levels <- sort(unique(Achats$NOM_SITE))

# --- DESIGN SYSTEM ---
# On définit une palette pour que les graphiques soient assortis au thème
my_palette <- c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F", "#EDC948", "#B07AA1", "#FF9DA7")