fluidPage(
  
    theme = shinytheme("lumen"),

    # Titre
    titlePanel("Suivis des performances des différents sites"),
    
    #Barre latéral
    sidebarLayout(
      
        sidebarPanel(
          #themeSelector(),
          
          selectInput(
            inputId = "annee",
            label = "Choisir une année :",
            selected = "Tout",
            choices = c("Tout",unique(year(Achats$Date.Achat)))
          ),
          
          selectInput(
            inputId = "site",
            label = "Choisir un site :",
            selected = "Tout",
            choices = c("Tout",unique(Achats$NOM_SITE)),
            multiple = TRUE
          ),
          
           selectInput(
             inputId = "sexe",
             label = "Choisir un sexe :",
             selected = "Tout",
             choices = c("Tout", unique(as.character(Clients$COD_SEXE)))
           ),


           selectInput(
             inputId = "age",
             label = "Tranche d'âge :",
             selected = "Tout",
             choices = c("Tout", "Moins de 30", "30 à 45", "Plus de 45")
           )

        ), 
        
        mainPanel(
          
          tabsetPanel(
            
            tabPanel("Vue globale",
                     
                     br(),
                     fluidRow(
                       column(2, uiOutput("kpi_montant_total")),
                       column(2, uiOutput("kpi_nb_achats")),
                       column(2, uiOutput("kpi_panier_moyen")),
                       column(2, uiOutput("kpi_nb_clients")),
                       column(2, uiOutput("kpi_montant_moyen_client"))
                     ),
                     br(),
                     
                     tags$style(HTML("
                        .kpi-box {
                          background-color: #f7f7f7;
                          padding: 15px;
                          border-radius: 8px;
                          text-align: center;
                        }
                        .kpi-box h3 {
                          font-weight: bold;
                          margin-top: -5px;
                        }
                      ")),
                     
                     
                     girafeOutput("distPlot", width = "80%", height = "auto"),
                     girafeOutput("evoMensuelle", width = "80%", height = "auto")
            ),
            
            tabPanel("Clients",
            ),
            
            tabPanel("Cartographie",
            )
            
          ),

        )
    )
)
