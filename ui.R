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
          )
        ), 
        
        mainPanel(
          
          tabsetPanel(
            
            tabPanel("Vue globale",
                     girafeOutput("distPlot"),
                     girafeOutput("evoMensuelle")
            ),
            
            tabPanel("Clients",
            ),
            
            tabPanel("Cartographie",
            )
            
          ),

        )
    )
)
