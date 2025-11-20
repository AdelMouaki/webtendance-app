fluidPage(
  
    # Titre
    titlePanel("Suivis des performances des différents sites"),
    
    #Barre latéral
    sidebarLayout(
      
        sidebarPanel(
          themeSelector(),
          
          selectInput(
            inputId = "annee",
            label = "Choisir une année :",
            choices = c("Tout",unique(year(Achats$Date.Achat)))
          ),
          
          selectInput(
            inputId = "site",
            label = "Choisir un site :",
            choices = c("Tout",unique(Achats$NOM_SITE)),
            multiple = TRUE
          )
        ), 
        
        mainPanel(
          
          tabsetPanel(
            
            tabPanel("Vue globale",plotOutput("distPlot")
            ),
            
            tabPanel("Clients",
            ),
            
            tabPanel("Cartographie",
            )
            
          ),

        )
    )
)
