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
            choices = c("Tout",year(unique(Achats$Date.Achat)))
          ),

        ),

        mainPanel(
            plotOutput("distPlot")
        )
    )
)
