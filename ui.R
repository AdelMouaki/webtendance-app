fluidPage(
  
  # =========================
  # THEME 
  # ========================= 
  theme = shinytheme("lumen"),
  
  # =========================
  # TITRE
  # =========================
  titlePanel("Suivi des performances e-commerce"),
  
  # =========================
  # MIS EN PAGE
  # =========================
  sidebarLayout(
    
    sidebarPanel(
      width = 3,
      
      h4("Filtres"),
      hr(),
      
      selectInput(
        "annee",
        "Année",
        choices = c("Tout", unique(year(Achats$Date.Achat))),
        selected = "Tout"
      ),
      
      selectInput(
        "site",
        "Site",
        choices = c("Tout", unique(Achats$NOM_SITE)),
        multiple = TRUE,
        selected = "Tout"
      ),
      
      selectInput(
        "sexe",
        "Sexe",
        choices = c("Tout", unique(Clients$COD_SEXE)),
        selected = "Tout"
      ),
      
      selectInput(
        "age",
        "Tranche d’âge",
        choices = c("Tout", "Moins de 30", "30 à 45", "Plus de 45"),
        selected = "Tout"
      )
    ),
    
    mainPanel(
      width = 9,
      
      tabsetPanel(
        
        # =========================
        # ONGLET VUE GLOBALE
        # =========================
        tabPanel(
          "Vue globale",
          
          # ===== KPI =====
          fluidRow(
            hr(),
            column(2, uiOutput("kpi_montant_total")),
            column(2, uiOutput("kpi_nb_achats")),
            column(2, uiOutput("kpi_panier_moyen")),
            column(3, uiOutput("kpi_nb_clients")),
            column(3, uiOutput("kpi_montant_moyen_client"))
          ),
          
          hr(),
          
          girafeOutput("distPlot", width = "auto", height = "450px"),
          
          hr(),
          
          girafeOutput("evoMensuelle", width = "auto", height = "450px")
        ),
        
        # =========================
        # ONGLET CARTOGRAPHIE
        # =========================
        tabPanel(
          "Cartographie",
          
          girafeOutput("map", width = "100%", height = "600px")
        ),
        
        # =========================
        # ONGLET CLIENTS
        # =========================
        tabPanel(
          "Clients",
          
          girafeOutput("clients_density", width = "100%", height = "600px")
        ),
        
        # =========================
        # ONGLET DÉTAIL CLIENT
        # =========================
        tabPanel(
          "Détail client",
          
          fluidRow(
            
            # === FICHE CLIENT (HAUT / DROITE) ===
            column(
              4,
              uiOutput("fiche_client")
            ),
            
            # === TABLE CLIENTS ===
            column(
              8,
              DT::dataTableOutput("table_clients")
            )
          )
        )
      )
    )
  ),
  
  # =========================
  # CSS GLOBAL
  # =========================
  tags$style(HTML("
    .kpi-box {
      background-color: #f8f9fa;
      padding: 15px;
      border-radius: 8px;
      text-align: center;
      box-shadow: 0 2px 6px rgba(0,0,0,0.05);
      margin-bottom: 10px;
    }
    .kpi-box h3 {
      font-weight: bold;
      margin: 5px 0 0 0;
    }
    .client-card {
      background: white;
      padding: 20px;
      border-radius: 8px;
      border-left: 5px solid #2c7be5;
      box-shadow: 0 2px 6px rgba(0,0,0,0.08);
    }
  "))
)
