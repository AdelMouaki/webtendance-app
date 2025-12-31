ui <- page_sidebar(
  
  fillable = FALSE, #scroll desactivé
  
  title = "Performance web-tendance",
  theme = bs_theme(
    version = 5,
    bootswatch = "zephyr", 
    primary = "#2c3e50",
    secondary = "#95a5a6"
  ),
  
  # =========================
  # FILTRES
  # =========================
  
  sidebar = sidebar(
    title = "Filtres",
    bg = "#f8f9fa", # Fond gris
    class = "p-3",  # Padding
    
    selectInput(
      "annee", "Année",
      choices = c("Tout", unique(year(Achats$Date.Achat))),
      selected = "Tout"
    ),
    
    selectInput(
      "site", "Site",
      choices = c("Tout", unique(Achats$NOM_SITE)),
      multiple = TRUE, selected = "Tout"
    ),
    
    selectInput(
      "sexe", "Sexe",
      choices = c("Tout", unique(Clients$COD_SEXE)),
      selected = "Tout"
    ),
    
    selectInput(
      "age", "Tranche d’âge",
      choices = c("Tout", "Moins de 30", "30 à 45", "Plus de 45"),
      selected = "Tout"
    ),
    
    # footer
    tags$hr(),
    tags$small(em("Adel Mouaki-Dadi", tags$br(),"Brun Bahoun Houtoukpe"), style = "color: #999;")
  ),
  
  # onglets
  navset_card_underline(
    title = "Tableau de bord",
    
  # ==============================================================================
  # GRAPHIQUES GLOBAL
  # ==============================================================================
  
    nav_panel(
      title = "Vue globale",
      icon = bs_icon("bar-chart-line"), # Icône
      
      # Ligne des KPI (Value Boxes)
      layout_columns(
        fill = FALSE,
        value_box(
          title = "Montant total",
          value = uiOutput("kpi_montant_total"),
          showcase = bs_icon("currency-euro"),
          theme = "primary"
        ),
        value_box(
          title = "Nombre d'achats",
          value = uiOutput("kpi_nb_achats"),
          showcase = bs_icon("cart-check"),
          theme = "teal"
        ),
        value_box(
          title = "Panier moyen",
          value = uiOutput("kpi_panier_moyen"),
          showcase = bs_icon("bag"),
          theme = "info"
        ),
        value_box(
          title = "Clients uniques",
          value = uiOutput("kpi_nb_clients"),
          showcase = bs_icon("people"),
          theme = "purple"
        )
      ),
      
      br(), 
      
      card(
        height = "1100px",
        full_screen = TRUE,
        card_header("Répartition du CA par site", class = "bg-light d-flex align-items-center"),
        card_body(
          class = "p-0",
          girafeOutput("distPlot", width = "100%", height = "100%")
        )
      ),
      
      br(), # Espace
      
      # --- Graph 2 ---
      card(
        height = "1200px",
        full_screen = TRUE,
        card_header("Évolution mensuelle des ventes", class = "bg-light d-flex align-items-center"),
        card_body(
          class = "p-0",
          girafeOutput("evoMensuelle", width = "100%", height = "100%")
        )
      )
    ),
    
  # ==============================================================================
  # CARTE
  # ==============================================================================
    
    nav_panel(
      title = "Cartographie",
      icon = bs_icon("geo-alt"),
      
      # KPI SPÉCIFIQUES À LA CARTE
      layout_columns(
        fill = FALSE,
        value_box(
          title = textOutput("titre_kpi_geo"), # Titre dynamique
          value = uiOutput("kpi_geo_ca"),
          showcase = bs_icon("currency-euro"),
          theme = "primary",
          height = "150px"
        ),
        value_box(
          title = "Nombre d'achats",
          value = uiOutput("kpi_geo_vol"),
          showcase = bs_icon("cart-check"),
          theme = "teal",
          height = "150px"
        ),
        value_box(
          title = "Panier moyen",
          value = uiOutput("kpi_geo_panier"),
          showcase = bs_icon("bag"),
          theme = "info",
          height = "150px"
        ),
        value_box(
          title = "Clients uniques",
          value = uiOutput("kpi_geo_clients"),
          showcase = bs_icon("people"),
          theme = "purple",
          height = "150px"
        )
      ),
      
      card(
        card_header("Maillage territorial des ventes"),
        height = "1300px", 
        full_screen = TRUE,
        girafeOutput("map", width = "100%", height = "100%")
      )
    ),
    
  # ==============================================================================
  # ANALYSE CLIENT
  # ==============================================================================
  
    nav_panel(
      title = "Analyse clients",
      icon = bs_icon("person-vcard"),
      
      layout_columns(
        col_widths = c(12),
        card(
          card_header("Profilage et comportement d'achat"),
          girafeOutput("clients_density", height = "900px")
        )
      )
    ),
    
  # ==============================================================================
  # DÉTAIL CLIENT
  # ==============================================================================
    
    nav_panel(
      title = "Détail client",
      icon = bs_icon("search"),
      
      layout_columns(
        col_widths = c(4, 8),
        
        # Fiche client à gauche
        uiOutput("fiche_client_ui"),
        
        # Table à droite
        card(
          card_header("Base de données clients"),
          DT::dataTableOutput("table_clients")
        )
      )
    )
  )
)