ui <- page_sidebar(
  
  # =========================
  # ACTIVATION DU SCROLL
  # =========================
  fillable = FALSE, # <--- C'EST LA CLÉ ! (Par défaut c'est TRUE)
  
  title = "Performance E-Commerce",
  theme = bs_theme(
    version = 5,
    bootswatch = "zephyr", 
    primary = "#2c3e50",
    secondary = "#95a5a6"
  ),
  
  # =========================
  # BARRE LATÉRALE (FILTRES)
  # =========================
  sidebar = sidebar(
    title = "Filtres",
    bg = "#f8f9fa", # Fond gris très léger pour détacher la sidebar
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
    
    # Petit footer dans la sidebar
    tags$hr(),
    tags$small(em("Dashboard étudiant - 2024"), style = "color: #999;")
  ),
  
  # =========================
  # CONTENU PRINCIPAL
  # =========================
  
  # On utilise navset_card_underline pour des onglets modernes et soulignés
  navset_card_underline(
    title = "Tableau de Bord",
    
    # --- ONGLET 1 : VUE GLOBALE ---
    nav_panel(
      title = "Vue Globale",
      icon = bs_icon("bar-chart-line"), # Icône
      
      # Ligne des KPI (Value Boxes)
      layout_columns(
        fill = FALSE,
        value_box(
          title = "Montant Total",
          value = uiOutput("kpi_montant_total"),
          showcase = bs_icon("currency-euro"),
          theme = "primary" # Couleur principale
        ),
        value_box(
          title = "Nombre d'achats",
          value = uiOutput("kpi_nb_achats"),
          showcase = bs_icon("cart-check"),
          theme = "teal"
        ),
        value_box(
          title = "Panier Moyen",
          value = uiOutput("kpi_panier_moyen"),
          showcase = bs_icon("bag"),
          theme = "info"
        ),
        value_box(
          title = "Clients Uniques",
          value = uiOutput("kpi_nb_clients"),
          showcase = bs_icon("people"),
          theme = "purple"
        )
      ),
      
      br(), 
      
      # ... Après les Value Boxes et le br() ...
      
      card(
        height = "750px", # Hauteur FIXE et COMPACTE
        full_screen = TRUE,
        card_header("Répartition du CA par Site", class = "bg-light d-flex align-items-center"),
        card_body(
          class = "p-0", # AUCUNE MARGE
          girafeOutput("distPlot", width = "100%", height = "100%")
        )
      ),
      
      br(), # Espace
      
      # --- GRAPHIQUE 2 (Courbes) ---
      card(
        height = "750px", # Hauteur FIXE et COMPACTE
        full_screen = TRUE,
        card_header("Évolution Mensuelle des Ventes", class = "bg-light d-flex align-items-center"),
        card_body(
          class = "p-0", # AUCUNE MARGE
          girafeOutput("evoMensuelle", width = "100%", height = "100%")
        )
      )
    ),
    
    # --- ONGLET 2 : CARTOGRAPHIE (Avec KPI Locaux) ---
    nav_panel(
      title = "Cartographie",
      icon = bs_icon("geo-alt"),
      
      # KPI SPÉCIFIQUES À LA CARTE
      layout_columns(
        fill = FALSE,
        value_box(
          title = textOutput("titre_kpi_geo"), # Titre dynamique (France ou Sélection)
          value = uiOutput("kpi_geo_ca"),
          showcase = bs_icon("geo-fill"),
          theme = "primary",
          height = "150px"
        ),
        value_box(
          title = "Volume Commandes",
          value = uiOutput("kpi_geo_vol"),
          showcase = bs_icon("box-seam"),
          theme = "teal",
          height = "150px"
        ),
        value_box(
          title = "Panier Moyen Zone",
          value = uiOutput("kpi_geo_panier"),
          showcase = bs_icon("basket"),
          theme = "purple",
          height = "150px"
        )
      ),
      
      card(
        card_header("Carte Interactive (Cliquez pour filtrer les KPI ci-dessus)"),
        height = "1200px", 
        full_screen = TRUE,
        girafeOutput("map", width = "100%", height = "100%")
      )
    ),
    
    # --- ONGLET 3 : ANALYSE CLIENTS ---
    nav_panel(
      title = "Analyse Clients",
      icon = bs_icon("person-vcard"),
      
      layout_columns(
        col_widths = c(12),
        card(
          card_header("Segmentation : Fréquence vs Montant"),
          girafeOutput("clients_density", height = "900px")
        )
      )
    ),
    
    # --- ONGLET 4 : DÉTAIL ---
    nav_panel(
      title = "Détail Client",
      icon = bs_icon("search"),
      
      layout_columns(
        col_widths = c(4, 8),
        
        # Fiche client à gauche
        uiOutput("fiche_client_ui"), # J'ai renommé pour éviter confusion
        
        # Table à droite
        card(
          card_header("Base de données Clients"),
          DT::dataTableOutput("table_clients")
        )
      )
    )
  )
)