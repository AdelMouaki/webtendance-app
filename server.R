server <- function(input, output, session) {

  ## formatage des nombres
  f_num <- function(x) { format(x, big.mark = " ", scientific = FALSE) }
  
  # Info-bulles et survolement
  style_tooltip <- "background: #2c3e50; color: white; padding:8px; border-radius:4px; font-size:12px; box-shadow: 2px 2px 5px rgba(0,0,0,0.3);"
  style_hover   <- "filter: brightness(1.2) drop-shadow(0 0 3px rgba(0,0,0,0.3)); cursor:pointer;"
  
  # ==============================================================================
  # DONNÉES RÉACTIVES
  # ==============================================================================
  
  ## Filtrage des données 
  donnees_globale <- reactive({
    df <- achats_traites
    if (input$annee != "Tout") df <- filter(df, year(Date.Achat) == input$annee)
    if (!("Tout" %in% input$site)) df <- filter(df, NOM_SITE %in% input$site)
    if (!("Tout" %in% input$sexe)) df <- filter(df, COD_SEXE %in% input$sexe)
    if (!("Tout" %in% input$age)) df <- filter(df, TrancheAge %in% input$age)
    df
  })
  
  ## Filtrage des données géographiques pour KPI
  donnees_geo <- reactive({
    df <- donnees_globale()
    if (!is.null(input$map_selected)) df <- filter(df, dep %in% input$map_selected)
    df
  })
  
  # ==============================================================================
  # KPI
  # ==============================================================================
  
  kpi_global <- reactive({
    df <- donnees_globale()
    list(total = sum(df$Mnt.Achat, na.rm=TRUE), nb = nrow(df), panier = mean(df$Mnt.Achat, na.rm=TRUE), clients = n_distinct(df$Id.Client))
  })
  
  ## KPI global
  output$kpi_montant_total <- renderText({ f_num(kpi_global()$total) })
  output$kpi_nb_achats     <- renderText({ f_num(kpi_global()$nb) })
  output$kpi_panier_moyen  <- renderText({ paste(f_num(round(kpi_global()$panier)), "€") })
  output$kpi_nb_clients    <- renderText({ f_num(kpi_global()$clients) })
  
  ## Calcul des KPI selon la zone sélectionnée
  stats_geo <- reactive({
    df <- donnees_geo()
    list(ca = sum(df$Mnt.Achat, na.rm=TRUE), vol = nrow(df), panier = ifelse(nrow(df)==0, 0, mean(df$Mnt.Achat, na.rm=TRUE)),clients = n_distinct(df$Id.Client))
  })
  
  output$titre_kpi_geo   <- renderText({ ifelse(is.null(input$map_selected), "France entière", "Zone(s) sélectionnée(s)") })
  output$kpi_geo_ca      <- renderUI({ h3(paste(f_num(stats_geo()$ca), "€")) })
  output$kpi_geo_vol     <- renderUI({ h3(f_num(stats_geo()$vol)) })
  output$kpi_geo_panier  <- renderUI({ h3(paste(f_num(round(stats_geo()$panier)), "€")) })
  output$kpi_geo_clients <- renderUI({ h3(f_num(stats_geo()$clients)) })
  
  
  # ==============================================================================
  # GRAPHIQUES GLOBAL
  # ==============================================================================
  
  output$distPlot <- renderGirafe({
    ## Arrêt si aucune donnée disponible
    req(nrow(donnees_globale()) > 0)
    
    ## Agrégation par site et tri décroissant
    df <- donnees_globale() %>%
      group_by(NOM_SITE) %>% summarise(Montant = sum(Mnt.Achat, na.rm = TRUE)) %>% arrange(desc(Montant)) %>%
      mutate(NOM_SITE = factor(NOM_SITE, levels = sites_levels))
    
    gg <- ggplot(df, aes(x = reorder(NOM_SITE, -Montant), y= Montant)) +
      geom_col_interactive(aes(fill = NOM_SITE, data_id = NOM_SITE,
                               tooltip = paste0("<b>Site :</b> ", NOM_SITE, "<br><b>Montant :</b> ", f_num(Montant), " €"))) + 
      scale_y_continuous(labels = f_num) +
      labs(x = NULL, y = NULL) + scale_fill_viridis_d(drop = FALSE) + theme_minimal() +
      theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust = 0.8), legend.position = "none")
    
    ## interactif
    girafe(ggobj = gg, width_svg = 8) %>% 
      girafe_options(opts_hover(css = style_hover), opts_tooltip(css = style_tooltip, use_fill = TRUE),
                     opts_selection(type = "multiple", only_shiny = TRUE), opts_toolbar(saveaspng = FALSE))
  })
  
  output$evoMensuelle <- renderGirafe({
    req(nrow(donnees_globale()) > 0)
    
    ## Agrégation mensuelle
    df <- donnees_globale() %>%
      mutate(Mois = floor_date(Date.Achat, unit = "month"), NOM_SITE = factor(NOM_SITE, levels = sites_levels)) %>%
      group_by(Mois, NOM_SITE) %>% summarise(Montant = sum(Mnt.Achat, na.rm = TRUE), .groups="drop") %>%
      mutate(tooltip = paste0("<b>", format(Mois, "%b %Y"), "</b><br><b>Site :</b> ", NOM_SITE, "<br><b>Montant :</b> ", f_num(Montant), " €"))
    
    gg <- ggplot(df, aes(x = Mois, y = Montant, color = NOM_SITE, group = NOM_SITE)) +
      geom_line_interactive(aes(tooltip = tooltip, data_id = NOM_SITE), linewidth = 1) +
      geom_point_interactive(aes(tooltip = tooltip, data_id = NOM_SITE), size = 3) +
      scale_y_continuous(labels = f_num) + scale_color_viridis_d(drop = FALSE) +
      labs(x = NULL, y = NULL, color = NULL) + theme_minimal() + theme(legend.position = "bottom")
    
    girafe(ggobj = gg, width_svg = 8, height_svg = 5) %>%
      
      ## Grise les autres courbes au survol
      girafe_options(opts_hover(css = "stroke-width: 3px; r: 7px; transition: all 0.2s ease;"),
                     opts_hover_inv(css = "opacity:0.2; filter: grayscale(100%);"),
                     opts_tooltip(css = style_tooltip, use_fill = TRUE), opts_selection(type = "multiple", only_shiny = TRUE),
                     opts_sizing(rescale = TRUE), opts_toolbar(saveaspng = FALSE))
  })
  
  # ==============================================================================
  # CARTE
  # ==============================================================================
  
  output$map <- renderGirafe({
    ## Lis les données géographiques
    if (!exists("deps")) deps <- st_read("departements.geojson", quiet = TRUE)
    
    achats_dep <- donnees_globale() %>% group_by(dep) %>% summarise(ca = sum(Mnt.Achat, na.rm = TRUE), .groups = "drop")
    
    ## Jointure : Données spatiales + Données d'achat
    carte_join <- deps %>% left_join(achats_dep, by = c("code" = "dep"))
    
    gg <- ggplot(carte_join) +
      geom_sf_interactive(aes(fill = ca, data_id = code,
                              tooltip = paste0("<b>", nom, " (", code, ")</b><br>CA: ", f_num(ca), " €")), color = "white", size = 0.1) +
      scale_fill_viridis_c(option = "magma", direction = -1, name = "Chiffre d'Affaires (€)", labels = f_num) +
      theme_void() + theme(legend.position = "right", legend.title = element_text(face = "bold", size = 10)) 
    
    # CSS pour la séléction
    css_sel <- "fill: #800020 !important; stroke: #800020 !important; stroke-width: 1.5px !important; filter: drop-shadow(0 0 2px rgba(0,0,0,0.3)) !important; opacity: 1 !important;" 
    
    girafe(ggobj = gg, width_svg = 8, height_svg = 6, 
           options = list(opts_hover(css = "filter: brightness(1.2); cursor: pointer;"),
                          opts_tooltip(css = style_tooltip), # Tooltip blanc sur fond sombre
                          opts_selection(type = "multiple", css = css_sel, only_shiny = TRUE),
                          opts_toolbar(saveaspng = FALSE), opts_sizing(rescale = TRUE)))
  })
  
  # ==============================================================================
  # ANALYSE CLIENT
  # ==============================================================================
  
  output$clients_density <- renderGirafe({
    
    ## Transformation log pour linéariser la distribution
    df <- donnees_globale() %>% group_by(Id.Client) %>%
      summarise(nb = n(), mnt = sum(Mnt.Achat, na.rm=TRUE), .groups="drop") %>%
      filter(nb > 0, mnt > 0) %>% mutate(lnb = log(nb), lmnt = log(mnt))
    
    p <- ggplot(df, aes(x = lnb, y = lmnt)) +
      geom_bin2d_interactive(bins = 30, aes(fill = after_stat(count), data_id = after_stat(paste0(x, "_", y)),
                                            tooltip = paste0("<b>Nombre de clients :</b> ", after_stat(count), "<br><b>Achats (≈) :</b> ", round(exp(after_stat(x))),
                                                             "<br><b>Total (≈) :</b> ", f_num(round(exp(after_stat(y)))), " €"))) +
      scale_fill_viridis_c(name = "Nombre de clients", labels = f_num, trans = "sqrt") +
      labs(title = "Densité des clients", x = "log Nombre d’achats", y = "log Montant total (€)") + theme_minimal(base_size = 13)
    
    girafe(ggobj = p, width_svg = 10, height_svg = 6) %>%
      girafe_options(opts_hover(css = "stroke: black; stroke-width: 1.5px;"), opts_hover_inv(css = "opacity:0.25;"),
                     opts_tooltip(css = style_tooltip), opts_selection(type = "none"), opts_sizing(rescale = TRUE))
  })
  
  # ==============================================================================
  # DÉTAIL CLIENT
  # ==============================================================================
  
  ## Identification du site favori basé sur la fréquence d'achat par client
  site_prefere <- reactive({
    donnees_globale() %>% count(Id.Client, NOM_SITE) %>% arrange(desc(n)) %>% group_by(Id.Client) %>% slice(1) %>% select(Id.Client, Site_Favori = NOM_SITE)
  })
  
  ## Agrégation des indicateurs de performance (CA, Panier, Volume) par client
  stats_clients <- reactive({
    donnees_globale() %>% group_by(Id.Client) %>% summarise(Commandes = n(), Total = sum(Mnt.Achat), Panier = mean(Mnt.Achat)) %>% arrange(desc(Total))
  })
  
  ## Fusion des infos  avec les statistiques d'achat
  donnees_affichage <- reactive({
    req(nrow(stats_clients()) > 0)
    infos <- Clients %>% select(Id.Client, TXT_NOM, TXT_PRENOM, COD_SEXE, Age, COD_POSTAL)
    stats_clients() %>% left_join(infos, by="Id.Client") %>% left_join(site_prefere(), by="Id.Client") %>%
      select(Id.Client, TXT_NOM, TXT_PRENOM, COD_SEXE, Age, COD_POSTAL, Site_Favori, Commandes, Total, Panier)
  })
  
  ## Rendu du tableau interactif permettant la sélection d'un profil
  output$table_clients <- DT::renderDataTable({
    DT::datatable(donnees_affichage(), selection="single", rownames=FALSE, options=list(pageLength=8, scrollX=TRUE, dom='frtp')) %>%
      DT::formatCurrency(c("Total", "Panier"), currency="€", digits=0)
  })
  
  ## Génération dynamique de la fiche du client sélectionné
  output$fiche_client_ui <- renderUI({
    
    ## Gestion de l'affichage par défaut (si aucune ligne n'est sélectionnée)
    if (is.null(input$table_clients_rows_selected)) {
      return(card(height="400px", class="d-flex align-items-center justify-content-center bg-light text-muted", 
                  h5("Sélectionnez un client", class="mt-2")))
    }
    
    sel <- donnees_affichage() %>% slice(input$table_clients_rows_selected)
    
    ## Attribution du badge selon le CA total
    badge_clt <- if (sel$Total >= 1000000) span(class="badge bg-primary", "💎 Diamant") else 
      if (sel$Total >= 500000) span(class="badge bg-info", "💿 Platine") else 
        if (sel$Total >= 100000) span(class="badge bg-warning text-dark", "🏆 Or") else 
          span(class="badge bg-secondary", "Standard")
    
    color_avatar <- if(sel$COD_SEXE == "Homme") "text-primary" else "text-danger"
    
    ## Construction de la carte HTML
    card(
      class = "shadow border-0 p-0 overflow-hidden", style = "border-radius: 12px;",
      
      div(class = "bg-primary text-white p-3 text-center",
          h4(class="m-0 fw-bold", paste(sel$TXT_PRENOM, sel$TXT_NOM))
      ),
      
      card_body(
        class = "p-3",
        div(class="row align-items-center h-100",
            
            # COLONNE GAUCHE : Avatar
            div(class="col-4 text-center border-end pe-3", 
                bs_icon("person-circle", size="4.5em", class=paste("mb-3", color_avatar)), 
                div(badge_clt)
            ),

            div(class="col-8 ps-4", 
                
                # Sexe
                div(class="d-flex justify-content-between align-items-center mb-2 border-bottom pb-1", 
                    span(class="text-muted small", "Sexe"), 
                    strong(sel$COD_SEXE)),
                
                # Age
                div(class="d-flex justify-content-between align-items-center mb-2 border-bottom pb-1", 
                    span(class="text-muted small", "Age"), 
                    strong(sel$Age)),
                
                # CP
                div(class="d-flex justify-content-between align-items-center mb-2 border-bottom pb-1", 
                    span(class="text-muted small", "Code Postal"), 
                    strong(sel$COD_POSTAL)),
                
                # Site
                div(class="d-flex justify-content-between align-items-center", 
                    span(class="text-muted small", "Site Favori"), 
                    span(class="text-primary fw-bold", sel$Site_Favori))
            )
        )
      ),
      
      div(class = "bg-light p-3 d-flex justify-content-around align-items-center border-top",
          div(class="text-center", h5(sel$Commandes, class="fw-bold m-0"), tags$small(class="text-muted", "Commandes")),
          div(class="text-center", h5(format(sel$Total, big.mark=" ", scientific=FALSE), class="fw-bold m-0 text-primary"), tags$small(class="text-muted", "Total €")),
          div(class="text-center", h5(format(round(sel$Panier), big.mark=" "), class="fw-bold m-0"), tags$small(class="text-muted", "Panier Moy."))
      )
    )
  })
}