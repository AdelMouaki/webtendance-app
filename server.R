server <- function(input, output, session) {
  
  # Jeu de données filtré selon les choix utilisateur
  achats_filtres <- reactive({
    donnees_filtrees <- Achats

    # Filtre année
    if (input$annee != "Tout") {
      donnees_filtrees <- donnees_filtrees %>%
        filter(year(Date.Achat) == input$annee)
    }
  
     # Filtre site
     if (!("Tout" %in% input$site)) {
       donnees_filtrees <- donnees_filtrees %>%
         filter(NOM_SITE %in% input$site)
     }

     # Filtre sexe
     if (!("Tout" %in% input$sexe)) {
       donnees_filtrees <- donnees_filtrees %>%
         filter(COD_SEXE %in% input$sexe)
     }

     # Filtre tranche d'age
     if (!("Tout" %in% input$age)) {
       donnees_filtrees <- donnees_filtrees %>%
         filter(TrancheAge %in% input$age)
    }


    donnees_filtrees
  })
  
  kpi <- reactive({
    df <- achats_filtres()
    
    tibble(
      montant_total = sum(df$Mnt.Achat, na.rm = TRUE),
      nb_achats = nrow(df),
      panier_moyen = ifelse(nrow(df) == 0, 0, mean(df$Mnt.Achat, na.rm = TRUE)),
      nb_clients = n_distinct(df$Id.Client),
      montant_moyen_client = df %>% 
        group_by(Id.Client) %>% 
        summarise(total = sum(Mnt.Achat, na.rm=TRUE)) %>% 
        summarise(mean(total)) %>% 
        pull()
    )
  })
  
  output$kpi_montant_total <- renderUI({
    div(class="kpi-box",
        h4("Montant total (en €)"),
        h3(format(kpi()$montant_total, big.mark=" ", scientific = FALSE))
    )
  })
  
  output$kpi_nb_achats <- renderUI({
    div(class="kpi-box",
        h4("Nombre d'achats"),
        h3(kpi()$nb_achats, big.mark=" ")
    )
  })
  
  output$kpi_panier_moyen <- renderUI({
    div(class="kpi-box",
        h4("Panier moyen"),
        h3(format(round(kpi()$panier_moyen), big.mark=" "))
    )
  })
  
  output$kpi_nb_clients <- renderUI({
    div(class="kpi-box",
        h4("Nombre de clients"),
        h3(kpi()$nb_clients, big.mark=" ")
    )
  })
  
  output$kpi_montant_moyen_client <- renderUI({
    div(class="kpi-box",
        h4("Montant moyen par client"),
        h3(format(round(kpi()$montant_moyen_client), big.mark=" "))
    )
  })
  
  
  # Graphique des achats par site
  output$distPlot <- renderGirafe({
    
    SiteAchatsFiltre <- achats_filtres() %>%
      group_by(NOM_SITE) %>% # On regroupe les lignes par site
      summarise(Montant = sum(Mnt.Achat, na.rm = TRUE)) %>%
      arrange(desc(Montant)) %>%
      mutate(NOM_SITE = factor(NOM_SITE, levels = sites_levels), Montant_fmt = format(Montant, big.mark = " ", scientific = FALSE)) # On transforme NOM_SITE en facteur avec l'ordre du tri précédent
    
    SiteAchats <- ggplot(SiteAchatsFiltre, aes(x = reorder(NOM_SITE, -Montant), y= Montant)) +
      geom_col_interactive(aes( fill = NOM_SITE, tooltip = paste0( "<b>Site :</b> ", NOM_SITE, "<br><b>Montant :</b> ", Montant_fmt, " €" ),data_id = NOM_SITE))+ #Mis en forme de l'infobulle 
           
      scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE))+ #Affichage des montants sans écriture scientifique + séparateur de milliers
      labs(
        title = paste0("Montant d'achat par site en ", input$annee),
        x = "Site",
        y = "Montant total (en €)"
      )+ 
      scale_fill_viridis_d(drop = FALSE, name = "Site") +
      theme_minimal() +
      theme(
      axis.text.x = element_text(angle = 20, vjust = 1, hjust = 0.8) #ajustement de la rotation du texte
      )
    
    
    hover_css <- "
    filter: brightness(1.4) drop-shadow(0 0 5px rgba(78, 84, 200, 0.5)); 
    transition: all 0.5s ease-out;  
    "# Effet de lumière au survol et transition de 0.5sec pour la fluidité
    
    tooltip_css <- "
    background:white;
    color: #ECF0F1;
    padding:8px;
    border-radius:6px;
    font-size:13px;
    "
    
    girafe(
      ggobj = SiteAchats,
      width_svg = 8 #largeur du graphique 
      )%>% 
      girafe_options(
        opts_hover(css = hover_css),
        opts_tooltip(css = tooltip_css, use_fill = TRUE),
        opts_selection(
          type = "multiple",
          only_shiny = TRUE
        )
      )

  })
  
  # Graphique d'évolution mensuelle des montants
  output$evoMensuelle <- renderGirafe({
    evoMensuelleFiltre <- achats_filtres() %>%
      mutate(
        Mois = floor_date(Date.Achat, unit = "month"), NOM_SITE = factor(NOM_SITE, levels = sites_levels))%>%
      group_by(Mois, NOM_SITE) %>%
      summarise(
        Montant_total = sum(Mnt.Achat, na.rm = TRUE)) %>%
      arrange(Mois) %>%
      mutate(
        Mois_aff = format(Mois, "%b %Y"),
        Montant_fmt = format(Montant_total, big.mark = " ", scientific = FALSE),
        tooltip_txt = paste0(
          "<b>Mois :</b> ", Mois_aff,
          "<br><b>Site :</b> ", NOM_SITE,
          "<br><b>Montant :</b> ", Montant_fmt, " €"
        ),
        data_id_txt = NOM_SITE
      )
    
    evoMensuelle <- ggplot(
      evoMensuelleFiltre,
      aes(x = Mois, y = Montant_total, color = NOM_SITE, group = NOM_SITE)) +
      geom_line_interactive(
        aes(tooltip = tooltip_txt, data_id = data_id_txt),
        linewidth = 1) +
      geom_point_interactive(
        aes(
          tooltip = tooltip_txt,
          data_id = data_id_txt),
        size = 3) +
      scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) +
      scale_color_viridis_d(drop = FALSE, name = "Site") +
      labs(
        title = paste0("Évolution mensuelle des montants en ", input$annee),
        x = "Date",
        y = "Montant total (en €)",
        color = "Site"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 20, vjust = 1, hjust = 0.8)
      )
    
    # CSS
    hover_css <- "
    stroke-width: 2px;
    r: 9px;
    transition: all 0.5s ease;
    "
    
    tooltip_css <- "
    background:white;
    color: #ECF0F1;
    padding:8px;
    border-radius:6px;
    font-size:13px;
    "
    # options
    girafe(
      ggobj = evoMensuelle,
      width_svg = 10,
      height_svg = 6
    ) %>%
      girafe_options(
        opts_hover(css = hover_css),
        opts_tooltip(css = tooltip_css, use_fill = TRUE),
        opts_sizing(rescale = TRUE),
        opts_selection(
          type = "multiple",
          only_shiny = TRUE
        ),
        css = "
        stroke: black;
        stroke-width: 3px;
        opacity: 1;
      ",
    opts_hover_inv(
      css = "opacity:0.2;"
    ))

  })
  
  
  carte_data <- reactive({
    
    deps <- st_read(
      "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements.geojson",
      quiet = TRUE
    )
    
    achats_dep <- achats_filtres() %>%
      mutate(
        dep = case_when(
          substr(COD_POSTAL, 1, 2) == "20" & substr(COD_POSTAL, 1, 3) < "202" ~ "2A",
          substr(COD_POSTAL, 1, 2) == "20" & substr(COD_POSTAL, 1, 3) >= "202" ~ "2B",
          TRUE ~ substr(COD_POSTAL, 1, 2)
        )
      ) %>%
      group_by(dep) %>%
      summarise(
        nb_commandes = n(),
        montant_total = sum(Mnt.Achat, na.rm = TRUE),
        .groups = "drop"
      )
    
    deps %>% left_join(achats_dep, by = c("code" = "dep"))
  })
  
  output$map <- renderGirafe({
    
    p <- ggplot(carte_data()) +
      geom_sf_interactive(
        aes(
          fill = montant_total,
          tooltip = paste0(
            "<b>Département : </b>", nom,
            "<br><b>Montant : </b>", format(montant_total, big.mark = " "),
            "<br><b>Commandes : </b>", nb_commandes
          )
        ),
        color = "white",
      ) +
      scale_fill_viridis_c(
        na.value = "grey90",
        labels = scales::label_number(big.mark = " ")
      ) +
      labs(
        title = paste0("Évolution mensuelle des montants en ", input$annee))+
      theme_void()
    
    girafe(ggobj = p)
  })
  
  # =========================
  # CLIENTS
  # =========================
  clients_segmentation <- reactive({
    
    achats_filtres() %>%
      group_by(Id.Client) %>%
      summarise(
        nb_achats = n(),
        montant_total = sum(Mnt.Achat, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(nb_achats > 0, montant_total > 0) %>%
      mutate(
        log_nb_achats = log(nb_achats),
        log_montant_total = log(montant_total)
      )
  })
  
  output$clients_density <- renderGirafe({
    
    df <- clients_segmentation()
    
    p <- ggplot(df, aes(
      x = log_nb_achats,
      y = log_montant_total
    )) +
      
      geom_bin2d_interactive(
        bins = 30,
        aes(
          fill = after_stat(count),
          tooltip = paste0(
            "<b>Nb de clients :</b> ", after_stat(count),
            "<br><b>Nb d’achats (≈) :</b> ", round(exp(after_stat(x))),
            "<br><b>Montant total (≈) :</b> ",
            format(round(exp(after_stat(y))), big.mark = " "), " €"
          ),
          data_id = after_stat(paste0(x, "_", y))
        )
      ) +
      
      scale_fill_viridis_c(
        name = "Nb clients",
        labels = scales::label_number(big.mark = " "),
        trans = "sqrt"
      ) +
      
      labs(
        title = "Densité des clients par comportement d'achat",
        x = "log(Nombre d’achats)",
        y = "log(Montant total (€))"
      ) +
      
      theme_minimal(base_size = 13)
    
    girafe(
      ggobj = p,
      width_svg = 10,
      height_svg = 6
    ) %>%
      girafe_options(
        opts_hover(css = "
        stroke: black;
        stroke-width: 1.5px;
      "),
        opts_hover_inv(css = "opacity:0.25;"),
        opts_tooltip(css = "
        background: white;
        color: black;
        padding: 8px;
        border-radius: 6px;
        font-size: 13px;
      "),
        opts_selection(type = "single", only_shiny = TRUE),
        opts_sizing(rescale = TRUE)
      )
  })
  
####CLIENTS DETAILLES####
  
  clients_table <- reactive({
    
    achats_filtres() %>%
      group_by(Id.Client,TXT_NOM,TXT_PRENOM, COD_SEXE,TrancheAge,COD_POSTAL,NBR_ENFT) %>%
      summarise(
        nb_achats = n(),
        montant_total = sum(Mnt.Achat, na.rm = TRUE),
        panier_moyen = mean(Mnt.Achat, na.rm = TRUE),
        site_prefere = NOM_SITE[which.max(table(NOM_SITE))],
        .groups = "drop") %>%
      arrange(desc(montant_total))
  })
  
  output$table_clients <- DT::renderDataTable({
    
    DT::datatable(
      clients_table(),
      rownames = FALSE,
      selection = "single",
      options = list(
        pageLength = 10,
        autoWidth = TRUE
      )
    ) %>%
      DT::formatCurrency(
        c("montant_total", "panier_moyen"),
        currency = "€",
        mark = " ",
        digits = 0
      )
  })
  
  client_selectionne <- reactive({
    
    req(input$table_clients_rows_selected)
    
    clients_table() %>%
      slice(input$table_clients_rows_selected)
  })
  
  output$fiche_client <- renderUI({
    
    df <- client_selectionne()
    req(nrow(df) == 1)
    
    div(
      class = "client-card",
      
      h3(paste(df$TXT_PRENOM, df$TXT_NOM)),
      tags$hr(),
      
      fluidRow(
        column(6, strong("ID client : "), df$Id.Client),
        column(6, strong("Sexe : "), df$COD_SEXE)
      ),
      
      fluidRow(
        column(6, strong("Tranche d’âge : "), df$TrancheAge),
        column(6, strong("Code postal : "), df$COD_POSTAL)
      ),
      
      fluidRow(
        column(6, strong("Nombre d’enfants : "), df$NBR_ENFT),
        column(6, strong("Site préféré : "), df$site_prefere)
      ),
      
      tags$hr(),
      
      fluidRow(
        column(4, strong("Nb achats"), df$nb_achats),
        column(4, strong("Montant total (€)"),
               format(round(df$montant_total), big.mark = " ")),
        column(4, strong("Panier moyen (€)"),
               format(round(df$panier_moyen), big.mark = " "))
      )
    )
  })
  
  
}
