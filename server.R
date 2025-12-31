server <- function(input, output, session) {
  
  # =========================
  # 0. PRÉPARATION DES DONNÉES
  # =========================
  
  Achats_Ready <- Achats %>%
    mutate(
      dep = case_when(
        substr(COD_POSTAL, 1, 2) == "20" & substr(COD_POSTAL, 1, 3) < "202" ~ "2A",
        substr(COD_POSTAL, 1, 2) == "20" & substr(COD_POSTAL, 1, 3) >= "202" ~ "2B",
        TRUE ~ substr(COD_POSTAL, 1, 2)
      )
    )
  
  # =========================
  # 1. LOGIQUE DE FILTRAGE
  # =========================
  
  donnees_globale <- reactive({
    df <- Achats_Ready
    if (input$annee != "Tout") df <- filter(df, year(Date.Achat) == input$annee)
    if (!("Tout" %in% input$site)) df <- filter(df, NOM_SITE %in% input$site)
    if (!("Tout" %in% input$sexe)) df <- filter(df, COD_SEXE %in% input$sexe)
    if (!("Tout" %in% input$age)) df <- filter(df, TrancheAge %in% input$age)
    df
  })
  
  kpi_geo_data <- reactive({
    df <- donnees_globale()
    selection_dep <- input$map_selected
    if (!is.null(selection_dep) && length(selection_dep) > 0) {
      df <- df %>% filter(dep %in% selection_dep)
    }
    df
  })
  
  # =========================
  # 2. KPI (VUE GLOBALE)
  # =========================
  
  kpi_global <- reactive({
    df <- donnees_globale()
    list(
      total = sum(df$Mnt.Achat, na.rm = TRUE),
      nb = nrow(df),
      panier = ifelse(nrow(df) == 0, 0, mean(df$Mnt.Achat, na.rm = TRUE)),
      clients = n_distinct(df$Id.Client)
    )
  })
  
  output$kpi_montant_total <- renderText({ format(kpi_global()$total, big.mark=" ", scientific=FALSE) })
  output$kpi_nb_achats <- renderText({ format(kpi_global()$nb, big.mark=" ") })
  output$kpi_panier_moyen <- renderText({ paste(format(round(kpi_global()$panier), big.mark=" "), "€") })
  output$kpi_nb_clients <- renderText({ format(kpi_global()$clients, big.mark=" ") })
  
  # =========================
  # 3. KPI (CARTOGRAPHIE)
  # =========================
  
  stats_geo <- reactive({
    df <- kpi_geo_data()
    list(
      ca = sum(df$Mnt.Achat, na.rm=TRUE),
      vol = nrow(df),
      panier = ifelse(nrow(df)==0, 0, mean(df$Mnt.Achat, na.rm=TRUE))
    )
  })
  
  output$titre_kpi_geo <- renderText({
    if(is.null(input$map_selected)) "France Entière (Cliquez pour filtrer)" else "Zone Sélectionnée"
  })
  
  output$kpi_geo_ca <- renderUI({ h3(paste(format(stats_geo()$ca, big.mark=" ", scientific=FALSE), "€")) })
  output$kpi_geo_vol <- renderUI({ h3(format(stats_geo()$vol, big.mark=" ", scientific=FALSE)) })
  output$kpi_geo_panier <- renderUI({ h3(paste(format(round(stats_geo()$panier), big.mark=" ", scientific=FALSE), "€")) })
  
  # =========================
  # 4. GRAPHIQUES VUE GLOBALE (VERSION SCROLLABLE / MILLIONS)
  # =========================
  
  output$distPlot <- renderGirafe({
    req(nrow(donnees_globale()) > 0)
    
    df_site <- donnees_globale() %>%
      group_by(NOM_SITE) %>%
      summarise(Montant = sum(Mnt.Achat, na.rm = TRUE)) %>%
      arrange(desc(Montant)) %>% 
      mutate(Site_Label = str_trunc(NOM_SITE, 20))
    
    gg <- ggplot(df_site, aes(x = reorder(Site_Label, -Montant), y = Montant)) +
      geom_col_interactive(
        aes(fill = NOM_SITE, 
            tooltip = paste0("<b>", NOM_SITE, "</b><br>", format(Montant, big.mark=" ", scientific=FALSE), " €"), 
            data_id = NOM_SITE),
        width = 0.6
      ) +
      scale_y_continuous(
        labels = scales::label_number(scale = 1e-6, suffix = " M€", accuracy = 0.1),
        expand = expansion(mult = c(0, 0.05))
      ) +
      scale_fill_viridis_d(option = "D") + 
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 11) + 
      theme(
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(5, 5, 5, 5),
        axis.text.x = element_text(angle = 0, face = "bold", color="#444444"), 
        axis.text.y = element_text(color="#666666")
      )
    
    girafe(
      ggobj = gg, 
      width_svg = 12, height_svg = 6, 
      options = list(
        opts_hover(css = "filter: brightness(1.1); stroke: white; stroke-width: 2px; cursor: pointer;"),
        opts_tooltip(css="background:white; color:#333; padding:8px; border-radius:4px; box-shadow: 0px 2px 5px rgba(0,0,0,0.2); font-size:12px;"),
        opts_selection(type = "none"), 
        opts_toolbar(saveaspng = FALSE),
        opts_sizing(rescale = TRUE)
      )
    )
  })
  
  output$evoMensuelle <- renderGirafe({
    req(nrow(donnees_globale()) > 0)
    
    df_evo <- donnees_globale() %>%
      mutate(Mois = floor_date(Date.Achat, unit = "month")) %>%
      group_by(Mois, NOM_SITE) %>%
      summarise(Montant = sum(Mnt.Achat, na.rm=TRUE), .groups="drop")
    
    gg <- ggplot(df_evo, aes(x = Mois, y = Montant, color = NOM_SITE, group = NOM_SITE)) +
      geom_line_interactive(
        aes(data_id = NOM_SITE, tooltip = NOM_SITE), 
        linewidth = 1, alpha = 0.9
      ) +
      geom_point_interactive(
        aes(tooltip = paste0("<b>", format(Mois, "%B %Y"), "</b><br>",
                             NOM_SITE, "<br>",
                             "<b>", format(Montant, big.mark=" ", scientific=FALSE), " €</b>"), 
            data_id = NOM_SITE), 
        size = 2, shape = 21, fill = "white", stroke = 1.2
      ) +
      scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = " M€", accuracy = 0.1)) +
      scale_color_viridis_d(option = "D", name = NULL) +
      theme_minimal(base_size = 11) + 
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(color = "#666666")
      )
    
    girafe(
      ggobj = gg, 
      width_svg = 12, height_svg = 6, 
      options = list(
        opts_hover(css = "stroke-width: 3px; opacity: 1;"), 
        opts_hover_inv(css = "opacity: 0.15;"), 
        opts_tooltip(css = "background:white; color:#333; padding:10px; border-radius:5px; box-shadow: 0px 4px 10px rgba(0,0,0,0.2); font-size:13px;"),
        opts_selection(type = "none"), 
        opts_toolbar(saveaspng = FALSE),
        opts_sizing(rescale = TRUE)
      )
    )
  })
  
  # =========================
  # 5. CARTOGRAPHIE
  # =========================
  
  output$map <- renderGirafe({
    file_map <- "departements.geojson"
    validate(need(file.exists(file_map), "ERREUR CRITIQUE : Le fichier 'departements.geojson' est introuvable."))
    
    deps <- st_read(file_map, quiet = TRUE)
    
    achats_dep <- donnees_globale() %>%
      group_by(dep) %>%
      summarise(montant_total = sum(Mnt.Achat, na.rm = TRUE), nb_commandes = n(), .groups = "drop")
    
    carte_join <- deps %>% left_join(achats_dep, by = c("code" = "dep"))
    
    gg <- ggplot(carte_join) +
      geom_sf_interactive(
        aes(fill = montant_total, 
            tooltip = paste0("<b>", nom, " (", code, ")</b><br>CA: ", format(montant_total, big.mark=" ", scientific=FALSE), " €"),
            data_id = code), 
        color = "white", size = 0.1
      ) +
      scale_fill_viridis_c(
        option = "magma", 
        direction = -1, 
        labels = function(x) format(x, big.mark=" ", scientific=FALSE), 
        name="CA (€)"
      ) +
      theme_void() + theme(legend.position = "right")
    
    girafe(ggobj = gg, width_svg = 8, height_svg = 6, 
           options = list(
             opts_tooltip(css = "background:white; color:#333; padding:5px; border-radius:4px;"), 
             opts_hover(css = "fill: #2c3e50; cursor: pointer;"),
             opts_selection(type = "multiple", css = "fill: #FF9F1C; stroke: white; stroke-width: 2px;", only_shiny = TRUE)
           ))
  })
  
  # =========================
  # 6. ANALYSE CLIENTS (Ton ancienne version restaurée)
  # =========================
  
  clients_segmentation <- reactive({
    donnees_globale() %>%
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
    
    p <- ggplot(df, aes(x = log_nb_achats, y = log_montant_total)) +
      
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
        title = NULL, 
        x = "log(Nombre d’achats)",
        y = "log(Montant total (€))"
      ) +
      
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank()
      )
    
    girafe(
      ggobj = p,
      width_svg = 12, 
      height_svg = 6,
      options = list(
        opts_hover(css = "stroke: #333333; stroke-width: 1.5px;"),
        opts_hover_inv(css = "opacity:0.25;"),
        opts_tooltip(css = "background: white; color: black; padding: 8px; border-radius: 6px; font-size: 13px; box-shadow: 0 2px 6px rgba(0,0,0,0.2);"),
        opts_selection(type = "none"), 
        opts_sizing(rescale = TRUE)
      )
    )
  })
  
  # =========================
  # 7. TABLE & FICHE CLIENT (Avec Segmentation Grands Comptes)
  # =========================
  
  clients_stats <- reactive({
    donnees_globale() %>% group_by(Id.Client) %>%
      summarise(Commandes=n(), Total=sum(Mnt.Achat, na.rm=TRUE), Panier=mean(Mnt.Achat, na.rm=TRUE)) %>%
      arrange(desc(Total))
  })
  
  clients_display <- reactive({
    req(nrow(clients_stats()) > 0)
    infos <- Clients %>% select(Id.Client, TXT_NOM, TXT_PRENOM, COD_SEXE, Age, TrancheAge, COD_POSTAL)
    clients_stats() %>% left_join(infos, by="Id.Client") %>% 
      select(Id.Client, TXT_NOM, TXT_PRENOM, COD_SEXE, Age, TrancheAge, COD_POSTAL, Commandes, Total, Panier)
  })
  
  output$table_clients <- DT::renderDataTable({
    DT::datatable(clients_display(), selection="single", rownames=FALSE, 
                  options=list(pageLength=8, scrollX=TRUE, dom='tp', deferRender=TRUE)) %>%
      DT::formatCurrency(c("Total", "Panier"), currency="€", digits=0)
  })
  
  output$fiche_client_ui <- renderUI({
    
    if (is.null(input$table_clients_rows_selected)) {
      return(
        card(
          height = "400px",
          class = "d-flex align-items-center justify-content-center text-muted border-dashed",
          style = "border: 2px dashed #ccc; background-color: #f8f9fa;", 
          div(class="text-center",
              bs_icon("cursor-fill", size = "3em", class="text-secondary mb-3"),
              h5("Sélectionnez un client"),
              tags$small("Cliquez sur une ligne du tableau pour voir le profil détaillé.")
          )
        )
      )
    }
    
    sel <- clients_display() %>% slice(input$table_clients_rows_selected)
    req(nrow(sel) > 0)
    
    # SEGMENTATION GRANDS COMPTES
    statut_badge <- if (sel$Total >= 1000000) {
      span(class="badge rounded-pill", style="background-color: #4b0082; color: white; border: 1px solid #fff;", "💎 Client DIAMANT")
    } else if (sel$Total >= 500000) {
      span(class="badge rounded-pill bg-info text-white", "💿 Client PLATINE")
    } else if (sel$Total >= 100000) {
      span(class="badge rounded-pill bg-warning text-dark", "🏆 Client OR")
    } else {
      span(class="badge rounded-pill bg-secondary", "Client Standard")
    }
    
    avatar_color <- if(sel$COD_SEXE == "Homme") "text-primary" else if(sel$COD_SEXE == "Femme") "text-danger" else "text-secondary"
    
    card(
      class = "shadow border-0", 
      style = "border-radius: 15px; overflow: hidden;",
      
      div(
        class = "bg-primary text-white p-3 d-flex justify-content-between align-items-center",
        style = "background: linear-gradient(135deg, #2c3e50 0%, #4ca1af 100%);",
        div(
          h4(paste(sel$TXT_PRENOM, sel$TXT_NOM), class="m-0 fw-bold"),
          tags$small(class="opacity-75", paste("ID Client:", sel$Id.Client))
        ),
        bs_icon("person-vcard", size="2em")
      ),
      
      card_body(
        class = "p-4",
        div(class="row align-items-center mb-4",
            div(class="col-4 text-center",
                bs_icon("person-circle", size="5em", class=avatar_color),
                div(class="mt-2", statut_badge)
            ),
            div(class="col-8",
                h6(class="text-muted text-uppercase fw-bold mb-3", "Informations"),
                div(class="d-flex justify-content-between border-bottom py-1", span("Sexe :"), strong(sel$COD_SEXE)),
                div(class="d-flex justify-content-between border-bottom py-1", span("Âge :"), strong(sel$Age)),
                div(class="d-flex justify-content-between border-bottom py-1", span("CP :"), strong(sel$COD_POSTAL)),
                div(class="d-flex justify-content-between py-1", span("Dép :"), strong(substr(sel$COD_POSTAL, 1, 2)))
            )
        ),
        hr(class="my-4"),
        h6(class="text-muted text-uppercase fw-bold mb-3", "Statistiques"),
        div(class="row g-2",
            div(class="col-4",
                div(class="p-2 rounded text-center h-100", style="background-color: #e3f2fd; color: #0d47a1;",
                    bs_icon("bag-check-fill", size="1.5em", class="mb-2"),
                    h4(sel$Commandes, class="fw-bold m-0"),
                    tags$small("Achats")
                )
            ),
            div(class="col-4",
                div(class="p-2 rounded text-center h-100", style="background-color: #e8f5e9; color: #1b5e20;",
                    bs_icon("currency-euro", size="1.5em", class="mb-2"),
                    h4(format(round(sel$Total), big.mark=" ", scientific=FALSE), class="fw-bold m-0"),
                    tags$small("Total")
                )
            ),
            div(class="col-4",
                div(class="p-2 rounded text-center h-100", style="background-color: #f3e5f5; color: #4a148c;",
                    bs_icon("cart-plus", size="1.5em", class="mb-2"),
                    h4(format(round(sel$Panier), big.mark=" ", scientific=FALSE), class="fw-bold m-0"),
                    tags$small("Panier")
                )
            )
        )
      )
    )
  })
}