function(input, output, session) {
  
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
    
    donnees_filtrees
  })
  
  # Graphique des achats par site
  output$distPlot <- renderGirafe({
    
    SiteAchatsFiltre <- achats_filtres() %>%
      group_by(NOM_SITE) %>% # On regroupe les lignes par site
      summarise(Montant = sum(Mnt.Achat)) %>%
      arrange(desc(Montant)) %>%
      mutate(NOM_SITE = factor(NOM_SITE, levels = NOM_SITE), Montant_fmt = format(Montant, big.mark = " ", scientific = FALSE)) # On transforme NOM_SITE en facteur avec l'ordre du tri précédent
    
    SiteAchats <- ggplot(SiteAchatsFiltre, aes(x = NOM_SITE, y = Montant)) +
      geom_col_interactive(aes( fill = NOM_SITE, tooltip = paste0( "<b>Site :</b> ", NOM_SITE, "<br><b>Montant :</b> ", Montant_fmt, " €" ),data_id = NOM_SITE))+ #Mis en forme de l'infobulle 
           
      scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE))+ #Affichage des montants sans écriture scientifique + séparateur de milliers
      labs(
        title = paste0("Montant d'achat par site en ", input$annee),
        x = "Site",
        y = "Montant total (en €)"
      )+ 
      scale_fill_viridis_d() +
      theme_minimal() +
      theme(
      axis.text.x = element_text(angle = 20, vjust = 1, hjust = 0.8) #ajustement de la rotation du texte
      )
    
    
    hover_css <- "
    filter: brightness(1.4) drop-shadow(0 0 5px rgba(78, 84, 200, 0.5)); 
    transition: all 0.5s ease-out;  
    "# Effet de halo au survol et transition de 0.5sec pour la fluidité
    
    tooltips_css <-"
    background:white;padding:8px;border-radius:6px;font-size:13px; 
    " # fond blanc et definition de la taille et bords
    
    girafe(
      ggobj = SiteAchats,
      width_svg = 8 #largeur du graphique 
      )%>% 
      girafe_options(
        opts_hover(css = hover_css),
        opts_tooltip(css = tooltips_css)
      )

  })
  
  # Graphique d'évolution mensuelle des montants
  
  output$evoMensuelle <- renderGirafe({
  dataMois <- achats_filtres() %>%
    mutate(
      Mois = format(Date.Achat, "%Y-%m"),
      Montant_fmt = format(Mnt.Achat, big.mark = " ", scientific = FALSE)
    ) %>%
    group_by(Mois) %>%
    summarise(Montant_total = sum(Mnt.Achat)) %>%
    arrange(Mois) %>%
    mutate(
      Mois_aff = format(as.Date(paste0(Mois, "-01")), "%b %Y"),
      Montant_fmt = format(Montant_total, big.mark = " ", scientific = FALSE)
    )
  })
  
}
