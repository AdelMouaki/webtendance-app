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
      theme_minimal() 
    
    girafe(
      ggobj = SiteAchats,
      width_svg = 8, #largeur du graphique
    )
  })
}
