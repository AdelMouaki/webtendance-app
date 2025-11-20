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
  output$distPlot <- renderPlot({
    
    SiteAchats <- achats_filtres() %>%
      group_by(NOM_SITE) %>% # On regroupe les lignes par site
      summarise(Montant = sum(Mnt.Achat)) %>%
      arrange(desc(Montant)) %>%
      mutate(NOM_SITE = factor(NOM_SITE, levels = NOM_SITE)) # On transforme NOM_SITE en facteur avec l'ordre du tri précédent
    
    ggplot(SiteAchats, aes(x = NOM_SITE, y = Montant)) +
      geom_col() +
      labs(
        title = paste0("Montant d'achat par site en ", input$annee),
        x = "Site",
        y = "Montant total"
      )
  })
}
