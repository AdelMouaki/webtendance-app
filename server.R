function(input, output, session) {

  Achats_Filtre <- reactive({
    
      if(input$annee != "Tout")
      {
        Achats <- Achats %>% 
          filter(year(Date.Achat) == input$annee)
      }
      else{
        Achats
      }
  })
      
  
    output$distPlot <- renderPlot({
        ggplot(Achats_Filtre(),aes(x = NOM_SITE, y = Mnt.Achat))+
        geom_bar(stat = "identity")+
        labs(
          title = paste0("Montant d'achat par site en ",input$annee), 
          x = "Nom du site", 
          y = "Montant d'achat total" )
    })

}
