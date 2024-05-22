server <- function(input, output, session) {
  
  nom_tab <- reactive(input$tabs)
  
  final_nom_tab <- reactive({
    str_sub(nom_tab(),
            as.numeric(gregexpr("_", nom_tab())) + 1,
            nchar(nom_tab()))
  })
  
  inici_nom_tab <- reactive({
    str_sub(nom_tab(), 1, 3)
  })
  
  #Escollim quin df utilitza el selector de columnes
  df_referencia <- reactive({

    lloguer_estudi <- switch(input$nivell_territorial,
                             "Ciutat" = lloguer_ciutat,
                             "Districte" = lloguer_districtes,
                             "Barri" = lloguer_barris)

    nivell_estudi <- switch(input$nivell_territorial,
                            "Ciutat" = pob_uni_ciutat,
                            "Districte" = pob_uni_districte,
                            "Barri" = pob_uni_barri)
    
    tipus_domicili <- switch(input$nivell_territorial,
                             "Ciutat" = tipus_domicili_ciutat,
                             "Districte" = tipus_domicili_districte,
                             "Barri" = tipus_domicili_barri)
    
    poblacio_origen <- switch(input$nivell_territorial,
                              "Ciutat" = regio_naixement_ciutat,
                              "Districte" = regio_naixement_districte,
                              "Barri" = regio_naixement_barri)
    
    renda_bruta <- switch(input$nivell_territorial,
                          "Ciutat" = sous_ciutat,
                          "Districte" = sous_districte,
                          "Barri" = sous_barri)
    
    hab_turistics <- switch(input$nivell_territorial,
                            "Ciutat" = hab_tur_ciutat,
                            "Districte" = hab_tur_districte,
                            "Barri" = hab_tur_barri)
    
    grup_edat <- switch(input$nivell_territorial,
                        "Ciutat" = pob_edat_ciutat,
                        "Districte" = pob_edat_districte,
                        "Barri" = pob_edat_barri)
    
    df_referencia <- switch(final_nom_tab(),
                            "dashboard" = NULL,
                            "lloguer" = lloguer_estudi,
                            "estudis" = nivell_estudi,
                            "domicilis" = tipus_domicili,
                            "poborigen" = poblacio_origen,
                            "renda" = renda_bruta,
                            "turistics" = hab_turistics,
                            "grupedat" = grup_edat)
    
    df_filter_anys <- df_referencia[complete.cases(df_referencia),] %>%
      pull(Data_Referencia) %>%
      unique() %>% sort()
    
    df_filter_anys <- df_filter_anys %>% as.numeric()
    
    if (inici_nom_tab() != "var"){
      updateSliderInput(session, "any_selector", 
                        min = min(df_filter_anys, na.rm = T), 
                        max = max(df_filter_anys, na.rm = T),
                        step = 1)
    } else {
      updateSliderInput(session, "any_selector", 
                        min = 2015 + input$anys_variacions, 
                        max = 2023,
                        step = 1)
    }
    
    return(df_referencia)
  })

  #Escollim el shp/sf que es farà servir
  choice_shp <- reactive({
    switch(input$nivell_territorial,
           "Ciutat" = shp_ciutat,
           "Districte" = shp_districtes,
           "Barri" = shp_barris)
  })
  choice_info <- reactive({
    switch(input$nivell_territorial,
           "Ciutat" = info_ciutat,
           "Districte" = info_districtes,
           "Barri" = info_barris)
  })
  
  #Obtenim el nom del camp que mirem segons barri/districte
  filtre_territorial <- reactive({
    switch(input$nivell_territorial,
           "Ciutat" = "Codi_Ciutat",
           "Districte" = "Codi_Districte",
           "Barri" = "Codi_Barri")
  })
  
  #Obtenim el codi del barri/districte
  codi_territori <- reactive({
    choice_shp() %>%
      filter(NOM %in% input$seleccio_territorial) %>%
      pull(!!sym(filtre_territorial()))
  })
  
  #Segons la selecció del input$nivell territorial, actualitzem input$seleccio_territorial
  observeEvent(input$nivell_territorial, {
    choices <- choice_shp()$NOM
      
    if(input$nivell_territorial == "Barri"){
      choices <- llista_dist_barr
      updatePickerInput(session, "seleccio_territorial", choices = choices)
    }
    
    updatePickerInput(session, "seleccio_territorial", choices = choices)
  })
  
  plot_dades <- reactive({ 

    df_estudi <- df_referencia() %>%
      mutate(width_value = 1.5)
    
    if (input$nivell_territorial != "Ciutat"){
      # Si no és ciutat, mirem quin codi volem filtrar
      # if (input$seleccio_territorial != "Tots"){
      if (!is.null(input$seleccio_territorial)){
        # Apliquem un filtre a la columna indicada amb el codi corresponent
        df_filter <- df_estudi %>%
          mutate(width_value = ifelse(!!sym(filtre_territorial()) %in% codi_territori(),
                                      5, 1.5))
      } else {
        # Si hem seleccionat "Tots" a barri/districte, tampoc filtrem
        df_filter <- df_estudi
      }
    } else {
      df_filter <- df_estudi
    }
    
    if ("Desc_valor_CA" %in% colnames(df_filter)){
      df_filter <- df_filter %>%
        mutate(Desc_Valor_CA = as.factor(Desc_Valor_CA))
    }
    
    variable_categoria <- switch(final_nom_tab(),
                                 "estudis" = "NIV_EDUCA_esta",
                                 "domicilis" = "TIPUS_DOMICILI",
                                 "poborigen" = "LLOC_NAIX_REGIO",
                                 "grupedat" = "EDAT_10",
                                 FALSE)
    
    input_value <- switch(variable_categoria,
                          "NIV_EDUCA_esta" = input$nivell_estudi_selector,
                          "TIPUS_DOMICILI" = input$tipus_domicili_selector,
                          "LLOC_NAIX_REGIO" = input$origen_poblacio,
                          "EDAT_10" = input$grup_edat,
                          FALSE)
    
    if(!isFALSE(variable_categoria)){
      if(length(input_value) > 0){
        df_filter <- df_filter %>%
          filter(Desc_Valor_CA %in% input_value)
      }
    }

    df_filter
  })
  
  #### PLOTS ####

  # Lloguer
  output$plot_lloguer <- renderHighchart({
    
    hchart(plot_dades() %>%
             filter(Data_Referencia == input$any_selector) %>%
             mutate(Preu_Lloguer = floor(Preu_Lloguer)) %>%
             arrange(desc(Preu_Lloguer)),
           type = "bar",
           hcaes(x = NOM, 
                 y = Preu_Lloguer,
                 color = Preu_Lloguer,
                 borderWidth = (width_value-1.5)*2),
           borderColor = "red",
           name = "Preu Lloguer") %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = "€/m2")) %>%
      exportar_highchart()
  })
  
  output$hchart_plot_lloguer <- renderUI({
    
    plot_dades <- plot_dades() %>%
      filter(Data_Referencia == input$any_selector) %>%
      mutate(Preu_Lloguer = floor(Preu_Lloguer))
    
    plot_height = max(1000,
                      70*nrow(unique(plot_dades[,filtre_territorial()])))
    
    highchartOutput("plot_lloguer", height = plot_height)
    
  })
  
  output$evol_lloguer <- renderHighchart({
    
    plot_dades <- plot_dades() %>%
      mutate(Preu_Lloguer = floor(Preu_Lloguer))
    
    # if (input$seleccio_territorial != "Tots"){
    if (!is.null(input$seleccio_territorial)){
      plot_dades <- plot_dades %>%
        filter(NOM %in% input$seleccio_territorial)
    }
    
    if (input$nivell_territorial == "Barri"){
      plot_dades <- plot_dades %>%
        filter(Data_Referencia >= 2013)
    }
    
    paleta_espectral <- colorFactor(
      "viridis", 
      domain = c(1:nrow(unique(plot_dades[,"NOM"]))),
      na.color = "transparent")
    
    hchart(plot_dades,
           type = "line",
           hcaes(x = Data_Referencia, 
                 y = Preu_Lloguer,
                 group = NOM)
           ) %>%
      hc_colors(paleta_espectral(1:nrow(unique(plot_dades[,"NOM"])))) %>%
      hc_xAxis(title = list(text = "Any")) %>%
      hc_yAxis(title = list(text = "€/m2")) %>%
      exportar_highchart()
  })
  
  # Estudis
  output$plot_estudis <- renderHighchart({
    
    variable_grup <- "Desc_Valor_CA"
    variable_filtre <- "NIV_EDUCA_esta"
    if (input$percentatge == "%"){
      variable_repres <- "Perc_Poblacio"
      nom_eix_y <- "% Població"
    } else {
      variable_repres <- "Poblacio"
      nom_eix_y <- "Població"
    }
    
    plot_dades <- plot_dades() %>%
      mutate(Desc_Valor_CA = as.factor(Desc_Valor_CA),
             Perc_Poblacio = round(Perc_Poblacio*100, 2)) %>%
      filter(Data_Referencia == input$any_selector)
    
    # if (input$nivell_estudi_selector != "Tots"){
    if (!is.null(input$nivell_estudi_selector)){
      plot_dades <- plot_dades %>%
        filter(Desc_Valor_CA %in% input$nivell_estudi_selector)
    }
    
    # if (input$seleccio_territorial != "Tots"){
    if (!is.null(input$seleccio_territorial)){
      plot_dades <- plot_dades %>%
        filter(NOM %in% input$seleccio_territorial)
    }

    paleta_espectral <- colorFactor(
      "viridis", 
      domain = c(1:nrow(unique(plot_dades[,variable_grup]))),
      na.color = "transparent")
    
    hchart(plot_dades,
           type = "bar",
           hcaes(x = NOM,  
                 y = !!sym(variable_repres), 
                 group = fct_reorder(as.factor(Desc_Valor_CA),
                                     !!sym(variable_filtre))),
           borderColor = "red") %>%
      hc_colors(paleta_espectral(1:nrow(unique(plot_dades[,variable_grup])))) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = nom_eix_y)) %>%
      exportar_highchart()
    
  })
  
  output$hchart_plot_estudis <- renderUI({

    plot_dades <- plot_dades() %>%
      mutate(Desc_Valor_CA = as.factor(Desc_Valor_CA)) %>%
      filter(Data_Referencia == input$any_selector)
    
    # if (input$nivell_estudi_selector != "Tots"){
    if (!is.null(input$nivell_estudi_selector)){
      plot_dades <- plot_dades %>%
        filter(Desc_Valor_CA %in% input$nivell_estudi_selector)
    }
    
    # if (input$seleccio_territorial != "Tots"){
    if (!is.null(input$seleccio_territorial)){
      plot_dades <- plot_dades %>%
        filter(NOM %in% input$seleccio_territorial)
    }
    
    plot_height = max(1000,
                      70*nrow(unique(plot_dades[,filtre_territorial()])))
    
    highchartOutput("plot_estudis", height = plot_height)
    
  })
  
  output$evol_estudis <- renderHighchart({
    
    variable_filtre <- "NIV_EDUCA_esta"
    variable_grup <- "Desc_Valor_CA"
    
    if (input$percentatge == "%"){
      variable_repres <- "Perc_Poblacio"
      nom_eix_y <- "% Població"
    } else {
      variable_repres <- "Poblacio"
      nom_eix_y <- "Població"
    }
    
    plot_dades <- plot_dades() %>%
      mutate(Desc_Valor_CA = as.factor(Desc_Valor_CA),
             Perc_Poblacio = round(Perc_Poblacio*100, 2))
    
    
    # if (input$nivell_estudi_selector != "Tots"){
    if (!is.null(input$nivell_estudi_selector)){
      plot_dades <- plot_dades %>%
        filter(Desc_Valor_CA %in% input$nivell_estudi_selector)
    }
    
    # if (input$seleccio_territorial != "Tots"){
    if (length(input$seleccio_territorial)==1){
      plot_dades <- plot_dades %>%
        filter(NOM %in% input$seleccio_territorial)
    } else {
      
      # if (input$nivell_estudi_selector != "Tots"){
      if (!is.null(input$seleccio_territorial)){
        plot_dades <- plot_dades %>%
          filter(NOM %in% input$seleccio_territorial)
      } 
      if(length(input$nivell_estudi_selector)==1){
        variable_grup <- "NOM"
      } else {
        plot_dades <- plot_dades %>%
          group_by(Data_Referencia, NIV_EDUCA_esta, Desc_Valor_CA) %>%
          summarise(Poblacio = sum(Poblacio, na.rm = T),
                    REF_Poblacio = sum(REF_Poblacio, na.rm = T),
                    Perc_Poblacio = round(100*Poblacio/REF_Poblacio, 2),
                    NOM = "Barcelona",
                    Codi_Ciutat = "01")

      }
    }
    
    paleta_espectral <- colorFactor(
      "viridis", 
      domain = c(1:nrow(unique(plot_dades[,variable_grup]))),
      na.color = "transparent")
    
    hchart(plot_dades,
           type = "line",
           hcaes(x = Data_Referencia, 
                 y = !!sym(variable_repres),
                 group = fct_reorder(as.factor(!!sym(variable_grup)),
                                     !!sym(variable_filtre)))
    ) %>%
      hc_colors(paleta_espectral(1:nrow(unique(plot_dades[,variable_grup])))) %>%
      hc_xAxis(title = list(text = "Any")) %>%
      hc_yAxis(title = list(text = nom_eix_y)) %>%
      exportar_highchart()
  })
  
  # Domicilis
  output$plot_domicilis <- renderHighchart({
    
    variable_grup <- "Desc_Valor_CA"
    variable_filtre <- "TIPUS_DOMICILI"
    if (input$percentatge == "%"){
      variable_repres <- "Perc_Domicilis"
      nom_eix_y <- "% Domicilis"
    } else {
      variable_repres <- "Domicilis"
      nom_eix_y <- "Domicilis"
    }
    
    plot_dades <- plot_dades() %>%
      mutate(Desc_Valor_CA = as.factor(Desc_Valor_CA),
             Perc_Domicilis = round(Perc_Domicilis*100, 2)) %>%
      filter(Data_Referencia == input$any_selector)
    
    # if (input$tipus_domicili_selector != "Tots"){
    if (!is.null(input$tipus_domicili_selector)){
      plot_dades <- plot_dades %>%
        filter(Desc_Valor_CA %in% input$tipus_domicili_selector)
    }
    
    # if (input$seleccio_territorial != "Tots"){
    if (!is.null(input$seleccio_territorial)){
      plot_dades <- plot_dades %>%
        filter(NOM %in% input$seleccio_territorial)
    }
    
    paleta_espectral <- colorFactor(
      "viridis", 
      domain = c(1:nrow(unique(plot_dades[,variable_grup]))),
      na.color = "transparent")
    
    hchart(plot_dades,
           type = "bar",
           hcaes(x = NOM,
                 y = !!sym(variable_repres), 
                 group = fct_reorder(as.factor(Desc_Valor_CA),
                                     !!sym(variable_filtre))),
           borderColor = "red") %>%
      hc_colors(paleta_espectral(1:nrow(unique(plot_dades[,variable_grup])))) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = nom_eix_y)) %>%
      exportar_highchart()
    
  })
  
  output$hchart_plot_domicilis <- renderUI({
    
    variable_filtre <- "TIPUS_DOMICILI"
    
    plot_dades <- plot_dades() %>%
      mutate(Desc_Valor_CA = as.factor(Desc_Valor_CA)) %>%
      filter(Data_Referencia == input$any_selector)
    
    # if (input$tipus_domicili_selector != "Tots"){
    if (!is.null(input$tipus_domicili_selector)){
      plot_dades <- plot_dades %>%
        filter(Desc_Valor_CA %in% input$tipus_domicili_selector)
    }
    
    # if (input$seleccio_territorial != "Tots"){
    if (!is.null(input$seleccio_territorial)){
      plot_dades <- plot_dades %>%
        filter(NOM %in% input$seleccio_territorial)
    }
    
    plot_height = max(1000,
                      70*nrow(unique(plot_dades[,filtre_territorial()])))
    
    highchartOutput("plot_domicilis", height = plot_height)
    
  })
  
  output$evol_domicilis <- renderHighchart({
    
    variable_filtre <- "TIPUS_DOMICILI"
    variable_grup <- "Desc_Valor_CA"
    
    if (input$percentatge == "%"){
      variable_repres <- "Perc_Domicilis"
      nom_eix_y <- "% Domicilis"
    } else {
      variable_repres <- "Domicilis"
      nom_eix_y <- "Domicilis"
    }
    
    plot_dades <- plot_dades() %>%
      mutate(Desc_Valor_CA = as.factor(Desc_Valor_CA),
             Perc_Domicilis = round(Perc_Domicilis*100, 2))
    
    # if (input$tipus_domicili_selector != "Tots"){
    if (!is.null(input$tipus_domicili_selector)){
      plot_dades <- plot_dades %>%
        filter(Desc_Valor_CA %in% input$tipus_domicili_selector)
    }
    
    # if (input$seleccio_territorial != "Tots"){
    if (length(input$seleccio_territorial)==1){
      plot_dades <- plot_dades %>%
        filter(NOM %in% input$seleccio_territorial)
    } else {
      # if (input$tipus_domicili_selector != "Tots"){
      if (!is.null(input$seleccio_territorial)){
        plot_dades <- plot_dades %>%
          filter(NOM %in% input$seleccio_territorial)
      }
      if(length(input$nivell_estudi_selector)==1){
        variable_grup <- "NOM"
      } else {
        plot_dades <- plot_dades %>%
          group_by(Data_Referencia, TIPUS_DOMICILI, Desc_Valor_CA) %>%
          summarise(Domicilis = sum(Domicilis, na.rm = T),
                    REF_Domicilis = sum(REF_Domicilis, na.rm = T),
                    Perc_Domicilis = round(100*Domicilis/REF_Domicilis, 2),
                    NOM = "Barcelona",
                    Codi_Ciutat = "01")
      }
    }
    
    paleta_espectral <- colorFactor(
      "viridis", 
      domain = c(1:nrow(unique(plot_dades[,variable_grup]))),
      na.color = "transparent")
    
    hchart(plot_dades,
           type = "line",
           hcaes(x = Data_Referencia, 
                 y = !!sym(variable_repres),
                 group = fct_reorder(as.factor(!!sym(variable_grup)),
                                     !!sym(variable_filtre)))
    ) %>%
      hc_colors(paleta_espectral(1:nrow(unique(plot_dades[,variable_grup])))) %>%
      hc_xAxis(title = list(text = "Any")) %>%
      hc_yAxis(title = list(text = nom_eix_y)) %>%
      exportar_highchart()
  })
  
  # Naixement
  output$plot_origen <- renderHighchart({
    
    variable_grup <- "Desc_Valor_CA"
    variable_filtre <- "LLOC_NAIX_REGIO"
    if (input$percentatge == "%"){
      variable_repres <- "Perc_Poblacio"
      nom_eix_y <- "% Població"
    } else {
      variable_repres <- "Poblacio"
      nom_eix_y <- "Població"
    }
    
    plot_dades <- plot_dades() %>%
      mutate(Desc_Valor_CA = as.factor(Desc_Valor_CA),
             Perc_Poblacio = round(Perc_Poblacio*100, 2)) %>%
      filter(Data_Referencia == input$any_selector)
    
    # if (input$origen_poblacio != "Tots"){
    if (!is.null(input$origen_poblacio)){
      plot_dades <- plot_dades %>%
        filter(Desc_Valor_CA %in% input$origen_poblacio)
    }
    
    # if (input$seleccio_territorial != "Tots"){
    if (!is.null(input$seleccio_territorial)){
      plot_dades <- plot_dades %>%
        filter(NOM %in% input$seleccio_territorial)
    }
    
    paleta_espectral <- colorFactor(
       "viridis", 
       domain = c(1:nrow(unique(plot_dades[,variable_grup]))),
       na.color = "transparent")
    
    hchart(plot_dades,
           type = "bar",
           hcaes(x = NOM,  
                 y = !!sym(variable_repres), 
                 group = fct_reorder(as.factor(Desc_Valor_CA), 
                                     !!sym(variable_filtre))),
           borderColor = "red") %>%
      hc_colors(paleta_espectral(1:nrow(unique(plot_dades[,variable_grup])))) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = nom_eix_y)) %>%
      exportar_highchart()
    })
  
  output$hchart_plot_origen <- renderUI({
    
    plot_dades <- plot_dades() %>%
      mutate(Desc_Valor_CA = as.factor(Desc_Valor_CA)) %>%
      filter(Data_Referencia == input$any_selector)
    
    # if (input$origen_poblacio != "Tots"){
    if (!is.null(input$origen_poblacio)){
      plot_dades <- plot_dades %>%
        filter(Desc_Valor_CA %in% input$origen_poblacio)
    }
    
    # if (input$seleccio_territorial != "Tots"){
    if (!is.null(input$seleccio_territorial)){
      plot_dades <- plot_dades %>%
        filter(NOM %in% input$seleccio_territorial)
    }
    
    plot_height = max(1000,
                      70*nrow(unique(plot_dades[,filtre_territorial()])))
    
    highchartOutput("plot_origen", height = plot_height)
    
  })
  
  output$evol_origen <- renderHighchart({
    
    variable_filtre <- "LLOC_NAIX_REGIO"
    variable_grup <- "Desc_Valor_CA"
    
    if (input$percentatge == "%"){
      variable_repres <- "Perc_Poblacio"
      nom_eix_y <- "% Població"
    } else {
      variable_repres <- "Poblacio"
      nom_eix_y <- "Població"
    }
    
    plot_dades <- plot_dades() %>%
      mutate(Desc_Valor_CA = as.factor(Desc_Valor_CA),
             Perc_Poblacio = round(Perc_Poblacio*100, 2))
    
    # if (input$origen_poblacio != "Tots"){
    if (!is.null(input$origen_poblacio)){
      plot_dades <- plot_dades %>%
        filter(Desc_Valor_CA %in% input$origen_poblacio)
    }
    
    # if (input$seleccio_territorial != "Tots"){
    if (length(input$seleccio_territorial)==1){
      plot_dades <- plot_dades %>%
        filter(NOM %in% input$seleccio_territorial)
    } else {
      # if (input$tipus_domicili_selector != "Tots"){
      if (!is.null(input$seleccio_territorial)){
        plot_dades <- plot_dades %>%
          filter(NOM %in% input$seleccio_territorial)
      }
      if(length(input$origen_poblacio)==1){
        variable_grup <- "NOM"
      } else {
        plot_dades <- plot_dades %>%
          group_by(Data_Referencia, LLOC_NAIX_REGIO, Desc_Valor_CA) %>%
          summarise(Poblacio = sum(Poblacio, na.rm = T),
                    REF_Poblacio = sum(REF_Poblacio, na.rm = T),
                    Perc_Poblacio = round(100*Poblacio/REF_Poblacio, 2),
                    NOM = "Barcelona",
                    Codi_Ciutat = "01")
      }
    }
    
    
    paleta_espectral <- colorFactor(
      "viridis", 
      domain = c(1:nrow(unique(plot_dades[,variable_grup]))),
      na.color = "transparent")
    
    hchart(plot_dades,
           type = "line",
           hcaes(x = Data_Referencia, 
                 y = !!sym(variable_repres),
                 group = fct_reorder(as.factor(!!sym(variable_grup)),
                                     !!sym(variable_filtre)))
    ) %>%
      hc_colors(paleta_espectral(1:nrow(unique(plot_dades[,variable_grup])))) %>%
      hc_xAxis(title = list(text = "Any")) %>%
      hc_yAxis(title = list(text = nom_eix_y)) %>%
      exportar_highchart()
  })
  
  # Renda
  output$plot_renda <- renderHighchart({
    
    hchart(plot_dades() %>%
             filter(Data_Referencia == input$any_selector) %>%
             mutate(Import_Renda_Bruta = floor(Import_Renda_Bruta)) %>%
             arrange(desc(Import_Renda_Bruta)),
           type = "bar",
           hcaes(x = NOM, 
                 y = Import_Renda_Bruta, 
                 color = Import_Renda_Bruta,
                 borderWidth = (width_value-1.5)*2),
           borderColor = "red",
           name = "Renda Bruta") %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = "€/pp")) %>%
      exportar_highchart()
    
  })
  
  output$hchart_plot_renda <- renderUI({
    
    plot_dades <- plot_dades() %>%
      filter(Data_Referencia == input$any_selector) %>%
      mutate(Import_Renda_Bruta = floor(Import_Renda_Bruta))
    
    plot_height = max(1000,
                      70*nrow(unique(plot_dades[,filtre_territorial()])))
    
    highchartOutput("plot_renda", height = plot_height)
    
  })
  
  output$evol_renda <- renderHighchart({
    
    plot_dades <- plot_dades() %>%
      mutate(Import_Renda_Bruta = floor(Import_Renda_Bruta))
    
    # if (input$seleccio_territorial != "Tots"){
    if (!is.null(input$seleccio_territorial)){
      plot_dades <- plot_dades %>%
        filter(NOM %in% input$seleccio_territorial)
    }
    
    paleta_espectral <- colorFactor(
      "viridis", 
      domain = c(1:nrow(unique(plot_dades[,"NOM"]))),
      na.color = "transparent")
    
    hchart(plot_dades,
           type = "line",
           hcaes(x = Data_Referencia, 
                 y = Import_Renda_Bruta,
                 group = NOM)
    ) %>%
      hc_colors(paleta_espectral(1:nrow(unique(plot_dades[,"NOM"])))) %>%
      hc_xAxis(title = list(text = "Any")) %>%
      hc_yAxis(title = list(text = "€/pp")) %>%
      exportar_highchart()
  })
  
  # Turisme
  output$plot_turisme <- renderHighchart({
    
    hchart(plot_dades() %>%
             filter(Data_Referencia == input$any_selector) %>%
             arrange(desc(Pisos)),
           type = "column",
           hcaes(x = NOM,
                 y = Pisos, 
                 color = Pisos,
                 borderWidth = (width_value-1.5)*2),
           borderColor = "red",
           name = "Habitatges turístics") %>%
      hc_xAxis(title = list(text = "")) %>%
      exportar_highchart()
    
  })
  
  # Edat
  output$plot_edat <- renderHighchart({
    
    variable_grup <- "Desc_Valor_CA"
    variable_filtre <- "EDAT_10"
    if (input$percentatge == "%"){
      variable_repres <- "Perc_Poblacio"
      nom_eix_y <- "% Població"
    } else {
      variable_repres <- "Poblacio"
      nom_eix_y <- "Població"
    }
    
    plot_dades <- plot_dades() %>%
      mutate(Desc_Valor_CA = as.factor(Desc_Valor_CA),
             Perc_Poblacio = round(Perc_Poblacio*100, 2)) %>%
      filter(Data_Referencia == input$any_selector)
    
    # if (input$grup_edat != "Tots"){
    if (!is.null(input$grup_edat)){
      plot_dades <- plot_dades %>%
        filter(Desc_Valor_CA %in% input$grup_edat)
    }
    
    # if (input$seleccio_territorial != "Tots"){
    if (!is.null(input$seleccio_territorial)){
      plot_dades <- plot_dades %>%
        filter(NOM %in% input$seleccio_territorial)
    }
    
    paleta_espectral <- colorFactor(
      "viridis", 
      domain = c(1:nrow(unique(plot_dades[,variable_grup]))),
      na.color = "transparent")
    
    hchart(plot_dades,
           type = "bar",
           hcaes(x = NOM,  
                 y = !!sym(variable_repres), 
                 group = fct_reorder(as.factor(Desc_Valor_CA), 
                                     !!sym(variable_filtre))),
           borderColor = "red") %>%
      hc_colors(paleta_espectral(1:nrow(unique(plot_dades[,variable_grup])))) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = nom_eix_y)) %>%
      exportar_highchart()
    
  })
  
  output$hchart_plot_edat <- renderUI({
    
    plot_dades <- plot_dades() %>%
      mutate(Desc_Valor_CA = as.factor(Desc_Valor_CA)) %>%
      filter(Data_Referencia == input$any_selector)
    
    # if (input$grup_edat != "Tots"){
    if (!is.null(input$grup_edat)){
      plot_dades <- plot_dades %>%
        filter(Desc_Valor_CA %in% input$grup_edat)
    }
    
    # if (input$seleccio_territorial != "Tots"){
    if (!is.null(input$seleccio_territorial)){
      plot_dades <- plot_dades %>%
        filter(NOM %in% input$seleccio_territorial)
    }
    
    plot_height = max(1000,
                      70*nrow(unique(plot_dades[,filtre_territorial()])))
    
    highchartOutput("plot_edat", height = plot_height)
    
    })
  
  output$evol_edat <- renderHighchart({
    
    variable_filtre <- "EDAT_10"
    variable_grup <- "Desc_Valor_CA"
    
    if (input$percentatge == "%"){
      variable_repres <- "Perc_Poblacio"
      nom_eix_y <- "% Població"
    } else {
      variable_repres <- "Poblacio"
      nom_eix_y <- "Població"
    }
    
    plot_dades <- plot_dades() %>%
      mutate(Desc_Valor_CA = as.factor(Desc_Valor_CA),
             Perc_Poblacio = round(Perc_Poblacio*100, 2))
    
    # if (input$grup_edat != "Tots"){
    if (!is.null(input$grup_edat)){
      plot_dades <- plot_dades %>%
        filter(Desc_Valor_CA %in% input$grup_edat)
    }
    
    # if (input$seleccio_territorial != "Tots"){
    if (length(input$seleccio_territorial)==1){
      plot_dades <- plot_dades %>%
        filter(NOM %in% input$seleccio_territorial)
    } else {
      # if (input$tipus_domicili_selector != "Tots"){
      if (!is.null(input$seleccio_territorial)){
        plot_dades <- plot_dades %>%
          filter(NOM %in% input$seleccio_territorial)
      }
      if(length(input$grup_edat)==1){
        variable_grup <- "NOM"
      } else {
        plot_dades <- plot_dades %>%
          group_by(Data_Referencia, EDAT_10, Desc_Valor_CA) %>%
          summarise(Poblacio = sum(Poblacio, na.rm = T),
                    REF_Poblacio = sum(REF_Poblacio, na.rm = T),
                    Perc_Poblacio = round(100*Poblacio/REF_Poblacio, 2),
                    NOM = "Barcelona",
                    Codi_Ciutat = "01")
      }
    }
    
    paleta_espectral <- colorFactor(
      "viridis", 
      domain = c(1:nrow(unique(plot_dades[,variable_grup]))),
      na.color = "transparent")
    
    hchart(plot_dades,
           type = "line",
           hcaes(x = Data_Referencia, 
                 y = !!sym(variable_repres),
                 group = fct_reorder(as.factor(!!sym(variable_grup)),
                                     !!sym(variable_filtre)))
    ) %>%
      hc_colors(paleta_espectral(1:nrow(unique(plot_dades[,variable_grup])))) %>%
      hc_xAxis(title = list(text = "Any")) %>%
      hc_yAxis(title = list(text = nom_eix_y)) %>%
      exportar_highchart()
  })
  
  #### DADES VARIACIÓ
  dades_variacio <- reactive({
    
    variable_abs <- switch(final_nom_tab(),
                           "lloguer" = "Preu_Lloguer",
                           "domicilis" = "Domicilis",
                           "renda" = "Import_Renda_Bruta",
                           "Poblacio")
    
    variable_per <- switch(final_nom_tab(),
                           "lloguer" = "Preu_Lloguer",
                           "domicilis" = "Perc_Domicilis",
                           "renda" = "Import_Renda_Bruta",
                           "Perc_Poblacio")
    
    variable_categoria <- switch(final_nom_tab(),
                                 "estudis" = "NIV_EDUCA_esta",
                                 "domicilis" = "TIPUS_DOMICILI",
                                 "poborigen" = "LLOC_NAIX_REGIO",
                                 "grupedat" = "EDAT_10",
                                 FALSE)
    
    var_ref <- switch(variable_categoria,
                      "TIPUS_DOMICILI" = "REF_Domicilis",
                      "REF_Poblacio")
    
    input_value <- switch(variable_categoria,
                          "NIV_EDUCA_esta" = input$nivell_estudi_selector,
                          "TIPUS_DOMICILI" = input$tipus_domicili_selector,
                          "LLOC_NAIX_REGIO" = input$origen_poblacio,
                          "EDAT_10" = input$grup_edat,
                          FALSE)
    
    variable_grup <- "Desc_Valor_CA"
    
    filtre_territorial <- filtre_territorial()
    
    plot_dades <- plot_dades() %>%
      filter(Data_Referencia >= 2015)
    
    if ("Perc_Poblacio" %in% colnames(plot_dades)){
      plot_dades <- plot_dades %>%
        mutate(Perc_Poblacio = round(Perc_Poblacio*100, 2))
    }
    if ("Perc_Domicilis" %in% colnames(plot_dades)){
      plot_dades <- plot_dades %>%
        mutate(Perc_Domicilis = round(Perc_Domicilis*100, 2))
    }
    
    # # if (input$seleccio_territorial != "Tots"){
    # if (!is.null(input$seleccio_territorial)){
    #   # plot_dades <- plot_dades %>%
    #   #   filter(NOM == input$seleccio_territorial)
    # } else {
    #   if (!isFALSE(variable_categoria)){
    #     # if (input_value != "Tots"){
    #     if (!is.null(input_value)){
    #       plot_dades <- plot_dades %>%
    #         filter(!!sym(variable_grup) %in% input_value)
    #     } else {
    #       plot_dades_var <- plot_dades %>%
    #         group_by(Data_Referencia, !!sym(variable_categoria)) %>%
    #         mutate(!!variable_abs := sum(!!sym(variable_abs), na.rm = T),
    #                   !!var_ref := sum(!!sym(var_ref), na.rm = T),
    #                   !!variable_per := round(100*!!sym(variable_abs)/!!sym(var_ref), 2),
    #                   Codi_Ciutat = "01") %>%
    #         ungroup()
    #     }
    #   }
    # }
    
    if (length(input$seleccio_territorial)==1){
      # plot_dades <- plot_dades %>%
      #   filter(NOM %in% input$seleccio_territorial)
    } else {
      if (!is.null(input$seleccio_territorial)){
        # plot_dades <- plot_dades %>%
        #   filter(NOM %in% input$seleccio_territorial)
      }
      if (!isFALSE(variable_categoria)){
        # if (input_value != "Tots"){
        if (length(input_value)==1){
          variable_grup <- "NOM"
        } else {
          plot_dades_var <- plot_dades %>%
            mutate(Codi_Ciutat = "01")
            # group_by(Data_Referencia, !!sym(variable_categoria)) %>%
            # mutate(!!variable_abs := sum(!!sym(variable_abs), na.rm = T),
            #        !!var_ref := sum(!!sym(var_ref), na.rm = T),
            #        !!variable_per := round(100*!!sym(variable_abs)/!!sym(var_ref), 2),
            #        Codi_Ciutat = "01") %>%
            # ungroup()
        }
      }
    }
    
    # Filtrem les dades segons la variable_categoria i input_value
    # if (!isFALSE(variable_categoria)){
    #   # if (input_value != "Tots"){
    #   if (!is.null(input_value)){
    #     plot_dades <- plot_dades %>%
    #       filter(!!sym(variable_grup) %in% input_value)
    #   }
    # }
    
    anys_var <- as.numeric(input$anys_variacions)
    
    comparar_passat <- function(plot_dades, files_retrocedir, variable_abs, variable_per){
      plot_dades %>%
        mutate(Var = floor(!!sym(variable_abs) -
                             lag(!!sym(variable_abs), files_retrocedir)),
               Var_Perc = round(!!sym(variable_per) -
                                  lag(!!sym(variable_per), files_retrocedir),2),
               Perc_Var = 
                 round(100*Var_Perc /
                         lag(!!sym(variable_per), files_retrocedir), 2))
    }
    
    if (!isFALSE(variable_categoria)){
      
      files_retrocedir <- anys_var*
        nrow(unique(plot_dades[,filtre_territorial]))*
        nrow(unique(plot_dades[,variable_categoria]))
      
      plot_dades <- comparar_passat(plot_dades, files_retrocedir, variable_abs, variable_per)
      # if (input_value == "Tots" & input$seleccio_territorial == "Tots"){
      if (length(input_value)!=1 & length(input$seleccio_territorial)!=1){
        plot_dades_var <- comparar_passat(plot_dades_var, files_retrocedir, variable_abs, variable_per)
      } else {
        plot_dades_var <- NULL
      }
      
    } else {
    
      files_retrocedir <- anys_var*
        nrow(unique(plot_dades[,filtre_territorial]))
      
      plot_dades <- comparar_passat(plot_dades, files_retrocedir, variable_abs, variable_per)
      plot_dades_var <- NULL
      
    }
    
    list(
      original = plot_dades,
      plot_var = plot_dades_var
    )
    
  })
  
  output$plot_var_lloguer <- 
    output$plot_var_estudis <- 
    output$plot_var_domicilis <-
    output$plot_var_poborigen <-
    output$plot_var_renda <-
    output$plot_var_grupedat <- renderHighchart({
      
      variable_categoria <- switch(final_nom_tab(),
                                   "estudis" = "NIV_EDUCA_esta",
                                   "domicilis" = "TIPUS_DOMICILI",
                                   "poborigen" = "LLOC_NAIX_REGIO",
                                   "grupedat" = "EDAT_10",
                                   FALSE)
      
      input_value <- switch(variable_categoria,
                            "NIV_EDUCA_esta" = input$nivell_estudi_selector,
                            "TIPUS_DOMICILI" = input$tipus_domicili_selector,
                            "LLOC_NAIX_REGIO" = input$origen_poblacio,
                            "EDAT_10" = input$grup_edat,
                            FALSE)
      
      nom_eix_y <- switch(final_nom_tab(),
                          "lloguer" = "lloguer (€/m2)",
                          "renda" = "renda (€/pp)",
                          "domicilis" = "domicilis",
                          "població")
      
      variable_grup <- "Desc_Valor_CA"
      
      if (!isFALSE(variable_categoria)){
        if (length(input$seleccio_territorial)!=1){
          if(length(input_value)==1){
            variable_grup <- "NOM"    
          }
        }
      } else {
        variable_grup <- "NOM"
      }
      
      dades_variacio <- dades_variacio()$original
      
      # # if(input$seleccio_territorial != "Tots"){
      # if(!is.null(input$seleccio_territorial)){
      #   dades_variacio <- dades_variacio %>%
      #     filter(NOM %in% input$seleccio_territorial)
      # } else {
      #   if(!isFALSE(variable_categoria)){
      #     # if (input_value == "Tots"){
      #     if (is.null(input_value)){
      #       dades_variacio <- dades_variacio()$plot_var %>%
      #         group_by(Data_Referencia, Codi_Ciutat, !!sym(variable_categoria)) %>%
      #         slice_head(n = 1) %>%
      #         ungroup()
      #     }
      #   }
      # }
      
      if(length(input$seleccio_territorial)==1){
        dades_variacio <- dades_variacio %>%
          filter(NOM %in% input$seleccio_territorial)
      } else {
        if (!is.null(input$seleccio_territorial)){
          dades_variacio <- dades_variacio %>%
            filter(NOM %in% input$seleccio_territorial)
        }
        if(!isFALSE(variable_categoria)){
          # if (input_value == "Tots"){
          if (length(input_value)!=1){
            dades_variacio <- dades_variacio()$plot_var
            if(length(input$seleccio_territorial)>0){
              dades_variacio <- dades_variacio %>%
                filter(NOM %in% input$seleccio_territorial)
            }
            dades_variacio <- dades_variacio %>%
              group_by(Data_Referencia, !!sym(variable_categoria)) %>%
              slice_head(n = 1) %>%
              ungroup()
          }
        }
      }
      
      paleta_espectral <- colorFactor(
        "viridis",
        domain = c(1:nrow(unique(dades_variacio[,variable_grup]))),
        na.color = "transparent")
      
      if (isFALSE(variable_categoria)){
        hchart(dades_variacio,
               type = "line",
               hcaes(x = Data_Referencia, 
                     y = Var,
                     group = as.factor(!!sym(variable_grup)))
               )  %>%
          hc_colors(paleta_espectral(1:nrow(unique(dades_variacio[,variable_grup])))) %>%
          hc_xAxis(title = list(text = "Any"), min = 2015+input$anys_variacions, tickInterval = 1) %>%
          hc_yAxis(title = list(text = paste0("Variació ", nom_eix_y)),
                   plotLines = list(
                     list(value = 0,
                          color = "black", width = 3, 
                          dashStyle = "Dash",
                          zIndex = 5  # assegura que la línia estigui al davant
                     )
                   )) %>%
          exportar_highchart()
      } else {
        hchart(dades_variacio,
               type = "line",
               hcaes(x = Data_Referencia, 
                     y = Var,
                     group = fct_reorder(as.factor(!!sym(variable_grup)),
                                         !!sym(variable_categoria)))
               ) %>%
          hc_colors(paleta_espectral(1:nrow(unique(dades_variacio[,variable_grup])))) %>%
          hc_xAxis(title = list(text = "Any"), min = 2015+input$anys_variacions, tickInterval = 1) %>%
          hc_yAxis(title = list(text = paste0("Variació ", nom_eix_y)),
                   plotLines = list(
                     list(value = 0,
                          color = "black", width = 3, 
                          dashStyle = "Dash", 
                          zIndex = 5   # assegura que la línia estigui al davant
                     )
                   )) %>%
          exportar_highchart()
      }
  })
  
  output$plot_perc_lloguer <-
    output$plot_perc_estudis <- 
    output$plot_perc_domicilis <-
    output$plot_perc_poborigen <-
    output$plot_perc_renda <-
    output$plot_perc_grupedat <- renderHighchart({
      
      variable_categoria <- switch(final_nom_tab(),
                                   "estudis" = "NIV_EDUCA_esta",
                                   "domicilis" = "TIPUS_DOMICILI",
                                   "poborigen" = "LLOC_NAIX_REGIO",
                                   "grupedat" = "EDAT_10",
                                   FALSE)
      
      input_value <- switch(variable_categoria,
                            "NIV_EDUCA_esta" = input$nivell_estudi_selector,
                            "TIPUS_DOMICILI" = input$tipus_domicili_selector,
                            "LLOC_NAIX_REGIO" = input$origen_poblacio,
                            "EDAT_10" = input$grup_edat,
                            FALSE)
      
      nom_eix_y <- switch(final_nom_tab(),
                          "lloguer" = "lloguer",
                          "renda" = "renda",
                          "domicilis" = "domicilis",
                          "població")
      
      variable_grup <- "Desc_Valor_CA"
      
      if (!isFALSE(variable_categoria)){
        if (length(input$seleccio_territorial)!=1){
          if(length(input_value)==1){
            variable_grup <- "NOM"    
          }
        }
      } else {
        variable_grup <- "NOM"
      }
      
      dades_variacio <- dades_variacio()$original
      
      # if(input$seleccio_territorial != "Tots"){
      if(length(input$seleccio_territorial)==1){
        dades_variacio <- dades_variacio %>%
          filter(NOM %in% input$seleccio_territorial)
      } else {
        if (!is.null(input$seleccio_territorial)){
          dades_variacio <- dades_variacio %>%
            filter(NOM %in% input$seleccio_territorial)
        }
        if(!isFALSE(variable_categoria)){
          # if (input_value == "Tots"){
          if (length(input_value)!=1){
            dades_variacio <- dades_variacio()$plot_var
            if(length(input$seleccio_territorial)>0){
              dades_variacio <- dades_variacio %>%
                filter(NOM %in% input$seleccio_territorial)
            }
            dades_variacio <- dades_variacio %>%
              group_by(Data_Referencia, Codi_Ciutat, !!sym(variable_categoria)) %>%
              slice_head(n = 1) %>%
              ungroup()
          }
        }
      }
      
      paleta_espectral <- colorFactor(
        "viridis",
        domain = c(1:nrow(unique(dades_variacio[,variable_grup]))),
        na.color = "transparent")
      
      if (isFALSE(variable_categoria)){
        hchart(dades_variacio,
               type = "line",
               hcaes(x = Data_Referencia, 
                     y = Perc_Var,
                     group = as.factor(!!sym(variable_grup)))
        ) %>%
          hc_colors(paleta_espectral(1:nrow(unique(dades_variacio[,variable_grup])))) %>%
          hc_xAxis(title = list(text = "Any"), min = 2015+input$anys_variacions, tickInterval = 1) %>%
          hc_yAxis(title = list(text = paste0("% Variació ", nom_eix_y)),
                   plotLines = list(
                     list(value = 0,
                          color = "black", width = 3, 
                          dashStyle = "Dash", 
                          zIndex = 5   # assegura que la línia estigui al davant
                     )
                   )) %>%
          exportar_highchart()
      } else {
        hchart(dades_variacio,
               type = "line",
               hcaes(x = Data_Referencia, 
                     y = Var_Perc,
                     group = fct_reorder(as.factor(!!sym(variable_grup)),
                                         !!sym(variable_categoria)))
        ) %>%
          hc_colors(paleta_espectral(1:nrow(unique(dades_variacio[,variable_grup])))) %>%
          hc_xAxis(title = list(text = "Any"), min = 2015+input$anys_variacions, tickInterval = 1) %>%
          hc_yAxis(title = list(text = paste0("% Variació ", nom_eix_y)),
                   plotLines = list(
                     list(value = 0,
                          color = "black", width = 3, 
                          dashStyle = "Dash", 
                          zIndex = 5  # assegura que la línia estigui al davant 
                     )
                   )) %>%
          exportar_highchart()
      }
    })
  
  #### SF ####
  
  sf_dades <- reactive({
    
    sf_dades <-  choice_info() %>%
      left_join(plot_dades() %>% subset(select = -c(NOM)),
                by = filtre_territorial()) %>%
      st_as_sf()
    
    sf_dades
  })
  
  sf_dades_var <- reactive({
    
    sf_dades <-  choice_info() %>%
      left_join(dades_variacio()$origin %>% subset(select = -c(NOM)),
                by = filtre_territorial()) %>%
      st_as_sf()
    
    sf_dades
  })
  
  #### MAPES ####
  
  output$map_lloguer <- renderLeaflet({ #creem el mapa amb les dades
    
    sf_dades <- sf_dades() %>%
      filter(Data_Referencia == input$any_selector) %>%
      mutate(Preu_Lloguer = floor(Preu_Lloguer))
    
    domini_valors <- seq(min(sf_dades$Preu_Lloguer, na.rm = T),
                         max(sf_dades$Preu_Lloguer, na.rm = T),
                         length.out = 10)
    
    raster_dades <- raster(ext = extent(sf_dades))
    raster_dades <- rasterize(sf_dades, raster_dades, field = "Preu_Lloguer")
    
    paleta_espectral <- colorNumeric(
      "viridis", 
      domain = domini_valors,
      na.color = "transparent")
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = sf_dades, color = "black", opacity = 1, weight = ~width_value,
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 2,
                                                      bringToFront = TRUE),
                  label = ~paste(input$nivell_territorial,": ", NOM, "<br>",
                                 "Valor: ", Preu_Lloguer) %>% lapply(., HTML)) %>%
      addRasterImage(raster_dades, opacity = 0.7, colors = paleta_espectral, project = F) %>%
      addLegend(position = "topright",
                title = "Preu Lloguer (€/m2)",
                opacity = 1,
                pal = paleta_espectral,
                values = domini_valors)
      
  })
  
  output$map_estudis <- renderLeaflet({ #creem el mapa amb les dades
    
    sf_dades <- sf_dades() %>%
      filter(Data_Referencia == input$any_selector)
    
    raster_dades_o <- raster(ext = extent(sf_dades))
    
    variable_grup <- "Desc_Valor_CA"
    if (input$percentatge == "%"){
      variable_repres <- "Perc_Poblacio"
      nom_eix_y <- "% Població"
    } else {
      variable_repres <- "Poblacio"
      nom_eix_y <- "Població"
    }
    
    # if (input$nivell_estudi_selector %in% c("Tots")){
    if (length(input$nivell_estudi_selector)!=1){
      domini_valors <- c(1:length(unique(sf_dades %>% pull(variable_grup))))
      titol_llegenda <- "Nivell d'estudis majoritari"
      paleta_espectral <- colorFactor(
        "viridis", 
        domain = domini_valors,
        na.color = "transparent")
      
      sf_dades <- sf_dades %>%
        mutate(Desc_Valor_CA = as.factor(Desc_Valor_CA),
               Perc_Poblacio = round(Perc_Poblacio*100, 2)) %>%
        group_by(Data_Referencia, !!sym(filtre_territorial()), Desc_Valor_CA) %>%
        arrange(desc(Perc_Poblacio)) %>%
        ungroup(Desc_Valor_CA) %>%
        mutate(NIV_EDUCA_esta = as.integer(factor(NIV_EDUCA_esta))) %>% #Fem això per canviar-lis els valors
        slice_head(n = 1) %>%
        ungroup()

      raster_dades <- rasterize(sf_dades, raster_dades_o, field = "NIV_EDUCA_esta")

    } else {
      
      sf_dades <- sf_dades %>%
        mutate(Desc_Valor_CA = as.factor(Desc_Valor_CA),
               Perc_Poblacio = round(Perc_Poblacio*100, 2)) %>%
        filter(Desc_Valor_CA %in% input$nivell_estudi_selector)
      
      domini_valors <- seq(min(sf_dades %>% pull(!!sym(variable_repres)), na.rm = T),
                           max(sf_dades %>% pull(!!sym(variable_repres)), na.rm = T),
                           length.out = 10)
      
      if (input$percentatge == "%"){
        titol_llegenda <- "Percentatge respecte al total (%)"
      } else {
        titol_llegenda <- "Població"
      }
      
      paleta_espectral <- colorNumeric(
        "viridis",
        domain = domini_valors,
        na.color = "transparent")
      
      raster_dades <- rasterize(sf_dades, raster_dades_o, field = variable_repres)
    }
    mapa <- leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = sf_dades, color = "black", opacity = 1, weight = ~width_value,
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 2,
                                                      bringToFront = TRUE),
                  label = ~paste(Desc_Valor_CA, "<br>",
                                 input$nivell_territorial,": ", NOM, "<br>",
                                 input$percentatge,": ", get(variable_repres)) %>% lapply(., HTML)) %>%
      addRasterImage(raster_dades, opacity = 0.7, colors = paleta_espectral, project = F)
    
    # if (input$nivell_estudi_selector %in% c("Tots")){
    if (length(input$nivell_estudi_selector)!=1){
      
      labels_llegenda <- dimensions_pad %>%
        filter(Desc_Dimensio == "NIV_EDUCA_esta") %>%
        pull(Desc_Valor_CA) %>%
        intersect(unique(sf_dades() %>% pull(variable_grup)))
      
      labels_tallades <- substr(labels_llegenda, 1, 30)
      labels_tallades <- ifelse(nchar(labels_llegenda) > 30,
                                paste0(labels_tallades, "..."),
                                labels_tallades)
      
      # colors_paleta <- colorRampPalette(brewer.pal(length(domini_valors),"viridis"))
      colors_paleta <- viridis_pal()(length(domini_valors))
      
      mapa %>% addLegend(position = "topright",
                         title = titol_llegenda,
                         opacity = 1,
                         # colors = colors_paleta(length(domini_valors)),
                         colors = colors_paleta,
                         values = domini_valors,
                         labels = labels_tallades)
    } else {
      
      mapa %>% addLegend(position = "topright",
                         title = titol_llegenda,
                         opacity = 1,
                         pal = paleta_espectral,
                         values = domini_valors)
    }
    
  })
  
  output$map_domicilis <- renderLeaflet({ #creem el mapa amb les dades
    
    sf_dades <- sf_dades() %>%
      filter(Data_Referencia == input$any_selector)
    
    raster_dades_o <- raster(ext = extent(sf_dades))
    
    variable_grup <- "Desc_Valor_CA"
    if (input$percentatge == "%"){
      variable_repres <- "Perc_Domicilis"
      nom_eix_y <- "% Domicilis"
    } else {
      variable_repres <- "Domicilis"
      nom_eix_y <- "Domicilis"
    }
    
    # if (input$tipus_domicili_selector %in% c("Tots")){
    if (length(input$tipus_domicili_selector)!=1){
      domini_valors <- c(1:length(unique(sf_dades %>% pull(variable_grup))))
      titol_llegenda <- "Tipus de domicili majoritari"
      paleta_espectral <- colorFactor(
        "viridis", 
        domain = domini_valors,
        na.color = "transparent")
      
      sf_dades <- sf_dades %>%
        mutate(Desc_Valor_CA = as.factor(Desc_Valor_CA),
               Perc_Domicilis = round(Perc_Domicilis*100, 2)) %>%
        group_by(Data_Referencia, !!sym(filtre_territorial()), Desc_Valor_CA) %>%
        arrange(desc(Perc_Domicilis)) %>%
        ungroup(Desc_Valor_CA) %>%
        mutate(TIPUS_DOMICILI = as.integer(factor(TIPUS_DOMICILI))) %>% #Fem això per canviar-lis els valors
        slice_head(n = 1) %>%
        ungroup()
      
      raster_dades <- rasterize(sf_dades, raster_dades_o, field = "TIPUS_DOMICILI")
      
    } else {
      
      sf_dades <- sf_dades %>%
        mutate(Desc_Valor_CA = as.factor(Desc_Valor_CA),
               Perc_Domicilis = round(Perc_Domicilis*100, 2)) %>%
        filter(Desc_Valor_CA %in% input$tipus_domicili_selector)
      
      domini_valors <- seq(min(sf_dades %>% pull(!!sym(variable_repres)), na.rm = T),
                           max(sf_dades %>% pull(!!sym(variable_repres)), na.rm = T),
                           length.out = 10)
      
      if (input$percentatge == "%"){
        titol_llegenda <- "Percentatge respecte al total (%)"
      } else {
        titol_llegenda <- "Domicilis"
      }
      
      paleta_espectral <- colorNumeric(
        "viridis",
        domain = domini_valors,
        na.color = "transparent")
      
      raster_dades <- rasterize(sf_dades, raster_dades_o, field = variable_repres)
    }
    
    mapa <- leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = sf_dades, color = "black", opacity = 1, weight = ~width_value,
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 2,
                                                      bringToFront = TRUE),
                  label = ~paste(Desc_Valor_CA, "<br>",
                                 input$nivell_territorial,": ", NOM, "<br>",
                                 input$percentatge,": ", get(variable_repres)) %>% lapply(., HTML)) %>%
      addRasterImage(raster_dades, opacity = 0.7, colors = paleta_espectral, project = F)
    
    # if (input$tipus_domicili_selector %in% c("Tots")){
    if (length(input$tipus_domicili_selector)!=1){
      
      labels_llegenda <- dimensions_pad %>%
        filter(Desc_Dimensio == "TIPUS_DOMICILI") %>%
        pull(Desc_Valor_CA) %>%
        intersect(unique(sf_dades() %>% pull(variable_grup)))
      
      labels_tallades <- substr(labels_llegenda, 1, 30)
      labels_tallades <- ifelse(nchar(labels_llegenda) > 30,
                                paste0(labels_tallades, "..."),
                                labels_tallades)
      
      # colors_paleta <- colorRampPalette(brewer.pal(length(domini_valors),"viridis"))
      colors_paleta <- viridis_pal()(length(domini_valors))
      
      mapa %>% addLegend(position = "topright",
                         title = titol_llegenda,
                         opacity = 1,
                         # colors = colors_paleta(length(domini_valors)),
                         colors = colors_paleta,
                         values = domini_valors,
                         labels = labels_tallades)
    } else {
      
      mapa %>% addLegend(position = "topright",
                         title = titol_llegenda,
                         opacity = 1,
                         pal = paleta_espectral,
                         values = domini_valors)
    }
    
  })
  
  output$map_origen <- renderLeaflet({ #creem el mapa amb les dades
    
    sf_dades <- sf_dades() %>%
      filter(Data_Referencia == input$any_selector)
    
    raster_dades_o <- raster(ext = extent(sf_dades()))
    
    variable_grup <- "Desc_Valor_CA"
    if (input$percentatge == "%"){
      variable_repres <- "Perc_Poblacio"
      nom_eix_y <- "% Població"
    } else {
      variable_repres <- "Poblacio"
      nom_eix_y <- "Població"
    }
    
    # if (input$origen_poblacio %in% c("Tots")){
    if (length(input$origen_poblacio)!=1){
      
      domini_valors <- c(1:length(unique(sf_dades %>% pull(variable_grup))))
      titol_llegenda <- "Regió de naixement majoritari"
      paleta_espectral <- colorFactor(
        "viridis", 
        domain = domini_valors,
        na.color = "transparent")
      
      sf_dades <- sf_dades %>%
        mutate(Desc_Valor_CA = as.factor(Desc_Valor_CA),
               Perc_Poblacio = round(Perc_Poblacio*100, 2)) %>%
        group_by(Data_Referencia, !!sym(filtre_territorial()), Desc_Valor_CA) %>%
        arrange(desc(Perc_Poblacio)) %>%
        ungroup(Desc_Valor_CA) %>%
        mutate(LLOC_NAIX_REGIO = as.integer(factor(LLOC_NAIX_REGIO))) %>% #Fem això per canviar-lis els valors
        slice_head(n = 1) %>%
        ungroup()
      
      raster_dades <- rasterize(sf_dades, raster_dades_o, field = "LLOC_NAIX_REGIO")
      
    } else {
      
      sf_dades <- sf_dades %>%
        mutate(Desc_Valor_CA = as.factor(Desc_Valor_CA),
               Perc_Poblacio = round(Perc_Poblacio*100, 2)) %>%
        filter(Desc_Valor_CA %in% input$origen_poblacio)
      
      domini_valors <- seq(min(sf_dades %>% pull(!!sym(variable_repres)), na.rm = T),
                           max(sf_dades %>% pull(!!sym(variable_repres)), na.rm = T),
                           length.out = 10)
      
      if (input$percentatge == "%"){
        titol_llegenda <- "Percentatge respecte al total (%)"
      } else {
        titol_llegenda <- "Població"
      }
      
      paleta_espectral <- colorNumeric(
        "viridis",
        domain = domini_valors,
        na.color = "transparent")
      
      raster_dades <- rasterize(sf_dades, raster_dades_o, field = variable_repres)
    }
    
    mapa <- leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = sf_dades, color = "black", opacity = 1, weight = ~width_value,
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 2,
                                                      bringToFront = TRUE),
                  label = ~paste(Desc_Valor_CA, "<br>",
                                 input$nivell_territorial,": ", NOM, "<br>",
                                 input$percentatge,": ", get(variable_repres)) %>% lapply(., HTML)) %>%
      addRasterImage(raster_dades, opacity = 0.7, colors = paleta_espectral, project = F)
    
    # if (input$origen_poblacio %in% c("Tots")){
    if (length(input$origen_poblacio)!=1){
      
      labels_llegenda <- unique(dimensio_pais$Desc_Valor_CA) %>%
        intersect(unique(sf_dades() %>% pull(Desc_Valor_CA)))
      
      labels_tallades <- substr(labels_llegenda, 1, 30)
      labels_tallades <- ifelse(nchar(labels_llegenda) > 30,
                                paste0(labels_tallades, "..."),
                                labels_tallades)
      
      # colors_paleta <- colorRampPalette(brewer.pal(length(domini_valors),"viridis"))
      colors_paleta <- viridis_pal()(length(domini_valors))
      
      mapa %>% addLegend(position = "topright",
                         title = titol_llegenda,
                         opacity = 1,
                         # colors = colors_paleta(length(domini_valors)),
                         colors = colors_paleta,
                         values = domini_valors,
                         labels = labels_tallades)
    } else {
      
      mapa %>% addLegend(position = "topright",
                         title = titol_llegenda,
                         opacity = 1,
                         pal = paleta_espectral,
                         values = domini_valors)
    }
    
  })
  
  output$map_renda <- renderLeaflet({ #creem el mapa amb les dades
    
    sf_dades <- sf_dades() %>%
      filter(Data_Referencia == input$any_selector) %>%
      mutate(Import_Renda_Bruta = floor(Import_Renda_Bruta))
    
    domini_valors <- seq(min(sf_dades$Import_Renda_Bruta, na.rm = T),
                         max(sf_dades$Import_Renda_Bruta, na.rm = T),
                         length.out = 10)
    
    raster_dades <- raster(ext = extent(sf_dades))
    raster_dades <- rasterize(sf_dades, raster_dades, field = "Import_Renda_Bruta")
    
    paleta_espectral <- colorNumeric(
      "viridis", 
      domain = domini_valors,
      na.color = "transparent")
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = sf_dades, color = "black", opacity = 1, weight = ~width_value,
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 2,
                                                      bringToFront = TRUE),
                  label = ~paste(input$nivell_territorial,": ", NOM, "<br>",
                                 "Valor: ", Import_Renda_Bruta) %>% lapply(., HTML)) %>%
      addRasterImage(raster_dades, opacity = 0.7, colors = paleta_espectral, project = F) %>%
      addLegend(position = "topright",
                title = "Renda bruta per persona (€/pp)",
                opacity = 1,
                pal = paleta_espectral,
                values = domini_valors)
    
  })
  
  output$map_turisme <- renderLeaflet({ #creem el mapa amb les dades
    
    sf_dades <- sf_dades()%>%
      filter(Data_Referencia == input$any_selector)
    
    domini_valors <- seq(min(sf_dades$Pisos, na.rm = T),
                         max(sf_dades$Pisos, na.rm = T),
                         length.out = 10)
    
    raster_dades <- raster(ext = extent(sf_dades))
    raster_dades <- rasterize(sf_dades, raster_dades, field = "Pisos")
    
    paleta_espectral <- colorNumeric(
      "viridis", 
      domain = domini_valors,
      na.color = "transparent")
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = sf_dades, color = "black", opacity = 1, weight = ~width_value,
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 2,
                                                      bringToFront = TRUE),
                  label = ~paste(input$nivell_territorial,": ", NOM, "<br>",
                                 "Valor: ", Pisos) %>% lapply(., HTML)) %>%
      addRasterImage(raster_dades, opacity = 0.7, colors = paleta_espectral, project = F) %>%
      addLegend(position = "topright",
                title = "Habitatges turístics",
                opacity = 1,
                pal = paleta_espectral,
                values = domini_valors)
    
  })
  
  output$map_edat <- renderLeaflet({ #creem el mapa amb les dades
    
    sf_dades <- sf_dades() %>%
      filter(Data_Referencia == input$any_selector)
    
    raster_dades_o <- raster(ext = extent(sf_dades()))
    
    variable_grup <- "Desc_Valor_CA"
    if (input$percentatge == "%"){
      variable_repres <- "Perc_Poblacio"
      nom_eix_y <- "% Població"
    } else {
      variable_repres <- "Poblacio"
      nom_eix_y <- "Població"
    }
    
    # if (input$grup_edat %in% c("Tots")){
    if (length(input$grup_edat)!=1){
      
      domini_valors <- c(1:length(unique(sf_dades %>% pull(variable_grup))))
      titol_llegenda <- "Grup d'edat majoritari"
      paleta_espectral <- colorFactor(
        "viridis", 
        domain = domini_valors,
        na.color = "transparent")
      
      sf_dades <- sf_dades %>%
        mutate(Desc_Valor_CA = as.factor(Desc_Valor_CA),
               Perc_Poblacio = round(Perc_Poblacio*100, 2)) %>%
        group_by(Data_Referencia, !!sym(filtre_territorial()), Desc_Valor_CA) %>%
        arrange(desc(Perc_Poblacio)) %>%
        ungroup(Desc_Valor_CA) %>%
        mutate(EDAT_10 = as.integer(factor(EDAT_10))) %>% #Fem això per canviar-lis els valors
        slice_head(n = 1) %>%
        ungroup()
      
      raster_dades <- rasterize(sf_dades, raster_dades_o, field = "EDAT_10")
      
    } else {
      
      sf_dades <- sf_dades %>%
        mutate(Desc_Valor_CA = as.factor(Desc_Valor_CA),
               Perc_Poblacio = round(Perc_Poblacio*100, 2)) %>%
        filter(Desc_Valor_CA %in% input$grup_edat)
      
      domini_valors <- seq(min(sf_dades %>% pull(!!sym(variable_repres)), na.rm = T),
                           max(sf_dades %>% pull(!!sym(variable_repres)), na.rm = T),
                           length.out = 10)
      
      if (input$percentatge == "%"){
        titol_llegenda <- "Percentatge respecte al total (%)"
      } else {
        titol_llegenda <- "Població"
      }
      
      paleta_espectral <- colorNumeric(
        "viridis",
        domain = domini_valors,
        na.color = "transparent")
      
      raster_dades <- rasterize(sf_dades, raster_dades_o, field = variable_repres)
    }
    
    mapa <- leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = sf_dades, color = "black", opacity = 1, weight = ~width_value,
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 2,
                                                      bringToFront = TRUE),
                  label = ~paste(Desc_Valor_CA, "<br>",
                                 input$nivell_territorial,": ", NOM, "<br>",
                                 input$percentatge,": ", get(variable_repres)) %>% lapply(., HTML)) %>%
      addRasterImage(raster_dades, opacity = 0.7, colors = paleta_espectral, project = F)
    
    # if (input$grup_edat %in% c("Tots")){
    if (length(input$grup_edat)!=1){
      
      labels_llegenda <- unique(dimensio_edat$Desc_Valor_CA) %>%
        intersect(unique(sf_dades() %>% pull(Desc_Valor_CA)))
      
      labels_tallades <- substr(labels_llegenda, 1, 30)
      labels_tallades <- ifelse(nchar(labels_llegenda) > 30,
                                paste0(labels_tallades, "..."),
                                labels_tallades)
      
      # colors_paleta <- colorRampPalette(brewer.pal(length(domini_valors),"viridis"))
      colors_paleta <- viridis_pal()(length(domini_valors))
      
      mapa %>% addLegend(position = "topright",
                         title = titol_llegenda,
                         opacity = 1,
                         # colors = colors_paleta(length(domini_valors)),
                         colors = colors_paleta,
                         values = domini_valors,
                         labels = labels_tallades)
    } else {
      
      mapa %>% addLegend(position = "topright",
                         title = titol_llegenda,
                         opacity = 1,
                         pal = paleta_espectral,
                         values = domini_valors)
    }
  })
  
  output$map_var_lloguer <- 
    output$map_var_estudis <-
    output$map_var_domicilis <-
    output$map_var_poborigen <-
    output$map_var_renda <-
    output$map_var_grupedat <- renderLeaflet({
    
    variable_categoria <- switch(final_nom_tab(),
                                 "estudis" = "NIV_EDUCA_esta",
                                 "domicilis" = "TIPUS_DOMICILI",
                                 "poborigen" = "LLOC_NAIX_REGIO",
                                 "grupedat" = "EDAT_10",
                                 FALSE)
    
    input_value <- switch(variable_categoria,
                          "NIV_EDUCA_esta" = input$nivell_estudi_selector,
                          "TIPUS_DOMICILI" = input$tipus_domicili_selector,
                          "LLOC_NAIX_REGIO" = input$origen_poblacio,
                          "EDAT_10" = input$grup_edat,
                          FALSE)
    
    titol_llegenda <- switch(final_nom_tab(),
                             "lloguer" = "lloguer (€/m2)",
                             "renda" = "renda (€/pp)",
                             "domicilis" = "domicilis",
                             "població")
    
    sf_dades_var <- sf_dades_var() %>%
      filter(Data_Referencia == input$any_selector)
    
    if (!isFALSE(variable_categoria)){
      sf_dades_var <- sf_dades_var %>%
        mutate(Desc_Valor_CA = as.factor(Desc_Valor_CA))
    }
    
    raster_dades_o <- raster(ext = extent(sf_dades_var))
    
    variable_grup <- "NOM"
    
    if(!isFALSE(variable_categoria)){
      variable_grup <- "Desc_Valor_CA"
    }
    
    dades_variacio <- dades_variacio()$original
    
    
    if (!isFALSE(variable_categoria)){
      # if(input_value == "Tots"){
      if(length(input_value)!=1){
        
        domini_valors <- c(1:length(unique(sf_dades_var %>% pull(variable_grup))))
        titol_llegenda <- "Variació positiva més alta"

        paleta_espectral <- colorFactor(
          "viridis",
          domain = domini_valors,
          na.color = "transparent")
        
        sf_dades_var <- sf_dades_var %>%
          group_by(Data_Referencia, !!sym(filtre_territorial()), Desc_Valor_CA) %>%
          arrange(desc(Var)) %>%
          ungroup(Desc_Valor_CA) %>%
          mutate(!!sym(variable_categoria) := as.integer(factor(!!sym(variable_categoria)))) %>% #Fem això per canviar-lis els valors
          slice_head(n = 1) %>%
          ungroup
        
        raster_dades <- rasterize(sf_dades_var, raster_dades_o, field = variable_categoria)
        
        if (variable_categoria == "LLOC_NAIX_REGIO"){
          labels_llegenda <- dimensio_pais %>%
            pull(Desc_Valor_CA)
        } else {
          if (variable_categoria == "EDAT_10"){
            labels_llegenda <- dimensio_edat %>%
              pull(Desc_Valor_CA)
          } else {
          labels_llegenda <- dimensions_pad %>%
            filter(Desc_Dimensio == variable_categoria) %>%
            pull(Desc_Valor_CA)
          }
        }
        
        labels_llegenda <- labels_llegenda %>%
          intersect(unique(sf_dades_var() %>% pull(Desc_Valor_CA)))
        
        labels_tallades <- substr(labels_llegenda, 1, 30)
        labels_tallades <- ifelse(nchar(labels_llegenda) > 30,
                                  paste0(labels_tallades, "..."),
                                  labels_tallades)
        
        # colors_paleta <- colorRampPalette(brewer.pal(length(domini_valors),"viridis"))
        colors_paleta <- viridis_pal()(length(domini_valors))
      } else {
        
        sf_dades_var <- sf_dades_var %>%
          filter(Desc_Valor_CA %in% input_value)
        
        domini_valors <- seq(min(sf_dades_var %>% pull(Var), na.rm = T),
                             max(sf_dades_var %>% pull(Var), na.rm = T),
                             length.out = 10)
        
        paleta_espectral <- colorNumeric(
          "viridis",
          domain = domini_valors,
          na.color = "transparent")
        
        raster_dades <- rasterize(sf_dades_var, raster_dades_o, field = "Var")
      }
    } else {
      
      domini_valors <- seq(min(sf_dades_var %>% pull(Var), na.rm = T),
                           max(sf_dades_var %>% pull(Var), na.rm = T),
                           length.out = 10)
      
      paleta_espectral <- colorNumeric(
        "viridis",
        domain = domini_valors,
        na.color = "transparent")
      
      raster_dades <- rasterize(sf_dades_var, raster_dades_o, field = "Var")
    }
    
    mapa <- leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addRasterImage(raster_dades, opacity = 0.7, colors = paleta_espectral, project = F)
    
    if (!isFALSE(variable_categoria)){
      mapa <- mapa %>%
        addPolygons(data = sf_dades_var, color = "black", opacity = 1, weight = ~width_value,
                    highlightOptions = highlightOptions(color = "white",
                                                        weight = 2,
                                                        bringToFront = TRUE),
                    label = ~paste(get(variable_grup), "<br>",
                                   input$nivell_territorial,": ", NOM, "<br>",
                                   "Variació: ", Var) %>% lapply(., HTML))
      
      # if(input_value == "Tots"){
      if(length(input_value)!=1){
        
        mapa %>% addLegend(position = "topright",
                           title = titol_llegenda,
                           opacity = 1,
                           # colors = colors_paleta(length(domini_valors)),
                           colors = colors_paleta,
                           values = domini_valors,
                           labels = labels_tallades)
      } else {
        mapa %>% addLegend(position = "topright",
                  title = paste0("Variació ", titol_llegenda),
                  opacity = 1,
                  pal = paleta_espectral,
                  values = domini_valors)
      }
      
      
    } else {
      mapa %>%
        addPolygons(data = sf_dades_var, color = "black", opacity = 1, weight = ~width_value,
                    highlightOptions = highlightOptions(color = "white",
                                                        weight = 2,
                                                        bringToFront = TRUE),
                    label = ~paste(input$nivell_territorial,": ", NOM, "<br>",
                                   "Variació: ", Var) %>% lapply(., HTML)) %>%
        addLegend(position = "topright",
                  title = paste0("Variació ", titol_llegenda),
                  opacity = 1,
                  pal = paleta_espectral,
                  values = domini_valors)
    }
  })
  
  output$map_perc_lloguer <-
    output$map_perc_estudis <-
    output$map_perc_domicilis <-
    output$map_perc_poborigen <-
    output$map_perc_renda <-
    output$map_perc_grupedat <- renderLeaflet({
    
    variable_categoria <- switch(final_nom_tab(),
                                 "estudis" = "NIV_EDUCA_esta",
                                 "domicilis" = "TIPUS_DOMICILI",
                                 "poborigen" = "LLOC_NAIX_REGIO",
                                 "grupedat" = "EDAT_10",
                                 FALSE)
    
    input_value <- switch(variable_categoria,
                          "NIV_EDUCA_esta" = input$nivell_estudi_selector,
                          "TIPUS_DOMICILI" = input$tipus_domicili_selector,
                          "LLOC_NAIX_REGIO" = input$origen_poblacio,
                          "EDAT_10" = input$grup_edat,
                          FALSE)
    
    titol_llegenda <- switch(final_nom_tab(),
                             "lloguer" = "lloguer (€/m2)",
                             "renda" = "renda (€/pp)",
                             "domicilis" = "domicilis",
                             "població")
    
    sf_dades_var <- sf_dades_var() %>%
      filter(Data_Referencia == input$any_selector)
    
    if (!isFALSE(variable_categoria)){
      sf_dades_var <- sf_dades_var %>%
        mutate(Desc_Valor_CA = as.factor(Desc_Valor_CA))
    }
    
    raster_dades_o <- raster(ext = extent(sf_dades_var))
    
    variable_grup <- "NOM"
    
    if(!isFALSE(variable_categoria)){
      variable_grup <- "Desc_Valor_CA"
    }
    
    dades_variacio <- dades_variacio()
    
    if (!isFALSE(variable_categoria)){
      # if(input_value == "Tots"){
      if(length(input_value)!=1){
        
        domini_valors <- c(1:length(unique(sf_dades_var %>% pull(variable_grup))))
        titol_llegenda <- "% Variació positiva més alta"
        
        paleta_espectral <- colorFactor(
          "viridis",
          domain = c(domini_valors),
          na.color = "transparent")
        
        sf_dades_var <- sf_dades_var %>%
          group_by(Data_Referencia, !!sym(filtre_territorial()), Desc_Valor_CA) %>%
          arrange(desc(Var_Perc)) %>%
          ungroup(Desc_Valor_CA) %>% 
          mutate(!!sym(variable_categoria) := as.integer(factor(!!sym(variable_categoria)))) %>% #Fem això per canviar-lis els valors
          slice_head(n = 1) %>%
          ungroup
        
        raster_dades <- rasterize(sf_dades_var, raster_dades_o, field = variable_categoria)
        
        if (variable_categoria == "LLOC_NAIX_REGIO"){
          labels_llegenda <- dimensio_pais %>%
            pull(Desc_Valor_CA)
        } else {
          if (variable_categoria == "EDAT_10"){
            labels_llegenda <- dimensio_edat %>%
              pull(Desc_Valor_CA)
          } else {
            labels_llegenda <- dimensions_pad %>%
              filter(Desc_Dimensio == variable_categoria) %>%
              pull(Desc_Valor_CA)
          }
        }
        
        labels_llegenda <- labels_llegenda %>%
          intersect(unique(sf_dades_var() %>% pull (Desc_Valor_CA)))
        
        labels_tallades <- substr(labels_llegenda, 1, 30)
        labels_tallades <- ifelse(nchar(labels_llegenda) > 30,
                                  paste0(labels_tallades, "..."),
                                  labels_tallades)
        
        # colors_paleta <- colorRampPalette(brewer.pal(length(domini_valors),"viridis"))
        colors_paleta <- viridis_pal()(length(domini_valors))
      } else {
        
        sf_dades_var <- sf_dades_var %>%
          filter(Desc_Valor_CA %in% input_value)
        
        domini_valors <- seq(min(sf_dades_var %>% pull(Var_Perc), na.rm = T),
                             max(sf_dades_var %>% pull(Var_Perc), na.rm = T),
                             length.out = 10)
        
        paleta_espectral <- colorNumeric(
          "viridis",
          domain = domini_valors,
          na.color = "transparent")
        
        raster_dades <- rasterize(sf_dades_var, raster_dades_o, field = "Var_Perc")
      }
    } else {
      
      domini_valors <- seq(min(sf_dades_var %>% pull(Perc_Var), na.rm = T),
                           max(sf_dades_var %>% pull(Perc_Var), na.rm = T),
                           length.out = 10)
      
      paleta_espectral <- colorNumeric(
        "viridis",
        domain = domini_valors,
        na.color = "transparent")
      
      raster_dades <- rasterize(sf_dades_var, raster_dades_o, field = "Perc_Var")
    }
    
    mapa <- leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addRasterImage(raster_dades, opacity = 0.7, colors = paleta_espectral, project = F)
    
    if (!isFALSE(variable_categoria)){
      mapa <- mapa %>%
        addPolygons(data = sf_dades_var, color = "black", opacity = 1, weight = ~width_value,
                    highlightOptions = highlightOptions(color = "white",
                                                        weight = 2,
                                                        bringToFront = TRUE),
                    label = ~paste(get(variable_grup), "<br>",
                                   input$nivell_territorial,": ", NOM, "<br>",
                                   "% Variació: ", Var_Perc) %>% lapply(., HTML))
      
      # if(input_value == "Tots"){
      if(length(input_value)!=1){
        mapa %>% addLegend(position = "topright",
                           title = titol_llegenda,
                           opacity = 1,
                           # colors = colors_paleta(length(domini_valors)),
                           colors = colors_paleta,
                           values = domini_valors,
                           labels = labels_tallades)
      } else {
        mapa %>% addLegend(position = "topright",
                           title = paste0("% Variació ", titol_llegenda),
                           opacity = 1,
                           pal = paleta_espectral,
                           values = domini_valors)
      }
      
      
    } else {
      mapa %>%
        addPolygons(data = sf_dades_var, color = "black", opacity = 1, weight = ~width_value,
                    highlightOptions = highlightOptions(color = "white",
                                                        weight = 2,
                                                        bringToFront = TRUE),
                    label = ~paste(input$nivell_territorial,": ", NOM, "<br>",
                                   "% Variació: ", Perc_Var) %>% lapply(., HTML)) %>%
        addLegend(position = "topright",
                  title = paste0("% Variació ", titol_llegenda),
                  opacity = 1,
                  pal = paleta_espectral,
                  values = domini_valors)
    }
  })
  
  #### GENTRIFICACIO ####
  
  observeEvent(inici_nom_tab(),{
    if (inici_nom_tab() == "gen"){
    
      updateSliderInput(session, "any_selector", 
                        min = 2020, 
                        max = 2023,
                        step = 1)
      
      updateSelectInput(session, "nivell_territorial", 
                        selected = "Barri")
    }
  })
  
  kmeans_clusters <- reactive({
    
    cluster_gentrificacio <- lapply(df_gentrificacio, function(x){
      subset(x, select = -c(Punt_Gentrificacio, Codi_Barri))
    })
    
    set.seed(NULL)
    
    # Calculem la "llavor" que ens dona millor qualitat en el conjunt de clusters
    qual_maxim <- 0
    seed_maxim <- 0
    
    for (n_seed in c(1:100)){
      
      clusters <- list()
      
      set.seed(n_seed)
      
      clusters[["2020"]] <- kmeans(cluster_gentrificacio[["2020"]], 3)
      clusters[["2021"]] <- kmeans(cluster_gentrificacio[["2021"]], centers = clusters[["2020"]]$centers)
      clusters[["2022"]] <- kmeans(cluster_gentrificacio[["2022"]], centers = clusters[["2021"]]$centers)
      clusters[["2023"]] <- kmeans(cluster_gentrificacio[["2023"]], centers = clusters[["2022"]]$centers)
      
      values_qual <- lapply(clusters, function(x){
        100*x$betweenss/x$totss
      })
      
      qual_seed <- prod(unlist(values_qual))
      
      if (qual_seed > qual_maxim){
        qual_maxim <- qual_seed
        seed_maxim <- n_seed
      }
    }
    
    set.seed(seed_maxim)
    
    clusters <- list()
    
    clusters[["2020"]] <- kmeans(cluster_gentrificacio[["2020"]], 3)
    clusters[["2021"]] <- kmeans(cluster_gentrificacio[["2021"]], centers = clusters[["2020"]]$centers)
    clusters[["2022"]] <- kmeans(cluster_gentrificacio[["2022"]], centers = clusters[["2021"]]$centers)
    clusters[["2023"]] <- kmeans(cluster_gentrificacio[["2023"]], centers = clusters[["2022"]]$centers)
    
    set.seed(NULL)
    
    clusters
    
    # Canviem el valor de la etiqueta perquè sigui coherent amb l'altre càlcul de gentrificació
    canviar_etiqueta <- function(df) {
      tibble(cluster = df$cluster) %>%
        mutate(cluster = case_when(
          cluster == 2 ~ 3,
          cluster == 3 ~ 2,
          TRUE ~ cluster
        ))
    }
    
    # Aplicar la funció a cada any de la llista
    clusters <- lapply(clusters, canviar_etiqueta)
  })
  
  lopez_gay_clusters <- reactive({
    
    clusters <- lapply(df_gentrificacio, function(x){
      x %>%
        mutate(Perc_50 = quantile(Punt_Gentrificacio, probs = c(0.5)),
               Perc_75 = quantile(Punt_Gentrificacio, probs = c(0.75))) %>%
        mutate(cluster = if_else(Punt_Gentrificacio >= Perc_75, 1,
                                 if_else(Punt_Gentrificacio >= Perc_50, 2, 3))) %>%
        dplyr::select(cluster)
    })
    
    clusters
    
  })
  
  output$mapa_gentrificacio_cluster <- renderLeaflet({
    
    df_gentrificacio_clusters <- Map(function(df, clust){
      df$cluster <- clust$cluster
      return(df)
    }, df_gentrificacio, kmeans_clusters())
    
    sf_dades <- lapply(df_gentrificacio_clusters, function(x){
      info_barris %>% st_as_sf %>% left_join(x)
    })
    
    raster_dades <- raster(ext = extent(info_barris %>% st_as_sf))
    
    raster_gentrificacio <- lapply(sf_dades, function(x){
      rasterize(x, raster_dades, field = "cluster")
    })
    
    any_represent <- input$any_selector %>% as.character
    
    raster_represent <- raster_gentrificacio[[any_represent]]
    sf_dades_represent <- sf_dades[[any_represent]]
    
    sf_dades_represent <- sf_dades_represent %>%
      mutate(width_value = if_else(NOM %in% input$seleccio_territorial, 5, 1.5))
    
    print(sf_dades_represent)
    paleta_espectral <- colorFactor(
      "viridis", 
      domain = c(1:3),
      na.color = "transparent")
    
    mapa <- leaflet() %>%
      addProviderTiles("CartoDB.Positron",
                       options = providerTileOptions(minZoom = 11)) %>%
      addRasterImage(raster_represent, opacity = 0.7, colors = paleta_espectral, project = F) %>%
      addPolygons(data = sf_dades_represent, color = "black", opacity = 1, weight = ~width_value,
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 2,
                                                      bringToFront = TRUE),
                  label = ~paste("Barri: ", NOM, "<br>",
                                 "Grup Gentrificació: ", cluster) %>% lapply(., HTML)) %>%
      addLegend(position = "topright",
                title = "Índex gentrificació ",
                opacity = 1,
                colors = paleta_espectral(1:3),
                values = c(1:3),
                labels = c("Gentrificant",
                           "Lleugera tendència gentrificadora",
                           "Sense dinàmiques gentrificadores"))
    
  })
  
  output$mapa_gentrificacio_quantils <- renderLeaflet({
    
    df_gentrificacio_clusters <- Map(function(df, clust){
      df$cluster <- clust$cluster
      return(df)
    }, df_gentrificacio, lopez_gay_clusters())
    
    sf_dades <- lapply(df_gentrificacio_clusters, function(x){
      info_barris %>% st_as_sf %>% left_join(x)
    })
    
    raster_dades <- raster(ext = extent(info_barris %>% st_as_sf))
    
    raster_gentrificacio <- lapply(sf_dades, function(x){
      rasterize(x, raster_dades, field = "cluster")
    })
    
    any_represent <- input$any_selector %>% as.character
    
    raster_represent <- raster_gentrificacio[[any_represent]]
    sf_dades_represent <- sf_dades[[any_represent]]
    
    sf_dades_represent <- sf_dades_represent %>%
      mutate(width_value = if_else(NOM %in% input$seleccio_territorial, 5, 1.5))
    
    paleta_espectral <- colorFactor(
      "viridis", 
      domain = c(1:3),
      na.color = "transparent")
    
    mapa <- leaflet() %>%
      addProviderTiles("CartoDB.Positron",
                       options = providerTileOptions(minZoom = 11)) %>%
      addRasterImage(raster_represent, opacity = 0.7, colors = paleta_espectral, project = F) %>%
      addPolygons(data = sf_dades_represent, color = "black", opacity = 1, weight = ~width_value,
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 2,
                                                      bringToFront = TRUE),
                  label = ~paste("Barri: ", NOM, "<br>",
                                 "Grup Gentrificació: ", cluster, "<br>",
                                 "Puntació Gentrificació: ", round(Punt_Gentrificacio, 2)) %>% lapply(., HTML)) %>%
      addLegend(position = "topright",
                title = "Índex gentrificació ",
                opacity = 1,
                colors = paleta_espectral(1:3),
                values = c(1:3),
                labels = c("Gentrificant",
                           "Lleugera tendència gentrificadora",
                           "Sense dinàmiques gentrificadores"))
    
  })
  
  output$taula_comparativa <- renderDT({
    
    valors_quantils <- Map(function(df, clust){
      df$cluster <- clust$cluster
      return(df)
    }, df_gentrificacio, lopez_gay_clusters())
    
    valors_cluster <- Map(function(df, clust){
      df$cluster <- clust$cluster
      return(df)
    }, df_gentrificacio, kmeans_clusters())
    
    # valors_quantils <- lopez_gay_clusters()
    # valors_cluster <- kmeans_clusters()
    
    any_represent <- input$any_selector %>% as.character
    
    valors_quantils <- valors_quantils[[any_represent]] %>%
      left_join(info_barris[,c("Codi_Barri", "NOM")]) %>% 
      subset(select = c(Codi_Barri, NOM, cluster, Punt_Gentrificacio)) %>%
      mutate(Punt_Gentrificacio = round(Punt_Gentrificacio, 2)) %>%
      arrange(Codi_Barri) %>%
      rename("Codi Barri" = Codi_Barri,
             "Índex Gentrificació" = Punt_Gentrificacio,
             "Quantil" = cluster,
             "Barri" = NOM)
    
    valors_cluster <- valors_cluster[[any_represent]] %>%
      left_join(info_barris[,c("Codi_Barri", "NOM")]) %>%
      subset(select = c(Codi_Barri, NOM, cluster)) %>%
      arrange(Codi_Barri) %>%
      rename("Codi Barri" = Codi_Barri,
             "K-Means" = cluster,
             "Barri" = NOM)
    
    full_join(valors_cluster, valors_quantils) %>%
      datatable(options = list(
        pageLength = 20
      ))
    
    
    
    
  })
  
  
  #### MAPA BARRA LATERAL ####
  
  output$mapsidebar <- renderLeaflet({ #creem el mapa selector
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron",
                       options = providerTileOptions(minZoom = 11)) %>%
      addPolygons(data = choice_shp(), color = "blue", opacity = 1, weight = 1.5,
                  highlightOptions = highlightOptions(color = "red",
                                                      weight = 2.5,
                                                      bringToFront = TRUE),
                  label = ~paste(NOM))
  })
  
  observe({
    selected <- input$seleccio_territorial
    
    # Actualitzar els polígons al mapa segons la selecció
    leafletProxy("mapsidebar", data = choice_shp()) %>%
      clearShapes() %>%
      addPolygons(color = "blue", opacity = 1, weight = 1.5,
                  fillColor = ~ifelse(NOM %in% selected, "red", "transparent"),
                  fillOpacity = ~ifelse(NOM %in% selected, 0.5, 0),
                  highlightOptions = highlightOptions(color = "red",
                                                      weight = 2.5,
                                                      bringToFront = TRUE),
                  label = ~paste(NOM),
                  layerId = ~NOM)
  })
  
  #Obtenim les coordenades dels mapes i obtenim el codi del barri/districte
  obtenir_info_mapes <- function(map_input_id) {
    observeEvent(input[[map_input_id]], { 
      clicked_coords <- input[[map_input_id]]
      lng <- as.numeric(clicked_coords$lng)
      lat <- as.numeric(clicked_coords$lat)
      pnt_sf <- st_sfc(st_point(c(lng, lat)), crs = 4326)
      interseccions <- which(st_intersects(pnt_sf, choice_shp(), sparse = F))
      if (any(interseccions)){ # només s'actualitza si cliquem dins dels polígons
        selected_territorial <- choice_shp()[interseccions,]$NOM
        seleccions_actuals <- input$seleccio_territorial
        noves_seleccions <- union(setdiff(selected_territorial, seleccions_actuals),
                                  setdiff(seleccions_actuals, selected_territorial))
        # noves_seleccions <- unique(c(selected_territorial, seleccions_actuals))
        updatePickerInput(session, "seleccio_territorial",
                          selected = noves_seleccions)
      }
    })
  }
  
  obtenir_info_mapes("mapsidebar_click")
  obtenir_info_mapes("map_lloguer_click")
  obtenir_info_mapes("map_estudis_click")
  obtenir_info_mapes("map_domicilis_click")
  obtenir_info_mapes("map_origen_click")
  obtenir_info_mapes("map_renda_click")
  obtenir_info_mapes("map_turisme_click")
  obtenir_info_mapes("map_edat_click")
  obtenir_info_mapes("map_var_lloguer_click")
  obtenir_info_mapes("map_perc_lloguer_click")
  obtenir_info_mapes("map_var_estudis_click")
  obtenir_info_mapes("map_perc_estudis_click")
  obtenir_info_mapes("map_var_domicilis_click")
  obtenir_info_mapes("map_perc_domicilis_click")
  obtenir_info_mapes("map_var_poborigen_click")
  obtenir_info_mapes("map_perc_poborigen_click")
  obtenir_info_mapes("map_var_renda_click")
  obtenir_info_mapes("map_perc_renda_click")
  obtenir_info_mapes("map_var_grupedat_click")
  obtenir_info_mapes("map_perc_grupedat_click")
  obtenir_info_mapes("mapa_gentrificacio_cluster_click")
  obtenir_info_mapes("mapa_gentrificacio_quantils_click")
}
