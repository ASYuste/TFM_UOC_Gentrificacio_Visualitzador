# GLOBAL del visualitzador
# TFM - UOC
# Albert Salvador Yuste

suppressWarnings(suppressMessages({
  
  if (!require(pacman)) install.packages("pacman", dependencies = T); library(pacman)
  
  p_load(shiny, tidyverse, shinydashboard, highcharter, here, leaflet, sf,
         readxl, raster, scales, RColorBrewer, shinycssloaders, colorspace,
         imputeTS, viridis, shinyWidgets, DT)
  
  dimensions_pad <- read_csv(file.path(here(),"data/pad-dimensions/pad_dimensions.csv"))
  
  ##### Carreguem els shps ####
  shp_files <- list.files(file.path(here(),"data/20170706_Districtes_Barris/Unitats_Administratives_BCN/"),
                          pattern = ".shp", full.names = T)
  
  # SHP ciutat
  shp_ciutat <- st_read(dsn = shp_files[8]) %>% #0301040100_TermeMunicipal_UNITATS_ADM.shp
    st_transform(., crs = 4326) %>%
    mutate(Codi_Ciutat = "01")
  
  info_ciutat <- tibble(Codi_Ciutat = shp_ciutat$Codi_Ciutat,
                        NOM = shp_ciutat$NOM,
                        geometry = shp_ciutat$geometry)
  
  # SHP districte
  shp_districtes <- st_read(dsn = shp_files[4]) %>%
    st_transform(., crs = 4326) %>%
    rename(Codi_Districte = DISTRICTE)
  
  info_districtes <- tibble(Codi_Districte = shp_districtes$Codi_Districte,
                            NOM = shp_districtes$NOM,
                            geometry = shp_districtes$geometry)
  
  # SHP barri
  shp_barris <- st_read(dsn = shp_files[3]) %>%
    st_transform(., crs = 4326) %>%
    rename(Codi_Barri = BARRI)
  
  info_barris <- tibble(Codi_Barri = shp_barris$Codi_Barri,
                        NOM = shp_barris$NOM,
                        geometry = shp_barris$geometry)
  
  # Llista Districtes-Barri
  llista_dist_barr <- tibble(Codi_Districte = shp_barris$DISTRICTE,
                             Codi_Barri = shp_barris$Codi_Barri,
                             Barri = shp_barris$NOM) %>%
    left_join(info_districtes %>% rename(Districte = NOM)) %>%
    subset(select = -c(geometry)) %>%
    arrange(Codi_Districte, Codi_Barri) %>%
    group_by(Districte) %>%
    summarise(Barris = list(Barri)) %>%
    deframe()
  ########
  
  #### Lloguers ####
  lloguers_df <- read_csv(file.path(here(), "data/L1/preus_lloguers.csv"))
  
  lloguer_ciutat <- lloguers_df %>%
    filter(!is.na(Codi_Ciutat)) %>%
    subset(select = -c(Codi_Districte, Codi_Barri)) %>%
    pivot_wider(names_from = Variable,
                values_from = Valor) %>%
    left_join(info_ciutat[,c(1:2)])
  
  lloguer_districtes <- lloguers_df %>%
    filter(!is.na(Codi_Districte)) %>%
    subset(select = -c(Codi_Ciutat, Codi_Barri)) %>%
    pivot_wider(names_from = Variable,
                values_from = Valor) %>%
    left_join(info_districtes[,c(1:2)])
  
  lloguer_barris <- lloguers_df %>%
    filter(!is.na(Codi_Barri)) %>%
    subset(select = -c(Codi_Ciutat, Codi_Districte)) %>%
    pivot_wider(names_from = Variable,
                values_from = Valor) %>%
    left_join(info_barris[,c(1:2)])
  
  
  ########
  
  #### Nivell Estudis ####
  pob_uni_df <- read_csv(file.path(here(), "data/L1/pad_mdb_niv-educa-esta.csv"))
  
  dimensions_uni <- dimensions_pad %>%
    filter(Desc_Dimensio == "NIV_EDUCA_esta") %>%
    subset(select = c(Codi_Valor, Desc_Valor_CA)) %>%
    rename(NIV_EDUCA_esta = Codi_Valor)
  
  pob_uni_barri <- pob_uni_df %>%
    group_by(Data_Referencia, Codi_Barri, NIV_EDUCA_esta) %>%
    summarise(Poblacio = sum(Poblacio, na.rm = T)) %>%
    ungroup(NIV_EDUCA_esta) %>%
    mutate(REF_Poblacio = sum(Poblacio, na.rm = T),
           Perc_Poblacio = Poblacio/REF_Poblacio) %>%
    ungroup() %>%
    left_join(info_barris[,c(1:2)]) %>%
    left_join(dimensions_uni)
  
  pob_uni_districte <- pob_uni_df %>%
    group_by(Data_Referencia, Codi_Districte, NIV_EDUCA_esta) %>%
    summarise(Poblacio = sum(Poblacio, na.rm = T)) %>%
    ungroup(NIV_EDUCA_esta) %>%
    mutate(REF_Poblacio = sum(Poblacio, na.rm = T),
           Perc_Poblacio = Poblacio/REF_Poblacio) %>%
    ungroup() %>%
    left_join(info_districtes[,c(1:2)]) %>%
    left_join(dimensions_uni)
  
  pob_uni_ciutat <- pob_uni_df %>%
    group_by(Data_Referencia, Codi_Ciutat, NIV_EDUCA_esta) %>%
    summarise(Poblacio = sum(Poblacio, na.rm = T)) %>%
    ungroup(NIV_EDUCA_esta) %>%
    mutate(REF_Poblacio = sum(Poblacio, na.rm = T),
           Perc_Poblacio = Poblacio/REF_Poblacio) %>% 
    ungroup() %>%
    left_join(info_ciutat[,c(1:2)]) %>%
    left_join(dimensions_uni)
  
  ########
  
  #### Domicilis per tipus de vivenda ####
  tipus_domicili_df <- read_csv(file.path(here(), "data/L1/pad_dom_mdb_tipus-domicili.csv"))
  
  # Reduïm algunes variables
  tipus_domicili_df <- tipus_domicili_df %>%
    mutate(TIPUS_DOMICILI = 
             if_else(TIPUS_DOMICILI %in% c(1,2), 1,
                     if_else(TIPUS_DOMICILI %in% c(3,4),2,
                             if_else(TIPUS_DOMICILI %in% c(5:8), TIPUS_DOMICILI-2,
                                     if_else(TIPUS_DOMICILI %in% c(9, 10), 7,
                                             TIPUS_DOMICILI - 3)))))
  
  dimensions_domicili <- dimensions_pad %>%
    filter(Desc_Dimensio == "TIPUS_DOMICILI") %>%
    subset(select = c(Codi_Valor, Desc_Valor_CA)) %>%
    rename(TIPUS_DOMICILI = Codi_Valor)
  
  dimensions_domicili_new <- tibble(
    "TIPUS_DOMICILI" = c(1:9),
    "Desc_Valor_CA" = c("Una persona de 18 a 64 anys",
                        "Una persona de 65 anys i més",
                        dimensions_domicili$Desc_Valor_CA[5:8],
                        "Dues persones o més: una de 18 anys i més amb altres menors de 18 anys",
                        dimensions_domicili$Desc_Valor_CA[11:12])
  )
  
  tipus_domicili_df <- tipus_domicili_df %>%
    group_by(Data_Referencia, Codi_Barri, TIPUS_DOMICILI) %>%
    mutate(Domicilis = sum(Domicilis, na.rm = T)) %>%
    slice_head(n = 1) %>%
    ungroup()
  
  tipus_domicili_barri <- tipus_domicili_df %>%
    group_by(Data_Referencia, Codi_Barri, TIPUS_DOMICILI) %>%
    summarise(Domicilis = sum(Domicilis, na.rm = T)) %>%
    ungroup(TIPUS_DOMICILI) %>%
    mutate(REF_Domicilis = sum(Domicilis, na.rm = T),
           Perc_Domicilis = Domicilis/REF_Domicilis) %>%
    ungroup() %>%
    left_join(info_barris[,c(1:2)]) %>%
    left_join(dimensions_domicili_new)
  
  tipus_domicili_districte <- tipus_domicili_df %>%
    group_by(Data_Referencia, Codi_Districte, TIPUS_DOMICILI) %>%
    summarise(Domicilis = sum(Domicilis, na.rm = T)) %>%
    ungroup(TIPUS_DOMICILI) %>%
    mutate(REF_Domicilis = sum(Domicilis, na.rm = T),
           Perc_Domicilis = Domicilis/REF_Domicilis) %>%
    ungroup() %>%
    left_join(info_districtes[,c(1:2)]) %>%
    left_join(dimensions_domicili_new)
  
  tipus_domicili_ciutat <- tipus_domicili_df %>%
    group_by(Data_Referencia, Codi_Ciutat, TIPUS_DOMICILI) %>%
    summarise(Domicilis = sum(Domicilis, na.rm = T)) %>%
    ungroup(TIPUS_DOMICILI) %>%
    mutate(REF_Domicilis = sum(Domicilis, na.rm = T),
           Perc_Domicilis = Domicilis/REF_Domicilis) %>%
    ungroup() %>%
    left_join(info_ciutat[,c(1:2)]) %>%
    left_join(dimensions_domicili_new)
  
  ########
  
  #### Població per regió de naixement ####
  regio_naixement_df <- read_csv(file.path(here(), "data/L1/pad_mdb_lloc-naix-regio.csv"))
  
  # Utilitzem una nova equivalència que s'ha utilitzat anteriorment al definir l'anterior arxiu
  # Adaptem els valors de LLOC_NAIX_REGIO per fer alguns agrupaments per continents
  dimensio_pais <- tibble(
    "LLOC_NAIX_REGIO" = c(1:7),
    "Desc_Valor_CA" = c("Àfrica", "Amèrica central/sud", "Amèrica del nord",
                        "Àsia", "Europa", "Oceania", "No consta")
  )
  
  regio_naixement_barri <- regio_naixement_df %>%
    group_by(Data_Referencia, Codi_Barri, LLOC_NAIX_REGIO) %>%
    summarise(Poblacio = sum(Poblacio, na.rm = T),
              REF_Poblacio = sum(REF_Poblacio)) %>%
    ungroup(LLOC_NAIX_REGIO) %>%
    mutate(Perc_Poblacio = Poblacio/REF_Poblacio) %>%
    ungroup() %>%
    left_join(info_barris[,c(1:2)]) %>%
    left_join(dimensio_pais)
  
  regio_naixement_districte <- regio_naixement_df %>%
    group_by(Data_Referencia, Codi_Districte, LLOC_NAIX_REGIO) %>%
    summarise(Poblacio = sum(Poblacio, na.rm = T),
              REF_Poblacio = sum(REF_Poblacio, na.rm = T)) %>%
    ungroup(LLOC_NAIX_REGIO) %>%
    mutate(Perc_Poblacio = Poblacio/REF_Poblacio) %>%
    ungroup() %>%
    left_join(info_districtes[,c(1:2)]) %>%
    left_join(dimensio_pais)
  
  regio_naixement_ciutat <- regio_naixement_df %>%
    group_by(Data_Referencia, Codi_Ciutat, LLOC_NAIX_REGIO) %>%
    summarise(Poblacio = sum(Poblacio, na.rm = T),
              REF_Poblacio = sum(REF_Poblacio, na.rm = T)) %>%
    ungroup(LLOC_NAIX_REGIO) %>%
    mutate(Perc_Poblacio = Poblacio/REF_Poblacio) %>%
    ungroup() %>%
    left_join(info_ciutat[,c(1:2)]) %>%
    left_join(dimensio_pais)
  
  ########
  
  #### Sous ####
  
  sous_df <- read_csv(file.path(here(),"data/L1/atles-renda-bruta-per-persona.csv"))
  
  sous_barri <- sous_df %>%
    group_by(Data_Referencia, Codi_Barri) %>%
    summarise(Poblacio = sum(Poblacio, na.rm = T),
              Import = sum(Import, na.rm = T)) %>%
    ungroup() %>%
    mutate(Import_Renda_Bruta = Import/Poblacio) %>%
    left_join(info_barris[,c(1:2)])
  
  sous_districte <- sous_df %>%
    group_by(Data_Referencia, Codi_Districte) %>%
    summarise(Poblacio = sum(Poblacio, na.rm = T),
              Import = sum(Import, na.rm = T)) %>%
    ungroup() %>%
    mutate(Import_Renda_Bruta = Import/Poblacio) %>%
    left_join(info_districtes[,c(1:2)])
  
  sous_ciutat <- sous_df %>%
    group_by(Data_Referencia, Codi_Ciutat) %>%
    summarise(Poblacio = sum(Poblacio, na.rm = T),
              Import = sum(Import, na.rm = T)) %>%
    ungroup() %>%
    mutate(Import_Renda_Bruta = Import/Poblacio) %>%
    left_join(info_ciutat[,c(1:2)])
  
  ########
  
  #### Habitatges turístics ####
  
  hab_tur_df <- read_csv(file.path(here(), "data/L1/nombre_habitatge_turistic.csv"))
  
  hab_tur_barri <- hab_tur_df %>%
    group_by(Data_Referencia, Codi_Barri) %>%
    summarise(Pisos = sum(Pisos, na.rm = T)) %>%
    ungroup() %>%
    left_join(info_barris[,c(1:2)])
  
  hab_tur_districte <- hab_tur_df %>%
    group_by(Data_Referencia, Codi_Districte) %>%
    summarise(Pisos = sum(Pisos, na.rm = T)) %>%
    ungroup() %>%
    left_join(info_districtes[,c(1:2)])
  
  hab_tur_ciutat <- hab_tur_df %>%
    group_by(Data_Referencia, Codi_Ciutat) %>%
    summarise(Pisos = sum(Pisos, na.rm = T)) %>%
    ungroup() %>%
    left_join(info_ciutat[,c(1:2)])
  
  ########
  
  #### Edat població ####
  
  pob_edat_df <- read_csv(file.path(here(), "data/L1/pad_mdb_edat-10.csv"))
  
  dimensio_edat <- data.frame(
    EDAT_10 = c(1:8),
    Desc_Valor_CA = c("0-9 anys",
                      "10-19 anys",
                      "20-29 anys",
                      "30-39 anys",
                      "40-49 anys",
                      "50-59 anys",
                      "60-69 anys",
                      ">70 anys"))
  
  pob_edat_barri <- pob_edat_df %>%
    group_by(Data_Referencia, Codi_Barri, EDAT_10) %>%
    summarise(Poblacio = sum(Poblacio, na.rm = T)) %>%
    ungroup(EDAT_10) %>%
    mutate(REF_Poblacio = sum(Poblacio, na.rm = T),
           Perc_Poblacio = Poblacio/REF_Poblacio) %>%
    ungroup() %>%
    left_join(info_barris[,c(1:2)]) %>%
    left_join(dimensio_edat)
  
  pob_edat_districte <- pob_edat_df %>%
    group_by(Data_Referencia, Codi_Districte, EDAT_10) %>%
    summarise(Poblacio = sum(Poblacio, na.rm = T)) %>%
    ungroup(EDAT_10) %>%
    mutate(REF_Poblacio = sum(Poblacio, na.rm = T),
           Perc_Poblacio = Poblacio/REF_Poblacio) %>%
    ungroup() %>%
    left_join(info_districtes[,c(1:2)]) %>%
    left_join(dimensio_edat)
  
  pob_edat_ciutat <- pob_edat_df %>%
    group_by(Data_Referencia, Codi_Ciutat, EDAT_10) %>%
    summarise(Poblacio = sum(Poblacio, na.rm = T)) %>%
    ungroup(EDAT_10) %>%
    mutate(REF_Poblacio = sum(Poblacio, na.rm = T),
           Perc_Poblacio = Poblacio/REF_Poblacio) %>%
    ungroup() %>%
    left_join(info_ciutat[,c(1:2)]) %>%
    left_join(dimensio_edat)
  
  ########
  
  #### Càlcul gentrificació
  
  df_gentrificacio <- read_csv(file.path(here(), "data/results/gentrificacio.csv"))
  
  df_gentrificacio <- df_gentrificacio %>%
    group_split(Data_Referencia, .keep = FALSE) # Afegir ".kepp = FALSE" si volem que "Data_Referencia" ja no hi aparegui
  
  names(df_gentrificacio) <- c(2020:2023)
  
  ########
  
  #Funció exportació highchart
  exportar_highchart <- function(x){
    hc_exporting(x,
      enabled = TRUE,
      buttons = list(
        contextButton = list(
          menuItems = list(
            list(
              text = "Descarregar PNG",
              onclick = JS("function () {
                  this.exportChartLocal({ type: 'image/png' }); }")
            ),
            list(
              text = "Descarregar JPEG",
              onclick = JS("function () {
                  this.exportChartLocal({ type: 'image/jpeg' }); }")
            ),
            list(
              text = "Descarregar PDF",
              onclick = JS("function () {
                  this.exportChartLocal({ type: 'application/pdf' }); }")
            ),
            list(
              text = "Descarregar SVG",
              onclick = JS("function () {
                  this.exportChartLocal({ type: 'image/svg+xml' }); }")
            )
          )
        )
      )
    )
    
  }
  
}))

