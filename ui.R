
# Define UI for application
ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "UOC - TFM"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Situació anual", tabName = "situacio_anual",
               menuSubItem("Lloguer", tabName = "lloguer", icon = icon("house")),
               menuSubItem("Nivell estudis", tabName = "estudis", icon = icon("graduation-cap")),
               menuSubItem("Tipus Domicilis", tabName = "domicilis", icon = icon("people-roof")),
               menuSubItem("Lloc naixement", tabName = "poborigen", icon = icon("earth-europe")),
               menuSubItem("Renda bruta", tabName = "renda", icon = icon("wallet")),
               menuSubItem("Habitatges turístics", tabName = "turistics", icon = icon("hotel")),
               menuSubItem("Grups d'edat", tabName = "grupedat", icon = icon("calendar-days"))
               ),
      menuItem("Variació anual", tabName = "variacio_anual",
               menuSubItem("Lloguer", tabName = "var_lloguer", icon = icon("house")),
               menuSubItem("Nivell estudis", tabName = "var_estudis", icon = icon("graduation-cap")),
               menuSubItem("Tipus Domicilis", tabName = "var_domicilis", icon = icon("people-roof")),
               menuSubItem("Lloc naixement", tabName = "var_poborigen", icon = icon("earth-europe")),
               menuSubItem("Renda bruta", tabName = "var_renda", icon = icon("wallet")),
               menuSubItem("Grups d'edat", tabName = "var_grupedat", icon = icon("calendar-days"))
               ),
      menuItem("Càlcul gentrificació", tabName = "gentrificacio")
    ),
    
    conditionalPanel(
      condition = 
      "input.tabs === 'estudis' || input.tabs === 'domicilis'
      || input.tabs === 'poborigen' || input.tabs === 'grupedat'",
      radioButtons("percentatge", "Format",
                   choices = c("%", "Absolut"),
                   selected = "%",
                   inline = T)
    ),
    conditionalPanel(
      condition = "input.tabs !== 'dashboard'",
      sliderInput("any_selector", "Any:",
                  min = 1997, max = 2023, value = 1997, step = 1, sep = "")
    ),
    conditionalPanel(
      condition = 
        "input.tabs === 'estudis' || input.tabs === 'var_estudis'",
      pickerInput("nivell_estudi_selector", "Nivell d'estudis:",
                  choices = c(
                    dimensions_pad %>%
                      filter(Desc_Dimensio == "NIV_EDUCA_esta") %>%
                      pull(Desc_Valor_CA)),
                  multiple = T,
                  options = list(`actions-box` = TRUE,
                                 `none-selected-text` = "Tots"))
    ),
    conditionalPanel(
      condition = 
        "input.tabs === 'domicilis' || input.tabs === 'var_domicilis'",
      pickerInput("tipus_domicili_selector", "Tipus de domicili:",
                  choices = c(
                    dimensions_pad %>%
                      filter(Desc_Dimensio == "TIPUS_DOMICILI") %>%
                      pull(Desc_Valor_CA)),
                  multiple = T,
                  options = list(`actions-box` = TRUE,
                                 `none-selected-text` = "Tots"))
    ),
    conditionalPanel(
      condition = 
        "input.tabs === 'poborigen' || input.tabs === 'var_poborigen'",
      pickerInput("origen_poblacio", "Regió de naixement:",
                  choices = c(unique(dimensio_pais$Desc_Valor_CA)),
                  multiple = T,
                  options = list(`actions-box` = TRUE,
                                 `none-selected-text` = "Tots"))
    ),
    conditionalPanel(
      condition = 
        "input.tabs === 'grupedat' || input.tabs === 'var_grupedat'",
      pickerInput("grup_edat", "Grup d'edat:",
                  choices = c(unique(dimensio_edat$Desc_Valor_CA)),
                  multiple = T,
                  options = list(`actions-box` = TRUE,
                                 `none-selected-text` = "Tots"))
    ),
    conditionalPanel(
      condition = "input.tabs === 'var_lloguer' || input.tabs === 'var_estudis' || input.tabs === 'var_domicilis'
      || input.tabs === 'var_poborigen' || input.tabs === 'var_renda' || input.tabs === 'var_grupedat'",
      sliderInput("anys_variacions", "Variació anys:",
                  min = 1, max = 5, value = 1),
    ),
    selectInput("nivell_territorial", "Nivell territorial:",
                choices = c("Ciutat",
                            "Districte",
                            "Barri"),
                selected = "Ciutat"),
    pickerInput("seleccio_territorial", "Selecciona:",
                choices = c("Barcelona"),
                selected = "Barcelona",
                multiple = T,
                options = list(`actions-box` = TRUE,
                               `none-selected-text` = "Tots",
                               `live-search` = TRUE,
                               `size` = 10,
                               `select-on-tab` = T)
                ),
    leafletOutput("mapsidebar")
  ),
  dashboardBody(
    tags$head(tags$style(HTML(
      '.capcalera { 
        font-size: 20px;
        line-height: 50px;
        text-align: left;
        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        padding: 0 15px;
        overflow: hidden;
        color: white;
      }
      .custom-box {
          background-color: #F8F8F8;
          border: 3px solid #615ca6; 
      }
    '))),
    tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").
        append(\'<span class="capcalera"> Anàlisi de la gentrificació a la ciutat de Barcelona</span>\');
      })
     ')),
    tabItems(
      # Primer tab: Dashboard
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Resum",
                    "Aquí hi hauria el resum del teu dashboard."
                )
              )
      ),
      # Segon tab: Gràfics
      tabItem(tabName = "lloguer",
              fluidRow(
                column(
                  width = 6,
                  fluidRow(
                    box(leafletOutput("map_lloguer")  %>%
                          shinycssloaders::withSpinner(),
                        width = 12, status = "primary",
                        class = "custom-box"
                    )
                  ),
                  fluidRow(
                    box(highchartOutput("evol_lloguer") %>%
                          shinycssloaders::withSpinner(),
                        width = 12, status = "primary",
                        class = "custom-box"
                    )
                  )
                ),
                column(
                  width = 6,
                  fluidRow(
                    box(uiOutput("hchart_plot_lloguer") %>%
                          shinycssloaders::withSpinner(),
                        width = 12, status = "primary",
                        class = "custom-box"
                    )
                  )
                )
              ) 
      ),
      # Segon tab: Gràfics
      tabItem(tabName = "estudis",
              fluidRow(
                column(
                  width = 6,
                  fluidRow(
                    box(leafletOutput("map_estudis") %>%
                          shinycssloaders::withSpinner(),
                        width = 12, status = "primary",
                        class = "custom-box"
                    )
                  ),
                  fluidRow(
                    box(highchartOutput("evol_estudis") %>%
                          shinycssloaders::withSpinner(),
                        width = 12, status = "primary",
                        class = "custom-box"
                    )
                  )
                ),
                column(
                  width = 6,
                  fluidRow(
                    box(uiOutput("hchart_plot_estudis") %>%
                          shinycssloaders::withSpinner(),
                        width = 12, status = "primary",
                        class = "custom-box"
                    )
                  )
                )
                
              )
      ),
      tabItem(tabName = "domicilis",
              fluidRow(
                column(
                  width = 6,
                  fluidRow(
                    box(leafletOutput("map_domicilis") %>%
                          shinycssloaders::withSpinner(),
                        width = 12, status = "primary",
                        class = "custom-box"
                    )
                  ),
                  fluidRow(
                    box(highchartOutput("evol_domicilis") %>%
                          shinycssloaders::withSpinner(),
                        width = 12, status = "primary",
                        class = "custom-box"
                    )
                  )
                ),
                column(
                  width = 6,
                  fluidRow(
                    box(uiOutput("hchart_plot_domicilis") %>%
                          shinycssloaders::withSpinner(),
                        width = 12, status = "primary",
                        class = "custom-box"
                    )
                  )
                )
                
              )
      ),
      tabItem(tabName = "poborigen",
              fluidRow(
                column(
                  width = 6,
                  fluidRow(
                    box(leafletOutput("map_origen") %>%
                          shinycssloaders::withSpinner(),
                        width = 12, status = "primary",
                        class = "custom-box"
                    )
                  ),
                  fluidRow(
                    box(highchartOutput("evol_origen") %>%
                          shinycssloaders::withSpinner(),
                        width = 12, status = "primary",
                        class = "custom-box"
                    )
                  )
                ),
                column(
                  width = 6,
                  fluidRow(
                    box(uiOutput("hchart_plot_origen") %>%
                          shinycssloaders::withSpinner(),
                        width = 12, status = "primary",
                        class = "custom-box"
                    )
                  )
                )
                
              )
      ),
      tabItem(tabName = "renda",
              fluidRow(
                column(
                  width = 6,
                  fluidRow(
                    box(leafletOutput("map_renda") %>%
                          shinycssloaders::withSpinner(),
                        width = 12, status = "primary",
                        class = "custom-box"
                    )
                  ),
                  fluidRow(
                    box(highchartOutput("evol_renda") %>%
                          shinycssloaders::withSpinner(),
                        width = 12, status = "primary",
                        class = "custom-box"
                    )
                  )
                ),
                column(
                  width = 6,
                  fluidRow(
                    box(uiOutput("hchart_plot_renda") %>%
                          shinycssloaders::withSpinner(),
                        width = 12, status = "primary",
                        class = "custom-box"
                    )
                  )
                )
                
              )
      ),
      tabItem(tabName = "turistics",
              fluidRow(
                box(highchartOutput("plot_turisme") %>%
                      shinycssloaders::withSpinner(),
                    width = 12, status = "primary",
                    class = "custom-box"
                )
              ),
              fluidRow(
                box(leafletOutput("map_turisme") %>%
                      shinycssloaders::withSpinner(),
                    width = 12, status = "primary",
                    class = "custom-box"
                )
              )
      ),
      tabItem(tabName = "grupedat",
              fluidRow(
                column(
                  width = 6,
                  fluidRow(
                    box(leafletOutput("map_edat") %>%
                          shinycssloaders::withSpinner(),
                        width = 12, status = "primary",
                        class = "custom-box"
                    )
                  ),
                  fluidRow(
                    box(highchartOutput("evol_edat") %>%
                          shinycssloaders::withSpinner(),
                        width = 12, status = "primary",
                        class = "custom-box"
                    )
                  )
                ),
                column(
                  width = 6,
                  fluidRow(
                    box(uiOutput("hchart_plot_edat") %>%
                          shinycssloaders::withSpinner(),
                        width = 12, status = "primary",
                        class = "custom-box"
                    )
                  )
                )
                
              )
      ),
      tabItem(tabName = "var_lloguer",
              fluidRow(
                box(highchartOutput("plot_var_lloguer") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, status = "primary",
                    class = "custom-box"
                ),
                box(highchartOutput("plot_perc_lloguer") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, status = "primary",
                    class = "custom-box"
                )
              ),
              fluidRow(
                box(leafletOutput("map_var_lloguer") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, status = "primary",
                    class = "custom-box"
                ),
                box(leafletOutput("map_perc_lloguer") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, status = "primary",
                    class = "custom-box"
                )
              )
      ),
      tabItem(tabName = "var_estudis",
              fluidRow(
                box(highchartOutput("plot_var_estudis") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, status = "primary",
                    class = "custom-box"
                ),
                box(highchartOutput("plot_perc_estudis") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, status = "primary",
                    class = "custom-box"
                )
              ),
              fluidRow(
                box(leafletOutput("map_var_estudis") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, status = "primary",
                    class = "custom-box"
                ),
                box(leafletOutput("map_perc_estudis") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, status = "primary",
                    class = "custom-box"
                )
              )
      ),
      tabItem(tabName = "var_domicilis",
              fluidRow(
                box(highchartOutput("plot_var_domicilis") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, status = "primary",
                    class = "custom-box"
                ),
                box(highchartOutput("plot_perc_domicilis") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, status = "primary",
                    class = "custom-box"
                )
              ),
              fluidRow(
                box(leafletOutput("map_var_domicilis") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, status = "primary",
                    class = "custom-box"
                ),
                box(leafletOutput("map_perc_domicilis") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, status = "primary",
                    class = "custom-box"
                )
              )
      ),
      tabItem(tabName = "var_poborigen",
              fluidRow(
                box(highchartOutput("plot_var_poborigen") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, status = "primary",
                    class = "custom-box"
                ),
                box(highchartOutput("plot_perc_poborigen") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, status = "primary",
                    class = "custom-box"
                )
              ),
              fluidRow(
                box(leafletOutput("map_var_poborigen") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, status = "primary",
                    class = "custom-box"
                ),
                box(leafletOutput("map_perc_poborigen") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, status = "primary",
                    class = "custom-box"
                )
              )
      ),
      tabItem(tabName = "var_renda",
              fluidRow(
                box(highchartOutput("plot_var_renda") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, status = "primary",
                    class = "custom-box"
                ),
                box(highchartOutput("plot_perc_renda") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, status = "primary",
                    class = "custom-box"
                )
              ),
              fluidRow(
                box(leafletOutput("map_var_renda") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, status = "primary",
                    class = "custom-box"
                ),
                box(leafletOutput("map_perc_renda") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, status = "primary",
                    class = "custom-box"
                )
              )
      ),
      tabItem(tabName = "var_grupedat",
              fluidRow(
                box(highchartOutput("plot_var_grupedat") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, status = "primary",
                    class = "custom-box"
                ),
                box(highchartOutput("plot_perc_grupedat") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, status = "primary",
                    class = "custom-box"
                )
              ),
              fluidRow(
                box(leafletOutput("map_var_grupedat") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, status = "primary",
                    class = "custom-box"
                ),
                box(leafletOutput("map_perc_grupedat") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, status = "primary",
                    class = "custom-box"
                )
              )
      ),
      tabItem(tabName = "gentrificacio",
              column(
                width = 6,
                fluidRow(
                  box(leafletOutput("mapa_gentrificacio_cluster")  %>%
                        shinycssloaders::withSpinner(),
                      width = 12, status = "primary",
                      class = "custom-box"
                  ) 
                ),
                fluidRow(
                  box(leafletOutput("mapa_gentrificacio_quantils")  %>%
                        shinycssloaders::withSpinner(),
                      width = 12, status = "primary",
                      class = "custom-box"
                  )
                )
              ),
              column(
                width = 6,
                fluidRow(
                  box(DTOutput("taula_comparativa") %>%
                        shinycssloaders::withSpinner(),
                      width = 12, status = "primary",
                      class = "custom-box"
                  )
                )
              )
              
      )
    )
  )
)
