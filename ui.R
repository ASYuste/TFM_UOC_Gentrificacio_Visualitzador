# UI del visualitzador
# TFM - UOC
# Albert Salvador Yuste

ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "UOC - TFM"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Presentació", tabName = "dashboard", icon = icon("person-chalkboard")),
      menuItem("Anàlisi anual", tabName = "situacio_anual", icon = icon("layer-group"),
               menuSubItem("Lloguer", tabName = "lloguer", icon = icon("house")),
               menuSubItem("Nivell estudis", tabName = "estudis", icon = icon("graduation-cap")),
               menuSubItem("Tipus Domicilis", tabName = "domicilis", icon = icon("people-roof")),
               menuSubItem("Lloc naixement", tabName = "poborigen", icon = icon("earth-europe")),
               menuSubItem("Renda bruta", tabName = "renda", icon = icon("wallet")),
               menuSubItem("Habitatges turístics", tabName = "turistics", icon = icon("hotel")),
               menuSubItem("Grups d'edat", tabName = "grupedat", icon = icon("calendar-days"))
               ),
      menuItem("Evolució anual", tabName = "variacio_anual", icon = icon("chart-line"),
               menuSubItem("Lloguer", tabName = "var_lloguer", icon = icon("house")),
               menuSubItem("Nivell estudis", tabName = "var_estudis", icon = icon("graduation-cap")),
               menuSubItem("Tipus Domicilis", tabName = "var_domicilis", icon = icon("people-roof")),
               menuSubItem("Lloc naixement", tabName = "var_poborigen", icon = icon("earth-europe")),
               menuSubItem("Renda bruta", tabName = "var_renda", icon = icon("wallet")),
               menuSubItem("Grups d'edat", tabName = "var_grupedat", icon = icon("calendar-days"))
               ),
      menuItem("Càlcul gentrificació", tabName = "gentrificacio", icon = icon("dashboard"))
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
                    dimensions_domicili_new %>%
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
    conditionalPanel(
      condition = "input.tabs !== 'dashboard'",
      selectInput("nivell_territorial", "Nivell territorial:",
                  choices = c("Ciutat",
                              "Districte",
                              "Barri"),
                  selected = "Ciutat")
    ),
    conditionalPanel(
      condition = "input.tabs !== 'dashboard'",
      pickerInput("seleccio_territorial", "Selecciona:",
                  choices = c("Barcelona"),
                  selected = "Barcelona",
                  multiple = T,
                  options = list(`actions-box` = TRUE,
                                 `none-selected-text` = "Tots",
                                 `live-search` = TRUE,
                                 `size` = 10,
                                 `select-on-tab` = T)
                  )
    ),
    conditionalPanel(
      condition = "input.tabs !== 'dashboard'",
      leafletOutput("mapsidebar")
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML(
      'body, .content-wrapper, .right-side {
        height: 100%;
        background-color: #FDF8FF; /* Color de fons */
      }
      
      .capcalera { 
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
      .dt-center {text-align: center;}
      .dt-vertical-line-right {
        border-right: 2px solid black;
      }
      .dt-vertical-line-left {
        border-left: 2px solid black;
      }
    '))),
    tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").
        append(\'<span class="capcalera"> Anàlisi de la gentrificació a la ciutat de Barcelona</span>\');
      })
     ')),
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Presentació",
                    # background = "blue",
                    width = 12, #color = "purple",
                    solidHeader = TRUE,
                    p("Benvingut/da al visualitzador interactiu del projecte titulat
                      'Evolució de la gentrificació a la ciutat de Barcelona i anàlisi dels
                      factors que la caracteritzen'. Aquest 
                      projecte s'engloba dins del Treball Final de Màster del mateix títol, 
                      dins del màster de Ciència de Dades de la
                      Universitat Oberta de Catalunya (UOC) per al curs acadèmic 2023-2024."),
                    p("Aquesta web, tal i com se'n deriva del títol del projecte, persegueix l'objectiu de 
                      proporcionar elements gràfics interactuables i modificables que reflecteixin
                      la situació dels processos de gentrificació a Barcelona, identificant-ne els
                      factors que la provoquen, així com la seva evolució en el temps, i poder
                      determinar un nivell de gentrificació per als diferents barris de la ciutat."),
                    p("A mida que avanci per les diferents pestanyes observarà que a la columna esquerra
                      apareixeran selectors de diferent tipologia en què pot seleccionar una varietat
                      de variables adaptades al contingut que té davant (anys, grups de població, informació
                      a nivell de ciutat / districte / barri, consultes per a regions i grups concrets, etc.). També
                      observarà un mapa a la part inferior on pot seleccionar els districtes / barris dels
                      quals vulgui realitzar una consulta concreta. L'animem a que provi totes les combinacions
                      que li siguin d'interès!"),
                    p("Al llarg de les diferents pestanyes també podrà observar diferents mapes. Juntament
                      amb el que trobarà a la part inferior de la barra esquerra, tots són interactuables i
                      mútuament sincronitzats. Si vol seleccionar un barri / districte en concret,
                      pot seleccionar-lo dels llistat de l'esquerra o cliqui
                      en qualsevol dels mapes que tingui davant i es destacarà en tots ells. Per desmarcar un
                      barri / districte marcat, pot clicar-hi novament a sobre o desmarcar-lo del llistat de
                      la barra esquerra."),
                    p("En quant al contingut que hi trobarà, aquesta pàgina web està dividida en tres parts:"),
                    HTML("
                         <ul>
                          <li><b>Anàlisi anual:</b> La primera de les pestanyes que pot observar a l'esquerra 
                         d'aquest text conté informació anual d'un seguit de variables que s'han considerat influents
                         a l'hora de determinar els processos de gentrificació. Per a cada variable apareixeran
                         a la columna esquerra un seguit de selectors i filtres per tal de consultar els valors dels
                         diferents anys, consulta de conjunts de població concrets, granularitat segons si es
                         consulten valors de barri / districte / ciutat, etc.</li>
                         <li><b>Evolució anual:</b> Es segueixen observant la majoria de les variables de l'anterior
                         pestanya, però tenint en compte la seva variació respecte a alguns anys enrere. Igual
                         que en l'anterior cas, pot modificar de quin any vol realitzar la consulta i respecte
                         a quants anys enrere es vol comparar. El rang de dates màxim de consulta en aquesta 
                         pestanya està entre l'any 2016 i 2023, i la diferència d'anys a comparar és entre
                         1 i 5 anys de diferència.</li>
                         <li><b>Càlcul gentrificació:</b> La última de les parts presenta les representacions
                         gràfiques dels resultats dels nivells de gentrificació seguint dues metodologies diferents.
                         A diferència dels casos anteriors, la precisió dels càlculs es limita a valors de barris,
                         i el valor d'anys de comparació entre variables amb el pas del temps que s'ha establer per al
                         seu càlcul està fixat a 5 anys, per lo que els rangs de data de consulta està limitat
                         entre els anys 2020 i 2023. També pot observar la distribució de les puntuacions de les
                         diferents variables per barris i els valors que s'obtenen segons el mètode aplicat.
                         Finalment es pot observar en forma de taula els valors obtinguts.</li>
                         </ul>
                         <p>
                         <p>Per a qualsevol dubte no dubti a consultar a través del
                         següent <a href='mailto:asalvadory@uoc.edu'>enllaç</a>.
                         <p>
                         <p>Alhora, si desitja analitzar el codi amb el que s'ha realitzat aquesta interfície el podrà
                         trobar al següent projecte de
                         <a href='https://github.com/ASYuste/TFM_UOC_Gentrificacio_Visualitzador' target='_blank'>
                         Github</a>. A més, si vol consultar el projecte del qual se'n deriva tot el contingut
                         aquí disponible, pot trobar el projecte
                         original al següent enllaç de
                         <a href='https://github.com/ASYuste/TFM_UOC_Gentrificacio' target='_blank'>Github</a>.
                         <p>
                         
                         <p>Moltes gràcies per la seva atenció, i que gaudeixi de la visualització!
                         <p>
                         <p>
                         <div style='text-align: right;'<p>Albert Salvador Yuste
                         <p>
                         <img src='logo-UOC-2linies.png' alt='Logotip_UOC' width='400'></div>")
                )
              )
      ),
      tabItem(tabName = "lloguer",
              fluidRow(
                valueBoxOutput("lloguer_info_ciutat", width = 6),
                valueBoxOutput("lloguer_info_any", width = 6)
              ),
              fluidRow(
                column(
                  width = 6,
                  fluidRow(
                    box(title = "Distribució espacial del preu del lloguer",
                        background = "blue",
                        leafletOutput("map_lloguer")  %>%
                          shinycssloaders::withSpinner(),
                        width = 12, color = "purple",
                        class = "custom-box"
                    )
                  ),
                  fluidRow(
                    box(title = "Evolució anual del preu del lloguer",
                        background = "blue",
                        highchartOutput("evol_lloguer") %>%
                          shinycssloaders::withSpinner(),
                        width = 12, color = "purple",
                        class = "custom-box"
                    )
                  )
                ),
                column(
                  width = 6,
                  fluidRow(
                    box(title = "Classificació del preu del lloguer per zones",
                        background = "blue",
                        uiOutput("hchart_plot_lloguer") %>%
                          shinycssloaders::withSpinner(),
                        width = 12, color = "purple",
                        class = "custom-box"
                    )
                  )
                )
              ) 
      ),
      tabItem(tabName = "estudis",
              fluidRow(
                valueBoxOutput("estudis_info_ciutat", width = 6),
                valueBoxOutput("estudis_info_any", width = 6)
              ),
              fluidRow(
                column(
                  width = 6,
                  fluidRow(
                    box(title = "Distribució espacial dels nivells d'estudis",
                        background = "blue",
                        leafletOutput("map_estudis") %>%
                          shinycssloaders::withSpinner(),
                        width = 12, color = "purple",
                        class = "custom-box"
                    )
                  ),
                  fluidRow(
                    box(title = "Evolució anual dels nivells d'estudis",
                        background = "blue",
                        highchartOutput("evol_estudis") %>%
                          shinycssloaders::withSpinner(),
                        width = 12, color = "purple",
                        class = "custom-box"
                    )
                  )
                ),
                column(
                  width = 6,
                  fluidRow(
                    box(title = "Nivell d'estudi per zones",
                        background = "blue",
                        uiOutput("hchart_plot_estudis") %>%
                          shinycssloaders::withSpinner(),
                        width = 12, color = "purple",
                        class = "custom-box"
                    )
                  )
                )
                
              )
      ),
      tabItem(tabName = "domicilis",
              fluidRow(
                valueBoxOutput("domicilis_info_ciutat", width = 6),
                valueBoxOutput("domicilis_info_any", width = 6)
              ),
              fluidRow(
                column(
                  width = 6,
                  fluidRow(
                    box(title = "Distribució espacial dels tipus de domicili",
                        background = "blue",
                        leafletOutput("map_domicilis") %>%
                          shinycssloaders::withSpinner(),
                        width = 12, color = "purple",
                        class = "custom-box"
                    )
                  ),
                  fluidRow(
                    box(title = "Evolució anual dels tipus de domicilis",
                        background = "blue",
                        highchartOutput("evol_domicilis") %>%
                          shinycssloaders::withSpinner(),
                        width = 12, color = "purple",
                        class = "custom-box"
                    )
                  )
                ),
                column(
                  width = 6,
                  fluidRow(
                    box(title = "Tipus de domicilis per zones",
                        background = "blue",
                        uiOutput("hchart_plot_domicilis") %>%
                          shinycssloaders::withSpinner(),
                        width = 12, color = "purple",
                        class = "custom-box"
                    )
                  )
                )
                
              )
      ),
      tabItem(tabName = "poborigen",
              fluidRow(
                valueBoxOutput("poborigen_info_ciutat", width = 6),
                valueBoxOutput("poborigen_info_any", width = 6)
              ),
              fluidRow(
                column(
                  width = 6,
                  fluidRow(
                    box(title = "Distribució espacial del llocs de naixement",
                        background = "blue",
                        leafletOutput("map_origen") %>%
                          shinycssloaders::withSpinner(),
                        width = 12, color = "purple",
                        class = "custom-box",
                        footer = div("Nota: No es tenen en compte els nascuts a Espanya",
                                     style = "text-align: right; color: black;") 
                    )
                  ),
                  fluidRow(
                    box(title = "Evolució anual dels llocs de naixement",
                        background = "blue",
                        highchartOutput("evol_origen") %>%
                          shinycssloaders::withSpinner(),
                        width = 12, color = "purple",
                        class = "custom-box",
                        footer = div("Nota: No es tenen en compte els nascuts a Espanya",
                                     style = "text-align: right; color: black;")
                    )
                  )
                ),
                column(
                  width = 6,
                  fluidRow(
                    box(title = "Regió de naixement per zones",
                        background = "blue",
                        uiOutput("hchart_plot_origen") %>%
                          shinycssloaders::withSpinner(),
                        width = 12, color = "purple",
                        class = "custom-box",
                        footer = div("Nota: No es tenen en compte els nascuts a Espanya",
                                     style = "text-align: right; color: black;")
                    )
                  )
                )
                
              )
      ),
      tabItem(tabName = "renda",
              fluidRow(
                valueBoxOutput("renda_info_ciutat", width = 6),
                valueBoxOutput("renda_info_any", width = 6)
              ),
              fluidRow(
                column(
                  width = 6,
                  fluidRow(
                    box(title = "Distribució espacial de les rendes",
                        background = "blue",
                        leafletOutput("map_renda") %>%
                          shinycssloaders::withSpinner(),
                        width = 12, color = "purple",
                        class = "custom-box"
                    )
                  ),
                  fluidRow(
                    box(title = "Evolució anual de les rendes",
                        background = "blue",
                        highchartOutput("evol_renda") %>%
                          shinycssloaders::withSpinner(),
                        width = 12, color = "purple",
                        class = "custom-box"
                    )
                  )
                ),
                column(
                  width = 6,
                  fluidRow(
                    box(title = "Classificació de les rendes per zones",
                        background = "blue",
                        uiOutput("hchart_plot_renda") %>%
                          shinycssloaders::withSpinner(),
                        width = 12, color = "purple",
                        class = "custom-box"
                    )
                  )
                )
                
              )
      ),
      tabItem(tabName = "turistics",
              fluidRow(
                valueBoxOutput("turistics_info_ciutat", width = 6),
                valueBoxOutput("turistics_info_any", width = 6)
              ),
              fluidRow(
                box(title = "Classificació de pisos turístics per zones",
                    background = "blue",
                    highchartOutput("plot_turisme") %>%
                      shinycssloaders::withSpinner(),
                    width = 12, color = "purple",
                    class = "custom-box"
                )
              ),
              fluidRow(
                box(title = "Distribució espacial de pisos turístics",
                    background = "blue",
                    leafletOutput("map_turisme") %>%
                      shinycssloaders::withSpinner(),
                    width = 12, color = "purple",
                    class = "custom-box"
                )
              )
      ),
      tabItem(tabName = "grupedat",
              fluidRow(
                valueBoxOutput("grupedat_info_ciutat", width = 6),
                valueBoxOutput("grupedat_info_any", width = 6)
              ),
              fluidRow(
                column(
                  width = 6,
                  fluidRow(
                    box(title = "Distribució espacial dels grups d'edat",
                        background = "blue",
                        leafletOutput("map_edat") %>%
                          shinycssloaders::withSpinner(),
                        width = 12, color = "purple",
                        class = "custom-box"
                    )
                  ),
                  fluidRow(
                    box(title = "Evolució anual dels grups d'edat",
                        background = "blue",
                        highchartOutput("evol_edat") %>%
                          shinycssloaders::withSpinner(),
                        width = 12, color = "purple",
                        class = "custom-box"
                    )
                  )
                ),
                column(
                  width = 6,
                  fluidRow(
                    box(title = "Grups d'edat per zones",
                        background = "blue",
                        uiOutput("hchart_plot_edat") %>%
                          shinycssloaders::withSpinner(),
                        width = 12, color = "purple",
                        class = "custom-box"
                    )
                  )
                )
                
              )
      ),
      tabItem(tabName = "var_lloguer",
              fluidRow(
                valueBoxOutput("var_lloguer_info_ciutat", width = 6),
                valueBoxOutput("var_lloguer_info_any", width = 6)
              ),
              fluidRow(
                box(title = "Variació lloguer (€/m2)",
                    background = "blue",
                    highchartOutput("plot_var_lloguer") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, color = "purple",
                    class = "custom-box"
                ),
                box(title = "Variació lloguer (%)",
                    background = "blue",
                    highchartOutput("plot_perc_lloguer") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, color = "purple",
                    class = "custom-box"
                )
              ),
              fluidRow(
                box(title = "Distribució espacial variació (€/m2)",
                    background = "blue",
                    leafletOutput("map_var_lloguer") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, color = "purple",
                    class = "custom-box"
                ),
                box(title = "Distribució espacial variació percentual (%)",
                    background = "blue",
                    leafletOutput("map_perc_lloguer") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, color = "purple",
                    class = "custom-box"
                )
              )
      ),
      tabItem(tabName = "var_estudis",
              fluidRow(
                valueBoxOutput("var_estudis_info_ciutat", width = 6),
                valueBoxOutput("var_estudis_info_any", width = 6)
              ),
              fluidRow(
                box(title = "Variació nivell d'estudis",
                    background = "blue",
                    highchartOutput("plot_var_estudis") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, color = "purple",
                    class = "custom-box"
                ),
                box(title = "Variació percentual nivell d'estudis (%)",
                    background = "blue",
                    highchartOutput("plot_perc_estudis") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, color = "purple",
                    class = "custom-box"
                )
              ),
              fluidRow(
                box(title = "Distribució espacial variació",
                    background = "blue",
                    leafletOutput("map_var_estudis") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, color = "purple",
                    class = "custom-box"
                ),
                box(title = "Distribució espacial variació percentual (%)",
                    background = "blue",
                    leafletOutput("map_perc_estudis") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, color = "purple",
                    class = "custom-box"
                )
              )
      ),
      tabItem(tabName = "var_domicilis",
              fluidRow(
                valueBoxOutput("var_domicilis_info_ciutat", width = 6),
                valueBoxOutput("var_domicilis_info_any", width = 6)
              ),
              fluidRow(
                box(title = "Variació tipus de domicilis",
                    background = "blue",
                    highchartOutput("plot_var_domicilis") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, color = "purple",
                    class = "custom-box"
                ),
                box(title = "Variació percentual tipus de domicilis (%)",
                    background = "blue",
                    highchartOutput("plot_perc_domicilis") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, color = "purple",
                    class = "custom-box"
                )
              ),
              fluidRow(
                box(title = "Distribució espacial variació",
                    background = "blue",
                    leafletOutput("map_var_domicilis") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, color = "purple",
                    class = "custom-box"
                ),
                box(title = "Distribució espacial variació percentual (%)",
                    background = "blue",
                    leafletOutput("map_perc_domicilis") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, color = "purple",
                    class = "custom-box"
                )
              )
      ),
      tabItem(tabName = "var_poborigen",
              fluidRow(
                valueBoxOutput("var_poborigen_info_ciutat", width = 6),
                valueBoxOutput("var_poborigen_info_any", width = 6)
              ),
              fluidRow(
                box(title = "Variació lloc de naixement",
                    background = "blue",
                    highchartOutput("plot_var_poborigen") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, color = "purple",
                    class = "custom-box",
                    footer = div("Nota: No es tenen en compte els nascuts a Espanya",
                                 style = "text-align: right; color: black;")
                ),
                box(title = "Variació percentual lloc de naixement (%)",
                    background = "blue",
                    highchartOutput("plot_perc_poborigen") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, color = "purple",
                    class = "custom-box",
                    footer = div("Nota: No es tenen en compte els nascuts a Espanya",
                                 style = "text-align: right; color: black;")
                )
              ),
              fluidRow(
                box(title = "Distribució espacial variació",
                    background = "blue",
                    leafletOutput("map_var_poborigen") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, color = "purple",
                    class = "custom-box",
                    footer = div("Nota: No es tenen en compte els nascuts a Espanya",
                                 style = "text-align: right; color: black;")
                ),
                box(title = "Distribució espacial variació percentual (%)",
                    background = "blue",
                    leafletOutput("map_perc_poborigen") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, color = "purple",
                    class = "custom-box",
                    footer = div("Nota: No es tenen en compte els nascuts a Espanya",
                                 style = "text-align: right; color: black;")
                )
              )
      ),
      tabItem(tabName = "var_renda",
              fluidRow(
                valueBoxOutput("var_renda_info_ciutat", width = 6),
                valueBoxOutput("var_renda_info_any", width = 6)
              ),
              fluidRow(
                box(title = "Variació renda anual (€)",
                    background = "blue",
                    highchartOutput("plot_var_renda") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, color = "purple",
                    class = "custom-box"
                ),
                box(title = "Variació renda anual (%)",
                    background = "blue",
                    highchartOutput("plot_perc_renda") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, color = "purple",
                    class = "custom-box"
                )
              ),
              fluidRow(
                box(title = "Distribució espacial variació (€)",
                    background = "blue",
                    leafletOutput("map_var_renda") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, color = "purple",
                    class = "custom-box"
                ),
                box(title = "Distribució espacial variació percentual (%)",
                    background = "blue",
                    leafletOutput("map_perc_renda") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, color = "purple",
                    class = "custom-box"
                )
              )
      ),
      tabItem(tabName = "var_grupedat",
              fluidRow(
                valueBoxOutput("var_grupedat_info_ciutat", width = 6),
                valueBoxOutput("var_grupedat_info_any", width = 6)
              ),
              fluidRow(
                box(title = "Variació grups d'edat",
                    background = "blue",
                    highchartOutput("plot_var_grupedat") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, color = "purple",
                    class = "custom-box"
                ),
                box(title = "Variació percentual grups d'edat (%)",
                    background = "blue",
                    highchartOutput("plot_perc_grupedat") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, color = "purple",
                    class = "custom-box"
                )
              ),
              fluidRow(
                box(title = "Distribució espacial variació",
                    background = "blue",
                    leafletOutput("map_var_grupedat") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, color = "purple",
                    class = "custom-box"
                ),
                box(title = "Distribució espacial variació percentual (%)",
                    background = "blue",
                    leafletOutput("map_perc_grupedat") %>%
                      shinycssloaders::withSpinner(),
                    width = 6, color = "purple",
                    class = "custom-box"
                )
              )
      ),
      tabItem(tabName = "gentrificacio",
              fluidRow(
                valueBoxOutput("gentrificacio_info_ciutat", width = 6),
                valueBoxOutput("gentrificacio_info_any", width = 6)
              ),
              fluidRow(
                box(
                  title = "Nivell de gentrificació per K-Means",
                  background = "blue",
                  leafletOutput("mapa_gentrificacio_cluster")  %>%
                    shinycssloaders::withSpinner(),
                  width = 6, color = "purple",
                  class = "custom-box"
                ),
                box(title = "Nivell de gentrificació per puntuació de variables (índex de gentrificació)",
                    background = "blue",
                    leafletOutput("mapa_gentrificacio_quantils")  %>%
                      shinycssloaders::withSpinner(),
                    width = 6, color = "purple",
                    class = "custom-box"
                )
              ),
              fluidRow(
                width = 12,
                box(
                  title = "Distribució de les puntuacions de les variables",
                  fluidRow(
                    column(width = 6),
                    column(
                      width = 3,
                      selectInput("puntuacio_gen", tags$span(style="color: black;","Variable:"),
                                  choices = c("Renda" = "Punt_Renda",
                                              "Pisos turístics" = "Punt_Pisos",
                                              "Domicilis unifamiliars" = "Punt_Domicilis",
                                              "Edat" = "Punt_Edat",
                                              "Lloc naixement" = "Punt_Regio",
                                              "Població universitària" = "Punt_Uni",
                                              "Lloguer" = "Punt_Lloguer",
                                              "Gentrificació" = "Punt_Gentrificacio")
                      )
                    ),
                    column(
                      width = 3,
                      selectInput("metode_estudi", tags$span(style="color: black;","Mètode:"),
                                  choices = c("K-Means",
                                              "Índex de gentrificació")
                      )
                    )
                  ),
                  background = "blue",
                  fluidRow(
                    width = 12,
                    column(
                      width = 6,
                      leafletOutput("mapa_gentrificacio_puntuacions")  %>%
                        shinycssloaders::withSpinner()
                    ),
                    column(
                      width = 6,
                      highchartOutput("plot_gentrificacio_classificacions")  %>%
                        shinycssloaders::withSpinner()
                    )
                  ),
                  width = 12, color = "purple",
                  class = "custom-box"
                )
              ),
              fluidRow(
                width = 12,
                box(title = "Taula comparativa entre mètodes (K-Means vs. Índex de gentrificació)",
                    background = "blue",
                    DTOutput("taula_comparativa") %>% withSpinner(),
                    width = 12, color = "purple",
                    class = "custom-box",
                    footer = div("Nota: En cas de discrepància amb el barris assenyalats al mapa,
                                   aquest últim prevaleix sobre la taula.",
                                 style = "text-align: right; color: black;")
                )
                
              )
            )
      )
    )
  )
