VPN_NW <- tabItem(tabName = "VPN_NW",
                            sidebarPanel(width = 4,
                                         column(12,
                                                shiny::hr(),
                                                div(title = "", 
                                                    style = "margin-top: 10px; margin-bottom: 20px;",
                                                    radioGroupButtons("id_tipo_campo_NW2",
                                                                      label= HTML("<li>  Seleccione el tipo de Campo de interes </li>"),
                                                                      choices = c("RONDON" = 'CANO_RONDON',
                                                                                  "CARICARE" = 'CARICARE'),
                                                                      checkIcon = list(yes = icon("check")),
                                                                      select = 'CARICARE' ,
                                                                      justified = TRUE
                                                    )),
                                                
                                                tags$script("$(\"input:radio[name = 'id_tipo_campo_NW2'][value = 'CANO_RONDON']\").parent().css('background-color', '#CDC673');"),
                                                tags$script("$(\"input:radio[name = 'id_tipo_campo_NW2'][value = 'CARICARE']\").parent().css('background-color', '#B4CDCD');"),
                                                
                                                
                                         ),
                                         
                                         
                                         
                                         column(12,
                                                shiny::hr(),
                                                div(title = "",
                                                    p(tags$b(HTML("<li> Seleccione el tipo de pozo  analizar </li>")))),
                                                
                                                
                                                div(title = "", 
                                                    awesomeCheckbox(inputId = "v_dist_vpn_nw", label = "Pozo", value = FALSE,status = "success")),
                                                
                                                conditionalPanel(condition = "input.v_dist_vpn_nw == true",
                                                                 selectInput(inputId = "tipo_well_NW2", label = strong(""),
                                                                             choices = "",
                                                                             selected = "")
                                                                 
                                                )
                                                
                                                
                                         ),
                                         
                                         column(12,
                                                shiny::hr(),
                                                div(title = "",
                                                    p(tags$b(HTML("<li> Seleccione el tipo de Brent a sensibilizar </li>")))),
                                                
                                                
                                                div(title = "", 
                                                    awesomeCheckbox(inputId = "v_dist_brent_vpn_p_nw2", label = "Brent", value = FALSE, status = "success")),
                                                
                                                tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: green}")),
                                                
                                                
                                                conditionalPanel(condition = "input.v_dist_brent_vpn_p_nw2 == true",
                                                                 sliderInput(inputId ="id_brent_nw3",label = strong(""), 50, 90, 60, step = 10),
                                                                 HTML("")
                                                                 
                                                )
                                                
                                                
                                         ),
                                         
                                         column(12,
                                                shiny::hr(),
                                                div(title = "",
                                                    p(tags$b(HTML("<li> Seleccione el porcentaje de descuento en capex </li>")))),
                                                
                                                
                                                div(title = "",
                                                    awesomeCheckbox(inputId = "v_dist_PD_vpn_p_nw2", label = "P de Descuento", value = FALSE, status = "success")),
                                                
                                                tags$style(HTML(".js-irs-1 .irs-single,.js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: green}")),
                                                
                                                
                                                conditionalPanel(condition = "input.v_dist_PD_vpn_p_nw2 == true",
                                                                 sliderInput(inputId ="id_PD_nw3",label = strong(""), -0.3, 0.3, 0.0, step = 0.1),
                                                                 HTML("")
                                                                 
                                                )
                                                
                                                
                                         ),
                                         column(12,
                                                shiny::hr(),
                                                conditionalPanel(condition = "input.v_dist_vpn_nw == true",
                                                                 div(title = "",
                                                                     style = "margin-top: 10px; margin-bottom: 20px;",
                                                                     radioGroupButtons("id_FC_nw3",
                                                                                       label= HTML("<li> Seleccione  analisis desea explorar </li>"),
                                                                                       choices = c("Distibucion Opex" = 'D_opex',
                                                                                                   "FC en Tiempo" = 'fc_t'),
                                                                                       checkIcon = list(yes = icon("check")),
                                                                                       select = 'fc_t' ,
                                                                                       justified = TRUE
                                                                     )),
                                                                 
                                                                 tags$script("$(\"input:radio[name = 'id_FC_nw3'][value = 'D_opex']\").parent().css('background-color', '#F7DB17');"),
                                                                 tags$script("$(\"input:radio[name = 'id_FC_nw3'][value = 'fc_t']\").parent().css('background-color', '#CCD32A');")
                                                ) 
                                         ),
                                         
                                         column(12,
                                                shiny::hr(),
                                                div(title = "",
                                                    style = "margin-top: 10px; margin-bottom: 20px;",
                                                    radioGroupButtons("id_socio_nw3",
                                                                      label= HTML("<li> Seleccione el tipo a evaluar </li>"),
                                                                      choices = c("Sin Socio" = 'sin_socio',
                                                                                  "Con socio" = 'con_socio'),
                                                                      checkIcon = list(yes = icon("check")),
                                                                      select = 'con_socio' ,
                                                                      justified = TRUE
                                                    )),
                                                
                                                tags$script("$(\"input:radio[name = 'id_socio_nw3'][value = 'sin_Socio']\").parent().css('background-color', '#CDC673');"),
                                                tags$script("$(\"input:radio[name = 'id_socio_nw3'][value = 'con_socio']\").parent().css('background-color', '#B4CDCD');"),
                                                
                                         ),
                                         
                                         column(12,
                                                shiny::hr(),
                                                div(title = "",
                                                    style = "margin-top: 10px; margin-bottom: 20px;", 
                                                    radioGroupButtons("id_tipo_costo_nw3", 
                                                                      label= HTML("<li> Seleccione el tipo de Costo de  interes </li>"), 
                                                                      choices = c("Pesimista" = 'pesimista',
                                                                                  "Probable" = 'probable',
                                                                                  "Optimista" = 'optimista'),
                                                                      checkIcon = list(yes = icon("check")),
                                                                      select = 'probable' ,
                                                                      justified = TRUE
                                                    )),
                                                
                                                
                                                
                                                tags$script("$(\"input:radio[name = 'id_tipo_costo_nw3'][value = 'pesimista']\").parent().css('background-color', '#9BCD9B');"),
                                                tags$script("$(\"input:radio[name = 'id_tipo_costo_nw3'][value = 'probable']\").parent().css('background-color', '#CDC673');"),
                                                tags$script("$(\"input:radio[name = 'id_tipo_costo_nw3'][value = 'optimista']\").parent().css('background-color', '#B4CDCD');"),
                                                
                                         ),
                            ),
                            mainPanel(width = 8,
                                      fluidRow(
                                        column(3, infoBoxOutput("users_vpn1_nw", width = 12)),
                                        column(3, infoBoxOutput("users_vpn2_nw", width = 12)),
                                        column(3, infoBoxOutput("users_vpn3_nw", width = 12))
                                      ),
                                      h4(textOutput("title_vpn_nw"), style = "color: black; text-align: left"),
                                      h5(textOutput("subtitle_vpn_nw"), style = "color: black; text-align: left"),
                                      withSpinner(plotlyOutput("vpn_pozo_nw2")),
                                      
                                      conditionalPanel(condition = "input.v_dist_vpn_nw == false",
                                                       
                                                       column(12, align = "center", #legend
                                                              style = "padding-bottom: 40px;",
                                                              p(column(1),
                                                                column(2, img(src = "quintile1.png", height = "16px"), "1 - VPN Positivo"), 
                                                                column(1, img(src = "quintile2.png", height = "16px"), "2 - VPN Negativo"),
                                                                column(1)))
                                      ),
                                      
                                      DT::dataTableOutput("table_well_nw2")
                                      
                            )
)
