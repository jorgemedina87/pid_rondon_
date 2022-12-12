VPN_WO<- tabItem(tabName = "VPN_WO", 
                 sidebarPanel(width = 4,
                              column(12,
                                     shiny::hr(),
                                     div(title = "", 
                                         style = "margin-top: 10px; margin-bottom: 20px;",
                                         radioGroupButtons("id_tipo_campo_VPNwo",
                                                           label = HTML("<li> Seleccione el tipo de Campo de interes </li>"),
                                                           choices = c("RONDON" = 'CANO_RONDON',
                                                                       "CARICARE" = 'CARICARE'),
                                                           checkIcon = list(yes = icon("check")),
                                                           select ='CARICARE' ,
                                                           justified = TRUE
                                         )),
                                     
                                     tags$script("$(\"input:radio[name = 'id_tipo_campo_VPNwo'][value = 'CANO_RONDON']\").parent().css('background-color', '#CDC673');"),
                                     tags$script("$(\"input:radio[name = 'id_tipo_campo_VPNwo'][value = 'CARICARE']\").parent().css('background-color', '#B4CDCD');"),
                                     
                                     
                              ),
                              
                              
                              
                              column(12,
                                     shiny::hr(),
                                     div(title = "",
                                         p(tags$b(HTML("<li> Seleccione el tipo de pozo analizar </li>")))),
                                     
                                     
                                     div(title="", # tooltip
                                         awesomeCheckbox(inputId = "v_dist_pozo2_1", label = "Pozo", value = FALSE,status="success")),
                                     
                                     conditionalPanel(condition = "input.v_dist_pozo2_1 == true",
                                                      selectInput(inputId = "tipo_VPN_wo", label = strong(""),
                                                                  choices = "",
                                                                  selected = "")
                                                      
                                     )
                                     
                                     
                              ),
                              
                              column(12,
                                     shiny::hr(),
                                     div(title = "",
                                         p(tags$b(HTML("<li> Seleccione el tipo de Brent a sensibilizar </li>")))),
                                     
                                     
                                     div(title = "", 
                                         awesomeCheckbox(inputId = "v_dist_brent_vpn_wo", label = "Brent", value = FALSE,status="success")),
                                     
                                     tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: green}")),
                                     
                                     
                                     conditionalPanel(condition = "input.v_dist_brent_vpn_wo == true",
                                                      sliderInput(inputId ="id_brent_VPN_WO",label = strong(""), 50, 90, 60, step = 10),
                                                      HTML("")
                                                      
                                     )
                                     
                                     
                              ),
                              
                              column(12,
                                     shiny::hr(),
                                     div(title = "",
                                         p(tags$b(HTML("<li> Seleccione el porcentaje de descuento en capex </li>")))),
                                     
                                     
                                     div(title="", # tooltip
                                         awesomeCheckbox(inputId = "v_dist_PD_vpn_wo", label = "P de Descuento", value = FALSE, status = "success")),
                                     
                                     tags$style(HTML(".js-irs-1 .irs-single,.js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: green}")),
                                     
                                     
                                     conditionalPanel(condition = "input.v_dist_PD_vpn_wo == true",
                                                      sliderInput(inputId ="id_PD2_vpn_wo",label = strong(""), -0.2, 0.2, 0.0, step = 0.1),
                                                      HTML("")
                                                      
                                     )
                                     
                                     
                              ),
                              column(12,
                                     shiny::hr(),
                                     conditionalPanel(condition = "input.v_dist_pozo2_1 == true",
                                                      
                                                      div(title = "", 
                                                          style = "margin-top: 10px; margin-bottom: 20px;",
                                                          radioGroupButtons("id_FC_VPN_WO",
                                                                            label = HTML("<li> Seleccione  analisis desea explorar. </li>"),
                                                                            choices = c("Distibucion Opex" = 'D_opex',
                                                                                        "FC en Tiempo" = 'fc_t'),
                                                                            checkIcon = list(yes = icon("check")),
                                                                            select ='fc_t' ,
                                                                            justified = TRUE
                                                          )),
                                                      
                                                      tags$script("$(\"input:radio[name = 'id_FC_VPN_WO'][value = 'D_opex']\").parent().css('background-color', '#F7DB17');"),
                                                      tags$script("$(\"input:radio[name = 'id_FC_VPN_WO'][value = 'fc_t']\").parent().css('background-color', '#CCD32A');"),
                                     )
                              ),
                              column(12,
                                     shiny::hr(),
                                     div(title = "",
                                         style = "margin-top: 10px; margin-bottom: 20px;",
                                         radioGroupButtons("id_socio_VPN_WO",
                                                           label= HTML("<li> Seleccione el tipo a evaluar. </li>"),
                                                           choices = c("Sin Socio"='sin_socio',
                                                                       "Con socio"='con_socio'),
                                                           checkIcon = list(yes = icon("check")),
                                                           select='con_socio' ,
                                                           justified = TRUE
                                         )),
                                     
                                     tags$script("$(\"input:radio[name = 'id_socio_VPN_WO'][value = 'sin_Socio']\").parent().css('background-color', '#CDC673');"),
                                     tags$script("$(\"input:radio[name = 'id_socio_VPN_WO'][value = 'con_socio']\").parent().css('background-color', '#B4CDCD');"),
                                     
                              ),
                              
                              column(12,
                                     shiny::hr(),
                                     div(title = "", # tooltip
                                         style = "margin-top: 10px; margin-bottom: 20px;", 
                                         radioGroupButtons("id_tipo_costovpn_wo", 
                                                           label = HTML("<li> Seleccione el tipo de Costo de  interes </li>"), 
                                                           choices = c("Pesimista" = 'pesimista',
                                                                       "Probable" = 'probable',
                                                                       "Optimista" = 'optimista'),
                                                           checkIcon = list(yes = icon("check")),
                                                           select ='probable' ,
                                                           justified = TRUE
                                         )),
                                     
                                     
                                     
                                     tags$script("$(\"input:radio[name = 'id_tipo_costovpn_wo'][value = 'pesimista']\").parent().css('background-color', '#9BCD9B');"),
                                     tags$script("$(\"input:radio[name = 'id_tipo_costovpn_wo'][value = 'probable']\").parent().css('background-color', '#CDC673');"),
                                     tags$script("$(\"input:radio[name = 'id_tipo_costovpn_wo'][value = 'optimista']\").parent().css('background-color', '#B4CDCD');"),
                                     
                              ),
                 ),
                 mainPanel(width = 8,
                           fluidRow(
                             column(3, infoBoxOutput("users_vpn_wo1", width = 12)),
                             column(3, infoBoxOutput("users_vpn_wo2", width = 12)),
                             column(3, infoBoxOutput("users_vpn_wo3", width = 12))
                           ),
                           h4(textOutput("title_vpn_wo"), style = "color: black; text-align: left"),
                           h5(textOutput("subtitle_vpn_wo"), style = "color: black; text-align: left"),
                           withSpinner(plotlyOutput("vpn_wo1")),
                           
                           conditionalPanel(condition = "input.v_dist_pozo2_1 == false",
                                            
                                            column(12, align = "center",
                                                   style = "padding-bottom: 40px;",
                                                   p(column(1),
                                                     column(2, img(src = "quintile1.png", height = "16px"), "1 - VPN Positivo"), 
                                                     column(1, img(src = "quintile2.png", height = "16px"), "2 - VPN Negativo"),
                                                     column(1)))
                           ),
                           
                           DT::dataTableOutput("table_vpn_wo1")
                           
                 )
)
