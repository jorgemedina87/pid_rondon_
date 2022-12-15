VPN_total <- tabItem(tabName = "VPN_total",
                     sidebarPanel(width = 4,
                                  column(12,
                                         
                                         div(title="",
                                             awesomeRadio("id_total_general_campo", label = shiny::HTML("<li> Seleccione el tipo de  analisis </li>"), #br required to try and keep alignment across columns
                                                          choices = list("General" = 1, "Campo" = 2),
                                                          selected = 1, inline = TRUE, checkbox = TRUE, status = "success")),
                                         
                                  ),
                                  
                                  conditionalPanel(condition = "input.id_total_general_campo == 2 ",
                                          column(12,
                                                 div(title = "Select what aspect of inequality you want to explore.", # tooltip
                                                     style = "margin-top: 10px; margin-bottom: 20px;",
                                                     radioGroupButtons("id_total_campo",
                                                                       label= shiny::HTML("<li> Seleccione el tipo de Campo de interes </li>"),
                                                                       choices = c("RONDON"   = 'CANO_RONDON',
                                                                                   "CARICARE" = 'CARICARE'),
                                                                       checkIcon = list(yes = icon("check")),
                                                                       select ='CARICARE' ,
                                                                       justified = TRUE
                                                     )
                                                 ),
                                                 
                                                 tags$script("$(\"input:radio[name = 'id_total_campo'][value = 'CANO_RONDON']\").parent().css('background-color', '#B4CDCD');"),
                                                 tags$script("$(\"input:radio[name = 'id_total_campo'][value = 'CARICARE']\").parent().css('background-color', '#CDC673');"),
                                                  ),
                                  ),
                                  
                                  
                                  column(12,
                                         shiny::hr(),
                                         div(title="",
                                             awesomeRadio("id_brent_vpn_total", label = shiny::HTML("<li> Seleccione el tipo de Brent a sensibilizar. </li>"), #br required to try and keep alignment across columns
                                                          choices = list("Portafolio" = 1, "Brent" = 2),
                                                          selected = 1, inline = TRUE, checkbox = TRUE, status = "success")),
                                         
                                  ),
                                  
                                  
                                  
                                  column(12,
                                         
                                         tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: green}")),
                                         
                                         conditionalPanel(condition = "input.id_brent_vpn_total == 2",
                                                          sliderInput(inputId = "id_brent_total",label = strong(""), 50, 90, 60, step = 10),
                                                          HTML("")
                                                          
                                         )
                                         
                                
                                  ),
                                  
                                  column(12,
                                         shiny::hr(),
                                         div(title = "", 
                                             style = "margin-top: 10px; margin-bottom: 20px;", 
                                             radioGroupButtons("id_tipo_costo_total", 
                                                               label = HTML("<li> Seleccione el tipo de Costo de interes </li>"), 
                                                               choices = c("Pesimista" = 'pesimista',
                                                                           "Probable" = 'probable',
                                                                           "Optimista" = 'optimista'),
                                                               checkIcon = list(yes = icon("check")),
                                                               select='probable' ,
                                                               justified = TRUE
                                             )),
                                         
                                         
                                         tags$script("$(\"input:radio[name = 'id_tipo_costo_total'][value = 'pesimista']\").parent().css('background-color', '#9BCD9B');"),
                                         tags$script("$(\"input:radio[name = 'id_tipo_costo_total'][value = 'probable']\").parent().css('background-color', '#CDC673');"),
                                         tags$script("$(\"input:radio[name = 'id_tipo_costo_total'][value = 'optimista']\").parent().css('background-color', '#B4CDCD');"),
                                         
                                  ),
                                  
                                  
                                  column(12,
                                         shiny::hr(),
                                         div(title = "",
                                             p(tags$b(HTML("<li> Seleccione el porcentaje de descuento en capex </li>")))),
                                         
                                         
                                         div(title="", # tooltip
                                             awesomeCheckbox(inputId = "id_PD_vpn_total", label = "Porcentaje de Descuento", value = FALSE, status = "success")),
                                         
                                         tags$style(HTML(".js-irs-1 .irs-single,.js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: green}")),
                                         
                                         
                                         conditionalPanel(condition = "input.id_PD_vpn_total == true",
                                                          sliderInput(inputId = "id_PD_total",label = strong(""), -0.2, 0.2, 0.0, step = 0.1),
                                                          HTML("")
                                                          
                                         )
                                         
                                         
                                  ),
                                  column(12,
                                         shiny::hr(),
                                         div(title = "",
                                             style = "margin-top: 10px; margin-bottom: 20px;",
                                             radioGroupButtons("id_socio_total",
                                                               label= HTML("<li> Seleccione el tipo a evaluar.</li>"),
                                                               choices = c("Sin Socio"='sin_socio',
                                                                           "Con socio"='con_socio'),
                                                               checkIcon = list(yes = icon("check")),
                                                               select='con_socio' ,
                                                               justified = TRUE
                                             )),
                                         
                                         tags$script("$(\"input:radio[name = 'id_socio_total'][value = 'sin_Socio']\").parent().css('background-color', '#CDC673');"),
                                         tags$script("$(\"input:radio[name = 'id_socio_total'][value = 'con_socio']\").parent().css('background-color', '#B4CDCD');"),
                                         
                                  ),
                                  
                                  
                                  
                                  column(12,
                                         shiny::hr(),
                                         div(title = "",
                                             em("Nota NW Pozos Infill - NFE Pozos Near Field Exploration"),
                                             p(tags$b(HTML("<li> Seleccione el  analisis por  basica WO o NW </li>")))),
                                         
                                         column(4,
                                          switchInput(inputId = "id_basic_", label = "Basica" , value = 1, onStatus = "success", offStatus = "danger")),
                                         
                                         column(4,
                                                switchInput(inputId = "id_wo_", label = "WO" , value = 1, onStatus = "success", offStatus = "danger")),
                                         column(4,
                                                switchInput(inputId = "id_nw_", label = "NW" , value = 1, onStatus = "success", offStatus = "danger")),
                                        
                                  ),
                                  
                                  
                                             
                                  ),
                      
                      
                      
                     mainPanel(width = 8, 
                               
                               conditionalPanel(condition = "input.id_brent_vpn_total == 1",
                                                fluidRow(
                                                  column(3,infoBoxOutput("box_total_vpn_port",width = 12)),
                                                  column(3,infoBoxOutput("box_total_opex_port",width = 12)),
                                                  column(3,infoBoxOutput("box_total_vol_port",width = 12))
                                                ),
                                                
                                                withSpinner(plotlyOutput("plot_total_vpn_port")),
                                                withSpinner(DT::dataTableOutput("table_total_vpn_port"))
                                                
                               ),
                               
                               
                               conditionalPanel(condition = "input.id_brent_vpn_total == 2",
                                                fluidRow(
                                                  column(3,infoBoxOutput("box_total_vpn_brent",width = 12)),
                                                  column(3,infoBoxOutput("box_total_opex_brent",width = 12)),
                                                  column(3,infoBoxOutput("box_total_vol_brent",width = 12))
                                                ),
                                                
                                                withSpinner(plotlyOutput("plot_total_vpn_brent")),
                                                withSpinner(DT::dataTableOutput("table_total_vpn_brent"))
                                                
                               )
                               
                               
                               
                     )
                     
                     
)
