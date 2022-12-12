
VPN_Basic <- tabItem(tabName = "VPN_Basic", "VPN",
                 fluidRow(
                   
                   sidebarPanel(width=4,
                                
                                
                                  column(12,
                                       shiny::hr(),
                                       div(title="", # tooltip
                                           style = "margin-top: 10px; margin-bottom: 20px;",
                                           radioGroupButtons("id_tipo_campo",
                                                             label= HTML("<li> Seleccione el tipo de Campo de interes </li>"),
                                                             choices = c("RONDON"='CANO_RONDON',
                                                                         "CARICARE"='CARICARE'),
                                                             checkIcon = list(yes = icon("check")),
                                                             select='RONDON' ,
                                                             justified = TRUE
                                           )),
                                       
                                       tags$script("$(\"input:radio[name='id_tipo_campo'][value='CANO_RONDON']\").parent().css('background-color', '#FF5F00');"),
                                       tags$script("$(\"input:radio[name='id_tipo_campo'][value='CARICARE']\").parent().css('background-color', '#808080');"),
                                       
                                       
                                ),
                                
                               
                                column(12,
                                       shiny::hr(),
                                       div(title="", # tooltip
                                           style = "margin-top: 10px; margin-bottom: 20px;",
                                           prettyRadioButtons("id_tipo_reserva",
                                                              inputId = "Id039",
                                                              label= HTML("<li> Seleccione el tipo de reserva </li>"),
                                                              choices = c('PDP',
                                                                          'PRBP',
                                                                          'PSP'),
                                                              icon = icon("check"), 
                                                              inline = TRUE, 
                                                              status = "danger",
                                                              fill = TRUE
                                                              
                                           )),
                                       tags$script("$(\"input:radio[name='id_tipo_reserva'][value='PDP']\").parent().css('background-color', '#FF5F00');"),
                                       tags$script("$(\"input:radio[name='id_tipo_reserva'][value='PRBP']\").parent().css('background-color', '#808080');"),
                                       tags$script("$(\"input:radio[name='id_tipo_reserva'][value='PSP']\").parent().css('background-color', '#808080');"),
                                       
                                ),
                                
                                
                                
                                column(12,
                                       shiny::hr(),
                                       div(title="",
                                           p(tags$b(HTML("<li> Selecciones el tipo de Brent a sensibilizar. </li>")))),
                                       
                                       
                                       div(title="", # tooltip
                                           awesomeCheckbox(inputId = "v_dist_brent_vpn_p", label = "Brent", value = FALSE,status="success")),
                                       
                                       tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: green}")),
                                       
                                       
                                       conditionalPanel(condition = "input.v_dist_brent_vpn_p == true",
                                                        sliderInput(inputId ="id_brent_p",label = strong(""), 40,100,50,step=10),
                                                        HTML("")
                                                        
                                       )
                                       
                                       
                                ),
                                
                                
                                column(12,
                                       shiny::hr(),
                                       div(title="", # tooltip
                                           style = "margin-top: 10px; margin-bottom: 20px;",
                                           radioGroupButtons("id_grafi_pp",
                                                             label= HTML("<li> Seleccione  analisis desea explorar. </li>"),
                                                             choices = c("Distibucion Opex"='D_opex',
                                                                         "FC en Tiempo"='fc_t',
                                                                         "FC Descontado"='fc_d'),
                                                             checkIcon = list(yes = icon("check")),
                                                             select='fc_t' ,
                                                             justified = TRUE
                                           )),
                                       
                                       tags$script("$(\"input:radio[name='id_grafi_pp'][value='D_opex']\").parent().css('background-color', '#F7DB17');"),
                                       tags$script("$(\"input:radio[name='id_grafi_pp'][value='fc_t']\").parent().css('background-color', '#CCD32A');"),
                                       tags$script("$(\"input:radio[name='id_grafi_pp'][value='fc_d']\").parent().css('background-color', '#004236');"),
                                       
                                ),
                                
                                column(12,
                                       shiny::hr(),
                                       div(title="", # tooltip
                                           style = "margin-top: 10px; margin-bottom: 20px;",
                                           radioGroupButtons("id_grafi_pp2",
                                                             label= HTML("<li> Seleccione el tipo a evaluar. </li>"),
                                                             choices = c("Sin Socio"='sin_Socio',
                                                                         "Con socio"='con_socio'),
                                                             checkIcon = list(yes = icon("check")),
                                                             select='con_socio' ,
                                                             justified = TRUE
                                           )),
                                       
                                       tags$script("$(\"input:radio[name='id_grafi_pp2'][value='sin_Socio']\").parent().css('background-color', '#F7DB17');"),
                                       tags$script("$(\"input:radio[name='id_grafi_pp2'][value='con_socio']\").parent().css('background-color', '#CCD32A');"),
                                       
                                ),
                                

                                mainPanel(width = 8, #Main panel
                                          
                                          fluidRow(
                                            column(3,infoBoxOutput("users_vpn1p",width = 12)),
                                            column(3,infoBoxOutput("users_vpn2p",width = 12)),
                                            column(3,infoBoxOutput("users_vpn3p",width = 12))
                                          ),
                                          column(12, align="center", #legend
                                                 style= "padding-bottom: 40px;",
                                                 p(column(1),
                                                   column(2, img(src="quintile1.png", height = "16px"), "1 - VPN Positivo"),
                                                   column(1, img(src="quintile2.png", height = "16px"), "2 - VPN Negativo"),
                                                   column(1)))
                                ),
                                
                                
                                
                   )
                 )
)



