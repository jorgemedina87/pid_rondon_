tab_perfil <- tabItem(tabName = "tab_perfil",
                      sidebarPanel(width = 4,
                                  column(12,
                                          div(title = "Select what aspect of inequality you want to explore.", # tooltip
                                              style = "margin-top: 10px; margin-bottom: 20px;",
                                              radioGroupButtons("id_perf_prod",
                                                                label = shiny::HTML("<li> Seleccione  analisis desea explorar.</li>"),
                                                                choices = c("Aceite"='Oil',
                                                                            "Agua"='Water',
                                                                            "Gas"='gas'),
                                                                checkIcon = list(yes = icon("check")),
                                                                select='Oil' ,
                                                                justified = TRUE
                                                                )
                                              ),

                                          tags$script("$(\"input:radio[name='id_perf_prod'][value = 'Oil']\").parent().css('background-color', '#9BCD9B');"),
                                          tags$script("$(\"input:radio[name='id_perf_prod'][value = 'Water']\").parent().css('background-color', '#CDC673');"),
                                          tags$script("$(\"input:radio[name='id_perf_prod'][value = 'gas']\").parent().css('background-color', '#B4CDCD');"),

                                        ),

                                   div(title="",
                                       style = "margin-top: 10px; margin-bottom: 20px;",
                                       #strong("Nota Se incluye perfil de  rondon para efecto de  analisis de  restriccion de facilidades", style = "color:#008B45"),
                                       awesomeRadio("id_activo_perfil_", label = shiny::HTML("<li> Seleccione el tipo de  &aacutenalisis </li>"), #br required to try and keep alignment across columns
                                                    choices = list("General" = 1, "Activo" = 2),
                                                    selected = 1, inline = TRUE, checkbox = TRUE, status = "success")),

                                   conditionalPanel(condition = "input.id_activo_perfil_ == 2 ",
                                                    div(title = "", # tooltip
                                                        style = "margin-top: 10px; margin-bottom: 20px;",
                                                        radioGroupButtons("id_perf_activo",
                                                                          label= HTML("<li> Seleccione el campo de interes </li>"),
                                                                          choices = c("Rondon" = 'RONDON',
                                                                                      "Cosecha" = 'COSECHA'
                                                                                      ), #campo cambiado CANO_RONDO
                                                                          checkIcon = list(yes = icon("check")),
                                                                          select='RONDON' ,
                                                                          justified = TRUE
                                                        )),
                                                    tags$script("$(\"input:radio[name='id_perf_activo'][value = 'RONDON']\").parent().css('background-color', '#CD8162');"),
                                                    tags$script("$(\"input:radio[name='id_perf_activo'][value = 'COSECHA']\").parent().css('background-color', '#CD8500');")
                                                    
                                   ),

                                   div(title="",
                                       style = "margin-top: 10px; margin-bottom: 20px;",
                                       awesomeRadio("id_tipo_perfil", label = shiny::HTML("<li> Seleccione el tipo de  analisis </li>"), #br required to try and keep alignment across columns
                                                    choices = list("General"= 1, "Basica"= 2, "Incremental" = 3),
                                                    selected = 1, inline = TRUE, checkbox = TRUE, status = "success")),

                                   column(12,
                                          shiny::hr(),
                                          shiny::HTML("<li> Seleccione el tipo de campo  analizar </li>"),

                                          div(title = "", 
                                              style = "margin-top: 10px; margin-bottom: 20px;",
                                              awesomeCheckbox(inputId = "id_perf_campo_", label = "Campo", value = FALSE, status ="success")
                                              ),

                                          conditionalPanel(condition = "input.id_perf_campo_ == true",
                                                           selectInput(inputId = "id_perf_campo", label = strong(""),
                                                                       choices = "",
                                                                       selected = "",
                                                                       multiple = TRUE)
                                                          )
                                          ),

                                   column(12,
                                          shiny::hr(),
                                          shiny::HTML("<li> Seleccione el tipo de trabajo analizar </li>"),


                                          div(title="", 
                                              style = "margin-top: 10px; margin-bottom: 20px;",# tooltip
                                              awesomeCheckbox(inputId = "id_perf_trabajo_", label = "Trabajo", value = FALSE,status="success")
                                              ),

                                          conditionalPanel(condition = "input.id_perf_trabajo_ == true",
                                                           selectInput(inputId = "id_perf_trabajo", label = strong(""),
                                                                       choices = "",
                                                                       selected = "",
                                                                       multiple = TRUE)
                                                           )
                                          ),




                                   column(12,
                                          shiny::hr(),
                                          shiny::HTML("<li> Seleccione el tipo de pozo  analizar </li>"),


                                          div(title="", 
                                              style = "margin-top: 10px; margin-bottom: 20px;",# tooltip
                                              awesomeCheckbox(inputId = "id_perf_pozo_", label = "pozo", value = FALSE,status="success")
                                              ),

                                          conditionalPanel(condition = "input.id_perf_pozo_ == true",
                                                           selectInput(inputId = "id_perf_pozo", label = strong(""),
                                                                       choices = "",
                                                                       selected = "",
                                                                       multiple = TRUE)
                                                          )
                                          ),


                                 


        ),
        
        
        
        mainPanel(width = 8, #Main panel
                  
                  fluidRow(column(6,
                                  withSpinner(plotlyOutput("plot_perf_reserva"))),
                           column(6,
                                  withSpinner(plotlyOutput("plot_perf_campo"))),
                          ),

                  fluidRow(column(4,
                                  withSpinner(plotlyOutput("plot_perf_basic_incr"))),
                           column(4,
                                  withSpinner(tableOutput("table_perf_basic_incr"))),
                           column(4,
                                  withSpinner(plotlyOutput("plot_perf_vol"))),
                           ),
                  )
        
            
                      
)
