Login <-  tabItem(tabName = "Login",
                            tagList(tags$h2(HTML("Plan Integrado de Desarrollo Rondon 2022"), class = "text-center")),       
                            shinyjs::useShinyjs(),
                            shinyauthr::loginUI("login"),
                            
                            
                            uiOutput("image_n_21"),
                            tags$h3(HTML("Gerencia de Desarrollo y Nuevos Descubrimientos -VDE"), class = "text-center")
                            
)