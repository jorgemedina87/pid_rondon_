##Packages ----
############################.
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(plotly)
library(shiny)
library(shinyBS) 
library(shinythemes) 
library(dplyr) 
library(ggplot2) 
library (DT) 
library(leaflet) 
library(plotly) 
library(shinyWidgets)
library(tibble) 
library(shinyBS) 
library(shinyjs)
library(shinydashboard) 
library(sp)
library(lubridate)
library(shinycssloaders) 
library(rmarkdown)
library(webshot) 
library(rintrojs)
library(tidyr)
library(stringr)
library(purrr)
library(rvest)

source("modules/ui_tab_perfil.R")
#source("modules/ui_VPN_Basic.R", local = TRUE)
source("modules/ui_VPN_total.R", local = TRUE)
source("modules/ui_Well_Well_Basic.R", local = TRUE)
source("modules/ui_VPN_WO.R", local = TRUE)
source("modules/ui_VPN_NW.R", local = TRUE)
#options(encoding = 'UTF-8')



ui <- dashboardPage(
  skin="green",
  title = "PID RONDON",
  # HEADER ------------------------------------------------------------------
  dashboardHeader(title = "PID RONDON"), 
  
  # SIDEBAR -----------------------------------------------------------------
  
  dashboardSidebar(
    br(),
    sidebarMenu(
      menuItem(HTML("<b> Perfiles </b>"), tabName = "tab_perfil", icon = icon("area-chart")),
      menuItem(HTML("<b> Evaluacion Economica </b>"), tabName = "VPN_total", icon = icon("dollar-sign")
               ),
       menuItem(HTML("<b> Pozo a Pozo </b>"),  icon = icon("dollar-sign"),
       menuSubItem("Basica", tabName = "Well_Well_Basic", icon = icon("fas fa-chart-area")),
       menuSubItem("WO", tabName = "VPN_WO", icon = icon("fas fa-chart-area")),
              menuSubItem("NW", tabName = "VPN_NW", icon = icon("fas fa-file-export"))
               ),
     
      br(),
      br(),
      hr(),
      menuItem(HTML("<b>Help</b>"), tabName = "tab_help", icon = icon("life-ring"), selected=TRUE)),
    
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    div(img(src = "logo.png",width = "200", height = "100"), style = "text-align: center;")

    ),

  
  # BODY --------------------------------------------------------------------
 
  
  dashboardBody(
   
    tabItems(
      tab_perfil,
      VPN_total,
      
      Well_Well_Basic,
      VPN_WO,
      VPN_NW 
    )
   
  )

)


  

