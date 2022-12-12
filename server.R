


# Read Pins ---------------------------------------------------------------
# library(pins)
# library(rsconnect)
###pat
library(patchwork)
library(lubridate)
library(shiny)
library(shinydashboard)
library(kableExtra)
library(waterfalls)
library(readr)
library(dplyr)
library(ggplot2)
library(readxl)
library(plotly)
library(DT)
library(tidyr)
library(rio)

### data   

#bd_perfil_pozo_v <- read_excel(file.path("data/bd_cr_c_nf_nn.xlsx"), sheet = "Sheet1")

#bd_perfil_pozo_v <- rio::import("https://github.com/jorgemedina87/pid_rondon_/raw/main/data/bd_cr_c_nf_nn.xlsx")

#bd_perfil_pozo_ <- read_excel(file.path("data/bd_perfil_pozo_.xlsx"), sheet = "Sheet1")

bd_perfil_pozo_ <- rio::import("https://github.com/jorgemedina87/pid_rondon_/raw/main/data/bd_perfil_pozo_.xlsx")


bd_WO<-  read.csv(file.path("data/bd_wo.csv"))

bd_NW <-  read.csv(file.path("data/bd_nw.csv"))

bd_prueba_b_pp <-  read.csv(file.path("data/bd_prueba_b_pp.csv"))

# bd_prueba_bi_bw <-read.csv(file.path("data/bd_prueba_bi_bw.csv"))
# 
# bd_prueba_bi_bw_nw <-read.csv(file.path("data/bd_prueba_bi_bw_nw.csv"))
# 
rawdata_total <-  read.csv(file.path("data/bd_total.csv"))


# id_campo <-bd_perfil_pozo_ %>%
#   select(well,CAMPO)%>%
#   unique()


# SERVER ------------------------------------------------------------------



server <- function(input, output, session) {

  #source("modules/server_perfil.R", local = TRUE)
  source("modules/server_vpn_total.R", local = TRUE)
  source("modules/server_Well_Well_Basic.R", local = TRUE)
  source("modules/server_VPN_WO.R", local = TRUE)
  source("modules/server_VPN_NW.R", local = TRUE)
  
}


