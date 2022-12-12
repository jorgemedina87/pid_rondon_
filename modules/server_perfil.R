
observe({
  
  if(input$id_perf_campo_ == FALSE){
    
    list_ <- 0
    
    
  }else if (input$id_activo_perfil_ == 2 & input$id_perf_campo_ == TRUE) {
    
    list_ <- bd_perfil_pozo_v %>%
      filter(activo == input$id_perf_activo) %>%
      pull(campo) %>%
      unique() %>% 
      simplify2array()
    
    
  }else {
    
    list_ <- bd_perfil_pozo_v %>%
      pull(campo) %>%
      unique() %>% 
      simplify2array()
    
    
    
  }
  updateSelectInput(session, "id_perf_campo", choices = list_ )
  
  
}) 





#################

observe({
  
  if(input$id_perf_trabajo_ == FALSE){
    
    list_ <-0
    
  }else if (input$id_activo_perfil_ == 2 & input$id_perf_trabajo_ == TRUE) {
    
    list_ <- bd_perfil_pozo_v %>%
      filter(activo == input$id_perf_activo) %>%
      pull(Trabajo) %>%
      unique() %>%
      simplify2array()
    
  }else {
    
    list_ <- bd_perfil_pozo_v %>%
      pull(Trabajo) %>%
      unique() %>%
      simplify2array()
    
  }
  
  updateSelectInput(session, "id_perf_trabajo", choices = list_)
  
  
  
})




observe({
  
  
  if(input$id_perf_pozo_ == FALSE){
    
    list_ <- 0
    
  }else if (input$id_activo_perfil_ == 2 & input$id_perf_campo_ == TRUE &
            input$id_perf_trabajo_ == TRUE & input$id_perf_pozo_ == TRUE) {
    
    list_ <- bd_perfil_pozo_v %>%
      filter(activo == input$id_perf_activo) %>%
      filter(campo %in% input$perf_campo_v) %>%
      filter(Trabajo %in% input$perf_trabajo_v) %>%
      pull(pozo) %>%
      unique() %>%
      simplify2array()
  }else {
    
    list_ <- bd_perfil_pozo_v %>%
      pull(pozo)%>%
      unique() %>%
      simplify2array()
    
  }
  
  updateSelectInput(session, "id_perf_pozo", choices =list_ )
  
  
})

###################solo graficos 


data_perf_basic_incr_ <- reactive({
  
  BD <- bd_perfil_pozo_v
  ########## Filtro General, Cosecha, Rondo
  if (input$id_activo_perfil_ == 2){
    BD <- BD %>%
      filter(activo == input$id_perf_activo)
    
  }else {}
  #####Filtro Campo
  ifelse(input$id_perf_campo_ == TRUE, BD <- BD %>% filter(campo %in% input$id_perf_campo), BD <- BD)
  ######## Filtro Tipo
  ##Filtro General, Basico, Incremental
  if (input$id_tipo_perfil == 2){
    BD <- BD %>% filter(Tipo == 'Basica')
  }else if (input$id_tipo_perfil == 3){
    BD <- BD %>% filter(Tipo == 'Incremental')
  }else{BD <- BD}
  #####Filtro Trabajo
  ifelse(input$id_perf_trabajo_ == TRUE, BD <- BD %>% filter(Trabajo %in% input$id_perf_trabajo), BD <- BD)
  ####Filtro Pozo
  ifelse(input$id_perf_pozo_ == TRUE, BD <- BD %>% filter(pozo %in% input$id_perf_pozo), BD <- BD)
  ######## Filtro Clasificacion
  ifelse(input$id_clasificacion_perf == 'General', BD <- BD, BD <- BD %>% filter(clasificacion == input$id_clasificacion_perf))
  
  BD <- BD %>%
    select(Date,input$id_perf_prod, Tipo)%>%
    group_by(Date, Tipo)%>%
    dplyr::rename(sum_p = input$id_perf_prod) %>%
    summarise(sum_ = sum(sum_p))
  
})
#####################


output$plot_perf_basic_incr <- renderPlotly({
  
  fig <-plot_ly(data_perf_basic_incr_(), x = ~ Date, y = ~ sum_, color = ~Tipo,
                type = 'scatter', mode = 'none',
                stackgroup = 'one')
  
  
  if (input$id_perf_prod == 'Oil'){
    fig %>%
      layout(
        xaxis = list(title = "DATE",
                     showgrid = FALSE),
        yaxis = list(title = "BOPD",
                     showgrid = FALSE))
    
  }else if (input$id_perf_prod == 'Water'){
    fig %>%
      layout(
        xaxis = list(title = "DATE",
                     showgrid = FALSE),
        yaxis = list(title = "BWPD",
                     showgrid = FALSE))
    
  }else if (input$id_perf_prod == 'gas'){
    fig %>%
      layout(
        xaxis = list(title = "DATE",
                     showgrid = FALSE),
        yaxis = list(title = "KPSD",
                     showgrid = FALSE))
    
  }
  
  
})


######################


data_perf_reserva_ <- reactive({
  
  BD <- bd_perfil_pozo_v
  
  if (input$id_activo_perfil_ == 2){
    BD <- BD %>% filter(activo == input$id_perf_activo)
  }else {}
  
  ifelse(input$id_perf_campo_ == TRUE, BD <- BD %>% filter(campo %in% input$id_perf_campo), BD <- BD)
 
  
  if (input$id_tipo_perfil == 2){
    BD <- BD %>% filter(Tipo == 'Basica')
  }else if (input$id_tipo_perfil == 3){
    BD <- BD %>% filter(Tipo == 'Incremental')
  }else{BD <- BD}
  
  ifelse(input$id_perf_trabajo_ == TRUE, BD <- BD %>% filter(Trabajo %in% input$id_perf_trabajo), BD <- BD)
  
  ifelse(input$id_perf_pozo_ == TRUE, BD <- BD %>% filter(pozo %in% input$id_perf_pozo), BD <- BD)
 
  ifelse(input$id_clasificacion_perf == 'General', BD <- BD, BD <- BD %>% filter(clasificacion == input$id_clasificacion_perf))
  
  BD <- BD %>%
    select(Date, input$id_perf_prod, Categoria)%>%
    group_by(Date, Categoria)%>%
    dplyr::rename(sum_p = input$id_perf_prod) %>%
    summarise(sum_ = sum(sum_p))
})


#####################.

#### grafica de bi

output$plot_perf_reserva <- renderPlotly({ 
  
  
  fig <-plot_ly(data_perf_reserva_(), x = ~ Date, y = ~ sum_, color = ~ Categoria,
                type='scatter', mode='none',
                stackgroup='one')
  
  
  if (input$id_perf_prod == 'Oil'){
    fig %>%
      layout(
        xaxis = list(title = "DATE",
                     showgrid = FALSE),
        yaxis = list(title = "BOPD",
                     showgrid = FALSE))
    
  }else if (input$id_perf_prod =='Water'){
    fig %>%
      layout(
        xaxis = list(title = "DATE",
                     showgrid = FALSE),
        yaxis = list(title = "BWPD",
                     showgrid = FALSE))
    
  }else if (input$id_perf_prod == 'gas'){
    
    fig %>%
      layout(
        xaxis = list(title = "DATE",
                     showgrid = FALSE),
        yaxis = list(title = "KPSD",
                     showgrid = FALSE))
    
  }
  
  
  
})


#########################
data_perf_campo_ <- reactive({
  
  BD <- bd_perfil_pozo_v
  if (input$id_activo_perfil_ == 2){
    
    BD <- BD %>% filter(activo == input$id_perf_activo)
  }else {}
  
  ifelse(input$id_perf_campo_ == TRUE, BD <- BD %>% filter(campo %in% input$id_perf_campo), BD <- BD)
  
  if (input$id_tipo_perfil == 2){
    BD <- BD %>% filter(Tipo == 'Basica')
  }else if (input$id_tipo_perfil == 3){
    BD <- BD %>% filter(Tipo == 'Incremental')
  }else{BD <- BD}
  
  ifelse(input$id_perf_trabajo_== TRUE, BD <- BD %>% filter(Trabajo %in% input$id_perf_trabajo), BD <- BD)
  
  ifelse(input$id_perf_pozo_ == TRUE, BD <- BD %>% filter(pozo %in% input$id_perf_pozo), BD <- BD)
  
  ifelse(input$id_clasificacion_perf == 'General', BD <- BD, BD <- BD %>% filter(clasificacion==input$id_clasificacion_perf))
  
  BD <- BD %>%
    select(Date,input$id_perf_prod, campo)%>%
    group_by(Date, campo)%>%
    dplyr::rename(sum_p = input$id_perf_prod) %>%
    summarise(sum_ = sum(sum_p))
  
  
})
#####################.

output$plot_perf_campo <- renderPlotly({
  
  
  fig <- plot_ly(data_perf_campo_(),x = ~Date, y = ~sum_, color = ~campo,
                type ='scatter', mode = 'none',
                stackgroup = 'one')
  
  if (input$id_perf_prod == 'Oil'){
    fig %>%
      layout(
        xaxis = list(title = "DATE",
                     showgrid = FALSE),
        yaxis = list(title = "BOPD",
                     showgrid = FALSE))
    
  }else if (input$id_perf_prod == 'Water'){
    fig %>%
      layout(
        xaxis = list(title = "DATE",
                     showgrid = FALSE),
        yaxis = list(title = "BWPD",
                     showgrid = FALSE))
    
  }else if (input$id_perf_prod == 'gas'){
    
    fig %>%
      layout(
        xaxis = list(title = "DATE",
                     showgrid = FALSE),
        yaxis = list(title = "KPSD",
                     showgrid = FALSE))
    
  }
  
})



###########GRAFICA HISTOGRAMA
################ histograma
data_perf_vol_ <- reactive({
  
  options(dplyr.summarise.inform = FALSE)
  BD <- bd_perfil_pozo_v
  
  if (input$id_activo_perfil_ == 2){
    BD <- BD %>% filter(activo == input$id_perf_activo)
  }else {}
  
  ifelse(input$id_perf_campo_ == TRUE, BD <- BD %>% filter(campo %in% input$id_perf_campo), BD <- BD)
  
  ifelse(input$id_perf_trabajo_ == TRUE, BD <- BD %>% filter(Trabajo %in% input$id_perf_trabajo), BD <- BD)
  
  ifelse(input$id_perf_pozo_ == TRUE,BD <- BD %>% filter(pozo %in% input$id_perf_pozo), BD <- BD)
  
  ifelse(input$id_clasificacion_perf == 'General', BD <- BD, BD <- BD %>% filter(clasificacion==input$id_clasificacion_perf))
  
  BD <- BD %>% group_by(Tipo, campo)
  
  if (input$id_perf_prod =='gas'){BD <- BD %>% summarise(Suma_vol = round(sum(Vol_gas), 3))
  }else if (input$id_perf_prod =='Water'){BD <- BD %>% summarise(Suma_vol = round(sum(vol_agua),3 ))
  }else{BD <- BD %>% summarise(Suma_vol = round(sum(Vol), 3))}
  
  BD <- BD %>%
    bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(., na.rm = TRUE) else "Total")))
  
  if (input$id_tipo_perfil == 2){
    BD <- BD %>% filter(Tipo == 'Basica')
  }else if (input$id_tipo_perfil == 3){
    BD <- BD %>% filter(Tipo == 'Incremental')
  }else{BD <- BD}
  
  BD[BD$campo=='Total',]
})


output$plot_perf_vol <- renderPlotly({

  fig <-plot_ly(data_perf_vol_(),x = ~Tipo, y = ~Suma_vol, color = ~Tipo,
                type = 'bar')
  

  if (input$id_perf_prod == 'Oil'){
    fig %>%
      layout(
        xaxis = list(title = "TIPO",
                     showgrid = FALSE),
        yaxis = list(title = "MBLS",
                     showgrid = FALSE))

  }else if (input$id_perf_prod == 'Water'){
    
    fig %>%
      layout(
        xaxis = list(title = "TIPO",
                     showgrid = FALSE),
        yaxis = list(title = "MBLS",
                     showgrid = FALSE))

  }else if (input$id_perf_prod == 'gas'){

    
    fig %>%
      layout(
        xaxis = list(title = "TIPO",
                     showgrid = FALSE),
        yaxis = list(title = "MBLS",
                     showgrid = FALSE))

  }
})




####salida Tabla

table_data <- reactive({
  
  options(dplyr.summarise.inform = FALSE)
  BD <- bd_perfil_pozo_v
  
  if (input$id_activo_perfil_ == 2){
    BD <- BD %>% filter(activo == input$id_perf_activo)
  }else {}
  
  ifelse(input$id_perf_campo_ == TRUE, BD <- BD %>% filter(campo %in% input$id_perf_campo), BD <- BD)
  
  ifelse(input$id_perf_trabajo_ == TRUE,BD <- BD %>% filter(Trabajo %in% input$id_perf_trabajo), BD <- BD)
  
  ifelse(input$id_perf_pozo_ == TRUE,BD <- BD %>% filter(pozo %in% input$id_perf_pozo), BD <- BD)
  
  ifelse(input$id_clasificacion_perf == 'General', BD <- BD, BD <- BD %>% filter(clasificacion==input$id_clasificacion_perf))
  
  BD <- BD %>% group_by(Tipo, campo)
  
  if (input$id_perf_prod =='gas'){BD <- BD %>% summarise(Suma_vol = round(sum(Vol_gas), 3))
  }else if (input$id_perf_prod == 'Water'){BD <- BD %>% summarise(Suma_vol = round(sum(vol_agua), 3))
  }else{BD <- BD %>% summarise(Suma_vol = round(sum(Vol), 3))}
  
  BD <- BD %>% spread(Tipo, Suma_vol) %>%
    bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(., na.rm = TRUE) else "Total")))
  
  if (input$id_tipo_perfil == 2){
    BD <- BD %>% select(campo, Basica)
  }else if (input$id_tipo_perfil == 3){
    BD <- BD %>% select(campo, Incremental)
  }else{BD <- BD}
})


####
output$table_perf_basic_incr <- renderTable({ head( table_data(), n = 11)},
                                            striped = TRUE, bordered = TRUE,
                                            hover = TRUE, spacing = 'xs')


