
#shinyjs::disable("id_socio_nw3")


observe({
  
  
  if(input$v_dist_vpn_nw == FALSE){
    
    t_categoria <-0
    
    
  }else {
    
    
    bd_e_a <- bd_NW %>%
      dplyr::filter(campo == input$id_tipo_campo_NW2)
    
    
    t_categoria <-unique(bd_e_a$well)
    
    
    
  }
  
  
  
  updateSelectInput(session, "tipo_well_NW2", choices = t_categoria)
  
  
}) 
#### pozo nw

well_data_nw2 <- reactive ({
  
  if (input$v_dist_vpn_nw == FALSE){
    
    bd_eco <- bd_NW %>%
      dplyr::filter(campo == input$id_tipo_campo_NW2)%>%
      dplyr::filter(Socio == input$id_socio_nw3)%>%
      dplyr::filter(E_Brent == input$id_brent_nw3)%>%
      dplyr::filter(tipo_costo == input$id_tipo_costo_nw3) %>%
      dplyr::filter(P_D == input$id_PD_nw3)
    
    
  } else {
    
    bd_eco <-bd_NW %>%
      dplyr::filter(campo == input$id_tipo_campo_NW2) %>%
      dplyr::filter(Socio == input$id_socio_nw3) %>%
      dplyr::filter(E_Brent == input$id_brent_nw3) %>%
      dplyr::filter(tipo_costo == input$id_tipo_costo_nw3) %>%
      dplyr::filter(P_D == input$id_PD_nw3)  %>%
      dplyr::filter(well == input$tipo_well_NW2)  
    
    
    
    
  }
  
  
  
  
  
})


plot_well_chart_nw2 <- function() {
  
  if (input$v_dist_vpn_nw==FALSE){
    
    dt<-data.frame(well_data_nw2())%>%
      dplyr::group_by(well)%>%
      dplyr::summarise(mean_vpn = quantile(VPN_incremental,0.5) / 1000)%>%
      ungroup() %>%
      dplyr::mutate(condicion = ifelse(mean_vpn >= 0, 1, 0))
    
    dt$POZO <- factor(dt$well, levels = unique(dt$well)[order(dt$mean_vpn, decreasing = TRUE)])
    
    
    if(sum(dt$condicion) == nrow(dt)){
      
      plot_ly(dt,x = ~POZO, y = ~mean_vpn,type = 'bar',
              color = ~mean_vpn >= 0, colors = c("#008000","#008000")) %>%
        layout(title = "VPN Pozo a Pozo",
               xaxis = list(title = ""),
               yaxis = list(title = "VPN (MUSD)"),
               autosize = TRUE,
               showlegend = FALSE)
    } else {
      
      plot_ly(dt,x = ~POZO, y = ~mean_vpn,type = 'bar',
              color = ~mean_vpn >= 0, colors = c("#FF0000","#008000")) %>%
        layout(title = "VPN Pozo a Pozo",
               xaxis = list(title = ""),
               yaxis = list(title = "VPN (MUSD)"),
               autosize = TRUE,
               showlegend = FALSE)
      
    }
    
    
  }else if (input$v_dist_vpn_nw==TRUE & input$id_FC_nw3=="D_opex"){
    
    
    
    bd_eco <-data.frame(well_data_nw2())%>%
      dplyr::select(well,fixed_costs,Gross_fluid,productor_well) %>%
      dplyr::rename('Costo fijo'=fixed_costs,
                    'Fluido Total'=Gross_fluid,
                    'Pozo Productor'=productor_well)%>%
      gather("variable", "valor",2:4)
    
    
    colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
    
    fig <- plot_ly(bd_eco, labels = ~variable, values = ~valor, type = 'pie',
                   textposition = 'inside',
                   textinfo = 'label+percent',
                   insidetextfont = list(color = '#FFFFFF'),
                   hoverinfo = 'text',
                   text = ~paste('$', valor, ' billions'),
                   marker = list(colors = colors,
                                 line = list(color = '#FFFFFF', width = 1)),
                   #The 'pull' attribute can also be used to create space between the sectors
                   showlegend = FALSE)
    fig <- fig %>% layout(title = '',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    fig
    
    
  }else if (input$v_dist_vpn_nw==TRUE & input$id_FC_nw3=="fc_t"){
    
    
    bd_eco <-data.frame(well_data_nw2())%>%
      dplyr::summarise(Ingreso=sum(ingreso_ing_b)/1000,
                       Opex= -sum(opex_ing_b)/1000,
                       Capex=-sum(capex_ing_b)/1000,
                       Impuesto=-sum(impuesto_ing_b)/1000,
                       abandono=-sum(abandono_ing_b)/1000,
                       VPN = sum(VPN_incremental,0.5)/1000) %>%
      dplyr::rename('1-Ingreso'=Ingreso,
                    '2-Opex'=Opex,
                    '3-Capex'=Capex,
                    '4-Impuesto'=Impuesto,
                    '5-abandono'=abandono,
                    '6-VPN'=VPN)%>%
      tidyr::gather(variable,Valor_MUSD)
    
    
    measure= c("relative", "relative","relative","relative","relative","total")
    
    
    data = data.frame(bd_eco,measure)
    
    
    
    data %>%
      plot_ly( name = "P50", type = "waterfall",
               measure = ~measure,
               x = ~variable, textposition = "outside", y= ~Valor_MUSD,
               text =~round(Valor_MUSD,1),
               connector = list(line = list(color= "rgb(63, 63, 63)"))) %>%
      layout(title = "Flujo de Caja Descontada",
             xaxis = list(title = ""),
             yaxis = list(title = "VPN (MUSD)"),
             autosize = TRUE,
             showlegend = TRUE)
    
    
  }else if (input$v_dist_vpn_nw==TRUE & input$id_FC_nw3=="fc_d"){
    
    
    
    fig <- bd_prueba_fc_v %>%
      tidyr::gather(year,Valor,8:length(bd_prueba_fc_v)) %>%
      dplyr::filter(E_Brent==input$id_brent_p)%>%
      dplyr::filter(P_D == input$id_PD_nw3)%>%
      dplyr::filter(well==input$tipo_well_NW2)%>%
      dplyr::mutate(Valor = as.numeric(Valor))%>%
      plot_ly(x = ~year, y = ~Valor, type = 'scatter', mode = 'lines', color=~well, colors= "darkgreen")
    
    
    fig <- fig %>% layout(title = "Flujo de Caja Pozo",
                          yaxis = list(title = "Cash Flow (KUSD)"),
                          xaxis = list (title = "year"))
    
    fig
    
    
  } else {
    
  }
  
  
}

# Creating plot for ui side

output$vpn_pozo_nw2 <- renderPlotly({ plot_well_chart_nw2() })


output$table_well_nw2 <- DT::renderDataTable({
  
  bd_f<-data.frame(well_data_nw2())%>%
    dplyr::select(well,VPN_Total,opex_T,LE_year_T,volumen_T_LE,lc_T,dc_T) %>%
    dplyr::group_by(well)%>%
    dplyr::summarise(VPN=round(quantile(VPN_Total,0.5)/1000,1),
                     Opex= round(quantile(opex_T,0.5)/1000,1),
                     LEyear=round(quantile(LE_year_T,0.5),0),
                     VOLLE=round(quantile(volumen_T_LE,0.5),1),
                     LC=round(quantile(lc_T,0.5),1),
                     DC=round(quantile(dc_T,0.5),1)
    ) %>%
    arrange(desc(VPN))
  
  
  
  sketch = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, 'Pozo'),
        th(colspan = 2, 'Total (MUSD)'),
        th(colspan = 2, HTML('Limite Econ&oacutemico')),
        th(colspan = 2, '(USD/BOE)')
      ),
      tr(
        th('VPN'),
        th('OPEX (Gross)'),
        th(HTML('A&ntildeo')),
        th('Vol ECP'),
        th('Costo de levantamiento'),
        th('Costo de desarrollo'),
        
      )
    )
  ))
  
  
  
  
  DT::datatable(bd_f,container = sketch, rownames = FALSE) %>%
    formatStyle('VPN',
                color = 'Black',
                backgroundColor = styleInterval(c(-1,0), c('red', 'yellow',"green"))
    )
  
  
  
})

output$users_vpn1_nw<- renderInfoBox({ 
  
  bd_f<-data.frame(well_data_nw2()) %>%
    dplyr::summarise(VPN = sum(VPN_Total))
  
  c=round(as.numeric(bd_f$VPN) / 1000, 2)
  
  
  infoBox(
    title = "VPN Total",
    value = c,
    subtitle = "MUSD",
    icon = icon("dollar-sign"),
    color ="yellow"
  )
  
  
})

output$users_vpn2_nw<- renderInfoBox({ 
  
  bd_f<-data.frame(well_data_nw2())%>%
    dplyr::summarise(opex=sum(opex_T))
  
  c=round(as.numeric(bd_f$opex)/1000,2)
  
  
  
  infoBox(
    title = "Opex",
    value = c,
    subtitle = "MUSD",
    icon = icon("dollar-sign"),
    color ="green"
  )
  
  
  
})

output$users_vpn3_nw<- renderInfoBox({ 
  
  bd_f<-data.frame(well_data_nw2())%>%
    dplyr::summarise(volumen_T_LE=sum(volumen_T_LE))
  
  c=round(as.numeric(bd_f$volumen_T_LE),2)
  
  
  infoBox(
    title = "Volumen Total LE",
    value = c,
    subtitle = "MBls",
    icon = icon("oil-can"),
    color ="black"
  )
  
  
})





#############################