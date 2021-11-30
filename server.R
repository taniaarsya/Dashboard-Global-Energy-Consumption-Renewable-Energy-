function(input, output) {
  
  output$worldmap2 = renderPlotly({
    
    clean1 = clean %>% filter(., Year==input$worldmap_year)
    
    plot_geo(clean1) %>% 
      add_trace(z = clean1[, input$worldmap_data], color = clean1[, input$worldmap_data], 
                colors = 'Greens',
                text = clean1$Country, 
                locations = clean1$Code, 
                marker = list(line = list(color = toRGB("grey"), width = 0.5))) %>% 
      colorbar(title = '', ticksuffix = '') %>% 
      layout(geo = list(
        showframe = FALSE,
        showcoastlines = FALSE,
        projection = list(type = 'Mercator')
      ))
    
  })
  
  ### WORLD MAP STATS BOX
  
  
  output$worldmap_stats_data = renderText(input$worldmap_data)
  
  output$worldmap_stats_1 = renderText({
    stats = clean %>% filter(., Year==input$worldmap_year) %>% 
      arrange(desc(eval(as.symbol(input$worldmap_data)))) %>% 
      select(., Country)
    
    paste("1. ", as.character(stats[1,1]))
    
  })
  
  output$worldmap_stats_12 = renderText({
    stats = clean %>% filter(., Year==input$worldmap_year) %>% 
      arrange(desc(eval(as.symbol(input$worldmap_data))))
    
    paste("Value: ", as.character(round(stats[1,input$worldmap_data], digits=2)))
    
  })
  
  output$worldmap_stats_2 = renderText({
    stats = clean %>% filter(., Year==input$worldmap_year) %>% 
      arrange(desc(eval(as.symbol(input$worldmap_data)))) %>% 
      select(., Country)
    
    paste("2. ", as.character(stats[2,1]))
    
  })
  
  output$worldmap_stats_22 = renderText({
    stats = clean %>% filter(., Year==input$worldmap_year) %>% 
      arrange(desc(eval(as.symbol(input$worldmap_data))))
    
    paste("Value: ", as.character(round(stats[2,input$worldmap_data], digits=2)))
    
  })
  
  output$worldmap_stats_3 = renderText({
    stats = clean %>% filter(., Year==input$worldmap_year) %>% 
      arrange(desc(eval(as.symbol(input$worldmap_data)))) %>% 
      select(., Country)
    
    paste("3. ", as.character(stats[3,1]))
    
  })
  
  output$worldmap_stats_32 = renderText({
    stats = clean %>% filter(., Year==input$worldmap_year) %>% 
      arrange(desc(eval(as.symbol(input$worldmap_data))))
    
    paste("Value: ", as.character(round(stats[3,input$worldmap_data], digits=2)))
    
  })
  
  output$plot_col <- renderPlotly({
    
    energy_progressed <- ggplot(merge(gather(country_consump,country,energy_consump, China:"United Arab Emirates"),aggregate(energy_consump ~ country ,gather(country_consump,country,energy_consump, China:"United Arab Emirates"), FUN = "sum") %>% 
                                        arrange(-energy_consump) %>%
                                        select(country) %>% 
                                        head(10)) %>% 
                                  group_by(country) %>% 
                                  arrange(country,Year) %>% 
                                  mutate(percent_increase = (energy_consump - lag(energy_consump, default = first(energy_consump)))/lag(energy_consump, default = first(energy_consump))),
                                aes(Year,percent_increase,color = country)) + geom_point(alpha = 0.7, show.legend = FALSE) + facet_wrap(~country)+ geom_smooth(method = "lm")+labs(title =  "Changes in Consumption of Energy Progressed")+theme_minimal()
    
    ggplotly(energy_progressed)
  })
  
  
  output$plot_colne <- renderPlotly({
    Energy_Consumption<- 
      ggplot(filter(gather(cont_consump,continent,Twh,World:CIS),
                    continent != "World"& continent != "CIS" & continent != "BRICS" & continent !=  "OECD"),aes(Year,Twh, color = continent))+
      geom_point(size = 2,alpha = .5) + geom_smooth()+labs(title = "Progress Energy on the Continent")+ scale_color_brewer(palette = "Set3")+facet_wrap(~ continent)+theme_minimal()
    ggplotly(Energy_Consumption)
  })
  
  
  output$energy30 <- renderPlotly({
    Energy_30years <- ggplot(gather(country_consump,country,energy_consump, China:"United Arab Emirates"),aes(country,energy_consump, fill = as.factor(Year))) +
    geom_col() + labs(title = "Countries Energy Consumption over past 30 years")+theme(axis.text.x = element_text(angle = 90))
    
    ggplotly(Energy_30years)
  })
  
  output$highest_energy <- renderPlotly({
    top10_highest <- aggregate(energy_consump ~ country ,gather(country_consump,country,energy_consump, China:"United Arab Emirates"), FUN = "sum") %>% 
      arrange(-energy_consump) %>% 
      head(10)%>% 
      ggplot(aes(country,energy_consump, fill= country))+geom_col()+ 
      theme(axis.text.x = element_text(angle = 90))+
      labs(title = "10 Countries With the Highest Energy Consumption")+theme_bw()+scale_fill_brewer(palette = 5) 
    
    ggplotly(top10_highest)
  })
  
  
  output$lowest_energy <- renderPlotly({
    Top10_lowest = aggregate(energy_consump ~ country ,gather(country_consump,country,energy_consump, China:"United Arab Emirates"), FUN = "sum") %>% 
      arrange(energy_consump) %>% 
      head(10) %>% 
      ggplot(aes(country,energy_consump, fill= country))+geom_col()+ 
      theme(axis.text.x = element_text(angle = 90))+labs(title = "10 Countries With the Lowest Energy Consumption")+theme_bw()+scale_fill_brewer(palette = 5) 
    
    ggplotly(Top10_lowest)
  })

  output$revs <- renderPlotly({
    type_energy <- filter(rbind(nonre_eng,re_eng_tot_gen),Mode.of.Generation != "Total") %>% 
      ggplot(aes(Mode.of.Generation,Contribution..TWh.,fill = energy_type))+geom_col()+
      theme(axis.text.x = element_text(angle = 90))+labs(title = "Energy Type")+theme_bw()+scale_fill_brewer(palette = 5) 
    
    ggplotly(type_energy)
  })
  
  
  output$energyty <- renderPlotly({
    re_vs <- filter(rbind(nonre_eng,re_eng_tot_gen),Mode.of.Generation != "Total") %>% 
      ggplot(aes(energy_type,Contribution..TWh.,fill = energy_type))+geom_col()+
      theme(axis.text.x = element_text(angle = 90))+labs(title = "Renewable sources vs Non-Renewable sources Energy Generation")+theme_bw()+scale_fill_brewer(palette = 5) 
    
    ggplotly(re_vs)
  })
  
  output$new1 <- renderPlotly({
    new <-ggplot(oecd_countries_90_20,aes(Year,Twh,fill = country))+ geom_col()+theme_classic() 
    
    ggplotly(new)
  })

  
  
  output$plot_22 <- renderPlotly({
    energyyears <- ggplot(gather(re_eng_perc_97_17,industry, percent_change,hydro_percent:geo_percent),aes(Year,percent_change, color = industry))+geom_line()+theme_bw()+
      labs(title = "Which renewable energy type has increased the most in use since 1990 -2020")
    ggplotly(energyyears)
    
  })
  
  output$plot_2 <- renderPlotly({
    renew <- gather(re_eng_countries,renew_source,energy_Twh,Hydro.TWh.:Total..TWh.)%>% 
      ggplot(aes(Country,energy_Twh, fill = renew_source))+ geom_col() + theme(axis.text.x = element_text(angle = 90))+theme_minimal()
    
    ggplotly(renew)
  })
  
  output$data_energy0 <- renderDataTable({
    datatable(data = clean,options = list(scrollx = T))
  })
  
  output$data_energy <- renderDataTable({
    datatable(data = country_consump,options = list(scrollx = T))
  })
  output$data_energy1 <- renderDataTable({
    datatable(data = cont_consump,options = list(scrollx = T))
  })
  output$data_energy2 <- renderDataTable({
    datatable(data = nonre_eng,options = list(scrollx = T))
  })
  output$data_energy3 <- renderDataTable({
    datatable(data = re_eng_gen_90_17,options = list(scrollx = T))
  })
  output$data_energy4 <- renderDataTable({
    datatable(data = re_eng_countries,options = list(scrollx = T))
  })

}


#filter(Mode.of.Generation == input$Mode.of.Generation)