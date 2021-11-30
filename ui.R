header <- dashboardHeader(title = "Renewable Energy Engineering",
                          titleWidth = 450)


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(text = "World Map for Energy",
             tabName = "consumption",
             icon = icon("globe")),
    
    
    menuItem(text = "Consumpsion Energy 30 years",
             tabName = "plot0",
             badgeLabel = "New",
             badgeColor = "red",
             icon = icon("dashboard")),
    
    
    menuItem(text = "Most and Lowest Energy",
             tabName = "plota",
             icon = icon("chart-bar")),
    
    menuItem(text = "Non Renewable VS Renewable",
             tabName = "plotb",
             icon = icon("uncharted")),
    
    menuItem(text = "Distribution of Energy",
             tabName = "plotc",
             icon = icon("fan")),
    
    menuItem(text = "Database Energy Around the World",
             tabName = "data0",
             icon = icon("table")),
    
    menuItem(text = "Database Country",
             tabName = "data",
             icon = icon("database")),
    
    menuItem(text = "Database Continent",
             tabName = "data1",
             icon = icon("database")),
    
    menuItem(text = "Database Non Renewables",
             tabName = "data2",
             icon = icon("database")),
    
    menuItem(text = "Database Renewable",
             tabName = "data3",
             icon = icon("database")),
    
    menuItem(text = "Database Total Power Generation",
             tabName = "data4",
             icon = icon("database")),
    
    sliderInput(
      inputId="worldmap_year",
      label="Select Year:",
      min=1990, max=2015,
      value=2015,
      sep=""
    ),
    radioButtons(
      inputId="worldmap_data",
      label="Select Data:", 
      choices=list("Renewable Share, Electricity Output (%)"="Share.Output",
                   "Renewable Share, TFEC (%)"="Share.TFEC",
                   "Renewable Electricity Output (GWh)"="Renewable.Output",
                   "Renewable TFEC (GWh)"="Renewable.TFEC")
    )
   
  
  )
)

body <- dashboardBody(
  
  tabItems(
    
    tabItem(tabName = "consumption",
            
            h2("DISTRIBUTION OF ENERGY USE IN ECONOMIC GROUPS BY COUNTRIES"),
            br(),
            fluidRow(
              
              valueBox(round(sum(re_eng_countries$Hydro.TWh.)), 
                       "HYDROELECTRIC POWER", 
                       icon = icon("charging-station"),
                       color = "green",
                       width = 6),
              valueBox(round(sum(re_eng_countries$Biofuel.TWh.)),
                       "BIOFUEL ENERGY", 
                       icon = icon("leaf"),
                       color = "olive",
                       width = 6),
              valueBox(round(sum(re_eng_countries$Solar.PV..TWh.)), 
                       "SOLAR PHOTOVOLTAIC", 
                       icon = icon("solar-panel"),
                       color = "green",
                       width = 6),
              valueBox(round(sum(re_eng_countries$Geothermal..TWh.)), 
                       "GEOTHERMAL ENERGY", 
                       icon = icon("broadcast-tower"),
                       color = "olive",
                       width = 6)),
            
            tabPanel("WORLD MAP", icon=icon('globe'),
                     br(),
                     
                     fluidRow(h1("Global Energy consumption & Renewable Energy")),
                     fluidRow(
                       column(3,
                              br(),
                              "TFEC = Total Final Energy Consumption",
                              br(),
                              br(),
                              "Explore data from the World Bank's",
                              br(),
                              "Sustainable Energy for All Initiative!",
                              br(),
                              br(),
                              "Browse by Region, Country and Income Group.",
                              br(),
                              "Hover over maps and plots for details."
                       ),
                       column(2,
                              br(),
                              wellPanel(h4(strong("Top Countries By"))),
                                        h4(strong(htmlOutput("worldmap_stats_data")))
                       ),
                       column(2,
                              br(),
                              wellPanel(h4(strong(htmlOutput("worldmap_stats_1"))),
                                        h4(strong(htmlOutput("worldmap_stats_12")))
                              )
                       ),
                       column(2,
                              br(),
                              wellPanel(h4(strong(htmlOutput("worldmap_stats_2"))),
                                        h4(strong(htmlOutput("worldmap_stats_22")))
                              )
                       ),
                       column(2,
                              br(),
                              wellPanel(h4(strong(htmlOutput("worldmap_stats_3"))),
                                        h4(strong(htmlOutput("worldmap_stats_32")))
                              )
                       )
                     )),
                              plotlyOutput("worldmap2",height = "700px")),
                     
    
    tabItem(tabName = "plot0", 
            
            fluidRow(
              box( width = 12, title = "Energy Consumption changed for the Continents/Regions", solidHeader = T, status = "primary",
                   tabBox(id = "tabset1",width = 12,
                          tabPanel("Energy for Continents", 
                                   
                                   plotlyOutput(outputId = "plot_col",height = "700px")),
                                   
                          tabPanel("Energy for Regions", 
                                   
                                   plotlyOutput(outputId = "plot_colne", height = "700px"))
                          ))
              )
            ),
    

    
    tabItem(tabName = "plota", 
            
            fluidRow(
              box( width = 12, title = "Which countries have the highest and Lowest energy Consumption", solidHeader = T, status = "primary",
                   tabBox(id = "tabset1",width = 12,
                          tabPanel("Countries Energy Consumption over past 30 years", 
                                   plotlyOutput(outputId = "energy30",height = "700px")),
                          tabPanel("10 countries with the highest energy consumption", 
                                   plotlyOutput(outputId = "highest_energy", height = "700px")),
                          tabPanel("10 countries with the lowest energy consumption", 
                                   plotlyOutput(outputId = "lowest_energy", height = "700px")))
              )
            )),
    
    tabItem(tabName = "plotb",
            
            fluidRow(
              box( width = 12, title = "Renewable sources vs Non-Renewable sources Energy Generation", solidHeader = T, status = "primary",
                   tabBox(id = "tabset1",width = 12,
                          tabPanel("Energy Type", 
                                   plotlyOutput(outputId = "revs",height = "700px")),
                          tabPanel("Which Energy Source Produces More Energy", 
                                   plotlyOutput(outputId = "energyty", height = "700px")))
                   
                   
              )
            )),
    
    tabItem(tabName = "plotc",
            
    fluidRow(
      box( width = 12, title = "Has Renewable Energy use Grown in Continents/Countries", solidHeader = T, status = "primary",
           tabBox(id = "tabset1",width = 12,
                  tabPanel("Distribution Of Energy Use In Economic", 
                           plotlyOutput(outputId = "new1",height = "800px")),
                  tabPanel("Renewable Energy in use since 1990 - 2020", 
                           plotlyOutput(outputId = "plot_22", height = "800px")),
                  tabPanel("Top 20 Countries that used Renewable energy", 
                           plotlyOutput(outputId = "plot_2", height = "800px")))
           
           
      )
    )),
            
    
    tabItem(tabName = "data0",
            dataTableOutput(outputId = "data_energy0")),
    tabItem(tabName = "data",
            dataTableOutput(outputId = "data_energy")),
    tabItem(tabName = "data1",
            dataTableOutput(outputId = "data_energy1")),
    tabItem(tabName = "data2",
            dataTableOutput(outputId = "data_energy2")),
    tabItem(tabName = "data3",
            dataTableOutput(outputId = "data_energy3")),
    tabItem(tabName = "data4",
            dataTableOutput(outputId = "data_energy4"))
  )
)

dashboardPage(
  skin = "green",
  header = header,
  body = body,
  sidebar = sidebar
)








