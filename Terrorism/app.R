library(shiny)
library(readxl)
library(readr)
library(tidyverse)
library(styler)
library(purrr)
library(ggthemes)
library(ggmap)
library(ggplot2)
library(ggthemes)
library(purrr)
library(shinythemes)
library(rworldmap)
library(ggrepel)
library(leaflet)
library(rsconnect)
library(RColorBrewer)

slider_bar_css <- "
.irs-bar,
.irs-bar-edge,
.irs-single,
.irs-grid-pol {
background: red;
border-color: red;
}"

ui <- fluidPage(theme = shinytheme("united"),
                  
                  navbarPage("Global Terrorism Database",
                             tabPanel("About",
                                      icon = icon("compass"),
                                      h3("Global Terrorism Data Explorer"),
                                      p("This Shiny App is for summarzing and visualizing data of terrorist events around the world since 1970. 
                                        The open-source database is from START (Study of Terrorism and Responses to Terrorism), 
                                        a Department of Homeland Security Center of Excellence led by the University of Maryland.",
                                        "A a link to the dataset and more information can be found", 
                                        a("here", href="https://www.kaggle.com/START-UMD/gtd", ".")),
                                      h4("Data Dictionary"),
                                      mainPanel(
                                        verbatimTextOutput(outputId = "Data"),
                                        br(),
                                        h4("Download Data"),
                                        p("Please click the download button below to have a local copy of the dataset."),
                                        downloadButton(outputId = "downloadData", label = "Download"),
                                        hr(),
                                        p("Date last updated: 04/15/2018"),
                                        p("Authors: Steven Aguilar and Sean Curl"))
                             ),
                             tabPanel("Global Terror Attacks",
                                      icon = icon("globe"),
                                      mainPanel(
                                        h4("Global Terror Attacks Time Series"),
                                        plotOutput(outputId = "GTA", width = "150%", height = 525),
                                        tags$style(slider_bar_css),
                                        uiOutput("testSlider"),
                                        h4("Global Terror Attacks: Fast Facts"),
                                        verbatimTextOutput(outputId = "GTAFF")
                                      )),
                             tabPanel("Terrorism by Region",
                                      icon = icon("bomb"),
                                      mainPanel(
                                        h4("Heat Map"),
                                        plotOutput("heat", width = "150%", height = 525),
                                        hr(),
                                        h4("Time-Series Analysis"),
                                        uiOutput("TBR"),
                                        plotOutput("line", width = "150%", height = 525)
                                      )
                             ),
                             tabPanel("Notorious Groups",
                                      icon = icon("group"),
                                      h4("Most Active Terrorist Groups"),
                                      plotOutput("NG", width = "100%", height = 500),
                                      hr(),
                                      fluidRow(
                                        column(3,
                                               uiOutput("Group")
                                        ),
                                        column(4, offset = 1,
                                               uiOutput("Decade")
                                        )
                                      )),
                             tabPanel("Interactive Map",
                                      icon = icon("map"),
                                      leafletOutput("Interactive", width = "100%", height = 544)
                             )
                  )
                  )


server <- function(input, output) {
  terrorism <- read_xlsx("Data/terrorism.xlsx", col_types = "text") %>%
    select("Year" = iyear,
           "Country" = country_txt, 
           "Region" = region_txt, 
           "Killed" = nkill, 
           "Wounded" = nwound, 
           "Group" = gname, 
           "City" = city,
           "lat" = latitude,
           "long" = longitude) %>%
    mutate(Killed = as.numeric(Killed),
           Wounded = as.numeric(Wounded),
           lat = as.numeric(lat),
           long = as.numeric(long),
           Year = as.numeric(Year)) %>%
    mutate(Casualties = Killed + Wounded)
  
  output$Data <- renderPrint({
    glimpse(terrorism, width = getOption("width"))
  })
  
  output$downloadData <- downloadHandler(
    filename = function(){
      paste("Terrorism-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file){
      write_csv(terrorism, file)
    },
    contentType = "text/csv"
  )
  
  GTA_graph_data <- reactive({
    terrorism %>%
      filter(Year %in% input$Year) %>%
      group_by(Country, long, lat, Casualties) %>%
      summarise(total = sum(Casualties))
  })
  
  output$testSlider <- renderUI({
    sliderInput("Year", "Select a Year", min = 1970, max = 2016, 
                value = 1970, step = 1, sep = "", width = "100%",
                animate = animationOptions(
                  interval = 4000, loop = TRUE, playButton = icon("play-circle"),
                  pauseButton = icon("pause")
                ))
  })
  
  output$GTA <- renderPlot({
    req(input$Year)
    if (input$Year != "1993") {
      mp <- NULL
      mapWorld <- borders("world", colour="gray50", fill="gray50")
      mp <- ggplot() +  mapWorld +
        geom_point(data = GTA_graph_data(), mapping = aes(x=long, 
                                                          y=lat, 
                                                          group = Country, 
                                                          size = total, 
                                                          color = "red"), 
                   show.legend = FALSE) +
        theme_classic() +
        theme(axis.title = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.line = element_blank()) +
        ggtitle(paste("Global Terror Attacks:", input$Year))
      mp
    } else{
      return(NULL)
    }
  })
  
  country_highest_attacks <- reactive({
    terrorism %>%
      filter(Year %in% input$Year) %>%
      group_by(Country) %>%
      summarise(n = n()) %>%
      arrange(desc(n)) %>%
      slice(1)
  })
  
  region_highest_attacks <- reactive({
    terrorism %>%
      filter(Year %in% input$Year) %>%
      group_by(Region) %>%
      summarise(n = n()) %>%
      arrange(desc(n)) %>%
      slice(1)
  })
  
  highest_killed <- reactive({
    terrorism %>%
      filter(Year %in% input$Year) %>%
      group_by(Year, Country, Killed) %>%
      summarise(max(Killed)) %>%
      arrange(desc(Killed)) %>%
      head(1)
  })
  
  output$GTAFF <- renderPrint({
    req(input$Year)
    print(paste("Country with Highest Terrorist Attacks:", country_highest_attacks()$Country))
    print(paste("Region with Highest Terrorist Attacks:", region_highest_attacks()$Region))
    print(paste("Attack with Highest Casualties:", highest_killed()$`max(Killed)`,
                "casualties in", highest_killed()$Country))
    
  })
  
  output$TBR <- renderUI({
    selectizeInput("reg", "Choose a region (or multiple)", 
                   choices = unique(terrorism$Region), multiple = TRUE,
                   selected = "North America")
  })
  
  TBR_graph_data <- reactive({
    terrorism %>%
      filter(Region %in% input$reg) %>%
      group_by(Region, Year) %>%
      summarise(total = sum(n())) %>%
      mutate(Year = as.numeric(Year))
  })
  
  output$line <- renderPlot({
    ggplot(TBR_graph_data(), 
           aes(x = Year, 
               y = total, 
               color = Region), show.legend = FALSE) +
      geom_line(stat = "identity", show.legend = FALSE) +
      geom_text_repel(
        data = subset(TBR_graph_data(), Year == 2016),
        aes(x = Year,
            y = total,
            color = Region,
            label = Region),
        show.legend = FALSE,
        inherit.aes = FALSE,
        size = 6,
        nudge_x = 200,
        segment.color = NA
      ) +
      xlab("Year") +
      ylab("Count") +
      ggtitle("Terrorist Attacks by Region") +
      scale_x_continuous(breaks = c(1970, 1980, 1990, 2000, 2010, 2020), 
                         limits = c(1970, 2016)) +
      theme_classic()
  })
  
  graph_data <- terrorism %>%
    select(everything(), -lat, -long) %>%
    mutate(Country = if_else(Country %in% c('Antigua and Barbuda',
                                            'Bosnia-Herzegovina',
                                            'Czechoslovakia',
                                            'East Germany (GDR)',
                                            'East Timor','Hong Kong',
                                            'International',
                                            'Macau',
                                            'New Hebrides',
                                            'North Yemen',
                                            "People's Republic of the Congo",
                                            'Republic of the Congo',
                                            'Rhodesia',
                                            'Serbia-Montenegro',
                                            'Slovak Republic',
                                            'South Vietnam',
                                            'South Yemen',
                                            'Soviet Union',
                                            'St. Kitts and Nevis',
                                            'St. Lucia',
                                            'Trinidad and Tobago',
                                            'United Kingdom',
                                            'United States',
                                            'Vatican City',
                                            'West Bank and Gaza Strip',
                                            'West Germany (FRG)',
                                            'Yugoslavia',
                                            'Zaire'
    ),
    case_when(
      Country == "Antigua and Barbuda" ~ "Antigua",
      Country == "Bosnia-Herzegovina" ~ "Bosnia and Herzegovina",
      Country == "Czechoslovakia" ~ "Czech Republic",
      Country == "East Germany (GDR)" ~ "Germany",
      Country == "East Timor" ~ "Timor-Leste",
      Country == "Hong Kong" ~ "China",
      Country == "International" ~ "Somalia",
      Country == "Macau" ~ "China",
      Country == "New Hebrides" ~ "Vanuatu",
      Country == "North Yemen" ~ "Yemen",
      Country == "People's Republic of the Congo" ~ "Democratic Republic of the Congo",
      Country == "Republic of the Congo" ~ "Democratic Republic of the Congo",
      Country == "Rhodesia" ~ "Zimbabwe",
      Country == "Serbia-Montenegro" ~ "Serbia",
      Country == "Slovak Republic" ~ "Slovakia",
      Country == "South Vietnam" ~ "Vietnam",
      Country == "South Yemen" ~ "Yemen",
      Country == "Soviet Union" ~ "Russia",
      Country == "St. Kitts and Nevis" ~ "Saint Kitts",
      Country == "St. Lucia" ~ "Saint Lucia",
      Country == "Trinidad and Tobago" ~ "Trinidad",
      Country == "United Kingdom" ~ "UK",
      Country == "United States" ~ "USA",
      Country == "Vatican City" ~ "Vatican",
      Country == "West Bank and Gaza Strip" ~ "Palestine",
      Country == "West Germany (FRG)" ~ "Germany",
      Country == "Yugoslavia" ~ "Bosnia and Herzegovina",
      Country == "Zaire" ~ "Democratic Republic of the Congo"
    ), 
    Country)) %>%
    group_by(Country) %>%
    summarise(total = sum(n())) %>%
    mutate(total = log1p(total))
  
  mapWorld <- borders("world", colour="gray50", fill="gray50")
  
  map <- mapWorld$data %>%
    rename("Country" = region )
  
  final <- merge(map, graph_data, by = "Country")
  
  output$heat <- renderPlot({
    
    ggplot(final, aes(map_id = Country)) + 
      geom_map(aes(fill = total), map = mapWorld$data, color='grey', size=0.5) + 
      expand_limits(x = final$long, y = final$lat) +
      theme_few() +
      theme(legend.position = "bottom",
            axis.ticks = element_blank(), 
            axis.title = element_blank(), 
            axis.text =  element_blank()) +
      scale_fill_gradient(low="white", high="darkred",
                          breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                          labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                          name = "log1p(Total)") +
      guides(fill = guide_colorbar(barwidth = 30, barheight = .5))
    
  })
  
  
  output$Group <- renderUI({
    checkboxGroupInput("Group", "Choose a Terrorist Group:", choices = c("Taliban", "Shining Path (SL)",
                                                                         "Islamic State of Iraq and the Levant (ISIL)", 
                                                                         "Farabundo Marti National Liberation Front (FMLN)", 
                                                                         "Al-Shabaab",
                                                                         "Irish Republican Army (IRA)",
                                                                         "Revolutionary Armed Forces of Colombia (FARC)",
                                                                         "New People's Army (NPA)",
                                                                         "Kurdistan Workers' Party (PKK)",
                                                                         "Boko Haram") )
  })
  
  output$Decade <- renderUI({
    checkboxGroupInput("Decade", "Choose a Decade:", choices = c("1970", "1980", "1990", "2000", "2010"))
  })
  
  Group_graph_data <- reactive({
    terrorism %>%
      filter(!Group == "Unknown" & Group %in% c("Taliban", "Shining Path (SL)",
                                                "Islamic State of Iraq and the Levant (ISIL)", 
                                                "Farabundo Marti National Liberation Front (FMLN)", 
                                                "Al-Shabaab",
                                                "Irish Republican Army (IRA)",
                                                "Revolutionary Armed Forces of Colombia (FARC)",
                                                "New People's Army (NPA)",
                                                "Kurdistan Workers' Party (PKK)",
                                                "Boko Haram")) %>%
      mutate(Decade = case_when(
        Year %in% c(1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979) ~ "1970",
        Year %in% c(1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989) ~ "1980",
        Year %in% c(1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999) ~ "1990",
        Year %in% c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009) ~ "2000",
        Year %in% c(2010, 2011, 2012, 2013, 2014, 2015, 2016) ~ "2010"
      )) %>%
      filter(Decade %in% input$Decade & Group %in% input$Group) %>%
      group_by(Group, long, lat) %>%
      summarise(total = n()) %>%
      arrange(desc(total))
  })
  
  
  output$NG <- renderPlot({
    validate(                      
      need(input$Group != "", "Please select a Group"),
      need(input$Decade != "", "Please select a Decade")
    )
    req(input$Group, input$Decade)
    lp <- NULL
    mapWorld <- borders("world", colour="gray50", fill="gray50")
    
    lp <- ggplot(data = Group_graph_data(), 
                 mapping = aes(x=long, y=lat)) +  mapWorld +
      geom_point(aes(color = Group), 
                 show.legend = FALSE) +
      geom_label_repel(data = Group_graph_data()[!base::duplicated(Group_graph_data()$Group),],
                       aes(long, lat, 
                           label = Group, 
                           color = Group),
                       show.legend = FALSE,
                       inherit.aes = FALSE) +
      theme_classic() +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank()) +
      ggtitle("Events by the Top 10 Terrorist Organizations since 1970")
    lp
    
  })
  
  pal <- rev(brewer.pal(9, "Reds"))
  
  int <- terrorism %>%
    filter(Casualties > 0 & Year > 2010) %>%
    na.omit(Casualties) %>%
    mutate(Casualties = log1p(Casualties))
  
  pal <- colorNumeric(
    palette = pal,
    domain = int$Casualties
  )
  
  output$Interactive <- renderLeaflet({
    leaflet(int) %>% 
      addProviderTiles(providers$Esri.WorldStreetMap) %>%
      addMiniMap(tiles = providers$Esri.WorldStreetMap,
                 toggleDisplay = TRUE) %>%
      setView(lng = -73.935242, lat = 40.730610, zoom = 4)  %>%
      addCircles(~long, ~lat, weight = 1,
                 radius = ~Casualties*25000, label = ~Group,
                 color = ~pal(Casualties)) %>%
      addLegend("bottomleft", pal = pal, values = ~Casualties,
                opacity = 1)
    
    
  })
  
}

shinyApp(ui, server)
