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
library(xlsx)
library(rworldmap)

shinyServer(function(input, output) {
  terrorism <- read_xlsx("C:/Data/globalterrorismdb_0617dist.xlsx", col_types = "text") %>%
    select("Year" = iyear, 
           "Month" = imonth, 
           "Day" = iday, 
           "Country" = country_txt, 
           "Region" = region_txt, 
           "AttackType" = attacktype1_txt, 
           "Target" = target1, 
           "Killed" = nkill, 
           "Wounded" = nwound, 
           "Summary" = summary, 
           "Group" = gname, 
           "Target_Type" = targtype1_txt, 
           "Weapon_type" = weapsubtype1_txt, 
           "Motive" = motive,
           "City" = city,
           "lat" = latitude,
           "long" = longitude) %>%
    mutate(Killed = as.numeric(Killed),
           Wounded = as.numeric(Wounded),
           lat = as.numeric(lat),
           long = as.numeric(long)) %>%
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
              value = 2016, step = 1, sep = "", width = "100%",
              animate = animationOptions(
                interval = 2000, loop = TRUE, playButton = icon("play-circle"),
                pauseButton = icon("pause")
              ))
  })
  
  output$GTA <- renderPlot({
    req(input$Year)
    if (input$Year != "1993") {
    mp <- NULL
    mapWorld <- borders("world", colour="gray50", fill="gray50")
    mp <- ggplot() +  mapWorld +
      geom_point(data = GTA_graph_data(), mapping = aes(x=long, y=lat, 
                                                        group = Country, size = total, color = "red"), 
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
    terrorism <- read_xlsx("C:/Data/globalterrorismdb_0617dist.xlsx", col_types = "text") %>%
      select("Year" = iyear, 
             "Month" = imonth, 
             "Day" = iday, 
             "Country" = country_txt, 
             "Region" = region_txt, 
             "AttackType" = attacktype1_txt, 
             "Target" = target1, 
             "Killed" = nkill, 
             "Wounded" = nwound, 
             "Summary" = summary, 
             "Group" = gname, 
             "Target_Type" = targtype1_txt, 
             "Weapon_type" = weapsubtype1_txt, 
             "Motive" = motive,
             "City" = city,
             "lat" = latitude,
             "long" = longitude) %>%
      mutate(Killed = as.numeric(Killed),
             Wounded = as.numeric(Wounded),
             lat = as.numeric(lat),
             long = as.numeric(long),
             Casualties = Killed + Wounded)
    
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
               y = total, colour = Region, group = Region)) +
      geom_line(stat = "identity") +
      xlab("Year") +
      ylab("Count") +
      ggtitle("Terrorist Attacks by Region") +
      scale_x_continuous(breaks = c(1970, 1980, 1990, 2000, 2010, 2020), 
                         limits = c(1970, 2016)) +
      theme(axis.text.x=element_text(angle = 90, hjust =1)) +
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
  
})
