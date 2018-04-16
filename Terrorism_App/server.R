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
           "lon" = longitude) %>%
    mutate(Killed = as.numeric(Killed),
           Wounded = as.numeric(Wounded),
           lat = as.numeric(lat),
           lon = as.numeric(lon)) %>%
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
      group_by(Country, lon, lat, Casualties) %>%
      summarise(total = sum(Casualties))
  })
  
  output$testSlider <- renderUI({
  sliderInput("Year", "Select a Year", min = min(x), max = max(x), 
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
      geom_point(data = GTA_graph_data(), mapping = aes(x=lon, y=lat, 
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
  
})
