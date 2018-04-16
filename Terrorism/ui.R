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

mycss <- "
.irs-bar,
.irs-bar-edge,
.irs-single,
.irs-grid-pol {
  background: red;
  border-color: red;
}"

shinyUI(fluidPage(theme = shinytheme("superhero"),
  
  navbarPage("Global Terrorism Database",
             tabPanel("About",
                      icon = icon("globe"),
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
                      mainPanel(
                        h4("Global Terror Attacks Time Series"),
                        plotOutput(outputId = "GTA", width = "150%", height = 525),
                        tags$style(mycss),
                        uiOutput("testSlider"),
                        h4("Global Terror Attacks: Fast Facts"),
                        verbatimTextOutput(outputId = "GTAFF")
                      )),
             tabPanel("Terrorism by Region"),
             tabPanel("Most Notorious Groups"),
             tabPanel("Terror Activities in USA"),
             tabPanel("World Terrorism Spread")
            )
  )
)