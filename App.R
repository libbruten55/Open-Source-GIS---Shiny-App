#################################################################
##           Practical 6 for GEOM184 - Open Source GIS         ##
##                      14/11/2025                             ##
##                  Creating a ShinyApp                        ##
##                         App.R                               ##
##        code by Diego Panici (d.panici@exeter.ac.uk)         ##
#################################################################


# Load packages ----
library(shiny)
library(leaflet)
library(sf)
library(raster)
library(ggplot2)
library(ggiraph)
library(RColorBrewer)
library(terra)
library(leafem)

options(shiny.maxRequestSize = 1000 * 1024^2)


source("Global.R")

# Define UI for visualisation ----
source("UI.R")

ui <- navbarPage("Instream large wood on the River Torridge", id = 'nav',
                 tabPanel("Map", 
                          div(class="outer",
                              leafletOutput("map", height = "calc(100vh - 70px)")
                          )
                 )
)

# Define the server that performs all necessary operations ----
server <- function(input, output, session){
  source("Server.R", local = TRUE)
}

shinyApp(ui, server)