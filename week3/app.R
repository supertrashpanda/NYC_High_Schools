library(shinyauthr)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyWidgets)


# Main login screen
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h3("Leave Your Feedbacks below", class = "text-center", style = "padding-top: 0;color:#333; font-weight:400;"),
                   textInput(inputId = "response", label = ""),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("submit", "Submit", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;",
                                 onclick ="window.open('https://supertrashpanda.shinyapps.io/track_user/', '_self')"
                                  ),
                     br(),
                     br()
                   ))
)



header <- dashboardHeader( title = span("Find NYC Highschool Programs",style="font-size:18px"),titleWidth=300,uiOutput("upbar"))

sidebar <- dashboardSidebar(width=300) 
body <- dashboardBody(shinyjs::useShinyjs(),
                      tags$head(includeHTML("google-analytics.html")
                                ),
tags$style(type = "text/css", "#mymap {height: calc(200vh - 200px) !important;}
                                                        div.leaflet-control-search.search-exp.leaflet-control {z-index:-10000000000 !important;}
                                 .leaflet-control {z-index:0};"),
loginpage)


ui<-dashboardPage(header, sidebar,body, skin = "black")

server <- function(input, output, session) {
NULL
  

}

shinyApp(ui = ui, server = server)



