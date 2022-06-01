library(shinyauthr)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(sodium)
library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(shinyWidgets)

prgms<-read.csv("https://raw.githubusercontent.com/supertrashpanda/JustCSV/main/restructured_programs.csv",check.names=FALSE)
schools<-read.csv("https://raw.githubusercontent.com/supertrashpanda/JustCSV/main/schools.csv",check.names=FALSE)


pal = colorFactor("Set1", domain = schools$school_accessibility_description)
color_offsel1 = pal(schools$school_accessibility_description)


# Main login screen
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="Your Email Account", label = tagList(icon("user"), "Username")),
                   #passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                     br(),
                     br(),
                     #tags$code("Username: myuser  Password: mypass"),
                     #tags$code("Username: myuser1  Password: mypass1")
                   ))
)

credentials = data.frame(
  username_id = c("myuser", "myuser1"),
  passod   = sapply(c("mypass", "mypass1"),password_store),
  permission  = c("basic", "advanced"), 
  stringsAsFactors = F
)

header <- dashboardHeader( title = "Simple Dashboard", uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(),tags$head(includeHTML("google-analytics.html")),
                      tags$style(type = "text/css", "#mymap {height: calc(200vh - 200px) !important;}"),
                      uiOutput("mymap"))


ui<-dashboardPage(header, sidebar,
                  body, skin = "blue")

server <- function(input, output, session) {
  
  login = FALSE
  USER <- reactiveValues(login = login)
  
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          pasverify<-FALSE
          if(nchar(Username)>0){pasverify<-TRUE}
          
          if(pasverify) {
            USER$login <- TRUE
          } 
        }
      }
    }    
  })
  
 
  
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("fa fa-sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })
  
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE ){ 
      dashboardSidebar(
        collapsed = FALSE,
        chooseSliderSkin("Flat"),
        h3("  Filters",style="padding-left:40px"),
        sliderInput("stu_num",
                    label =tags$span("Number of Total Students:",style = "padding-left:0px;font-weight: bold;font-size:13px;margin-bottom:0px;"),
                    min = 0,
                    max = 6100,
                    value =c(0,6100),
                    ticks=FALSE,
                    sep=""),
        checkboxInput("accessible","Fully Accessible schools", FALSE),
        br(),
        hr(),
        sliderInput("per_seat",
                    label =tags$span("Number of Grade 9 GE Applicants per Seat:",style = "padding-left:0px;font-weight: bold;font-size:13px;margin-bottom:0px;"),
                    min = 0,
                    max = 70,
                    value =c(0,70),
                    ticks=FALSE,
                    sep=""),
        tags$script(HTML("
        $(document).ready(function() {setTimeout(function() {
          supElement = document.getElementById('per_seat').parentElement;
          $(supElement).find('span.irs-max, span.irs-min').remove();}, 10);})
      ")),
        selectInput("priority",
                    "Top priority of program admission:",
                    choices = c(unique(prgms$top_priority),"Not selected"),
                    selected="Not selected"
      )
      )
    }
  })
  output$user<-reactive(input$userName)
  
  
  df<- reactive({
    b<-prgms[which((prgms$total_students>=input$stu_num[1])&(prgms$total_students<=input$stu_num[2])),]
    b<-b[which((b$ge_per_seat>=input$per_seat[1])&(b$ge_per_seat<=input$per_seat[2])),]
    a<-schools
    if(input$accessible){
      b<-b[b$school_accessibility_description=="Fully Accessible",]
    }
    
    
    if(input$priority!="Not selected"){
    b<-b[which(b$top_priority==input$priority),]#this is to perform subsetting on b (prgm-level)
    }
    
    if(nrow(b)<1)
    {return(0)}#no programs
    
    
    b<-b[,1:3]%>%spread(program,program_name)
    b[is.na(b)] <- ""
    b<-b%>%unite("prgms",2:ncol(b), remove = FALSE,sep=", ")
    b$prgms<-gsub(" ,", ",", b$prgms, fixed=TRUE)
    b$prgms<-trimws(b$prgms)
    b$prgms<-sub('\\,*$', '', b$prgms)
    b<-b%>%arrange(dbn)

  
    a<-a[which(a$dbn%in%b$dbn),]
    a<-a%>%arrange(dbn)
    a$prgms<-b$prgms
    
    return(a)
  })
  
  
  
  
  content <- reactive({
    paste("School:","<strong>","<a href=",df()$website,">",df()$school_name,"</a>","</strong>","<br/>",
                     "Total Number of Students:",df()$total_students,"<br/>",
                     "Accessibility:",df()$school_accessibility_description,"<br/>",
          "Number of Programs:",df()$n_prgm,"<br/>",
          "Program Matches:",df()$prgms,"<br/>")%>% 
      lapply(htmltools::HTML)
  })
  
  
  
  
 
  output$map<-renderLeaflet(
    if(!is.numeric(df())){

    leaflet(df()) %>%
      setView(lng=-73.957298,lat=40.742118,zoom = 12 )%>%
      addProviderTiles("Stamen.TonerLite") %>%
      addCircleMarkers(~long,~lat,radius=6,
                       popup =content(),stroke = FALSE,fillOpacity = 0.5,
                       clusterOptions = markerClusterOptions(maxClusterRadius = 30))%>%
      addSearchOSM()
    }
    else{
        leaflet() %>%
          setView(lng=-73.957298,lat=40.742118,zoom = 12 )%>%
          addProviderTiles("Stamen.TonerLite") %>%
          addSearchOSM()
      }
  )
  
 
    
    
  
  

  
  output$mymap <- renderUI({
    if (USER$login == TRUE ) {
      leafletOutput('map', width = "100%", height = "50%")
      
      
      
    }
    else {
      loginpage
    }
  })
  
  output$results <-  DT::renderDataTable({
    datatable(iris, options = list(autoWidth = TRUE,
                                   searching = FALSE))
  })
  
  output$results2 <-  DT::renderDataTable({
    datatable(mtcars, options = list(autoWidth = TRUE,
                                     searching = FALSE))
  })
  
}

shinyApp(ui = ui, server = server)



