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

#prgms<-read.csv("/Users/lingyunfan/all_repos/nyc_highschools/NYC_High_Schools/data/restructured_programs.csv",check.names=FALSE)
#schools<-read.csv("/Users/lingyunfan/all_repos/nyc_highschools/NYC_High_Schools/data/schools.csv",check.names=FALSE)

prgms<-read.csv("https://raw.githubusercontent.com/supertrashpanda/JustCSV/main/restructured_programs.csv",check.names=FALSE)
schools<-read.csv("https://raw.githubusercontent.com/supertrashpanda/JustCSV/main/schools.csv",check.names=FALSE)


pal = colorFactor("Set1", domain = schools$school_accessibility_description)
color_offsel1 = pal(schools$school_accessibility_description)

JScode <-
  "$(function() {
    setTimeout(function(){
      var vals = [0];
      var powStart = 0;
      var powStop = 10;
      for (i = powStart; i <= powStop; i++) {
        var val = Math.pow(10, i);
        val = parseFloat(val.toFixed(8));
        vals.push(val);
      }
      $('#revenue').data('ionRangeSlider').update({'values':vals})
    }, 5)})"

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

header <- dashboardHeader( title = span("Find NYC Highschool Programs",style="font-size:15px"), uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(),tags$head(includeHTML("google-analytics.html")),
                      tags$style(type = "text/css", "#mymap {height: calc(200vh - 200px) !important;}"),
                      uiOutput("mymap"))


ui<-dashboardPage(header, sidebar,
                  body, skin = "black")
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
    tags$li(a("Logout", 
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
        
        radioButtons(
          "view", "", 
          choiceNames=list(tags$span(style = "font-size:18px;", "View Each School"),
                    tags$span(style = "font-size:18px;", "View Each Program")),
          choiceValues=list("View Each School","View Each Program"),
          selected = "View Each School", 
          inline = FALSE),
        
        hr(),
        h4("  Filter the high school:",style="padding-left:30px"),
        #tags$head(tags$script(HTML(JScode))),
        setSliderColor(c("LightSlateGrey","LightSlateGrey"), 1:2),
        tags$head(
          tags$style(HTML(".shiny-input-container > label {padding: -10px;}"))
        ),
        sliderInput("stu_num",
                    label =tags$span("Number of Total Students:",style = "padding-left:0px;font-weight: bold;font-size:13px;margin-bottom:0px;"),
                    min = 0,
                    max = 6100,
                    value =c(0,6100),
                    ticks=FALSE,
                    sep=""),
       
        div(style = "margin-top:-10px"),
        checkboxInput("accessible","Fully Accessible Schools", FALSE),
        div(style = "margin-top:-20px"),
        checkboxInput("diversity","Schools with Admission Priority for Diversity", FALSE),
        div(style = "margin-top:-10px"),
        hr(),
        h4("  Filter the programs:",style="padding-left:30px"),
        sliderInput("per_seat",
                    label =tags$span("Number of Grade 9 GE Applicants per Seat:",style = "padding-left:0px;font-weight: bold;font-size:13px;margin-bottom:0px;"),
                    min = 0,
                    max = 70,
                    value =c(0,70),
                    ticks=FALSE,
                    sep=""),
      
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
    if(input$diversity){
      b<-b[!is.na(b$diadetails),]
    }
    
    
    if(input$priority!="Not selected"){
      b<-b[which(b$top_priority==input$priority),]#this is to perform subsetting on b (prgm-level)
    }
    
    if(nrow(b)<1)
    {return(0)}#no programs
    n_prgm=nrow(b)
    
    
    prgm_set<-b[,1:3]%>%spread(program,program_name)
    prgm_set[is.na(prgm_set)] <- ""
    prgm_set<-prgm_set%>%unite("prgms",2:ncol(prgm_set), remove = FALSE,sep=", ")
    prgm_set$prgms<-gsub(" ,", ",", prgm_set$prgms, fixed=TRUE)
    prgm_set$prgms<-trimws(prgm_set$prgms)
    prgm_set$prgms<-sub('\\,*$', '', prgm_set$prgms)
    prgm_set<-prgm_set%>%arrange(dbn)
    
    
    a<-a[which(a$dbn%in%prgm_set$dbn),]
    a<-a%>%arrange(dbn)
    a$prgms<-prgm_set$prgms
    
    return(list(df=a,n_prgm=n_prgm,df2=b))
  })
  
  
  
  
  content <- reactive({
    paste("School:","<strong>","<a href=",df()$df$website," target='_blank'>",df()$df$school_name,"</a>","</strong>","<br/>",
          "Total Number of Students:",df()$df$total_students,"<br/>",
          "Accessibility:",df()$df$school_accessibility_description,"<br/>",
          "Number of Programs:",df()$df$n_prgm,"<br/>",
          "Applicable Programs:","<strong>",df()$df$prgms,"</strong>","<br/>")%>% 
      lapply(htmltools::HTML)
  })
  
  
  content2 <- reactive({
    paste("School:","<strong>","<a href=",df()$df2$website," target='_blank'>",df()$df2$school_name,"</a>","</strong>","<br/>",
                   "Program:","<strong>",df()$df2$program_name,"</strong>","<br/>",
                   df()$df2$prgdesc)%>% 
      lapply(htmltools::HTML)
  })
  
  
  output$table<-renderDataTable(df()$df2[,c("school_name","program_name")])
  
  output$map<-renderLeaflet(
    if((!is.numeric(df()))&(input$view=="View Each School")){
      
      leaflet(df()$df) %>%
        setView(lng=-73.957298,lat=40.742118,zoom = 12 )%>%
        addProviderTiles("Stamen.TonerLite") %>%
        addCircleMarkers(~long,~lat,radius=6,
                         popup =content(),stroke = FALSE,fillOpacity = 0.5,
                         clusterOptions = markerClusterOptions(maxClusterRadius = 10))%>%
        addSearchOSM()
    }
    else if((!is.numeric(df()))&(input$view=="View Each Program")){
      leaflet(df()$df2) %>%
        setView(lng=-73.957298,lat=40.742118,zoom = 12 )%>%
        addProviderTiles("Stamen.TonerLite") %>%
        addCircleMarkers(~long,~lat,radius=6,stroke = FALSE,fillOpacity = 0.5,
                         popup =content2(),clusterOptions = markerClusterOptions(maxClusterRadius = 0.3))%>%
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
    if (USER$login == TRUE) {
      tagList(
      leafletOutput('map', width = "100%", height = "52%"),
      absolutePanel(class = "panel panel-default", fixed = TRUE,
                    draggable = TRUE, top = 60, left = "auto", right = 18, bottom = "auto",
                    width = 330, height = "auto",
                    tags$span("   Number of All Applicable Programs: ",as.character(df()$n_prgm),
                              style="font-size:16px; font-weight: bold;"),
                    hr(),
      
      dataTableOutput('table')
      )
      )
      
      
    }
    else {
      loginpage
    }
  })
  
  
  
}

shinyApp(ui = ui, server = server)



