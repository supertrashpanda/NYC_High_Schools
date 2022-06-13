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
#sports<-read.csv("/Users/lingyunfan/all_repos/nyc_highschools/NYC_High_Schools/data/sports.csv")
prgms<-read.csv("https://raw.githubusercontent.com/supertrashpanda/JustCSV/main/restructured_programs.csv",check.names=FALSE)
schools<-read.csv("https://raw.githubusercontent.com/supertrashpanda/JustCSV/main/schools.csv",check.names=FALSE)

the_list<-prgms$id
tokens<-prgms$info

pal = colorFactor("Set1", domain = schools$school_accessibility_description)
color_offsel1 = pal(schools$school_accessibility_description)
prgms$sport_ls<-lapply(prgms$sport,function(x) as.list(strsplit(x, ",")[[1]]))


#wheter list1 contain list2
contain_yes <- function(list1,list2) {
  result<-TRUE
  for(i in list2){
    if(!i %in% list1){result=FALSE}
  }
  return(result)
}



JScode <-
  "$(function() {
    setTimeout(function(){
      var vals = [0,100,300,500,1000,2000,6100];
    
      $('#stu').data('ionRangeSlider').update({'values':vals})
    }, 5)})"

# Main login screen
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("Get Started", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   selectInput("grade", label =  "What grade will your child be entering in September 2022?",
                               choices=c("","8th grade","9th grade","10th grade","Other")),
                   textInput("userName", placeholder="Your Email Account", label = tagList(icon("user"), "Username")),
                   selectInput("borough",label="Which borough of New York City do you live in?",choices=c("","Brooklyn","The Bronx","Manhattan","Staten Island","Queens","Other")),
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

header <- dashboardHeader( title = span("Find NYC Highschool Programs",style="font-size:18px"),titleWidth=300,uiOutput("upbar"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(),tags$head(includeHTML("google-analytics.html"),
                                                      includeCSS("www/mycss.css")
                                                      ),
                      tags$style(type = "text/css", "#mymap {height: calc(200vh - 200px) !important;}
                                                        div.leaflet-control-search.search-exp.leaflet-control {z-index:-10000000000 !important;}
                                 .leaflet-control {z-index:0};"),
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
  
  output$upbar <- renderUI({
    if(USER$login){
    fluidRow(
      column(width=4,
    tags$li(a("Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")),
    column(width=8,
           
    tags$li(a("Leave Your Feedbacks", 
              href="https://thepublicgood.shinyapps.io/feedback/"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")))}
  })
  

  
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE ){ 
      dashboardSidebar(
        width = 300,
        collapsed = FALSE,
        chooseSliderSkin("Flat"),
    
        
        tags$style(type = "text/css", "
              .marker-cluster-small {
	background-color: rgba(181, 226, 140, 0);
	}
.marker-cluster-small div {
	background-color: rgba(50, 94, 227, 0.6);
	}

.marker-cluster-medium {
	background-color: rgba(241, 211, 87, 0);
	}
.marker-cluster-medium div {
	background-color: rgba(50, 94, 227, 0.8);
	}

.marker-cluster-large {
	background-color: rgba(253, 156, 115, 0);
	}
.marker-cluster-large div {
	background-color: rgba(50, 94, 227, 0.9);
	}

	/* IE 6-8 fallback colors */
.leaflet-oldie .marker-cluster-small {
	background-color: rgb(181, 226, 140);
	}
.leaflet-oldie .marker-cluster-small div {
	background-color: rgb(110, 204, 57);
	}

.leaflet-oldie .marker-cluster-medium {
	background-color: rgb(241, 211, 87);
	}
.leaflet-oldie .marker-cluster-medium div {
	background-color: rgb(240, 194, 12);
	}

.leaflet-oldie .marker-cluster-large {
	background-color: rgb(253, 156, 115);
	}
.leaflet-oldie .marker-cluster-large div {
	background-color: rgb(241, 128, 23);
}

.marker-cluster {
	background-clip: content-box;
	border-radius: 20px;
	width: 16px;
	height: 16px;


	}
.marker-cluster div {
	width: 16px;
	height: 16px;
	margin-left: 10px;
	margin-top: 10px;
	text-align: center;
	border-radius: 10px;
	font: 12px 'Helvetica Neue', Arial, Helvetica, sans-serif;
	}
.marker-cluster span {
	line-height: 10px;
	}
        
             .irs-handle {display: none;}
             .selectize-input { padding: 2px;margin: 0px; min-height: 0;} 
             .selectize-dropdown { line-height: 15px; }
             .form-control { height:auto; padding:0px 0px;}
             .shiny-input-container { margin:0px; padding:0px;}
             .col-sm-6 {padding-right:10px;padding-left:5px;}
             div.form-group.shiny-input-container{padding-right:15px;padding-top:0px;}
              .form-control { height:auto; padding:0px 0px;}
              #this{padding-left:16px}
              #that{padding-right:-10px,margin-right:-10px}"
    
),

        # useShinyjs(),
        # radioButtons(
        #   "view", "", 
        #   choiceNames=list(tags$span(style = "font-size:18px;", "View Each School"),
        #             tags$span(style = "font-size:18px;", "View Each Program")),
        #   choiceValues=list("View Each School","View Each Program"),
        #   selected = "View Each School", 
        #   inline = FALSE),
        # 
        # hr(),
div(style = "margin-top:20px"),
        h4("Search and select programs by program names/codes, interests, neighborhood etc.",style="padding-left:15px"),
        div(style = "margin-top:1px"),
        pickerInput(
          inputId = "pick", label = NULL, 
          choices =the_list, 
          multiple = TRUE,
          options = pickerOptions(`live-search` = TRUE,
                                  actionsBox = TRUE,
                                  deselectAllText = "Clear Search"),
          choicesOpt = list(
            tokens = tokens
          )),
        # selectizeInput(
        #   inputId = "searchbar", 
        #   label = "",
        #   multiple = FALSE,
        #   width="100%",
        #   choices = c("Search Bar" = "", school_list),
        #   options = list(
        #     create = FALSE,
        #     placeholder = "School Name",
        #     maxItems = '1',
        #     onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
        #     onType = I("function (str) {if (str === \"\") {this.close();}}")
        #   )),
        #tags$head(tags$script(HTML(JScode))),
        setSliderColor(c("DeepSkyBlue","DeepSkyBlue"), 1:2),
        # tags$head(
        #   tags$style(HTML(".shiny-input-container > label {padding: -10px;}"))
        # ),
        hr(),
div(style = "margin-top:-6px"),
h4("  Filter by program:",style="padding-left:15px"),
div(style = "margin-top:-10px"),

# sliderInput("seat",
#             label =tags$span("Number of Grade 9 GE Applicants per Seat:",style = "padding-left:0px;font-weight: bold;font-size:13px;margin-bottom:0px;"),
#             min = 0,
#             max = 65,
#             value =c(0,65),
#             ticks=FALSE,
#             sep=""),

fluidRow(
  
  div(id="this",selectInput("priority",tags$span("Top priority of program admission:",style="font-size:13px"),
                            choices = c("Not selected",unique(prgms$top_priority)),
                            selected="Not selected",
                            width="300px"))),
hr(),
div(style = "margin-top:-5px"),
        h4("  Filter by high school:",style="padding-left:15px;padding-bottom:0px"),

div(style = "margin-top:5px"),

sliderTextInput("stu",
                    label =tags$span("Total Number of Students:",style = "padding-left:0px;font-weight: bold;font-size:13px;margin-bottom:0px;"),
                    choices=c(0,100,300,350,400,450,500,600,700,1000,2000,3000,4000,6100),
                selected = c(0,6100)
                ),

div(style = "margin-top:10px"),
selectizeInput('sport', tags$span("Sports offered by the school:",style="font-weight:bold; padding:0px"), 
               choices = sort(c("Basketball","Volleyball","Soccer","Outdoor Track","Baseball",    
                                "Softball","Bowling","Indoor Track","Cross Country", 'Tennis',       
                                "Wrestling","Handball","Swimming","Football","Flag Football",
                                "Badminton","Table Tennis","Golf","Lacrosse","Cricket",     
                                "Stunt","Fencing","Double Dutch","Gymnastics","Rugby",      
                                "Cheerleading")),selected = TRUE,  multiple = TRUE),
div(style = "margin-top:10px"),
fluidRow(
  
  column(width=6,
         div(id="thaat",selectInput("other",tags$span(HTML("Specific Pupulations Served:"),style="font-size:13px"),
                                    choices = c("Not selected","Girls only","Boys only","International students only","Transfer schools"),
                                    selected="Not selected"))),
  column(width=6,selectInput("diplo",
                             tags$span(HTML("Other special schools"),style="font-size:13px"),
                             choices = c("Not selected","Performance accessment schools","NYC 9-14 schools","Early college schools"),
                             selected="Not selected")
  )
),

div(style = "margin-top:-20px"),

fluidRow(

  column(width=6,
         div(id="that",selectInput("accessible",tags$span("Accessibility for physical disabilities:",style="font-size:13px"),
                             choices = c("Not selected",unique(prgms$school_accessibility_description)),
                             selected="Not selected"))),
  column(width=6,selectInput("ell",
                             tags$span(HTML("ELL<br>programs:"),style="font-size:13px"),
                             choices = c("Not selected",unique(prgms$ell)),
                             selected="Not selected")
  )
),
        div(style = "margin-top:-10px"),
        checkboxInput("diversity","Schools with Admission Priority for Diversity", FALSE),
actionButton("resetAll", "Reset all",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
       hr()

      )
    }
else{
  dashboardSidebar(
    width = 300,
    collapsed = FALSE)
}
  })
  
  
  output$user<-reactive(input$userName)
  
  
  
  
  df<- reactive({
    if(input$stu[1]>0|input$stu[2]<6100)
    {b<-prgms[which((prgms$total_students>=input$stu[1])&(prgms$total_students<=input$stu[2])),]}
    else{b<-prgms}
    if(nrow(b)<1)
    {return(list(n_prgm=0))}#no programs
    
    # if(input$seat[1]>0|input$seat[2]<65)
    # {b<-b[which((b$ge_per_seat>=input$seat[1])&(b$ge_per_seat<=input$seat[2])),]}
    # if(nrow(b)<1)
    # {return(list(n_prgm=0))}#no programs
    
    
    if(input$accessible!="Not selected"){
      b<-b[which(b$school_accessibility_description==input$accessible),]
      #this is to perform subsetting on b (prgm-level)
    }
    if(nrow(b)<1)
    {return(list(n_prgm=0))}#no programs
    
    if(input$diversity){
      b<-b[!is.na(b$diadetails),]
    }
    if(nrow(b)<1)
    {return(list(n_prgm=0))}#no programs
    # 
    # if(input$lunch){
    #   b<-b[b$lunch,]
    # }
    # if(nrow(b)<1)
    # {return(list(n_prgm=0))}#no programs
    

    if(nrow(b)<1)
    {return(list(n_prgm=0))}
    
    if(input$priority!="Not selected"){
      b<-b[which(b$top_priority==input$priority),]
      #this is to perform subsetting on b (prgm-level)
    }
    if(nrow(b)<1)
    {return(list(n_prgm=0))}#no programs
    
    if(input$other=="Girls only"){
      b<-b[which(b$girls==1),]
      #this is to perform subsetting on b (prgm-level)
    }
    else if(input$other=="Boys only"){
      b<-b[which(b$boys==1),]
    }
    else if(input$other=="International students only"){
      b<-b[which(b$international==1),]
    }
    else if(input$other=="Transfer schools"){
      b<-b[which(b$transfer==1),]
    }
    if(nrow(b)<1)
    {return(list(n_prgm=0))}#no programs
    

    
    b<-b[unlist(lapply(b$sport_ls,function(x) contain_yes(x,input$sport))),]
    if(nrow(b)<1)
    {return(list(n_prgm=0))}#no programs
    
    
    if(input$diplo=="Performance accessment schools"){
      b<-b[which(b$pbat==1),]
    }
    else if(input$diplo=="NYC 9-14 schools"){
      b<-b[which(b$ptech==1),]
    }
    else if(input$diplo=="Early college schools"){
      b<-b[which(b$earlycollege==1),]
    }
    if(nrow(b)<1)
    {return(list(n_prgm=0))}#no programs
    
    
    
    if(input$ell!="Not selected"){
      b<-b[which(b$ell==input$ell),]
    }
    if(nrow(b)<1)
    {return(list(n_prgm=0))}#no programs
    
    n_prgm=nrow(b)
    
    
    prgm_set<-b[,1:3]%>%spread(program,program_name)
    prgm_set[is.na(prgm_set)] <- ""
    prgm_set<-prgm_set%>%unite("prgms",2:ncol(prgm_set), remove = FALSE,sep=", ")
    prgm_set$prgms<-gsub(" ,", ",", prgm_set$prgms, fixed=TRUE)
    prgm_set$prgms<-trimws(prgm_set$prgms)
    prgm_set$prgms<-sub('\\,*$', '', prgm_set$prgms)
    prgm_set<-prgm_set%>%arrange(dbn)
    
    a<-schools
    a<-a[which(a$dbn%in%prgm_set$dbn),]
    a<-a%>%arrange(dbn)
    a$prgms<-prgm_set$prgms
    
    return(list(df=a,n_prgm=n_prgm,df2=b))
  })
  
  dff<-reactive({
    if(df()$n_prgm==0)
    {return(list(n_prgm=0))}
    if(length(input$pick)>0)
    {
      b<-df()$df2[df()$df2$id%in%input$pick,]
    }
    else{b<-df()$df2}
    n_prgm=nrow(b)
    if(n_prgm<1){return(list(n_prgm=0))}#no programs
    return(list(n_prgm=n_prgm,df2=b))
  
  })#this reactive function was intended to implement a dynamic pickInput with updating choices reacting to other filters. But my attempt failed.
  
  observeEvent(input$resetAll, {
    reset("pick")
    reset("seat")
    reset("accessible")
    reset("priorityfor")
    reset("other")
    reset("diversity")
    reset("diplo")
    #reset("lunch")
    reset("priority")
    reset("ell")
    reset("sport")
    updateSliderTextInput(session,'stu',selected = c(0,6100))
  
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

    paste("<strong>","School:","<strong>","<a href=",dff()$df2$website," target='_blank'>",dff()$df2$school_name,"</a>","</strong>","<br/>",
          "<strong>","Program:",dff()$df2$id,"</strong>","<br/>",
                   "<strong>Description:</strong>",dff()$df2$prgdesc,"<br/>",
          "<b>","Special Eligibility:","</b>",dff()$df2$eligibility,"<br/>",
          "<b>","School Accessibility:","</b>",dff()$df2$school_accessibility_description,"<br/>",
          "<b>","Admission Priority for Diversity:","</b>",dff()$df2$diadetails,"<br/>",
          "<b>","ELL Programs:","</b>",dff()$df2$ell)%>% 
      lapply(htmltools::HTML)
  })
  
  

  
  output$table<-renderDataTable(server = FALSE,
    if(dff()$n_prgm>0){
    datatable(dff()$df2[,c("school_name","program_name","code","prgdesc","interest","eligibility","top_priority","offer_rate1",
                           "grade9geapplicantsperseat","grade9swdapplicantsperseat","requirement_1","location","website","total_students","graduation_rate","school_accessibility_description","specialized","diadetails")],
              colnames=c("School name","Program name","Program code","Program description","Interests of Program","Program Eligibility",
                         "Top admission priority","Offer rate based on the admission priority","Grade 9 GE applicants per seat","Grade 9 SWD applicants per seat","Application requirement",
                         "School location","School website","Total number of students","Graduation rate","Physical accessibility","Requires specialized HS test","Diversity admission details"),
              rownames = FALSE,extensions = 'Buttons', 
              options = list(scrollX = TRUE
                             , pageLength = 5
                             , dom = 'rtip B'
                             ,info=FALSE
                             ,buttons=list(
                               list(
                                 extend = "copy",
                                 text = "Copy"
                               ),
                               list(
                                 extend = "excel",
                                 text = "Download Table"
                               ),
                               list(
                                 extend = "print",
                                 text = "Print Table"
                               )
                             )
                             ,autoWidth = TRUE
                             ,columnDefs = list(list(width = '125px', targets = list(0,1)),list(width = '1000px', targets = list(3)),
                                                list(width = '600px', targets = list(5,7,8,17)))
                             ,initComplete = JS(
                               "function(){$(this).addClass('compact');}")))%>%
                formatStyle(colnames(dff()$df2[,c("school_name","program_name")]),lineHeight='92%') 
    }
    else{NULL}

    )
  
  output$map<-renderLeaflet(
    if(dff()$n_prgm>0){
      leaflet(dff()$df2,options=leafletOptions(attributionControl=FALSE,zoomControl=FALSE)) %>%
        setView(lng=-73.9,lat=40.733812,zoom = 12 )%>%
    #     htmlwidgets::onRender("function(el, x) {
    #     L.control.zoom({ position: 'topleft' }).addTo(this)
    # }") %>%
        addProviderTiles("Stamen.TonerLite") %>%
        addCircleMarkers(~long,~lat,radius=6,stroke = FALSE,fillOpacity = 0.5,
                         popup =content2(),clusterOptions = markerClusterOptions(maxClusterRadius = 0.3,spiderfyDistanceMultiplier=1.5))%>%
        addSearchOSM(options = searchOptions(collapsed= FALSE,zoom=10,position="topleft"))
      
    }
    else{
      leaflet(options=leafletOptions(attributionControl=FALSE)) %>%
        setView(lng=-73.9,lat=40.733812,zoom = 12 )%>%
        addProviderTiles("Stamen.TonerLite") %>%
        addSearchOSM(options = searchOptions(collapsed= FALSE,zoom=10))
    }
  )
  
  
  
  
  
  
  
  
  output$mymap <- renderUI({
    if (USER$login == TRUE) {
      tagList(
#         tags$style(type = "text/css", "
#                    .leaflet-top{
#     z-index: unset !important;
# }
# 
# .leaflet-touch .leaflet-bar, .search-exp {
#     z-index: 100 !important;
# }"),
      leafletOutput('map', width = "100%", height = "52%"),
      absolutePanel(class = "panel panel-default", fixed = FALSE,
                    draggable = TRUE, top = 60, left = "auto", right = 15, bottom = "auto",
                    width = 330, height = 700,style="padding: 10px 20px 20px 20px;",
                    tags$span(HTML("Number of All Applicable Programs: "),as.character(dff()$n_prgm),
                              style="font-size:16px; font-weight: bold;"),
                    div(style = "margin-top:-15px"),
                    hr(),
                    div(style = "margin-top:-20px"),
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



