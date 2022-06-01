library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(sodium)

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
                     br(),
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
body <- dashboardBody(shinyjs::useShinyjs(),
                      uiOutput("body"))
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
          Password <- isolate(input$passwd)
          pasverify<-TRUE
          #if(length(which(credentials$username_id==Username))==1) { 
          pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
            #pasverify <- password_verify(pasmatch, Password)
            
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
      sidebarMenu(
        menuItem("Main Page", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Second Page", tabName = "second", icon = icon("th"))
      )
    }
  })
  output$user<-reactive(input$userName)
  
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      
      
      tabItems(
        
        # First tab
        tabItem(tabName ="dashboard", class = "active",
                div(input$userName),
                fluidRow(
                  box(width = 12, dataTableOutput('results'))
                )),
        
        # Second tab
        tabItem(tabName = "second",
                div(input$userName),
                fluidRow(
                  box(width = 12, dataTableOutput('results2'))
                )
        ))
      
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

runApp(list(ui = ui, server = server), launch.browser = TRUE)