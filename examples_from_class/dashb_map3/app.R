library(leaflet)
library(shiny)
library(shinydashboard)


# header board
header <- dashboardHeader(
  title = 'map with moving pins')
  # task list for status of data processing
   

# Side bar boardy
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = 'menu_tabs',
     menuItem('Map', tabName = 'map'),
     menuItem('Move', tabName = 'move')
  )
)

# Body board
body <- dashboardBody(
  tabItems(
    #tabitem 1
    tabItem(tabName = 'map',
            fluidRow(
              leafletOutput("mymap")
            )
    ),
    
    #tabitem 2
    tabItem(tabName = 'Move',
            fluidRow(
              actionButton("recalc", 
                           "New Points")
              )
           )
         )
       )

# Shiny UI
ui <- dashboardPage(
  title = 'test',
  dashboardHeader(),
  sidebar,
  body
)


server <- function(input, output, session) {
  # observe({
  #   req(input$mydata)
  #   updateTabItems(session, 'menu_tabs', 'menu2')
  # })
  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + -71, rnorm(40) + 43)
  }, ignoreNULL = FALSE)
                        
                          
  output$mymap <- renderLeaflet({
    leaflet() %>%  
      addTiles() %>%
      setView(lng = -71.0, 
              lat = 42.3, 
              zoom = 6) %>% 
      addMarkers(data = points())
  })
  
  
  # observe({
  #   req(input$mydata)
  #   proxy <- leafletProxy('map')
  #   print(proxy$id)
  #   proxy %>% 
  #     setView(runif(1) * 30 +2, runif(1) * 30 + 2, 7)
  # })
}


shinyApp(ui, server)