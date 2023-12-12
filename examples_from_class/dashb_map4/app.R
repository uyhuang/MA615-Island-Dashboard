library(leaflet)
library(shiny)
library(shinydashboard)




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
              leafletOutput("mymap"),
              p(),
              actionButton("recalc", 
                           "New points")
            )
    ),
    tabItem(tabName = 'move',
            fluidRow(
              leafletOutput("mymap2"),
              p(),
              actionButton("recalc2", 
                           "New points")
            )
    )
  )
)

ui <- fluidPage(
  leafletOutput("mymap"),
  
)

# Shiny UI
ui <- dashboardPage(
  title = 'test',
  dashboardHeader(
    title = 'map with moving pins'),
  sidebar,
  body
)


server <- function(input, output, session) {
  # observe({
  #   req(input$mydata)
  #   updateTabItems(session, 'menu_tabs', 'menu2')
  # })
  
  x1 = 42.3
  y1 = -71
  x2 = -28
  y2 = 32
  
  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + y1, rnorm(40) + x1)
  }, ignoreNULL = FALSE)
                        
                          
  output$mymap <- renderLeaflet({
    leaflet() %>%  
      addTiles() %>%
      setView(lng = y1, 
              lat = x1, 
              zoom = 6) %>% 
      addMarkers(data = points())
  })
  
  points2 <- eventReactive(input$recalc2, {
    cbind(rnorm(40) * 2 + y2, rnorm(40) + x2)
  }, ignoreNULL = FALSE)
  
  
  output$mymap2 <- renderLeaflet({
    leaflet() %>%  
      addTiles() %>%
      setView(lng = y2, 
              lat = x2, 
              zoom = 6) %>% 
      addMarkers(data = points2())
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