library(leaflet)
library(shiny)
library(shinydashboard)


# header board
header <- dashboardHeader(
  title = 'Pheno-Copter'
  # task list for status of data processing
  , dropdownMenuOutput('task_menu'))

# Side bar boardy
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = 'menu_tabs'
    , menuItem('menu1', tabName = 'menu1')
    , menuItem('menu2', tabName = 'menu2')
    
  )
)

# Body board
body <- dashboardBody(
  tabItems(
    #tabitem 1
    tabItem(
      tabName = 'menu1'
      , tags$a(
        id = "mydiv", href = "#", 'click me', 
        onclick = 'Shiny.onInputChange("mydata", Math.random());')
    ),
    tabItem(
      tabName = 'menu2'
      , leafletOutput('map')
      , verbatimTextOutput('summary')
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
  observe({
    req(input$mydata)
    updateTabItems(session, 'menu_tabs', 'menu2')
  })
  output$map <- renderLeaflet({
    leaflet() %>%  
      addTiles() %>%
      setView(lng = -71.0, lat = 42.3, zoom = 6)
      
  })
  output$summary <- renderPrint({
    print(input$mydata)
    print(leafletProxy('map')$id)
  })
  observe({
    req(input$mydata)
    proxy <- leafletProxy('map')
    print(proxy$id)
    proxy %>% 
      setView(runif(1) * 30 +2, runif(1) * 30 + 2, 7)
  })
}


shinyApp(ui, server)