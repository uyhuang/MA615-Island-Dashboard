library(shiny)
library(shinydashboard)
library(leaflet)
 
ui <- dashboardPage(
  skin = "green",
  
  dashboardHeader(title = "Marshall Islands Overview", titleWidth = 250),
  
  dashboardSidebar(
    sidebarMenu(
      
      id = "Sidebar",
    
      ### MENUS
      menuItem("Homepage", tabName = "homepage", icon = icon("home"), badgeLabel = "new", badgeColor = "green"),
      menuItem("General Description", tabName = "general", icon = icon("info-circle"), startExpanded = TRUE,
               menuSubItem("Map", tabName = "map", icon = icon("map-marker")),
               menuSubItem("Key Facts", tabName = "facts", icon = icon("key")),
               menuSubItem("Narrative", tabName = "narrative", icon = icon("align-left"))
      ),
      menuItem("Key Demographics", tabName = "demographics", icon = icon("globe")),
      menuItem("Regional Comparison", tabName = "comparison", icon = icon("chart-bar")),
      menuItem("SWOT Analysis", tabName = "swot", icon = icon("table"), badgeLabel = "hot!", badgeColor = "red")
    )
  ),
  
  dashboardBody(
    
    ## Set Font style and Background
    tags$head(
      tags$style(HTML("
      .main-header .logo {font-family: 'Comic Sans MS', sans-serif; font-size: 18}
      .main-sidebar {font-family: 'Comic Sans MS', sans-serif; font-size: 16}
      .content-wrapper, .content-wrapper * {font-family: 'Comic Sans MS', sans-serif;font-size:15}
      .content-wrapper:before {
          content: '';
          display: block;
          position: absolute;
          top: 0; bottom: 0; left: 0; right: 0;
          background: url('https://www.state.gov/wp-content/uploads/2022/02/Marshall-Islands-2048x1536.jpg') no-repeat center center;
          background-size: cover;
          opacity: 0.5;
          z-index: -1;
        }

        .content-wrapper {
          position: relative;
        }
                      "))
    ),
    
    ## Set Background Photo

    
    tabItems(
      ## Homepage
      tabItem(tabName = "homepage",
              hr(style = "border-top: 2px solid black; width: 90%;margin-top:5px;margin-bottom:5px"),
              hr(style = "border-top: 4px solid black; width: 90%; margin-top:5px;margin-bottom:5px"),
              h2("Welcome to",
                 style = "text-align: center; font-style: italic; font-size: 35px"),
              h2("Marshall Islands!",
                 style = "text-align: center; font-style: italic; font-size: 55px"),
              hr(style = "border-top: 4px solid black; width: 90%; margin-top:5px;margin-bottom:5px"),
              hr(style = "border-top: 2px solid black; width: 90%;margin-top:5px;margin-bottom:5px"),
              div(style = "text-align: center;", textOutput(outputId = "currentDate")),
              fluidRow(
                # A static infoBox
                infoBox("New Orders", 10 * 2, icon = icon("credit-card")),
                # Dynamic infoBoxes
                infoBoxOutput("progressBox"),
                infoBoxOutput("approvalBox")
              )
              ),
      
      ## GD: MAP
      tabItem(tabName = "map",
              h2("Map of Marshall Islands"),
              leafletOutput(outputId = "islandmap"),
              ),
      
      ## GD: Key Facts
      tabItem(tabName = "facts",
              h2("Key facts of Marshall Islands")
              ),
      
      ## GD: Narrative
      tabItem(tabName = "narrative",
              h2("Narrative")
              ),
      
      ## Key Demographics
      tabItem(tabName = "demographics",
              h2("Key Demographics of Marshall Islands")
                ),
      
      ## Regional Comparison
      tabItem(tabName = "comparison",
              h2("regional comparsion")
              ),
      
      ## SWOT
      tabItem(tabName = "swot",
              h2("SWOT")
              )
             )
    
  )
)

server <- function(input, output, session){
  
  ## Homepage Date
  output$currentDate <- renderText({
    format(Sys.Date(), "%Y-%m-%d, %A")
  })
  
  output$progressBox <- renderInfoBox({
    infoBox(
      "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
      color = "purple"
    )
  })
  
  ## Leaflet Map
  output$islandmap <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$Esri.WorldStreetMap)|>
      addMarkers(lng = 168.7345, lat = 7.23, popup=HTML("Marshall Islands<br/>168.7345 E, 7.23 N")) |>
      setView(lng = 168.7345, lat = 7.23, zoom = 7) |>
      addCircles(lng = 168.7345, lat = 7.23, weight = 1, 
                 radius = 50000,
                 color = "black", fillColor = "lightgreen", 
                 fillOpacity = 0.2, popup=HTML("Marshall Islands<br/>168.7345 E, 7.23 N"))
  })

}

shinyApp(ui, server)