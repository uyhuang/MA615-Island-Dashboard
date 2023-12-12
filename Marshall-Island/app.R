# Install and load the required packages
# install.packages("shiny")
# install.packages("shinydashboard")
library(shiny)
library(shinydashboard)
library(leaflet)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Marshall Islands Overview", titleWidth = 250),
  dashboardSidebar(
    width = 250,
    tags$head(
      tags$style(HTML("
      .skin-blue .main-sidebar {background-color: #4D5360; /* Dark grey background */}
      .skin-blue .sidebar-menu li a {color: #FFFFFF; /* White text color */}
      .main-sidebar {font-family: 'Comic Sans MS', sans-serif;}
      .sidebar-menu li a {font-size: 15px; /* Larger font size */}
                      ")),
      tags$link(href = "https://fonts.googleapis.com/css?family=Your+Font+Family", 
                rel = "stylesheet", 
                type = "text/css")),
    sidebarMenu(
      id = "sidebar", # Important: add an id to the sidebarMenu #does something
      menuItem("Homepage", tabName = "homepage", icon = icon("home")),
      menuItem("General Description", tabName = "general", icon = icon("info-circle"), startExpanded = TRUE,
               menuSubItem("Map", tabName = "map", icon = icon("map-marker")),
               menuSubItem("Key Facts", tabName = "facts", icon = icon("key")),
               menuSubItem("Narrative", tabName = "narrative", icon = icon("align-left"))
      ),
      menuItem("Key Demographics", tabName = "demographics", icon = icon("globe")),
      menuItem("Regional Comparison", tabName = "comparison", icon = icon("chart-bar")),
      menuItem("SWOT Analysis", tabName = "swot", icon = icon("table"))
    )
  ),
  
    dashboardBody(
      uiOutput("pageContent")
  )
)

# Define server logic
server <- function(input, output) { 
  output$pageContent <- renderUI({
    # Switch between the menu items based on the sidebar input
    switch(input$sidebar,
           "homepage" = div(
             div(class = "overlay-text",
                 style = 'position: absolute;',
                 style = 'transform: translate(70%,50%);',
                 h2("Welcome to"),
                 h1("Marshall Islands"),
                 style = "text-align: center;"
                 ),
             img(src = "https://www.state.gov/wp-content/uploads/2022/02/Marshall-Islands-2048x1536.jpg",
                 height = "500px",
                 width = "100%",
                 # hspace = "0%", 
                 # wspace = "0%",
                 style = "position: center;")
           ),
           "general" = h2("General Description Content"),
           "map" = {
             fluidRow(
               column(12, 
                      h2("Map of Marshall Islands"),
                      p("The map below shows the location of the Marshall Islands in the Pacific Ocean. It highlights the country's geographic context and its neighboring island nations."),
                      leafletOutput("map")
               )
             )
           },
           "facts" = h2("Key Facts about Marshall Island"),
           "narrative" = h2("A Brief Narrative Description of Marshall Island"),
           "demographics" = h2("Key Demographics"),
           "comparison" = h2("Regional Comparison"),
           "swot" = h2("SWOT Analysis")
    )
  })
  

  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 168.7345, lat = 7.1164, zoom = 5) %>%
      addCircles(lng = 168.7345, lat = 7.1164, weight = 1, 
                 radius = 50000, # Approximate radius in meters
                 color = "#ff7800", fillColor = "#ff7800", 
                 fillOpacity = 0.5, popup = "Marshall Islands")
  })
}


# Run the application
shinyApp(ui, server)
