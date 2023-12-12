## app.R ##

library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", 
               tabName = "dashboard",
               icon = icon("dashboard")),
      
      menuItem("widgets", 
               tabName = "widgets", 
               icon = icon("th"))
    )
    
    
  ),
  
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tabItems(
      #tabItem 1
      tabItem(tabName = "dashboard",
    fluidRow(
      box(plotOutput("plot1", height = 250)),
      
      box(
        title = "Controls",
        sliderInput("slider", 
                    "Number of observations:",
                    1, 100, 50)
      )
    )
  ),
  
  # tabItem 2
  
  tabItem(tabName = "widgets",
          h2("Widgets tab content")
  
     )
   )
  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)