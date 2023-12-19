library(shiny)
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(tidyverse)

## data 
all <- read.csv("all.csv")
MSnewborn <- read.csv("newborn.csv")
all$Indicator[all$Indicator == "GN_UNMPLY: Labour force unemployment rate"] <- "unemploymentRate"
all$Indicator[all$Indicator == "NT_ANT_HAZ_AVG: Mean Height-for-age"] <- "Mean_Height-for-age"

## Marshall Islands New Born
MSnewborn <- MSnewborn |> 
  select(TIME_PERIOD.Time.period, SEX.Sex, OBS_VALUE.Observation.Value) |>
  filter((SEX.Sex == "_T: Total") == F)
colnames(MSnewborn) <- c("Year", "Gender", "Newborn(1k)")
MSnewborn$Gender[MSnewborn$Gender == "F: Female"]<- "Female"
MSnewborn$Gender[MSnewborn$Gender == "M: Male"]<- "Male"

## Marshall Islands Total Population
MSpopulation <- all |>
  filter(Indicator == "Population(1k)" & Country == "Marshall Islands") |>
  select(-Indicator, -Country)

## Total Population
Population <- all |>
  filter(Indicator == "Population(1k)") |>
  select(-Indicator)

## Growth Rate
GrowthRate <- all |>
  filter(Indicator == "PopulationGrowthRate") |>
  select(-Indicator)

## Marshall Island Life Expectancy
MSLE <- all |>
  filter(Indicator == "LifeExpectancy" & Country == "Marshall Islands") |>
  select(-Indicator, -Country)

## Life Expectancy
LE <- all |>
  filter(Indicator == "LifeExpectancy") |>
  select(-Indicator)

## Literacy
Literacy <- all |>
  filter(Indicator == "LiteracyRate(>25)" | Indicator == "LiteracyRate(15-24)") |>
  filter(Year == 2011)

## Economic Activity Time
Height<- all |>
  filter(Indicator == "Mean_Height-for-age") |>
  filter((Year == 2007) == F) |>
  filter((Year == 2012) == F) |>
  select(-Year, - Indicator)


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
      menuItem("SWOT Analysis", tabName = "swot", icon = icon("table"), badgeLabel = "hot!", badgeColor = "red"),
      menuItem("Reference & Special Thanks", tabName = "thanks", icon = icon("heart"))
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
              
              ## title
              
              hr(style = "border-top: 2px solid black; width: 90%;margin-top:5px;margin-bottom:5px"),
              hr(style = "border-top: 4px solid black; width: 90%; margin-top:5px;margin-bottom:5px"),
              h2("Welcome to",
                 style = "text-align: center; font-style: italic; font-size: 35px"),
              h2("Marshall Islands!",
                 style = "text-align: center; font-style: italic; font-size: 55px"),
              hr(style = "border-top: 4px solid black; width: 90%; margin-top:5px;margin-bottom:5px"),
              hr(style = "border-top: 2px solid black; width: 90%;margin-top:5px;margin-bottom:5px"),
              div(p("By Yuchen Huang"), style = "text-align: center;", textOutput(outputId = "currentDate")),
              div(style = "height: 30px;"),
              
              ## Info box
              
              fluidRow(
                column(8, 
                       infoBox(title = "General Description", value = "Map, Key Facts, and Narrative", fill = TRUE,
                               subtitle = "Learn Basic information about Marshall Islands!",
                               color = "navy", width = NULL),
                       infoBox(title = "Key Demographics",
                               value = "Total Population, Education levels, Economic Activities, etc.", fill = TRUE,
                               subtitle = "See charts, graphs, and tables here!",
                               color = "blue", width = NULL),
                       infoBox("Regional Comparison", fill = TRUE,
                               value = "Comparison with Other Pacific Island Countries",
                               subtitle = "Check out differences in geography and economy!",
                               color = "light-blue", width = NULL),
                       infoBox("SWOT", fill = TRUE, 
                               value = "strengths, weaknesses, opportunities, and threats",
                               subtitle = "Evaluation of Marshall Islands in a competitive position!",
                               color = "teal", width = NULL)
                ),
                column(4, 
                       img(src = "https://cdn.britannica.com/44/192044-050-F3083027/Majuro-Marshall-Islands.jpg", height = "100%", width = "100%", style = "border-radius: 10px;")
                )
              )

              ),
      
      ## GD: MAP
      tabItem(tabName = "map",
              h2("Map of Marshall Islands",
                 style = "text-align: center; font-style: italic; font-size: 40px"),
              hr(style = "border-top: 2px solid black; width: 90%;margin-top:10px;margin-bottom:20px"),
              box(leafletOutput(outputId = "islandmap"), width = NULL,
                  p("Archipelagic Layout: The Marshall Islands consist of 29 atolls and 5 isolated islands."),
                  p("Shape and Distribution: The atolls and islands are spread out over a large area in the Pacific Ocean, forming two main island chains: the Ratak (Sunrise) and Ralik (Sunset) chains."),
                  p("Island Characteristics: The individual islands and atolls are typically low-lying, with narrow land masses surrounding central lagoons."),
                  p("Geographical Spread: The country covers approximately 750,000 square miles of ocean, although the total land area is only about 70 square miles."))
              ),
      
      ## GD: Key Facts
      tabItem(tabName = "facts",
              h2("Key facts of Marshall Islands",
                 style = "text-align: center; font-style: italic; font-size: 40px"),
              hr(style = "border-top: 2px solid black; width: 90%;margin-top:10px;margin-bottom:20px"),
              tabBox(
                id = "tabset1", height = "400px", width = NULL,
                tabPanel("Geography", 
                         p("The Marshall Islands (Marshallese: Ṃajeḷ), officially the Republic of the Marshall Islands (Marshallese: Aolepān Aorōkin Ṃajeḷ), is an island country west of the International Date Line and north of the equator in the Micronesia region in the Northwestern Pacific Ocean. The territory consists of 29 coral atolls and five islands, divided across two island chains: Ratak in the east and Ralik in the west. 97.87% of its territory is water, the largest proportion of water to land of any sovereign state. The country shares maritime boundaries with Wake Island to the north, Kiribati to the southeast, Nauru to the south, and the Federated States of Micronesia to the west. The capital and largest city is Majuro, home to approximately half of the country's population."),
                         img(src = "https://www.worldatlas.com/r/w960-q80/upload/e3/ec/d6/mh-01.png", 
                             alt = "Image of Marshall Islands", height = "400px", style = "border-radius: 10px;")),
                tabPanel("Demographics", 
                         "The estimated population is around 59,000, with a diverse mix of Micronesian cultures. The population is spread across its many islands, with the majority residing in urban areas like the capital, Majuro. Although the ancient skills are now in decline, the Marshallese were once able navigators, to use the stars and stick-and-shell charts. The image below shows the dress mats from Marshall Islands.",
                         img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f5/Dress_mat_from_Marshall_Islands%2C_Honolulu_Museum_of_Art.JPG/2560px-Dress_mat_from_Marshall_Islands%2C_Honolulu_Museum_of_Art.JPG",
                             alt = "Dress mat from Marshall Islands", height = "400px", style = "border-radius: 10px;")),
                tabPanel("Economy", 
                         "The islands have few natural resources, and their imports far exceed exports. According to the CIA, the value of exports in 2013 was approximately $53.7 million while estimated imports were $133.7 million. Agricultural products include coconuts, tomatoes, melons, taro, breadfruit, fruits, pigs and chickens. Industry is made of the production of copra and craft items, tuna processing, and tourism. The GDP in 2016 was an estimated $180 million, with a real growth rate of 1.7%. The GDP per capita was $3,300. The International Monetary Fund reported in mid-2016 that the economy of the Republic had expanded by about 0.5 percent in the Fiscal Year 2015 thanks to an improved fisheries sector. A surplus of 3% of GDP was recorded owing to record-high fishing license fees. Growth is expected to rise to about 1.5 percent and inflation to about 0.5 percent in FY2016, as the effects of the drought in earlier 2016 are offset by the resumption of infrastructure projects. In 2018, the Republic of Marshall Islands passed the Sovereign Currency Act, which made it the first country to issue their own cryptocurrency and certify it as legal tender; the currency is called the 'Sovereign'.Marshall Islands has signed a bilateral trade agreement with Taiwan in 2019, this agreement has been approved in 2023 and will take effect at a future date.",
                         img(src = "https://upload.wikimedia.org/wikipedia/commons/e/e2/Making_copra%2C_Marshall_Islands_%28from_a_book_published_in_1932%29.png",
                             alt = "Marshall Islands making Copra", height = "400px",style = "border-radius: 10px;")),
                tabPanel("Climate", 
                         "The Marshall Islands experiences a tropical rainforest climate without distinct seasons. It faces significant challenges from climate change, especially rising sea levels."),
                tabPanel("Education", 
                         "The Human Rights Measurement Initiative (HRMI)[169] finds that the Marshall Islands are fulfilling only 66.1% of what it should be fulfilling for the right to education based on the country's level of income. HRMI breaks down the right to education by looking at the rights to both primary education and secondary education. While taking into consideration the Marshall Islands' income level, the nation is achieving 65.5% of what should be possible based on its resources (income) for primary education and 66.6% for secondary education. The Ministry of Education is the education agency of the islands. Marshall Islands Public School System operates the state schools in the Marshall Islands. In the 1994–1995 school year the country had 103 elementary schools and 13 secondary schools. There were 27 private elementary schools and one private high school. Christian groups operated most of the private schools.
                         Historically the Marshallese population was taught in English first with Marshallese instruction coming later, but this was reversed in the 1990s to keep the islands' cultural heritage and so children could write in Marshallese. Now English language instruction begins in grade 3. Christine McMurray and Roy Smith wrote in Diseases of Globalization: Socioeconomic Transition and Health that this could potentially weaken the children's English skills. 
                         There are two tertiary institutions operating in the Marshall Islands, the College of the Marshall Islands[172] and the University of the South Pacific.",
                         img(src = "https://upload.wikimedia.org/wikipedia/commons/8/86/Seal_of_the_Marshall_Islands.svg",
                             alt = "Seal of the Marshall Islands", height = "400px", style = "border-radius: 10px;")),
                tabPanel("Healthcare", 
                         "
The healthcare system in the Marshall Islands faces significant challenges due to limited resources, geographic isolation, and a high prevalence of chronic diseases like diabetes. Additionally, the legacy of U.S. nuclear testing has left long-term health impacts, including increased cancer rates. Access to comprehensive medical care is constrained, especially on remote islands, and the nation is grappling with environmental health issues exacerbated by climate change.")
              )
              ),
      
      ## GD: Narrative
      tabItem(tabName = "narrative",
              box(width = NULL, div(class = "narrative-container",
                    h2("Narrative of Marshall Islands", style = "text-align: center; margin-bottom: 10px;"),
                    hr(style = "border-top: 2px solid black; width: 90%;margin-top:10px;margin-bottom:20px"),
                    p("The Marshall Islands, a chain of volcanic islands and coral atolls in the central Pacific Ocean, present a picturesque view of a tropical paradise. Known for their rich marine life and pristine beaches, these islands tell a story of resilience and beauty.", style = "padding: 0 20px;"),
                    img(src = "https://cdn.britannica.com/90/3290-004-A82FC255/Flag-of-Marshall-Islands.jpg", style = "width:80%;  height:auto; border-radius: 10px;"),
                    p("Historically significant for their role during World War II and the subsequent U.S. nuclear testing, the Marshall Islands today face challenges such as rising sea levels and climate change. Yet, the spirit of the Marshallese people remains undeterred, reflected in their vibrant culture and traditions.", style = "padding: 0 20px; margin-top: 20px;"),
                    p("Here's a deeper dive into the life, culture, and challenges of the Marshall Islands...", style = "padding: 0 20px; margin-top: 10px; font-style: italic;")
              ))),
      
      ## Key Demographics
      tabItem(tabName = "demographics",
              h2("Key Demographics of Marshall Islands", style = "text-align: center; font-style: italic; font-size: 40px"),
              hr(style = "border-top: 2px solid black; width: 90%;margin-top:10px;margin-bottom:20px"),
              tabBox(id = "tabset2", height = "400px", width = NULL,
                     
                     ## total population
                     tabPanel("Total Population",
                              sliderInput("TPyearRange", "Select Year Range:",
                                          min = 1950, max = 2023, value = c(1950, 2023)),
                              plotOutput("totalpopulationPlot")),
                     
                     ## Newborn
                     tabPanel("New Born",
                              sliderInput("NByearRange", "Select Year Range:",
                                          min = 1970, max = 2023, value = c(1970, 2023)),
                              plotOutput("newbornPlot")),
                     
                     ## LE
                     tabPanel("Life Expectancy",
                              sliderInput("LEyearRange", "Select Year Range:",
                                          min = 1950, max = 2023, value = c(1950,2023)),
                              plotOutput("LEPlot")
                              )
                     )
                ),
      
      ## Regional Comparison
      tabItem(tabName = "comparison",
              h2("Regional Comparsion", style = "text-align: center; font-style: italic; font-size: 40px"),
              hr(style = "border-top: 2px solid black; width: 90%;margin-top:10px;margin-bottom:20px"),
              tabBox(id = "tabset3", height = "400px", width = NULL,
                     
                     ## Countries Introduction
                     tabPanel("Similar Island Countries",
                              div(class = "row",
                                  div(class = "col-sm-4",
                                      h3("Kiribati"),
                                      p("Both Kiritabi and Marshall Islands are Pacific island nations with tropical climates, rich cultural heritage, and economies reliant on fishing and tourism. Key differences include Kiribati's British colonial history and vast marine protected areas, versus the Marshall Islands' U.S. ties and nuclear testing legacy. While both face climate change challenges, Kiribati is notably vulnerable due to its low-lying atolls."),
                                      img(src = "https://upload.wikimedia.org/wikipedia/commons/d/d3/Flag_of_Kiribati.svg", height = "150px")
                                  ),
                                  div(class = "col-sm-4",
                                      h3("Tonga"),
                                      p("Both are Pacific island nations with tropical climates and cultures deeply rooted in the Pacific Islander heritage. Economically, they rely on fisheries, tourism, and external assistance. The key distinctions lie in Tonga's status as a Polynesian kingdom with a unique monarchical system and its relatively higher elevation compared to the Marshall Islands, which has closer ties with the U.S. and is known for its U.S. military testing history. Both nations grapple with the impacts of climate change, but the Marshall Islands' low-lying atolls make it especially susceptible to sea-level rise."),
                                      img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/9/9a/Flag_of_Tonga.svg/2560px-Flag_of_Tonga.svg.png", height = "150px")
                                  ),
                                  div(class = "col-sm-4",
                                      h3("Tuvalu"),
                                      p("Both Tuvalu and the Marshall Islands are small island nations in the Pacific, sharing similar challenges due to their geographic location and climate. They have rich Pacific Island cultures and economies that largely depend on fishing, remittances, and international aid. Tuvalu is one of the smallest and least developed of these nations, facing severe threats from sea-level rise due to its low-lying atolls, similar to the Marshall Islands. While the Marshall Islands has historical ties with the U.S., particularly in relation to military testing, Tuvalu is known for its strong advocacy on climate change issues on the international stage."),
                                      img(src = "https://upload.wikimedia.org/wikipedia/commons/3/38/Flag_of_Tuvalu.svg", height = "150px")
                                      ))
                              ),
                     
                     ## Total Population
                     tabPanel("Total Population Comparison",
                              selectInput("AllTPgenderSelection", "Select Gender:", choices = c("Female", "Male")),
                              plotOutput("AllTPpopulationPlot")),
                     
                     ## Population Growth Rate
                     tabPanel("Population Growth Rate Comparison",
                              selectInput("GrowthYear", "Select Year:", choices = c(2000, 2020)),
                              plotOutput("growthPlot")),
                     
                     ## Life Expectancy
                     tabPanel("Life Expectancy Comparison",
                              sliderInput("yearRangeLE", "Select Year Range:",
                                          min = 1950, max = 2023, value = c(1950, 2023)),
                              plotOutput("lePlot")),
                     
                     ## Tonga vs. Marshall Islands
                     tabPanel("Literacy Comparison",
                              selectInput("genderSelectionLiteracy", "Select Gender:", choices = c("Female", "Male")),
                              plotOutput("literacyPlot")),
                     
                     ## Mean height for age
                     tabPanel("Mean Height for Age",
                              selectInput("genderSelectionHeight", "Select Gender:", choices = c("Female", "Male")),
                              plotOutput("heightPlot"),
                              p("Mean height for age is a measure of the average height of children and adolescents for a specific age group. It is a key indicator used in assessing children's growth and development."),
                              p("A negative mean height for age value (or Z-score) indicates that the average height of children in the specified age group is below the reference population's average. A more negative value means the height is further below the reference average. This can be a sign of stunting or chronic malnutrition, as it suggests that children are not achieving their potential growth."),
                              p("Positive Values: Conversely, a positive mean height for age value means that the average height of children in that age group is above the reference population's average. A higher positive value indicates a greater difference above the average. This can suggest better nutritional status and health conditions that are conducive to growth.")
                     )
              )
              ),
      
      ## SWOT
      tabItem(tabName = "swot",
              h2("Strengths, Weaknesses,", style = "text-align: center; font-style: italic; font-size: 40px"),
              h2("Opportunities, and Threats Analysis", style = "text-align: center; font-style: italic; font-size: 40px"),
              hr(style = "border-top: 2px solid black; width: 90%;margin-top:10px;margin-bottom:20px"),
              fluidRow(
                box(title = "Strengths", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    fluidRow(box(title = "Strategic Location", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 11,
                                 textOutput("strength0"))),
                    fluidRow(box(title = "Huge EEZ", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 11,
                                 textOutput("strength1"))),
                    fluidRow(box(title = "Natural Beauty", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 11,
                                 textOutput("strength2")))),
                box(title = "Opportunities", status = "success", solidHeader = TRUE, collapsible = TRUE,
                    fluidRow(box(title = "Tourism Development", status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 11,
                                 textOutput("opportunity0"))),
                    fluidRow(box(title = "Renewable Energy", status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 11,
                                 textOutput("opportunity1"))),
                    fluidRow(box(title = "International Aid and Partnerships", status = "success", solidHeader = TRUE, collapsible = TRUE, width = 11, collapsed = TRUE,
                                 textOutput("opportunity2")))),
              ),
              fluidRow(
                box(title = "Weaknesses", status = "warning", solidHeader = TRUE, collapsible = TRUE,
                    fluidRow(box(title = "Economic Dependency", status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 11,
                                 textOutput("weakness0"))),
                    fluidRow(box(title = "Limited Resources", status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 11,
                                 textOutput("weakness1"))),
                    fluidRow(box(title = "Climate Vulnerability", status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 11,
                                 textOutput("weakness2")))),
                box(title = "Threats", status = "danger", solidHeader = TRUE, collapsible = TRUE,
                    fluidRow(box(title = "Climate Change", status = "danger", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 11,
                                 textOutput("threat0"))),
                    fluidRow(box(title = "Economic Instability", status = "danger", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 11,
                                 textOutput("threat1"))),
                    fluidRow(box(title = "Political and Strategic Change", status = "danger", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 11,
                                 textOutput("threat2")))),
              ),
      ),
      
      ## Reference and Special Thanks
      tabItem(tabName = "thanks",
              h2("Reference & Special Thanks", style = "text-align: center; font-style: italic; font-size: 40px"),
              hr(style = "border-top: 2px solid black; width: 90%;margin-top:10px;margin-bottom:20px"),
              tabBox(id = "tabset4", width = NULL, height = "250px",
                     tabPanel("Data Sources",
                              a("United Nations Children's Fund(UNICEF)", href = "https://data.unicef.org/resources/data_explorer/unicef_f/?ag=UNICEF&df=GLOBAL_DATAFLOW&ver=1.0&dq=MHL..&startPeriod=1970&endPeriod=2023"),
                              p(""),
                              a("Wikipedia of Marshall Islands", href = "https://en.wikipedia.org/wiki/Marshall_Islands")),
                     tabPanel("Technical References",
                              a("Mastering Shiny", href = "https://mastering-shiny.org/index.html"),
                              p(""),
                              a("leaflet for R", href = "https://rstudio.github.io/leaflet/"),
                              p(""),
                              a("Shiny Dashboard", href = "https://rstudio.github.io/shinydashboard/structure.html#tabbox"),
                              p(""),
                              a("ChatGPT", href = "https://chat.openai.com")),
                     tabPanel("Literature References",
                              a("Wikipedia of Marshall Islands", href = "https://en.wikipedia.org/wiki/Marshall_Islands"),
                              p(""),
                              a("Wikipedia of Kiribati", href = "https://en.wikipedia.org/wiki/Kiribati"),
                              p(""),
                              a("Wikipedia of Tonga", href = "https://en.wikipedia.org/wiki/Tonga"),
                              p(""),
                              a("Wikipedia of Tuvalu", href = "https://en.wikipedia.org/wiki/Tuvalu"),
                              p(""),
                              a("ChatGPT", href = "https://chat.openai.com")),
                     tabPanel("Speical Thanks",
                              h4("Haviland Wright, Ph.D.", style = "text-align: center"),
                              h4("Aidan O’Hara", style = "text-align: center"),
                              h4("Mingrui Du", style = "text-align: center"),
                              h4("Stelle M", style = "text-align: center"),
                              h4("Zhixing Lin", style = "text-align: center")))
              )
             )
    
    
  )
)

server <- function(input, output, session){
  
  ## Homepage Date
  output$currentDate <- renderText({
    format(Sys.Date(), "%Y-%m-%d, %A")
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
  
  ## Key Demographics: Newborn
  output$newbornPlot <- renderPlot({
    filtered_data <- MSnewborn |>
      filter(Year >= input$NByearRange[1], Year <= input$NByearRange[2])
    
    ggplot(filtered_data, aes(x = Year, y = `Newborn(1k)`, fill = Gender)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      labs(title = "Newborn Distribution by Gender", x = "Year", y = "Newborns per 1000") +
      theme(title = element_text(size = 16),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12))
  })
  
  ## Key Demographics: Total Population
  output$totalpopulationPlot <- renderPlot({
    filtered_data <- MSpopulation |>
      filter(Year >= input$TPyearRange[1], Year <= input$TPyearRange[2])
    
    ggplot(filtered_data, aes(x = Year, y = `Value`, fill = Gender)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      labs(title = "Total Population by Year", x = "Year", y = "Population per 1000") +
      theme(title = element_text(size = 16),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12))
  })
  
  ## Marshall Islands:LE
  output$LEPlot <- renderPlot({
    filtered_data <- MSpopulation |>
      filter(Year >= input$LEyearRange[1], Year <= input$LEyearRange[2])
    
    ggplot(filtered_data, aes(x = Year, y = `Value`, fill = Gender)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      labs(title = "Life Expectancy by Year", x = "Year", y = "Life Expectancy", 
           caption = "A higher LE represents a longer life") +
      theme(title = element_text(size = 16),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12))
  })
  
  ## Comparison: Growth Plot
  output$growthPlot <- renderPlot({
    filtered_data <- GrowthRate |>
      filter(Year == input$GrowthYear) |>
      arrange(Country, Gender)
    
    ggplot(filtered_data, aes(x = Country, y = Value, fill = Gender)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(title = paste("Population Growth Rate Comparison in", input$GrowthYear),
           x = "Country", y = "Growth Rate (%)") +
      theme_minimal() +
      theme(title = element_text(size = 16),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12))
  })
  
  ## Comparison: Total Population
  output$AllTPpopulationPlot <- renderPlot({
    filtered_data <- Population |>
      filter(Gender == input$AllTPgenderSelection)
    ggplot(filtered_data, aes(x = Year, y = Value, color = Country)) +
      geom_smooth(method = "lm") +
      geom_point(size = 2) +
      labs(title = paste("Total Population Comparison -", input$AllTPgenderSelection),
           x = "Year", y = "Population") +
      theme_minimal()+
      theme(title = element_text(size = 16),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12))
  })
  
  ## Comparison: LE
  output$lePlot <- renderPlot({
    filtered_data <- LE |>
      filter(Year >= input$yearRangeLE[1], Year <= input$yearRangeLE[2])
    ggplot(filtered_data, aes(x = Year, y = Value, color = Country)) +
      geom_point(size =2)+
      geom_smooth(method = "lm") +
      labs(title = "Life Expectancy Comparison Over Years",
           x = "Year", y = "Life Expectancy") +
      theme_minimal() +
      theme(title = element_text(size = 16),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12))
  })
  
  ## Comparison: Literacy
  output$literacyPlot <- renderPlot({
    filtered_data <- Literacy |>
      filter(Gender == input$genderSelectionLiteracy)
    ggplot(filtered_data, aes(x = Country, y = Value, fill = Country)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(title = paste("Literacy Rate Comparison -", input$genderSelectionLiteracy),
           subtitle = "Marshall Island vs. Tonga",
           x = "Country", y = "Literacy Rate (%)") +
      theme_minimal()+
      theme(title = element_text(size = 16),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12))
  })
  
  ## Mean Height for Age
  output$heightPlot <- renderPlot({
    filtered_data <- Height |>
      filter(Gender == input$genderSelectionHeight)
    ggplot(filtered_data, aes(x = Country, y = Value, fill = Country)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(title = "Mean Height for Age Comparison by Gender",
           x = "Country", y = "Mean Height for Age (Z-score)") +
      theme_minimal()+
      theme(title = element_text(size = 16),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12))
  })
  
  
  ## SWOT
  output$strength0 <- renderText("Its location in the Pacific can be strategically important, which brings lease payments from the US for the use of Kwajalein Atoll.")
  output$strength1 <- renderText("As an archipelagic nation, the Marshall Islands possess an extensive territorial sea and Exclusive Economic Zone (EEZ) relative to its land area. The abundance of species and other marine resources within these waters endows the Marshall Islands with unique advantages in the development of marine resources")
  output$strength2 <- renderText("The islands are known for their stunning natural beauty, including pristine beaches and diverse marine life, which is a draw for tourism.")
  
  output$opportunity0 <- renderText("The Marshall Islands can leverage its pristine beaches and unique cultural heritage for eco and cultural tourism. Developing sustainable tourism infrastructure and practices offers economic growth while preserving natural beauty.")
  output$opportunity1 <- renderText("With potential for solar and wind energy, the Marshall Islands can reduce reliance on imported fuels and lead in climate change initiatives. This shift opens doors for international funding and green energy partnerships.")
  output$opportunity2 <- renderText("The strategic location and vast EEZ of the Marshall Islands enable significant roles in marine conservation and regional security. This positions them as key players in global climate advocacy and policy, enhancing resilience for themselves and other island nations.")
  
  output$weakness0 <- renderText("The Marshall Islands' economy heavily relies on external aid, particularly from agreements like the Compact of Free Association with the United States. This dependency underscores the need for diversifying the economy to enhance self-reliance.");
  output$weakness1 <- renderText("With limited natural resources, especially in terms of land and fresh water, the Marshall Islands face challenges in achieving self-sufficiency in food and energy, making it dependent on imports.")
  output$weakness2 <- renderText("Being low-lying atolls, the Marshall Islands are extremely vulnerable to climate change impacts, including sea-level rise and severe weather events, threatening their very existence and necessitating urgent adaptation measures.")
  
  output$threat0 <- renderText("The Marshall Islands faces severe threats from climate change, notably rising sea levels and extreme weather events, which pose existential risks to its low-lying atolls and can lead to displacement of communities and loss of land.")
  output$threat1 <- renderText("The nation's reliance on external aid and a narrow economic base makes it vulnerable to economic shocks. This dependence can lead to instability, especially in the face of global economic fluctuations.")
  output$threat2 <- renderText("As a small island nation, the Marshall Islands faces challenges in maintaining political autonomy and navigating complex global dynamics. The strategic importance of its location can attract international interest, potentially impacting its sovereignty and regional stability.")

}

shinyApp(ui, server)